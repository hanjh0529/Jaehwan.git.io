/*
************************  Survival Analysis PROJECT  ***********************************

Jaehwan Han

****************************************************************************
*/


*We first create a rich text format file where SAS will export all the outputs;
ods rtf file="\\tsclient\Drives\Fall 2019\SAS\1831 Project.rtf";

*Now, we import the dataset;
proc import datafile = '\\tsclient\Drives\Fall 2019\SAS\1831 Project\PROJDATA.csv'
 out = project
 dbms = CSV
 ;
run;

*We now define an id variable which will be required later;
data project;
	set project;
	label time = "Time to drug use (days)" treat = "Treatment group" age = "Age (yrs)" race = "Race" ndrugtx = "Prior drug treatments" beck = "Beck score" site = "Site" hercoc = "Drug type" ivhx = "IV drug use";
	id = _N_;
run;



/******************************** UNIVARIABLE ANALYSIS **************************************/


*First checking the univariate distributions and summary statistics;

proc univariate data = project;
var time;
run;

proc freq data = project;
tables censor;
run;

proc univariate data=project;
var age;
run;

proc freq data = project;
tables treat;
run;

proc freq data = project;
tables race;
run;

proc univariate data=project;
var ndrugtx;
run;

proc univariate data=project;
var beck;
run;

proc freq data = project;
tables site;
run;

proc freq data = project;
tables hercoc;
run;

proc freq data = project;
tables ivhx;
run;


/* Checking the KM estimate and curve without any adjustors */
proc lifetest data=project plots=(s);
time time*censor(0);
run;


/*Univariate log-rank tests and Cox PH models*/

* Checking p-values and retaining variables with p-value < 0.2;

ods graphics on;
proc lifetest data = project plots = survival(cl);
time time*censor(0);
strata treat;
run;
ods graphics off;

ods graphics on;
proc lifetest data = project plots = survival(cl);
time time*censor(0);
strata race;
run;
ods graphics off;

ods graphics on;
proc lifetest data = project plots = survival(cl);
time time*censor(0);
strata site;
run;
ods graphics off;

ods graphics on;
proc lifetest data = project plots = survival(cl);
time time*censor(0);
strata hercoc;
run;
ods graphics off;

ods graphics on;
proc lifetest data = project plots = survival(cl);
time time*censor(0);
strata ivhx;
run;
ods graphics off;

proc phreg data = project;
model time*censor(0) = age;
run;

proc phreg data = project;
model time*censor(0) = ndrugtx;
run;

proc phreg data = project;
model time*censor(0) = beck;
run;







/***************************  Model Selection  *****************************/

/* Using Forward selection */
proc phreg data= project;
class hercoc(param=ref ref='1') ivhx(param=ref ref='1');
model time*censor(0) = treat age race ndrugtx beck site hercoc ivhx / selection = forward slentry=0.05 start=1 details;
run; /* Selected variables = (ndrugtx, age, ivhx, treat) */

/* Using Backward selection */
proc phreg data= project;
class hercoc(param=ref ref='1') ivhx(param=ref ref='1');
model time*censor(0) = treat age race ndrugtx beck site hercoc ivhx / selection = backward slstay=0.05 include=1 details;
run; /* Selected variables = (ndrugtx, age, ivhx, treat) */

/* Using Stepwise selection */
proc phreg data= project;
class hercoc(param=ref ref='1') ivhx(param=ref ref='1');
model time*censor(0) = treat age race ndrugtx beck site hercoc ivhx / selection = stepwise slentry=0.1 slstay=0.05 include=1 details;
run;  /* Selected variables = (ndrugtx, age, ivhx, treat) */








/*************************  Regression Diagnostics  ****************************/

/* Global test with selected covariates*/
/* Testing Global Null hypothesis: Beta=0 */

proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = age treat ndrugtx ivhx;
run;


/* Check functional form of predictors using Martingale residuals, only for continous, not categorical */
/* As long as it is linear, it's okay */

* age;
proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = ndrugtx ivhx treat;
output out=outp resmart=Mart;
run; 

proc sgplot data=outp;
loess y=Mart x=age / smooth =0.5;
run;

*ndrugtx;
proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = age ivhx treat;
output out=outp resmart=Mart;
run; 

proc sgplot data=outp;
loess y=Mart x=ndrugtx / smooth =0.5;
run;






/****************************  PH assumption diagnoses ********************************/

/* Using log interaction approach */

proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = age ndrugtx treat ivhx time_age;
time_age = log(time)*age;
run; /* Not violate PH assumption */

proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = age ndrugtx treat ivhx time_ndrugtx;
time_ndrugtx = log(time)*ndrugtx;
run; /* Not violate PH assumption */

proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = age ndrugtx treat ivhx time_ivhx;
time_ivhx = log(time)*ivhx;
run; /* Not violate PH assumption */

proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = age ndrugtx treat ivhx time_treat;
time_treat = log(time)*treat;
run; /* Not violate PH assumption */


/* Using Standardized score residuals */

ods graphics on;
proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = age ndrugtx treat ivhx;
assess ph/resample=1000 npaths=0 seed=1;
run; /* PH assumption does not hold for Treat Covariate */
ods graphics off; 






/****************  Examine overall fit with Cox-snell Residuals  *********************/

*Using original model selected by the model selection methods;

proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = treat age ndrugtx ivhx;
output out=out_cox LOGSURV = h;
run;

data recode; set out_cox;
csr=-h;
cons=1;
run;

proc phreg data=recode;
model csr*censor(0)=cons;
output out=plotdat0 logsurv=mhaz;
run;

data plotdat; set plotdat0; estH=-mhaz; run;

proc sgplot data=plotdat;
scatter y=estH x=csr;
series x=csr y=csr;
yaxis values=(0 to 3 by 0.5);
xaxis values=(0 to 3 by 0.5);
run;







/******************  Check outliers using Deviance Residuals and DFBETAS  ****************************/

*Using original model selected by the model selection methods;

*Checking influential points using Deviance residuals;
proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = treat age ndrugtx ivhx;
output out=out_inf xbeta=xbeta resdev=dev;
run;

proc sgplot data=out_inf;
scatter y=dev x=xbeta;
run;

*Checking influential points using DFBETAs;
proc phreg data = project;
model time*censor(0) = treat age ndrugtx ivhx;
output out=out_inf dfbeta=dfb_treat dfb_age dfb_nd dfb_iv;
run;

proc sgplot data = out_inf;
scatter x=id y=dfb_treat;
refline 0 / axis = y;
run;

proc sgplot data = out_inf;
scatter x=id y=dfb_age;
refline 0 / axis = y;
run;

proc sgplot data = out_inf;
scatter x=id y=dfb_nd;
refline 0 / axis = y;
run;

proc sgplot data = out_inf;
scatter x=id y=dfb_iv;
refline 0 / axis = y;
run;








/**********************  Remedy for violation of PH assumption  **********************/


/* Piecewise model fitting */

* Searching for the median of time;
proc means data=project Median;
var time;
run; /* 166 = median event times */

/* Transforming "treat" to time-varying covariates and fitting a piecewise model */
ods graphics on;
proc phreg data=project;
class ivhx(param=ref ref='1');
model time*censor(0) = age ndrugtx ivhx treat_bef treat_after / risklimits;
if 0<=time<=166 then do; treat_bef=treat; treat_after=0; end;
else if time>166 then do; treat_bef=0; treat_after=treat; end;
estimate 'Age effect' age 10 / exp cl; /* Estimating Hazard ratio for 10yr increase in age */
label treat_bef = "Treat <166 days" treat_after = "Treat >166 days";
run;
ods graphics off;





*Closing the rtf file;
ods rtf close;
