/* TX_food 2016 */

data TX_food;
infile "H:\food.csv" DSD firstobs=2;
input COUNTY $ POP:comma. FI_rate:percent. BELOW:percent. BETWEEN:percent. ABOVE:percent.  POVERCAT  COSTMEAL:dollar.  MEDINCOME:dollar. URRURAL  DIABETES  HSGRAD;
label COUNTY = "County"
  	POP = "population"
  	FI_rate = "Food Insecure rate"
  	BELOW = "Below 165"
  	BETWEEN = "Between 165-185"
  	ABOVE = "Above 185"
  	POVERCAT = "Poverty category: 1= 165% below poverty level, 3= more than 185% poverty level"
  	COSTMEAL = "Cost per meal"
  	MEDINCOME = "Median income"
  	URRURAL = "Urban Rural: 0=Rural, 1=Urban"
  	DIABETES = "Diabetes prevalence"
  	HSGRAD = "High school graduates rate";
run;
 
/* Scatterplot matrix */
proc sgscatter data=TX_food;
matrix FI_rate POP URRURAL POVERCAT MEDINCOME COSTMEAL DIABETES HSGRAD / group=URRURAL;
run;
 
/* Ellipse Correlation Matrix */
names(food)

food <- TX_food %>% select(-c(BETWEEN165_185, ABOVE_185, BELOW_165))
data=cor(food[,-1])
# Build a Pannel of 100 colors with Rcolor Brewer
my_colors <- brewer.pal(5, "Spectral")
my_colors=colorRampPalette(my_colors)(100)

# Order the correlation matrix
ord <- order(data[1, ])
data_ord = data[ord, ord]
plotcorr(data_ord , col=my_colors[data_ord*50+50] , mar=c(1,1,1,1)  );

/* standardized var  */
PROC STANDARD DATA=TX_food MEAN=0 STD=1 OUT=food_scaled;
  VAR POP MEDINCOME COSTMEAL DIABETES HSGRAD;
RUN;
 
/* dummy variable */
data food2;
set food_scaled;
if POVERCAT = 2 then Pov_2 =1; else Pov_2 =0;
if POVERCAT = 3 then Pov_3 =1; else Pov_3 =0;
urban_dp = URRURAL*DIABETES;
mhi_dp = MEDINCOME*DIABETES;
hs_sq = HSGRAD**2;
mhi_sq = MEDINCOME**2;
label urban_dp ="urban_rural * diabetes_prevalence"
  	mhi_dp = "median houseincome * diabetes_prevalence"
  	hs_sq = "high school graduation squared"
  	mhi_Sq = "median house income squared";
Run;
 
/*Testing squared terms*/
proc reg data = food2 outest=est;
	model FI_rate = POP URRURAL Pov_2 Pov_3 MEDINCOME COSTMEAL DIABETES HSGRAD hs_sq mhi_sq/ss1 r p;
	plot r.*p.;
	plot npp.*r.;
	output out = outfood p=yhat r=resid stdr = stdresid ;
run;
 
/*Drop varaibles (sqaured terms, below, median, above, county, Povertcat)*/
data Total;
set food2 (KEEP = POP FI_rate Pov_2 Pov_3 COSTMEAL MEDINCOME URRURAL DIABETES HSGRAD urban_dp mhi_dp);
run;
 
/* Premeliminary Model Diagnostics */
proc reg data = Total outest=est;
	model FI_rate = POP COSTMEAL MEDINCOME Pov_2 Pov_3 URRURAL DIABETES HSGRAD urban_dp mhi_dp/ss1 vif r p;
	plot r.*p.;
	plot npp.*r.;
	output out = outfood p=yhat r=resid stdr = stdresid ;
run;
 
/* Check normality */
title "Model Normality and investigation";
proc univariate data= outfood normal;
var resid; 
histogram;
qqplot;
run;
 
/* BP test to detect homoscedasticity */
proc model data= Total;
FI_rate = a0+ b1*POP + b2*COSTMEAL + b3*MEDINCOME + b4*Pov_2 + b5*Pov_3 + b6*URRURAL + b7*DIABETES + b8*HSGRAD + b9*urban_dp + b10*mhi_dp;
fit FI_rate PARMS= (a0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10) / breusch=(POP COSTMEAL MEDINCOME Pov_2 Pov_3 URRURAL DIABETES HSGRAD urban_dp mhi_dp);
run;
 
/*Perform stepwise selection */
title "Stepwise Model selection";
proc reg data = Total outest = est1;
  	model FI_rate = POP COSTMEAL MEDINCOME Pov_2 Pov_3 URRURAL DIABETES HSGRAD urban_dp mhi_dp / r p selection = stepwise sle = 0.05 sls=0.1 aic;
  	plot r.*p.;
  	plot npp.*r.;
  	 output out = outfood p=yhat r=resid stdr = stdresid ;
run;
 
/*Perform backward selection */
title "backward Model selection";
proc reg data = Total outest = est;
  	model FI_rate = POP COSTMEAL MEDINCOME Pov_2 Pov_3 URRURAL DIABETES HSGRAD urban_dp mhi_dp / r p selection = backward aic;
  	plot r.*p.;
  	plot npp.*r.;
  	output out = outfood p=yhat r=resid stdr = stdresid ;
run;
 
/*Lasso model*/
proc glmselect data = food2 plots(stepaxis=normb)=all;
model FI_rate = POP COSTMEAL MEDINCOME Pov_2 Pov_3 URRURAL DIABETES HSGRAD urban_dp mhi_dp / selection=lasso(stop=none choose=AIC);
run;

/*Final Model */
data Final;
set Total (KEEP = FI_rate COSTMEAL MEDINCOME URRURAL DIABETES HSGRAD urban_dp mhi_dp);
run;
proc sgscatter data=Final;
matrix FI_rate COSTMEAL MEDINCOME URRURAL DIABETES HSGRAD urban_dp mhi_dp / group=URRURAL;
run;

/* Final Model Diagnostics */
proc reg data= Final;
model FI_rate = COSTMEAL MEDINCOME URRURAL DIABETES HSGRAD urban_dp mhi_dp / ss1;
plot nap.*r.;
plot r.*p.;
output out =outfoodfinal p=yhat r=resid stdr=stdresid;
run;
 
/* Final Model Check normality */
title "Final Model Normality and investigation";
proc univariate data= outfoodfinal normal;
var resid; 
histogram;
qqplot;
run;
 
/*Cook, DFFITS, leverage */
ods graphics on;
proc reg data= Total
plots(label) = (CooksD RstudentByLeverage DFFITS DFBETAS);
model FI_rate = COSTMEAL MEDINCOME URRURAL DIABETES HSGRAD urban_dp mhi_dp;
run;
ods graphics off;
 
/* Final model BP test to detect homoscedasticity */
proc model data= Total;
FI_rate = a0+ b1*COSTMEAl + b2*MEDINCOME + b3*URRURAL + b4*DIABETES + b5*HSGRAD + b6*urban_dp + b7*mhi_dp;
fit FI_rate PARMS= (a0 b1 b2 b3 b4 b5 b6 b7) / breusch=(COSTMEAL MEDINCOME URRURAL DIABETES HSGRAD urban_dp mhi_dp);
run;

/*Weighted Final model*/
data outfoodfinal;
set outfoodfinal;
absr=abs(resid); sqrr=resid*resid;
proc reg data=outfoodfinal;
model FI_rate = COSTMEAL MEDINCOME URRURAL DIABETES HSGRAD urban_dp mhi_dp;
output out=Weightmodel p=shat;
data Weightmodel;set Weightmodel;
wt=1/(shat*shat);
 

/* Weighted model BP test to detect homoscedasticity */
proc model data= Weightmodel;
FI_rate = a0+ b1*COSTMEAl + b2*MEDINCOME + b3*URRURAL + b4*DIABETES + b5*HSGRAD + b6*urban_dp + b7*mhi_dp;
fit FI_rate PARMS= (a0 b1 b2 b3 b4 b5 b6 b7) / breusch=(COSTMEAL MEDINCOME URRURAL DIABETES HSGRAD urban_dp mhi_dp);
run;

