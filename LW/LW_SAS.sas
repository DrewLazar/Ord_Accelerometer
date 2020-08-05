proc import datafile = 'C:/Users/buzzlightyear/Desktop/LW/AL.LW.2A.PCA.csv'
out= train_LW
dbms = csv
replace;
getnames = yes;
run;
proc import datafile = 'C:/Users/buzzlightyear/Desktop/LW/AL.LW.2B.PCA.csv'
out= test_LW
dbms = csv
replace;
getnames = yes;
run;
Data train_LW;
set train_LW;
Wt= round(0.45359237*Wt);
Ht= round(0.0254*Ht,0.01);
BMI=Wt/Ht**2;
run;
Data train_LW;
set train_LW;
Drop Ht Wt;
run;
Data test_LW;
set test_LW;
Wt= round(0.45359237*Wt);
Ht= round(0.0254*Ht,0.01);
BMI=Wt/Ht**2;
run;
Data test_LW;
set test_LW;
Drop Ht Wt;
run;
proc contents data = train_LW;
run;
proc format;
value $cos SED = '1'
		   LPA = '2'
		   MPA = '3'
		   VPA = '4';
run;
Data train_LW;
set train_LW;
format COS_Intensity $cos.;
run;
Data test_LW;
set test_LW;
format COS_Intensity $cos.;
run;
Proc print data = test_LW;
run;
proc logistic data = train_LW;
class Sex / param=ref;
model COS_Intensity= PC1 -- PC17 Sex Age BMI / unequalslopes =  Age;
score data = test_LW out=mypreds fitstat;
output out = preds p=p xbeta=linp; 
run;
proc freq data=mypreds;
tables F_COS_Intensity*I_COS_Intensity / norow nocol nopct;
test WTKap;
run;
goptions reset = all ;
symbol i = join w= 1 ;
proc sort data = preds;
by BMI;
run;
proc gplot data = preds;
   plot linp*BMI=_level_;
run;
quit;
/* 2 levels*/

proc import datafile = 'C:/Users/buzzlightyear/Desktop/LW/AL.LW.2A.2level.csv'
out= train_LW
dbms = csv
replace;
getnames = yes;
run;
proc import datafile = 'C:/Users/buzzlightyear/Desktop/LW/AL.LW.2B.2level.csv'
out= test_LW
dbms = csv
replace;
getnames = yes;
run;
proc format;
value $cos 1 = 'LPA'
		   2 = 'MVPA';
run;
Data train_LW;
set train_LW;
format COS_Intensity $cos.;
run;
Data test_LW;
set test_LW;
format COS_Intensity $cos.;
run;
Proc print data = test_LW;
run;
proc logistic data = train_LW;
class Sex / param=ref;
model COS_Intensity= PC1 -- PC17 Sex Age BMI;
score data = test_LW out=mypreds fitstat;
output out = preds p=p xbeta=linp; 
run;
