proc import datafile = 'C:/Users/buzzlightyear/Desktop/ANKLE/AL.ANK.2A.PCA.csv'
out= train_ANK
dbms = csv
replace;
getnames = yes;
run;
proc import datafile = 'C:/Users/buzzlightyear/Desktop/ANKLE/AL.ANK.2B.PCA.csv'
out= test_ANK
dbms = csv
replace;
getnames = yes;
run;
Data train_ANK;
set train_ANK;
Wt= round(0.45359237*Wt);
Ht= round(0.0254*Ht,0.01);
BMI=Wt/Ht**2;
run;
Data train_ANK;
set train_ANK;
Drop Ht Wt;
run;
Data test_ANK;
set test_ANK;
Wt= round(0.45359237*Wt);
Ht= round(0.0254*Ht,0.01);
BMI=Wt/Ht**2;
run;
Data test_ANK;
set test_ANK;
Drop Ht Wt;
run;
proc format;
value $cos SED = '1'
		   LPA = '2'
		   MPA = '3'
		   VPA = '4';
run;
Data train_ANK;
set train_ANK;
format COS_Intensity $cos.;
run;
Data test_ANK;
set test_ANK;
format COS_Intensity $cos.;
run;
proc logistic data = train_ANK;
class Sex / param=ref; 
model COS_Intensity= PC1 -- PC17 Sex Age BMI / unequalslopes = Age;
score data = test_ANK out=mypreds fitstat;
run;
proc freq data=mypreds;
table F_COS_Intensity*I_COS_Intensity / norow nocol nopct;
test WTKAP;
run;
/* 2 levels*/

proc import datafile = 'C:/Users/buzzlightyear/Desktop/ANKLE/AL.ANK.2A.2level.csv'
out= train_ANK
dbms = csv
replace;
getnames = yes;
run;
proc import datafile = 'C:/Users/buzzlightyear/Desktop/ANKLE/AL.ANK.2B.2level.csv'
out= test_ANK
dbms = csv
replace;
getnames = yes;
run;
proc format;
value $cos 1 = '1'
		   2 = '2';
run;
Data train_ANK;
set train_ANK;
format COS_Intensity $cos.;
run;
Data test_ANK;
set test_ANK;
format COS_Intensity $cos.;
run;
Proc print data = test_ANK;
run;
proc logistic data = train_ANK;
class Sex / param=ref;
model COS_Intensity= PC1 -- PC17 Sex Age BMI / unequalslopes = Age;
score data = test_ANK out=mypreds fitstat;
output out = preds p=p xbeta=linp; 
run;
