proc import datafile = 'C:/Users/buzzlightyear/Desktop/HIP/AL.HIP.2A.PCA.csv'
out= train_HIP
dbms = csv
replace;
getnames = yes;
run;
proc import datafile = 'C:/Users/buzzlightyear/Desktop/HIP/AL.HIP.2B.PCA.csv'
out= test_HIP
dbms = csv
replace;
getnames = yes;
run;
Data train_HIP;
set train_HIP;
Wt= round(0.45359237*Wt);
Ht= round(0.0254*Ht,0.01);
BMI=Wt/Ht**2;
run;
Data train_HIP;
set train_HIP;
Drop Ht Wt;
run;
Data test_HIP;
set test_HIP;
Wt= round(0.45359237*Wt);
Ht= round(0.0254*Ht,0.01);
BMI=Wt/Ht**2;
run;
Data test_HIP;
set test_HIP;
Drop Ht Wt;
run;
proc format;
value $cos SED = '1'
		   LPA = '2'
		   MPA = '3'
		   VPA = '4';
run;
Data train_HIP;
set train_HIP;
format COS_Intensity $cos.;
run;
Data test_HIP;
set test_HIP;
format COS_Intensity $cos.;
run;
proc logistic data = train_HIP;
class Sex / param=ref; 
model COS_Intensity= PC1 -- PC17 Sex Age BMI / unequalslopes = (Sex Age);
score data = test_HIP out=mypreds fitstat;
run;
proc freq data=mypreds;
table F_COS_Intensity*I_COS_Intensity / norow nocol nopct;
test WTKAP;
run;
/* 2 levels*/

proc import datafile = 'C:/Users/buzzlightyear/Desktop/HIP/AL.HIP.2A.2level.csv'
out= train_HIP
dbms = csv
replace;
getnames = yes;
run;
proc import datafile = 'C:/Users/buzzlightyear/Desktop/HIP/AL.HIP.2B.2level.csv'
out= test_HIP
dbms = csv
replace;
getnames = yes;
run;
proc format;
value $cos 1 = '1'
		   2 = '2';
run;
Data train_HIP;
set train_HIP;
format COS_Intensity $cos.;
run;
Data test_HIP;
set test_HIP;
format COS_Intensity $cos.;
run;
Proc print data = test_HIP;
run;
proc logistic data = train_HIP;
class Sex / param=ref;
model COS_Intensity= PC1 -- PC17 Sex Age BMI / unequalslopes = Age;
score data = test_HIP out=mypreds fitstat;
output out = preds p=p xbeta=linp; 
run;
