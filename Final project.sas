

DM 'LOG;CLEAR';
DM 'OUT;CLEAR';
DM 'ODSRESULTS; CLEAR;';
options nocenter nodate pageno=1 linesize=120 pagesize=68;
options nofmterr;

/*----------------------------------------+
+    THE GEORGE WASHINGTON UNIVERSITY     + 
+          PUBLIC HEALTH 6199             + 
+             FINAL PROJECT 		      +
+    PROGRAMMER: VERONICA SOUTHERLAND     +
+-----------------------------------------*/

OPTIONS NODATE NONUMBER LS=100;

TITLE1 'THE GEORGE WASHINGTON UNIVERSITY';
TITLE2 'PUBH 6199';
TITLE3 'Final Project: Descriptive Analysis';
TITLE4 "&SYSDATE";
TITLE5 'V Southerland & W Steen';
OPTIONS NODATE NONUMBER ;


data work;
 set work;
 title 'Final dataset';
 title1 '  ';

 smoke=0; if Smoking ='Yes' then smoke=1;
 hist=0; if History ='Yes' then hist=1;
 dis=0; if Disease = 'Yes' then dis=1;


 weightx=0; if Weight >=92 then weightx=1;

run;

/*check missing data */
proc means NMISS N;
run;

proc contents;
run;

/*Assess distribution of variables*/
proc freq;
tables smoke Weight dis hist;
run;

proc univariate normal plot;
var Concentration log;
run;
/* Crude associations with predictor variables*/
proc freq;
table smoke*dis/ chisq;
run;
proc freq;
table hist*dis/ chisq;
run;
proc ttest;
class dis;
var Weight;
run;

/*Bivariate analysis - PCV criteria smoking*/
proc logistic descending;
model weightx=hist;
title 'Criteria 1 - Weight and family history';
run;

/* Criteria #2 - Does F independently predict D among nonE (no smoking) */
proc logistic descending; 
model dis=hist; 
 where weightx eq 0; 
title 'Criteria #2 - Is family history independent predictors of disease among those under the mean weight'; 
run;
quit;

/* Full logistic regression */

/* Correlation matrix */
proc corr data = work;
  var dis Concentration Weight smoke hist;
run;

/* Full logistic regression */
PROC LOGISTIC DATA=work DESCENDING Plot= Effect;
class smoke(ref='0') hist(ref='0')/Param= Ref;
   model dis=hist Concentration Weight smoke/RISKLIMITS lackfit expb ;
ODDSRATIO 'Odds ratios'  SMOKE/Diff=Ref;
OUTPUT OUT=NEW XBETA= LINEAR P= PPROB;
RUN;

/* Output probabilities */
PROC UNIVARIATE DATA= NEW;
 VAR PPROB;
RUN;

/* Plot variables against log-odds */
Proc Sgplot Data= New;
 Loess x=Concentration y=pprob/ interpolation=linear;
Run;

Proc Sgplot Data= New;
 Loess x=Weight y=pprob/ interpolation=linear;
Run;

Proc Sgplot Data= New;
 Loess x=hist y=pprob/ interpolation=linear;
Run;

Proc Sgplot Data= New;
 Loess x=Smoke y=pprob/ interpolation=linear;
Run;

* ----------------------------;
*  E N D  O F  P R O G R A M  ;
*-----------------------------;
