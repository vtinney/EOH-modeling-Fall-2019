
/*Final Project Pubh 6199 */
/* V Southerland & W Steen */
/* Assess dataset for potential confounding variables */
/* Descriptive statistics */
/* Descriptive statistics */

DM 'LOG;CLEAR';
DM 'OUT;CLEAR';
DM 'ODSRESULTS; CLEAR;';
options nocenter nodate pageno=1 linesize=120 pagesize=68;
options nofmterr;

/*----------------------------------------+
+    THE GEORGE WASHINGTON UNIVERSITY     + 
+          PUBLIC HEALTH 6199             + 
+         Final project 				  +
+ DATA SET REFERENCE: ACE CLINICAL TRIALS +
+ PROGRAMMER: VERONICA TINNEY             +
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

run;

/*check missing data */
proc means NMISS N;
run;

proc contents;
run;

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
model smoke=Concentration;
title 'Criteria 1 - Concentration and smoking';
run;

/* Criteria #2 - Does F independently predict D among nonE (no smoking) */
proc logistic descending; 
model dis=log; 
 where smoke eq 0; 
title 'Criteria #2 - Is concentration independent predictors of disease among non-smokers'; 
run;
quit;

proc logistic descending;
model dis=smoke; 
where smoke eq 0;
title 'Criteria #2 - Is smoking independent predictors of disease among non-smokers';
run;
quit;
