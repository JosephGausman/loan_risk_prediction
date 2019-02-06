

libname loan 'E:\DSA\SAS Project\Presentation\Modeling';

libname loan '/folders/myfolders/SAS Project/Presentation/Modeling';


/*--------------------------------------------------------------------------------------------------------*/
/* Generation of random data 60% -------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/

data loan.mydata;
  set loan.HMEQ;
  if ranuni(1234567)<=0.6;
run;

/*--------------------------------------------------------------------------------------------------------*/
/* Exploration of the dataset ----------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/

proc contents data=loan.HMEQ; run;

proc print data=loan.mydata(obs=10); run;

proc means data=loan.HMEQ; run;

proc freq data=loan.HMEQ;
   tables bad reason job;
run;

/*--------------------------------------------------------------------------------------------------------

Feature Engineering - Treatment of Categorical Variables (JOB, REASON)

Conversion of char variables into numeric variables by adding a new column for each description
and 1 accordingly in the newly created variable (Like dummies in Python)

--------------------------------------------------------------------------------------------------------*/
data loan.mydata(drop=job reason Value_grp Value_grp2);
  set loan.mydata;
  JOB_Mgr=(JOB='Mgr');
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');
run;

proc contents data=loan.mydata; run;
proc print data=loan.mydata(obs=20); run;

/*--------------------------------------------------------------------------------------------------------*/
/* Macro Variables ---------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/

%LET inter_var=
clage
clno
debtinc
delinq
derog
loan
mortdue
ninq
value
yoj
;

%LET DSN=loan.mydata;
%LET RESP=BAD;
%LET GROUPS=10;

%MACRO LOGTCONT                        ;
      OPTIONS CENTER PAGENO=1 DATE;
	  data loan.test;                                
	    set &DSN;                                    
	  run;
	 
/* Division of each variable into 10 categories for 10  variables in inter_var */

	  %do i=1 %to 10;                               
	  %LET VBLE=%scan(&inter_var, &i)  ;              
       PROC RANK DATA = loan.test (KEEP=&RESP &VBLE) 
               GROUPS = &GROUPS                      
                  OUT = loan.temp1     ;             
            RANKS NEWVBLE              ;             

            VAR &VBLE                  ;
       RUN                             ;

/* Descriptive statistics for all the 10 variable*/
       
       PROC SUMMARY DATA = loan.temp1 NWAY ;         
            CLASS NEWVBLE              ;             
            VAR &RESP &VBLE            ;             
            OUTPUT OUT = loan.temp2                  
                  MEAN =                            
                  MIN(&VBLE)=MIN                      
                  MAX(&VBLE)=MAX                   
                     N = NOBS          ;             
       RUN                             ;
  
/* LOGIT for all the 10 groups of 10 10 variables */
	   
 	   DATA loan.temp2                 ;
            SET loan.temp2             ;
            IF &RESP NE 0 THEN                          
               LOGIT = LOG ( &RESP / (1- &RESP) )  ;    
            ELSE IF &RESP = 0 THEN LOGIT = .       ;    
       RUN                            ;

/* Creation of another group (99) for the missing values */

       PROC SQL NOPRINT;                        
        CREATE TABLE loan.JUNK3 AS                 
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(&RESP) AS &RESP
        FROM loan.test
        WHERE &VBLE=.
       ;

/* LOGIT for the missing values group*/

       DATA loan.JUNK3;
        SET loan.JUNK3;
        LOGIT=LOG(&RESP/(1-&RESP));      
       RUN;

/* The missing values group (99) is added to the rest of the groups */
  
       DATA loan.JUNK4;
        SET loan.temp2 loan.JUNK3;    
       RUN;

/*--------------------------------------------------------------------------------------------------------*/
/* Ploting------------------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/

       PROC PLOT DATA = loan.JUNK4         ;
            TITLE1 "Plot of Logit(Response) by &&VBLE" ;
            PLOT  LOGIT* &VBLE        ;
       RUN  ;

        proc plot data=loan.junk4;
        plot &resp*&vble;
        plot _freq_*&vble;
        TITLE2 "Plot of Response by &&VBLE" ;
        run;

       PROC PRINT DATA = loan.JUNK4 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped &&VBLE" ;
            VAR NEWVBLE NOBS &VBLE MIN MAX &RESP ;
            LABEL NEWVBLE = "&&VBLE Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;

	   %end;		

%MEND LOGTCONT      ;
%LOGTCONT      ;


/*--------------------------------------------------------------------------------------------------------*/
/* Treatment of missing values ---------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/

data loan.traindata;
  set loan.mydata;
  
  if CLAGE=. then CLAGE=94.325;             
  if CLAGE>300 then CLAGE=300;

  if CLNO<10 then CLNO=0;                   
  if CLNO=. then CLNO= 31.3878;

     DEBTINC_MISS=(DEBTINC=.);              

  if DELINQ=. then DELINQ=0;               

  if DEROG=. then DEROG=0;                

  if LOAN>30000 then LOAN=30000;            

  if MORTDUE=. then MORTDUE=54175.95;       
  if MORTDUE>120000 then MORTDUE=120000;
  
  if NINQ=. then NINQ=0;                  

     VALUE_MISS=(VALUE=.);                  
  if YOJ=. then YOJ=25;                   


run;

PROC CONTENTS DATA=loan.traindata;
RUN;

/*--------------------------------------------------------------------------------------------------------*/
/*---Modeling---------------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------------------------------------*/
/* Experimenting with coefficients of different variables ----------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------------------------------------*/
/* INPUT2 ------------------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/

%LET INPUT2=
JOB_Mgr
JOB_Office
JOB_Other
JOB_ProfExe
JOB_Sales
JOB_Self
JOB_miss
REASON_DebtCon
REASON_HomeImp
REASON_Miss
VALUE_MISS
clage
clno
delinq
derog
loan
mortdue
ninq
yoj
;

proc logistic data=loan.traindata descending;
model bad=&input2
  /selection=stepwise fast lackfit rsquare corrb stb;
run;

data loan.val;
  set loan.traindata;
logit=
-0.0868      
-0.4884      * JOB_Office
+0.9442      * JOB_Sales
+0.5408      * JOB_Sele
-2.2244      * JOB_miss
-0.2533      * REASON_DebtCon
+4.238       * VALUE_MISS
-0.00707     * CLAGE
-0.012       * CLNO
+0.7397      * DELINQ
+0.6421      * DEROG
-0.00002     * LOAN
+0.175       * NINQ
-0.0188      * YOJ
;
prob2=1/(1+exp(-logit)); run;

/*--------------------------------------------------------------------------------------------------------*/
/* INPUT3 ------------------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/

%LET INPUT3=
DEBTINC_MISS                   
JOB_Office
JOB_Other
JOB_Sales
JOB_Self
JOB_miss
REASON_HomeImp
VALUE_MISS
clage
clno
delinq
derog
ninq
yoj
;

proc logistic data=loan.traindata descending;
model bad=&input3
  /selection=stepwise fast lackfit rsquare corrb stb;
run;

data loan.val;
  set loan.traindata;
Logit=
-1.3884	    
+2.6813	    * DEBTINC_MISS
-0.4928	    * JOB_Office
+0.886	    * JOB_Sales
-2.0645	    * JOB_miss
+4.4174	    * VALUE_MISS
-0.00684	* CLAGE
-0.0109	    * CLNO
+0.6985	    * DELINQ
+0.5923   	* DEROG
+0.1214	    * NINQ
-0.0259	    * YOJ
;
prob3=1/(1+exp(-logit)); run;

/*---------------------------------------------------------------------------------------------------------*/
/* INPUT 4 ------------------------------------------------------------------------------------------------*/
/*---------------------------------------------------------------------------------------------------------*/

%LET INPUT4=
DEBTINC_MISS
JOB_Office
JOB_Sales
JOB_miss
REASON_HomeImp
VALUE_MISS
clage
clno
delinq
derog
ninq
yoj
;

proc logistic data=loan.traindata descending;
model bad=&input4
  /selection=stepwise fast lackfit rsquare corrb stb;
run;

data loan.val;
  set loan.traindata;
logit=
-1.3884	  
+2.6813  	* DEBTINC_MISS
-0.4928	    * JOB_Office
+0.886	    * JOB_Sales
-2.0645	    * JOB_miss
+4.4174	    * VALUE_MISS
-0.00684	* CLAGE
-0.0109	    * CLNO
+0.6985	    * DELINQ
+0.5923	    * DEROG
+0.1214	    * NINQ
-0.0259	    * YOJ
;
prob4=1/(1+exp(-logit)); run;

/*--------------------------------------------------------------------------------------------------------*/
/* Results -----------------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------*/

proc sort data=loan.val out=loan.val1;
   by descending prob4;
run;

proc rank		data = loan.val1
				out = loan.val_ranked
				groups = 20
				descending;
		var		prob4;
		ranks	rank;
run;

data loan.val_ranked(drop=rank prob4);
set loan.val_ranked;
	model_rank=rank + 1;
	model_score=prob4;
run;

title ' ';

PROC TABULATE DATA = loan.val_ranked MISSING NOSEPS;
            CLASS model_rank        ;
            VAR   model_score     bad  ;
	TABLES model_rank ALL, model_score*MEAN*F=5.3 bad='BAD'*(sum='# of Bad' n='# of Acct' mean*F=5.3);
RUN;

