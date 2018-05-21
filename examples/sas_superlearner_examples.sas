/**********************************************************************************************************************
* Author: Alex Keil
* Program: sas_superlearner_examples.sas
* Date: 20180521
* Project: super learning in the sas system
* Description: coding of some examples using the macro from the paper: 
	Keil, AP. Super Learning in the SAS system. ArXiv. 2018
Below are three macros that carry out super learner prediction
  test_bernoulli_int: simple confounding problem - predicting potential outcomes
  test_gaussian_int: example with continuous variable
  test_slnewlearner: example demonstrating how to introduce a new/custom learner
  test_slexternalpreds: example demonstrating how make predictions in a validation dataset
  test_cvsl: example demonstrating estimation of cross-validated super learner risk

* Released under the GNU General Public License: http://www.gnu.org/copyleft/gpl.html
**********************************************************************************************************************/
*clear the log window and the output window;
DM LOG 'clear;' CONTINUE; DM OUT 'clear;' CONTINUE; 
OPTIONS MERGENOBY = warn NODATE NONUMBER LINESIZE = 120  PAGESIZE=80 SKIP = 2 FORMDLIM = '-' MPRINT NOCENTER; TITLE1;TITLE2;
OPTIONS FORMCHAR = '|----|+|---+=|-/\<>*';


%MACRO test_bernoulli_int(n=300, trueRD = .1);
  *0) simulate data, including 5-fold cross validation sets;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO &N;
    py0_true = RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
    py = py0_true + &trueRD*(x); *true risk difference per unit exposure;
    py1_true = py0_true + &trueRD;
    rd_true = &truerd;
    y = RAND("bernoulli", py);
    OUTPUT;
   END;
  RUN;
  
  %SuperLearner(Y=y,
                intvars=x,
                binary_predictors= x l,
                ordinal_predictors=,
                nominal_predictors=,
                continuous_predictors=c c2,
                indata=a, 
                outdata=sl_testdata,
                library= logit rf lasso nn gampl,
  			    trtstrat=true, 
                folds=5, 
                method=NNLOGLIK, 
                dist=BERNOULLI 
  );
  PROC MEANS DATA = sl_testdata FW=5;
   TITLE 'Predicted outcomes';
   TITLE2 'no intervention (int=-1), never treatment (int=0), always treat (int=1)';
   VAR p_: py0_: py1_: py y;
   CLASS __int;
  RUN;
%MEND test_bernoulli_int;

%MACRO test_gaussian_int(n=300, trueMEANDIFF = 1);
  *0) simulate data, including 5-fold cross validation sets;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO &N;
    py0_true = RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
    py = py0_true + &trueMEANDIFF*(x); *true risk difference per unit exposure;
    py1_true = py0_true + &trueMEANDIFF;
    meandiff_true = &trueMEANDIFF;
    y = RAND("NORMAL", py, 1);
    OUTPUT;
   END;
  RUN;
  
  %SuperLearner(Y=y,
                intvars=x,
                binary_predictors= x l,
                continuous_predictors=c c2,
                indata=a, 
                outdata=sl_testdata,
                library= linreg lasso gampl,
  			    trtstrat=true, 
                folds=5, 
                method=CCLS, 
                dist=GAUSSIAN /*GAUSSIAN OR BERNOULLI*/
  );
  PROC MEANS DATA = sl_testdata FW=5;
   TITLE 'Predicted outcomes';
   TITLE2 'no intervention (__int=.o), never treatment (__int=0), always treat (__int=1)';
   VAR p_: py0_: py1_: py y;
   CLASS __int;
  RUN;
%MEND test_gaussian_int;

%MACRO test_slnewlearner(n=300, trueMEANDIFF = 1);
  /* template for adding in a custom learner*/
  *0) simulate data, including 5-fold cross validation sets;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO &N;
    py0_true = 10+RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
    py = py0_true + &trueMEANDIFF*(x); *true risk difference per unit exposure;
    py1_true = py0_true + &trueMEANDIFF;
    meandiff_true = &trueMEANDIFF;
    y = RAND("NORMAL", py, 1);
    OUTPUT;
   END;
  RUN;
  
  
  *adding in a custom learner (custom_cn for 'continuous' and custom_in for 'binary');
  * example: new link function with gamma distribution;
  %MACRO custom_cn(/*change macro name with [libraryname]_cn*/
                  Y=,indata=, outdata=, binary_predictors=, ordinal_predictors=, 
                  nominal_predictors=,  continuous_predictors=,weight=,suff=,seed=
  );
    /* gamma model */
    PROC GENMOD DATA = &indata;
    %IF ((&binary_predictors~=) OR (&ordinal_predictors~=) OR (&nominal_predictors~=)) %THEN CLASS &binary_predictors &ordinal_predictors &nominal_predictors ;;
     MODEL &Y = &binary_predictors &ordinal_predictors &nominal_predictors &continuous_predictors / LINK=LOG D=GAMMA;
     OUTPUT OUT = &OUTDATA PRED=p_custom&SUFF; /*change predicted value name with p_[libraryname]&suff*/
    RUN;
  %MEND custom_cn; /*optional: include macro name in mend statement with [libraryname]_cn*/
  
  %SuperLearner(Y=y,
                intvars=x,
                binary_predictors= x l,
                continuous_predictors=c c2,
                indata=a, 
                outdata=sl_testdata,
                library= linreg custom, /* note how new learner is used*/
                trtstrat=true, 
                folds=5, 
                method=CCLS, /*L2 ENTROPY*/
                dist=GAUSSIAN /*GAUSSIAN OR BERNOULLI*/
  );
  PROC MEANS DATA = sl_testdata FW=5;
   TITLE 'Predicted outcomes';
   TITLE2 'no intervention (int=-1), never treatment (int=0), always treat (int=1)';
   VAR p_: py0_: py1_: py y;
   CLASS __int;
  RUN;
%MEND test_slnewlearner;

%MACRO test_slexternalpreds(ntrain=300,nvalid=1000);
  *0) simulate data, including 5-fold cross validation sets;
  DATA train valid ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO %EVAL(&ntrain+&nvalid);
    py0_true = RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
    py = py0_true + 1*(x); *true risk difference per unit exposure;
    y = RAND("NORMAL", py, 0.5);
    KEEP x l c c2 y;
    IF id <= &ntrain THEN OUTPUT train;
    ELSE OUTPUT valid;
   END;
  RUN;
  TITLE "Super learner predictions in validation data";
  %SuperLearner(Y=y,
                X=x l c c2,
                indata=train, 
                preddata=valid, 
                outdata=sl_testdata,
                library= linreg lasso gampl,
                folds=10, 
                method=NNLS, 
                dist=GAUSSIAN /*GAUSSIAN OR BERNOULLI*/
  );
  PROC MEANS DATA = sl_testdata FW=5;
   TITLE 'Predicted outcomes in training/validation data';
   VAR p: y;
   CLASS __train;
  RUN;
  DATA mse(KEEP=__train squarederror:);
   SET sl_testdata;
   squarederror_sl = (y - p_SL_full)**2;
   squarederror_linreg = (y - p_linreg_full)**2;
   squarederror_lasso = (y - p_lasso_full)**2;
   squarederror_gampl = (y - p_gampl_full)**2;
  PROC MEANS DATA = mse FW=5 MEAN;
   TITLE 'Mean squared error of predictions in training/validation data';
   VAR squarederror:;
   CLASS __train;
  RUN;
%MEND test_slexternalpreds;

%MACRO test_cvsl(n=300);
  *0) simulate data;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO &N;
    py0_true = RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true + 0.1*py0_true**2, 1);
    c2 = RAND("normal", py0_true - 0.1*py0_true**2, .3);
    x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
    py = py0_true + 1*(x) + .3*c*x - .3*c2*x + .3*c*c - .3*c2*c2; *true risk difference per unit exposure;
    y = RAND("NORMAL", py, 1);
    OUTPUT;
   END;
  RUN;
  
  %_SuperLearner(Y=y,
                 binary_predictors= x l,
                 continuous_predictors= c c2,
                 indata= a , 
                 outdata= sl_cvout ,
                 library=linreg lassoint gampl , 
                 folds= 10, 
                 dist= GAUSSIAN,
                 method=CCLS, 
                 printres=TRUE
				 );
  %_CVSuperLearner(Y=y,
                   binary_predictors=  x l,
                   continuous_predictors=  c c2,
                   indata= a , 
                   outdata= sl_cvout ,
                   library=linreg lassoint gampl , 
                   slfolds= 10, 
                   cvslfolds=10,
                   dist= GAUSSIAN,
                   method=CCLS,
				   cleanup=TRUE,
				   printfoldres=FALSE,
				   quietnotes=TRUE,
				   logreport=FALSE
				   );
%MEND test_cvsl;
OPTIONS NOMPRINT;

* uncomment any of these to show examples;
*%test_bernoulli_int(n=300, trueRD = .1);
*%test_gaussian_int(n=300, trueMEANDIFF = 1);
*%test_slnewlearner(n=300, trueMEANDIFF = 1);
*%test_slexternalpreds(ntrain=300, nvalid=1000);
*%test_cvsl(n=300);