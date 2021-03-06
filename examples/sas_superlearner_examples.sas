/**********************************************************************************************************************
* Author: Alex Keil
* Program: sas_superlearner_examples.sas
* Date: 20180521
* Project: super learning in the sas system
* Description: coding of some examples using the macro from the paper: 
	Keil, AP. Super Learning in the SAS system. ArXiv. 2018
Below are several macros that demonstrate use of super learner macro
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


* developmental version of super learner macro;
FILENAME slgh URL "https://cirl-unc.github.io/SuperLearnerMacro/super_learner_macro.sas";
%INCLUDE slgh;


/**********************************************************************************************************************
Example 0: basic usage to predict a continuous outcome
**********************************************************************************************************************/

  *0) simulate data, including 5-fold cross validation sets;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO 300;
    py0_true = RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
    py = py0_true + 1*(x); 
    py1_true = py0_true + 1;
    rd_true = 0.1;
    y = RAND("normal", py, 1);
    OUTPUT;
   END;
  RUN;
  
  %SuperLearner(Y=y,
                x= x l c c2,
                indata=a, 
                outdata=sl_testdata,
                library= lasso rf cart,
                folds=5, 
                method=NNLS, 
                dist=GAUSSIAN 
  );
  PROC MEANS DATA = sl_testdata FW=5;
   TITLE 'Predicted outcomes';
   VAR p_: y;
  RUN;


/**********************************************************************************************************************
Example 1: predicted binary outcomes under an intervention
**********************************************************************************************************************/

  *0) simulate data, including 5-fold cross validation sets;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO 300;
    py0_true = RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
    py = py0_true + 0.1*(x); *true risk difference per unit exposure;
    py1_true = py0_true + 0.1;
    rd_true = 0.1;
    y = RAND("bernoulli", py);
    OUTPUT;
   END;
  RUN;
  
  %SuperLearner(Y=y,
                intvars=x,
                x= x l c c2,
                indata=a, 
                outdata=sl_testdata,
                library= logit lasso gampl /*rf nn */, /*uncomment rf and nn if you have sas enterprise miner installed */
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


/**********************************************************************************************************************
Example 2: predicted continuous outcomes under an intervention
**********************************************************************************************************************/


  *0) simulate data, including 5-fold cross validation sets;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO 300;
    py0_true = RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
    py = py0_true + 1*(x); *true risk difference per unit exposure;
    py1_true = py0_true + 1;
    meandiff_true = 1;
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


/**********************************************************************************************************************
Example 3a: creating a new learner (gamma regression)
**********************************************************************************************************************/

  /* template for adding in a custom learner*/
  *0) simulate data, including 5-fold cross validation sets;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO 300;
    py0_true = 10+RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-0.5 + 2*l + (c-10) + (c2-10))));
    py = py0_true + 1*(x); *true risk difference per unit exposure;
    py1_true = py0_true + 1;
    meandiff_true = 1;
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
                binary_predictors= x l,
                continuous_predictors=c c2,
                indata=a, 
                outdata=sl_testdata,
                library= linreg custom gampl, /* note how new learner is used*/
                folds=5, 
                method=CCLS, 
                dist=GAUSSIAN
  );
  PROC MEANS DATA = sl_testdata FW=5;
   TITLE 'Predicted outcomes';
   VAR p_:  y;
  RUN;


/**********************************************************************************************************************
Example 3b: creating a new learner (CART with different tuning parameters)
**********************************************************************************************************************/

  /* template for adding in a custom learner*/
  *0) simulate data, including 5-fold cross validation sets;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO 300;
    py0_true = 10+RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-0.5 + 2*l + (c-10) + (c2-10))));
    py = py0_true + 1*(x); *true risk difference per unit exposure;
    py1_true = py0_true + 1;
    meandiff_true = 1;
    y = RAND("NORMAL", py, 1);
    OUTPUT;
   END;
  RUN;
  
  
  *adding in a custom learner (custom_cn for 'continuous' and custom_in for 'binary');
  * example: regression tree with only 4 OR 12 leaves/terminal nodes;
 %MACRO cart4_cn(
                Y=, indata=, outdata=, binary_predictors=, ordinal_predictors=, 
                nominal_predictors=,  continuous_predictors=, weight=, id=, suff=, seed=
);
  /* regression tree, no cross validation */
  
  &suppresswarn %__SLwarning(%str(This function (CART) is untested below SAS 9.4 TS1M3));
  PROC HPSPLIT DATA = &indata SEED=&seed  CVMETHOD=NONE ;
    FORMAT &Y;
    ODS SELECT NONE;
    %IF &WEIGHT^= %THEN WEIGHT &weight;;
    TARGET &Y / LEVEL = interval;
    %IF (&ordinal_predictors~=) OR (&binary_predictors~=) OR (&nominal_predictors~=) %THEN 
         INPUT &binary_predictors &ordinal_predictors &nominal_predictors / LEVEL = nominal;;
    %IF (&continuous_predictors~=) %THEN 
          INPUT &continuous_predictors / LEVEL = interval;;
    ID _ALL_;
	PRUNE CC (LEAVES=4);
    OUTPUT out=&outdata (drop =  _node_ _leaf_ RENAME=(p_&Y = p_cart4&SUFF));
  RUN;
%MEND cart4_cn;
 %MACRO cart12_cn(
                Y=, indata=, outdata=, binary_predictors=, ordinal_predictors=, 
                nominal_predictors=,  continuous_predictors=, weight=, id=, suff=, seed=
);
  /* regression tree, no cross validation */
  
  &suppresswarn %__SLwarning(%str(This function (CART) is untested below SAS 9.4 TS1M3));
  PROC HPSPLIT DATA = &indata SEED=&seed  CVMETHOD=NONE ;
    FORMAT &Y;
    ODS SELECT NONE;
    %IF &WEIGHT^= %THEN WEIGHT &weight;;
    TARGET &Y / LEVEL = interval;
    %IF (&ordinal_predictors~=) OR (&binary_predictors~=) OR (&nominal_predictors~=) %THEN 
         INPUT &binary_predictors &ordinal_predictors &nominal_predictors / LEVEL = nominal;;
    %IF (&continuous_predictors~=) %THEN 
          INPUT &continuous_predictors / LEVEL = interval;;
    ID _ALL_;
	PRUNE CC (LEAVES=12);
    OUTPUT out=&outdata (drop =  _node_ _leaf_ RENAME=(p_&Y = p_cart12&SUFF));
  RUN;
%MEND cart12_cn;
  
  %SuperLearner(Y=y,
                binary_predictors= x l,
                continuous_predictors=c c2,
                indata=a, 
                outdata=sl_testdata,
                library= linreg gampl cart cart4 cart12, /* note how new learner is used*/
                folds=10, 
                method=CCLS, 
                dist=GAUSSIAN
  );
  PROC MEANS DATA = sl_testdata FW=5;
   TITLE 'Predicted outcomes';
   VAR p_:  y;
  RUN;


/**********************************************************************************************************************
Example 4: making predictions in a validation data set (example from documentation)
**********************************************************************************************************************/


  *0) simulate data, including 5-fold cross validation sets;
     DATA train valid ;
      LENGTH id x l 3;
      CALL STREAMINIT(1192887);
      DO id = 1 TO 2200;
        u = RAND("uniform")*0.1 + 0.4;  
        l = RAND("bernoulli", 1/(1+exp(-1 + u)));
        c = RAND("normal", u, 1);
        c2 = RAND("normal", u, .3);
        x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
        y = RAND("NORMAL", u + x, 0.5);
        KEEP x l c c2 y;
        IF id <= 200 THEN OUTPUT train;
        ELSE OUTPUT valid;
      END;
    RUN;

  TITLE "Super learner predictions in validation data";
  %SuperLearner(Y=y,
                X=x l c c2,
                indata=train, 
                preddata=valid, 
                outdata=sl_testdata,
                library= linreg enet gampl,
                folds=10, 
                method=NNLS, 
                dist=GAUSSIAN 
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
   squarederror_enet = (y - p_enet_full)**2;
   squarederror_gampl = (y - p_gampl_full)**2;
  PROC MEANS DATA = mse FW=5 MEAN;
   TITLE 'Mean squared error of predictions in training/validation data';
   VAR squarederror:;
   CLASS __train;
  RUN;



/**********************************************************************************************************************
Example 5: using CVSuperlearner to get cross-validated expected loss of super learner itself
 Also: Using the more advanced _SuperLearner and _CVSuperLearner functions
 Note that the seed value used can make a difference!
**********************************************************************************************************************/

  *0) simulate data;
  DATA A ;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO 300;
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
  
  TITLE "_superlearner, cvsuperlearner and _cvsuperlearner";
  %_SuperLearner(Y=y,
                 binary_predictors= x l,
                 continuous_predictors= c c2,
                 indata= a , 
                 outdata= sl_cvout ,
                 library=linreg lassoint gampl , 
                 folds= 5, 
                 dist= GAUSSIAN,
                 method=CCLS, 
                 printres=TRUE,
                 seed=123211
				 );
  %CVSuperLearner(Y=y,
                   binary_predictors=  x l,
                   continuous_predictors=  c c2,
                   indata= a , 
                   library=linreg lassoint gampl , 
                   folds= 5, 
                   dist= GAUSSIAN,
                   method=CCLS /*default seed is 12345*/
				   );
  %_CVSuperLearner(Y=y,
                   binary_predictors=  x l,
                   continuous_predictors=  c c2,
                   indata= a , 
                   library=linreg lassoint gampl , 
                   slfolds= 5, 
                   cvslfolds= 5, 
                   dist= GAUSSIAN,
                   method=CCLS,
                   seed=123211
				   );
