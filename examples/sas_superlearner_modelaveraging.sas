/**********************************************************************************************************************
* Author: Alex Keil
* Program: sas_superlearner_modelaveraging.sas
* Date: 20180521
* Project: super learning in the sas system
* Description: coding of some examples using the macro from the paper: 
	Keil, AP. Super Learning in the SAS system. ArXiv. 2018
Below is an example of using super learner to perform model averaging over a set of 
different covariate specifications. This can be used as a way to select the model
that minimizes the cross-validated loss over the set of models, or as a way
to develop an ensemble predictive model that allows uncertainty in model form

* Released under the GNU General Public License: http://www.gnu.org/copyleft/gpl.html
**********************************************************************************************************************/
*clear the log window and the output window;
DM LOG 'clear;' CONTINUE; DM OUT 'clear;' CONTINUE; 
OPTIONS MERGENOBY = warn NODATE NONUMBER LINESIZE = 120  PAGESIZE=80 SKIP = 2 FORMDLIM = '-' MPRINT NOCENTER; TITLE1;TITLE2;
OPTIONS FORMCHAR = '|----|+|---+=|-/\<>*';


* developmental version of super learner macro;
FILENAME slgh URL "https://cirl-unc.github.io/SuperLearnerMacro/super_learner_macro.sas";
%INCLUDE slgh;


/*step 0) helper functions */
OPTIONS CMPLIB = work.__funcs;
PROC FCMP OUTLIB = work.__funcs.MYEXPIT;
FUNCTION MYEXPIT(mu);
 mur = mu;
 IF mu<-700 THEN mur=-700;
 IF mu>700 THEN mur=700;
 lp = 1/(1+exp(-mur));
 RETURN(lp);
ENDSUB;
FUNCTION MYLOGIT(p);
 pr = p;
 IF p <= 0 THEN pr = 1e-12;
 IF p >= 1 THEN pr = 1-(1e-12);
 lmu = LOG(pr/(1-pr));
 RETURN(lmu);
ENDSUB;
RUN;

/* step 1) data generation*/
DATA train(DROP=i j) ;
 CALL STREAMINIT(1200120);
 * coefficients;
  ARRAY beta[7];
  ARRAY alpha[7];
  ARRAY V[10];
  beta0 = 0;
  *alpha0 = -3.85; 
  *alpha0 = 0; *causes massive errors;
  alpha0 = -2; 
  gamma = -0.4;  
  j=1;
  DO i = 0.8, -0.25, 0.6, -0.4, -0.8, -0.5, 0.7;
   beta[j] = i;
   j = j+1;
  END;
  j=1;
  DO i = 0.3, -0.36, -0.73, -0.2, 0.71, -0.19, 0.26;
   alpha[j] = i;
   j = j+1;
  END;
  DROP alpha: beta: v:;
  *simulation loop;
  *DO test = 0, 1; 
  test = 0; 
   DO cohort = 1 TO 1;
    DO id = 1 TO 1000; 
    * coefficients in outcome model;
     DO i = 1 TO 10;
         v[i] = RAND('NORMAL', 0, 1);
     END;
     w1 = (v[1]>0)*1.0; * binary;
     w2 = v[2];
     w3 = (v[3]>0)*1.0; * binary;
     w4 = v[4];
     w5 = ((0.2*v[1] + sqrt(1-0.2**2)*v[5])>0)*1.0; * binary;	
     w6 = ((0.9*v[2] + sqrt(1-0.9**2)*v[6])>0)*1.0; * binary;
     w7 = v[7];
     
     w8 = ((0.2*v[3] + sqrt(1-0.2**2)*v[8])>0)*1.0; * binary;
     w9 = ((0.9*v[4] + sqrt(1-0.9**2)*v[9])>0)*1.0; * binary;
     w10 = v[10];
        
     ps = MYEXPIT( beta0 + beta[1]*w1 + beta[2]*w2 + beta[3]*w3 + beta[4]*w4 + 
        beta[5]*w5 + beta[6]*w6 + beta[7]*w7 + beta[2]*w2**2 + beta[4]*w4**2 + beta[7]*w7**2 + 
        beta[1]*0.5*w1*w3 + beta[2]*0.7*w2*w4 + beta[3]*0.5*w3*w5 + beta[4]*0.7*w4*w6 + 
        beta[5]*0.5*w5*w7 + beta[1]*w1*w6 + beta[2]*0.7*w2*w3 + beta[3]*0.5*w3*w4 + 
        beta[4]*0.5*w4*w5 + beta[5]*0.5*w5*w6);
     r0 = alpha0 + alpha[1]*w1 + alpha[2]*w2 + alpha[3]*w3 + alpha[4]*w4 + alpha[5]*w8 + alpha[6]*w9 + alpha[7]*w10 + 
        0.3*alpha[1]*w1*w2 - 0.3*alpha[3]*w3*w4 + 
        0.2*alpha[4]*w3*w4*w9 - 0.2*alpha[6]*w1*w2*w10
        - 0.4*alpha[2]*w2**2 - 0.8*alpha[4]*w4**2 + 0.4*alpha[7]*w10**2; 
     r1 = r0 + 1*gamma + 
        -0.2*alpha[1]*w1 + 0.4*alpha[2]*w2 + 0.4*alpha[3]*w4 - 0.4*alpha[4]*w8 + 0.2*alpha[5]*w9 +
        alpha[4]*w1*w4*w10 - .5*alpha[6]*w3*w4*w9;

     a = RAND('BERNOULLI', ps);
     IF a=1 THEN mu_y = r1;
     ELSE IF a=0 THEN mu_y = r0;
     y = RAND('NORMAL', mu_y, 1);
     md_true=r1-r0;
     OUTPUT ;
   END; *id;
  END; *cohort;
RUN;

 /* CREATE A 'TEMPLATE'*/
%MACRO mtempl_cn(
                Y=,indata=, outdata=, binary_predictors=, ordinal_predictors=, 
                nominal_predictors=,  continuous_predictors=,weight=,suff=,seed=,nm=,
                fx=
);
  /* linear model */
  PROC GENMOD DATA = &indata;
    FORMAT &Y;
    ODS SELECT NONE;
    %IF ((&binary_predictors~=) OR (&ordinal_predictors~=) OR (&nominal_predictors~=)) %THEN CLASS &binary_predictors &ordinal_predictors &nominal_predictors ;;
    MODEL &Y = &fx / LINK=ID D=NORMAL;
    OUTPUT OUT = &OUTDATA PRED=p_&nm&SUFF;
  RUN;
%MEND mtempl_cn;

/* COMPARE 5 MODELS*/
%MACRO M1_CN(Y=,indata=, outdata=, binary_predictors=, ordinal_predictors=,nominal_predictors=,  continuous_predictors=,weight=,suff=,seed=);
%mtempl_cn(Y=&Y,indata=&indata, outdata=&outdata, binary_predictors=&binary_predictors, ordinal_predictors=&ordinal_predictors,
            nominal_predictors=&nominal_predictors, continuous_predictors=&continuous_predictors,weight=&weight,suff=&suff,seed=&seed,
                nm=m1,fx=%STR(a w1 w2 w3 w4 w8 w9 w10));
%MEND;

%MACRO M2_CN(Y=,indata=, outdata=, binary_predictors=, ordinal_predictors=,nominal_predictors=,  continuous_predictors=,weight=,suff=,seed=);
%mtempl_cn(Y=&Y,indata=&indata, outdata=&outdata, binary_predictors=&binary_predictors, ordinal_predictors=&ordinal_predictors,
            nominal_predictors=&nominal_predictors,  continuous_predictors=&continuous_predictors,weight=&weight,suff=&suff,seed=&seed,
                nm=m2,fx=%STR(a w1 w2 w3 w4 w5 w6 w7 w8 w9 w10));
%MEND;

%MACRO M3_CN(Y=,indata=, outdata=, binary_predictors=, ordinal_predictors=,nominal_predictors=,  continuous_predictors=,weight=,suff=,seed=);
%mtempl_cn(Y=&Y,indata=&indata, outdata=&outdata, binary_predictors=&binary_predictors, ordinal_predictors=&ordinal_predictors,
            nominal_predictors=&nominal_predictors,  continuous_predictors=&continuous_predictors,weight=&weight,suff=&suff,seed=&seed,
                nm=m3,fx=%STR(a w1 w2 w3 w4 w8 w9 w10 a*w1 a*w2 a*w3 a*w4 a*w8 a*w9 a*w10));
%MEND;

%MACRO M4_CN(Y=,indata=, outdata=, binary_predictors=, ordinal_predictors=,nominal_predictors=,  continuous_predictors=,weight=,suff=,seed=);
%mtempl_cn(Y=&Y,indata=&indata, outdata=&outdata, binary_predictors=&binary_predictors, ordinal_predictors=&ordinal_predictors,
            nominal_predictors=&nominal_predictors,  continuous_predictors=&continuous_predictors,weight=&weight,suff=&suff,seed=&seed,
                nm=m4,fx=%STR(a w1 w2 w3 w4 w8 w9 w10 w2*w2 w4*w4 w10*w10));
%MEND;

%MACRO M5_CN(Y=,indata=, outdata=, binary_predictors=, ordinal_predictors=,nominal_predictors=,  continuous_predictors=,weight=,suff=,seed=);
%mtempl_cn(Y=&Y,indata=&indata, outdata=&outdata, binary_predictors=&binary_predictors, ordinal_predictors=&ordinal_predictors,
            nominal_predictors=&nominal_predictors,  continuous_predictors=&continuous_predictors,weight=&weight,suff=&suff,seed=&seed,
                nm=m5,fx=%STR(a w1 w2 w3 w4 w8 w9 w10 a*w1 a*w2 a*w3 a*w4 a*w8 a*w9 a*w10 w2*w2 w4*w4 w10*w10));
%MEND;

  %SuperLearner(Y=y,
                x= a w1 w2 w3 w4 w8 w9 w10,
                indata=train, 
                outdata=sl_mavg,
                library= m1 m2 m3 m4 m5, 
                method=CCRIDGE, 
                dist=GAUSSIAN 
  );

/*
Learner    Coefficient     CVrisk

  m1         0.00004      1.12704
  m2         0.00002      1.13478
  m3         0.03083      1.10204
  m4         0.23196      1.01736
  m5         0.73715      1.00040
*/

 * getting cross validated loss for super learner;
%CVSuperLearner(Y=y,
                x= a w1 w2 w3 w4 w8 w9 w10,
                indata=train, 
                library= m1 m2 m3 m4 m5, 
                method=CCRIDGE, 
                dist=GAUSSIAN 
  );

  /*
                                    coef_     CVrisk_     CVrisk_    CVrisk_    CVrisk_
Obs    learner                     mean       mean      stderr       min        max

 1     super learner              .         0.99985    0.035909    0.82995    1.16367
 2     discrete super learner     .         1.00040    0.033945    0.84151    1.14656
 3     m1                        0.00536    1.12704    0.040399    0.96500    1.33944
 4     m2                        0.00001    1.13478    0.039217    0.97421    1.33571
 5     m3                        0.03340    1.10204    0.036406    0.96404    1.32704
 6     m4                        0.37259    1.01736    0.039561    0.83131    1.21561
 7     m5                        0.58865    1.00040    0.033945    0.84151    1.14656
*/

  /* results: model averaging results in better cross-validate fit than simply taking the best fitting model*/


* repeating the estimate of super learner firt with un-regularized method;
  %CVSuperLearner(Y=y,
                x= a w1 w2 w3 w4 w8 w9 w10,
                indata=train, 
                library= m1 m2 m3 m4 m5, 
                method=NNLS, 
                dist=GAUSSIAN 
  );

* results appear robust across different ensembling methods;
 /*
                                  coef_     CVrisk_     CVrisk_    CVrisk_    CVrisk_
Obs    learner                     mean       mean      stderr       min        max

 1     super learner              .         0.99984    0.035900    0.83009    1.16369
 2     discrete super learner     .         1.00040    0.033945    0.84151    1.14656
 3     m1                        0.00551    1.12704    0.040399    0.96500    1.33944
 4     m2                        0.00002    1.13478    0.039217    0.97421    1.33571
 5     m3                        0.02948    1.10204    0.036406    0.96404    1.32704
 6     m4                        0.36832    1.01736    0.039561    0.83131    1.21561
 7     m5                        0.59668    1.00040    0.033945    0.84151    1.14656
*/