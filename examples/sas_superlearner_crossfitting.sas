/**********************************************************************************************************************
* Author: Alex Keil
* Program: sas_superlearner_crossfitting.sas
* Date: 20180521
* Project: super learning in the sas system
* Description: coding of some examples using the macro from the paper: 
	Keil, AP. Super Learning in the SAS system. ArXiv. 2018
Below is an example of using super learner along with a cross-fitting algorithm
 to estimate causal effects using "double machine learning" as in
 Chernozhukov, Victor, et al. Double machine learning for treatment and causal parameters. 
 No. CWP49/16. cemmap working paper, Centre for Microdata Methods and Practice, 2016.
 
 This uses a cross-fit algorithm in combination with augmented IPW to estimate treatment effects
* Released under the GNU General Public License: http://www.gnu.org/copyleft/gpl.html
**********************************************************************************************************************/
*clear the log window and the output window;
DM LOG 'clear;' CONTINUE; DM OUT 'clear;' CONTINUE; 
OPTIONS MERGENOBY = warn NODATE NONUMBER LINESIZE = 120  PAGESIZE=80 SKIP = 2 FORMDLIM = '-' MPRINT NOCENTER; TITLE1;TITLE2;
OPTIONS FORMCHAR = '|----|+|---+=|-/\<>*';


* developmental version of super learner macro;
FILENAME slgh URL "https://cirl-unc.github.io/SuperLearnerMacro/super_learner_macro.sas";
%INCLUDE slgh;


  DATA a a1 a2;
  LENGTH id x l 3;
  CALL STREAMINIT(1192887);
  *observed data;
   DO id = 1 TO 5000;
    py0_true = RAND("uniform")*0.1 + 0.4;  
    l = RAND("bernoulli", 1/(1+exp(-1 + py0_true)));
    c = RAND("normal", py0_true, 1);
    c2 = RAND("normal", py0_true, .3);
    x = RAND("bernoulli", 1/(1+exp(-1.5 + 2*l + c + c2)));
    py = py0_true + .1*(x); 
    py1_true = py0_true + .1;
    rd_true = 0.1;
    y = RAND("bernoulli",  py);
    OUTPUT a;
    IF id <= 500 THEN OUTPUT a1;
    IF id > 500 THEN OUTPUT a2;
   END;
  RUN;

* first with just g-computation;
  %SuperLearner(Y=y,
                intvars=x,
                x= x l c c2,
                indata=a, 
                outdata=sl_gc,
                library= logit lasso gampl rf nn mars, 
                method=NNLOGLIK, 
                dist=BERNOULLI 
  );

PROC MEANS DATA = sl_gc NOPRINT;
 CLASS __INT;
 VAR p_sl_full;
 OUTPUT OUT = gf MEAN=py/;
run;
DATA gf (KEEP=py0 py1 rd);
 SET gf END=lobs;
 RETAIN py0 py1 rd;
 IF __int=0 THEN py0 = py;
 IF __int=1 THEN py1 = py;
 rd = py1-py0;
 IF lobs THEN OUTPUT;
RUN;

* now with IPW;
  %SuperLearner(Y=x,
                x= l c c2,
                indata=a, 
                outdata=sl_ipw,
                library= logit lasso gampl rf nn mars, 
                method=NNLOGLIK, 
                dist=BERNOULLI 
  );

DATA sl_ipw;
 SET sl_ipw;
 ps = p_sl_full;
 ipw = x/p_sl_full + (1-x)/(1-p_sl_full);




* now with AIPW;
DATA gc_ (KEEP=ID py0_GF py1_GF py);
 SET sl_gc(keep=id __int p_sl_full);
 BY id;
 RETAIN py0_GF py1_GF py;
 IF __INT=0 then py0_GF = p_sl_full;
 IF __INT=1 then py1_GF = p_sl_full;
 IF __INT<.z then py = p_sl_full;
 IF last.id THEN OUTPUT;
RUN;

DATA AIPW;
 MERGE gc_ sl_ipw;
 BY id;
    mu1_ipw = (y*x/ps);
    mu0_ipw = (y*(1-x)/(1-ps));
    * augmentation terms for AIPW;
    a1_aipw = (x-ps)/ps*py1_gf;
    a0_aipw = ((1-x)-(1-ps))/(1-ps)*py0_gf;
	py1_aipw = mu1_ipw - a1_aipw;
	py0_aipw = mu0_ipw - a0_aipw;
	rd_aipw = py1_aipw - py0_aipw;
RUN;



*using cross-fit to get estimate;
  %SuperLearner(Y=y,
                intvars=x,
                x= x l c c2,
                indata=a1, 
                preddata=a2, 
                outdata=sl_gc1,
                library= logit lasso gampl rf nn mars, 
                method=NNLOGLIK, 
                dist=BERNOULLI 
  );
  %SuperLearner(Y=y,
                intvars=x,
                x= x l c c2,
                indata=a2, 
                preddata=a1, 
                outdata=sl_gc2,
                library= logit lasso gampl rf nn mars, 
                method=NNLOGLIK, 
                dist=BERNOULLI 
  );
  
  %SuperLearner(Y=x,
                x= l c c2,
                indata=a1, 
                preddata=a2, 
                outdata=sl_ipw1,
                library= logit lasso gampl rf nn mars, 
                method=NNLOGLIK, 
                dist=BERNOULLI 
  );
  %SuperLearner(Y=x,
                x= l c c2,
                indata=a2, 
                preddata=a1, 
                outdata=sl_ipw2,
                library= logit lasso gampl rf nn mars, 
                method=NNLOGLIK, 
                dist=BERNOULLI 
  );

DATA sl_gcCF;
 SET sl_gc1(WHERE=(__train=0)) sl_gc2(WHERE=(__train=0));
RUN;
  
PROC MEANS DATA = sl_gcCF NOPRINT;
 CLASS __INT;
 VAR p_sl_full;
 OUTPUT OUT = gfCF MEAN=py/;
run;
DATA gfCF (KEEP=py0 py1 rd);
 SET gfCF END=lobs;
 RETAIN py0 py1 rd;
 IF __int=0 THEN py0 = py;
 IF __int=1 THEN py1 = py;
 rd = py1-py0;
 IF lobs THEN OUTPUT;
RUN;

  
DATA sl_ipwcf;
 SET sl_ipw1(WHERE=(__train=0)) sl_ipw2(WHERE=(__train=0));
 ps = p_sl_full;
 ipw = x/p_sl_full + (1-x)/(1-p_sl_full);




* now with AIPW;
DATA gc_cf (KEEP=ID py0_GF py1_GF py);
 SET sl_gccf(keep=id __int p_sl_full);
 BY id;
 RETAIN py0_GF py1_GF py;
 IF __INT=0 then py0_GF = p_sl_full;
 IF __INT=1 then py1_GF = p_sl_full;
 IF __INT<.z then py = p_sl_full;
 IF last.id THEN OUTPUT;
RUN;

DATA AIPWcf;
 MERGE gc_cf sl_ipwcf;
 BY id;
    mu1_ipw = (y*x/ps);
    mu0_ipw = (y*(1-x)/(1-ps));
    * augmentation terms for AIPW;
    a1_aipw = (x-ps)/ps*py1_gf;
    a0_aipw = ((1-x)-(1-ps))/(1-ps)*py0_gf;
	py1_aipw = mu1_ipw - a1_aipw;
	py0_aipw = mu0_ipw - a0_aipw;
	rd_aipw = py1_aipw - py0_aipw;
RUN;



PROC PRINT DATA = gf;
  TITLE 'G COMPUTATION';
RUN;
PROC GENMOD DATA = sl_ipw DESCENDING;
 TITLE "IPW";
 ODS SELECT geeemppest;
 CLASS ID;
 WEIGHT ipw;
 MODEL y = x / D=B LINK=ID;
 REPEATED SUBJECT=id / TYPE=ind;
run;
PROC MEANS DATA = aipw MEAN;
 TITLE 'AIPW';
 VAR mu0_ipw mu1_ipw py1_aipw py0_aipw rd_aipw;
RUN;
PROC PRINT DATA = gfCF;
  TITLE 'G COMPUTATION, cross fit';
RUN;
PROC GENMOD DATA = sl_ipwcf DESCENDING;
 TITLE "IPW, cross fit";
 ODS SELECT geeemppest;
 CLASS ID;
 WEIGHT ipw;
 MODEL y = x / D=B LINK=ID;
 REPEATED SUBJECT=id / TYPE=ind;
run;
PROC MEANS DATA = aipwcf MEAN;
 TITLE 'AIPW, cross fit';
 VAR py1_aipw py0_aipw rd_aipw;
RUN;

PROC MEANS DATA = a MEAN;
 TITLE 'TRUTH';
 VAR py0_true py1_true rd_true;
RUN;