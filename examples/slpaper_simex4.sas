/*
Simplified version of simulation example 4 from:
 A. P. Keil. Super Learning in the SAS system. ArXiv e-prints, May 2018. https://arxiv.org/abs/1805.08058
 
Adapted from :
 E. C. Polley and M. J. van der Laan. Super learner in prediction. Report, Division of Biostatistics, University of California, Berkeley, 2010.
 
Note that SAS Enterprise Miner High Performance Procedures must be enabled to run this analysis
*/
FILENAME slgh URL "https://cirl-unc.github.io/SuperLearnerMacro/super_learner_macro.sas";
%INCLUDE slgh;

* extra learners;
FILENAME el URL "https://cirl-unc.github.io/SuperLearnerMacro/extralearners/sas_superlearner_extra_learners.sas";
%INCLUDE el;

DATA train(DROP=i my) test(DROP=i my);
 CALL STREAMINIT(1229);
  DO i = 1 to 10100;
   x = rand('uniform')*8-4;
   my = 4*sin(3*CONSTANT('PI')*x) * (x>0);
   y = my + RAND('gaussian');
   IF i<=100 THEN OUTPUT train;
   ELSE OUTPUT test;
  END;
 RUN;
 
%_SuperLearner(y=y,
              continuous_predictors=x,
              indata=train, 
              preddata=test, 
              outdata=sl_outdata,
              library= linreg linregint rf bagging10 bagging01 bagging00 baggingms5 gam2 gam3 gam4 boost nn2 nn3 nn4 nn5 mars loess75 loess50 loess25 loess10,
              folds=10, 
              method=NNLS,
              dist=GAUSSIAN,
              quietwarning=TRUE, /* suppress some warnings being printed to the log */
              quietnotes=TRUE, /* suppress notes being printed to the log */
              timer=FALSE, 
			  printres=TRUE
); 

