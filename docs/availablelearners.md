# SuperLearnerMacro

### Available Learners (* denotes SAS Enterprise Miner procedure)

1. #### Learners based on SAS procedures
   **learner**: description (SAS procedure)

   - **back**: backward selection by BIC (HPGENSELECT)
   - ***bagging**: bootstrap aggregation of regression/classification trees (ARBOR)
   - ***bayesnet**: Bayesian network \[binary only\] (HPBNET)
   - **boxcox**: Box-Cox transformation of target variables, positively bound continuous Y only (TRANSREG)
   - ***boost**: Gradient boosting of regression/classification trees (ARBOR)
   - **cvcart**: classification/regression tree with cross validated selection of meta parameters (HPSPLIT)
   - **cart**: classification/regression tree, no cross validation (HPSPLIT)
   - **enet**: elastic net - warning for binary outcome: does not respect [0,1] probability space (GLMSELECT)
   - **gam**: generalized additive model with 3 df splines on all continuous variables (GAM)
   - **gampl**: faster generalized additive model with 3 df splines on all continuous variables (GAMPL)
   - **glm**: linear or logistic regression: slower wrappers for logit and linreg (GENMOD/REG)
   - **knn**: [bernoulli only] k-nearest neighbors classification (DISCRIM)
   - **lar**: least angle regression - warning for binary outcome: does not respect [0,1] probability space (GLMSELECT)
   - **lasso**: least absolute shrinkage and selection operator (HPGENSELECT)
   - **lassob**: LASSO with glmselect [use caution with binary variables] - may be appropriate for older sas versions (GLMSELECT)
   - **lassocv**: LASSO with cross-validated selection of shrinkage parameter (HPGENSELECT)
   - **logit**: main term logistic regression (GENMOD)
   - **linreg**: main term linear regression (REG)
   - **mars**: multivariate adaptive regression splines (ADAPTIVEREG)
   - **mean**: marginal mean of the prediction variable (GENMOD)
   - ***nbayes**: naive Bayes (HPBNET)
   - **bspline**: basis spline regression (TRANSREG)
   - **pbspline**: penalized basis spline regression \[SINGLE CONTINUOUS PREDICTOR ONLY\] (TRANSREG)
   - **probit**: main term probit regression (PROBIT)
   - **ridge**: ridge regression - warning for binary outcome: does not respect [0,1] probability space (REG)
   - **quantreg**: Quantile regression for the median - warning for binary outcome: does not respect [0,1] probability space (QUANTREG)
   - **rf**: random forest (HPFOREST)
   - **rfoob**: random forest, using out of bag predictions and modified selection criteria (HPFOREST)
   - ***nn**: neural network (HPNEURAL)
   - ***sherwood**: random forest, sampling with replacement (ARBOR)
   - **swise**: Stepwise model selection - may be appropriate alternative to lasso for older sas versions (HPGENSELECT)
   
   Also includes multiple learners that are identical to other learners but include all first order interaction terms:
   
   **backint**, **logitint**, **linregint**, **lassoint**, **lassobint**, **lassocvint**, **swiseint**, **larint**, **enetint**, **gamint**, **gamplint**, **marsint**, **probitint**
    
2. #### Learners based on R functions

   A number of included R functions (requires SAS/IML and RLANG system option enabled) allow a limited set of learners in the R programming language. Provided that R is installed and the RLANG option properly enabled, the required packages will be automatically installed the first time the learner is called (if running the SuperLearner or CVSuperLearner macros (internet connection must be active))
   
   - **r_bagging**:  bootstrap aggregation of regression/classification trees (requires ipred package)
   - **r_bart**:  Bayesian additive regression/classification trees (requires dbarts package)
   - **r_boost**:  Gradient boosting regression/classification (requires xgboost package)
   - **r_enet**:  elastic net regression/classification with cross validated selection of shrinkage parameter (requires glmnet package)
   - **r_gam**:  generalized additive model (requires gam package)
   - **r_lasso**:  LASSO regression/classification with cross validated selection of shrinkage parameter (requires glmnet package)
   - **r_mars**: MARS - multivariate adaptive regression splines (requires earth package)
   - **r_polymars**: MARS - multivariate adaptive polynomial regression splines (requires polspline package)
   - **r_rf**: random forest using R superlearner defaults (requires randomForest package)
   - **r_ridge**:  ridge regression/classification with cross validated selection of shrinkage parameter (requires glmnet package)
   - **r_svm**:  support vector machine regression/classification (requires e1071 package)
   
