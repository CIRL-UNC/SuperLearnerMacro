##### Available Learners (* denotes SAS Enterprise Miner procedure)

- **back**: backward selection by BIC
- **bagging**: * bootstrap aggregation of regression/classification trees
- **bayesnet**: * Bayesian network [binary only]
- **boxcox**: Box-Cox transformation of target variables, positively bound continuous Y only
- **boost**: * Gradient boosting of regression/classification trees
- **cvcart**: classification/regression tree with cross validated selection of meta parameters
- **cart**: classification/regression tree, no cross validation
- **enet**: elastic net - warning for binary outcome: does not respect [0,1] probability space 
- **gam**: generalized additive model with 3 df splines on all continuous variables
- **gampl**: faster generalized additive model with 3 df splines on all continuous variables
- **glm**: linear or logistic regression: slower wrappers for logit and linreg
- **knn**: [bernoulli only] k-nearest neighbors classification
- **lar**: least angle regression - warning for binary outcome: does not respect [0,1] probability space
- **lasso**: LASSO
- **lassob**: LASSO with glmselect [use caution with binary variables] - may be appropriate for older sas versions
- **lassocv**: LASSO with cross-validated selection of shrinkage parameter
- **logit**: main term logistic regression
- **linreg**: main term linear regression
- **mars**: multivariate adaptive regression splines
- **mean**: marginal mean of the prediction variable
- **nbayes**: * naive Bayes
- **bspline**: basis spline regression
- **pbspline**: penalized basis spline regression [SINGLE CONTINUOUS PREDICTOR ONLY]
- **probit**: main term probit regression
- **ridge**: ridge regression - warning for binary outcome: does not respect [0,1] probability space
- **quantreg**: Quantile regression for the median - warning for binary outcome: does not respect [0,1] probability space
- **rf**: random forest
- **rfoob**: random forest, using out of bag predictions and modified selection criteria
- **nn**: * neural network
- **sherwood**: * random forest, using proc ARBOR, sampling with replacement
- **swise**: Stepwise model selection with HPGENSELECT - may be appropriate alternative to lasso for older sas versions

Also includes multiple learners that are identical to other learners but include all first order interaction terms:

**backint**, **logitint**, **linregint**, **lassoint**, **lassobint**, **lassocvint**, **swiseint**, **larint**, **enetint**, **gamint**, **gamplint**, **marsint**, **probitint**
  
A number of included R functions (requires SAS/IML and RLANG system option enabled) allow a limited set of learners in the R programming language. Provided that R is installed and the RLANG option properly enabled, the required packages will be automatically installed the first time the learner is called (if running the SuperLearner or CVSuperLearner macros (internet connection must be active))

- **r_bagging**:  bootstrap aggregation of regression/classification trees (requires ipred, rpart packages)
- **r_bart**:  Bayesian additive regression/classification trees (requires dbarts package)
- **r_boost**:  Gradient boosting regression/classification (requires xgboost)
- **r_enet**:  elastic net regression/classification with cross validated selection of shrinkage parameter (requires glmnet package)
- **r_gam**:  generalized additive model (requires gam, foreach, splines packages)
- **r_lasso**:  LASSO regression/classification with cross validated selection of shrinkage parameter (requires glmnet package)
- **r_mars**: MARS - multivariate adaptive regression splines (requires earth package)
- **r_polymars**: MARS - multivariate adaptive polynomial regression splines (requires polspline package)
- **r_rf**: random forest using R superlearner defaults (requires randomForest package)
- **r_ridge**:  ridge regression/classification with cross validated selection of shrinkage parameter (requires glmnet package)
- **r_svm**:  support vector machine regression/classification (requires e1071)

