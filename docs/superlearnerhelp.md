### Usage details
The sas script containing the SuperLearner macro actually contains 4 main macros: %SuperLearner, %\_SuperLearner, %CVSuperLearner macro, and %\_CVSuperLearner

#### 1. %SuperLearner macro

Stacking is based on what Wolpert refers to as a set of 'level-0' models and a 'level-1' model, indexed by parameters ![equation](http://latex.codecogs.com/gif.latex?%5Cmathbf%7B%5Cbeta%7D%5Fm) and ![equation](http://latex.codecogs.com/gif.latex?%5Cmathbf%7B%5Calpha%7D) in some study sample *S*. Where



Level-0: ![equation](http://latex.codecogs.com/gif.latex?%5Chat%7BY%7D%5F%7Bm%7D%3Df%5Fm%28%5Cmathbf%7Bx%7D%3B%5Cmathbf%7B%5Cbeta%7D%5Fm%2CS%29%5Cmbox%7B%20for%20%7Dm%5Cin1%2C%5Cldots%2CM)


Level-1: ![equation](http://latex.codecogs.com/gif.latex?%5Chat%7BY%7D%5F%7Bsl%7D%3Df%5F%7Bsl%7D%28%5Chat%7B%5Cmathbf%7BY%7D%7D%5F%7B%5Cbar%7Bm%7D%7D%3B%5Cmathbf%7B%5Calpha%7D%2CS%29)

The parameterization of the macro is based loosely on this notation. Macro parameters include the following:

- **Y**: [value = variable name] the target variable, or outcome

- **X**: [value =   blank, or a space separated list of variable names] predictors of **Y** on the right side of the level-0 models. Note that this is a convenience function for the individual **[coding]_predictors** macro variables. The macro will make a guess at whether each predictor in **X** is continuous, categorical, or binary. (OPTIONAL but at least one of the **X** or **[coding]_predictors** - **binary_predictors**, **ordinal_predictors**, **nominal_predictors**, **continuous_predictors** - parameters must be specified, as described below). If **X** is specified and any one of the **[coding]_predictors** has a value, the macro will generate an error.

- **library**: [value =  a space separated list of learners] the names of the *m* level-0 models (e.g. glm lasso cart). A single learner can be used here if you only wish to know the cross-validated expected loss (e.g. mean-squared error). See [all available default learners here](docs/availablelearners.html "Available learners") and [how to construct new learners here](docs/newlearners.html "Custom learners")

- **indata**: [value = an existing dataset name] the dataset used for analysis that contains *Y* and all predictors (and weight variables, if needed)

- **preddata**: [OPTIONAL value = a dataset name] the validation dataset. A dataset which contains all predictors and possibly *Y* that is not used in model fitting but predictions for each learner and superlearner are made in these data

- **outdata**: [value = a dataset name; default: sl_out] an output dataset that will contain all predictions as well as all variables and observations in the **indata** and **preddata** datasets

- **dist**: [value = one of: GAUSSIAN,BERNOULLI; default GAUSSIAN] Super learner can be used to make predictions of a continuous (assumed gaussian in some learners) or a binary variable. Use GAUSSIAN for all continuous variables and BERNOULLI for all binary variables. Nominal/categorical variables currently not supported.

- **method**:[value = one of: NNLS,NNLOGLIK,CCLOGLIK,LOGLIK,NNLS,OLS,CCLAE,NNLAE,LAE; default NNLS] the method used to estimate the ![equation](http://latex.codecogs.com/gif.latex?%5Cmathbf%7B%5Calpha%7D) coefficients of the level-1 model.                    Methods are possibly indexed by prefixes: NN, CC, [none], where 

   NN implies non-negative coefficients that are standardized after fitting to sum to 1. 

   CC implies a convexity constraint where the super learner fit is subject to a constraint 
  that forces the coefficients to fall in [0,1] and sum to 1.0. No prefix implies no 
  constraints (which results in some loss of asymptotic properties such as the oracle property).
   Note: OLS violates this naming convention, but LS will also be accepted and is equivalent to OLS

   LS methods use an L2 loss function (least squares)
   LOGLIK methods use a loss function corresponding to the binomial likelihood with a logit link function
   LAE methods [experimental] use an L1 loss function (least absolute error), which will not penalize outliers as much as L2 methods, and is also non-differentiable at the minimum
  which may cause computational difficulties


- **by**: [OPTIONAL value = variable name] a by variable in the usual SAS usage. Separate super learner fits will be specified for each level of the by variable (only one allowed, unlike typical ``by'' variables. 
- **intvars**:[OPTIONAL value = variable name] an intervention variable that is included in the list of predictors. This is a convenience function that will make separate predictions for the intvars variable at 1 or 0 (with all other predictors remaining at their observed levels)
- **binary_predictors**: [value =  blank, or a space separated list of variable names] advanced specification of predictors: a space separated list of binary predictors (OPTIONAL but at least one of the **X** or **[coding]_predictors** parameters must be specified)
- **ordinal_predictors**: [value =  blank, or a space separated list of variable names]advanced specification of predictors: a space separated list of ordinal predictors (OPTIONAL but at least one of the **X** or **[coding]_predictors** parameters must be specified)
- **nominal_predictors**: [value =  blank, or a space separated list of variable names]advanced specification of predictors: a space separated list of nominal predictors (OPTIONAL but at least one of the **X** or **[coding]_predictors** parameters must be specified)
- **continuous_predictors**: [value =  blank, or a space separated list of variable names] advanced specification of predictors: a space separated list of continuous predictors (OPTIONAL but at least one of the **X** or **[coding]_predictors** parameters must be specified)
- **weight**: [OPTIONAL value = a variable name] a variable containing weights representing the relative contribution of each observation to the fit (a.k.a. case weights). Not all learners will respect non-integer weights, so weights will either be ignored or truncated by some procedures.
- **trtstrat**: [value  = true, false; DEFAULT: false] convenience function. If this is set to true and **intvars** is specified, then all fits will be stratified by levels of **intvars** (0,1) accepted only.
- **folds**: [value  = integer ; default: 10] number of cross-validation folds to use

#### 2. %\_SuperLearner macro
This is a less user-friendly version of the %SuperLearner macro that may be somewhat faster due to reduced error checking, and offers finer level controls. If the %SuperLearner macro completes successfully, it will give some example code that can be run with %_SuperLearner. Of note, there is no checking of parameter syntax, so the case-sensitive parameter arguments may cause an error in %\_SuperLearner, but not %SuperLearner.

One main difference is that %\_SuperLearner will make no guesses about variable types for **X**, so use of the **[coding]_predictors** is required for correct specification.

#### 3. %CVSuperLearner macro
This macro is used to estimate the cross-validated expected loss of super learner itself. It does not produce predictions! This gives an idea about whether super learner is the appropriate learner to use in a given scenario, and allows some choice between parameters of the the super learner model, such as the method (e.g. NNLS vs. CCLS).

- **folds**:[value = integer; default: 10] specifies two quantities (which can be individually specified in the %_CVSuperLearner macro):
 1. **slfolds** number of ''inner folds'' (number of folds within each super learner fit) should only be different from **cvslfolds** in odd cases
 2.  **cvslfolds**: number of ''outer folds'' (the number of folds for cross-validating super learner) should only be different from **slfolds** in odd cases

Options repeated from %SuperLearner

- **Y**: see %SuperLearner macro definition
- **X**: see %SuperLearner macro definition
- **by**: see %SuperLearner macro definition
- **binary_predictors**:  see %SuperLearner macro definition
- **ordinal_predictors**:  see %SuperLearner macro definition
- **nominal_predictors**: see %SuperLearner macro definition 
- **continuous_predictors**:  see %SuperLearner macro definition
- **weight**: see %SuperLearner macro definition
- **indata**: see %SuperLearner macro definition
- **dist**: see %SuperLearner macro definition (default: GAUSSIAN)
- **library**:  see %SuperLearner macro definition 
- **method**:  see %SuperLearner macro definition (default: NNLS)


#### 4. %\_CVSuperLearner macro
This is a less user-friendly version of the %CVSuperLearner macro that may be somewhat faster due to reduced error checking, and offers finer level controls. See the source code for further tuning options.


#### Further reading
##### About this macro
1. A. P. Keil. Super Learning in the SAS system. ArXiv e-prints, May 2018. [https://arxiv.org/abs/1805.08058](https://arxiv.org/abs/1805.08058)

##### About stacking
1. D. H. Wolpert. Stacked generalization. Neural networks, 5(2):241–259, 1992.

2. L. Breiman. Stacked regressions. Machine learning, 24(1):49–64, 1996.


##### About super learner
1. M. J. van der Laan, E. C. Polley, and A. E. Hubbard. Super learner. Report, Division of Biostatistics, University of California, Berkeley, 2007.

2. E. C. Polley and M. J. van der Laan. Super learner in prediction. Report, Division of Biostatistics, University of California, Berkeley, 2010.

#### Acknowledgements
This work would not have been possible without valuable advice and beta testing from the following people: Jessie K Edwards, Katie M O'Brien, Stephen R Cole, and many others
