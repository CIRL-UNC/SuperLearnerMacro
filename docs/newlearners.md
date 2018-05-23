# SuperLearnerMacro

### Creating new learners
All sas macros to enable a learner in the library are of the form:

    %learner_in(Y,indata,outdata, binary_predictors,ordinal_predictors,nominal_predictors,
    continuous_predictors,weight,suff,seed);

 Using this standard call, it is relatively straightforward to add new learners, provided that the conventions are followed.
Conventions:

1. each macro name must be structured like binary dep. vars:     %[library name]\_in  non-binary dep. vars: %[library name]\_cn where [library name] is the user defined name for a given learner. E.g. for a  random forest, [library name] is 'rf' (without quotes)

2. macro call/parameters must follow exact naming conventions as shown above in '%learner_in' example

3. the sample space of each predictor must be respected (e.g. &binary\_predictors should be used where binary predictors are appropriate for the learner.) For example,  model statements in, PROC genmod will contain all &..._predictor macro variables, but  &nominal\_predictors could be included in a CLASS statement, for example.

4. outdata must contain: all variables from indata dataset (the data used in the learner) PLUS a variable that follows the naming convention: p_[library name]&SUFF that is either a) predicted probability (binary dep var) or b) predicted value (non- binary dep var)

5. to include all interaction terms between predictors, the predictors must include  &SLIXterms, which may be a mix of discrete and continuous variables

Example: creating a learner that fits a generalized linear model with a log link and under a gamma error distribution

     %MACRO custom_cn(Y=,indata=, outdata=, binary_predictors=, ordinal_predictors=,nominal_predictors=,continuous_predictors=,weight=,suff=,seed=);  
      PROC GENMOD DATA = &indata;
      %IF ((&binary_predictors~=) OR (&ordinal_predictors~=) OR (&nominal_predictors~=)) %THEN CLASS &binary_predictors &ordinal_predictors &nominal_predictors ;;
       MODEL &Y = &binary_predictors &ordinal_predictors &nominal_predictors &continuous_predictors / LINK=LOG D=GAMMA;
       OUTPUT OUT = &OUTDATA PRED=p_custom&SUFF; /*change predicted value name with p_[libraryname]&suff*/
      RUN;
    %MEND custom_cn; /*optional: include macro name in mend statement with [libraryname]_cn*/

Custom leaners must have the same structure of the macro call, but the macro need not actually use the arguments. For example, rather than using **[coding]_predictors** arguments, one could hard code the model predictors, including interaction terms. Thus, one could create multiple learners with different covariate sets as a way to select from a number of possible parameterizations of the same model, but with different covariates (i.e. one could select the model with the lowest loss function, or take the model average based on the super learner fit).

