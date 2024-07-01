# missForestPredict comparison

## How to run and get results for a new dataset 

You can add datasets and visualize results in a similar manner as in the accompanying study.

1. Clone or download the project's repository and setup the R project on your machine.

2. Place your dataset(s) in the **data** directory.

3. Copy the **run_template.R** file in the  **runs_on_datasets_custom** directory and give it a meaningful name (e.g. run_datasetname)

4. Modify the sections tagged with **MODIFY - ** in the R script. Check the Notes below for recommendations for data format.

5. Check the config file **config.R**; you might want to skip imputation methods or prediction models or set the number of iterations according to your needs. You might also want to skip IterativeImputer if you don't have a python installation on your machine (set config_python as FALSE in this case). Check that all packages listed in the **config.R** are installed.

6. Run the script **run_datasetname** you created before.

7. If you want to run multiple datasets create a separate file for each dataset and repeat the steps 2 to 6 for each dataset.

8. Visualize the results using the markdown file **view_results_custom** from the **view_results_custom** directory. 

Recommendation: set N_iter in **config.R** to a small number first (e.g.: 3), run and view results. This will provide with an estimate of the time to run an iteration. 

Notes:

- Please provide data in a clean format; that is: data that you understand and you would feed to a model. Avoid providing data containing date/time fields, large free text, categorical variables with large number of categories, completely missing columns, etc. Best is to ensure that all your variables are either numeric or factor.

- Variable names x, y, z are not supported for the moment; please rename them (this is a current limitation in miceRanger package)

- Variable containing "binary_all" string are not allowed - this string is internally used for binarizing (dummy-coding) categorical variables

- For binary outcome, the outcome variable should not be a factor containing levels 0 and 1. This is a limitation  in the caret package. Please rename your outcome to "YES"" and "NO" in this case.

- If the outcome is numeric 0 and 1, it will be treated as a continuous outcome; make sure the outcome is character factor if you want it to be treated as binary

- Only binary and continuous outcomes are supported; other types like multinomial or time-to-event are not currently supported.

- The outcome column is expected to be complete for full evaluation; missing values are not supported in the outcome column

- You can run the code for either datasets with missing values or for datasets on which missingness is simulated. A combination between the two (e.g.: a dataset with missing values in one variable and simulated missing values on the other variables) is not supported

- On our selected datasets, all imputation methods have run without errors, except mice on IST. In case an imputation fails for reasons that are internal to the imputation method (e.g.: an error thrown by the function that does the imputation), skip the method using the skip_xxx parameters in the config file.

## How to run the project

To run all code, the scripts prefixed with run_ in the  **runs_on_datasets** directory should be run (run_diabetes.R should be run before run_diabetes_MV.R). 

## Project structure

All functions are in the **R** directory of the project. 

- The main function is **run_simulations** that takes a dataset (with or without missing values), runs all methods and returns the results. It is run for each dataset. 

- **init_results** initialzes a dataframe to store results

  - iteration_i = iteration number
  
  - iteration_j = effective iteration number. That's because knn might fail if it doesn't find neighbours; in the end it does not seem really needed but I keep it still out of caution. If knn fails, iteration_i stays the same, but iteration_j increments.
  
  - dataset = dataset name
  
  - metric = the metric (AUROC, NMSE, ...)
  
  - value = value for the metric
  
  - variable = variable if the metric is variable-wise (e.g.: NMSE on each variable); NA if the metric is for the prediction model (e.g.: AUROC)
  
  - method = imputation method
  
  - type = train_fit / train_predict / test_predict for timing (time_sec); train / test for variable-wise metrics
  
  - fail = records failures (knn mainly)
  
  - time_sec = the time it took to impute (separate for train, test...)
  
  - model = prediction model; only RF for the moment

- The other functions prefixed with **run_** run each imputation method and prediction models using the **run_pred_models** function and return the results.

- **impute_** functions take a train and a test set and return imputed train and test sets (only used for pyhon Iterative Imputer, caret bagging and caret knn)

- The function **run_pred_models** runs all prediction models (functions prefixed with **evaluate_**)

- **evaluate_** functions run all prediction models and return performance evaluation results

- **helpers** - smaller helper functions

- **cox_first_degree** - slope and intercept for calibration of binary prediction model

- **config.R** - configuration for all runs; also loads functions and packages

The **data** directory contains the datasets (downloaded from their source).

The **run_on_datasets** directory contains the scripts to run on each datasets. As a convention, the datasets with missing values are suffixed with _MV. The diabetes dataset is used twice: once with the original MVs and once removing variables with MVs and simulating missingness. The preprocessing of the dataset is done in run_diabetes_x.R saved and used in run_diabetes_MV_x.R. So run_diabetes_x.R should be run first (at least the preprocessing part of the script, until saving te preprocessed data).

The **results** directory is used to save the results later used in paper.Rmd.

The **runs_on_datasets_custom** and **view_results_custom** can be used for custom runs on datasets not originally included (see above)


