This folder contains code for fitting machine learning models on the processed/cleaned data and comparing their performance by making predictions.
The code produces a few tables and figures, which are saved in the `results` folder.

The ml-model-v1 file includes the simple linear model fitting for the dataset using different variables, as well as the following linear regression machine learning model and logistic machine learning models with metrics calculation and comparison.
1. The correlation between commute transportation and physical activity.
2. Both water consumption and physical activity as predictors for BMI as an outcome, and interaction is calculated.
3. Linear model：BMI vs. alcohol consumption
4. Linear model: BMI vs. all predictors including Gender, Age, History, Water, Alcohol, FAF, MTRANS.
5. Linear regression including simple linear model, lasso model and random forest model, best predictor combination will be selected according to RMSE.
6. Logistic models including discriminant analysis and multinominal logistic regression, best predictor combination will be selected according to accuracy.

Note:
It's the same code done 3 times:

* First, there is an R script that you can run which does all the computations.
* Second, there is a Quarto file which contains exactly the same code as the R script.
* Third, my current favorite, is a Quarto file with an approach where the code is pulled in from the R script and run.