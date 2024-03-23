This folder contains code to do some simple exploratory analysis on the processed/cleaned data.
The code produces a few tables and figures, which are saved in the `results` folder.

The exploratory-analysis file include the simple linear model fitting fot the dataset.
1. Commute transportation as predictor and physical activity as an outcome but needs more time to figure out.
2. Both water consumption and physical activity as predictors and BMI as an outcome.
3. BMI vs. alcohol consumption
4. BMI vs. all predictors that I choose including commute transportation, family history, water consumption and alcohol consumption.



Note:
It's the same code done 3 times:

* First, there is an R script that you can run which does all the computations.
* Second, there is a Quarto file which contains exactly the same code as the R script.
* Third, my current favorite, is a Quarto file with an approach where the code is pulled in from the R script and run.

The last version has the advantage of having code in one place for easy writing/debugging, and then being able to pull the code into the Quarto file for a nice combination of text/commentary and code.

Each way of doing this is a reasonable approach, pick whichever one you prefer or makes the most sense for your setup. Whichever approach you choose, add ample documentation/commentary so you and others can easily understand what's going on and what is done.