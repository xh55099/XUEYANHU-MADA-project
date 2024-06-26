---
editor_options: 
  markdown: 
    wrap: 72
---

# Overview

This is XUEYANHU class project repository for data analysis done with
R/Quarto/Github.

# Pre-requisites

This is a template for a data analysis project using R, Quarto, Github
and a reference manager that can handle bibtex. It is also assumed that
you have a word processor installed (e.g. MS Word or
[LibreOffice](https://www.libreoffice.org/)). You need that software
stack to make use of this template.

# Project structure

-   assets: storing reference pdf files and citation style file
-   data:
    -   raw-data: codebook created by me and original data file
        downloaded from website
-   processed-data: cleaned data saved in RDS file
-   products: manuscript in qmd file and its render output with same
    name in doc file
-   Rcode: including all codes needed
    -   analysis-code: including codes for simple exploratory analysis
        and machine learning models fitting both in qmd file and R
        script file
    -   dneda-code: including codes for simple exploratory analysis and
        machine learning models fitting both in qmd file and R script
        file
    -   mlmmodel-code: including codes for data cleaning process both in
        qmd and R script file
-   results:
    -   figures: all of the plots in png file created and saved from
        eda-code and analysis-code
    -   tables: summary tables created and saved from eda-code and 2
        linear model tables from analysis-code

# Project content

-   The `renv` folder is automatically generated by the `renv` package
    and you should never edit it manually. This folder is used to store
    information about the packages you are using in your project.
-   There are multiple special files in the repo.
    -   `README.md`: this file contains instructions or details about
        the folder it is located in. You are reading the project-level
        `README.md` file right now.
    -   `renv.lock`: a special file in JSON format used to keep a log of
        which packages and versions your project uses.
    -   `.gitignore`: this file gives instructions to the version
        control system, Git, and tells it which files we do not need to
        record versions of. Usually these are various files containing
        local settings.
    -   `.Rprofile`: whenever you restart the R session, R will source
        (run all code in) this script. Right now this is used by `renv`
        to make sure we have the correct packages and versions
        installed.

# Getting start

This is a Github template repository. The best way to get it and start
using it is [by following these
steps.](https://help.github.com/en/articles/creating-a-repository-from-a-template)

Once you got the repository, you can check out the examples by executing
them in order. First run the processing code, which will produce the
processed data. Then run the analysis scripts, which will take the
processed data and produce some results. Then you can run the
manuscript, poster and slides example files in any order. Those files
pull in the generated results and display them. These files also pull in
references from the `bibtex` file and format them according to the CSL
style.

You can read about keeping track of projects with `renv`
[here](https://rstudio.github.io/renv/articles/renv.html). Basically,
whenever you install new packages or update old packages, you need to
run `renv::snapshot()` to update the `renv.lock` file, which is a list
of packages and versions that the package uses. When you open the R
project on a new computer, you can run `renv::restore()` to reinstall
all the packages that you recorded in the `renv.lock` file.

# Reproducing the whole project

For reproducing this project, raw-data folder will be downloaded and
stored in a new folder called data and then included in a larger folder
with all the codes together. Processing-code qmd file under R folder
will be opened and run so the raw data will go through data cleaning
process and the cleaned data file will be saved in a folder called
process-data under the data folder. Then eda qmd file can be opened and
run using processed data file and the outputs will be saved in a newly
created fold called results. Results should include tables and figures
two subfolders.

Then, exploratory-analysis qmd file under R folder will be involved for
the whole data analysis and result will be seen directly in the file.

After all of the qmd files under R folder are run out, the final
manuscript will be generated. There are reference related files under
assets folders so it will be made sure download and then manuscript qmd
file under products fold will be find and run with a doc file saved
beside the qmd file.

Supplementary material file is under supplement subfolder and a pdf file
will be created and saved after running this qmd file.
