# Welcome

This folder contains materials for your Posit Academy course project. You will complete this project in Positron using a dedicated instance of Posit Workbench (instructions for accessing Posit Workbench will be shared at the beginning of your course).

**By accessing these Posit Academy course materials, you agree to Posit's [End User License Agreement](https://posit.co/about/eula/) and [Learning Services Agreement](https://posit.co/learning-services-agreement/).**

## Project Structure

- **Quarto files (.qmd) and  R scripts (.R)**: These are the project milestone files where you'll complete your work each week

- **data/**: Contains the datasets you'll explore

- **assets/**: Contains data dictionaries explaining the variables in each dataset

- **images/**: Contains milestone "solution" images for you to compare against your work

- **solutions/**: Contains "solution" datasets for you to compare against your work

## Getting Started

**Step 1: Install R packages you will need for this project using renv**

1. Open your Console tab at the bottom of the Positron IDE.

2. Run `install.packages("renv")` in your Console to install the renv package.

3. Run `renv::restore()` in your Console.  You will see output in the Console indicating that R packages are being installed. **Wait for this to complete before proceeding** (it may take several minutes).

4. Re-start your R session by clicking the Restart Console button (circular arrow icon) near the top-right corner of the Console.

Note: renv is a popular tool for managing reproducible R environments -- you can learn more about it here: https://rstudio.github.io/renv/

**Step 2: Open your first milestone file.**

1. In the Explorer tab on the left, open the file `census_01_quarto_functions.qmd`

2. Follow the instructions in this file to complete the exercises

## Note: One milestone at a time

Once you have set up your project in Positron, you will have access to all of the milestone files for your project. However, we encourage you to **only focus on the milestone corresponding to the current week of your Academy course**.

Milestone files are numbered sequentially according to the week of the course. For example:

Week 1 = `census_01_quarto_functions.qmd`  
Week 2 = `census_01_quarto_conditionals.qmd`  
etc.