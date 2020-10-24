# Body-Fat-Project
STAT628-Module2-Group11
## Goal
Accurate measurement of body fat is inconvenient/costly and it is desirable to have easy methods of estimating body fat that are not inconvenient/costly. 

In this project, we want to come up with a simple, robust, and accurate “rule-of-thumb” to estimate percentage of body fat using clinically available measurements. This “rule-of-thumb” is be based on a real data set of 252 men with measurements of their percentage of body fat and various body circumference measurements.

## Summary
`SUMMARY-Stat_628_Module_2.pdf` is an summary of our modeling process.

We firstly removed some outliers by the boxplot and then tested the multi-colinearity. Next, we used three different methods to handle the multi-colinearity and selected the models. Finally, R-squared, residuals and some other assumptions were checked.

Please see details in the summary file.

## App

Our shiny web-based app can be accessed on https://kouhuitong.shinyapps.io/bodyfat-cal/ .

## Code
There are R code files in the code director. 

`bodyfat-model.R` is the code for modeling and the `app.R` is for building the app.

## Data
`BodyFat.csv` is the raw data set of available measurements include age, weight, height, bmi, and various body circumference measurements. In particular, the variables listed below (from left to right in the data set) are:

- Percent body fat from Siri's (1956) equation
- Density determined from underwater weighing
- Age (years)
- Weight (lbs)
- Height (inches)
- Adioposity (bmi)
- Neck circumference (cm)
- Chest circumference (cm)
- Abdomen 2 circumference (cm)
- Hip circumference (cm)
- Thigh circumference (cm)
- Knee circumference (cm)
- Ankle circumference (cm)
- Biceps (extended) circumference (cm)
- Forearm circumference (cm)
- Wrist circumference (cm)


## Authors
- Huitong Kou - (hkou2@wisc.edu)
- Fengxia Dong - (fdong6@wisc.edu)
- Haohao Su -　(hsu69@wisc.edu)
