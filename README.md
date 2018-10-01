# Complete Multiple Regression

## Description
This R script performs a complete multiple regression on two data sets, using the `lm` function and different plots and tests to analyze the following assumptions of the model:
* The residuals are normally distributed 
* The residuals are not correlated
* The residuals have constant variance

The script also constructs a confidence interval and prediction interval for each data set.

## Data
### Grocery Store
#### A dataset from the textbook Applied Linear Statistical Models by Kutner, Nachtsheim, Neter, & Li
This [dataset](http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR09.txt) is based on the following: A large, national grocery retailer tracks productivity and costs of its facilities
closely. Data below were obtained from a single distribution center for a one-year period. Each
data point for each variable represents one week of activity. The variables included are the
number of cases shipped (X1) the indirect costs of the total labor hours as a percentage (X2),
a qualitative predictor called holiday that is coded 1 if the week has a holiday and 0 otherwise
(X3), and the total labor hours (Y).

### Brand Preference
#### A dataset from the textbook Applied Linear Statistical Models by Kutner, Nachtsheim, Neter, & Li
This [dataset](http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter%20%206%20Data%20Sets/CH06PR05.txt) is based on the following: In a small-scale experimental study of the relation between degree of brand
liking (Y) and moisture content (X1) and sweetness (X2) of the product, the following results
were obtained from the experiment based on a completely randomized design (data are coded).
