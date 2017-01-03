# AQM_Nov9_Advertising
David Dvorak  
November 12, 2016  



## Assignment Due Nov. 16, 2016

Derive and manually compute the following in R, then cross-check them to the R output from `lm()` (use Advertising.csv, as used in class):

* Estimates of the coefficients using matrix notation.
* Residuals using matrix notation.
* Plot residuals against each explanitory variable. Interpret.


Input the data on Sales vs. spending on TV, Newpaper and Radio advertising

```r
dat.adv <- read.csv("Advertising.csv")
Sales <- dat.adv$Sales
TV <- dat.adv$TV
Radio <- dat.adv$Radio
Newspaper <- dat.adv$Newspaper
```
Linear least squares regression for each of the possible explanatory variables

```r
mod <- lm(Sales ~ TV+Radio+Newspaper, dat.adv)
```
Residuals for linear least squares fitting

```r
res <- residuals(mod)
```

Plot Sales vs. Explanatory Variables

```r
#qplot(TV,Sales) # tilda usage: Sales "depends on" TV. Equivalent to plot(TV,S)
#qplot(Radio,Sales)
#qplot(Newspaper,Sales)
```
