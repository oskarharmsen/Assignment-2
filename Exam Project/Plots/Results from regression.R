# #### REg 1 One variable ----
# 
# Call:
#   lm(formula = votes.pers ~ agree.three.mean.party.storkreds, data = reg.data)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3052  -1984  -1234    233  54682 
# 
# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)                         -3118       1747  -1.785  0.07471 . 
# agree.three.mean.party.storkreds     6222       2024   3.074  0.00219 **
#   ---
#   Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 4135 on 713 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.01308,	Adjusted R-squared:  0.01169 
# F-statistic: 9.449 on 1 and 713 DF,  p-value: 0.002193
# 
# > length(lm1$fitted.values)
# [1] 715



# REG 2 Base line --------------------
# 
# Call:
#   lm(formula = votes.pers ~ agree.three.mean.party.storkreds + 
#        is.male + ran.last.election + age, data = reg.data, na.action = "na.omit")
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -4868  -1448   -527    557  53024 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                        929.72    1816.25   0.512  0.60889    
#   agree.three.mean.party.storkreds  5051.11    1901.04   2.657  0.00806 ** 
#   is.maleTRUE                       -243.26     310.81  -0.783  0.43408    
#   ran.last.electionTRUE            -3125.32     299.37 -10.440  < 2e-16 ***
#   age                                -22.91      11.61  -1.974  0.04879 *  
#   ---
#   Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 3851 on 710 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.1476,	Adjusted R-squared:  0.1428 
# F-statistic: 30.73 on 4 and 710 DF,  p-value: < 2.2e-16
# 
# > length(lm1$fitted.values)
# [1] 715


# REG 3 With party interaction--------------------------

# > summary(lm1)
# 
# Call:
#   lm(formula = votes.pers ~ agree.three.mean.party.storkreds * 
#        party + is.male + ran.last.election + age, data = reg.data, 
#      na.action = "na.omit")
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -7392  -1487   -309    576  50224 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                              -25706.19    7317.22  -3.513 0.000472 ***
# agree.three.mean.party.storkreds          38209.55    8318.52   4.593 5.18e-06 ***
# partyaa                                   24260.02    9521.94   2.548 0.011055 *    
# partyb                                    22003.66    9821.38   2.240 0.025383 *  
# partyc                                    27639.86   10178.91   2.715 0.006785 ** 
# partyf                                    28689.16   15085.14   1.902 0.057610 .  
# partyi                                    24436.21   10778.23   2.267 0.023687 *  
# partyk                                    25162.30    9513.48   2.645 0.008357 ** 
# partyo                                     8096.77    9577.39   0.845 0.398177    
# partyoe                                    2018.25   15141.28   0.133 0.893999    
# partyv                                    25027.70    8687.59   2.881 0.004089 ** 
# is.maleTRUE                                -399.26     290.60  -1.374 0.169902    
# ran.last.electionTRUE                     -2327.72     298.51  -7.798 2.32e-14 ***
# age                                         -17.80      10.92  -1.631 0.103373    
# agree.three.mean.party.storkreds:partyaa -31191.93   11186.38  -2.788 0.005443 ** 
# agree.three.mean.party.storkreds:partyb  -29342.62   11598.57  -2.530 0.011632 *  
# agree.three.mean.party.storkreds:partyc  -35785.87   12438.82  -2.877 0.004139 ** 
# agree.three.mean.party.storkreds:partyf  -37834.50   16934.67  -2.234 0.025792 *  
# agree.three.mean.party.storkreds:partyi  -32596.22   11816.19  -2.759 0.005958 ** 
# agree.three.mean.party.storkreds:partyk  -34076.12   11531.41  -2.955 0.003232 ** 
# agree.three.mean.party.storkreds:partyo  -10379.83   11194.28  -0.927 0.354123    
# agree.three.mean.party.storkreds:partyoe  -8915.36   16372.61  -0.545 0.586253    
# agree.three.mean.party.storkreds:partyv  -29824.39   10121.10  -2.947 0.003319 ** 
#   ---
#   Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 3545 on 692 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.2958,	Adjusted R-squared:  0.2735 
# F-statistic: 13.22 on 22 and 692 DF,  p-value: < 2.2e-16
# 
# > length(lm1$fitted.values)
# [1] 715


# REG 4: Without socialdemokratiet ----

# > summary(lm1)
# 
# Call:
#   lm(formula = votes.pers ~ agree.three.mean.party.storkreds + 
#        is.male + ran.last.election + age, data = reg.data, na.action = "na.omit")
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3844  -1089   -469    331  53796 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       1451.80    1767.22   0.822   0.4117    
# agree.three.mean.party.storkreds  3237.84    1849.95   1.750   0.0806 .  
# is.maleTRUE                       -204.66     313.31  -0.653   0.5139    
# ran.last.electionTRUE            -2610.15     306.36  -8.520   <2e-16 ***
#   age                                -15.09      11.63  -1.297   0.1950    
# ---
#   Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 3666 on 628 degrees of freedom
# (4 observations deleted due to missingness)
# Multiple R-squared:  0.1112,	Adjusted R-squared:  0.1055 
# F-statistic: 19.64 on 4 and 628 DF,  p-value: 3.023e-15
# 
# > length(lm1$fitted.values)
# [1] 633




# REG 5: with average distance to oth party -----
# > summary(lm1)
# 
# Call:
#   lm(formula = votes.pers ~ agree.three.mean.oth.party.storkreds + 
#        is.male + ran.last.election + age, data = reg.data, na.action = "na.omit")
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -4460  -1349   -521    360  53252 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           8686.99    2104.45   4.128 4.09e-05 ***
# agree.three.mean.oth.party.storkreds -3843.86    2402.01  -1.600   0.1100    
# is.maleTRUE                           -227.11     310.61  -0.731   0.4649    
# ran.last.electionTRUE                -3144.79     299.46 -10.501  < 2e-16 ***
# age                                    -28.44      11.56  -2.460   0.0141 *  
#   ---
#   Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 3860 on 712 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.1416,	Adjusted R-squared:  0.1368 
# F-statistic: 29.36 on 4 and 712 DF,  p-value: < 2.2e-16
# 
# > length(lm1$fitted.values)
# [1] 717

# REG 6: with average distance from party center -----
# > summary(lm1)
# 
# Call:
#   lm(formula = votes.pers ~ agree.party.mean + is.male + ran.last.election + 
#        age, data = reg.data, na.action = "na.omit")
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -4572  -1402   -590    410  53250 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            1672.22    2120.47   0.789   0.4306    
# agree.party.mean       4129.35    2268.80   1.820   0.0692 .  
# is.maleTRUE            -151.66     321.21  -0.472   0.6370    
# ran.last.electionTRUE -3088.73     308.98  -9.997   <2e-16 ***
#   age                     -22.45      12.03  -1.867   0.0623 .  
# ---
#   Signif. codes:  0 ???***??? 0.001 ???**??? 0.01 ???*??? 0.05 ???.??? 0.1 ??? ??? 1
# 
# Residual standard error: 3994 on 714 degrees of freedom
# Multiple R-squared:  0.1293,	Adjusted R-squared:  0.1244 
# F-statistic:  26.5 on 4 and 714 DF,  p-value: < 2.2e-16
# 
# > length(lm1$fitted.values)
# [1] 719



