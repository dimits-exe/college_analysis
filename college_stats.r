# Tsirmpas Dimitris p3190205

RESOURCE_PATH = "resources"

# Utility function to get a relative file path (including file extension)
# from a file name.
filepath_png <- function(name) {
  return(file.path(RESOURCE_PATH, paste(name, ".png", sep = "")))
}

# Utility function to save a plot to the disk.
my_save_plot <- function(name, plot_func, ...) {
  filepath = filepath_png(name)
  png(filepath)
  plot_func(...)
  dev.off()
}

# Import the data.
library(haven)
path = file.path("data", "05_colleges.sav")
input = read_sav(path)
df = data.frame(input)

# Check the provided variables' types and refactor them when needed.

class(df$gender)
df$gender <- as.factor(df$gender)
levels(df$gender) <- c("male", "female")

class(df$schtyp)
df$schtyp <- as.factor(df$schtyp)
levels(df$schtyp) <- c("public", "private")

class(df$prog)
df$prog <- as.factor(df$prog)
levels(df$prog) <- c("general", "academic", "vocation")

class(df$race)
df$race <- as.factor(df$race)
levels(df$race) <- c("hispanic", "asian", "african-amer", "white")

# Fix typo from source dataset.
names(df)[names(df) == 'genre'] <- 'gender'

class(df$math)
class(df$write)
class(df$socst)
df


# Get summary statistics from each of the quantitative variables.
library(psych)
describe(df$write)
describe(df$math)
describe(df$socst)

# Check for relationships between numerical variables.
corr.test(df[6:8], adjust="holm")

# Utility function which produces 
density_plot <- function(x, title) {
  d <- density(x)
  plot(d, xlab="Test score", main=title)
  polygon(d, col="lightblue", border="black")
}

# Plot and save
filepath = filepath_png("density_plots")
png(filepath)
par(mfrow=c(1,3))    # set the plotting area into a 1*3 array
density_plot(df$write, "Writing test scores")
density_plot(df$math, "Math test scores")
density_plot(df$write, "Social studies test scores")
dev.off()

# Perform correlation test on categorical variables.
# Manually test all combinations between all variables.
chisq.test(df$gender, df$race)
chisq.test(df$gender, df$schtyp)
chisq.test(df$gender, df$prog)
# The following tests have relative small samples, thus we use a simulated
# p value test with N=2000 (default) iterations. 
chisq.test(df$race, df$schtyp, simulate.p.value = T)
chisq.test(df$race, df$prog, simulate.p.value = T)
chisq.test(df$schtyp, df$prog, simulate.p.value = T)

# schtyp-prog is the only statistically significant correlation
# we thus produce a table using the sjtplot library
library(sjPlot)
sjt.xtab(var.row=df$schtyp, var.col=df$prog, show.row.prc = T)

# Test all relationships between the categorical variables and writing scores
# then save their boxplots to the disk.
# We explain in the report why we only consider writing scores as the dependent
# variable.
my_save_plot("write_race_boxplot", boxplot, formula=write ~ race, data=df, 
             xlab="Race", ylab="Writing Test Score")
my_save_plot("write_gender_boxplot", boxplot, formula=write ~ gender, data=df,
             xlab="Gender", ylab="Writing Test Score")
my_save_plot("write_prog_boxplot", boxplot, formula=write ~ prog, data=df,
             xlab="Previous Program", ylab="Writing Test Score")
my_save_plot("write_schtyp_boxplot", boxplot, formula=write ~ schtyp, data=df,
             xlab="School Type", ylab="Writing Test Score")


# ===== erotima b =====

# Split our dataset into female and male slices.
men_write = df[df$gender == "male",]
women_write = df[df$gender == "female",]

# Compute differences between the means. Reference for the methodology is 
# available in the report.
diff <- mean(df$write) - women_write$write
diff

# Check if differences are normal by making a qqplot.
qqnorm(diff)
qqline(diff)

# Plot is inconclusive, thus run shapiro and lillie normality test.
shapiro.test(diff)
library(nortest)
lillie.test(diff)

# Test whether the mean is well behaved by examining the normality of the 
# residuals' kurtosis.
library(moments)
jarque.test(diff)

# Test whether the differences are homogeneous.
library(car)
leveneTest(write ~ gender, df)
bartlett.test(write ~ gender, df)

# Preconditions for parametric t-test failed, thus execute a non-parametric 
# test for the significance in differences in mean
wilcox.test(men_write$write, women_write$write)
# Test whether women specifically have a *higher* grade than men.
wilcox.test(men_write$write, women_write$write, alternative = "less")

# ANOVA test on whether previous program influences writing score.
# We perform an ANOVA test since the mode of the prog variable is > 2.
formula = write ~ prog
anova <- aov(formula, data=df)
summary(anova)

# Perform variance analysis.
leveneTest(formula, df)
bartlett.test(formula, df)
# homogeneity confirmed

# Perform normality test.
shapiro.test(anova$res)
lillie.test(anova$res)
# not normal

# Test whether the mean is well behaved by examining the resulting boxplot.
my_save_plot("write_prog_boxplot", boxplot, formula=formula, data=df)
# not good mean

# We thus perform a non-parametric ANOVA test.
kruskal.test(formula, data=df)
# significant differences
# show boxplots


# ===== erotima c =====

# Create the base model which attempts to predict the math score based on 
# other variables except for the id (and obviously the math variable itself).
math_model = lm(math ~ . - id,df)
summary(math_model)

# Plot normalized residuals in order to check for outliers.
# Plot manually saved because of multiple functions used for one plot.
filepath = filepath_png("lm_math_residual_plot")
png(filepath)
plot(math_model$fit, rstandard(math_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
dev.off()
# no outliers found

# Race doesn't seem to be statistically significant, we thus remove it from 
# our model
no_race_model = lm(math ~ gender + prog + write + socst, data=df)
summary(no_race_model)

# This model has a worse adjusted r squared score and thus we re-instate the race
# variable. All other variables are statistically significant, and thus kept.
optimal_math_model = lm(math ~ gender + prog + write + socst + race, data=df)
summary(optimal_math_model)

# Compare BIC values between the two models
BIC(math_model)
BIC(optimal_math_model)

# Perform Bonferroni outlier test
outlierTest(optimal_math_model)
# examine outlier
df[194,]

# Check for normality
shapiro.test(rstandard(optimal_math_model))
lillie.test(rstandard(optimal_math_model))
qqnorm(rstandard(optimal_math_model))
qqline(rstandard(optimal_math_model))
# most likely not normal

# Check for homogeneity.

# A function that breaks a numeric list into its 4 quantiles
quantcut <- function(x, digits=6) { 
  cut(x, breaks=quantile(x), include.lowest=TRUE, dig.lab = digits) 
}

# Split the fitted values, split among the 4 quantiles
qfits <- quantcut(optimal_math_model$fit)
# Homogeneity test.
leveneTest(rstandard(optimal_math_model), qfits)
bartlett.test(rstandard(optimal_math_model), qfits)
# Plot the residuals against their quantiles and save the boxplot to the disk.
my_save_plot("lm_math_residual_boxplot", boxplot, rstandard(optimal_math_model)~qfits, 
             boxcol=0,boxfill=3, medlwd=2, 
             medcol="white", cex=1.5, pch=16, col='blue', xlab = "Quantiles", 
             ylab="Standardized residuals", names=c("Q1","Q2","Q3","Q4"))

# Check for variable autocorrelation
durbinWatsonTest(optimal_math_model)
durbinWatsonTest(optimal_math_model, method="normal")

# Check for multi-colinearity
vif(optimal_math_model)

#check for linearity
plot(optimal_math_model, 1)
# not linear

# Test whether logarithmic variables help linearity
log_math_model = lm(log(math) ~ race + gender + prog + log(write) + log(socst), data=df)
summary(log_math_model)
plot(log_math_model, 1)
# They don't

# Output model summary to latex.
library(stargazer)
stargazer(optimal_math_model, type="latex", 
          title="Linear regression model predicting math test scores, taking into 
          account other test scores.", ci=T, label="tab::lm_math_peeking", df=T,
          out="lm_math_peeking.tex", report=('vc*p'))


# Create the base model which attempts to predict the social studies score based on 
# other variables except for the id (and obviously the socst variable itself).
socst_model = lm(socst ~ . - id, df)
summary(socst_model)

# Plot normalized residuals in order to check for outliers.
# Plot manually saved because of multiple functions used for one plot.
filepath = filepath_png("lm_socst_residual_plot")
png(filepath)
plot(socst_model_orig$fit, rstandard(socst_model_orig))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
dev.off()
# Use the identify function to find the outlier's index
#identify(socst_model_orig$fit, rstandard(socst_model_orig),n=1)
# outlier 163 found

# Check the details of the outlier
df[163,]

# Use an automated stepwise model selection routine to determine best model (by BIC)
fullModel = lm(socst ~ . - id, data = df) 
nullModel = lm(socst ~ 1, data = df) 
optimal_socst_model = step(
      socst_model,
      direction = 'both', 
      scope = list(upper = fullModel, 
                   lower = nullModel), 
      trace = 0, # do not show the step-by-step process of model selection
      k=2) #choose by BIC as we want the best explanatory, not predictive model 

# View optimal model stats
summary(optimal_socst_model)

# Compare BIC values
BIC(socst_model)
BIC(optimal_socst_model)

# Perform Bonferroni outlier test
outlierTest(optimal_socst_model)
# Examine outlier
df[147,]

# Check for normal residuals
shapiro.test(rstandard(optimal_socst_model))
lillie.test(rstandard(optimal_socst_model)) 

qqnorm(rstandard(optimal_socst_model))
qqline(rstandard(optimal_socst_model))

# As above, check for residual homogeneity by checking the normalized residuals
# for each of the respective fitted values' quantiles
qfits <- quantcut(optimal_socst_model$fit)
leveneTest(rstandard(optimal_socst_model), qfits)
bartlett.test(rstandard(optimal_socst_model), qfits)
my_save_plot("lm_socst_residual_boxplot", boxplot, rstandard(optimal_socst_model)~qfits, 
             boxcol=0,boxfill=3, medlwd=2, medcol="white", cex=1.5, pch=16, 
             col='blue', xlab = "Quantiles", ylab="Standardized residuals", 
             names=c("Q1","Q2","Q3","Q4"))

# Check for autocorrelation
durbinWatsonTest(optimal_socst_model)
durbinWatsonTest(optimal_socst_model, method="normal")

# Check for multi-colinearity
vif(optimal_socst_model)

#check for linearity
plot(optimal_socst_model, 1)

# Output model summary to latex
stargazer(optimal_socst_model, type="latex", 
          title="Linear regression model predicting social study test scores, taking into 
          account other test scores.", ci=T, label="tab::lm_socst_peeking", df=T,
          out="lm_socst_peeking.tex", report=('vc*p'))


# ===== erotima d =====

# Base math model is equivalent to the one used above, without the writing score variable
no_peek_mmodel = lm(math ~ gender + race + schtyp + prog + socst, data=df)
summary(no_peek_mmodel)

# Repeat previously established routine to check for outliers
filepath = filepath_png("lm_math_nopeeking_residual_plot")
png(filepath)
plot(no_peek_mmodel$fit, rstandard(no_peek_mmodel))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
#identify(no_peek_mmodel$fit, rstandard(no_peek_mmodel),n=3)
# possible outliers: 22, 37, 194
dev.off()

fullModel = lm(math ~ . - id - write, data = df) 
nullModel = lm(math ~ 1, data = df) 
optimal_nopeek_math_model = step(fullModel, direction = 'both',
                           scope = list(upper = fullModel, lower = nullModel), 
                           trace = 0,  k=2)
# View optimal model stats
summary(no_peek_mmodel)
summary(optimal_nopeek_math_model)

# Compare BIC values
BIC(no_peek_mmodel)
BIC(optimal_nopeek_math_model)

# Perform bonferroni outlier test
outlierTest(optimal_nopeek_math_model)
# Examine outlier
df[22,]

# Check for normal residuals
shapiro.test(rstandard(optimal_nopeek_math_model))
lillie.test(rstandard(optimal_nopeek_math_model))

# Repeat previously established routine to check for homogeneity
qfits <- quantcut(optimal_nopeek_math_model$fit)
leveneTest(rstandard(optimal_nopeek_math_model), qfits)
bartlett.test(rstandard(optimal_nopeek_math_model), qfits)
my_save_plot("lm_math_nopeeking_residual_boxplot", boxplot, 
             rstandard(optimal_nopeek_math_model)~qfits, 
             boxcol=0,boxfill=3, medlwd=2, medcol="white", cex=1.5, pch=16, 
             col='blue', xlab = "Quantiles", ylab="Standardized residuals", 
             names=c("Q1","Q2","Q3","Q4"), report=('vc*p'))

# Check for autocorrelation
durbinWatsonTest(optimal_nopeek_math_model)
durbinWatsonTest(optimal_nopeek_math_model, method="normal")

# Check for multi-colinearity
vif(optimal_nopeek_math_model)

# Check linearity
plot(optimal_nopeek_math_model, 1, asp=1) 
# almost linear, we consider the test marginally passed 

# Output table to latex
stargazer(optimal_nopeek_math_model, type="latex", 
          title="Linear regression model predicting math test scores, 
          without relying on the writing tests.", ci=T, label="tab::lm_math_nopeeking", 
          df=T, out="lm_math_nopeeking.tex", report=('vc*p'))

# Similarly, the base model for social studies, without including the writing scores
socst_nopeek_model = lm(socst ~ gender + race + schtyp + prog + math, data=df)
summary(socst_nopeek_model)

# Repeat previously established routine to check for outliers
filepath = filepath_png("lm_socst_nopeeking_residual_plot")
png(filepath)
plot(socst_nopeek_model$fit, rstandard(socst_nopeek_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
# no outliers
dev.off()

# Use an automated stepwise model selection routine to determine best model (by AIC)
fullModel = lm(socst ~ . - id - write, data = df) 
nullModel = lm(socst ~ 1, data = df) 
optimal_socst_nopeek_model = step(socst_nopeek_model, direction = 'both',
                                  scope = list(upper = fullModel, lower = nullModel), 
                                  trace = 0,  k=2)
# View optimal model stats
summary(optimal_socst_nopeek_model)

# Compare BIC values
BIC(socst_nopeek_model)
BIC(optimal_socst_nopeek_model)

# Perform Bonferroni outlier test
outlierTest(optimal_socst_nopeek_model)
# same outlier as above socst model

# check for normality
shapiro.test(rstandard(optimal_socst_nopeek_model))
lillie.test(rstandard(optimal_socst_nopeek_model))

# Repeat previously established routine to check for homogeneity
qfits <- quantcut(optimal_socst_nopeek_model$fit)
leveneTest(rstandard(optimal_socst_nopeek_model), qfits)
bartlett.test(rstandard(optimal_socst_nopeek_model), qfits)
my_save_plot("lm_socst_nopeeking_residual_boxplot", boxplot, 
             rstandard(optimal_socst_nopeek_model)~qfits, 
             boxcol=0,boxfill=3, medlwd=2, medcol="white", cex=1.5, pch=16, 
             col='blue', xlab = "Quantiles", ylab="Standardized residuals", 
             names=c("Q1","Q2","Q3","Q4"), report=('vc*p'))

# Check for autocorrelation
durbinWatsonTest(optimal_socst_nopeek_model)
durbinWatsonTest(optimal_socst_nopeek_model, method="normal")

# Check for multi-colinearity
vif(optimal_socst_nopeek_model)

# Check for linearity
plot(optimal_socst_nopeek_model, 1)

# Output model summary to latex
stargazer(optimal_socst_nopeek_model, type="latex", 
          title="Linear regression model predicting social study test scores, 
          without relying on the writing tests.", ci=T, label="tab::lm_socst_nopeeking",
          df=T, out="lm_socst_nopeeking.tex", report=('vc*p'))