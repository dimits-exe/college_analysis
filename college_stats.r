# Tsirmpas Dimitris p3190205

RESOURCE_PATH = "resources"

# Utility function to get a relative file path (including file extension)
# from a file name.
filepath_png <- function(name) {
  return(file.path(RESOURCE_PATH, paste(name, ".png", sep = "")))
}

# Utility function to save a plot to the disk.
my_save_plot <- function(name, plot_func, ...) {
  # display plot
  plot_func(...)
  # save plot
  filepath = filepath_png(name)
  png(filepath)
  plot_func(...)
  dev.off() # COMMENT THIS LINE TO PREVENT PLOT SAVING
}

# Import the data.
library(haven)
path = file.path("data", "05_colleges.sav")
input = read_sav(path)
df = data.frame(input)

# Check the provided variables' types and refactor them when needed.

# Fix typo from source dataset.
names(df)[names(df) == 'genre'] <- 'gender'

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
par(mfrow=c(3,1))    # set the plotting area into a 1*3 array
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

# Test whether the differences are homogeneous.
library(car)
leveneTest(write ~ gender, df)
bartlett.test(write ~ gender, df)

# kurtosis.norm.test can't be used since its package is no longer available
# but the mean and median are incredibly close to each other in our sample
# so we can most likely use a parametric t-test

# execute a parametric t-test for the significance in differences in mean
t.test(men_write$write, women_write$write)
# Test whether women specifically have a *higher* grade than men.
t.test(men_write$write, women_write$write, alternative = "less")

# display and save error barplots
library(gplots)
my_save_plot("writing_gender_error_plot", plotmeans, write~gender, data=df, xlab="Gender", ylab="Writing test score",
          connect=F)

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

# Perform a non-parametric ANOVA test just in case
kruskal.test(formula, data=df)
# significant differences here too

# post-hoc statistics
pairwise.t.test(df$write, df$prog)
round(TukeyHSD(anova)$prog, 3)

# display and save error barplots
my_save_plot("write_prog_error_plot", plotmeans, write~prog, data=df, 
              xlab="Previous program", ylab="Writing test score",
              connect=F)

# ===== erotima c =====

# ===== MATH MODEL  =====
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
my_save_plot("lm_math_linear_plot", plot, optimal_math_model, 1)
# marginally not linear

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
          out="lm_math_peeking.tex", report=('vc*p'), no.space=TRUE)


# ===== SOCST MODEL  =====
# Create the base model which attempts to predict the social studies score based on 
# other variables except for the id (and obviously the socst variable itself).
socst_model = lm(socst ~ . - id, df)
summary(socst_model)

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

# Plot normalized residuals in order to check for outliers.
# Plot manually saved because of multiple functions used for one plot.
filepath = filepath_png("lm_socst_residual_plot")
png(filepath)
plot(optimal_socst_model$fit, rstandard(optimal_socst_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
dev.off()
# Use the identify function to find the outlier's index
#identify(socst_model_orig$fit, rstandard(socst_model_orig),n=1)
# outlier 163 found
# Examine outlier
df[163,]

# Perform Bonferroni outlier test
outlierTest(optimal_socst_model)
# no outliers

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
          out="lm_socst_peeking.tex", report=('vc*p'), no.space=TRUE)


# ===== erotima d =====

# ===== MATH MODEL  =====
# Base math model is equivalent to the one used above, without the writing score variable
no_peek_mmodel = lm(math ~ gender + race + schtyp + prog + socst, data=df)
summary(no_peek_mmodel)

fullModel = lm(math ~ . - id - write, data = df) 
nullModel = lm(math ~ 1, data = df) 
optimal_nopeek_math_model = step(fullModel, direction = 'both',
                           scope = list(upper = fullModel, lower = nullModel), 
                           trace = 0,  k=2)
# View optimal model stats
summary(optimal_nopeek_math_model)

# Compare BIC values
BIC(no_peek_mmodel)
BIC(optimal_nopeek_math_model)

# Repeat previously established routine to check for outliers
filepath = filepath_png("lm_math_nopeeking_residual_plot")
png(filepath)
plot(optimal_nopeek_math_model$fit, rstandard(optimal_nopeek_math_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
#identify(optimal_nopeek_math_model$fit, rstandard(optimal_nopeek_math_model),n=3)
# possible outliers: 22, 37, 194
dev.off()

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
my_save_plot("lm_math_nopeek_linear_plot", plot,optimal_nopeek_math_model, 1, asp=1)
# marginally not linear

# Output table to latex
stargazer(optimal_nopeek_math_model, type="latex", 
          title="Linear regression model predicting math test scores, 
          without relying on the writing tests.", ci=T, label="tab::lm_math_nopeeking", 
          df=T, out="lm_math_nopeeking.tex", report=('vc*p'), no.space=TRUE)


# ===== SOCST MODEL  =====
# Similarly, the base model for social studies, without including the writing scores
socst_nopeek_model = lm(socst ~ gender + race + schtyp + prog + math, data=df)
summary(socst_nopeek_model)

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

# Repeat previously established routine to check for outliers
filepath = filepath_png("lm_socst_nopeeking_residual_plot")
png(filepath)
plot(optimal_socst_nopeek_model$fit, rstandard(optimal_socst_nopeek_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
# no outliers
dev.off()

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
          df=T, out="lm_socst_nopeeking.tex", report=('vc*p'), no.space=TRUE)


# ===== erotima e =====

# ===== MATH MODEL  =====
# a function that cuts a vector to bins of (0,50] and (50,100]
cut_score <- function(x) {
  return(cut(x, breaks=c(0,50,100), labels=c("Fail","Pass"), include.lowest = TRUE))
}

# create a new dataframe containing the binned test scores
cutdf = data.frame(df)
cutdf$cut_write = cut_score(df$write)
cutdf$cut_socst = cut_score(df$socst)
cutdf$cut_math = cut_score(df$math)
cutdf

# create the math model from before but which is only allowed to look at
# the binary test scores
cut_math_model = lm(math ~.-id-socst-write-cut_math, cutdf)
summary(cut_math_model)

# Use an automated stepwise model selection routine to determine best model (by AIC)
nullModel = lm(math ~ 1, data = df) 
opt_cut_math_model = step(cut_math_model, direction = 'both',
                                  scope = list(upper = cut_math_model,
                                               lower = nullModel), 
                                  trace = 0,  k=2)
# View optimal model stats
summary(opt_cut_math_model)

# Compare BIC values
BIC(cut_math_model)
BIC(opt_cut_math_model)

# Repeat previously established routine to check for outliers
filepath = filepath_png("lm_math_binary_residual_plot")
png(filepath)
plot(opt_cut_math_model$fit, rstandard(opt_cut_math_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
# no outliers
dev.off()

# Perform Bonferroni outlier test
outlierTest(opt_cut_math_model)

# false positive
cutdf[22,]

# check for normality
shapiro.test(rstandard(opt_cut_math_model))
lillie.test(rstandard(opt_cut_math_model))

# Repeat previously established routine to check for homogeneity
qfits <- quantcut(opt_cut_math_model$fit)
leveneTest(rstandard(opt_cut_math_model), qfits)
bartlett.test(rstandard(opt_cut_math_model), qfits)
my_save_plot("lm_socst_nopeeking_residual_boxplot", boxplot, 
             rstandard(opt_cut_math_model)~qfits, 
             boxcol=0,boxfill=3, medlwd=2, medcol="white", cex=1.5, pch=16, 
             col='blue', xlab = "Quantiles", ylab="Standardized residuals", 
             names=c("Q1","Q2","Q3","Q4"), report=('vc*p'))

# Check for autocorrelation
durbinWatsonTest(opt_cut_math_model)
durbinWatsonTest(opt_cut_math_model, method="normal")

# Check for multi-colinearity
vif(opt_cut_math_model)

# Check for linearity
plot(opt_cut_math_model, 1)

# Output model summary to latex
stargazer(opt_cut_math_model, type="latex", 
          title="Linear regression model predicting social study test scores, 
          using binary variables for the other test scores.", ci=T, label="tab::lm_cut_math",
          df=T, out="lm_cut_math.tex", report=('vc*p'), no.space=TRUE)


# ===== SOCST MODEL  =====

# create the socst model from before but which is only allowed to look at
# the binary test scores
cut_socst_model = lm(socst ~.-id-write-cut_socst-math, cutdf)
summary(cut_socst_model)

# Use an automated stepwise model selection routine to determine best model (by AIC)
nullModel = lm(math ~ 1, data = df) 
opt_cut_socst_model = step(cut_socst_model, direction = 'both',
                          scope = list(upper = cut_socst_model,
                                       lower = nullModel), 
                          trace = 0,  k=2)
# View optimal model stats
summary(opt_cut_socst_model)

# Compare BIC values
BIC(cut_socst_model)
BIC(opt_cut_socst_model)

# Repeat previously established routine to check for outliers
filepath = filepath_png("lm_socst_binary_residual_plot")
png(filepath)
plot(opt_cut_socst_model$fit, rstandard(opt_cut_socst_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
# no outliers
dev.off()

# Perform Bonferroni outlier test
outlierTest(opt_cut_socst_model)
# same outlier as above model

# check for normality
shapiro.test(rstandard(opt_cut_socst_model))
lillie.test(rstandard(opt_cut_socst_model))

# Repeat previously established routine to check for homogeneity
qfits <- quantcut(opt_cut_socst_model$fit)
leveneTest(rstandard(opt_cut_socst_model), qfits)
bartlett.test(rstandard(opt_cut_socst_model), qfits)
my_save_plot("lm_socst_nopeeking_residual_boxplot", boxplot, 
             rstandard(opt_cut_socst_model)~qfits, 
             boxcol=0,boxfill=3, medlwd=2, medcol="white", cex=1.5, pch=16, 
             col='blue', xlab = "Quantiles", ylab="Standardized residuals", 
             names=c("Q1","Q2","Q3","Q4"), report=('vc*p'))

# Check for autocorrelation
durbinWatsonTest(opt_cut_socst_model)
durbinWatsonTest(opt_cut_socst_model, method="normal")


# Check for multi-colinearity
vif(opt_cut_socst_model)

# Check for linearity
plot(opt_cut_socst_model, 1)

# Output model summary to latex
stargazer(opt_cut_socst_model, type="latex", 
          title="Linear regression model predicting social study test scores, 
          using only binary variables for the other test scores.", ci=T, 
          label="tab::lm_cut_socst",
          df=T, out="lm_cut_math.tex", report=('vc*p'), no.space=TRUE)