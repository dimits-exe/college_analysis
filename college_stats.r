RESOURCE_PATH = "resources"

filepath_png <- function(name) {
  return(file.path(RESOURCE_PATH, paste(name, ".png", sep = "")))
}

save_plot <- function(name, plot_func, ...) {
  filepath = filepath_png(name)
  png(filepath)
  plot_func(...)
  dev.off()
}

library(haven)
path = file.path("data", "05_colleges.sav")
input = read_sav(path)
df = data.frame(input)

# check and refactor dataframe

class(df$genre)
df$genre <- as.factor(df$genre)
levels(df$genre) <- c("male", "female")

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

library(psych)
describe(df$write)
describe(df$math)
describe(df$socst)

# check for relationships between numerical variables
corr.test(df[6:8], adjust="holm")
rcorr(cbind(df$write, df$mathm, df$socst), type="spearman")

save_plot("write_race_boxplot", boxplot, formula=write ~ race, data=df)
save_plot("write_genre_boxplot", boxplot, formula=write ~ genre, data=df)
save_plot("write_prog_boxplot", boxplot, formula=write ~ prog, data=df)
save_plot("write_schtyp_boxplot", boxplot, formula=write ~ schtyp, data=df)


# ===== erotima b =====
men_write = df[df$genre == "male",]
women_write = df[df$genre == "female",]

# compute diff
diff <- mean(df$write) - women_write$write
diff

# check if differences are normal
qqnorm(diff)
qqline(diff)
shapiro.test(diff)

library(moments)
jarque.test(diff)

library(car)
leveneTest(write ~ genre, data=df)
bartlett.test(formula, df)

# use Welch test regardless of variance analysis
t.test(men_write$write, women_write$write)
t.test(men_write$write, women_write$write, alternative = "less")


formula = write ~ prog
anova <- aov(formula, data=df)
summary(anova)

# variance analysis
leveneTest(formula, data=df)
bartlett.test(formula, df)
# homogeneity confirmed

# normality test
shapiro.test(anova$res)
# not normal

# check mean
save_plot("write_prog_boxplot", boxplot, formula=formula, data=df)
# not good mean

# non-parametric means test
kruskal.test(formula, data=df)
# significant differences
# show boxplots


# ===== erotima c =====
math_model = lm(math ~ .,df)
summary(math_model)

# check for outliers

# manually saved because of multiple functioned used for one plot
filepath = filepath_png("lm_math_residual_plot")
png(filepath)
plot(math_model$fit, rstandard(math_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
dev.off()
# no outliers found

# check for normality
shapiro.test(rstandard(math_model))

W#check for homogeneity
quantcut <- function(x, digits=6) { 
    cut(x, breaks=quantile(x), include.lowest=TRUE, dig.lab = digits) 
}

qfits <- quantcut(math_model$fit)
leveneTest(rstandard(math_model), qfits)
save_plot("lm_math_residual_boxplot", boxplot, rstandard(math_model)~qfits, boxcol=0,boxfill=3, medlwd=2, 
        medcol="white", cex=1.5, pch=16, col='blue', xlab = "Quantiles", 
        ylab="Standardized residuals", names=c("Q1","Q2","Q3","Q4"))

# check for autocorrelation
durbinWatsonTest(math_model)
durbinWatsonTest(math_model, method="normal")

no_race_model = lm(math ~ genre + prog + write + socst, data=df)
summary(no_race_model)

optimized_math_model = lm(math ~ genre + prog + write + socst + race, data=df)
summary(optimized_math_model)

# Do we need to re-check preconditions here?

library(stargazer)
stargazer(optimized_math_model, type="latex", 
          title="Linear regression model predicting math test scores, taking into 
          account other test scores.", ci=T, label="tab::lm_math_peeking", df=T,
          out="lm_math_peeking.tex")

socst_model_orig = lm(socst ~ . - id,df)
summary(socst_model)

# check for outliers
filepath = filepath_png("lm_socst_residual_plot")
png(filepath)
plot(socst_model_orig$fit, rstandard(socst_model_orig))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
dev.off()
#identify(socst_model_orig$fit, rstandard(socst_model_orig),n=1)
# outlier 163 found

df[163,]

socst_model = lm(socst ~ . -id ,df[-163,])
summary(socst_model)

# check for normality
shapiro.test(rstandard(socst_model))

qfits <- quantcut(socst_model$fit)
leveneTest(rstandard(socst_model), qfits)
save_plot("lm_socst_residual_boxplot", boxplot, rstandard(socst_model)~qfits, 
          boxcol=0,boxfill=3, medlwd=2, medcol="white", cex=1.5, pch=16, 
          col='blue', xlab = "Quantiles", ylab="Standardized residuals", 
          names=c("Q1","Q2","Q3","Q4"))

# check for autocorrelation
durbinWatsonTest(socst_model)
durbinWatsonTest(socst_model, method="normal")

library(MASS)
fullModel = lm(socst ~ . - id, data = df[-163,]) 
nullModel = lm(socst ~ 1, data = df[-163,]) 
optimal_socst_model = stepAIC(
                  socst_model,
                  direction = 'both', 
                  scope = list(upper = fullModel, 
                               lower = nullModel), 
                  trace = 0) # do not show the step-by-step process of model selection
summary(optimal_socst_model)

# Do we need to re-check preconditions here?

stargazer(optimal_socst_model, type="latex", 
          title="Linear regression model predicting social study test scores, taking into 
          account other test scores.", ci=T, label="tab::lm_socst_peeking", df=T,
          out="lm_socst_peeking.tex", report=('vc*p'))


# ===== erotima d =====

no_peek_mmodel = lm(math ~ genre + race + schtyp + prog + socst, data=df)
summary(no_peek_mmodel)

# manually saved because of multiple functioned used for one plot
filepath = filepath_png("lm_math_nopeeking_residual_plot")
png(filepath)
plot(no_peek_mmodel$fit, rstandard(no_peek_mmodel))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
#identify(no_peek_mmodel$fit, rstandard(no_peek_mmodel),n=3)
# possible outliers: 22, 37, 194
dev.off()

# check for normality
shapiro.test(rstandard(no_peek_mmodel))

qfits <- quantcut(no_peek_mmodel$fit)
leveneTest(rstandard(no_peek_mmodel), qfits)
save_plot("lm_math_nopeeking_residual_boxplot", boxplot, rstandard(no_peek_mmodel)~qfits, 
          boxcol=0,boxfill=3, medlwd=2, medcol="white", cex=1.5, pch=16, 
          col='blue', xlab = "Quantiles", ylab="Standardized residuals", 
          names=c("Q1","Q2","Q3","Q4"), report=('vc*p'))

# check for autocorrelation
durbinWatsonTest(no_peek_mmodel)
durbinWatsonTest(no_peek_mmodel, method="normal")

fullModel = lm(math ~ . - id - write, data = df) 
nullModel = lm(math ~ 1, data = df) 
optimal_nopeek_mmodel = stepAIC(no_peek_mmodel,
                              direction = 'backward', 
                              scope = list(upper = fullModel, 
                                           lower = nullModel), 
                              trace = 0)
summary(optimal_nopeek_mmodel)

# Do we need to re-check preconditions here?

stargazer(optimal_nopeek_mmodel, type="latex", 
          title="Linear regression model predicting math test scores, 
          without relying on the writing tests.", ci=T, label="tab::lm_math_nopeeking", 
          df=T, out="lm_math_nopeeking.tex", report=('vc*p'))

socst_nopeek_model = lm(socst ~ genre + race + schtyp + prog + math, data=df)
summary(socst_nopeek_model)

# manually saved because of multiple functioned used for one plot
filepath = filepath_png("lm_socst_nopeeking_residual_plot")
png(filepath)
plot(socst_nopeek_model$fit, rstandard(socst_nopeek_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
# no outliers
dev.off()

# check for normality
shapiro.test(rstandard(socst_nopeek_model))

qfits <- quantcut(socst_nopeek_model$fit)
leveneTest(rstandard(socst_nopeek_model), qfits)
save_plot("lm_socst_nopeeking_residual_boxplot", boxplot, rstandard(socst_nopeek_model)~qfits, 
          boxcol=0,boxfill=3, medlwd=2, medcol="white", cex=1.5, pch=16, 
          col='blue', xlab = "Quantiles", ylab="Standardized residuals", 
          names=c("Q1","Q2","Q3","Q4"), report=('vc*p'))

# check for autocorrelation
durbinWatsonTest(socst_nopeek_model)
durbinWatsonTest(socst_nopeek_model, method="normal")

fullModel = lm(socst ~ . - id - write, data = df) 
nullModel = lm(socst ~ 1, data = df) 
optimal_socst_nopeek_model = stepAIC(socst_nopeek_model,
                                direction = 'both', 
                                scope = list(upper = fullModel, 
                                             lower = nullModel), 
                                trace = 0)
summary(optimal_socst_nopeek_model)

AIC(optimal_socst_model)
AIC(socst_nopeek_model)

stargazer(optimal_socst_nopeek_model, type="latex", 
          title="Linear regression model predicting social study test scores, 
          without relying on the writing tests.", ci=T, label="tab::lm_socst_nopeeking",
          df=T, out="lm_socst_nopeeking.tex", report=('vc*p'))