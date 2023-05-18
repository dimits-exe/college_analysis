RESOURCE_PATH = "resources"

save_plot <- function(filename, plot_func, ...) {
  filepath = file.path(RESOURCE_PATH, paste(filename, ".png", sep = ""))
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
# https://link.springer.com/article/10.3758/BF03210951
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
# https://bpspsychub.onlinelibrary.wiley.com/doi/abs/10.1348/000711004849222
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


# =====erotima c =====
math_model = lm(math ~ .,df)
summary(math_model)

# check for outliers
plot(math_model$fit, rstandard(math_model))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
# no outliers found

# check for normality
shapiro.test(rstandard(math_model))

W#check for homogeneity
quantcut <- function(x, digits=6) { 
    cut(x, breaks=quantile(x), include.lowest=TRUE, dig.lab = digits) 
}

qfits <- quantcut(math_model$fit)
leveneTest(rstandard(math_model), qfits)
boxplot(rstandard(math_model)~qfits, boxcol=0,boxfill=3, medlwd=2, 
        medcol="white", cex=1.5, pch=16, col='blue', xlab = "Quantiles", 
        ylab="Standardized residuals")

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
plot(socst_model_orig$fit, rstandard(socst_model_orig))
abline(h=-1.96, col='red', lwd=2, lty=2)
abline(h=1.96, col='red', lwd=2, lty=2)
#identify(socst_model_orig$fit, rstandard(socst_model_orig),n=1)
# outlier 163 found

df[163,]

socst_model = lm(socst ~ . -id ,df[-163,])
summary(socst_model)

# check for normality
shapiro.test(rstandard(socst_model))

qfits <- quantcut(socst_model$fit)
leveneTest(rstandard(socst_model), qfits)
boxplot(rstandard(socst_model)~qfits, boxcol=0,boxfill=3, medlwd=2, 
        medcol="white", cex=1.5, pch=16, col='blue', xlab = "Quantiles", 
        ylab="Standardized residuals")

# check for autocorrelation
durbinWatsonTest(socst_model)
durbinWatsonTest(socst_model, method="normal")

library(MASS)
fullModel = lm(socst ~ . - id, data = df[-163,]) 
nullModel = lm(socst ~ 1, data = df[-163,]) 
optimal_socst_model = stepAIC(socst_model,
                direction = 'both', 
                scope = list(upper = fullModel, 
                             lower = nullModel), 
                trace = 0) # do not show the step-by-step process of model selection
summary(optimal_socst_model)

# Do we need to re-check preconditions here?

stargazer(optimal_socst_model, type="latex", 
          title="Linear regression model predicting social study test scores, taking into 
          account other test scores.", ci=T, label="tab::lm_socst_peeking", df=T,
          out="lm_socst_peeking.tex")