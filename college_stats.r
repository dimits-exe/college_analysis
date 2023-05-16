RESOURCE_PATH = "resources"

save_plot <- function(filename, plot_func, ...) {
  filepath = file.path(RESOURCE_PATH, paste(filename, ".png"))
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
corr.test(df[6:8], adjust="holm", minlength = 3)
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

# use Welch test regardless of variance analysis
# https://bpspsychub.onlinelibrary.wiley.com/doi/abs/10.1348/000711004849222
t.test(men_write$write, women_write$write)
t.test(men_write$write, women_write$write, alternative = "less")


formula = write ~ prog
anova <- aov(formula, data=df)
summary(anova)

# variance analysis
library(car)
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

