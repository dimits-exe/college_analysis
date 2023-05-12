library(haven)
path = file.path("data", "05_colleges.sav")
df = read_sav(path)
df

library(psych)
describe(df$write)
describe(df$math)
describe(df$socst)

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

qqnorm(df$write)
qqline(df$write)


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
boxplot(formula, data=df)
# not good mean

# non-parametric means test
kruskal.test(formula, data=df)
# significant differences
# show boxplots


# ===== erotima c =====



