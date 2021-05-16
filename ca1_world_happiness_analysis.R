# ============================================================
# SECTION 1: DATA PREPARATION
# ============================================================

# 1. Read dataset
# ============================================================
# Read dataset from csv file
world_happiness <- read.csv('World-happiness.csv', na='')

# Examine the structure of the dataset
str(world_happiness)

# The total rows
nrow(world_happiness)
# 1949


# 2. Examine the missing data 
# ============================================================
# List rows with missing values
incomplete_data <- world_happiness[!complete.cases(world_happiness),]

# Show the number rows of missing data
nrow(incomplete_data)
# 241

# Visualize the missing data
# install packages("VIM")
library(VIM)
incomplete_data <- aggr(world_happiness, prop = FALSE, numbers = TRUE)
summary(incomplete_data)

# Comment: 
# The number row of missing data is 241, 
# while the total rows of the original dataset is 1949
# The percentage of missing data: 241/1949 = 0.1236 = 12.36%
# 12.36% is NOT a small number, so do not remove all missing data rows.
# I will check in particular sub-dataset(s) in the next section.


# ============================================================
# SECTION 2: HYPOTHESIS TEST
# ============================================================

# Show general correlation between variables
# install.packages("psych")
library(psych)
pairs.panels(world_happiness, 
             smooth = TRUE, # If TRUE, draws loess smooths    
             scale = FALSE, # If TRUE, scales the correlation text font    
             density = TRUE, # If TRUE, adds density plots and histograms    
             ellipses = TRUE, # If TRUE, draws ellipses    
             method = "spearman", # Correlation method (also "pearson" or "kendall")    
             pch = 21, # pch symbol    
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit    
             cor = TRUE, # If TRUE, reports correlations    
             jiggle = FALSE, # If TRUE, data points are jittered    
             factor = 2, # Jittering factor    
             hist.col = 4, # Histograms color    
             stars = TRUE, # If TRUE, adds significance level with stars    
             ci = TRUE) # If TRUE, adds confidence intervals  


# 1. QUESTION 1: 
# Does "Log GDP per capita" affect "Life Ladder"?
# ============================================================
# Hypothesis Test:
# H0: "Log GDP per capita" does not affect "Life Ladder"
# H1: "Log GDP per capita" affects "Life Ladder"

# 1.1. Variables:
# ============================================================
# Log GDP per capita: Numeric (Continuous variable)
# Life Ladder: Numeric (Continuous variable)

# 1.2. Data preparation for this test
# ============================================================
# Slice the dataset with 2 variables only for analysis
gdp_vs_lifeladder <- world_happiness[, (names(world_happiness) 
                                        %in% c("Log.GDP.per.capita","Life.Ladder"))]

# Check missing values of gdp_vs_lifeladder
incomplete_data <- gdp_vs_lifeladder[!complete.cases(gdp_vs_lifeladder),]

# Show the number rows of missing data
nrow(incomplete_data)
# 36

# Remove the missing data
gdp_vs_lifeladder <- na.omit(gdp_vs_lifeladder)

# 1.3. Examine the correlation between 2 variables
# ============================================================

# Use plot() first to examine correlations between 2 variables
attach(gdp_vs_lifeladder)

opar = par(no.readonly = TRUE)
# Plot with 1 column
par(mfrow = c(1,1))

plot(Log.GDP.per.capita, 
     Life.Ladder, 
     pch = 19, 
     col = "blue",
     main = "Correlation between Log GDP per capita and Life Ladder", 
     xlab = "Log GDP per capita", 
     ylab = "Life Ladder")

# Use QQ plot to show the correlation between 2 variables
with(gdp_vs_lifeladder,
     qqplot(Log.GDP.per.capita, 
            Life.Ladder,
            main = "Correlation between Log GDP per capita and Life Ladder",
            xlab = "Log GDP per capita",
            ylab = "Life Ladder"))

# 1.4. Check whether the variables are normally distributed or not
# ============================================================

# Use histogram to show the distribution plots 
opar = par(no.readonly = TRUE)
# Plot with 2 columns
par(mfrow = c(1,2))

hist(Log.GDP.per.capita, col = "blue", 
     main = "Disribution of Log GDP per capita")
hist(Life.Ladder, col = "blue", 
     main = "Distribution of Life Ladder")

# Use Quantile-quantile plot to check if the data is distributed normally or not
qqnorm(Log.GDP.per.capita,
       main = "Normal QQ plot of Log GDP per capita",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Log.GDP.per.capita, col = 'red')

qqnorm(Life.Ladder,
       main = "Normal QQ plot of Life Ladder",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Life.Ladder, col = 'red')

# Use the Shapiro-Wilks test
normality_test <- shapiro.test(Log.GDP.per.capita)
normality_test$p.value
# p-value = 2.342605e-20 < 0.05
# So the Log GDP per capita variable is not normally distributed

normality_test <- shapiro.test(Life.Ladder)
normality_test$p.value
# p-value = 1.409681e-12 < 0.05
# So the Life Ladder variable is not normally distributed

# 1.5. Hypothesis testing:
# ============================================================

# Independent variable: Log GDP per capita
# Dependent variable = Life Ladder

# Both variables are continuous and the dependent variable is NOT normally distributed, 
# We need to use a Non-parametric test
# ==> the Spearman’s Correlation Coefficient

corr <- cor.test(Log.GDP.per.capita,
                 Life.Ladder, 
                 method = 'spearman')
corr

# rho = 0.8037236 
# p-value < 2.2e-16

# The p-value is < 0.05 (the Cut-off) so we reject H0. 
# Therefore "Log GDP per capita" affects "Life Ladder" (p < 2.2e-16)

# rho = 0.8037236, there is a positive correlation 
# between "Log GDP per capita" and "Life Ladder"

detach(gdp_vs_lifeladder)



# 2. QUESTION 2: 
# Does "Freedom to make life choices" affect "Positive affect"?
# ============================================================
# Hypothesis Test:
# H0: "Freedom to make life choices" does not affect "Positive affect"
# H1: "Freedom to make life choices" affects "Positive affect"

# 2.1. Variables:
# ============================================================
# Freedom to make life choices: Numeric (Continuous variable)
# Positive affect: Numeric (Continuous variable)

# 2.2. Data preparation for this test
# ============================================================
# Slice the dataset with 2 variables only for analysis
freedom_vs_positive <- world_happiness[, (names(world_happiness) 
                                        %in% c("Freedom.to.make.life.choices",
                                               "Positive.affect"))]

# Check missing values of freedom_vs_positive
incomplete_data <- freedom_vs_positive[!complete.cases(freedom_vs_positive),]

# Show the number rows of missing data
nrow(incomplete_data)
# 46

# Remove the missing data
freedom_vs_positive <- na.omit(freedom_vs_positive)

# 2.3. Examine the correlation between 2 variables
# ============================================================

# Use plot() first to examine correlations between 2 variables
attach(freedom_vs_positive)

opar = par(no.readonly = TRUE)
# Plot with 1 column
par(mfrow = c(1,1))

plot(Freedom.to.make.life.choices, 
     Positive.affect, 
     pch = 19, 
     col = "blue",
     main = "Correlation between Freedom to make life choices and Positive affect", 
     xlab = "Freedom to make life choices", 
     ylab = "Positive affect")

# Use QQ plot to show the correlation between 2 variables
with(freedom_vs_positive,
     qqplot(Freedom.to.make.life.choices, 
            Positive.affect,
            main = "Correlation between Freedom to make life choices and Positive affect",
            xlab = "Freedom to make life choices",
            ylab = "Positive affect"))

# 2.4. Check whether the variables are normally distributed or not
# ============================================================

# Use histogram to show the distribution plots 
opar = par(no.readonly = TRUE)
# Plot with 2 columns
par(mfrow = c(1,2))

hist(Freedom.to.make.life.choices, col = "blue", 
     main = "Disribution of Freedom to make life choices")
hist(Positive.affect, col = "blue", 
     main = "Distribution of Positive affect")

# Use Quantile-quantile plot to check if the data is distributed normally or not
qqnorm(Freedom.to.make.life.choices,
       main = "Normal QQ plot of Freedom to make life choices",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Freedom.to.make.life.choices, col = 'red')

qqnorm(Positive.affect,
       main = "Normal QQ plot of Positive affect",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Positive.affect, col = 'red')

# Use the Shapiro-Wilks test
normality_test <- shapiro.test(Freedom.to.make.life.choices)
normality_test$p.value
# p-value = 6.378868e-22 < 0.05
# So the Freedom to make life choices variable is not normally distributed

normality_test <- shapiro.test(Positive.affect)
normality_test$p.value
# p-value = 2.630667e-18 < 0.05
# So the Positive affect variable is not normally distributed

# 2.5. Hypothesis testing:
# ============================================================

# Independent variable: Freedom to make life choices
# Dependent variable = Positive affect

# Both variables are continuous and the dependent variable is NOT normally distributed, 
# We need to use a Non-parametric test
# ==> the Spearman’s Correlation Coefficient

corr <- cor.test(Freedom.to.make.life.choices,
                 Positive.affect, 
                 method = 'spearman')
corr
# rho = 0.6093102 
# p-value < 2.2e-16

# The p-value is < 0.05 (the Cut-off) so we reject H0. 
# Therefore "Freedom to make life choices" affects "Positive affect" (p < 2.2e-16)

# rho = 0.6093102, there is a positive correlation 
# between "Freedom to make life choices" and "Positive affect"

detach(freedom_vs_positive)



# 3. QUESTION 3: 
# Does "Perceptions of corruption" affect "Negative affect"?
# ============================================================
# Hypothesis Test:
# H0: "Perceptions of corruption" does not affect "Negative affect"
# H1: "Perceptions of corruption" affects "Negative affect"

# 3.1. Variables:
# ============================================================
# Perceptions of corruption: Numeric (Continuous variable)
# Negative affect: Numeric (Continuous variable)

# 3.2. Data preparation for this test
# ============================================================
# Slice the dataset with 2 variables only for analysis
corruption_vs_negative <- world_happiness[, (names(world_happiness) 
                                          %in% c("Perceptions.of.corruption",
                                                 "Negative.affect"))]

# Check missing values of corruption_vs_negative
incomplete_data <- corruption_vs_negative[!complete.cases(corruption_vs_negative),]

# Show the number rows of missing data
nrow(incomplete_data)
# 116

# Remove the missing data
corruption_vs_negative <- na.omit(corruption_vs_negative)

# 3.3. Examine the correlation between 2 variables
# ============================================================

# Use plot() first to examine correlations between 2 variables
attach(corruption_vs_negative)

opar = par(no.readonly = TRUE)
# Plot with 1 column
par(mfrow = c(1,1))

plot(Perceptions.of.corruption, 
     Negative.affect, 
     pch = 19, 
     col = "blue",
     main = "Correlation between Perceptions of corruption and Negative affect", 
     xlab = "Perceptions of corruption", 
     ylab = "Negative affect")

# Use QQ plot to show the correlation between 2 variables
with(corruption_vs_negative,
     qqplot(Perceptions.of.corruption, 
            Negative.affect,
            main = "Correlation between Perceptions of corruption and Negative affect",
            xlab = "Perceptions of corruption",
            ylab = "Negative affect"))

# 3.4. Check whether the variables are normally distributed or not
# ============================================================

# Use histogram to show the distribution plots 
opar = par(no.readonly = TRUE)
# Plot with 2 columns
par(mfrow = c(1,2))

hist(Perceptions.of.corruption, col = "blue", 
     main = "Disribution of Perceptions of corruption")
hist(Negative.affect, col = "blue", 
     main = "Distribution of Negative affect")

# Use Quantile-quantile plot to check if the data is distributed normally or not
qqnorm(Perceptions.of.corruption,
       main = "Normal QQ plot of Perceptions of corruption",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Perceptions.of.corruption, col = 'red')

qqnorm(Negative.affect,
       main = "Normal QQ plot of Negative affect",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Negative.affect, col = 'red')

# Use the Shapiro-Wilks test
normality_test <- shapiro.test(Perceptions.of.corruption)
normality_test$p.value
# p-value = 6.892285e-39 < 0.05
# So the Perceptions of corruption variable is not normally distributed

normality_test <- shapiro.test(Negative.affect)
normality_test$p.value
# p-value = 6.166824e-20 < 0.05
# So the Negative affect variable is not normally distributed

# 3.5. Hypothesis testing:
# ============================================================

# Independent variable: Perceptions of corruption
# Dependent variable = Negative affect

# Both variables are continuous and the dependent variable is NOT normally distributed, 
# We need to use a Non-parametric test
# ==> the Spearman’s Correlation Coefficient

corr <- cor.test(Perceptions.of.corruption,
                 Negative.affect, 
                 method = 'spearman')
corr
# rho = 0.2084022  
# p-value < 2.2e-16

# The p-value is < 0.05 (the Cut-off) so we reject H0. 
# Therefore "Perceptions of corruption" affects "Negative affect" (p < 2.2e-16)

# rho = 0.2084022, there is a positive correlation 
# between "Perceptions of corruption" and "Negative affect"

detach(corruption_vs_negative)



# 4. QUESTION 4: 
# Does "Log GDP per capita" affects "Healthy life expectancy at birth"?
# ============================================================
# Hypothesis Test:
# H0: "Log GDP per capita" does not affect "Healthy life expectancy at birth"
# H1: "Log GDP per capita" affects "Healthy life expectancy at birth"

# 4.1. Variables:
# ============================================================
# Log GDP per capita: Numeric (Continuous variable)
# Healthy life expectancy at birth: Numeric (Continuous variable)

# 4.2. Data preparation for this test
# ============================================================
# Slice the dataset with 2 variables only for analysis
gdp_vs_lifeexpectancy <- world_happiness[, (names(world_happiness) 
                                        %in% c("Log.GDP.per.capita",
                                               "Healthy.life.expectancy.at.birth"))]

# Check missing values of gdp_vs_lifeexpectancy
incomplete_data <- gdp_vs_lifeexpectancy[!complete.cases(gdp_vs_lifeexpectancy),]

# Show the number rows of missing data
nrow(incomplete_data)
# 73

# Remove the missing data
gdp_vs_lifeexpectancy <- na.omit(gdp_vs_lifeexpectancy)

# 4.3. Examine the correlation between 2 variables
# ============================================================

# Use plot() first to examine correlations between 2 variables
attach(gdp_vs_lifeexpectancy)

opar = par(no.readonly = TRUE)
# Plot with 1 column
par(mfrow = c(1,1))

plot(Log.GDP.per.capita, 
     Healthy.life.expectancy.at.birth, 
     pch = 19, 
     col = "blue",
     main = "Correlation between Log GDP per capita and Healthy life expectancy at birth", 
     xlab = "Log GDP per capita", 
     ylab = "Healthy life expectancy at birth")

# Use QQ plot to show the correlation between 2 variables
with(gdp_vs_lifeexpectancy,
     qqplot(Log.GDP.per.capita, 
            Healthy.life.expectancy.at.birth,
            main = "Correlation between Log GDP per capita and Healthy life expectancy at birth",
            xlab = "Log GDP per capita",
            ylab = "Healthy life expectancy at birth"))

# 4.4. Check whether the variables are normally distributed or not
# ============================================================

# Use histogram to show the distribution plots 
opar = par(no.readonly = TRUE)
# Plot with 2 columns
par(mfrow = c(1,2))

hist(Log.GDP.per.capita, col = "blue", 
     main = "Disribution of Log GDP per capita")
hist(Healthy.life.expectancy.at.birth, col = "blue", 
     main = "Distribution of Healthy life expectancy at birth")

# Use Quantile-quantile plot to check if the data is distributed normally or not
qqnorm(Log.GDP.per.capita,
       main = "Normal QQ plot of Log GDP per capita",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Log.GDP.per.capita, col = 'red')

qqnorm(Healthy.life.expectancy.at.birth,
       main = "Normal QQ plot of Healthy life expectancy at birth",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Healthy.life.expectancy.at.birth, col = 'red')

# Use the Shapiro-Wilks test
normality_test <- shapiro.test(Log.GDP.per.capita)
normality_test$p.value
# p-value = 3.927664e-20 < 0.05
# So the Log GDP per capita variable is not normally distributed

normality_test <- shapiro.test(Healthy.life.expectancy.at.birth)
normality_test$p.value
# p-value = 1.02031e-25 < 0.05
# So the Healthy life expectancy at birth variable is not normally distributed

# 4.5. Hypothesis testing:
# ============================================================

# Independent variable: Log GDP per capita
# Dependent variable = Healthy life expectancy at birth

# Both variables are continuous and the dependent variable is NOT normally distributed, 
# We need to use a Non-parametric test
# ==> the Spearman’s Correlation Coefficient

corr <- cor.test(Log.GDP.per.capita,
                 Healthy.life.expectancy.at.birth, 
                 method = 'spearman')
corr

# rho = 0.8586289 
# p-value < 2.2e-16

# The p-value is < 0.05 (the Cut-off) so we reject H0. 
# Therefore "Log GDP per capita" affects "Healthy life expectancy at birth" (p < 2.2e-16)

# rho = 0.8586289, there is a positive correlation 
# between "Log GDP per capita" and "Healthy life expectancy at birth"

detach(gdp_vs_lifeexpectancy)



# 5. QUESTION 5:
# Does "Generosity" affect "Life Ladder"?
# ============================================================
# Hypothesis Test:
# H0: "Generosity" does not affect "Life Ladder"
# H1: "Generosity" affects "Life Ladder"

# 5.1. Variables:
# ============================================================
# Generosity: Numeric (Continuous variable)
# Life Ladder: Numeric (Continuous variable)

# 5.2. Data preparation for this test
# ============================================================
# Slice the dataset with 2 variables only for analysis
generosity_vs_lifeladder <- world_happiness[, (names(world_happiness) 
                                        %in% c("Generosity","Life.Ladder"))]

# Check missing values of generosity_vs_lifeladder
incomplete_data <- generosity_vs_lifeladder[!complete.cases(generosity_vs_lifeladder),]

# Show the number rows of missing data
nrow(incomplete_data)
# 89

# Remove the missing data
generosity_vs_lifeladder <- na.omit(generosity_vs_lifeladder)

# 5.3. Examine the correlation between 2 variables
# ============================================================

# Use plot() first to examine correlations between 2 variables
attach(generosity_vs_lifeladder)

opar = par(no.readonly = TRUE)
# Plot with 1 column
par(mfrow = c(1,1))

plot(Generosity, 
     Life.Ladder, 
     pch = 19, 
     col = "blue",
     main = "Correlation between Generosity and Life Ladder", 
     xlab = "Generosity", 
     ylab = "Life Ladder")

# Use QQ plot to show the correlation between 2 variables
with(generosity_vs_lifeladder,
     qqplot(Generosity, 
            Life.Ladder,
            main = "Correlation between Generosity and Life Ladder",
            xlab = "Generosity",
            ylab = "Life Ladder"))

# 5.4. Check whether the variables are normally distributed or not
# ============================================================

# Use histogram to show the distribution plots 
opar = par(no.readonly = TRUE)
# Plot with 2 columns
par(mfrow = c(1,2))

hist(Generosity, col = "blue", 
     main = "Disribution of Generosity")
hist(Life.Ladder, col = "blue", 
     main = "Distribution of Life Ladder")

# Use Quantile-quantile plot to check if the data is distributed normally or not
qqnorm(Generosity,
       main = "Normal QQ plot of Generosity",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Generosity, col = 'red')

qqnorm(Life.Ladder,
       main = "Normal QQ plot of Life Ladder",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
qqline(Life.Ladder, col = 'red')

# Use the Shapiro-Wilks test
normality_test <- shapiro.test(Generosity)
normality_test$p.value
# p-value = 1.644733e-21 < 0.05
# So the Generosity variable is not normally distributed

normality_test <- shapiro.test(Life.Ladder)
normality_test$p.value
# p-value = 2.53242e-12 < 0.05
# So the Life Ladder variable is not normally distributed

# 5.5. Hypothesis testing:
# ============================================================

# Independent variable: Generosity
# Dependent variable = Life Ladder

# Both variables are continuous and the dependent variable is NOT normally distributed, 
# We need to use a Non-parametric test
# ==> the Spearman’s Correlation Coefficient

corr <- cor.test(Generosity,
                 Life.Ladder, 
                 method = 'spearman')
corr

# rho = 0.1642112 
# p-value < 1.035e-12

# The p-value is < 0.05 (the Cut-off) so we reject H0. 
# Therefore "Generosity" affects "Life Ladder" (p < 1.035e-12)

# rho = 0.1642112, there is a positive correlation 
# between "Generosity" and "Life Ladder"

detach(generosity_vs_lifeladder)
