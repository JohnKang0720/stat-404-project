# Data Preprocessing
df <- read.csv("./data.csv", header = TRUE, sep = ",")
df <- df[df$Place != "Marine Drive",]

library(dplyr)
library(tidyr)
library(MASS)

df <- df %>%
  pivot_longer(starts_with("Rep"), 
               names_to = "Speed", 
               values_to = "value") %>%
  as.data.frame()

df$Time  <- factor(df$Time)
df$Place <- factor(df$Place)
df$ID <- seq_len(nrow(df))

# ----------------------------------

# Initial Model
fit <- lm(sqrt(value) ~ Place + Time, data=df)
summary(fit)

# ----------------------------------

# Assumption Plots
par(mfrow=c(2,2))
plot(fit)

# ----------------------------------

# Box-Cox Transformation
boxcox(value ~ Place + Time, data = df,
   lambda = seq(0, 2.5, len = 21), ylab = "Log likelihood")
   
# ----------------------------------

# Effect plots
interaction.plot(df$Place, df$Time, df$value,
                 main="Interaction Plot",
                 xlab="Place",
                 ylab="Mean of Value",
                 trace.label="Time",
                 col=c("red","blue","green"),
                 lty=1,
                 lwd=2)

plot.design(value ~ Place + Time, data=df)

# ----------------------------------

# Final model
fit <- lm(sqrt(value) ~ Place + Time, data=df)
summary(fit)

# ----------------------------------

# Contrasts (main, interaction) - might need to fix/edit
SaudervsNest <- c(0, -1, 1)
IKBvsSauder <- c(-1, 0, 1)

EarlyvsLate <- c(0, -1/4, 1/4 , -1/4, 1/4)
MorningvsLunch <- c(-1/2, 0, 0, 1/2, 0)

contrasts(df$Place) <- cbind(SaudervsNest, IKBvsSauder)
contrasts(df$Time) <- cbind(EarlyvsLate, MorningvsLunch)

place.aov <- aov(sqrt(value) ~ Place, data = df)
summary(place.aov, split = list(Place = list(SaudervsNest = 1, IKBvsSauder = 2)))

time.aov <- aov(sqrt(value) ~ Time, data = df)
summary(time.aov, split = list(Time = list(EarlyvsLate = 1, MorningvsLunch = 2)))

aov_model <- aov(sqrt(value) ~ Place + Time, data = df)
summary(aov_model, split = list(
    Place = list(SaudervsNest = 1, IKBvsSauder = 2),
    Time  = list(EarlyvsLate = 1, MorningvsLunch = 2)
))