library(ggplot2)
library(tidyverse)
library(broom)
library(leaps)

db = read.csv("dataset.csv")
attach(db)

# Determining parameters ==========================
# Stepwise regression with AIC
mod0 = lm(valence ~ 1)
mod.upper = lm(valence ~ danceability + energy + key + loudness + mode + speechiness
               + acousticness + instrumentalness + liveness + tempo + duration_ms
               + time_signature)
step(mod0, scope = list(lower = mod0, upper = mod.upper))
# Best subsets regression with adjusted R2
n = length(db[,1])
mod = regsubsets(cbind(danceability,energy,key,loudness,mode,speechiness,
                       acousticness,instrumentalness,liveness,tempo,
                       duration_ms,time_signature),valence)
summary.mod = summary(mod)
rss = summary.mod$rss
mses = c(rss[1]/(n-2),rss[2]/(n-3),rss[3]/(n-4),rss[4]/(n-5),rss[5]/(n-6),
         rss[6]/(n-7),rss[7]/(n-8),rss[8]/(n-9))
mses
summary.mod$adjr2
summary.mod$which

# Model after parameters selection:
mod1 = lm(valence ~ danceability + energy + speechiness + acousticness 
          + liveness + duration_ms)

