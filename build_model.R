library(ggplot2)
library(GGally)
library(tidyverse)
library(broom)
library(leaps)
library(MASS)

mydata = read.csv("dataset.csv")
attach(mydata)

# Determining parameters ==========================
# Stepwise regression with AIC
mod0 = lm(valence ~ 1)
mod.upper = lm(valence ~ danceability + energy + key + loudness + mode + speechiness
               + acousticness + instrumentalness + liveness + tempo + duration_ms
               + time_signature)
step(mod0, scope = list(lower = mod0, upper = mod.upper))
# Best subsets regression with adjusted R2
n = length(mydata[,1])
mod = regsubsets(cbind(danceability,energy,key,loudness,mode,speechiness,
                       acousticness,instrumentalness,liveness,tempo,
                       duration_ms,time_signature),valence)
summary.mod = summary(mod)
rss = summary.mod$rss
mses = c(rss[1]/(n-2),rss[2]/(n-3),rss[3]/(n-4),rss[4]/(n-5),rss[5]/(n-6),
         rss[6]/(n-7),rss[7]/(n-8),rss[8]/(n-9))
# mses
# summary.mod$adjr2
# summary.mod$which

# Model after parameters selection:
mod1 = lm(valence ~ danceability + energy + speechiness + acousticness 
          + liveness + duration_ms)

#Correlation heatmap:
mydata2 = mydata[,c(4,5,9,10,12,20)]
cormat = round(cor(mydata2),2)
library(reshape2)
melted_cormat = melt(cormat)
head(melted_cormat)
get_upper_tri = function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri = get_upper_tri(cormat)
upper_tri
melted_cormat = melt(upper_tri, na.rm = TRUE)
# Heatmap
ggheatmap = ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

mod0 = lm(valence ~ danceability + energy + speechiness + acousticness 
          + liveness + duration_ms + I(energy*acousticness))


red.mod1 = lm(valence ~ danceability + energy + duration_ms + acousticness)
fit.lm = red.mod1 # Current Model

## Determining influential points ==========================
# Determining outliers
outl = rstudent(fit.lm)
index = seq(1, length(outl), by=1)
ggplot(fit.lm, aes(x=index, y=outl)) + geom_point() + ylim(-4,4) +
  geom_hline(yintercept=-3, color="red") +
  geom_hline(yintercept=3, color="red") +
  labs(title="Studentized Deleted Residuals", x="Index", y="rstudent values")
# Determining highly leverage points
lev = hatvalues(fit.lm)
cutoff = mean(lev)*3
which(lev > cutoff) # 318 346 365 378 389 394 407 435 451 453
ggplot(fit.lm, aes(x=lev, y=rep(0,length(lev)))) + 
  geom_point(color = ifelse(lev > cutoff,'red','black')) + ylim(-0.4,0.4) +
  geom_vline(xintercept = cutoff, color = "blue")
flagged_points = data.frame(mydata[which(lev > cutoff),]) # Flagged data points
reduceddata = mydata[-c(which(lev > cutoff)),] # Reduced data without highly leverage points
rd_val = reduceddata$valence
rd_dance = reduceddata$danceability
rd_enr = reduceddata$energy
rd_dur = reduceddata$duration_ms
rd_acs = reduceddata$acousticness
rd.mod = lm(rd_val ~ rd_dance + rd_enr + rd_dur + rd_acs)

## Checking LINE conditions (1) Pre transform ==========================
# Residual vs fit plot initial model
fit.lm %>% augment() %>%
  ggplot(., aes(x = .fitted, y = .resid)) +
  geom_point()  +
  geom_hline(yintercept = 0, col=4) +
  labs(x = 'Fitted Values', y = 'Residuals') + 
  ggtitle("Residual vs Fit (Pre-transformation)")
# Normal Q-Q initial model
fit.lm %>% augment() %>%
  ggplot(., aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(col="red") +
  ggtitle("Normal Q-Q Plot (Pre-transformation)")

#Box-Cox
bc = boxcox(fit.lm,lambda = seq(-1, 1.5, length = 10))
ind = which(bc$y == max(bc$y))
maxlambda = bc$x[ind]
trnsf.mod = lm(valence^maxlambda ~ danceability + energy + duration_ms + acousticness)


## Checking LINE conditions (2) After transform ==========================
# Residual vs. fit plot
trnsf.mod %>% augment() %>%
  ggplot(., aes(x = .fitted, y = .resid)) +
  geom_point()  +
  geom_hline(yintercept = 0, col=4) +
  labs(x = 'Fitted Values', y = 'Residuals') + 
  ggtitle("Residual vs Fit (After transformation)")
# Normal Q-Q initial model
trnsf.mod %>% augment() %>%
  ggplot(., aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line(col="red") +
  ggtitle("Normal Q-Q Plot (After transformation)")
# Shapiro-Wilk
mod.resid = resid(trnsf.mod)
shapiro.test(mod.resid)