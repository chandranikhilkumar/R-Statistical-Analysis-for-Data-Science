# Read the prostate cancer data

prostate <- read.table("C:/Users/yxl121030/My Documents/R/prostate_cancer.csv", header = T, sep = ",")

# Scatterplot of psa level with other quantitative and qualitative data

plot(prostate$cancervol, prostate$psa,xlab = "Cancer Volume",ylab = "PSA Level")
plot(prostate$weight, prostate$psa,xlab = "Weight",ylab = "PSA Level")
plot(prostate$age, prostate$psa,xlab = "Age",ylab = "PSA Level")
plot(prostate$benpros, prostate$psa,xlab = "Benpros",ylab = "PSA Level")
plot(prostate$vesinv, prostate$psa,xlab = "Vesinv",ylab = "PSA Level")
plot(prostate$capspen, prostate$psa,xlab = "Capspen",ylab = "PSA Level")
plot(prostate$gleason, prostate$psa,xlab = "Gleason",ylab = "PSA Level")

attach(prostate)

# we first plot psa against all 7 variables

fit1 <- lm(psa ~ cancervol + weight + age + benpros + vesinv + capspen + gleason)

summary(fit1)

# Round 1: determine the next to remove

fit21 <- update(fit1, . ~ . - cancervol)
fit22 <- update(fit1, . ~ . - weight)
fit23 <- update(fit1, . ~ . - age)
fit24 <- update(fit1, . ~ . - benpros)
fit25 <- lm(psa ~ cancervol + weight + age + benpros + capspen + gleason)
fit26 <- update(fit1, . ~ . - capspen)
fit27 <- lm(psa ~ cancervol + weight + age + benpros + vesinv + capspen)

anova(fit1, fit21) # F-test value: 0.0009359 (*)
anova(fit1, fit22) # F-test value: 0.8787
anova(fit1, fit23) # F-test value: 0.262
anova(fit1, fit24) # F-test value: 0.2829
anova(fit1, fit25) # F-test value: 0.07519
anova(fit1, fit26) # F-test value: 0.4123
anova(fit1, fit27) # F-test value: 0.1776

# Round 2: the fit22 yields the largest P-value, remove this "weight" variable, and then determine the next one

fit31 <- update(fit22, . ~ . - cancervol)
fit32 <- update(fit22, . ~ . - age)
fit33 <- update(fit22, . ~ . - benpros)
fit34 <- lm(psa ~ cancervol + age + benpros + capspen + gleason)
fit35 <- update(fit22, . ~ . - capspen)
fit36 <- lm(psa ~ cancervol + age + benpros + vesinv + capspen)

anova(fit22, fit31) # F-test value: 0.0008356 (*)
anova(fit22, fit32) # F-test value: 0.2624
anova(fit22, fit33) # F-test value: 0.2402
anova(fit22, fit34) # F-test value: 0.07304
anova(fit22, fit35) # F-test value: 0.4103
anova(fit22, fit36) # F-test value: 0.1777

# Round 3: the fit35 yields the largest P-value, remove this "capspen" variable, and then determine the next one

fit41 <- update(fit35, . ~ . -cancervol)
fit42 <- update(fit35, . ~ . -age)
fit43 <- update(fit35, . ~ . -benpros)
fit44 <- lm(psa ~ cancervol + age + benpros + gleason)
fit45 <- lm(psa ~ cancervol + age + benpros + vesinv)

anova(fit35, fit41) # F-test value: 0.00004072 (*)
anova(fit35, fit42) # F-test value: 0.2674
anova(fit35, fit43) # F-test value: 0.2326
anova(fit35, fit44) # F-test value: 0.01641 (*)
anova(fit35, fit45) # F-test value: 0.1471

# Round 4: the fit42 yields the largest P-value, remove this "age" variable, and then determine the next one

fit51 <- update(fit42, . ~ . -cancervol)
fit52 <- update(fit42, . ~ . -benpros)
fit53 <- lm(psa ~ cancervol + benpros + gleason)
fit54 <- lm(psa ~ cancervol + benpros + vesinv)

anova(fit42, fit51) # F-test value: 0.00002973 (*)
anova(fit42, fit52) # F-test value: 0.3987
anova(fit42, fit53) # F-test value: 0.02163 (*)
anova(fit42, fit54) # F-test value: 0.2062

# Round 5: the fit52 yields the largest P-value, remove this "benpros" variable, and then detrmine the next one

fit61 <- update(fit52, . ~ . -cancervol)
fit62 <- lm(psa ~ cancervol + gleason)
fit63 <- lm(psa ~ cancervol + vesinv)

anova(fit52, fit61) # F-test value: 0.00003718 (*)
anova(fit52, fit62) # F-test value: 0.02476 (*)
anova(fit52, fit63) # F-test value: 0.1693

# Round 6: the fit63 yields the largest P-value, remove this "gleason" variable, and then detrmine the next one

fit71 <- update(fit63, . ~ . -cancervol)
fit72 <- lm(psa ~ cancervol)

anova(fit63, fit71) # F-test value: 0.00003718 (*)
anova(fit63, fit72) # F-test value: 0.02476 (*)

# Since both two P-values are significant. Thus we stop here. And the final regression will be psa ~ cancervol + vesinv

summary(fit63)
qqnorm(resid(fit63))
qqline(resid(fit63))

# calculate the predicted psa value, the b1 for cancervol is 2.477; the b2 for vesinv is 24.647; the intecept b0 is 1.060

mean1 <- mean(prostate$cancervol)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mean2 <- Mode(prostate$vesinv)

mean1
mean2


predicted_psa <- 2.477*mean1 + 24.647*mean2 + 1.060

predicted_psa
