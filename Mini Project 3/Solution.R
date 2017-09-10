#Section 2: R scripts

#We read the original txt file and then rextracted the data for the Bass, Tenor, Alto and Soprano groups.
singer.dat = read.table("C:/Users/yxl121030/My Documents/R/singer.txt", header = T, sep = ",")
soprano.dat = singer.dat[singer.dat$voice.part == "Soprano", c("height", "voice.part")]
soprano = soprano.dat$height
alto.dat = singer.dat[singer.dat$voice.part == "Alto", c("height", "voice.part")]
alto = alto.dat$height
tenor.dat = singer.dat[singer.dat$voice.part == "Tenor", c("height", "voice.part")]
tenor = tenor.dat$height
bass.dat = singer.dat[singer.dat$voice.part == "Bass", c("height", "voice.part")]
bass = bass.dat$height

#for part (a), We constructed the box plot for the exploratory analysis.
boxplot(bass, tenor, alto, soprano, main="Box plot for singers", names = c("Bass", "Tenor", "Alto", "Soprano")) 

#for part (b), we calculated the p-value for the hypothesis testing as stated in Section 1.
number.bass = length(bass)
mu.bass = mean(bass)
sd.bass = sd(bass)
number.tenor = length(tenor)
mu.tenor = mean(tenor)
sd.tenor = sd(tenor)

Zobs = (mu.bass - mu.tenor) / sqrt( (sd.bass^2)/number.bass + (sd.tenor^2)/number.tenor)

p_value = 1-pnorm(Zobs)
p_value





#R scripts

#We first calculate the T-statistics using the degrees of freedom 19.
num2 = 20
mu2 = 9.02
sd2 = 2.22

obs2 = (9.02 - 10) / (sd2/sqrt(num2))
obs2

#We then use the regular method to calculate the p value.
p2 = 1 - pt(obs2, df = num2 - 1)
p2

#We then use the Monte Carlo method to generate 10,000 random variables which follow the standard T-distribution
#with the degrees of freedom 19, and then calculated the p value.
N=10000
list2 = rt(N, df = num2 - 1)
lower_count = length(list2[list2 >= obs2])
p2_montecarlo = (lower_count + 1) / (N + 1)
p2_montecarlo

##We also used the Monte Carlo method to generate 10,000 random variables which follow the Normal distribution
list3 = rnorm(10000, 0, 1)
lower_count2 = length(list3[list3 >= obs2])
p3_montecarlo = (lower_count2 + 1) / (N + 1)
p3_montecarlo




#R scripts

#We first calculated the 95% confidence interval for the difference in mean credit
#limits of all credit cards issued in January 2011 and in May 2011.
num_jan = 400
num_may = 500
mu_jan = 2635
mu_may = 2887
sd_jan = 365
sd_may = 412
alpha = 0.05

mu_diff = mu_may - mu_jan
se_diff = sqrt((sd_jan^2)/num_jan + (sd_may^2)/num_may)

ci = mu_diff + c(-1, 1) * qnorm(1 - (alpha/2)) * se_diff
ci

#We then caculated the p value for the hypothesis testing.
Zobs = mu_diff/se_diff
Zobs

p3 = 1 - pnorm(Zobs)
p3

