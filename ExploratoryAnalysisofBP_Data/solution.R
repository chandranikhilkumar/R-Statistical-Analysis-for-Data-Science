#generate a function to generate random numbers following the bernoulli

#distribution using the Monte Carlo method conf.int <‐ function(n, p, alpha) { 
x = runif(n)

y = (x<p)

z = as.numeric(y) #generate n random numbers following Bernoulli(p)




#we then calculate the sample mean, standard deviation and the confidence interval;

mu = mean(z)

sigma = sqrt(mu*(1‐mu))

ci <‐ mu + c(‐1, 1) * qnorm(1 ‐ (alpha/2)) * sigma/sqrt(n)

return(ci)

}




#define p, aplha and n values;

p = 0.90 alpha = 0.05 n=5
conf.int(n, p, alpha)




#calculate the nominal confidence interval

nominal.ci = p + c(‐1,1) * qnorm(1 ‐ (alpha/2)) * sqrt(p*(1‐p)/n)

nominal.ci
