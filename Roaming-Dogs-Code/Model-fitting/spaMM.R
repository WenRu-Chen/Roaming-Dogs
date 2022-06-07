library(MASS)
library(spaMM)

rSample <- function(nb,rho,sigma2_u,resid,intercept,slope,pairs=TRUE) {
  ## sample pairs of adjacent locations
  if (pairs) {
    x <- rnorm(nb/2); x <- c(x,x+0.001)
    y <- rnorm(nb/2); y <- c(y,y+0.001)
  } else {x <- rnorm(nb);y <- rnorm(nb)}
  dist <- dist(cbind(x,y)) ## distance matrix between locations
  m <- exp(-rho*as.matrix(dist)) ## correlation matrix
  b <- mvrnorm(1,rep(0,nb),m*sigma2_u) ## correlated random effects
  pred <- sample(nb) ## some predictor variable
  obs <- intercept+slope*pred + b +rnorm(nb,0,sqrt(resid)) ## response
  data.frame(obs=obs,x,y,pred=pred)
}
set.seed(123)
d1 <- rSample(nb=40,rho=3,sigma2_u=0.5,resid=0.5,intercept=-1,slope=0.1)


HL1 <- spaMM::fitme(obs~pred+Matern(1|x+y),data=d1,fixed=list(nu=0.5),method="REML")

plot(HL1)

res.pear = residuals(HL1,type = c("pearson")) # c("deviance", "pearson", "response", "std_dev_res")
res.resp = residuals(HL1,type = c("response"))

#======
data(scotlip)
lipfit <- HLCor(cases~I(prop.ag/10)+adjacency(1|gridcode)
               +offset(log(expec)),
               data=scotlip,family=poisson(),adjMatrix=Nmatrix)

#====

require(graphics)
library(CompGLM)

 comp = data.frame(nu = numeric(),lam = numeric(), x = numeric() )

for (i in c(0.5, 1, 1.5, 2)){
  sample <- rcomp(n = 10000, lam = 5, nu = i)
  dt = data.frame(nu = i,lam = 5, x = sample)
  comp = dt %>% rbind(dt)
  
}

sample_nu_1 <- rcomp(n = 10000, lam = 5, nu = 1)
print(mean(sample_nu_1))
sample_nu_.5 <- rcomp(n = 10000, lam = 5, nu = .5)
sample_nu_1.5 <- rcomp(n = 10000, lam = 5, nu = 1.5)
sample_nu_2 <- rcomp(n = 10000, lam = 5, nu = 2)
barplot(table(sample))

data("scotlip")

