# We donwload 3 assets 
# Coke - Amazon - Google
library(quantmod)
getSymbols(c('AMZN','COKE','GOOG'),
           src='yahoo',
           return.class='zoo', 
           from='2019-01-01',
           to='2019-12-31')
plot(AMZN$AMZN.Adjusted)

sAll=merge(AMZN$AMZN.Adjusted,
        COKE$COKE.Adjusted,
        GOOG$GOOG.Adjusted,
        all=FALSE)
plot(sAll)

dlnSstocks=diff(log(sAll))
plot(dlnSstocks)

VCM=var(dlnSstocks)/dt
View(VCM)

sqrt(diag(VCM))

mu=apply(dlnSstocks,2,mean)/dt+0.5*diag(VCM)
View(mu)

getSymbols('DTB3', src='FRED', return.class='zoo')

plot(DTB3)
r = na.omit(DTB3)/100
plot(r)
plot(window(r, start='2019-01-01', end='2019-12-31' ))

r=mean(window(r, start='2019-01-01', end='2019-12-31' ))
r

PTF=solve(VCM)%*%(mu-r)
PTF
sum(PTF)

1-sum(PTF)

1/8*PTF

# Define the GOP
theta_GOP=solve(VCM)%*%(mu-r)

#Define risk aversion
delta=7.25

theta_U=1/delta*theta_GOP

#Safety first portfolio 
mu_E=0.10
xi2=as.numeric((mu-r)%*%solve(VCM)%*%(mu-r))
theta_SF=sqrt(2*(mu_E-r)/xi2)*theta_GOP


# Mean variance portfolio
mu_E=0.12
theta_MV=(mu_E-r)/xi2*theta_GOP
theta_MKT=theta_GOP/sum(theta_GOP)
theta_MV/sum(theta_MV)
theta_SF/sum(theta_SF)

