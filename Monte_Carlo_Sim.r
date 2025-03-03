library(quantmod)

getSymbols('^GSPC', src='yahoo', return.class='zoo', from='1920-01-01')


View(GSPC)
S=GSPC$GSPC.Adjusted
plot(S)
abline(v=as.Date('1995-01-01'),
       lty=2, col='blue')
abline(v=as.Date('2008-09-15'),
       lty=2,col='red') 

dlnS=diff(log(S))
plot(dlnS)

dt=1/250
sigma=sqrt(var(dlnS)/dt)
mu=mean(dlnS)/dt+0.5*sigma^2

# Download DTB3 

getSymbols('DTB3', src = 'FRED', return.class='zoo')

plot(DTB3)
r=na.omit(DTB3)/100
plot(r)

mean(r)

#Monte Carlo simulation

T=5 #years to be simulated 
N=100 #number of simulations
S0=25 #starting value

Sim=array(NA, dim=c(T/dt,N))
Sim[1,]=S0

View(Sim) #not called properly but keep


dW=array(rnorm(T/dt*N,0,sqrt(dt)), dim=c(T/dt,N))

for (i in 2:(T/dt)){
  Sim[i,]=Sim[i-1,]+
  Sim[i-1,]*sigma*dW[i-1,]
    }
View(Sim)
matplot(Sim, type='l')
abline(h=55,lwd=3,lty=3)

