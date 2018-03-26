library(pomp)
library(ggplot2)

dat = read.csv("https://ionides.github.io/531w18/10/parus.csv")

parus = pomp(dat,times='year',t0=1959)

#Create skeleton
skel = Csnippet("
        DN=r*N*exp(-N/k);
")

#Create pomp object
parus = pomp(parus,skeleton=map(skel),paramnames=c("r","k"),statenames=c("N"))

traj = trajectory(parus,params=c(N.0=1,r=8,k=1),as.data.frame=TRUE)

par(mfrow=c(1,2))
#Plot data
plot(dat,type='l',xlab = 'Year',ylab='Population',main='Parus Data',bty='n')
#Plot the deterministic trajectory
plot(traj$N~traj$time,type='l',bty='n',ylab='Population Density (N)',xlab='Year',main='Trajectory\nN.0=1, r=8, k=1')

#Add stochasticity to ricker equation
stochStep <- Csnippet("
  e = rnorm(0,sigma);
  N = r*N*exp(-N/k+e);
")
parus = pomp(parus,
             rprocess=discrete.time.sim(step.fun=stochStep,delta.t=1),
             paramnames=c("r","k","sigma"),
             statenames=c("N","e"))

sim = simulate(parus,
               params=c(N.0=1,e.0=0,r=8,k=1,sigma=0.5),
               as.data.frame=TRUE,states=TRUE)
plot(N~time,data=sim,type='l',xlab='Year',ylab='N',
      main='Population Density Trajectory & Simulations')
lines(N~time,data=traj,type='l',col='blue')

#Modify measurement model to reflect negative binomial distribution
rmeas = Csnippet("pop = rnbinom(phi*N*psi/(1-psi),psi);")
dmeas = Csnippet("lik = dnbinom(pop,phi*N*psi/(1-psi),psi,give_log);")

parus = pomp(parus,
             rmeasure=rmeas,
             dmeasure=dmeas,
             statenames=c("N"),
             paramnames=c("phi","psi"))

#One simulation from set of parameters showing oscillatory behavior
coef(parus) = c(N.0=100,e.0=0,r=6,k=125,sigma=0.01,phi=1,psi=0.1)
sims = simulate(parus,nsim=1,as.data.frame=TRUE,include.data=TRUE)
ggplot(data=sims,mapping=aes(x=time,y=pop))+geom_line()+
  facet_wrap(~sim)+ggtitle()