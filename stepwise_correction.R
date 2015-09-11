#Jeffrey Uslan
#fabricate some on-off data
len=1000
good_vec=round(runif(len,max=.75))
plot(good_vec)
noise=rnorm(len,-.1,.1)
#give it noise
noise_vec=good_vec+noise
plot(noise_vec)

#introduce some "step noise"
steps=round(rnorm(20,-1,1))
t=sapply(steps,function(x) x*rep(1,50))
steps_10=NULL
for(i in 1:ncol(t)) steps_10=c(steps_10,t[,i])
step_noise=noise_vec+steps_10
plot(step_noise)

#try to fix using moving median
filter_window=5
filt=runmed((step_noise),filter_window)

plot(filt)
title("note the averaging issue between steps")
unstep_noise=step_noise-filt
plot(unstep_noise,type="p")
title("attempted adjustments")


#fit a step function fit
df=data.frame(x=1:len,y=step_noise)
fit=lm(df$y~0+cut(df$x,20))

step_fit=as.vector((coef(summary(fit))[,1]))
t=sapply(step_fit,function(x) x*rep(1,50))
steps_10=NULL
for(i in 1:ncol(t)) steps_10=c(steps_10,t[,i])
unstep_noise_cut=step_noise-steps_10
plot(unstep_noise_cut)
title("much better")


par(mfrow=c(1,1))
plot(noise_vec)
plot(unstep_noise,type="p")
plot(unstep_noise_cut)
