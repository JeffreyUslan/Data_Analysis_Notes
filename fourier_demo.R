

set.seed(1)
acq.freq <- 200
t=seq(1,acq.freq+1)
time     <- 1
w        <- 2*pi/time
ts       <- seq(0,time,1/acq.freq)

t_clean=4*sin(3*w*ts)
fft_t_clean=fft(t_clean)
plot(Mod(fft_t_clean)[0:(acq.freq/2)])

t1 <- t_clean +3*rnorm(acq.freq+1)
plot(Mod(fft(t1))[0:(acq.freq/2)])

trajectory <- t1+seq(1,acq.freq+1)/10
plot(Mod(fft(trajectory))[0:(acq.freq/2)])

plot(trajectory, type="l")
lines(t1, type="l",col="blue")
lines(t_clean, type="l",col="red")

#Is there lost information? No
X.k <- fft(trajectory)
X.unf=fft(X.k,inverse=TRUE)/length(trajectory)
X.unf=Mod(X.unf)
plot(X.unf,type="l")
lines(trajectory, type="l",col="blue")
mean(abs(X.unf-trajectory))

#demo1
#sinusoidal with noise
# fix with fourier
X.hold=fft(t1)
plot(Mod(X.hold)[0:(acq.freq/2)])

thresh= 200
X.hold[which(Mod(X.hold)<thresh)]=0
X.unf=fft(X.hold,inverse=TRUE)/length(trajectory)
X.unf=as.double(X.unf)
plot(t1, type="l")
lines(X.unf,type="l",col="red")
lines(t_clean,type="l",col="blue")

test=fft(t1)
test=c(test[1:5],rep(0,(length(trajectory)-10)),test[(length(trajectory)-5):length(trajectory)])
test=fft(test,inverse=TRUE)/length(trajectory)
test=as.double(test)
lines(test,type="l",col="green")




#demo3
#sinusoidal with noise and linear trend
# fix with fourier and linear subtraction
X.hold=fft(trajectory)
plot(Mod(X.hold)[0:(acq.freq/2)])

thresh= 200
X.hold[which(Mod(X.hold)<thresh)]=0
X.unf=fft(X.hold,inverse=TRUE)/length(trajectory)
X.unf=as.double(X.unf)
dis=X.unf
plot(trajectory, type="l")
lines(X.unf,type="l",col="red")
lines(t_clean,type="l")


#the right way
this=lm(trajectory~t)
trajectory2=this$residuals
X.hold<- fft(trajectory2)

plot(Mod(X.hold)[0:(acq.freq/2)])
thresh= 200
X.hold[which(abs(X.hold)<thresh)]=0
X.unf=fft(X.hold,inverse=TRUE)/length(trajectory)
X.unf=as.double(X.unf)

plot(trajectory2, type="l")
lines(X.unf,type="l",col="blue")
lines(t_clean,type="l",col="red")





