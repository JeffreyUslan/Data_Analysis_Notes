
len=1000

#time indicator
x=seq(1:len)
time     <- 1
w        <- 2*pi/time
ts       <- seq(time/len,time,1/len)
#simple noise
noise=rnorm(len,mean=0,sd=.3)
#heteroscedastic noise
#sinusoidals
sin_1=sin(1*w*ts)
sin_2=(1/2)*sin(6*w*ts)
sin_4=(1/3)*sin(26*w*ts)
sin_8=(1/4)*sin(133*w*ts)
clean_period=sin_1+sin_2+sin_4+sin_8
plot(clean_period)

var_pres=sin_1+sin_2+sin_4+sin_8+noise+.01*x
final_sim=var_pres+c(rep(0,1),var_pres[2:len])+c(rep(0,2),var_pres[3:len])+c(rep(0,3),var_pres[4:len])


plot(final_sim)
lm_model=lm(final_sim~x)
line_fit=predict(lm_model)
lines(line_fit,col="yellow")
#remove linear trend
clean_1=final_sim-line_fit
plot(clean_1)
filt_1=filter(clean_1,rep(1/2,2))
filt_2=filter(clean_1,rep(1/4,4))
filt_3=filter(clean_1,rep(1/8,8))
filt_4=filter(clean_1,rep(1/16,16))
filt_5=filter(clean_1,rep(1/32,32))

lines(filt_1,col="red")
lines(filt_2,col="green3")
lines(filt_3,col="blue")
lines(filt_4,col="cyan")
lines(filt_5,col="magenta")

#ACF diagnostics
acf(na.omit(filt_1))


#FFT exploration
X.hold=Mod(fft(na.omit(clean_1)))
X.hold=X.hold[1:length(X.hold)/2]
plot(X.hold)
ordered_ind=order(X.hold,decreasing=TRUE)
head(ordered_ind,n=10)

#fit model with suspected sinusoids



