# import philliesAttendance
hdf<-happiness
data.class(hdf)
pdf<-as.data.frame(hdf)
data.class(hdf)

hdf
dim(hdf)
names(hdf)

osdf<-hdf[157,]
#hdf$obs<-1:nrow(pdf)
# pdf$WinPctL1<-cv.lag(pdf$WinPct,1)  # create lagged variable WinPct1
# pdf$uRateL1 <-cv.lag(pdf$uRate,1)   # create Lagged variable URate1


par(mfcol=c(3,3))  # histograms
# ASSIGNMENT - add titles and labels
hist(hdf$Score)
#  hist(isdf$obs)
hist(hdf$GDP.per.capita)
hist(hdf$Healthy.life.expectancy)
hist(hdf$Freedom.to.make.life.choices)



par(mfcol=c(2,2))  # scatterplot w/ smooth line overlay
# scatter.smooth shows nonlinearities, if they exist
# ASSIGNMENT - add titles and labels
scatter.smooth(hdf$obs,hdf$Score)
scatter.smooth(hdf$GDP.per.capita,hdf$Score)
scatter.smooth(hdf$Healthy.life.expectancy,hdf$Score)
scatter.smooth(hdf$Freedom.to.make.life.choices,hdf$Score)


#Descriptive Statistics
library("YRmisc")
ds.summ(hdf)  # install, load YRmisc
ds.summ(hdf[,2:5],2)[,-c(8:9)]  # take out variance column
#Pivot table on attendance by stadium
mean(isdf$PhilliesAtt)
tapply(isdf$PhilliesAtt,isdf$Stadium,mean)
data.frame(tapply(isdf$PhilliesAtt,isdf$Stadium,mean))

dim(hdf)
hdf<-na.omit(hdf)
cor(hdf$Score,hdf$GDP.per.capita)  #cor of 2 vectors
cor(hdf[,3:9])  # 1. high, low, agree?          2. ?Multicollinearity
round(cor(hdf[,3:9]),2)  # 1. high, low, agree? 2. ?Multicollinearity

hdfa<-na.omit(hdf) # eliminate rows with NAs (missing values)
names(hdfa)
# linear regression - add variables
fit<-lm(Score~obs+GDP.per.capita+Healthy.life.expectancy,data=hdfa,na.action=na.omit)
summary(fit)
reg.dw(fit)   # durbinWatson test for serial correlation
predValue<-round(predict(fit,osdf)) # predict attendance for osdf, 
# apply fit object to out of sample data frame
predValue   # display predicted vale

# validation - checking assumptions 
vdf<-data.frame(isdfa,compositePred=fit$fitted.values,resids=fit$residuals)
par(mfcol=c(1,1))
plot(vdf$obs,vdf$PhilliesAtt,type = "n",xlim=c(0,63));
lines(vdf$obs,vdf$PhilliesAtt,lty=1);
lines(vdf$obs,vdf$compositePred,lty=3);
text(63,predValue,"*")

# assignment - labels and titles
hist(vdf$resids)   # Are Residuals normally distributed
scatter.smooth(vdf$compositePred,vdf$PhilliesAtt) # ? linear relationship
# tsplot 2 from YRMisc - does not work
# pl.2ts(vdf$PhilliesAtt,vdf$compositePred,"TSPlot: Act & Pred Attendance") # do actual and predicted values track each other?
plot(vdf$obs,vdf$PhilliesAtt,type = "n");
lines(vdf$obs,vdf$PhilliesAtt,lty=1);
lines(vdf$obs,vdf$compositePred,lty=3);  

par(mfcol=c(3,3));
scatter.smooth(vdf$obs,vdf$resids)
scatter.smooth(vdf$uRate,vdf$resids)
scatter.smooth(vdf$WinPct,vdf$resids)
scatter.smooth(vdf$RunsScored,vdf$resids)

# Misc
names(fit)    # display names of fit object
fit$coefficients # display coefficients in equation
fit$fitted.values # display fitted (predicted) values 
fit$residuals     # display residuals (errors)
####
## Gam - General Additive Models
## load package "gam"
#fit<-gam(PhilliesAtt~s(obs)+s(WinPct)+s(uRate)+s(unRate),data=isdf,na.action=na.omit)
#   plot.gam(fit)