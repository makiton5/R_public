# Download the raw CSV file
d <- read.csv('https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv')
# Extract date, with adding a dummy
d1 <- data.frame(day = d[, 5], num = 1, flag = d[,16])
d1[is.na(d1[,3]),3] <- 0 # replace na with 0

# Aggregate as daily sum
library(dplyr)
d1$day <- as.Date(d1$day) # Put date into Date class
# Aggregate with group-by
d2 <- d1 %>% group_by(day) %>% summarise(sum(num)) %>% as.data.frame()
names(d2)[2] <- 'num'
d2[,3] <- cumsum(d2$num)
names(d2)[3] <- 'total'

d2[,4] <- d2[,2]/d2[,3]*14
names(d2)[4] <- 'Rt'

# set up Kalman filter setting
library(dlm)
mod <- dlmModPoly(order = 1) #1st order random walk

#build_dlm <- function(par){
#  mod$W[1,1] <- exp(par[1])
#  mod$V[1,1] <- exp(par[2])
#  return(mod)
#}

# MLE for parameter
#lapply(list(c(0,1),c(1,10),c(20,3)), function(parms){
#  dlmMLE(y=(d2$Rt),parm=parms,build=build_dlm)})

#fit_dlm <- dlmMLE(y=d2$Rt,parm=c(0, 0) ,build=build_dlm,hessian=TRUE)

#exp(fit_dlm$par)*sqrt(diag(solve(fit_dlm$hessian)))

#mod <- build_dlm(fit_dlm$par)
mod$C0 = 1
mod$W = 0.05
mod$V = 0.4

#mod$W[2,2] = 0.1
#mod$V[1,1] = 0.4

# Filtering
dlmFiltered_obj <- dlmFilter(y=d2$Rt,mod=mod)
str(dlmFiltered_obj,max.level = 1)

# Statistics from
#m <- dropFirst(dlmFiltered_obj$m[,1])
m <- dropFirst(dlmFiltered_obj$m)
m_sdev <- sqrt(
  dropFirst(as.numeric(
    dlmSvd2var(dlmFiltered_obj$U.C, dlmFiltered_obj$D.C)
  ))
)

#m_sdev = 1;

#95% range
m_quantl <- (m+qnorm(0.025,sd=m_sdev))
m_quantu <- (m+qnorm(0.975,sd=m_sdev))


# Create a title with start and end dates
mtitle <- paste0('Tokyo, daily from ', d2$day[1], ' to ', d2$day[nrow(d2)])
xrange <- c(d2$day[7], d2$day[nrow(d2)])
yrange <- c(0,5)
yrange2 <- c(0,200)

par(oma = c(0, 0, 0, 2))

plot(d2$day, d2$Rt,
     main = mtitle, xlab = 'Date', ylab = 'Reproduction number, Et',
     type = 'l', col = 'gray', xlim = xrange, ylim = yrange)

par(new = T)
plot(d2$day, m,
     main = mtitle, xlab = 'Date', ylab = '',
     type = 'l', col = 'blue', xlim = xrange, ylim = yrange, lwd=3)

par(new = T)

plot(d2$day, m_quantu,
     main = mtitle, xlab = 'Date', ylab = '',
     type = 'l', col = 'red', xlim = xrange, ylim = yrange,lty=2)
par(new = T)

plot(d2$day, m_quantl,
     main = mtitle, xlab = 'Date', ylab = '',
     type = 'l', col = 'red', xlim = xrange, ylim = yrange,lty=2)


abline(h=1,col='gray',type='l',lty=2)
abline(h=2,col='gray',type='l',lty=2)
abline(h=3,col='gray',type='l',lty=2)
abline(h=4,col='gray',type='l',lty=2)
abline(h=5,col='gray',type='l',lty=2)
abline(h=6,col='gray',type='l',lty=2)
abline(h=7,col='gray',type='l',lty=2)
abline(h=8,col='gray',type='l',lty=2)
abline(h=9,col='gray',type='l',lty=2)

#axis(1)   # x
axis(2)   # left y

par(new = T)

plot(d2$day, d2$num,
     main = mtitle, xlab = 'Date',  ylab = '',
     type = 'l', col = 'black', xlim = xrange, ylim = yrange2, lwd=3,lty=1,axes = FALSE)

#plot(xd2$day, d2$num, xlim = xrange, ylim = yrange2,
#     xlab = "", ylab = "", type = "l", col = "cyan", lwd = 2,
#     axes = FALSE)

mtext("New daily positive reported",side = 4, line = 3)  # right y label
axis(4)                        # right y

box()
legend("topleft", legend = c("Reproduction filtered", "Reproduction raw",'New posivite reported'), col = c("blue", "gray","black"), lty = 1)

