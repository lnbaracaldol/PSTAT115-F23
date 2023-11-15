
## Hoff figs
fig_1_1_a <- function(cols) {
  
    
  #### Fig 1.1
  a <- 2
  b <- 20

  par(mar=c(3,3,1,1), mgp=c(1.75,.75,0))
  
  dbinom(0,20,.05)
  n<-20
  x<-0:n
  del<-.25
  plot( range(x-del), c(0,.4),xlab="number infected in the sample",
        ylab="probability",type="n")
  
  points( x-del,dbinom(x,n,.05),type="h",col=cols[3],lwd=3)
  points( x,dbinom(x,n,.10),type="h",col=cols[2],lwd=3)
  points( x+del,dbinom(x,n,.20),type="h",col=cols[1],lwd=3)
  legend(10,.35,legend=c(
    expression(paste(theta,"=0.05",sep="")), 
    expression(paste(theta,"=0.10",sep="")),
    expression(paste(theta,"=0.20",sep="")) ),
    lwd=c(3,3,3), 
    col=cols[3:1] ,bty="n") 
  
  par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0))
 
}


fig_1_1_b <- function(cols, tmp) {
  
  a<-2 ; b<-20
  y<-0 ; n<-20
  
  
  (a+y)/(a+b+n)
  (a+y-1)/(a-1+b+n-1)
  pbeta(.20,a+y,b+n-y) - pbeta(.05,a+y,b+n-y)
  pbeta(.10,a+y,b+n-y)
  
  
  par(mar=c(3,3,1,1), mgp=c(1.75,.75,0))
  theta <- seq(0,1,length=500)
  plot(theta, dbeta(theta, a+y, b+n-y),
       type="l",
       xlab="percentage infected in the population",
       ylab="", lwd=3, ylim=c(0,16),
       col=cols[1]
  )
  lines(theta, dbeta(theta,a,b),col=cols[2],lwd=3)
  legend(.5,14,legend=c( expression(paste(italic("p"),"(",theta,")",sep="")), 
                         expression(paste(italic("p"),"(",theta,"|",italic("y"),")",sep=""))  ), 
         bty="n", lwd=c(2,2),col=cols[c(2, 1)])
  
  par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0))
}








fig_3_9 <- function(cols) {
  
  bachelors_data <- scan("../data/menchild30bach.dat")
  no_bachelors_data <- scan("../data/menchild30nobach.dat")
  
  ### prior parameters
  a <- 2
  b <- 1
  
  ### bachelors data
  num_bachelors <- length(bachelors_data)
  y_bachelors <- sum(bachelors_data)
  
  ### data in group B
  num_no_bachelors <- length(no_bachelors_data)
  y_no_bachelors <- sum(no_bachelors_data)

  y1 <- bachelors_data
  y2 <- no_bachelors_data
  
  set.seed(1)
  n1 <- num_bachelors
  n2 <- num_no_bachelors
  s1 <- y_bachelors
  s2 <- y_no_bachelors
  
  par(mar = c(3, 3, 1, 1), mgp = c(1.75, .75, 0))
  par(mfrow = c(1, 2))
  
  par(
    mfrow = c(1, 2),
    mar = c(3, 3, 1, 1),
    mgp = c(1.75, .75, 0)
  )
  plot(
    table(y1),
    type = "h",
    xlab = expression(italic(y)),
    ylab = expression(italic(n[1](y))),
    col = cols[1] ,
    lwd = 3
  )
  mtext("Less than bachelor's", side = 3)
  plot(
    table(y2),
    type = "h",
    xlab = expression(italic(y)),
    ylab = expression(italic(n[2](y))),
    col = cols[2],
    lwd = 3
  )
  mtext("Bachelor's or higher", side = 3, lwd = 3)
  
}




fig_3_10 <- function(cols) {

  bachelors_data <- scan("../data/menchild30bach.dat")
  no_bachelors_data <- scan("../data/menchild30nobach.dat")
  
  ### prior parameters
  a <- 2
  b <- 1
  
  ### bachelors data
  num_bachelors <- length(bachelors_data)
  y_bachelors <- sum(bachelors_data)
  
  ### data in group B
  num_no_bachelors <- length(no_bachelors_data)
  y_no_bachelors <- sum(no_bachelors_data)
    
  set.seed(1)
  n1 <- num_bachelors
  n2 <- num_no_bachelors
  s1 <- y_bachelors
  s2 <- y_no_bachelors
  
  a<-2 ; b<-20
  y<-0 ; n<-20
  n<-20
  
  par(mar = c(3, 3, 1, 1), mgp = c(1.75, .75, 0))
  par(mfrow = c(1, 2))
  a <- 2
  b <- 1
  xtheta <- seq(0, 5, length = 1000)
  plot(
    xtheta,
    dgamma(xtheta, a + s1, b + n1),
    type = "l",
    col = cols[1],
    lwd=2,
    xlab = expression(theta),
    ylab = expression(paste(
      italic("p("), theta, "|", y[1], "...", y[n], ")", sep = ""
    )),
    ylim=c(0,5)
  )
  lines(xtheta,
        dgamma(xtheta, a + s2, b + n2),
        col = cols[2],
        lwd = 2)
  lines(
    xtheta,
    dgamma(xtheta, a, b),
    type = "l",
    lty = 2,
    lwd = 2
  )
  abline(h = 0, col = "black")
  #dev.off()
  #pdf("fig4_6.pdf",family="Times",height=3,width=6)
  #par(mar=c(3,3,1,1),mgp=c(1.75,.75,0))
  #par(mfrow=c(1,2))
  y <- (0:12)
  plot(
    y - .1,
    dnbinom(y, size = (a + s1), mu = (a + s1) / (b + n1)) ,
    col = cols[1] ,
    type = "h",
    ylab = expression(paste(
      italic("p("), y[n + 1], "|", y[1], "...", y[n], ")", sep = ""
    )),
    xlab = expression(italic(y[n + 1])),
    ylim = c(0, .40),
    lwd = 3
  )
  points(
    y + .1,
    dnbinom(y, size = (a + s2), mu = (a + s2) / (b + n2)) ,
    col = cols[2],
    type = "h",
    lwd = 3
  )

  
  
}
############# Chapter 4 ##############

figure_4_1 <- function(col) {
  
  par(mar = c(3, 3, .25, 1), mgp = c(1.75, .75, 0))
  par(mfrow = c(2, 3))
  set.seed(1)
  a <- 68
  b <- 45
  set.seed(1)
  theta.support <- seq(0, 3, length = 100)
  theta.sim10 <- rgamma(10, a, b)
  theta.sim100 <- rgamma(100, a, b)
  theta.sim1000 <- rgamma(1000, a, b)
  
  xlim <- c(.75, 2.25)
  ylim = c(0, 2.5)
  lty = 1
  
  hist(
    theta.sim10,
    prob = T,
    xlim = xlim,
    ylim = ylim,
    xlab = "",
    main = "",
    ylab = "",
    col = col
  )
  lines(
    theta.support,
    dgamma(theta.support, a, b),
    col = "black",
    lwd = 2,
    lty = lty
  )
  text(2.1, 2.25, expression(paste(italic(S), "=10", sep = "")))
  
  hist(
    theta.sim100,
    prob = T,
    xlim = xlim,
    ylim = ylim,
    xlab = "",
    main = "" ,
    ylab = "",
    col = col
  )
  lines(
    theta.support,
    dgamma(theta.support, a, b),
    col = "black",
    lwd = 2,
    lty = lty
  )
  text(2.1, 2.25, expression(paste(italic(S), "=100", sep = "")))
  
  hist(
    theta.sim1000,
    prob = T,
    xlim = xlim,
    ylim = ylim,
    xlab = "",
    main = "" ,
    ylab = "",
    col=col
  )
  lines(
    theta.support,
    dgamma(theta.support, a, b),
    col = "black",
    lwd = 2,
    lty = lty
  )
  text(2.1, 2.25, expression(paste(italic(S), "=1000", sep = "")))
  
  
  plot(
    density(theta.sim10),
    xlim = xlim,
    ylim = ylim,
    xlab = expression(theta),
    main = "",
    ylab = "",
    col = col,
    lwd = 2
  )
  lines(
    theta.support,
    dgamma(theta.support, a, b),
    col = "black",
    lwd = 2,
    lty = lty
  )
  
  plot(
    density(theta.sim100),
    xlim = xlim,
    ylim = ylim,
    xlab = expression(theta),
    main = "",
    ylab = "",
    col = col,
    lwd = 2
  )
  lines(
    theta.support,
    dgamma(theta.support, a, b),
    col = "black",
    lwd = 2,
    lty = lty
  )
  
  plot(
    density(theta.sim1000),
    xlim = xlim,
    ylim = ylim,
    xlab = expression(theta),
    main = "",
    ylab = "",
    col = col,
    lwd = 2
  )
  lines(
    theta.support,
    dgamma(theta.support, a, b),
    col = "black",
    lwd = 2,
    lty = lty
  )
  
}



######### Chapter 5 #################

figure_5_6 <- function(cols){
  
  par(mfrow = c(1, 2), mar = c(3, 3, 1, 1),  mgp = c(1.75, .75, 0))
  
  b <- (100 - 112) ^ 2
  s2 <- 13 ^ 2
  n <- 1:50
  
  k <- 1
  brk1 <- (n / (k + n)) ^ 2 + n * (k / (k + n)) ^ 2 * b / s2
  k <- 2
  brk2 <- (n / (k + n)) ^ 2 + n * (k / (k + n)) ^ 2 * b / s2
  k <- 3
  brk3 <- (n / (k + n)) ^ 2 + n * (k / (k + n)) ^ 2 * b / s2
  
  plot(range(n),
       c(0.4, 1.1),
       type = "n",
       xlab = "sample size",
       ylab = "relative MSE (PM/MLE)")
  abline(h = 1, lty = 2, lwd = 2)
  lines(n, brk1, col = cols[1], lwd = 2)
  lines(n, brk2, col = cols[2], lwd = 2)
  lines(n, brk3, col = cols[3], lwd = 2)
  legend(20,.8,
    legend = c(
      expression(kappa[0] == 0),
      expression(kappa[0] == 1),
      expression(kappa[0] == 2),
      expression(kappa[0] == 3)
    ),
    lwd = c(2, 2, 2),
    lty = c(2, 1, 1, 1),
    col = c("black", cols),
    bty = "n"
  )
  
  ####
  theta0 <- 112
  mu0 <- 100
  n <- 10
  s2m <- s2 / n
  x <- seq(theta0 - 4 * sqrt(s2m), theta0 + 4 * sqrt(s2m), length = 100)
  plot(
    x,
    dnorm(x, theta0, sqrt(s2m)),
    type = "l",
    lwd = 2,
    ylim = c(0, .13),
    lty = 2,
    xlab = "IQ",
    ylab = ""
  )
  abline(v = theta0)
  for (k in 1:3) {
    w <- n / (n + k)
    lines(
      x,
      dnorm(x, w * theta0 + (1 - w) * mu0, sqrt(w ^ 2 * s2m)),
      type = "l",
      col = cols[k],
      lwd = 2
    )   
  }
}


figure_5_7 <- function(cols) {

  par(mfrow=c(1,2),mar=c(2.75,2.75,.5,.5),mgp=c(1.70,.70,0))

  bachelors_data <- scan("../data/menchild30bach.dat")
  no_bachelors_data <- scan("../data/menchild30nobach.dat")
  CHILDS <- c(bachelors_data, no_bachelors_data)

  ###

  set.seed(1)
  NY<-NULL
  N<-c(5,15,45)
  for(n in N) {
    for(sim in 1:5000) {
      y<-sample(CHILDS,n)
      NY<-rbind(NY, c(n,mean(y),var(y)) )
    } 
  }
  
  plot(table(CHILDS)/sum(table(CHILDS)),type="h",
       xlab=expression(paste(italic(y),"=number of children",sep="" )), 
       ylab=expression(italic(p(y))))
  
  x<-seq(0,6,length=200)
  plot( range(NY[,2]),c(0,1.7),type="n",xlab="number of children", 
        ylab=expression( italic( p(bar(y))) ), col=cols[1])
  
  i <- 1
  for( n in N) {
    yb<- NY[NY[,1]==n,2]
    lines(stats::density(yb, adjust=2) ,col=cols[i], lwd=2)
    i <- i+1
  }
  abline(v=mean(CHILDS))
  legend( 2.35,1.8,legend=c("n=5","n=15","n=45"),lwd=c(2,2,2),col=
            cols, bty="n")
  
  
}
