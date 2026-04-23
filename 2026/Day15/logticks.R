#functions for the creation of breaks in log scale https://stackoverflow.com/a/57229468/4268720
#Modified to create major and minor ticks
logticks <- function(datavar,type) {
  minimum <- 1/10^abs(floor(log10(min(datavar, na.rm=TRUE))))
  maximum <- 1*10^abs(floor(log10(max(datavar, na.rm=TRUE)))+1)
  multiple <- floor(log10(maximum/minimum))
  yourtickvector <- c()
  if (type=="min_breaks") {
    yourtickvector <- c(minimum)
    for (x in seq(0,multiple)) {
      andadd <- seq(minimum*10^x,minimum*10^(x+1),minimum*10^x)[-1]
      yourtickvector <- c(yourtickvector,andadd)
    }
  } else if (type=="labels") {
    for (x in seq(0,multiple)) {
      andadd <- c(minimum*10^x,rep("",8))
      yourtickvector <- c(yourtickvector,andadd)
    }
    yourtickvector <- c(yourtickvector,minimum*10^multiple)
  } else if (type=="breaks") {
    for (x in seq(0,multiple)) {
      andadd <- c(minimum*10^x)
      yourtickvector <- c(yourtickvector,andadd)
    }
    yourtickvector <- c(yourtickvector,minimum*10^multiple)
  }
  return(yourtickvector)
}