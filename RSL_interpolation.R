#I digitized (using imageJ) the sea level
#curve and interpolated between the points:
  #Add sea-level curve####
  #This curve (grey) is digitized from Fukuyo et al 20202
  #Interpolate between points to get a sea level value for every year
  SL<- read.csv("fukuyo_et_al_2020_sea_level.csv")
  names(SL)[1]<-"RSL"
  RSL <- SL$RSL
  Time <- SL$Time
  x <- SL$Time
  y <- SL$RSL
  
  par(mfrow = c(2,1))
  plot(Time, RSL, main = "Fukuyo et al 2020 grey line (linear interpolation)")
  points(approx(Time, RSL), col = 2, pch = "*")
  points(approx(x, y, method = "constant"), col = 4, pch = "*")
  
  f <- approxfun(x, y)
  curve(f(x), 0, 6000, col = "green2")
  points(x, y)
  is.function(fc <- approxfun(x, y, method = "linear")) # TRUE
  
  plot(approxfun(x, y, rule = 2:1), 0, 6000,
       col = "tomato", add = TRUE, lty = 3, lwd = 2)
  
  approx(x, y, method="linear", xout=seq(min(x),max(x),by=1))
  plot(Time, RSL, main = "Fukuyo et al 2020 grey line (linear interpolation)")
  points(approx(Time, RSL, method="linear", xout=seq(min(x),max(x),by=1)))
  
  write.csv(approx(x, y, method="linear", xout=seq(min(x),max(x),by=1)), "sea_level_interpolated2.csv")