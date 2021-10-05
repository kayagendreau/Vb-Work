##Cleaning Functions
glacier.area <- function(x){
  xnas <- table(is.na(x[]))
}


perc.removed <- function(x,y){ #x is the original data set, y is the cleaned data set--function returns the percentage of x removed from y
  x <- calc(x, fun=function(x){as.numeric(!is.na(x))})
  y <- calc(y, fun=function(x){as.numeric(!is.na(x))})
  xs<- sum(values(x))
  ys<- sum(values(y))
  100 - (ys/xs)*100
}

perc.left <- function(x,y){ #x is the original data set, y is the cleaned data set--function returns the percentage of x that y is.
  x <- calc(x, fun=function(x){as.numeric(!is.na(x))})
  y <- calc(y, fun=function(x){as.numeric(!is.na(x))})
  xs<- sum(values(x))
  ys<- sum(values(y))
  (ys/xs)*100
}

perc.na <- function(x){ #function tells you how much of your data set is NA values, TRUE values are the NA values
  xnas <- table(is.na(x[]))
  print(table(is.na(x[])))
  xnas[2]/(xnas[1]+xnas[2])
}

perc.change <- function(x,y){
  ((y-x)/(abs(x)))*100
}

perc.change(110, 100)

clean.bysd <- function(x,y){ #x is the data set you want to clean, y is number of sd to clean by
  z <- y
  (xm <- mean(values(x), na.rm=TRUE))
  (xsd <- sd(values(x), na.rm=TRUE))
  calc(x, fun=function(x){ifelse(x> xm + z*xsd|x< xm - z*xsd, NA, x)})
}

clean.tbperc <- function(x,y){  #x is the data set you want to clean, y is the percentage you want to clean both the top and bottom of the data set by
  z <- y/100
  calc(x,fun=function(x){
    ifelse(x> quantile(x, probs = 1-z, na.rm=TRUE)|
             x< quantile(x, probs = z, na.rm=TRUE), NA, x)})
}

clean.topperc <- function(x,y){  #x is the data set you want to clean, y is the percentage you want to clean the top of the data set by
  z <- y/100
  calc(x,fun=function(x){
    ifelse(x> quantile(x, probs = z, na.rm=TRUE), NA, x)})
}

clean.bottperc <- function(x,y){  #x is the data set you want to clean, y is the percentage you want to clean the bottom of the data set by
  z <- y/100
  calc(x,fun=function(x){
    ifelse(x< quantile(x, probs = z, na.rm=TRUE), NA, x)})
}

clean.greaterdegree <- function(x,y){ #Cleans for x greater than y degrees: x is a slope layer you want cleaned, y is the degree cut off the function cleans for
  degy <- y
  calc(x, fun=function(x){ifelse((x*(180/pi)) > degy, NA, x)})
}

clean.lesserdegree <- function(x,y){ #Cleans for x less than y degrees: x is a slope layer you want cleaned, y is the degree cut off the function cleans for 
  degy <- y
  calc(x, fun=function(x){ifelse((x*(180/pi)) < degy, NA, x)})
}

clean.lesservariable <- function(x,y){ #Cleans for x greater than y degrees: x is a slope layer you want cleaned, y is the degree cut off the function cleans for
  degy <- y
  calc(x, fun=function(x){ifelse((x) < degy, NA, x)})
}

clean.greatervariable <- function(x,y){ #Cleans for x greater than y degrees: x is a slope layer you want cleaned, y is the degree cut off the function cleans for
  degy <- y
  calc(x, fun=function(x){ifelse((x) > degy, NA, x)})
}

apply.clean <- function(x,y){          #x is the data set you want to apply your cleaning to, y is the cleaned data
  xna <- calc(x, fun = function(x) {
    as.numeric(!is.na(x))
  }
  )
  yna <- calc(y,
              fun = function(x) {
                as.numeric(!is.na(x))
              }
  )
  nas <-
    overlay(xna, yna,
            fun = function(x, y) {
              ifelse(x - y == 0, NA, x)
            }
    )
  mask(x, nas, inverse=TRUE)
}

layer.diff <- function(x,y){          #Function finds the difference between two layers
  xna <- calc(x, fun = function(x) {
    as.numeric(!is.na(x))
  }
  )
  yna <- calc(y,
              fun = function(x) {
                as.numeric(!is.na(x))
              }
  )
  nas <-
    overlay(xna, yna,
            fun = function(x, y) {
              ifelse(x - y == 0, NA, x)
            }
    )
}

# library(moments)
# skewness(chim.stack.glacier$slope1997, na.rm=TRUE)

skew1 <- function(x, na.rm=FALSE){
  x <- x
  xmean <- mean(values(x), na.rm=na.rm)
  xmedian <- median(values(x), na.rm=na.rm)
  xsd <- sd(values(x), na.rm = na.rm)
  xlength <- length(x)
  (sqrt((xlength*(xlength-1)))/(xlength-2))*((((sum(values(x), na.rm = na.rm))-xmean)/xlength)/(xsd^3))
}

skew <- function(x, na.rm=FALSE){
  (x <- x)
  (xmean <- mean(values(x), na.rm=na.rm))
  (xmedian <- median(values(x), na.rm=na.rm))
  (xsd <- sd(values(x), na.rm = na.rm))
  (xlength <- length(x))
  ((3*(xmean-xmedian))/xsd)
}

##Analysis Functions

model.forward.r2.vals <- function(x,y){ #x is the model you want the r2 values for, y is the data set of the model
  lengthx<- length(coef(x))- 1
  i <- 1
  z <- names(x$model[1])
  variable.added <- matrix(ncol = 2, nrow = lengthx)
  while (i <= lengthx) {
    variable.added[i] <- names(x$coefficients[i + 1])
    data1 <- as.data.frame(variable.added)
    i <- i+1
  }
  i <- 1
  while (i <= lengthx){
    modeli <- lm(paste(paste(z),"~", paste(data1$V1[1:i], collapse=" + ", sep = "")), data = y)
    variable.added[i,2] <- summary(modeli)$adj.r.squared
    i <- i+1
  }
  data2 <- as.data.frame(variable.added)
  ifelse(class(data2$V2) == "factor", data2$V2 <- as.numeric(levels(data2$V2)), 
         data2$V2 <- as.numeric(data2$V2))
  data2
}

model.forward.r2.plot <- function(x,y){ #x is the model you want to plot, y is the data set of the model
  lengthx<- length(coef(x))- 1
  i <- 1
  z <- names(x$model[1])
  variable.added <- matrix(ncol = 2, nrow = lengthx)
  while (i <= lengthx) {
    variable.added[i] <- names(x$coefficients[i + 1])
    data1 <- as.data.frame(variable.added)
    i <- i+1
  }
  i <- 1
  while (i <= lengthx){
    modeli <- lm(paste(paste(z),"~", paste(data1$V1[1:i], collapse=" + ", sep = "")), data = y)
    variable.added[i,2] <- summary(modeli)$adj.r.squared
    i <- i+1
  }
  data2 <- as.data.frame(variable.added)
  ifelse(class(data2$V2) == "factor", data2$V2 <- as.numeric(levels(data2$V2)), 
         data2$V2 <- as.numeric(data2$V2))
  level_order <- factor(data2$V1, level = c(paste(data2$V1, sep = "\"\"" )))
  
  ggplot() + 
    geom_point(data=data2, aes(x =  level_order, y = V2)) +
    theme(axis.text = element_text(size = 10),axis.text.x = element_text(angle=90, hjust=1)) +
    labs(title = "Adj R2 Values by Variable Added")
}

variogram.plot <- function(data, model, nugget = 20, ...) { #data is the data set, model is the model you want variogramed
  data$resid.y <- resid(model)
  vg.resid.y <-
    variogram(resid.y ~ 1,
              data = data,
              ... )
  (vgfit.resid.y <-
      fit.variogram(vg.resid.y, vgm(
        model = "Sph",
        nugget = nugget,
        ... 
      )))
  print(vgfit.resid.y)
  print(plot(vg.resid.y, vgfit.resid.y, main = "Variogram"))
  return(vgfit.resid.y)
}

variogram.vals <- function(data, model, vgfit.resid, resolution) { #data is the data set used in the original model, vgfit.resid is the returned values from the variogram.plot function, resolution is the resolution of your original data set used to construct your model being evaluated (ie. resolution = 10 for a 10x10m resolution data set)
  data$resid.y <- resid(model)
  range.est.diff.on <- vgfit.resid$range[2]
  Acor.diff.on <- pi*range.est.diff.on^2
  print("Acor")
  print(Acor.diff.on)
  A.diff.on <- resolution^2*nrow(data)
  print("A")
  print(A.diff.on)
  print("SD of residuals")
  print(sd(data$resid.y))
  #print(sd(x$names(y$model[1])))
  print("Variance of residuals")
  print(sqrt(var(data$resid.y)*Acor.diff.on/A.diff.on*(1/5)))
  #print(sqrt(var(x$names(y$model[1]))*Acor.diff.on/A.diff.on*(1/5)))
  print("N eff by Rolstad")
  print(5*A.diff.on/Acor.diff.on) #N eff by Rolstad
  print("sill (not partial) SD")
  print(sqrt(sum(vgfit.resid$psill))) #sill (not partial) SD
  print("SE")
  print(sqrt(sum(vgfit.resid$psill))/sqrt((5*A.diff.on/Acor.diff.on))) #SE
}

variogram.it <- function(data, model, resolution, width, nugget = 20, ...) { #data is the data set, model is the model you want variogramed
  data$resid.y <- resid(model)
  vg.resid.y <-
    variogram(resid.y ~ 1,
              data = data,
              width = width,
              ... )
  (vgfit.resid.y <-
      fit.variogram(vg.resid.y, vgm(
        model = "Sph",
        nugget = nugget,
        ... 
      )))
  print(vgfit.resid.y)
  print(plot(vg.resid.y, vgfit.resid.y, main = "Variogram"))
  range.est.diff.on <- vgfit.resid.y$range[2]
  Acor.diff.on <- pi*range.est.diff.on^2
  print("Acor")
  print(Acor.diff.on)
  A.diff.on <- resolution^2*nrow(data)
  print("A")
  print(A.diff.on)
  print("SD of residuals")
  print(sd(data$resid.y))
  #print(sd(x$names(y$model[1])))
  print("Variance of residuals")
  print(sqrt(var(data$resid.y)*Acor.diff.on/A.diff.on*(1/5)))
  #print(sqrt(var(x$names(y$model[1]))*Acor.diff.on/A.diff.on*(1/5)))
  print("N eff by Rolstad")
  print(5*A.diff.on/Acor.diff.on) #N eff by Rolstad
  print("sill (not partial) SD")
  print(sqrt(sum(vgfit.resid.y$psill))) #sill (not partial) SD
  print("SE")
  print(sqrt(sum(vgfit.resid.y$psill))/sqrt((5*A.diff.on/Acor.diff.on))) #SE
}



degree <- function(d, step.n.slope.clean, diff.step.n){
  step2.clean <- clean.greaterdegree(step.n.slope.clean, d)
  chim.stack.glacier$diff.step.n <- apply.clean(chim.stack.glacier$difference, step2.clean)
  print(perc.removed(chim.stack.glacier$difference, chim.stack.glacier$diff.step.n))
} #change chim.stack.glacier$difference
 




