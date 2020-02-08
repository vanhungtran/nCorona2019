
# This analysis was developed basing on the post at #https://www.reddit.com/r/dataisbeautiful/comments/f0mj7s/coronavirus_case_updated_update_and_forecasts_oc/


library(propagate)

par(family="serif")

Y <- c(4,326, 579, 844,1312,2015,2801,4579,6061,7816,9821,11948,14551,17387,20626,24553,28276,31439,34878,34956)

n <- as.numeric(length(Y))

mydf <- data.frame(y = Y, x = seq(1:n))








#Linear model

lin_mod0=lm(y~1 + x , data = mydf)
lin_mod=lm(y~1 + x + I(x^2) +  I(x^3), data = mydf)
lin_mod1=lm(y~1 + x + I(x^2) +  I(x^3), data = mydf)
BIC(lin_mod0,lin_mod,lin_mod1)





#Plotting the model

#plot(mydf$x,mydf$y)
#points(mydf$x, predict(lin_mod), type="l", ylab = "Confired cases")







# Exponential and Logistic (sigmoid) functions


mod1 <- nls(y ~ exp(a + b * x), data = mydf, start = list(a = 0, b = 1))

mod2 <- nls(y ~ SSlogis(x, Asym, xmid, scal), mydf)




print(getInitial(y ~ SSgompertz(x, Asym, b2, b3),
                 data = mydf), digits = 5)


mod3 <- nls(y  ~ SSgompertz(x, Asym, b2, b3),
            data = mydf)





BIC(mod1,mod2, mod3, lin_mod)



(PREP1 <- predictNLS(mod1, newdata = data.frame(x = c(n+1, n+2, n+3)), interval = "prediction"))

(PREP2 <- predictNLS(mod2, newdata = data.frame(x = c(n+1, n+2, n+3)), interval = "prediction"))

(PREP3 <- predictNLS(mod3, newdata = data.frame(x = c(n+1, n+2, n+3)), interval = "prediction"))



(PREP4 <- predict(lin_mod, newdata = data.frame(x = c(n+1, n+2, n+3)), interval = "prediction"))








C2 <-as.data.frame(rbind(PREP2$prop[[1]]$sim[1],
                         
                         PREP2$prop[[2]]$sim[1],
                         
                         PREP2$prop[[3]]$sim[1]))




C1 <-as.data.frame(rbind(PREP1$prop[[1]]$sim[1],
                         
                         PREP1$prop[[2]]$sim[1],
                         
                         PREP1$prop[[3]]$sim[1]))






C3 <-as.data.frame(rbind(PREP3$prop[[1]]$sim[1],
                         
                         PREP3$prop[[2]]$sim[1],
                         
                         PREP3$prop[[3]]$sim[1]))




C4 <-c(PREP4[1,1],PREP4[2,1],PREP4[3,1])




  
  
  plot(mydf$x, mydf$y, ylab = "Confirmed cases", xlab="", xlim=c(0,n+3),
       
       ylim=c(0,65000),
       
       col="navy", lwd=2, axes=F,
       
       main = "2019 nCov confirmed cases (updated 2/08 at 19h (Beijing time))",
       
       #     
       #     
       #      title(sub ="Hey Only\nim right\ncool huh?",adj=1)   
       #     
       #      sub = "BIC(Exponential) = 342.6
       #  
       #  BIC(Logistic) = 292.9
       #  BIC(Linear polynomial 2) = 292.9
       # BIC(Gompertz) = 292.9
       #  (smaller is better fit)")
       
       
       
  )
  
  
  axis(2)
  
  axis(1, at=c(1, n, n+1, n+3), lab=c("1/20", "2/08","2/09", "2/11"))
  
  
  
  text(2, 5000, "obvervation")
  
  
  
  
  text(14, 32000, "34,956 total cases at 7pm (Beijing time)")
  
  
  
  
  lines(seq(1:n), predict(mod1, list(x = seq(1:n))), lty=2, lwd=2, col='red')
  
  
  
  
  
  legend(1, 60000, legend=c("Exponential (BIC = 342.6348)"),
         
         col=c("red"), lty=2)
  
 
 # add the second line 
  lines(seq(1:n), predict(mod2, list(x = seq(1:n))), lty=2, lwd=2,col='green')
  
  
  legend(1, 60000, legend=c("Exponential (BIC = 342.6348)", "Logistic (BIC =  292.8836)"),
         
         col=c("red", "green"), lty=2)
  
  text(3, 45000, "smaller is better fit")
  
  # add the third line 
  
  lines(seq(1:n), predict(mod3, list(x = seq(1:n))), lty=2, lwd=2,col='black')
  
  
  
  legend(1, 60000, legend=c("Exponential (BIC = 342.6348)", "Logistic (BIC =  292.8836)" ,"Gompertz (BIC = 272.4123)"),
         
         col=c("red", "green","black"), lty=2)
  
  text(3, 45000, "smaller is better fit")
  
  
  
  # add the last line 
  
  
  
  
  lines(seq(1:n), predict(lin_mod, list(x = seq(1:n))), lty=2, lwd=2,col='blue')
  
  
  legend(1, 60000, legend=c("Exponential (BIC = 342.6348)", "Logistic (BIC =  292.8836)" ,"Gompertz (BIC = 272.4123)","Polynomial 2 linear regression  (BIC =  283.9546)"),
         
         col=c("red", "green","black","blue"), lty=2)
  
  text(3, 45000, "smaller is better fit")
  
  
  
  
  
  
  arrows(11, 50000, 18,50000)
  
  text(15, 47000, "3-day forecast for each model")
  
  
  
  
  
  lines(c(n+1, n+2, n+3),C1$Mean, col="red", lwd=2)
  
 
   lines(c(n+1, n+2, n+3),C2$Mean, col="green", lwd=2)
  
  lines(c(n+1, n+2, n+3),C3$Mean, col="black", lwd=2)
  
  lines(c(n+1, n+2, n+3),C4, col="blue", lwd=2)
  
  
  
  
  
  
  
  
  
  
  
  
  ##################################################################################################################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
