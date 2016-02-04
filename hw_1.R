#loads packages and creates dataframe
library(ggplot2)
library(car)
library(dplyr)
library(gmodels)
library(multcomp)
library(multtest) # for testing contrasts
library(agricolae) # for Fisher LSD, Duncan's, scheffe...
library(extrafont)

treatment <- as.factor(c(1,1,1,1,1,1,1,1,1,
                         2,2,2,2,2,2,2,2,
                         3,3,3,3,3,3,3,3,3,3,3,3,3,3,3))

values <- as.numeric(c(1.06, 0.79, 0.82, 0.89, 1.05, 0.95, 0.65, 1.15, 
                       1.12, 1.58, 1.45, 0.57, 1.16, 1.12, 0.91, 0.83, 0.43, 
                       0.29, 0.06, 0.44, 0.55, 0.61, 0.43, 0.51, 0.10, 0.53, 0.34, 0.06, 0.09, 0.17, 0.17, 0.60))

data <- data.frame(treatment, values)
View(data)

#part c
data$resids <- data$values-mean(data$values)
(ggplot(data,aes(x=treatment, y=values)) + geom_boxplot()) + labs(x="Treatment", title="Treatment Boxplots")
(qqnorm(data$resids, main = "Normal Probability Plot of Residuals"))


#part d
leveneTest(values, treatment)


#part e
lm <- aov(values ~ treatment, data = data)
summary(lm) #regular anova
oneway.test(values ~ treatment, data = data) #welch's


#part f
fit.contrast(lm, "treatment", c(1,1,-2))

#for hand calculation on part f
(mean1 <- mean(data$value[data$treatment == "1"]))
(mean2 <- mean(data$value[data$treatment == "2"]))
(mean3 <- mean(data$value[data$treatment == "3"]))

(var1 <- var(data$value[data$treatment == "1"]))
(var2 <- var(data$value[data$treatment == "2"]))
(var3 <- var(data$value[data$treatment == "3"]))

(est.cont <- 1*mean1 + 1*mean2 - 2*mean3) #estimated contrast
(sp <- (8*var1 + 7*var2 + 14*var3)/(nrow(data) - 3)) #pooled variance
(stderr.cont <- sqrt(sp*((1/9)+(1/8)+(4/15)))) #std error of the contrast
(t.stat <- est.cont/stderr.cont) #t-stat
t.stat^2 #checks to make sure t.stat^f = Fstat from SAS output 


#part g
fit.contrast(lm, "treatment", c(1,-.5,-.5), conf.int = .90)

#checking part g
(est.cont <- (1*mean1)  + (-.5*mean2) + (-.5*mean3)) #estimated contrast
(sp <- (8*var1 + 7*var2 + 14*var3)/(nrow(data) - 3)) #pooled variance
(stderr.cont <- sqrt(sp*((1/9)+(((-.5)^2)/8)+(((-.5)^2)/15)))) #std error of the contrast
crit.val <- 1.699 # alpha/2=.05, df=N-t=31-3=29

lower <- est.cont - crit.val*stderr.cont
upper <- est.cont + crit.val*stderr.cont

(CI <- (paste(lower, upper)))

#part h (see - http://wwwuser.gwdg.de/~cscherb1/content/Statistics%20Course%20files/Working%20with%20orthogonal%20contrasts%20in%20R.pdf) for more examples
contrast.matrix <- cbind(c(1, -1, 0), c(1, 0, -1)) #sets up contrast matrix
colnames(contrast.matrix) <- c("1 vs 2", "1 vs 3") #re-names columns
rownames(contrast.matrix) <- c("treat 1", "treat 2", "treat 3") #renames rows
contrast.matrix #views contrast matrix

(contrasts(treatment) <- contrast.matrix)
summary.lm(aov(values~treatment))
  

#part i - not complete
(pairwise.t.test(values, treatment, p.adj = "bonf"))# doesn't give crit value, just if pairs are sig.
(TukeyHSD(lm)) # doesn't give crit value, just if pairs are sig.

summary(lm)
(LSD.test(data$value, data$treatment, MSerror = 0.0674, DFerror = 29, alpha = 0.05))      #use MS and DF for error term here
(duncan.test(data$value, data$treatment, MSerror = 0.0674, DFerror = 29, alpha = 0.05))   #use MS and DF for error term here
(SNK.test(data$value, data$treatment, MSerror = 0.0674, DFerror = 29, alpha = 0.05) )     #use MS and DF for error term here
(scheffe.test(data$value, data$treatment, MSerror = 0.0674, DFerror = 29, alpha = 0.05))  #use MS and DF for error term here

#part j
data$treatment2 <- "1"
data$treatment2[data$treatment == "3"] <- "3"

lm.reduced <- aov(values ~ treatment2, data = data) #reduced model
summary(lm.reduced)
summary(lm)

