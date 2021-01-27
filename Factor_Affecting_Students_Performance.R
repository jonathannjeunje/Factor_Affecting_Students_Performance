###################################################################################################
##### Two Factors Fixed Effet Design
##### Design: Factor A = Attendance Rate; Factor B = Section; Treatment = Test
numFacA=2;
numFacB=4;
numObvs=3;
facA=c(rep("< 80%",numFacB*numObvs),rep(">= 80%",numFacB*numObvs));
facB=c(rep(c(rep("SEC1",numObvs),rep("SEC2",numObvs),rep("SEC3",numObvs),rep("SEC4",numObvs)),numFacA));

yij=c(207.38,	203.75,	123.33,	207.75,	204.17,	131.88,	202.80,	185.63,	124.00,	178.50,	195.30,	93.75, 207.55,	231.90,	176.25,	230.11,	262.13,	195.45,	194.82,	228.18,	147.78,	216.50,	229.71,	184.17);

facA<-as.factor(facA); # makes row variable as a factor in r
facB<-as.factor(facB); # makes col variable as a factor in r

dat=data.frame(facA,facB,yij); dat;

##### Graphing multiple boxplots
par(mfrow=c(1,2))# Partitioning your graphics window, this is optional, you can ignore it. 
boxplot(yij~facA) #multiple box plots for facA effects
boxplot(yij~facB) #nultiple box plots for facB effects

##### Summary Statistics
tmp <- do.call(data.frame, 
               list(mean = tapply(yij, facA, mean),
                    sd = tapply(yij, facA, sd),
                    median = tapply(yij, facA, median),
                    min = tapply(yij, facA, min),
                    max = tapply(yij, facA, max),
                    n = tapply(yij, facA, length))); tmp
tmp <- do.call(data.frame, 
               list(mean = tapply(yij, facB, mean),
                    sd = tapply(yij, facB, sd),
                    median = tapply(yij, facB, median),
                    min = tapply(yij, facB, min),
                    max = tapply(yij, facB, max),
                    n = tapply(yij, facB, length))); tmp

##### ANOVA table construction with AOV command.
aov.out=aov(yij~facA+facB+facA*facB, data=dat); summary(aov.out);

##### Normality assumption check of the residuals with Shapiro-Wilks test.
qqnorm(aov.out$residuals); qqline(aov.out$residuals,col="red")
shapiro.test(aov.out$residuals);

##### Homogeneity of variance assumption check with Levene's test.
library(car); 
leveneTest(yij~facA, data=dat);
leveneTest(yij~facB, data=dat);
plot(aov.out$fitted.values,aov.out$residuals);abline(h=0,col="red",lwd=2);

##### Pairwise comparisons with TukeyHSD test.
TukeyHSD(aov.out,"facA");
TukeyHSD(aov.out,"facB")

##### Graphing interaction plots
interaction.plot(dat$facA,dat$facB,dat$yij,fixed=TRUE)

##### Response Surface Model, requires RSM package####
#install.packages("rsm")
library(rsm)
rsm2k<-rsm(yij~FO(facA,facB)+SO(facA,facB)+TWI(facA,facB),data=dat)
summary(rsm2k)

##### Graphing contour and RSM plots####
par(mfrow=c(1,2))
contour(rsm2k, ~ facA+facB, image = TRUE, main="Contour Plot")
persp(rsm2k, facA~facB, zlab = "y",col="red",main="Response Surface Plot")
