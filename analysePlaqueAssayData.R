
# Load some libraries
library(multcomp)
library(lme4)

# Load the data
plaques<-read.table("PlaqueAssayData.txt",header=T)
plaques$treat<-factor(plaques$treat)
plaques$rep<-factor(plaques$rep)
summary(plaques)

# Set control group as the baseline
contrasts(plaques$treat)=contr.treatment(levels(plaques$treat),base=3)

# Plot the data
with(plaques,plot(treat,log10(pfu),xlab="treatment",ylab="log10 pfu"))

# Fit a linear mixed model to the log-transformed data
model<-lmer(log10(pfu)~treat+(1|rep),data=plaques,REML=F)

# Model simplification
dropterm(model,test="Chi")

# Model checking
par(mfrow=c(2,1))
plot(predict(model),resid(model))
qqnorm(resid(model))
qqline(resid(model))

# Final model
summary(model)

# Pairwise comparison of treatments
summary(glht(model,linfct=mcp(treat="Tukey")))
