devtools::install_git("https://github.com/ccolonescu/PoEdata")
library(PoEdata)
data("cps4_small", package="PoEdata")

xeduc <- 12
mod1 <- lm(log(wage)~educ, data=cps4_small)
smod1 <- summary(mod1)
b1 <- coef(smod1)[[1]]
b2 <- coef(smod1)[[2]]
sighat2 <- smod1$sigma^2
yhat1 <- exp(b1+b2*cps4_small$educ+sighat2/2)
rg1 <- cor(cps4_small$wage,yhat1)^2


mod2 <- lm(wage~I(educ^2), data=cps4_small)
smod2 <- summary(mod2)
yhat2 <- predict(mod2)
sighat2 <- smod2$sigma^2
rg2 <- cor(cps4_small$wage, yhat2)^2


mod3 <- lm(wage~educ, data=cps4_small)
smod3 <- summary(mod3)
yhat3 <- predict(mod3)
sighat2 <- smod3$sigma^2
rg3 <- cor(cps4_small$wage, yhat3)^2



mod4 <- lm(log(wage)~log(educ), data=cps4_small)
smod4 <- summary(mod4)
b1 <- coef(smod4)[[1]]
b2 <- coef(smod4)[[2]]
sighat2 <- smod4$sigma^2
yhat4 <- exp(b1+b2*log(cps4_small$educ)+sighat2/2)
rg4 <- cor(cps4_small$wage,yhat4)^2

mod5 <- lm(wage~I(educ^3), data=cps4_small)
smod5 <- summary(mod5)
yhat5 <- predict(mod5)
sighat2 <- smod5$sigma^2
rg5 <- cor(cps4_small$wage, yhat5)^2

mod6 <- lm(log(wage)~I(educ^2), data=cps4_small)
smod6 <- summary(mod6)
b1 <- coef(smod6)[[1]]
b2 <- coef(smod6)[[2]]
sighat2 <- smod6$sigma^2
yhat6 <- exp(b1+b2*(cps4_small$educ)^2+sighat2/2)
rg6 <- cor(cps4_small$wage,yhat6)^2


mod7 <- lm(log(wage)~I(educ^3), data=cps4_small)
smod7 <- summary(mod7)
b1 <- coef(smod7)[[1]]
b2 <- coef(smod7)[[2]]
sighat2 <- smod7$sigma^2
yhat7 <- exp(b1+b2*(cps4_small$educ+sighat2)^3/2)
rg7 <- cor(cps4_small$wage,yhat7)^2



