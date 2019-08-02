
##Script de anova con permutaciones y comparaciones múltiples


#DRY_MATTER


#mod3 <- aovp(DRY_MATTER ~ TREATMENT, data=data)
#summary(mod3)


#library(multcomp)
#Tukey.mod3<-glht(mod3, mcp(TREATMENT = "Tukey"))
#summary(Tukey.mod3)
#cld(Tukey.mod3,level=0.05,decreasing=F)

#Para M.arvensis (Quesada)

class(dat_bm_ar_q$Tratamiento)
dat_bm_ar_q$Tratamiento<- as.factor(dat_bm_ar_q$Tratamiento)
mod3<- aovp(Biomasa_g~Tratamiento,data=dat_bm_ar_q)
summary(mod3)

library(multcomp)
Tukey.mod3<-glht(mod3, mcp(Tratamiento = "Tukey"))
summary(Tukey.mod3)
cld(Tukey.mod3,level=0.05,decreasing=F)

#Para M.arvensis Baza
class(dat_bm_ar_b$Tratamiento)
dat_bm_ar_b$Tratamiento<- as.factor(dat_bm_ar_b$Tratamiento)

mod3<- aovp(Biomasa_g~Tratamiento,data=dat_bm_ar_b)
summary(mod3)

library(multcomp)
Tukey.mod3<-glht(mod3, mcp(Tratamiento = "Tukey"))
summary(Tukey.mod3)
cld(Tukey.mod3,level=0.05,decreasing=F)

#Para M.rytidocarpoides

class(dat_bm_ry$Tratamiento)
dat_bm_ry$Tratamiento<- as.factor(dat_bm_ry$Tratamiento)


mod3<- aovp(Biomasa_g~Tratamiento,data=dat_bm_ry)
summary(mod3)

library(multcomp)
Tukey.mod3<-glht(mod3, mcp(Tratamiento = "Tukey"))
summary(Tukey.mod3)
cld(Tukey.mod3,level=0.05,decreasing=F)

#Para M.moricandioides

class(dat_bm_mo$Tratamiento)
dat_bm_mo$Tratamiento<- as.factor(dat_bm_mo$Tratamiento)

mod3<- aovp(Biomasa_g~Tratamiento,data=dat_bm_mo)
summary(mod3)

library(multcomp)
Tukey.mod3<-glht(mod3, mcp(Tratamiento = "Tukey"))
summary(Tukey.mod3)
cld(Tukey.mod3,level=0.05,decreasing=F)
