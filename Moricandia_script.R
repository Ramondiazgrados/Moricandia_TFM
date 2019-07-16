---
title: "Impacto de la planta ruderal Moricandia arvensis sobre otras dos especies de Moricandia (M.rytidocarpoides y M.moricandioides"
author: "Ramón Diaz"
date: ""
output:
  html_document:
    toc: true
    number_sections: true
  pdf_document: default
  word_document: default
---
```{r}
knitr::opts_chunk$set(echo = TRUE)
options(Encoding="UTF-8")
options(tikzDefaultEngine = "xetex")
```

#Primero cargamos los paquetes necesarios
```{r}
library(readxl)
library(dplyr)
library(ggplot2)
library(calibrate)
library(MASS)
library(DataExplorer)
library(lmPerm)
```

#Segundo cargamos los datos 

1. Datos de biomasa
```{r}
dat_bm<- read_excel("Moricandia_BIOMASA_bueno.xlsx")
View(dat_bm[1:10,])
dat_bm<- dat_bm %>% 
  rename(
    Competencia= X__1)
```
2. Datos de supervivencia
```{r}
dat_sup <-read_excel("Moricandia.xlsx")
View(dat_sup[1:10,])
```
#Análisis exploratorio 
Podríamos realizar un análisis explortatorio de las variables que tenemos. (Esto podría obviarlo, porque tampoco creo que sirva mucho a parte de ver un poco la distribución de los datos)
1. Análisis exploratorio de los datos de biomasa
```{r}
plot_str(dat_bm)
plot_histogram(dat_bm)
plot_bar(dat_bm)
plot_density(dat_bm)
plot_correlation(dat_bm, type = 'continuous','Review.Date')

```

Podemos obviar los ceros del dataframe, quedándonos solmanete con las filas que pertenecen a macetas dónde sí han sobrevivido plantas y se ha podido medir la biomasa. 
```{r}
dat_bm <- dat_bm %>% filter(dat_bm$`Biomasa (mg)` != 0)
```

2. Análisis exploratorio de los datos de supervivencia
```{r}
plot_str(dat_sup)
plot_histogram(dat_sup)
plot_bar(dat_sup)
plot_density(dat_sup)
```

#Comparación de tratamientos con lmperm
Vamos a escoger solo datos de Moricandia arvensis
```{r}
dat_bm_ar<-subset(dat_bm, dat_bm$Especie=="M.arvensis")

dat_bm_ar_q <-subset(dat_bm_ar, dat_bm_ar$Localidad=="Quesada")
range(dat_bm_ar_q$`Biomasa(g)`)
summary(dat_bm_ar_q)

dat_bm_ar_b <-subset(dat_bm_ar, dat_bm_ar$Localidad=="Baza")
range(dat_bm_ar_b$`Biomasa(g)`)
summary(dat_bm_ar_b)
```

Ahora vamos a coger los datos de Moricandia rytidocarpoides

```{r}
dat_bm_ry<- subset(dat_bm, dat_bm$Especie=="M.rytidocarpoides")
range(dat_bm_ry$`Biomasa(g)`)
summary(dat_bm_ry)
```

Por último también cogemos solo los datos de Moricandia moricandioides
```{r}
dat_bm_mo <-subset(dat_bm, dat_bm$Especie=="M.moricandioides")
range(dat_bm_mo$`Biomasa(g)`)
summary(dat_bm_mo)
```

#Empezamos con PERMANOVA
Primero vamos a ver si Moricandia arvensis (Quesada) tiene diferente comportamiento según los tipos de tratamiento (densidad, suelo y competencia)

```{r}
summary(lmp(dat_bm_ar_q$`Biomasa_g`~dat_bm_ar_q$Suelo*dat_bm_ar_q$Densidad*dat_bm_ar_q$Competencia, perm="Exact"))
```

En cuanto a Moricandia arvensis (Baza), los resultados de la permanova son: 

```{r}
  summary(lmp(dat_bm_ar_b$`Biomasa_g`~dat_bm_ar_b$Suelo*dat_bm_ar_b$Densidad*dat_bm_ar_b$Competencia, perm="Exact"))
```

También lo podemos ver para Moricandia rytidocarpoides
```{r}
summary(lmp(dat_bm_ry$`Biomasa_g`~dat_bm_ry$Suelo*dat_bm_ry$Densidad*dat_bm_ry$Competencia, perm="Exact"))
```


Y finalmente para Moricandia moricandioides 

```{r}
summary(lmp(dat_bm_mo$`Biomasa_g`~dat_bm_mo$Suelo*dat_bm_mo$Densidad*dat_bm_mo$Competencia, perm="Exact"))
```

#Gráficos biomasa

Aquí está la función para calcular la media y las desviaciones de los datos para realizar así los gráficos.

```{r}
################ the functin to caclulte mean, se, sd ####################
summarySE<-function(data=NULL, measurevar, groupvars=NULL, na.rm=T,
                    conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=T) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
```

PRIMERO LOS GRÁFICOS DE MORICANDIA ARVENSIS EN CONDICIONES DE QUESADA 

1. Biomasa Moricandia arvensis según el tipo de suelo


```{r}
summ1 <- summarySE(dat_bm_ar_q, measurevar = "Biomasa_g", groupvars=c("Suelo")) ##using function below
summ1<- summ1[-5,]
ggplot(data=summ1,aes(x=Suelo,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (Q)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

2. Moricandia arvensis según el tipo de densidad

```{r}
summ2 <- summarySE(dat_bm_ar_q, measurevar = "Biomasa_g", groupvars=c("Densidad")) ##using function below
summ2<- summ2[-5,]
ggplot(data=summ2,aes(x=Densidad,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (Q)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Densidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Alta" = "Alta", "Baja" = "Baja"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```


3. Moricandia arvensis según el tipo de competencia
```{r}
summ3 <- summarySE(dat_bm_ar_q, measurevar = "Biomasa_g", groupvars=c("Competencia")) ##using function below
summ3<- summ3[-5,]
ggplot(data=summ3,aes(x=Competencia,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (Q)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Competencia"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Interespecifica" = "Interespecífica", "Intraespecifica" = "Intraespecífica"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```


4. Moricandia arvensis (Q) según el tipo de suelo y densidad

```{r}
summ4 <- summarySE(dat_bm_ar_q, measurevar = "Biomasa_g", groupvars=c("Suelo","Densidad")) ##using function below
summ4<- summ4[-5,]
ggplot(data=summ4,aes(x=Suelo,y=Biomasa_g,group=Densidad,colour=Densidad))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle("Biomasa de Moricandia arvensis (Q)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Densidad"),labels = c("Alta", "Baja "))+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1.2)))+
  theme(legend.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.80,0.30))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))

```

5. Moricandia arvensis según tipo de suelo y competencia 

```{r}
summ5 <- summarySE(dat_bm_ar_q, measurevar = "Biomasa_g", groupvars=c("Suelo","Competencia")) ##using function below
summ5<- summ5[-5,]
ggplot(data=summ5,aes(x=Suelo,y=Biomasa_g,group=Competencia,colour=Competencia))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (Q)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Competencia"),labels = c("Interespecífica", "Intraespecífica "))+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1.1)))+
  theme(legend.text  = element_text(size = rel(1.1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.73,0.20))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

6.Biomasa Moricandia arvensis según la competencia y densidad 


```{r}
summ6 <- summarySE(dat_bm_ar_q, measurevar = "Biomasa_g", groupvars=c("Densidad","Competencia")) ##using function below
summ6<- summ6[-5,]
ggplot(data=summ6,aes(x=Densidad,y=Biomasa_g,group=Competencia,colour=Competencia))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (Q)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Competencia"),labels = c("Interespecífica", "Intraespecífica "))+
  labs(x=c("Densidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Alta","Baja"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1)))+
  theme(legend.text  = element_text(size = rel(1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.25,0.18), legend.background = element_blank())+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

MORICANDIA ARVENSIS EN CONDICIONES DE BAZA

7. Biomasa Moricandia arvensis(B) según el tipo de suelo


```{r}
summ7 <- summarySE(dat_bm_ar_b, measurevar = "Biomasa_g", groupvars=c("Suelo")) ##using function below
summ7<- summ7[-5,]
ggplot(data=summ7,aes(x=Suelo,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (B)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

8. Moricandia arvensis (B) según el tipo de densidad

```{r}
summ8 <- summarySE(dat_bm_ar_b, measurevar = "Biomasa_g", groupvars=c("Densidad")) ##using function below
summ8<- summ8[-5,]
ggplot(data=summ8,aes(x=Densidad,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (B)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Densidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Alta" = "Alta", "Baja" = "Baja"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```


9. Moricandia arvensis(B) según el tipo de competencia
```{r}
summ9 <- summarySE(dat_bm_ar_b, measurevar = "Biomasa_g", groupvars=c("Competencia")) ##using function below
summ9<- summ9[-5,]
ggplot(data=summ9,aes(x=Competencia,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (B)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Competencia"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Interespecifica" = "Interespecífica", "Intraespecifica" = "Intraespecífica"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```


10. Moricandia arvensis (B) según el tipo de suelo y densidad

```{r}
summ10 <- summarySE(dat_bm_ar_b, measurevar = "Biomasa_g", groupvars=c("Suelo","Densidad")) ##using function below
summ10<- summ10[-5,]
ggplot(data=summ10,aes(x=Suelo,y=Biomasa_g,group=Densidad,colour=Densidad))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle("Biomasa de Moricandia arvensis (B)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Densidad"),labels = c("Alta", "Baja "))+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1.2)))+
  theme(legend.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.80,0.30))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))

```

11. Moricandia arvensis(B) según tipo de suelo y competencia 

```{r}
summ11 <- summarySE(dat_bm_ar_b, measurevar = "Biomasa_g", groupvars=c("Suelo","Competencia")) ##using function below
summ11<- summ11[-5,]
ggplot(data=summ11,aes(x=Suelo,y=Biomasa_g,group=Competencia,colour=Competencia))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (B)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Competencia"),labels = c("Interespecífica", "Intraespecífica "))+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1.1)))+
  theme(legend.text  = element_text(size = rel(1.1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.75,0.20), legend.background = element_blank())+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

12.Biomasa Moricandia arvensis(B) según la competencia y densidad 


```{r}
summ12 <- summarySE(dat_bm_ar_b, measurevar = "Biomasa_g", groupvars=c("Densidad","Competencia")) ##using function below
summ12<- summ12[-5,]
ggplot(data=summ12,aes(x=Densidad,y=Biomasa_g,group=Competencia,colour=Competencia))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis (B)")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Competencia"),labels = c("Interespecífica", "Intraespecífica "))+
  labs(x=c("Densidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Alta","Baja"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1)))+
  theme(legend.text  = element_text(size = rel(1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.73,0.80))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

GRÁFICOS DE MORICANDIA RYTIDOCARPOIDES

13. Biomasa Moricandia rytidocarpoides según el tipo de suelo


```{r}
summ13 <- summarySE(dat_bm_ry, measurevar = "Biomasa_g", groupvars=c("Suelo")) ##using function below
summ13<- summ13[-5,]
ggplot(data=summ13,aes(x=Suelo,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia rytidocarpoides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

14. Moricandia rytidocarpoides según el tipo de densidad

```{r}
summ14 <- summarySE(dat_bm_ry, measurevar = "Biomasa_g", groupvars=c("Densidad")) ##using function below
summ14<- summ14[-5,]
ggplot(data=summ14,aes(x=Densidad,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia rytidocarpoides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Densidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Alta" = "Alta", "Baja" = "Baja"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```


15. Moricandia rytodcarpoides según el tipo de competencia
```{r}
summ15 <- summarySE(dat_bm_ry, measurevar = "Biomasa_g", groupvars=c("Competencia")) ##using function below
summ15<- summ15[-5,]
ggplot(data=summ15,aes(x=Competencia,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia rytidocarpoides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Competencia"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Interespecifica" = "Interespecífica", "Intraespecifica" = "Intraespecífica"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```


16. Moricandia rtydocarpoides según el tipo de suelo y densidad

```{r}
summ16 <- summarySE(dat_bm_ry, measurevar = "Biomasa_g", groupvars=c("Suelo","Densidad")) ##using function below
summ16<- summ16[-5,]
ggplot(data=summ16,aes(x=Suelo,y=Biomasa_g,group=Densidad,colour=Densidad))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle("Biomasa Moricandia rytidocarpoides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Densidad"),labels = c("Alta", "Baja "))+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1.2)))+
  theme(legend.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.80,0.20))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))

```

17. Moricandia rytidocarpoides según tipo de suelo y competencia 

```{r}
summ17 <- summarySE(dat_bm_ry, measurevar = "Biomasa_g", groupvars=c("Suelo","Competencia")) ##using function below
summ17<- summ17[-5,]
ggplot(data=summ17,aes(x=Suelo,y=Biomasa_g,group=Competencia,colour=Competencia))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia rytidocarpoides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Competencia"),labels = c("Interespecífica", "Intraespecífica "))+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1.1)))+
  theme(legend.text  = element_text(size = rel(1.1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.28,0.83))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```
18.Biomasa Moricandia rytidocarpoides según la competencia y densidad 


```{r}
summ18 <- summarySE(dat_bm_ry, measurevar = "Biomasa_g", groupvars=c("Densidad","Competencia")) ##using function below
summ18<- summ18[-5,]
ggplot(data=summ18,aes(x=Densidad,y=Biomasa_g,group=Competencia,colour=Competencia))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia rytidocarpoides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Competencia"),labels = c("Interespecífica", "Intraespecífica "))+
  labs(x=c("Densidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Alta","Baja"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1)))+
  theme(legend.text  = element_text(size = rel(1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.73,0.80))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

GRÁFICOS DE MORICANDIA MORICANDIOIDES

19. Biomasa Moricandia moricandioides según el tipo de suelo


```{r}
summ19 <- summarySE(dat_bm_mo, measurevar = "Biomasa_g", groupvars=c("Suelo")) ##using function below
summ19<- summ19[-5,]
ggplot(data=summ19,aes(x=Suelo,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia moricandioides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

20. Moricandia rytidocarpoides según el tipo de densidad

```{r}
summ20 <- summarySE(dat_bm_mo, measurevar = "Biomasa_g", groupvars=c("Densidad")) ##using function below
summ20<- summ20[-5,]
ggplot(data=summ20,aes(x=Densidad,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia moricandioides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Densidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Alta" = "Alta", "Baja" = "Baja"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```


21. Moricandia moricandioides según el tipo de competencia
```{r}
summ21 <- summarySE(dat_bm_mo, measurevar = "Biomasa_g", groupvars=c("Competencia")) ##using function below
summ21<- summ21[-5,]
ggplot(data=summ21,aes(x=Competencia,y=Biomasa_g))+
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia moricandioides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  labs(x=c("Competencia"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Interespecifica" = "Interespecífica", "Intraespecifica" = "Intraespecífica"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```


22. Moricandia moricandioides según el tipo de suelo y densidad

```{r}
summ22 <- summarySE(dat_bm_mo, measurevar = "Biomasa_g", groupvars=c("Suelo","Densidad")) ##using function below
summ22<- summ22[-5,]
ggplot(data=summ22,aes(x=Suelo,y=Biomasa_g,group=Densidad,colour=Densidad))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle("Biomasa de Moricandia moricandioides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Densidad"),labels = c("Alta", "Baja "))+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1.2)))+
  theme(legend.text  = element_text(size = rel(1.2)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.80,0.20))+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))

```

23. Moricandia moricandioides según tipo de suelo y competencia 

```{r}
summ23 <- summarySE(dat_bm_mo, measurevar = "Biomasa_g", groupvars=c("Suelo","Competencia")) ##using function below
summ23<- summ23[-5,]
ggplot(data=summ23,aes(x=Suelo,y=Biomasa_g,group=Competencia,colour=Competencia))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia moricandioides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Competencia"),labels = c("Interespecífica", "Intraespecífica "))+
  labs(x=c("Suelo"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("No" = "No perturbado", "Si" = "Perturbado"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1.1)))+
  theme(legend.text  = element_text(size = rel(1.1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.28,0.83), legend.background = element_blank())+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

24.Biomasa Moricandia moricandioides según la competencia y densidad 


```{r}
summ24 <- summarySE(dat_bm_mo, measurevar = "Biomasa_g", groupvars=c("Densidad","Competencia")) ##using function below
summ24<- summ24[-5,]
ggplot(data=summ24,aes(x=Densidad,y=Biomasa_g,group=Competencia,colour=Competencia))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia moricandioides")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Competencia"),labels = c("Interespecífica", "Intraespecífica "))+
  labs(x=c("Densidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Alta","Baja"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1)))+
  theme(legend.text  = element_text(size = rel(1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.72,0.86), legend.background = element_blank())+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

Podemos hacer gráficos comparando Moricandia arvensis según localicad (Baza o Quesada)

```{r}
summ25 <- summarySE(dat_bm_ar, measurevar = "Biomasa_g", groupvars=c("Localidad","Suelo")) ##using function below
summ25<- summ25[-5,]
ggplot(data=summ25,aes(x=Localidad,y=Biomasa_g,group=Suelo,colour=Suelo))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Suelo"),labels = c("No"="No perturbado", "Si"="Perturbado"))+
  labs(x=c("Localidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Quesada"="Quesada","Baza"="Baza"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1)))+
  theme(legend.text  = element_text(size = rel(1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.72,0.46), legend.background = element_blank())+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```


```{r}
summ26 <- summarySE(dat_bm_ar, measurevar = "Biomasa_g", groupvars=c("Localidad","Competencia")) ##using function below
summ26<- summ26[-5,]
ggplot(data=summ26,aes(x=Localidad,y=Biomasa_g,group=Competencia,colour=Competencia))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Competencia"),labels = c("Interespecífica", "Intraespecífica "))+
  labs(x=c("Localidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Quesada"="Quesada","Baza"="Baza"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1)))+
  theme(legend.text  = element_text(size = rel(1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.72,0.46), legend.background = element_blank())+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

```{r}
summ27 <- summarySE(dat_bm_ar, measurevar = "Biomasa_g", groupvars=c("Localidad","Densidad")) ##using function below
summ27<- summ27[-5,]
ggplot(data=summ27,aes(x=Localidad,y=Biomasa_g,group=Densidad,colour=Densidad))+
  geom_line(size=1.5)+  
  geom_point(size=3)+
  ggtitle(" Biomasa Moricandia arvensis")+
  geom_errorbar(aes(ymin=Biomasa_g-se,ymax=Biomasa_g +se),width=0.1,position = position_dodge(0.01),size=1.5)+
  scale_color_manual(values=c("red3","royalblue1"),name=c("Densidad"),labels = c("Alta", "Baja "))+
  labs(x=c("Localidad"),y=c("Biomasa(g)"))+
  scale_x_discrete(labels=c("Quesada"="Quesada","Baza"="Baza"))+
  theme(axis.title = element_text(size = rel(1.5)))+
  theme(axis.text  = element_text(size = rel(1.2)))+
  theme(legend.title = element_text(size = rel(1)))+
  theme(legend.text  = element_text(size = rel(1)))+
  theme(axis.line = element_line(color = 'black'))+
  theme(legend.position=c(0.80,0.15), legend.background = element_blank())+
  theme( panel.grid.minor = element_blank()
         ,panel.background = element_rect(fill="white")
         ,panel.border = element_rect(fill=NA, color = "black"))
```

