library(dplyr)
library(geosphere)
library(readxl)

# Data Import, Data Frame Construction:

# pasted_data.df <- read.table(pipe("pbpaste"), sep="\t", header=TRUE)
pasted_data.df <- readxl::read_excel("data/sep_21/9_21_23_290Meeting_Data.xlsx")
pasted_data.df

dim(pasted_data.df) # Should have 149 rows
r_data.1.df <- pasted_data.df[1:149, ]
dim(r_data.1.df)

#Data transformations:


r_data.1.df$logperi<-I(log(r_data.1.df$periarea )) 
I()
r_data.1.df <- mutate(r_data.1.df, 
                      corin=r_data.1.df$column=="Corinthian")

r_data.1.df <- mutate(r_data.1.df, 
                      doric=r_data.1.df$column=="Doric"|
                          r_data.1.df$column=="Doric with moulding"|
                          r_data.1.df$column=="Ionic and Doric")

r_data.1.df <- mutate(r_data.1.df, 
                      ionic=r_data.1.df$column=="Ionic"|
                          r_data.1.df$column=="Ionic and Doric")

r_data.1.df <- mutate(r_data.1.df, tuscan=r_data.1.df$column=="Tuscan")

r_data.1.df <- mutate(r_data.1.df, pillar=r_data.1.df$column=="no columns")

r_data.1.df <- mutate(r_data.1.df, latent=r_data.1.df$column=="unknown")


# Distance from Amplitheater of Pompeii (in meters using exact geo-distance)
## p2 is Amplitheater of Pompeii location

p2<-c(40.7512, 14.4952)
PAGD<- rep(0,dim(r_data.1.df)[[1]])
for(j in 1:dim(r_data.1.df)[[1]]){
    PAGD[[j]]<-distGeo(c(r_data.1.df$lat[[j]],r_data.1.df$long[[j]]), p2)}

## Conversion to kilometers:
r_data.1.df$PeAqGeoD<- PAGD/1000


# Corinthian Column Model (7/10/23: should have Std.Error .7029 and P=.0038)
II.pomp.1.Corin.lm <- glm(visib~PeAqGeoD+logperi+corin,
                          family=binomial(link= "logit"), data=r_data.1.df )
summary(II.pomp.1.Corin.lm) 

dim(r_data.1.df)
r_data.1.df[[5]]
length(r_data.1.df[[5]])
sum(r_data.1.df[[5]])
length(r_data.1.df[[5]])-sum(r_data.1.df[[5]])


# Doric Column Model  
II.pomp.1.Doric.lm <- glm(visib~PeAqGeoD+logperi+doric,
                          family=binomial(link= "logit"), data=r_data.1.df )
summary(II.pomp.1.Doric.lm) 

# Doric Column Model 
II.pomp.1.CorinDoric.lm <- glm(visib~ PeAqGeoD+logperi+corin+doric,
                               family=binomial(link= "logit"), data=r_data.1.df )
summary(II.pomp.1.CorinDoric.lm) 


# corinthian Column Model w/o log(pa)  
II.pomp.1.CorinWOlogpa.lm <- glm(visib~ PeAqGeoD+corin ,
                                 family=binomial(link= "logit"), data=r_data.1.df )
summary(II.pomp.1.CorinWOlogpa.lm) 

# corinthian Column Model w/o PeAqGeoD  
II.pomp.1.CorinWOd.lm <- glm(visib~ logperi+corin ,
                             family=binomial(link= "logit"), data=r_data.1.df )
summary(II.pomp.1.CorinWOd.lm) 




# Code for printable ANOVA tables
library(car)
anova(II.pomp.1.Corin.lm)[,c(1,3)]
anova(II.pomp.1.Corin.lm)
objects(summary(II.pomp.1.Corin.lm))
round(summary(II.pomp.1.Corin.lm)$coefficients,4)
round(summary(II.pomp.1.Corin.lm)$coefficients[c(2,3,4,1),c(1,2,4)],4)

anova.print<-round(summary(II.pomp.1.Corin.lm)$coefficients[c(2,3,4,1),c(1,2,4)],4)
colnames(anova.print) = c("Coef. Estimate", "Std. Error", "P-value")
rownames(anova.print) = c("Corinthian", "Distance", "log(pa)", "Intercept")

typeof(anova.print)
anova.print.df<-as.data.frame(anova.print)



library(sjPlot)
library(stargazer)

tab_model(II.pomp.1.Corin.lm)

tab_model(II.pomp.1.Corin.lm, transform = NULL, show.se = TRUE,
          show.ci = FALSE, digits.p = 4, p.style = "numeric_stars",
          show.r2 = FALSE, show.aic = TRUE, string.se = "  Std. Error  ",
          string.p = "P-value",  dv.labels = "Visibility")
# string.ci = "95% CI", ...



tab_model(II.pomp.1.Corin.lm, transform = NULL, show.se = TRUE,
          digits.p = 4, p.style = "numeric",
          show.r2 = FALSE, show.aic = TRUE, string.se = "Std. Error",
          string.ci = "95% CI", string.p = "P-value",  dv.labels = "Visibility",
          pred.labels = c("(Intercept)","Corinthian","Distance","log(Peri. Area)"),
          CSS = list(css.thead = 'padding:0.1cm',
                     css.tdata = 'padding:0.2cm',
                     css.depvarhead = '+font-size:12px;',
                     css.summary = '+font-size:14px;',
                     css.summary = 'text-align: left;'))


#Comparison of models

## "fig 3"
tab_model(II.pomp.1.Corin.lm, II.pomp.1.Doric.lm, 
          transform = NULL, show.se = TRUE,
          digits.p = 4, p.style = "numeric",
          show.r2 = FALSE, show.aic = TRUE, string.se = "Std. Error",
          string.ci = "95% CI", string.p = "P-value", 
          dv.labels = c("Corinthian visibility predictor", 
                        "Doric visibility predictor"),
          pred.labels = c("(Intercept)","Distance","log(pa)",
                          "Corinthian","Doric"),
          CSS = list(css.depvarhead = '+font-size:12px;',
                     css.summary = '+font-size:14px;',
                     css.summary = 'text-align: left;'), file = "CorinWOlogpa81023.doc")


## "fig 4"

tab_model(II.pomp.1.Corin.lm, II.pomp.1.CorinWOlogpa.lm , II.pomp.1.CorinWOd.lm,
          transform = NULL, show.se = TRUE,
          digits.p = 4, p.style = "numeric",
          show.r2 = FALSE, show.aic = TRUE, string.se = "Std. Error",
          show.ci = FALSE, string.p = "P-value", 
          dv.labels = c("Full model", "Without log(pa)", "Without d
                        
                        istance"),
          pred.labels = c("(Intercept)","Distance","log(pa)","Corinthian"),
          CSS = list(css.depvarhead = '+font-size:12px;',
                     css.summary = '+font-size:14px;'), file = "CorinWOlogpaWOd.doc")



tab_model(II.pomp.1.Corin.lm, II.pomp.1.CorinWOd.lm , 
          transform = NULL, show.se = TRUE,
          digits.p = 4, p.style = "numeric",
          show.r2 = FALSE, show.aic = TRUE, string.se = "Std. Error",
          show.ci = FALSE, string.p = "P-value", 
          dv.labels = c("With Distance", "Without Distance"),
          pred.labels = c("(Intercept)","Distance","log(Peri. Area)","Corinthian"),
          CSS = list(css.depvarhead = '+font-size:14px;',
                     css.summary = '+font-size:14px;'), file = "CorinWOd.pdf" )



tableGrob(II.pomp.1.Corin.lm)





