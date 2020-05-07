################Cleaning################
# Data cleaning and multiple imputation

library(here)
library(tidyverse)
library(magrittr)
library(Amelia)
library(readstata13)
options(scipen = 999)#no scientific notation
set.seed(666)
df<-read.dta13(here("data","mexico_survey.dta"))
#things to get a sense of data:
#vtable::vtable(df)
#df %>% select(treatklj,loyalautpurity,harmcarefair,armyparticipate,punish,trustarmy,expert) %>% view()
#naniar::vis_miss(df)


#drop vars
outcomes<-c("trustarmy","armyparticipate","punish","expert")
del1 <- colnames(df[ , 1:2])
del2 <- colnames(df[ , 9:24])
del3 <- colnames(df[ , 26:31])
del4 <- colnames(df[ , 33:34])
del5 <- colnames(df[ , 36:38])
del6 <- colnames(df[ , 42:43])
del7 <- colnames(df[ , 51:59])
del8 <- colnames(df[ , 71:77])
drops <- c(del1, del2, del3, del4, del5, del6, del7, del8, "consent", "harmcarefair", "loyalautpurity", "sampleother2", "independence", "female")
keeps <- setdiff(colnames(df),drops)
df <- df[,keeps] #drop finished var which only takes on val of 1

df <- df %>% mutate(
  gender = as.numeric(gender)-1,
  urban = as.numeric(urban),
  gated = as.numeric(gated),
  armycontact = as.numeric(armycontact),
  armymurder = as.numeric(armymurder)-1,
  novictim = as.numeric(novictim)-1,
  privatization = as.numeric(privatization),
  poverty = as.numeric(poverty),
  morality = as.numeric(morality),
  autleader = as.numeric(autleader),
  viol_rights = as.numeric(viol_rights),
  armyjustice = as.numeric(armyjustice),
  armymex = as.numeric(armymex),
  armyrespect = as.numeric(armyrespect),
  armycorrupt = as.numeric(armycorrupt),
  left = as.numeric(left)-1,
  discipline = as.numeric(discipline),
  sample = as.numeric(sample),
  strongleader = as.numeric(strongleader),
  moralcitizen = as.numeric(moralcitizen),
  hardmethods = as.numeric(hardmethods),
  hardtrad = as.numeric(hardtrad),
  decency = as.numeric(decency),
  respect_aut = as.numeric(respect_aut),
  protest = as.numeric(protest),
  challenger = as.numeric(challenger),
  extremist = as.numeric(extremist),
  armyparticipate = as.numeric(armyparticipate),
  punish = as.numeric(punish),
  expert = as.numeric(expert)-1,
  trustarmy = as.numeric(trustarmy)
)

df <- df %>% mutate(
  treat = case_when(
    treatklj == 1 ~ 0,
    treatklj == 0 ~ 1
  )
)
treat_col<-which(colnames(df)=="treatklj")
df<-df[,-treat_col]

#missing data
ids<-"subid"
noms<-c("sample","gender","urban","reduceviolence","narco_rights","interest","abroad","independence","curiosity","obedience","consideration","street","expert")
noms<-setdiff(noms,drops)
ords<-c("gated","income","country_perception","years_school","lawobj","polcontact","armycontact","munsafe","colsafe","kidnap","disappear","robbery","rape","armymurder","cartelmurder","extortion","novictim","victim","familyvictim","privatization","poverty","morality","gov_strategy","autleader","armyjustice","armymex","armyrespect","armycorrupt","left","strongleader","moralcitizen","hardmethods","hardtrad","discipline","decency","respect_aut","protest","challenger","extremist","crim_rights","armyrules","armyparticipate","punish")                                                                                  
ords<-setdiff(ords,drops)
multi_impute<-amelia(df,m=20,noms=noms,ords=ords,idvars=ids)

#imputed data to estimate with
mi_df1<-multi_impute$imputations$imp1
mi_df2<-multi_impute$imputations$imp2
mi_df3<-multi_impute$imputations$imp3
mi_df4<-multi_impute$imputations$imp4
mi_df5<-multi_impute$imputations$imp5

#save data
save(list=c("multi_impute","df","outcomes",
            "drops","keeps",
            "mi_df1","mi_df2","mi_df3","mi_df4","mi_df5"),
     file=here("data","clean_data.RDS"))

save(list=c("multi_impute"),
     file=here("figures","missingness_plot.RDS"))


