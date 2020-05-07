################Analysis################

library(here)
library(estimatr)
library(tidyverse)
library(magrittr)
library(Amelia)
library(readstata13)
library(dummies)
options(scipen = 999)#no scientific notation
load(here("data","clean_data.RDS"))
#make balance plots
outcomes
covs<-setdiff(colnames(df),outcomes) #get only covariate names
covs_df<-df[,covs] #df w/ only covariates
bin_df<-dummy.data.frame(covs_df) #df where all factors are dummies
bin_df_treat<-bin_df[bin_df$treat==1,] #df w/ treat obs
bin_df_cont<-bin_df[bin_df$treat==0,] #df w/ cont obs
#calculate absolute standardized differences in means
bin_df_sds<-apply(bin_df,2,sd,na.rm=TRUE) #sds for all vars
bin_df_treat_means<-apply(bin_df_treat,2,mean,na.rm=TRUE) #means for all vars in treat
bin_df_cont_means<-apply(bin_df_cont,2,mean,na.rm=TRUE) #means for all vars in cont
abs_sd_diffs<-abs((bin_df_treat_means-bin_df_cont_means)/bin_df_sds)#abs std diff in means
sd_diffs<-(bin_df_treat_means-bin_df_cont_means)/bin_df_sds#std diff in means
sd_diffs<-sd_diffs[names(sd_diffs)!="treat"] #get rid of treatment var b/c it'll obvs be unbalanced
sd_diffs_sort<-sort(sd_diffs)

#balance plot
#which(abs_sd_diffs>.2,ind = TRUE) find variables w/ worst balance
#plot(NA,xlim=c(0,.5),ylim=c(0,length(abs_sd_diffs)+2),main="Balance across covariates",
#     xlab="Absolute standardized differences in means across treatment and control for all variables in data",yaxt="no",
#     ylab="Different variables")
#points(abs_sd_diffs,1:length(abs_sd_diffs)+1)
#abline(v=.1,lty="dashed",col="grey")
#abline(v=.2,lty="dashed",col="grey")


       
sd_diffs_sort<-sort(sd_diffs)
sds_g<-sd_diffs_sort[which(sd_diffs_sort>0)]
sds_l<-sd_diffs_sort[which(sd_diffs_sort<0)]



expert_na_index<-which(!is.na(df$expert))
df_final<-df[expert_na_index,]


diffm_expert<-df_final%$%difference_in_means(expert~treat)
diffm_trustarmy<-df_final%$%difference_in_means(trustarmy~treat)
diffm_punish<-df_final%$%difference_in_means(punish~treat)
diffm_armyparticipate<-df_final%$%difference_in_means(armyparticipate~treat)

#models a la Lin (2013) where treatment is interacted with all covariates and robust s.e. computed
lmlin_expert<-df_final%$%lm_lin(expert~treat,
                                covariates=~gender+urban+armycontact+armycorrupt+left)
lmlin_trustarmy<-df_final%$%lm_lin(trustarmy~treat,
                                   covariates=~gender+urban+armycontact+armycorrupt+left)
lmlin_punish<-df_final%$%lm_lin(punish~treat,
                                covariates=~gender+urban+armycontact+armycorrupt+left)
lmlin_armyparticipate<-df_final%$%lm_lin(armyparticipate~treat,
                                         covariates=~gender+urban+armycontact+armycorrupt+left)


#########clean MI data
mi_list<-list(mi_df1,mi_df2,mi_df3,mi_df4,mi_df5)


model_func<-function(df,outcome){
        formula<-reformulate("treat",outcome)
        model<-lm_lin(formula,
                    covariates=~gender+urban+armycontact+armycorrupt+left,
                    data=df)
        out<-c()
        out[1]<-model$coefficients[2]
        out[2]<-model$std.error[2]
        out[3]<-model$conf.low[2]
        out[4]<-model$conf.high[2]
        return(out)
}

mi_func<-function(df){
        out<-map(outcomes,model_func,df=df)
        return(out)
}

final_out<-map(mi_list,mi_func)
unnest_model_outputs<-map(final_out,unlist)
model_output_matrix<-map(unnest_model_outputs,function(x){matrix(x,ncol=4,byrow=TRUE)})
model_ouput_avgs<-apply(simplify2array(model_output_matrix), 1:2, mean)
model_ouput_avgs_df<-as.data.frame(model_ouput_avgs)
rownames(model_ouput_avgs_df)<-outcomes
colnames(model_ouput_avgs_df)<-c("coefficient","std.error","conf.low","conf.high")


#estimates
trustarmy_estimates<-c(diffm_trustarmy$coefficients,
                       lmlin_trustarmy$coefficients[2],
                       model_ouput_avgs_df$coefficient[1])
trustarmy_ses<-c(diffm_trustarmy$std.error,
                 lmlin_trustarmy$std.error[2],
                 model_ouput_avgs_df$std.error[1])
trustarmy_low<-c(diffm_trustarmy$conf.low,
                 lmlin_trustarmy$conf.low[2],
                 model_ouput_avgs_df$conf.low[1])
trustarmy_high<-c(diffm_trustarmy$conf.high,
                  lmlin_trustarmy$conf.high[2],
                  model_ouput_avgs_df$conf.high[1])

armyparticipate_estimates<-c(diffm_armyparticipate$coefficients,
                             lmlin_armyparticipate$coefficients[2],
                             model_ouput_avgs_df$coefficient[2])
armyparticipate_ses<-c(diffm_armyparticipate$std.error,
                       lmlin_armyparticipate$std.error[2],
                       model_ouput_avgs_df$std.error[2])
armyparticipate_low<-c(diffm_armyparticipate$conf.low,
                       lmlin_armyparticipate$conf.low[2],
                       model_ouput_avgs_df$conf.low[2])
armyparticipate_high<-c(diffm_armyparticipate$conf.high,
                        lmlin_armyparticipate$conf.high[2],
                        model_ouput_avgs_df$conf.high[2])

punish_estimates<-c(diffm_punish$coefficients,
                    lmlin_punish$coefficients[2],
                    model_ouput_avgs_df$coefficient[3])
punish_ses<-c(diffm_punish$std.error,
              lmlin_punish$std.error[2],
              model_ouput_avgs_df$std.error[3])
punish_low<-c(diffm_punish$conf.low,
              lmlin_punish$conf.low[2],
              model_ouput_avgs_df$conf.low[3])
punish_high<-c(diffm_punish$conf.high,
               lmlin_punish$conf.high[2],
               model_ouput_avgs_df$conf.high[3])

expert_estimates<-c(diffm_expert$coefficients,
                    lmlin_expert$coefficients[2],
                    model_ouput_avgs_df$coefficient[4])
expert_ses<-c(diffm_expert$std.error,
              lmlin_expert$std.error[2],
              model_ouput_avgs_df$std.error[4])
expert_low<-c(diffm_expert$conf.low,
              lmlin_expert$conf.low[2],
              model_ouput_avgs_df$conf.low[4])
expert_high<-c(diffm_expert$conf.high,
               lmlin_expert$conf.high[2],
               model_ouput_avgs_df$conf.high[4])


save(list=c("sds_g","sds_l",
            "trustarmy_high","trustarmy_low","trustarmy_ses","trustarmy_estimates",
            "armyparticipate_high","armyparticipate_low","armyparticipate_ses","armyparticipate_estimates",
            "punish_high","punish_low","punish_ses","punish_estimates",
            "expert_high","expert_low","expert_ses","expert_estimates"),file=here("figures","figures.RDS"))






















