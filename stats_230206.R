#AIM: Stats and graphs for the audience experiment, Is  feeding rate influenced by the simulated presence of novel individuals?
#Author: D'Amelio Pietro B.


#libraries
library(lme4)
library(sjPlot)
library(emmeans)
library(DHARMa)
library(optimx)
library(ggplot2)
library(ggdist)
library(EnvStats)
library(dplyr)


#load the dataset
dat<-read.csv("C:/Users/pietr/Documents/Sociable_Weavers/Audience_effect/Datasets/audience_dataset.csv")



#average group size

datgs<-unique(dat[ , c("BirdID", "ColNestLaying")])


gs<-data.frame(count(datgs, ColNestLaying ))
mean(gs$n)
#3.617886
sd(gs$n)
#1.422998

#pairs with helpers
length(which(gs$n==2))
#31
(123-31)/123
#0.7479675


#number of helpers per sex

daths<-unique(dat[ , c("BirdID", "ColNestLaying", "sex", "class_breeder")])

length(daths$BirdID[daths$class_breeder=="helper"])
#201
length(na.omit(daths$BirdID[daths$class_breeder=="helper" & daths$sex=="M"]))
#142
length(na.omit(daths$BirdID[daths$class_breeder=="helper" & daths$sex=="F"]))
#55



#erase birds for which we have no sex
dat<-subset(dat, !is.na(dat$sex))

#ID and breeding attempts of the birds without sex
# > dat$BirdID[which(is.na(dat$sex))]
# [1] "U" "U" "U" "U"
# > dat$ColNestLaying[which(is.na(dat$sex))]
# [1] "2_41_2017-10-05"  "2_2_2017-09-27"   "7_11_2017-12-15"  "42_21_2017-12-14"




#plot age distribution of male and female helpers

datha<-unique(dat[ , c("BirdID", "ColNestLaying", "sex", "class_breeder", "age_seasons")])


layout(matrix(c(1,2), 2, 2, byrow = TRUE))
barplot(table(na.omit(datha$age_seasons[datha$class_breeder=="helper" & datha$sex=="F"])),ylab = "N female helpers", xlab="age (seasons)")

barplot(table(na.omit(datha$age_seasons[datha$class_breeder=="helper" & datha$sex=="M"])),ylab = "N male helpers", xlab="age (seasons)")

title("Helpers age distribution", line = -1, outer = T, cex.main=1.3)
#export as EPS 870 x 350
#some adjustment needed in post production because of overlapping labels



#average chick age
datca<-unique(dat[ , c("ColNestLaying","D_chicks")])


datca<-datca %>% 
  group_by(ColNestLaying) %>% 
  slice_min(order_by = D_chicks)

mean(datca$D_chicks)
sd(datca$D_chicks)

#plot age distribution of the used chicks


barplot(table(datca$D_chicks),ylab = "N of nests", xlab="age (days)", main = " Chicks' age")

#exported as eps 750? x 350







#do I want to account for Bird ID over the whole dataset or for Bird ID per breeding attempt?
#the same bird can be a breeder and a helper in different seasons/breeding attempts
#it should not make much of a difference but you can try both strategies and see
##updates: decided to have BirdID per breeding attempt because it can be
#          over different seasons and have different roles (helpers vs breeders)


#transform numeric to factor when necessary
dat$Recording_day<-as.factor(dat$Recording_day)
dat$Playback_file<-as.factor(dat$Playback_file)
dat$Colony<-as.factor(dat$Colony)

#scale continuous variables
dat$Time_Since_sun_rise.z<-scale(dat$Time_Since_sun_rise)
dat$N_chicks.z<-scale(dat$N_chicks)
dat$D_chicks.z<-scale(dat$D_chicks)
dat$wind.z<-scale(dat$wind)
dat$temperature.z<-scale(dat$temperature)














#do recording day, playback order and playback files need to be random or fixed effect?

#run with all of them fixed

mod<-glmer(Feeding_visits~ Playback_type*class_breeder*sex +
             Time_Since_sun_rise.z+
             N_chicks.z+ 
             D_chicks.z+ 
             temperature.z+
             wind.z+
             Recording_day +
             playback +
             Playback_file +
             (1|Season/Colony/Nest/BirdID_CNL), data=dat ,family ="poisson",
           glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))


#if playback file is fixed it throws an "error" saying that 
#fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients

#this is due to because "playback file", if I put it as random factor, it does not throw the warning
#is it because the playback file 13 was only used in my year?
#(I had to add a male playback because one male playback turned out to be a male)

dat1<-subset(dat, Playback_file!="13")
dat1$Playback_file<- factor(dat1$Playback_file)


mod<-glmer(Feeding_visits~ Playback_type*class_breeder*sex +
             Time_Since_sun_rise.z+
             N_chicks.z+ 
             D_chicks.z+ 
             temperature.z+
             wind.z+
             Recording_day +
             playback +
             Playback_file +
             (1|Season/Colony/Nest/BirdID_CNL), data=dat1 ,family ="poisson",
           glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))


#NO, it gives the same error


#playback file should be a random factor, 
#these are just some of the individuals we could have chosen to playback.
#each file is used only for one sex


mod<-glmer(Feeding_visits~ Playback_type*class_breeder*sex +
             Time_Since_sun_rise.z+
             N_chicks.z+ 
             D_chicks.z+ 
             temperature.z+
             wind.z+
             Recording_day +
             playback +
             (1|Playback_file) +
             (1|Season/Colony/Nest/BirdID_CNL), data=dat,family ="poisson")

# Warning message:
#   In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#                  Model failed to converge with max|grad| = 0.0291039 (tol = 0.002, component 1)
# 

#convergence issue, this can be solved by changing the optimizer

###FINAL MODEL ####
#add optimization for the convergence issue 
mod<-glmer(Feeding_visits~ Playback_type*class_breeder*sex +
             Time_Since_sun_rise.z+
             N_chicks.z+ 
             D_chicks.z+ 
             temperature.z+
             wind.z+
             Recording_day +
             playback +
             (1|Playback_file) +
             (1|Season/Colony/Nest/BirdID_CNL), data=dat ,family ="poisson", 
           glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))


# boundary (singular) fit: see help('isSingular')
# Warning message:
#   In optimx.check(par, optcfg$ufn, optcfg$ugr, optcfg$uhess, lower,  :
#                     Parameters or bounds appear to have different scalings.
#                   This can cause poor performance in optimization. 
#                   It is important for derivative free methods like BOBYQA, UOBYQA, NEWUOA.
# 

#the results are almost identical with and without optimization



#check model assumption
plotQQunif(mod)
plotResiduals(mod)
testDispersion(mod)

simulationOutput <- simulateResiduals(fittedModel = mod)
plot(simulationOutput)
testQuantiles(simulationOutput)
# all assumptions met



#testZeroInflation(mod) #to check for zero inflation if birds with 0  feeding rate were included





#check if specific female playback files gave different results for the males

datF<-subset(dat, Playback_type=="F")
datF_M<-subset(datF, sex=="M")


modF<-glmer(Feeding_visits~ class_breeder +
             Time_Since_sun_rise.z+
             N_chicks.z+ 
             D_chicks.z+ 
             temperature.z+
             wind.z+
             #Recording_day +
             #playback +
             Playback_file +
             (1|Season/Colony/Nest/BirdID_CNL), data=datF_M ,family ="poisson", 
           glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))



plot_model(modF, type = "pred", show.data = T)

plot_model(modF)

#no difference between the different playbacks



#check the same only for helpers
datF_M_H<-subset(datF_M, class_breeder=="helper")

modF_M_H<-glmer(Feeding_visits~ Time_Since_sun_rise.z+
              N_chicks.z+ 
              D_chicks.z+ 
              temperature.z+
              wind.z+
              #Recording_day +
              #playback +
              Playback_file +
              (1|Season/Colony/Nest/BirdID_CNL), data=datF_M_H ,family ="poisson", 
            glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))

plot_model(modF_M_H)

#no difference between the different playbacks
#even just for male helpers






#does the reference season matters for the plots/p-values? #change the reference group and check

dat$Recording_day<-ifelse(dat$Recording_day=="1", "4", dat$Recording_day)

dat$Season<-ifelse(dat$Season=="2019/2020", "1019/2022", dat$Season)

dat$Playback_file<-ifelse(dat$Playback_file=="1", "13", dat$Playback_file)

dat$playback<-ifelse(dat$playback=="five", "zfive", dat$playback)




####TABLE #####

tab_model(mod,file="audience.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)





#post-hocs


emmeans(mod, pairwise ~Playback_type|sex|class_breeder, adjust = "bon")


ph<-as.data.frame(pairs(emmeans(mod,  ~Playback_type|sex|class_breeder), adjust = "bonferroni"))










##PLOT ####

##Plot raw data

#simple boxplot
boxplot(Feeding_visits ~ Playback_type:class_breeder:sex, data = dat) 

#raw data points
ggplot(aes(x=Playback_type, y=Feeding_visits, color=class_breeder), data = dat) +
  facet_grid(class_breeder~ sex)+
  geom_jitter(width = 0.2, alpha=0.3)


#define the colors you want to use (chosen from color blind palettes http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
cbPalette <- c("#E69F00", "#56B4E9")


#assign labels names for sex and breeder types
sexes <- c(
  `F` = "Females",
  `M` = "Males")
classbreeder<- c(
  `parent` = "Parents",
  `helper` = "Helpers")


#plot raw data, box plot and density distribution
#facet by sex and breeder type
ggplot(aes(x=Playback_type, y=Feeding_visits, color=class_breeder, fill=class_breeder), data = dat) +
  facet_grid(class_breeder~ sex, labeller = labeller(sex= as_labeller(sexes),
                                                     class_breeder=classbreeder))+
  
  
  stat_halfeye(
    ## custom bandwidth
    #adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.4, 
    ## remove slab interval
    .width = 0, 
    slab_color="black",
    slab_size=0.5,
    #slab_fill="#E69F00",
    point_colour = NA)+ 
  
  geom_boxplot(
    width = .15, 
    outlier.shape = NA,
    fill=NA,
    color="black"
  ) +
  
  geom_jitter(width = 0.15, height = 0.07, alpha=0.3)+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(x = "Playback type", y= "Feeding visits")+
  theme_classic()+
  scale_x_discrete(labels=c("Control","Female", "Male"))+
  theme(legend.position = "none")



#plot model estimates
plot_model(mod, type = "pred", show.data = T)
plot_model(mod)

plot_model(mod, type = "int" , show.data = T)+ geom_jitter()


plot_model(mod, type = "int", terms =c("Playback_type","class_breeder","sex"))



#integrate raw data with model estimates

p<-get_model_data(mod, type = "int", terms =c("Playback_type","class_breeder","sex") )

#extract estimates
estimates<-data.frame(p[[4]])

#rename columns to easily merge with original dataset
colnames(estimates)[which(names(estimates) == "group")] <- "class_breeder"
colnames(estimates)[which(names(estimates) == "facet")] <- "sex"
colnames(estimates)[which(names(estimates) == "x")] <- "Playback_type"

estimates$Playback_type[which(estimates$Playback_type=="1")]<-"C"
estimates$Playback_type[which(estimates$Playback_type=="2")]<-"F"
estimates$Playback_type[which(estimates$Playback_type=="3")]<-"M"

estimates$group_col<-NULL

#merge dataset and estimates
dat_model<-merge(dat,estimates, by=c("Playback_type", "class_breeder", "sex") )
#dat_model$interaction<-paste(dat_model$Playback_type, dat_model$class_breeder, dat_model$sex, sep = "_")

#define the colors you want to use (chosen from color blind palettes http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
cbPalette <- c("#E69F00", "#56B4E9")


#assign labels names for sex and breeder types
# sexes <- c(
#   `F` = "Females",
#   `M` = "Males")
classbreeder<- c(
  `parent` = "Parents",
  `helper` = "Helpers")



#put males first
dat_model$sex[dat_model$sex=="M"]<-"aM"
sexes <- c(
  `F` = "Females",
  `aM` = "Males")

ggplot(aes(x=factor(Playback_type,levels=c("F","C","M")), 
           y=Feeding_visits, color=class_breeder, fill=class_breeder), data = dat_model) +
  facet_grid(class_breeder ~ sex,  scales = "free_x", space = "free_x", 
             labeller = labeller(sex= as_labeller(sexes),
                                 class_breeder=classbreeder))+
  
  stat_halfeye(
    ## custom bandwidth
    #adjust = .5, 
    ## adjust height
    width = .5, 
    ## move geom to the right
    justification = -.4, 
    ## remove slab interval
    .width = 0, 
    slab_color="black",
    slab_size=0.5,
    #slab_fill="#E69F00",
    point_colour = NA)+ 
  
  geom_boxplot(
    width = .12,
    outlier.shape = NA,
    fill=NA,
    #color="black",
    position= position_nudge(x=-.25)
  ) +
  
  geom_jitter(width = 0.12, height = 0.07, alpha=0.3)+
  
  
  geom_segment(aes( y=conf.low, yend=conf.high, 
                    x=as.numeric(factor(Playback_type,levels=c("F","C","M") )),
                    xend=as.numeric(factor(Playback_type,levels=c("F","C","M") ))),
               color="black", size=1.1)+
  geom_point(aes( y=predicted, x=as.numeric(factor(Playback_type,levels=c("F","C","M") ))),
             color="black", size=2.5)+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(x = "Playback type", y= "Feeding visits")+
  theme_classic()+
  scale_x_discrete(labels=c("Female","Control", "Male"))+
  theme(legend.position = "none")+
  stat_n_text(y.pos = 17)

#extract as a pdf, 6.35 x 5 (inc)









### male helpers ####
#Follow up analysis focusing only on male helpers, the group of birds where we expect effect for the treatment. 

dat_malehelp<-subset(dat, sex=="M"& class_breeder=="helper")


#scale
dat_malehelp$Time_Since_sun_rise.z<-scale(dat_malehelp$Time_Since_sun_rise)
dat_malehelp$N_chicks.z<-scale(dat_malehelp$N_chicks)
dat_malehelp$D_chicks.z<-scale(dat_malehelp$D_chicks)
dat_malehelp$wind.z<-scale(dat_malehelp$wind)
dat_malehelp$temperature.z<-scale(dat_malehelp$temperature)
dat_malehelp$age_seasons.z<-scale(dat_malehelp$age_seasons)



#plot feeding rate by age (in seasons). Do older birds feed more? (not at all)

plot(jitter(dat_malehelp$age_seasons),dat_malehelp$Feeding_visits )

ggplot(aes(x=as.factor(age_seasons), y=Feeding_visits), data = dat_malehelp)+
  geom_violin()+ geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.5)




#model

mod_malehelp<-glmer(Feeding_visits~ Playback_type*age_categorical +
             Time_Since_sun_rise.z+
             N_chicks.z+ 
             D_chicks.z+ 
             temperature.z+
             wind.z+
             Recording_day +
             playback +
             (1|Playback_file) +
             (1|Season/Colony/Nest/BirdID_CNL), data=dat_malehelp,family ="poisson",
             glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))




#asses fitting

plotQQunif(mod_malehelp)
plotResiduals(mod_malehelp)

#fitting is very good


#plot the results

plot_model(mod_malehelp, type = "pred", show.data = T)

plot_model(mod_malehelp, type = "int", show.data = T)



#integrate raw data with model estimates

estimates_h<-get_model_data(mod_malehelp, type = "int", terms =c("Playback_type","age_categorical") )

colnames(estimates_h)[which(names(estimates_h) == "group")] <- "age_categorical"
colnames(estimates_h)[which(names(estimates_h) == "x")] <- "Playback_type"

estimates_h$Playback_type[which(estimates_h$Playback_type=="1")]<-"C"
estimates_h$Playback_type[which(estimates_h$Playback_type=="2")]<-"F"
estimates_h$Playback_type[which(estimates_h$Playback_type=="3")]<-"M"

estimates_h$group_col<-NULL

dat_model_h<-merge(dat_malehelp,estimates_h, by=c("Playback_type", "age_categorical") )










#define the colors you want to use (chosen from color blind palettes http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
cbPalette <- c("#F0E442", "#D55E00")


#assign labels names for categories of age

agecategorical<- c(
  `could_breed` = "Old enough to breed",
  `too_young` = "Too young to breed")




ggplot(aes(x=factor(Playback_type,levels=c("F","C","M")), 
           y=Feeding_visits, color=age_categorical, fill=age_categorical), data = dat_model_h) +
  
   facet_grid(cols  = vars(age_categorical),
              labeller = labeller(age_categorical= as_labeller(agecategorical)))+
  
  stat_halfeye(
    ## custom bandwidth
    #adjust = .5, 
    ## adjust height
    width = .5, 
    ## move geom to the right
    justification = -.4, 
    ## remove slab interval
    .width = 0, 
    slab_color="black",
    slab_size=0.5,
    #slab_fill="#E69F00",
    point_colour = NA)+ 
  
  geom_boxplot(
    width = .12,
    outlier.shape = NA,
    fill=NA,
    #color="black",
    position= position_nudge(x=-.25)
  ) +
  
  geom_jitter(width = 0.12, height = 0.07, alpha=0.3)+
  
  
  geom_segment(aes( y=conf.low, yend=conf.high, 
                    x=as.numeric(factor(Playback_type,levels=c("F","C","M") )),
                    xend=as.numeric(factor(Playback_type, levels=c("F","C","M")))),
               color="black", size=1.1)+
  geom_point(aes( y=predicted, x=as.numeric(factor(Playback_type, levels=c("F","C","M")))),
             color="black", size=2.5)+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  labs(x = "Playback type", y= "Feeding visits")+
  theme_classic()+
  scale_x_discrete(labels=c("Female","Control", "Male"))+
  theme(legend.position = "none")+
  stat_n_text(y.pos = 13)

#export as a pdf, 6.35 x 2.55 (inc)





####TABLE #####

tab_model(mod_malehelp,file="audience_malehelp.doc", transform=NULL, show.icc = FALSE, show.zeroinf = FALSE, show.re.var=TRUE)





#post-hocs


emmeans(mod_malehelp, pairwise ~ Playback_type  |age_categorical, adjust = "bon")


ph<-as.data.frame(pairs(emmeans(mod_malehelp,  ~Playback_type  |age_categorical), adjust = "bonferroni"))









