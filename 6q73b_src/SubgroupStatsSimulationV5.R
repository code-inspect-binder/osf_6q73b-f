#-------------------------------------------------------------------------------
#Title: Simulating subgroup analyses
#Author: Annaliese Beery
#Date: March 2018
#Purpose: Imagine multiple scenarios where you test one or both sexes 
#         When do you lose power, and when don't you (using factorial analyses)?
#-------------------------------------------------------------------------------

#Libraries
library("ggplot2") #needed for pretty graphs
#-------------------------------------------------------------------------------

iterations = 10000  #Use lower # when troubleshooting
#Simulation parameters:  (See excel sheet for more readable version, more info below)
  #   CV (SD/mean) held constant at 50
  #   *Simulation 1: no sex diff, med trx diff (5%)
  #   Simulation 2: treatment effect in females but not males
  #   Simulation 3b: both sexes go up, but only sig in one sex, interaction effect
  #   *Simulation 4: interaction of sex and trx, 5% each up and down
  #   *Simulation 5: (called scenario 2) big sex diff, med trx diff.
  #   * means simulation results included in manuscript figure

#   Parameters are in lists indexed by simulation number: parameter =c(sim1, sim2,...)
TrxA_fem_mean=c(50,50,50,50,50)  #these numbers are stored as doubles by default
TrxA_fem_sd=c(1,2.5,2.5,2.5,1)
TrxA_male_mean=c(50, 51, 50, 52.5, 60)   
TrxA_male_sd=c(1, 2.55, 2.55, 2.625, 1.2) 
TrxB_fem_mean=c(51, 52.5, 52.5, 52.5, 51) 
TrxB_fem_sd=c(1.02, 2.625, 2.625, 2.625, 1.02)
TrxB_male_mean=c(51, 51, 52.02, 50, 61.2)   
TrxB_male_sd=c(1.02, 2.55, 2.601, 2.5, 1.224)  

sim=1  # set this to 1, 2, 3, 4, or 5 and save output from each time (or write a loop)
#note -- simulation #s here don't match manuscript -- space for only a subset and reordered
#-------------------------------------------------------------------------------

#Step one: generate fake data based on specific distributions.  

#Make array "results" to hold p-value results of stat tests from each run (later convert to dataframe)
results=matrix(data=NA, nrow=iterations,ncol=7)
colnames(results)=c("Iteration", "t-test_12F", "t-test_12M", "v2wANOVA_6m6f", "ut-test_6m6f", "x", "z_aov_interaction") 
  #v and u in front of column names are to control ordering in the graph (it's alphabetical by default). Fix labels later.

#Run many simulations: generate distributions, perform stat tests, keep stat results only
for (i in 1:iterations){  
#i=1  #use for troubleshooting only
  #Note: rnorm generates random numbers from a normal distribution
  #      syntax is rnorm(n, mean, sd)
  #      so rnorm(12, 3, .25) generates 12 numbers from a normal dist. with mean 3 and sd=.25

  Afemales = rnorm(12, TrxA_fem_mean[sim], TrxA_fem_sd[sim])
  Amales = rnorm(12, TrxA_male_mean[sim], TrxA_male_sd[sim])
  Bfemales = rnorm(12, TrxB_fem_mean[sim], TrxB_fem_sd[sim])
  Bmales = rnorm(12,TrxB_male_mean[sim], TrxB_male_sd[sim]) #something weird happens when change these to 63 and 1.26!

  #t-tests
  p_12f = t.test(Afemales, Bfemales)$p.value #by group, using 12 of one sex
  p_12m = t.test(Amales, Bmales)$p.value #using 12 of other sex

  #make a temporary dataframe with first 6 of the females, first 6 of the males for subsequent tests
  #alternate approach would be to make them as new distributions.  Didn't do that.
  sex=c('F','F','F','F','F','F','F','F','F','F','F','F','M','M','M','M','M','M','M','M','M','M','M','M','F','F','F','F','F','F','F','F','F','F','F','F','M','M','M','M','M','M','M','M','M','M','M','M')
  group=c('A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','A','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B','B')
  subset1=c(1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0)
  subset2=c(0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1)
  measure=c(Afemales,Amales,Bfemales,Bmales)
  simdata12each_df=data.frame(sex, group, measure, subset1, subset2)   #subset 2 not really needed in here 
  simdata6each_sub1=subset(simdata12each_df, subset1==1)
 
  #visually check data
  #qplot(x=sex, y=measure, data = simdata12each, geom = 'boxplot')
  #qplot(x=group, y=measure, data = simdata6each_sub1, geom = 'boxplot')
  
  #2-way anova on subset 1 (n=6/sex/group)
  aov_sub1 = aov(measure ~ sex * group, simdata6each_sub1)  # * gives you main effects and interaction
  # summary(aov_sub1)  # shows pieces of output and order of p-values
  p_aov_group_6m6f = summary(aov_sub1)[[1]][, 'Pr(>F)'][2] #this picks out the second of the p-values, i.e. group
  p_aov_int_6m6f = summary(aov_sub1)[[1]][, 'Pr(>F)'][3] #this picks out 3rd p-value, interaction effect

  # t-test on subset 1 (n=6/sex/group)
  p_6m6f = t.test(measure ~ group, data=simdata6each_sub1)$p.value #using 6 each sex, t-tests
  
  #store group effects in results matrix ("Iteration", "p12F", "p12M", "pANOVA_all", "pANOVA_sub1", "pANOVA_sub2")
  results[i,1]= i                 #iteration
  results[i,2]= p_12f             #t-test p-val for 12 f, trx A vs. B, results are stored as a double
  results[i,3]= p_12m             #t-test p-val for 12 m, trx A vs. B
  results[i,4]= p_aov_group_6m6f  #2-way anova p-val for treatment (called "group")
  results[i,5]= p_6m6f            #t-test p-val for 6f,6m trx A vs B
  results[i,7]= p_aov_int_6m6f    #interaction effect from 2-way anova
}

#Note: Results matrix interpretation: (first row is iteration number)
# 1) t-test p-value of 12 group A females vs. 12 group B females
# 2) t-test p-value of 12 group A males vs. 12 group B males
# 4) 2-way anova by sex and group, p-value for group with 6m,6f per group, subset 1
# 5) t-test by group, p-value for group with 6m,6f per group, should look worst
# 6) blank, to make space
# 7) interaction effect
#note, re-ordered in graph by alphabet of label!

#version to put in one graph
results_df=as.data.frame(results) #make it a data frame for easier manipulation
results_gathered_df <- tidyr::gather(data=results_df, key="Statistic", value="p_value", 2:7)

#version to make 2 separate graphs -- cut because 2nd graph had different x scale.
#new approach: put a blank spacer in and add to main graph, then cover with white bar
  #results_df=as.data.frame(results) #make it a data frame for easier manipulation
  #interactions_df=data.frame(results_df$Iteration,results_df$aov_interaction)  # Just the interactions part
  #results_df$aov_interaction<- NULL  #remove aov interaction column from results data frame for main graph
  #results_gathered_df <- tidyr::gather(data=results_df, key="Statistic", value="p_value", 2:7)
  #interactions_gathered_df <- tidyr::gather(data=interactions_df, key="Statistic", value="p_value", 2)

#-------------------------------------------------------------------------------
# Graphing results
#-------------------------------------------------------------------------------

#GRAPH 1: Distribution of p-values for each group (for whichever simulation just run)
#note, will give error about 10,000 or however many points removed because of spacer column "x", ignore!
group_colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "#C77CFF")

#qplot(x=Statistic, y=p_value, data=results_gathered_df, geom="violin", fill=Statistic)
ggplot(data=results_gathered_df, aes(x=Statistic, y=p_value, fill=Statistic)) +   
  geom_violin() + 
  xlab("simulation group") +
  ylab("distribution of p-values") +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0.0, 1.0)) +
  scale_fill_manual(values = group_colors)

# --------------------------------------------------------------
# PAUSE here in running to save output graph in desired format
# Otherwise assign to a variable for later saving because code below makes a new graph.

#uncomment to determine: what colors are those violins defaulting to, anyhow? Use above to force re-use.
#p <- ggplot(results_gathered_df, aes(x=Statistic, y=p_value, fill=Statistic)) +
#  geom_point(shape = 21, size = 4)
#ggplot_build(p)$data  
#ANSWER: group 1: F8766D, 2 is: 7CAE00, 3: #00BFC4, 4: C77CFF, 

#GRAPH 2: Input data that yielded a particular distribution of p-values
#Note, plot actual mean and SD...don't need to hold on to the simulated data
#sim=5  # can set this to any of the simulations, or leave as above

#Make a dataframe of the Group, Mean, Lower, and Upper bounds
Group = c("Female Trx A", "Male Trx A", "Female Trx B", "Male Trx B")
Mean = c(TrxA_fem_mean[sim], TrxA_male_mean[sim], TrxB_fem_mean[sim], TrxB_male_mean[sim])
Lower = c(TrxA_fem_mean[sim]-TrxA_fem_sd[sim], TrxA_male_mean[sim]-TrxA_male_sd[sim], TrxB_fem_mean[sim]-TrxB_fem_sd[sim], TrxB_male_mean[sim]-TrxB_male_sd[sim])
Upper = c(TrxA_fem_mean[sim]+TrxA_fem_sd[sim], TrxA_male_mean[sim]+TrxA_male_sd[sim], TrxB_fem_mean[sim]+TrxB_fem_sd[sim], TrxB_male_mean[sim]+TrxB_male_sd[sim])
sim_input_df = data.frame(Group, Mean, Lower, Upper)

ggplot() + 
  geom_errorbar(data=sim_input_df, mapping=aes(x=Group, ymin=Lower, ymax=Upper), width=0.2, size=.5, color="black") + 
  geom_point(data=sim_input_df, mapping=aes(x=Group, y=Mean), size=2, shape=16) +
  scale_y_continuous(limits = c(47, 54)) +
  theme(legend.position="none") +
  ylab("Mean +/- SD") 
#  xlab("") #ditch x-axis "group" label. Comment out if making graphs same size as before


