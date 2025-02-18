#Step 1: Create a nl object:
library(nlrx)

#install.packages('unixtools', repos = 'http://www.rforge.net/')
# library(unixtools)
# unixtools::set.tempdir("~/new/tmp/path")

# NetLogo installation path:
netlogopath <- file.path("/home/master/Documents/projects/NetLogo-6.4.0-64") ##where NetLogo is located 
modelpath <- file.path("/home/master/Documents/projects/folmer_model/folmer_model_Fogo.nlogo") ##where model is located
outpath <- file.path("/home/master/out") ##where you want to files to end up

#Step 2: Attach an experiment
#In this example, we want to calculate sensitivity for 4 outputs 
#(consumption rate, residence time, patch visits, final patch food count ).
#We vary all numeric model parameters to estimate their sensitivity on the 4 defined output metrics.
#Thus, we define parameter ranges and distribution functions for all our numeric model parameters.
#We set the runtime of the model to 8760 ticks and measure our metrics on each tick
#(tickmetrics = "true"). However, for calculation of sensitivity indices,
#we only want to consider the last 200 ticks. Thus, we set evalticks to seq(8560,8760).

nl <- nl(nlversion = "6.4.0",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 1024) #Java virtual machine memory capacity in megabytes

nl@experiment <- experiment(expname = "FogoSensitivityModel",
                            outpath = outpath,
                            repetition = 1,
                            tickmetrics = "true",
                            idsetup = "setup",
                            idgo = "relocate",
                            runtime = 8760,
                            evalticks = seq(8560,8760),
                            metrics = c("mean [current_expected_consumption_rate] of turtles",
                                        "mean [residence_time] of turtles",
                                        "mean [landed] of patches with [is_border = false]",
                                        "mean [true_amount_of_food] of patches with [is_border = false]"),
                            
                            variables = list("kernel_range" = list(min=3, max=7, qfun="qunif"),
                                             "theta" = list(min=0.5, max=1.5, qfun="qunif"),
                                             "attack_rate" = list(min=0.5, max=1.5, qfun="qunif"),
                                             "the_percentage" = list(min=14, max=29,  qfun="qunif")),
                            
                            constants = list("density" = 0.4, ##change out with 5.0 for high density trials
                                             "interference_rate" = 0.5,
                                             "intensity_of_conspecific_attraction" = 0.5))


#Step 3: Attach a simulation design
#We use the simdesign_morris() function to attach a Morris Sensitivity Analysis design.
#The morrislevels parameter sets the number of different values for each parameter (sampling density).
#The morrisr parameter sets the number of repeated samplings (sampling size). 
#The morrisgridjump parameter sets the number of levels that are increased/decreased for computing the elementary effects.
#Morris recommendation is to set this value to levels / 2.
#More information on the Morris specific parameters can be found in the description of the morris function in the sensitivity package (?morris).

nl@simdesign <- simdesign_morris(nl=nl,
                                 morristype="oat",
                                 morrislevels=24,
                                 morrisr=40,
                                 morrisgridjump=12,
                                 nseeds=1)

check<-nl@simdesign@siminput

# Check to see if variables are valid 
eval_variables_constants(nl)

#Step 4: Run simulations
#To execute the simulations, we can use the function run_nl_all().

results <- run_nl_all(nl)

#Step 5: Investigate output
#First, we need to attach the results to the nl object.

setsim(nl, "simoutput") <- results
saveRDS(nl, file.path(outpath, "morris.rds"))

#After results have been attached, we can use the analyze_nl() 
#function to calculate Morris sensitivity indices.
morris <- analyze_nl(nl)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
####Output into CSV####
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
write.csv(results,"~/Results0.4density.csv")
write.csv(morris,"~/Morris0.4Sensitivty.csv") ##change to /Morris5.0Sensitivity when running high density trial

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
####Plots####
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(ggrepel)


#NOTE IT WILL NOT WORK IF YOU HAVE PLYR LOADED!!!

#here we are averaging all the mustar and sigma values, group for each parameter.
output.data <- read.csv("~/Morris0.4Sensitivity.csv")%>%
 mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

plot0.4<-ggplot(output.data, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Sensitivity Analysis")+
  xlab("mustar") +
  ylab("sigma")+
#  ylim(0, 3000)+
  #  xlim(0, 1100)+
  #geom_text(aes(label=parameter), size=3, hjust=-0.1, vjust=0)+
  geom_text_repel(aes(label = parameter),
                  box.padding   = 0.35, 
                  point.padding = 0.15,
                  segment.color = 'grey50',
                  size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"),
        axis.text.y = element_text(size=12,  colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_rect(fill= "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1)) 

plot0.4

#here we are averaging all the mustar and sigma values, group for each parameter.
output.data <- read.csv("~/Desktop/PhD/Thesis/Chapter 2/Sensitivity Analysis/Morris5.0Sensitivity.csv")%>%
  mutate_if(is.logical, as.character)%>%
  group_by(parameter, index)%>%
  summarize(value = mean(value))%>%
  pivot_wider(names_from = index, values_from = value)

plot5.0<-ggplot(output.data, aes(x=mustar, y=sigma)) + 
  geom_point(shape=22, fill = "black", size=2)+
  ggtitle("Sensitivity Analysis")+
  xlab("mustar") +
  ylab("sigma")+
  #  ylim(0, 3000)+
  #  xlim(0, 1100)+
  #geom_text(aes(label=parameter), size=3, hjust=-0.1, vjust=0)+
  geom_text_repel(aes(label = parameter),
                  box.padding   = 0.35, 
                  point.padding = 0.15,
                  segment.color = 'grey50',
                  size = 3) +
  theme(axis.text.x = element_text(size=12,  colour = "black"),
        axis.text.y = element_text(size=12,  colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.background = element_rect(fill= "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, colour = "black", size = 1)) 

plot5.0
