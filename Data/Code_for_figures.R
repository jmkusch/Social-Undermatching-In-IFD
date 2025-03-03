
library(ggplot2)
library(ggpubr)

##############################Figure 3 generation:

visits<-read.csv("~/patch_visits_per_patch.csv")

visits$density <-as.factor(visits$density)
visits$sociality <-as.factor(visits$sociality)
visits$interference <-as.factor(visits$interference)
visits$rounded_patch<-as.numeric(visits$rounded_patch)

visits0.4<-visits[visits$density == 0.4,]
visits2.0<-visits[visits$density == 2,]
visits5.0<-visits[visits$density == 5,]

#to avoid error when calculating the log where there are 0's, the entire data set has a "1" added before the calculation
visits0.4$ymin<-log(abs((visits0.4$mean_visit+1)-visits0.4$se))
visits0.4$ymax<-log((visits0.4$mean_visit+1)+visits0.4$se)

visits2.0$ymin<-log(abs((visits2.0$mean_visit+1)-visits2.0$se))
visits2.0$ymax<-log((visits2.0$mean_visit+1)+visits2.0$se)

visits5.0$ymin<-log(abs((visits5.0$mean_visit+1)-visits5.0$se))
visits5.0$ymax<-log((visits5.0$mean_visit+1)+visits5.0$se)

g1<-ggplot(visits0.4[visits0.4$interference == 0.1,], aes(x=log(rounded_patch+1), y=log(mean_visit+1), colour = sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0, 8)+
  xlim(1,2.5)+
  labs(tag = (expression(paste(bold("0.4 Density"))))) +
  theme(plot.tag.position = c(0.2, 1.07), plot.tag = element_text(size = 11))+
  geom_abline(slope = 5.3, intercept = -5.3, linetype = 5, linewidth = 0.75)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Patch visits")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(1.3,0.1,0.1,0.5, 'cm'))+
  annotate(geom="text", x=1.1, y=7.9, label="A)",
           color="black", size = 5)


g2<-ggplot(visits0.4[visits0.4$interference == 0.5,], aes(x=log(rounded_patch+1), y=log(mean_visit+1), colour = sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0, 8)+
  xlim(1,2.5)+
  geom_abline(slope = 5.3, intercept = -5.3, linetype = 5, linewidth = 0.75)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Patch visits")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(1.3,0.1,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.1, y=7.9, label="B)",
           color="black", size = 5)

g3<-ggplot(visits0.4[visits0.4$interference == 1,], aes(x=log(rounded_patch+1), y=log(mean_visit+1), colour = sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0, 8)+
  xlim(1,2.5)+
  geom_abline(slope = 5.3, intercept = -5.3, linetype = 5, linewidth = 0.75)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Patch visits")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(1.3,0.1,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.1, y=7.9, label="C)",
           color="black", size = 5)

g4<-ggplot(visits2.0[visits2.0$interference == 0.1,], aes(x=log(rounded_patch+1), y=log(mean_visit+1), colour = sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0, 8)+
  xlim(1,2.5)+
  labs(tag = (expression(paste(bold("2.0 Density"))))) +
  theme(plot.tag.position = c(0.2, 1.07), plot.tag = element_text(size = 11))+
  geom_abline(slope = 5.3, intercept = -5.3, linetype = 5, linewidth = 0.75)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Patch visits")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.1,0.1,0.5, 'cm'))+
  annotate(geom="text", x=1.1, y=8, label="D)",
           color="black", size = 5)

g5<-ggplot(visits2.0[visits2.0$interference == 0.5,], aes(x=log(rounded_patch+1), y=log(mean_visit+1), colour = sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0, 8)+
  xlim(1,2.5)+
  geom_abline(slope = 5.3, intercept = -5.3, linetype = 5, linewidth = 0.75)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Patch visits")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.1,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.1, y=8, label="E)",
           color="black", size = 5)

g6<-ggplot(visits2.0[visits2.0$interference == 1,], aes(x=log(rounded_patch+1), y=log(mean_visit+1), colour = sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0, 8)+
  xlim(1,2.5)+
  geom_abline(slope = 5.3, intercept = -5.3, linetype = 5, linewidth = 0.75)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Patch visits")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.1,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.1, y=8, label="F)",
           color="black", size = 5)

g7<-ggplot(visits5.0[visits5.0$interference == 0.1,], aes(x=log(rounded_patch+1), y=log(mean_visit+1), colour = sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0, 8)+
  xlim(1,2.5)+
  labs(tag = (expression(paste(bold("5.0 Density"))))) +
  theme(plot.tag.position = c(0.2, 1.07), plot.tag = element_text(size = 11))+
  geom_abline(slope = 5.3, intercept = -5.3, linetype = 5, linewidth = 0.75)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Patch visits")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.1,0.1,0.5, 'cm'))+
  annotate(geom="text", x=1.1, y=8, label="G)",
           color="black", size = 5)

g8<-ggplot(visits5.0[visits5.0$interference == 0.5,], aes(x=log(rounded_patch+1), y=log(mean_visit+1), colour = sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0, 8)+
  xlim(1,2.5)+
  geom_abline(slope = 5.3, intercept = -5.3, linetype = 5, linewidth = 0.75)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Patch visits")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.1,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.1, y=8, label="H)",
           color="black", size = 5)

g9<-ggplot(visits5.0[visits5.0$interference == 1,], aes(x=log(rounded_patch+1), y=log(mean_visit+1), colour = sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="Attraction", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0, 8)+
  xlim(1,2.5)+
  geom_abline(slope = 5.3, intercept = -5.3, linetype = 5, linewidth = 0.75)+
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.4))+
  labs(x = "Patch food count", y = "Patch visits")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.1,0.1,0.1, 'cm'),
        legend.key.size = unit(0.2, "cm"))+
  annotate(geom="text", x=1.1, y=8, label="I)",
           color="black", size = 5)


plot<-ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9, labels = c("0.1 Competition", "0.5 Competition", "1.0 Competition"),
                label.x = 0.00, label.y = 1.02,font.label=list(color = 'black', size = 11),
                nrow = 3, ncol = 3, common.legend = FALSE)
annotate_figure(plot,
                left = text_grob("Patch Visits", 
                                 color = "black", face = "bold", size = 14, 
                                 rot = 90),
                bottom = text_grob("Patch Value", 
                                   color = "black", size = 14, face = "bold"))

#############Figure 4 generation:


res_time<-read.csv("~/residence_time_per_patch.csv")

res_time$Density <-as.factor(res_time$Density)
res_time$Sociality <-as.factor(res_time$Sociality)
res_time$Interference <-as.factor(res_time$Interference)

res_time<-res_time[res_time$residence_time != 0,] #for main text, keep with zeros for supplement

res_time0.4<-res_time[res_time$Density == 0.4,]
res_time2.0<-res_time[res_time$Density == 2,]
res_time5.0<-res_time[res_time$Density == 5,]

res_time0.4$ymin<-(abs(res_time0.4$residence_time-res_time0.4$se))
res_time0.4$ymax<-(res_time0.4$residence_time+res_time0.4$se)

res_time2.0$ymin<-(abs(res_time2.0$residence_time-res_time2.0$se))
res_time2.0$ymax<-(res_time2.0$residence_time+res_time2.0$se)

res_time5.0$ymin<-(abs(res_time5.0$residence_time-res_time5.0$se))
res_time5.0$ymax<-(res_time5.0$residence_time+res_time5.0$se)


p1<-ggplot(res_time0.4[res_time0.4$Interference == 0.1,], aes(x=(patch_value), y=residence_time, colour = Sociality))+
  geom_point(size = 0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0.8, 2.1)+
  labs(tag = (expression(paste(bold("0.4 Density"))))) +
  theme(plot.tag.position = c(0.2, 1.07), plot.tag = element_text(size = 11))+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Residence Time")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(1.3,0.4,0.1,0.5, 'cm'))+
  annotate(geom="text", x=1.3, y=2.1, label="A)",
           color="black", size = 4)

p2<-ggplot(res_time0.4[res_time0.4$Interference == 0.5,], aes(x=(patch_value), y=residence_time, colour = Sociality))+
  geom_point(size=0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0.8, 2.1)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Residence Time")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(1.3,0.4,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.3, y=2.1, label="B)",
           color="black", size = 4)

p3<-ggplot(res_time0.4[res_time0.4$Interference == 1,], aes(x=(patch_value), y=residence_time, colour = Sociality))+
  geom_point(size = 0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0.8, 2.1)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Residence Time")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(1.3,0.4,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.3, y=2.1, label="C)",
           color="black", size = 4)

p4<-ggplot(res_time2.0[res_time2.0$Interference == 0.1,], aes(x=patch_value, y=residence_time, colour = Sociality))+
  geom_point(size = 0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0.8, 2.1)+
  labs(tag = (expression(paste(bold("2.0 Density"))))) +
  theme(plot.tag.position = c(0.2, 1.07), plot.tag = element_text(size = 11))+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Residence Time")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.4,0.1,0.5, 'cm'))+
  annotate(geom="text", x=1.3, y=2.1, label="D)",
           color="black", size = 4)

p5<-ggplot(res_time2.0[res_time2.0$Interference == 0.5,], aes(x=patch_value, y=residence_time, colour = Sociality))+
  geom_point(size = 0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0.8, 2.1)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Residence Time")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.4,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.3, y=2.1, label="E)",
           color="black", size = 4)

p6<-ggplot(res_time2.0[res_time2.0$Interference == 1,], aes(x=patch_value, y=residence_time, colour = Sociality))+
  geom_point(size = 0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0.8, 2.1)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Residence Time")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.4,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.3, y=2.1, label="F)",
           color="black", size = 4)

p7<-ggplot(res_time5.0[res_time5.0$Interference == 0.1,], aes(x=patch_value, y=residence_time, colour = Sociality))+
  geom_point(size = 0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0.8, 2.1)+
  labs(tag = (expression(paste(bold("5.0 Density"))))) +
  theme(plot.tag.position = c(0.2, 1.07), plot.tag = element_text(size = 11))+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Residence Time")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.4,0.1,0.5, 'cm'))+
  annotate(geom="text", x=1.3, y=2.1, label="G)",
           color="black", size = 4)

p8<-ggplot(res_time5.0[res_time5.0$Interference == 0.5,], aes(x=patch_value, y=residence_time, colour = Sociality))+
  geom_point(size = 0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0.8, 2.1)+
  theme(legend.position = "none")+
  labs(x = "Patch food count", y = "Residence Time")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.4,0.1,0.1, 'cm'))+
  annotate(geom="text", x=1.3, y=2.1, label="H)",
           color="black", size = 4)

p9<-ggplot(res_time5.0[res_time5.0$Interference == 1,], aes(x=patch_value, y=residence_time, colour = Sociality))+
  geom_point(size = 0.5)+
  geom_errorbar(aes(ymin = ymin, ymax=ymax), alpha = 0.7)+
  scale_colour_manual(name="Attraction", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  ylim(0.8, 2.1)+
  theme(legend.position = "inside", legend.position.inside = c(0.85, 0.91))+
  labs(x = "Patch food count", y = "Residence Time")+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.4,0.1,0.1, 'cm'),
        legend.key.size = unit(0.2, "cm"))+
  annotate(geom="text", x=1.3, y=2.1, label="I)",
           color="black", size = 4)

plot<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, labels = c("0.1 Competition", "0.5 Competition", "1.0 Competition"),
                label.x = 0.00, label.y = 1.02,font.label=list(color = 'black', size = 11),
                nrow = 3, ncol = 3, common.legend = FALSE)
annotate_figure(plot,
                left = text_grob("Residence Time", 
                                 color = "black", face = "bold", size = 14, 
                                 rot = 90),
                bottom = text_grob("Patch Value", 
                                   color = "black", size = 14, face = "bold"))



################################Figure 5 generation:

mean_rtc<-read.csv("~/mean_residence_time_and_consumption_rate.csv")

plot1<-ggplot(mean_rtc[mean_rtc$Density == 0.4,], aes(x=Interference , y=(residence_time), colour = Sociality))+
  geom_boxplot()+
  scale_colour_manual(name="Sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  theme(legend.position="none")+
  labs(x = "Competition", y = "Residence Time")+
  ylim(1, 1.3)+
  annotate(geom="text", x=0.6, y=1.3, label="A)",
           color="black", size = 5)+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.1,0.1,0.1, 'cm'))

plot2<-ggplot(mean_rtc[mean_rtc$Density == 2,], aes(x=Interference , y=(residence_time), colour = Sociality))+
  geom_boxplot()+
  scale_colour_manual(name="Sociality", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "Competition", y = "Residence Time")+
  ylim(1, 1.3)+
  annotate(geom="text", x=0.6, y=1.3, label="B)",
           color="black", size = 5)+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.1,0.1,0.1, 'cm'))

plot3<-ggplot(mean_rtc[mean_rtc$Density == 5,], aes(x=Interference , y=(residence_time), colour = Sociality))+
  geom_boxplot()+
  scale_colour_manual(name="Attraction", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  theme(legend.position = "inside", legend.position.inside = c(0.8, 0.85))+
  labs(x = "Competition", y = "Residence Time")+
  ylim(1, 1.3)+
  annotate(geom="text", x=0.6, y=1.3, label="C)",
           color="black", size = 5)+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.8,0.1,0.1,0.1, 'cm'),
        legend.key.size = unit(0.2, "cm"))

plot4<-ggplot(mean_rtc[mean_rtc$Density == 0.4,], aes(x=Interference, y=(mean_expected_consumption_rate), colour = Sociality))+
  geom_boxplot()+
  scale_colour_manual(name="Attraction", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "Competition", y = "Consumption Rate")+
  ylim(0.65, 0.95)+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.4,0.1,0.1,0.1, 'cm'))+
  annotate(geom="text", x=0.6, y=0.95, label="D)",
           color="black", size = 5)

plot5<-ggplot(mean_rtc[mean_rtc$Density == 2,], aes(x=Interference, y=(mean_expected_consumption_rate), colour = Sociality))+
  geom_boxplot()+
  scale_colour_manual(name="Attraction", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "Competition", y = "Consumption Rate")+
  ylim(0.65, 0.95)+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.4,0.1,0.1,0.1, 'cm'))+
  annotate(geom="text", x=0.6, y=0.95, label="E)",
           color="black", size = 5)

plot6<-ggplot(mean_rtc[mean_rtc$Density == 5,], aes(x=Interference, y=(mean_expected_consumption_rate), colour = Sociality))+
  geom_boxplot()+
  scale_colour_manual(name="Attraction", 
                      values = c("0"="lightsalmon", "0.5"="royalblue4", "1" = "brown"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "Competition", y = "Consumption Rate")+
  ylim(0.65, 0.95)+
  theme(axis.text.x=element_text(colour="black",size=14), axis.text.y=element_text(colour="black",size=14),
        axis.title=element_blank(), plot.margin = margin(0.4,0.1,0.1,0.1, 'cm'))+
  annotate(geom="text", x=0.6, y=0.95, label="F)",
           color="black", size = 5)


plotConsumptionRate<-ggarrange(plot1,plot2,plot3,plot4,plot5,plot6, 
                               labels = c("    0.4 Density", "    2.0 Density", "    5.0 Density"),
                               label.x = c(0.1), label.y = 1.01,
                               nrow = 2, ncol = 3, common.legend = FALSE)
annotate_figure(plotConsumptionRate,
                bottom = text_grob("Competition", 
                                   color = "black", size = 14, face = "bold"))



