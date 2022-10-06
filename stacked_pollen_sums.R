library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

spec_group<-read.csv("spec_group.csv")
spec_group<-spec_group[-c(5)]
spec_group[,1:5]<- spec_group[,1:5] / rowSums(spec_group[,1:5]) * 100

names(spec_group)[1]<-"Disturbed"
names(spec_group)[2]<-"Littoral"
names(spec_group)[3]<-"Mangrove"
names(spec_group)[4]<-"Rainforest"
names(spec_group)[5]<-"Wetland"

avai_groups<- spec_group[spec_group$Site == "Avaiovuna Swamp", ]
fine_groups<- spec_group[spec_group$Site == "Finemui Swamp", ]
loto_groups<- spec_group[spec_group$Site == "Lotofoa Swamp", ]
ngof_groups<- spec_group[spec_group$Site == "Ngofe Marsh", ]

#Avai'o'vuna
avai_long <- gather(avai_groups, Group, Percentage, "Disturbed":"Wetland", factor_key=TRUE)

avai_bar<-ggplot(avai_long, aes(fill=Group, y=Percentage, x=Cal_yrs_BP, width=100)) + 
  geom_area(position="stack", stat="identity") +
  ggtitle("B. Avai'o'vuna Swamp") +
  xlab("Cal years BP") +
  ylab("%") +
  scale_fill_manual(values = c("#ff6666","#669900","#00cc66","#00cccc", "#cc66ff")) +
  xlim(6000, 0) +
  coord_flip() +
  theme_bw() +
  geom_vline(xintercept=c(3949), linetype="dashed", 
             color = "black", size=1)
avai_bar

#Finemui
fine_long <- gather(fine_groups, Group, Percentage, "Disturbed":"Wetland", factor_key=TRUE)

fine_bar<-ggplot(fine_long, aes(fill=Group, y=Percentage, x=Cal_yrs_BP, width=100)) + 
  geom_area(position="stack", stat="identity") +
  ggtitle("D. Finemui Swamp") +
  xlab("Cal years BP") +
  ylab("%") +
  scale_fill_manual(values = c("#ff6666","#669900", "#00cc66","#00cccc", "#cc66ff")) +
  xlim(6000, 0) +
  coord_flip() +
  theme_bw() +
  geom_vline(xintercept=c(3456,5206), linetype="dashed", 
             color = "black", size=1)
fine_bar

#Lotofoa
loto_groups <-loto_groups[order(loto_groups$Cal_yrs_BP),]

#the year 3859BP appears twice because of a slump in the age-depth model, here I changed it by a year each way
loto_groups$Cal_yrs_BP<-c(166,469,776,1090,1402,1710,2020,2328,2633,3245,3553,3858,3860,4164,4473,4778,5086,5396,5701,6007,6311,6614,6920,7226)

loto_long <- gather(loto_groups, Group, Percentage, "Disturbed":"Wetland", factor_key=TRUE)

loto_bar<-ggplot(loto_long, aes(fill=Group, y=Percentage, x=Cal_yrs_BP, width=100)) + 
  geom_area(position="stack", stat="identity") +
  ggtitle("C. Lotofoa Swamp") +
  xlab("Cal years BP") +
  ylab("%") +
  scale_fill_manual(values = c("#ff6666","#669900","#00cc66", "#00cccc", "#cc66ff")) +
  xlim(6000, 0) +
  coord_flip() +
  theme_bw() +
  geom_vline(xintercept=3836, linetype="dashed", 
             color = "black", size=1)
loto_bar

#Ngofe
ngof_long <- gather(ngof_groups, Group, Percentage, "Disturbed":"Wetland", factor_key=TRUE)

ngof_bar<-ggplot(ngof_long, aes(fill=Group, y=Percentage, x=Cal_yrs_BP, width=100)) + 
  geom_area(position="stack", stat="identity") +
  ggtitle("A. Ngofe Marsh") +
  xlab("Cal years BP") +
  ylab("%") +
  scale_fill_manual(values = c("#ff6666","#669900","#00cc66","#00cccc", "#cc66ff")) +
  xlim(6000, 0) +
  coord_flip() +
  theme_bw() +
  geom_vline(xintercept=c(372,1365,3686,3993), linetype="dashed", 
             color = "black", size=1)
ngof_bar
#T1B removed 5854 cal years BP

plot_bar<-ggarrange(ngof_bar, avai_bar, loto_bar, fine_bar,
                    common.legend = TRUE, legend = "bottom")

annotate_figure(plot_bar, top = text_grob)

