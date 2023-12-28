setwd('the_path_to/RNN-Neo')
windowsFonts()

color_list <-c("#89C190","#ddf0f8","#b6d7ec","#8fbee0","#68a4d4","#418bc8")
print(color_list)

data <- read.csv('./result/result_FR_TTIF_AUPRC_R-PoD.csv') 
data$method <- factor(data$method,levels = c("ML_voting", "RNN_MP", "RNN_NP","RNN_MB","RNN_NB","RNN_voting"))


total <- data
total$dataset='TOTAL'
data <- rbind(data,total)

data$dataset <- factor(data$dataset,levels = c("NCI-test", "HiTIDE","TESLA","TOTAL"))

data2 <- data  %>% select(FR100,dataset,method)


p <- ggplot(data2,aes(x=dataset,y=FR100,fill=method))+
  geom_boxplot(width=0.6,alpha=1,lwd= 0.6,colour = 'black',outlier.size = 0.8,position = position_dodge(width=0.85) )+
  stat_summary(fun="mean", geom="point", shape=23, size=1.5, 
               color="#C00000", fill="#C00000",alpha=0.9,
               aes(group=method),position=position_dodge(width=0.85)) +
  # geom_hline(yintercept = mean(data2 %>% filter(method=="ML_voting") %>% .$FR100), colour = "#968C8C", linetype=2,linewidth=0.5)+
  theme_bw() + theme(panel.grid=element_blank())+
  theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
        axis.text = element_text(colour = 'black'),
        plot.title = element_text(face="bold",hjust=0.5,size = 17),
        axis.ticks=element_line(color="black",linewidth=0.8),
        # axis.text.x = element_text(angle = 45,vjust = 0.68,hjust = 0.65),
        panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
        legend.position = "none" )+ggtitle('')+xlab('')+ylab('FR100')+
  scale_fill_manual(values =color_list)
p
ggsave(paste0('./fig/FR.png'),dpi=1200,p,width = 8, height = 3.8)



data2 <- data  %>% select(TTIF,dataset,method)


p <- ggplot(data2,aes(x=dataset,y=TTIF,fill=method))+
  geom_boxplot(width=0.6,alpha=1,lwd= 0.6,colour = 'black',outlier.size = 0.8,position = position_dodge(width=0.85) )+
  stat_summary(fun="mean", geom="point", shape=23, size=1.5, 
               color="#C00000", fill="#C00000",alpha=0.9,
               aes(group=method),position=position_dodge(width=0.85)) +
  theme_bw() + theme(panel.grid=element_blank())+
  theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
        axis.text = element_text(colour = 'black'),
        plot.title = element_text(face="bold",hjust=0.5,size = 17),
        axis.ticks=element_line(color="black",linewidth=0.8),
        # axis.text.x = element_text(angle = 45,vjust = 0.68,hjust = 0.65),
        panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
        legend.position = "none" )+ggtitle('')+xlab('')+ylab('TTIF')+
  scale_fill_manual(values =color_list)
p
ggsave(paste0('./fig/TTIF.png'),dpi=1200,p,width = 8, height = 3.8)



data2 <- data  %>% select(AUPRC,dataset,method)


p <- ggplot(data2,aes(x=dataset,y=AUPRC,fill=method))+
  geom_boxplot(width=0.6,alpha=1,lwd= 0.6,colour = 'black',outlier.size = 0.8,position = position_dodge(width=0.85) )+
  stat_summary(fun="mean", geom="point", shape=23, size=1.5, 
               color="#C00000", fill="#C00000",alpha=0.9,
               aes(group=method),position=position_dodge(width=0.85)) +
  theme_bw() + theme(panel.grid=element_blank())+
  theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
        axis.text = element_text(colour = 'black'),
        plot.title = element_text(face="bold",hjust=0.5,size = 17),
        axis.ticks=element_line(color="black",linewidth=0.8),
        # axis.text.x = element_text(angle = 45,vjust = 0.68,hjust = 0.65),
        panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
        legend.position = "none" )+ggtitle('')+xlab('')+ylab('AUPRC')+
  scale_fill_manual(values =color_list)
p
ggsave(paste0('./fig/AUPRC.png'),dpi=1200,p,width = 8, height = 3.8)



data2 <- data  %>% select(R_PoD,dataset,method)


p <- ggplot(data2,aes(x=dataset,y=R_PoD,fill=method))+
  geom_boxplot(width=0.6,alpha=1,lwd= 0.6,colour = 'black',outlier.size = 0.8,position = position_dodge(width=0.85) )+
  stat_summary(fun="mean", geom="point", shape=23, size=1.5, 
               color="#C00000", fill="#C00000",alpha=0.9,
               aes(group=method),position=position_dodge(width=0.85)) +
  theme_bw() + theme(panel.grid=element_blank())+
  theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
        axis.text = element_text(colour = 'black'),
        plot.title = element_text(face="bold",hjust=0.5,size = 17),
        axis.ticks=element_line(color="black",linewidth=0.8),
        # axis.text.x = element_text(angle = 45,vjust = 0.68,hjust = 0.65),
        panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
        legend.position = "none" )+ggtitle('')+xlab('')+ylab('R-PoD')+
  scale_fill_manual(values =color_list)
p
ggsave(paste0('./fig/R_PoD.png'),dpi=1200,p,width = 8, height = 3.8)