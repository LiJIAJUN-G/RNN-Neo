setwd('C:/Users/22927/Desktop/RNN-Neo')
windowsFonts()

data <- read.csv('./result/result_pTuneos.csv')


data <- data%>% 
  gather(key = "FR", value = "FR_value",
         - method,- patient,- AUPRC,- TTIF, - R_PoD)
data$FR <- factor(data$FR,levels = c("FR20", "FR50", "FR100"))
data$method <- factor(data$method,levels = c("pTuneos","RNN_NB","RNN_voting"))


##
color_list <-c("#7BA79D","#68a4d4","#418bc8")


R_PoD_plot <- function(data){
  data2 <- data %>% select(R_PoD,method)
  p <- ggplot(data2,aes(x=method,y=R_PoD,fill=method))+
    stat_boxplot(geom = "errorbar",
                 width=0.13,lwd= 0.4)+
    geom_boxplot(width=0.5,alpha=1,lwd= 0.4,colour = 'black',outlier.size = 0.7)+
    stat_summary(fun="mean", geom="point", shape=23, size=1.5, color="#C00000", fill="#C00000",alpha=0.8) +
    geom_hline(yintercept = mean(data2 %>% filter(method=="pTuneos") %>% .$R_PoD), colour = "#968C8C", linetype=2,linewidth=0.5)+
    theme_bw() + theme(panel.grid=element_blank())+
    theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(face="bold",hjust=0.5,size = 17),
          axis.ticks=element_line(color="black",linewidth=0.8),
          axis.text.x = element_text(angle = 30,vjust = 0.68,hjust = 0.65),
          panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
          legend.position = "none" )+ggtitle('')+xlab('')+ylab(expression("R_PoD"[20]))+
    scale_fill_manual(values =color_list)
  
  
  ggsave('./fig/ptuneos_R_PoD.png',dpi=1200,p,width = 3.8, height = 5)
  p
}
R_PoD_plot(data)

TTIF_plot <- function(data){
  data2 <- data %>% select(TTIF,method)
  p <- ggplot(data2,aes(x=method,y=TTIF,fill=method))+
    stat_boxplot(geom = "errorbar",
                 width=0.13,lwd= 0.4)+
    geom_boxplot(width=0.5,alpha=1,lwd= 0.4,colour = 'black',outlier.size = 0.7)+
    stat_summary(fun="mean", geom="point", shape=23, size=1.5, color="#C00000", fill="#C00000",alpha=0.8) +
    geom_hline(yintercept = mean(data2 %>% filter(method=="pTuneos") %>% .$TTIF), colour = "#968C8C", linetype=2,linewidth=0.5)+
    theme_bw() + theme(panel.grid=element_blank())+
    theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(face="bold",hjust=0.5,size = 17),
          axis.ticks=element_line(color="black",linewidth=0.8),
          axis.text.x = element_text(angle = 30,vjust = 0.68,hjust = 0.65),
          panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
          legend.position = "none" )+ggtitle('')+xlab('')+ylab('TTIF')+
    scale_fill_manual(values =color_list)
  
  
  ggsave('./fig/ptuneos_TTIF.png',dpi=1200,p,width = 3.8, height = 5)
  p
}
TTIF_plot(data)

AUPRC_plot <- function(data){
  data2 <- data %>% select(AUPRC,method)
  p <- ggplot(data2,aes(x=method,y=AUPRC,fill=method))+
    stat_boxplot(geom = "errorbar",
                 width=0.13,lwd= 0.4)+
    geom_boxplot(width=0.5,alpha=1,lwd= 0.4,colour = 'black',outlier.size = 0.7)+
    stat_summary(fun="mean", geom="point", shape=23, size=1.5, color="#C00000", fill="#C00000",alpha=0.8) +
    geom_hline(yintercept = mean(data2 %>% filter(method=="pTuneos") %>% .$AUPRC), colour = "#968C8C", linetype=2,linewidth=0.5)+
    theme_bw() + theme(panel.grid=element_blank())+
    theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(face="bold",hjust=0.5,size = 17),
          axis.ticks=element_line(color="black",linewidth=0.8),
          axis.text.x = element_text(angle = 30,vjust = 0.68,hjust = 0.65),
          panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
          legend.position = "none" )+ggtitle('')+xlab('')+ylab('AUPRC')+
    scale_fill_manual(values =color_list)
  
  
  ggsave(paste0('./fig/pTuneos_AUPRC.png'),dpi=1200,p,width = 3.8, height = 5)
  p
}
AUPRC_plot(data)

FR100_plot <- function(data){
  data2 <- data %>% select(FR,method,FR_value) %>% filter(FR=='FR100')
  p <- ggplot(data2,aes(x=method,y=FR_value,fill=method))+
    stat_boxplot(geom = "errorbar",
                 width=0.13,lwd= 0.4)+
    geom_boxplot(width=0.5,alpha=1,lwd= 0.4,colour = 'black',outlier.size = 0.7)+
    stat_summary(fun="mean", geom="point", shape=23, size=1.5, color="#C00000", fill="#C00000",alpha=0.8) +
    geom_hline(yintercept = mean(data2 %>% filter(method=="pTuneos") %>% .$FR_value), colour = "#968C8C", linetype=2,linewidth=0.5)+
    theme_bw() + theme(panel.grid=element_blank())+
    theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(face="bold",hjust=0.5,size = 17),
          axis.ticks=element_line(color="black",linewidth=0.8),
          axis.text.x = element_text(angle = 30,vjust = 0.68,hjust = 0.65),
          panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
          legend.position = "none" )+ggtitle('')+xlab('')+ylab('FR100')+
    scale_fill_manual(values =color_list)
  
  
  ggsave(paste0('./fig/pTuneos_FR100.png'),dpi=1200,p,width = 3.8, height = 5)
  p
}
FR100_plot(data)

data <- read.csv('./data/num_ptuneos.csv') %>%   
  gather(key = "topn", value = "num",- dataset, - method)

data$topn <- factor(data$topn,levels = c("Top20", "Top50", "Top100"))
data$method <- factor(data$method,levels = c("pTuneos","RNN_NB","RNN_voting"))



p <- ggplot(data=data, mapping=aes(x = topn, y = num, fill= method))+
  geom_bar(stat="identity",position=position_dodge(0.85),width=0.75,color="black")+ 
  geom_text(aes(label=num),size=4,vjust=-0.5,position=position_dodge(0.85))+
  theme_bw() + theme(panel.grid=element_blank())+
  scale_y_continuous(expand = c(0, 0),limits= c(0,44))+xlab("")+ 
  theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
        axis.text = element_text(colour = 'black'),
        # axis.text.x = element_text(angle = 45,vjust = 0.60),
        plot.title = element_text(face="bold",hjust=0.5,size = 17),
        axis.ticks=element_line(color="black",linewidth=0.8),
        panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
        legend.position = "none" )+
  ggtitle("")+labs(fill = "")+ylab('NIN')+
  scale_fill_manual(values =color_list)
p
ggsave('./fig/pTuneos_bar.png',dpi=1200,p,width = 4.1, height = 3.8)
































