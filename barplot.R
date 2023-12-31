setwd('C:\\Users\\22927\\Desktop\\RNN-Neo')
windowsFonts()

data <- read.csv('./result/result_num.csv') %>% arrange(dataset)

for(i in 1:4){
  a=apply(data[(6*i-5):(6*i),1:3],1, function(x) x - data[(6*i-5),1:3])
  data[(6*i-5):(6*i),1:3]=reduce(a, function(x, y) rbind(x,y))
}


data <- data %>% 
  gather(key = "topn", value = "num",
         - dataset, - method)
data$topn <- factor(data$topn,levels = c("Top20", "Top50", "Top100"))
data$method <- factor(data$method,levels = c("ML_voting", "RNN_MP", "RNN_NP","RNN_MB","RNN_NB","RNN_voting"))



color_list <-c("#89C190","#ddf0f8","#b6d7ec","#8fbee0","#68a4d4","#418bc8")
##
color_list <- rep(c("#ddf0f8","#b6d7ec","#8fbee0","#68a4d4","#418bc8"),9)

data$group <- paste0(data$method,'_',data$topn,'_',data$dataset)

data <- data %>% dplyr::filter(method!='ML_voting') %>% dplyr::filter(dataset!='TOTAL')

data$dataset <- factor(data$dataset,levels=c('NCI-test','HiTIDE','TESLA'))
data$group <- factor(data$group,levels = c(
  'RNN_MP_Top20_HiTIDE','RNN_NP_Top20_HiTIDE','RNN_MB_Top20_HiTIDE','RNN_NB_Top20_HiTIDE','RNN_voting_Top20_HiTIDE',
  'RNN_MP_Top20_NCI-test','RNN_NP_Top20_NCI-test','RNN_MB_Top20_NCI-test','RNN_NB_Top20_NCI-test','RNN_voting_Top20_NCI-test',
  'RNN_MP_Top20_TESLA','RNN_NP_Top20_TESLA','RNN_MB_Top20_TESLA','RNN_NB_Top20_TESLA','RNN_voting_Top20_TESLA',
  'RNN_MP_Top50_HiTIDE','RNN_NP_Top50_HiTIDE','RNN_MB_Top50_HiTIDE','RNN_NB_Top50_HiTIDE','RNN_voting_Top50_HiTIDE',
  'RNN_MP_Top50_NCI-test','RNN_NP_Top50_NCI-test','RNN_MB_Top50_NCI-test','RNN_NB_Top50_NCI-test','RNN_voting_Top50_NCI-test',
  'RNN_MP_Top50_TESLA','RNN_NP_Top50_TESLA','RNN_MB_Top50_TESLA','RNN_NB_Top50_TESLA','RNN_voting_Top50_TESLA',
  'RNN_MP_Top100_HiTIDE','RNN_NP_Top100_HiTIDE','RNN_MB_Top100_HiTIDE','RNN_NB_Top100_HiTIDE','RNN_voting_Top100_HiTIDE',
  'RNN_MP_Top100_NCI-test','RNN_NP_Top100_NCI-test','RNN_MB_Top100_NCI-test','RNN_NB_Top100_NCI-test','RNN_voting_Top100_NCI-test',
  'RNN_MP_Top100_TESLA','RNN_NP_Top100_TESLA','RNN_MB_Top100_TESLA','RNN_NB_Top100_TESLA','RNN_voting_Top100_TESLA'
))



p <- ggplot(data=data, mapping=aes(x=dataset, y=num, fill=group))+
  geom_bar(stat="identity",position=position_dodge(0.85),width=0.75,color="black")+ 
  geom_text(aes(label=num),size=4,vjust=-0.15,position=position_dodge(0.85))+
  theme_classic()+
  scale_y_continuous(expand = c(0, 0),limits= c(-5,17))+
  theme(axis.ticks.x=element_blank(),axis.text.x = element_blank(),
        text = element_text(size = 15,colour = 'black' ,family = "sans"),
        axis.text = element_text(colour = 'black'),
        # axis.text.x = element_text(angle = 45,vjust = 0.60),
        plot.title = element_text(face="bold",hjust=0.5,size = 17),
        axis.ticks=element_line(color="black",linewidth=0.8),
        # panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
        legend.position = "none")+
  ggtitle('')+labs(fill = "")+ylab('NIN')+xlab("")+
  geom_hline(yintercept = 0)+
  scale_fill_manual(values =color_list)
  
p
ggsave(paste0('./fig/bar.png') ,p,dpi=1200,width = 4.1*3, height = 3.8)

































