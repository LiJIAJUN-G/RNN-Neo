setwd('the_path_to/RNN-Neo')
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
color_list <-c("#ddf0f8","#b6d7ec","#8fbee0","#68a4d4","#418bc8")

bar_function <- function(data,dataset_){
  data <- data %>% dplyr::filter(dataset==dataset_) %>% dplyr::filter(method!='ML_voting')
  p <- ggplot(data=data, mapping=aes(x = topn, y = num, fill= method))+
    geom_bar(stat="identity",position=position_dodge(0.85),width=0.75)+ 
    theme_bw() + theme(panel.grid=element_blank())+
    scale_y_continuous(expand = c(0, 0),limits= c(-17,17))+
    theme(text = element_text(size = 15,colour = 'black' ,family = "sans"),
          axis.text = element_text(colour = 'black'),
          # axis.text.x = element_text(angle = 45,vjust = 0.60),
          plot.title = element_text(face="bold",hjust=0.5,size = 17),
          axis.ticks=element_line(color="black",linewidth=0.8),
          panel.border = element_rect(fill=NA,color="black", linewidth=1, linetype="solid"),
          legend.position = "none" )+
    ggtitle(dataset_)+labs(fill = "")+ylab('NIN')+xlab("")+ 
    scale_fill_manual(values =color_list)
  p
  ggsave(paste0('./fig/bar_',dataset_,'.png') ,p,dpi=1200,width = 4.1, height = 3.8)
}

bar_function(data,'NCI-test')
bar_function(data,'HiTIDE')
bar_function(data,'TESLA')
bar_function(data,'TOTAL')
###NCI







