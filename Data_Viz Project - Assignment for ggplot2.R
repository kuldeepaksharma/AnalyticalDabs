library(ggplot2)
library(ggrepel)
library(ggthemes)

pl <- ggplot(df,aes(x=CPI,y=HDI))

pl2 <- pl + geom_point(aes(color=Region),shape=1) + theme(legend.position 
                                                          = 'top',legend.box = 'horizontal', 
                                                          axis.title.x = element_text(size = 8,face = 'bold.italic'),
                                                          axis.title.y = element_text(size = 8,face = 'bold.italic')) 
pl3 <- pl2 + geom_smooth(aes(group=1),method = 'lm', formula = 'y ~ log(x)',color='red',se =F)

pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan","Afghanistan", 
                   "Congo", "Greece", "Argentina", "Brazil","India", "Italy", "China", 
                   "South Africa", "Spain","Botswana", "Cape Verde", "Bhutan", "Rwanda", 
                   "France","US", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl4 <- pl3 + geom_text_repel(aes(label=Country),
                       data=subset(df,Country %in% pointsToLabel),color='gray20',size = 3)
pl5 <- pl4 + scale_x_continuous(name = 'Corruption Perceptions Index, 2011 (10=least corrupt)',
                                limits = c(1,10),breaks = 1:10) + scale_y_continuous(name = 'Human Development Index, 2011 (1=best)',
                                 limits = c(0.2,1)) + ggtitle('Corruption and Human Development')
print(pl5)

