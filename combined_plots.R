INT_DATA_denmark<- read.csv("~/Documents/Projects/SEFASI/outputs/INT_DATA_denmark.csv")
INT_DATA_denmark$country <- "denmark"
INT_DATA_england<- read.csv("~/Documents/Projects/SEFASI/outputs/INT_DATA_england.csv")
INT_DATA_england$country <- "england"
INT_DATA_senegal<- read.csv("~/Documents/Projects/SEFASI/outputs/INT_DATA_senegal.csv")
INT_DATA_senegal$country <- "senegal"

INT_DATA<- rbind(INT_DATA_denmark,INT_DATA_england,INT_DATA_senegal)

INT_DATA_abs_denmark<- read.csv("~/Documents/Projects/SEFASI/outputs/INT_DATA_abs_denmark.csv")
INT_DATA_abs_denmark$country <- "denmark"
INT_DATA_abs_england<- read.csv("~/Documents/Projects/SEFASI/outputs/INT_DATA_abs_england.csv")
INT_DATA_abs_england$country <- "england"
INT_DATA_abs_senegal<- read.csv("~/Documents/Projects/SEFASI/outputs/INT_DATA_abs_senegal.csv")
INT_DATA_abs_senegal$country <- "senegal"

INT_DATA_abs<- rbind(INT_DATA_abs_denmark,INT_DATA_abs_england,INT_DATA_abs_senegal)

  int_levels = c('HU0',
                 'AU0',
                 'EU0',
                 'HH0',
                 'AA0',
                 'HE0',
                 'AH0',
                 'EH0',
                 'HA0',
                 'EA0',
                 'AE0',
                 'EE0',
                 'HH90',
                 'HH75_HU75',
                 'HU80_AU70_ALLT80',
                 'HU80_AU70_ALLT70',
                 'EH50_EA50',
                 'HA50_AH50',
                 'HU50_AU50_EU50'
  )
P_1 <-   ggplot(INT_DATA)+ geom_boxplot(aes(x=factor(variable,levels = int_levels),y=value*100,fill=factor(variable, levels = int_levels)))+
  ylab(label="% Reduction in resistant bacteria in humans over 20 years")+
  xlab(label="Intervention")+ 
  scale_fill_manual(labels = c('H_U=0',
                               'A_U=0',
                               'E_U=0',
                               'HH=0',
                               'AA=0',
                               'HE=0',
                               'AH=0',
                               'EH=0',
                               'HA=0',
                               'EA=0',
                               'AE=0',
                               'EE=0',
                               'HH=90',
                               'HH=75 and H_U=75',
                               'H_U=80, A_U=70, allT=80',
                               'H_U=80, A_U=70, allT=70',
                               'EH=50 and EA=50',
                               'HA=50 and AH=50',
                               'H_U=50, A_U=50, E_U=50'
  ),values=cols)+
  # 'no decay of AB-resistance in humans',
  # 'no decay of AB-resistance in animals',
  # 'no decay of AB-resistance in environment'))+
  theme_classic() +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
  theme(axis.text.x = element_text(color="black", 
                                   size=15),
        axis.text.y = element_text(color="black", 
                                   size=15)) + theme(legend.title = element_blank()) +
  #scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
  scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
  
  theme(legend.position = 'bottom')+guides(fill=guide_legend(ncol=3))+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  facet_wrap(~country, scales="free")
ggsave(paste("~/Documents/Projects/SEFASI/outputs/combined_ints", ".png",sep=""), width = 400, height = 200, units='mm',dpi=1000)

P2 <- ggplot(INT_DATA)+ geom_boxplot(aes(x=country,y=value*100,fill=country))+
  ylab(label="% Reduction in resistant bacteria in humans over 20 years")+
  xlab(label="Intervention")+ 
  scale_fill_manual(labels = c('DENMARK',
                               'ENGLAND',
                               'SENEGAL'
                               
  ),values=cols)+
  # 'no decay of AB-resistance in humans',
  # 'no decay of AB-resistance in animals',
  # 'no decay of AB-resistance in environment'))+
  theme_classic() +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
  theme(axis.text.x = element_text(color="black", 
                                   size=15),
        axis.text.y = element_text(color="black", 
                                   size=15)) + theme(legend.title = element_blank()) +
  #scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
  scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
  
  theme(legend.position = 'bottom')+guides(fill=guide_legend(ncol=3))+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  facet_wrap(~factor(variable,levels = int_levels), scales="free")
ggsave(paste("~/Documents/Projects/SEFASI/outputs/combined_countries", ".png",sep=""), width = 400, height = 300, units='mm',dpi=1000)


P_1_abs <-   ggplot(INT_DATA_abs)+ geom_boxplot(aes(x=factor(variable,levels = int_levels),y=value*100,fill=factor(variable, levels = int_levels)))+
  ylab(label="Absolute reduction in resistant bacteria in humans over 20 years")+
  xlab(label="Intervention")+ 
  scale_fill_manual(labels = c('H_U=0',
                               'A_U=0',
                               'E_U=0',
                               'HH=0',
                               'AA=0',
                               'HE=0',
                               'AH=0',
                               'EH=0',
                               'HA=0',
                               'EA=0',
                               'AE=0',
                               'EE=0',
                               'HH=90',
                               'HH=75 and H_U=75',
                               'H_U=80, A_U=70, allT=80',
                               'H_U=80, A_U=70, allT=70',
                               'EH=50 and EA=50',
                               'HA=50 and AH=50',
                               'H_U=50, A_U=50, E_U=50'
  ),values=cols)+
  # 'no decay of AB-resistance in humans',
  # 'no decay of AB-resistance in animals',
  # 'no decay of AB-resistance in environment'))+
  theme_classic() +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
  theme(axis.text.x = element_text(color="black", 
                                   size=15),
        axis.text.y = element_text(color="black", 
                                   size=15)) + theme(legend.title = element_blank()) +
  #scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
  scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
  
  theme(legend.position = 'bottom')+guides(fill=guide_legend(ncol=3))+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  facet_wrap(~country, scales="free")
ggsave(paste("~/Documents/Projects/SEFASI/outputs/combined_ints_abs", ".png",sep=""), width = 400, height = 200, units='mm',dpi=1000)

P2_abs <- ggplot(INT_DATA_abs)+ geom_boxplot(aes(x=country,y=value*100,fill=country))+
  ylab(label="% Reduction in resistant bacteria in humans over 20 years")+
  xlab(label="Intervention")+ 
  scale_fill_manual(labels = c('DENMARK',
                               'ENGLAND',
                               'SENEGAL'
                               
  ),values=cols)+
  # 'no decay of AB-resistance in humans',
  # 'no decay of AB-resistance in animals',
  # 'no decay of AB-resistance in environment'))+
  theme_classic() +
  theme(panel.border = element_rect(linetype = "solid", fill = NA)) +
  theme(text = element_text(size=15,colour="black")) + # scale_color_brewer(palette="Set1")+ 
  theme(axis.text.x = element_text(color="black", 
                                   size=15),
        axis.text.y = element_text(color="black", 
                                   size=15)) + theme(legend.title = element_blank()) +
  #scale_color_manual(name="",values = c("Humans" = "#3B9AB2","Animals" = "darkgreen", "Environment" ="#EBCC2A" ))+
  scale_colour_discrete(name = "", labels = c("Animals", "Environment", "Humans"))+
  
  theme(legend.position = 'bottom')+guides(fill=guide_legend(ncol=3))+
  theme(
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank())+
  scale_y_continuous(limits=c(0,100),expand = c(0,0)) +
  facet_wrap(~factor(variable,levels = int_levels), scales="free")
ggsave(paste("~/Documents/Projects/SEFASI/outputs/combined_countries_abs", ".png",sep=""), width = 400, height = 300, units='mm',dpi=1000)


