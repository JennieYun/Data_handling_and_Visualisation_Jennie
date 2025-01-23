library(readr)
ec <- read_csv("/Users/zyeon/Downloads/data_economist.csv")
head(ec)
str(ec)
levels(ec$Region)
ec$Region <- factor(ec$Region,levels = c("EU W. Europe",
                                         "Americas",
                                         "Asia Pacific",
                                         "East EU Cemt Asia",
                                         "MENA",
                                         "SSA"),
                    labels = c("OECD",
                               "Americas",
                               "Asia &\nOceania",
                               "Central &\nEastern Europe",
                               "Middle East &\nNorth Africa",
                               "Sub-Saharan\nAfrica"))
levels(ec$Region)

library(ggplot2)

ggplot(ec,aes(x=CPI,y=HDI,color=Region))+
  geom_point(shape=1,size=3,stroke=0.8)

?geom_smooth

ggplot(ec,aes(x=CPI,y=HDI,color=Region))+
  geom_point(shape=1,size=3,stroke=0.8)+
  geom_smooth(method="lm",formula=y~poly(x,2),se=F,size=1.1,color="red")

p <- ggplot(ec,aes(x=CPI,y=HDI,color=Region))+
  geom_smooth(method="lm",formula=y~poly(x,2),se=F,color="red",size=0.8)+
  geom_point(shape=1,size=3,stroke=0.8)
p

labels <- c("Congo","Afghanistan","Sudan","Myanmar","Iraq","Venezuela","Russia","Argentina","Brazil","Italy","South Africa","Cape Verde","Bhutan","Botswana","Britian","New Zealand","Greece","China","India","Rwanda","Spain","France","United States","Japan","Norway","Singapore","Barbados", "Germany", "Korea (South)")

p+geom_text(data=subset(ec,Country %in% labels),aes(label=Country),color="black")



############## Font installation ##################

install.packages("extrafont")
install.packages("devtools")
library(devtools)
library(extrafont)

#It's recommended codes by the website ########
library(extrafont)
font_import(pattern="Gidole",prompt=FALSE)
# load fonts for pdf
loadfonts()
# list available fonts in R
fonts()
##################################

install.packages("showtext")
library(showtext)
font_add("Gidole", "/Users/zyeon/Downloads/GidoleFont/Gidole-Regular.ttf")
showtext_auto()

library(ggrepel)
p <- p+geom_text_repel(data=subset(ec,Country %in% labels),aes(label=Country),
                       color="black",box.padding=unit(1,'lines'),segment.size=0.25,
                       size=3,family="Gidole")
p

p <- p+scale_x_continuous(name="Corruption Perceptions Index, 2011 (10=least corrupt)",
                          breaks=1:10,limits=c(1,10))+
  scale_y_continuous(name="Human Development Index, 2011 (1=best)",
                     breaks=seq(from=0,to=1,by=0.1),limits=c(0.2,1))
p


p <- p+scale_x_continuous(name="Corruption Perceptions Index, 2011 (10=least corrupt)",
                          breaks=1:10,limits=c(1,10))+
  scale_y_continuous(name="Human Development Index, 2011 (1=best)",
                     breaks=seq(from=0,to=1,by=0.1),limits=c(0.2,1))
p


p <- p+scale_color_manual(values=c("#23576E","#099FDB","#29B00E", "#208F84","#F55840","#924F3E"))+
  scale_fill_manual(name="trend",values="red",labels=expression(paste(R^2,"=52%")))
p

p <- p+labs(title="Corruption and human development",
            caption="Sources: Transparency International; UN Human Development Report")
p


p <- p+guides(color=guide_legend(nrow=1))+
  theme_bw(base_family="Gidole")+
  theme(legend.position="top")
p

p+theme(panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        legend.title=element_blank(),
        axis.title=element_text(face="italic"),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(color="grey60"),
        plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0,size=8))
