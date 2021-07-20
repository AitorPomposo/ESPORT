install.packages("tidyverse")
library("tidyverse")
install.packages("ggthemes")
library("ggthemes")


dfdata <- read.table("data.csv", sep=";", dec=".", quote = "\"",
                        header=TRUE, skip = 0, na.strings = "NA")

str(dfdata)
dfdata$Date <- as.Date(dfdata$Date)
str(dfdata$Date)

MinutePlayed<- dfdata %>% group_by(Player.ID, Position.Id) %>% summarise(Minutos=sum(Time.Played), na.rm=T) %>%
  arrange(desc(Minutos)) 


#MinutePlayed[1:10,]
#Numrow <- nrow(MinutePlayed)
#MinutePlayed[(nrow(MinutePlayed)-9L):nrow(MinutePlayed),]


ggplot(MinutePlayed, aes(x=Minutos)) + geom_histogram(binwidth = 250, color= "black", fill= "red") +
  facet_grid(Position.Id~.)

ggplot(MinutePlayed, aes(x=Minutos, fill= as.factor(Position.Id))) + geom_histogram(binwidth = 250, color= "black") +
  facet_grid(Position.Id~.) + guides(fill="none") + 
    scale_fill_manual(values=c("blue","red","green","yellow"))

ggplot(MinutePlayed, aes(x=Minutos, fill= as.factor(Position.Id))) + geom_histogram(binwidth = 250, color= "black") +
  facet_grid(factor(as.numeric(as.factor(MinutePlayed$Position.Id)), levels= 1:4, labels = c("Goalkeeper","Defender","Midfielder","Atacker"))~.) + guides(fill="none") + 
  scale_fill_manual(values=c("blue","red","green","yellow"))

ggplot(MinutePlayed, aes(x=Minutos, fill= as.factor(Position.Id))) + geom_histogram(binwidth = 250, color= "black") +
  facet_grid(factor(as.numeric(as.factor(MinutePlayed$Position.Id)), levels= 1:4, labels = c("Goalkeeper","Defender","Midfielder","Attacker"))~., scales= "free_y") + guides(fill="none") + 
  scale_fill_manual(values=c("blue","red","green","yellow"))


MinutePlayedTeam<- dfdata %>% group_by(Player.ID, Team.Id) %>% summarise(Minutos=sum(Time.Played), na.rm=T) %>%
  arrange(desc(Minutos)) 

#Lo mismo pero ahora hago la separación con los nombres

MinutePlayedTeamName<- dfdata %>% group_by(Player.ID, Team, Team.Id) %>% summarise(Minutos=sum(Time.Played), na.rm=T) %>%
  arrange(desc(Minutos)) 

pal <- colorRampPalette(c("lightblue","chartreuse3","darkgoldenrod2","coral3"))

ggplot(MinutePlayedTeamName, aes(x=Minutos, y=Team, fill=as.factor(Team))) + geom_boxplot() + 
  guides(fill= "none") + geom_jitter(shape=21, alpha=0.2, size= 0.5) +
  scale_fill_manual(values=pal(20))

#MinutePlayedTeamNameHalf1 <- MinutePlayedTeamName[MinutePlayedTeamName$Team.Id<32,]

#ggplot(MinutePlayedTeamNameHalf1, aes(x=Minutos)) + geom_histogram(binwidth = 250, color= "black", fill= "red") +
#  facet_grid(Team~.)

#Para el shiny, podemos poner a un equipo y que en función de esa elección, salga ese gráfico
Prueba <- MinutePlayedTeamName[MinutePlayedTeamName$Team == "Manchester City",]

ggplot(Prueba, aes(x=Minutos)) + geom_histogram(binwidth = 250, color= "black", fill= "red")

#Vamos a filtrar a porteros para ver cómo quedaron en cada partido

Porteros <- dfdata[dfdata$Position.Id == 1,] %>%   arrange(Date)
Porteros$Partido <- as.integer(paste(Porteros$Team.Id, Porteros$Opposition.id, sep="")) 
if(Porteros$Venue == "Away") Porteros$Partido <- as.integer(paste(Porteros$Opposition.id, Porteros$Team.Id, sep=""))
Porteros$Partido <- Porteros[Porteros$Venue == "Away",]

# La parte de arriba aun no me ha salido