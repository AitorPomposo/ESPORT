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



