library(tidyverse)
library(ggplot2)
library(stringr)

#create empty data frames
sbs <<- data.frame()
DIs <<- data.frame()

#get fields for the data files
fields <- read_csv("C:/R directory/Retrosheet event files by season/fields.csv")


files <- c("C:/R directory/Retrosheet event files by season/Decade files/all1910s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all1920s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all1930s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all1940s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all1950s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all1960s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all1970s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all1980s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all1990s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all2000s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all2010s.csv",
           "C:/R directory/Retrosheet event files by season/Decade files/all2020s.csv")



#function to get the stolen base and DI data from an inputted file
getyeardata <- function(filename){
  
  #read in the file
  dataf <- read_csv(filename, 
                    col_names = pull(fields, Header),
                    na = character())
  #extract game year
  dataf <- dataf %>% mutate(GAME_YEAR = substr(GAME_ID, 4, 7))
  dataf <- dataf %>% filter(GAME_YEAR != "")
  
  #get events with attempted stolen bases
  thissbs <- dataf %>% filter(EVENT_CD %in% c(4,6)) %>%  group_by(GAME_YEAR, EVENT_CD) %>% summarize(N = n())
  
  #get DI events 
  thisDIs <- filter(dplyr::filter(dataf, grepl("DI", EVENT_TX)))
  thisDIs <- thisDIs %>% group_by(GAME_YEAR) %>% summarise(N = n())
  
  #add averages per game
  gamesperyear <- dataf  %>%  group_by(GAME_YEAR)  %>% distinct(GAME_ID) %>% summarise(GAMES = n())
  
  #add game year to sbs df
  thissbs <- left_join(thissbs, gamesperyear, by = "GAME_YEAR")
  thissbs <- thissbs %>% mutate(avgsbspgame = N/GAMES)
  thisDIs <- left_join(thisDIs, gamesperyear, by = "GAME_YEAR")
  thisDIs <- thisDIs %>% mutate(avgdipgame = N/GAMES)
  
  #combine with the global sbs and DIs data frames
  sbs <<- rbind(sbs, thissbs)
  DIs <<- rbind(DIs, thisDIs)
  
}


#get all the data from all the files using the function
lapply(files, FUN = getyeardata)

#add a type column
sbs <- sbs %>% mutate(Type = "sbs")
DIs <- DIs %>% mutate(Type = "dis")

#create plot
#stolen bases and caught stealing
sbsplot <- 
  ggplot(data = sbs, aes(GAME_YEAR, N, color = factor(EVENT_CD))) +
  geom_point() +
  geom_smooth(aes(group = EVENT_CD), span= 0.3) +
  scale_color_manual("", labels = c("Successful SB", "Failed SB"), values = c("blue", "red")) +
  scale_x_discrete(name ="Game Year", breaks=c(seq(from=1910, to=2020, by=10))) +
  labs(y = "Attempted SBs") 



#plot for average per game
sbsavgplot <- 
  ggplot(data = sbs, aes(GAME_YEAR, avgsbspgame, color = factor(EVENT_CD))) +
  geom_point() +
  geom_smooth(aes(group = EVENT_CD), span= 0.3) +
  scale_color_manual("", labels = c("Successful SB", "Failed SB"), values = c("blue", "red")) +
  scale_x_discrete(name ="Game Year", breaks=c(seq(from=1910, to=2020, by=10))) +
  labs(y = "Avg Attempted SBs Per Game") +
  theme(legend.position="top")


totalsbsatt <- sbs %>% filter(Type == "sbs") %>% group_by(GAME_YEAR) %>% summarize(N = sum(N), GAMES = (sum(GAMES)/2))
totalsbsatt <- totalsbsatt  %>% mutate(totalavgsbspergame = N/GAMES)


#total attempted stolen bases
totalsbsattplot <-  
  ggplot(data = totalsbsatt, aes(x = GAME_YEAR, y = N)) +
  geom_point() +
  geom_smooth(aes(group = "Type"), span= 0.3) +
  scale_x_discrete(name ="Game Year", breaks=c(seq(from=1910, to=2020, by=10))) +
  scale_y_continuous(name = "Total attempted SBs", limits = c(500,4750)) 


#total attempted stolen bases by avg per game
totalavgsbsattplot <-  
  ggplot(data = totalsbsatt, aes(x = GAME_YEAR, y =totalavgsbspergame)) +
  geom_point() +
  geom_smooth(aes(group = "Type"), span = 0.3) +
  scale_x_discrete(name ="Game Year", breaks=c(seq(from=1910, to=2020, by=10))) +
  scale_y_continuous(name = "Avg attempted SBs per game", limits = c(0,4)) 


#total DIs
DIsplot <-
  ggplot(data = DIs, aes(GAME_YEAR, N)) +
  geom_point() +
  geom_smooth(aes(group = Type), span= 0.3) +
  scale_x_discrete(name ="Game Year", breaks=c(seq(from=1910, to=2020, by=10))) +
  labs(y = "Number of SBs disregarded by defense") 

#total DIs by avg per game
DIsavgplot <-
  ggplot(data = DIs, aes(GAME_YEAR, avgdipgame)) +
  geom_point() +
  geom_smooth(aes(group = Type), span= 0.3) +
  scale_x_discrete(name ="Game Year", breaks=c(seq(from=1910, to=2020, by=10))) +
  labs(y = "Avg SBs disregarded by defense per game") 


cs <- sbs %>% filter(EVENT_CD == 6) %>% select(GAME_YEAR, cs = N, GAMES)
sb <- sbs %>% filter(EVENT_CD == 4) %>% select(GAME_YEAR, sb = N)
cssb <- inner_join(cs, sb)
di <- DIs %>% select(GAME_YEAR, di = N)
dijoined <- inner_join(cssb, di)
dijoined <- dijoined %>% mutate(csdirate = (cs/(cs+sb-di)), csrate = cs/(sb+cs))
view(dijoined)

#plot of cs rates with and without DIs
CSrateplot <-
  ggplot(data = dijoined, aes(GAME_YEAR)) +
  geom_point(aes(y = csrate, color = "blue")) +
  geom_point(aes(y = csdirate, color = "red")) +
  #geom_smooth(aes(group = colour), span= 0.3) +
  scale_x_discrete(name ="Game Year", breaks=c(seq(from=1910, to=2020, by=10))) +
  labs(y = "Rate of caught stealing per year") +
  theme(legend.position="top") +
  scale_color_manual("", labels = c("CSRate", "CSRate without DIs"), values = c("blue", "red")) 


#arrange all the plots
grid.arrange(totalsbsattplot, totalavgsbsattplot)
grid.arrange(sbsavgplot)
grid.arrange(totalavgsbsattplot, DIsavgplot)
grid.arrange(CSrateplot)


#DIS for 2000s
dataf <- read_csv(files[10], 
                  col_names = pull(fields, Header),
                  na = character())
#extract game year
data00s <- dataf %>% mutate(GAME_YEAR = substr(GAME_ID, 4, 7))
data00s <- data00s %>% filter(GAME_YEAR != "")

#get number of DIs per inning and the average score difference
thisDIs00s <- filter(dplyr::filter(data00s, grepl("DI", EVENT_TX)))
thisDIs00s <- thisDIs00s %>% select(INN_CT, OUTS_CT, HOME_SCORE_CT, AWAY_SCORE_CT)
thisDIs00s <- thisDIs00s %>% mutate(scorediff = abs(HOME_SCORE_CT - AWAY_SCORE_CT))
thisDIs00sbyinning <- thisDIs00s %>% group_by(INN_CT) %>% summarise(scorediffavg = round(mean(scorediff),2), N = n())

#write.csv(thisDIs00sbyinning, "2000sDIs.csv")

#DIS for 2010s
dataf2 <- read_csv(files[11], 
                  col_names = pull(fields, Header),
                  na = character())
#extract game year
data10s <- dataf2 %>% mutate(GAME_YEAR = substr(GAME_ID, 4, 7))
data10s <- data10s %>% filter(GAME_YEAR != "")

#get number of DIs per inning and the average score difference
thisDIs10s <- filter(dplyr::filter(data10s, grepl("DI", EVENT_TX)))
thisDIs10s <- thisDIs10s %>% select(INN_CT, OUTS_CT, HOME_SCORE_CT, AWAY_SCORE_CT)
thisDIs10s <- thisDIs10s %>% mutate(scorediff = abs(HOME_SCORE_CT - AWAY_SCORE_CT))
thisDIs10sbyinning <- thisDIs10s %>% group_by(INN_CT) %>% summarise(scorediffavg = round(mean(scorediff),2), N = n())

#write.csv(thisDIs10sbyinning, "2010sDIs.csv")
