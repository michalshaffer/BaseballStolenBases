dev.off()        # clear all plots
rm(list=ls())    # clear Global Environment
cat("\f")        # clear Console 

##### Q) Who is the best Stealing base players in 2019?

library(tidyverse)
library(Lahman)
library(ggrepel)

Batting %>%
  filter(yearID == 2019) -> Batting_2019

Batting_2019 %>%
  group_by(playerID) %>%
  summarize(SB=sum(SB)) -> sb_2019
# the total number of stealing home for each player in 2019


sb_2019 %>%
  arrange(desc(SB)) -> sb_2019
head(sb_2019)
# The best BS player in 2019 was Mark smith


##### Q) Who is the best Stealing base or Caughted Stealing players with at least 5000 career at-bats (All season)?

sb_leader <- function(data) {
  data %>%
    group_by(playerID) %>%
    summarize(SB = sum(SB)) %>%
    arrange(desc(SB)) %>%
  head(1)
}

Batting %>%
  mutate(year = yearID) %>%
  split(pull(., year)) %>%
  map_df(sb_leader, .id = "year")

Batting %>%
  group_by(playerID) %>%
  summarize(tAB = sum(AB, na.rm = TRUE),
            tCS = sum(CS, na.rm = TRUE),
            tSB = sum(SB, na.rm = TRUE)) -> long_careers

Batting_5000 <- filter(long_careers, tAB >= 5000)

Batting_5000 %>%
  arrange(desc(tSB)) -> Batting_5000

head(Batting_5000)
# The best Stealing base or Caughted Stealing player with at least 5000 career at-bats is Ricky henderson.(1,406 stolen bases)

##### Scatterplot of the SB rates and CS rates of all players with at least 5000 career at-bats

ggplot(Batting_5000, aes(x = tCS / tAB, y = tSB / tAB)) +
  geom_point() + geom_smooth() +
  geom_text_repel(data = filter(Batting_5000, tSB/tAB >= 0.10),
                  aes(label = playerID))

# A smoothing curve is added to the plot to show SB rates and CS rates have a positive association above 0.005 tCS/tAB.
# It is clear from the graph that batters with higher CS rates tend to have higher BS rates.



##### Value of the base stealing

fields <- read.csv("C:/Users/82102/Desktop/fields.csv")
data2016 <- read.csv("C:/Users/82102/Desktop/all2016.csv",
                     col.names = pull(fields, Header),
                     na = character())

data2016 %>%
  mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED =
           (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) +
           (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3)) ->
  data2016

data2016 %>%
  group_by(HALF.INNING) %>%
  summarize(Outs.Inning = sum(EVENT_OUTS_CT),
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) ->
  half_innings

data2016 %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = MAX.RUNS - RUNS) ->
  data2016

data2016 %>%
  mutate(BASES =
         paste(ifelse(BASE1_RUN_ID > '', 1, 0),
               ifelse(BASE2_RUN_ID > '', 1, 0),
               ifelse(BASE3_RUN_ID > '', 1, 0), sep = ""),
         STATE = paste(BASES, OUTS_CT)) ->
  data2016

data2016 %>%
  mutate(NRUNNER1 =
           as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1),
         NRUNNER2 =
           as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 |
                        BAT_DEST_ID == 2),
         NRUNNER3 =
           as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 |
                        RUN3_DEST_ID == 3 | BAT_DEST_ID == 3),
         NOUTS = OUTS_CT + EVENT_OUTS_CT,
         NEW.BASES = paste(NRUNNER1, NRUNNER2,
                           NRUNNER3, sep = ""),
         NEW.STATE = paste(NEW.BASES, NOUTS)) ->
  data2016

data2016 %>%
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0)) ->
  data2016

data2016 %>%
  filter(Outs.Inning == 3) -> data2016C

data2016C %>%
  group_by(STATE) %>%
  summarize(Mean = mean(RUNS.ROI)) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS

data2016 %>%
  left_join(select(RUNS, -Outs), by = "STATE") %>%
  rename(Runs.State = Mean) %>%
  left_join(select(RUNS, -Outs),
            by = c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = Mean) %>%
  replace_na(list(Runs.New.State = 0)) %>%
  mutate(run_value = Runs.New.State - Runs.State +
           RUNS.SCORED) -> data2016

data2016 %>%
  filter(EVENT_CD %in% c(4, 6)) -> stealing

stealing %>%
  group_by(EVENT_CD) %>%
  summarize(N = n()) %>%
  mutate(pct = N / sum(N))
# Among all stolen base attempts, the proportion of stolen bases is equal to 2213 / (2213 + 712) = 0.757

stealing %>% group_by(STATE) %>% summarize(N = n()) -> stealing_base
cbind(stealing_base[1:3,], stealing_base[4:6,], stealing_base[7:9,],stealing_base[10:12,])
# A frequency table for the STATE variable.
# Stolen base attempts typically happen with a runner only on first

ggplot(stealing, aes(run_value, fill = factor(EVENT_CD))) +
  geom_histogram() +
  scale_fill_manual(name = "EVENT_CD",
                    values = c("blue", "gray70"),
                    labels = c("Stolen Base (SB)",
                               "Caught Stealing (CS)"))
# all of the successful SBs have positive run value, although most of the values fall in the interval from 0 to 0.3.
# In contrast, the unsuccessful CSs(as expected) have negative run values.
# Three spikes for negative run values correspond to CS when there is only a runner on first with 0, 1, and 2 outs.

# We create a new data frame that gives the attempted stealing data when there is a runner on first base with one out (state ¡°100 1¡±).
stealing %>% filter(STATE == "100 1") -> stealing.1001

stealing.1001 %>%
  group_by(EVENT_CD) %>%
  summarize(N = n()) %>%
  mutate(pct = N / sum(N))
# we see the runner successfully stole 498 times out of 498 + 210 attempts for a success rate of 70.3%.

stealing.1001 %>%
  group_by(NEW.STATE) %>%
  summarize(N = n()) %>%
  mutate(pct = N / sum(N))
# On 457 occurrences, the runner successfully advanced to second base.
# On an additional 39 occurrences, the runner advanced to third.
# Perhaps this extra base was due to a bad throw from the catcher or a misplay by the infielder.

stealing.1001 %>% summarize(Mean = mean(run_value))
# Stolen base attempts are worthwhile in this situation, although the value overall is about 0.007 runs per attempt

