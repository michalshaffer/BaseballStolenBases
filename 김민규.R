---
title: "Untitled"
author: "Kim min kyu"
date: "2021?? 5?? 20??"
output: html_document
---

```{r}
library(baseballr)
library(Lahman)
library(tidyverse)
library(broom)
library(ggrepel)
library(gridExtra)
setwd('C:/Users/user/Desktop/Paper')
```

```{r}
fields <- read_csv('fields.csv')
data2016 <- read_csv('all2016.csv', col_names=pull(fields, Header), na=character())
head(data2016)
```

```{r}
### Runs scored in the remainder of the inning
data2016 <- data2016 %>% 
  mutate(RUNS = AWAY_SCORE_CT + HOME_SCORE_CT,
         HALF.INNING = paste(GAME_ID, INN_CT, BAT_HOME_ID),
         RUNS.SCORED = (BAT_DEST_ID > 3) + (RUN1_DEST_ID > 3) + (RUN2_DEST_ID > 3) + (RUN3_DEST_ID > 3))

half_innings_2016 <- data2016 %>% 
  group_by(HALF.INNING) %>% 
  summarize(Outs.Inning = sum(EVENT_OUTS_CT),
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start)

data2016 <- data2016 %>% 
  inner_join(half_innings_2016, by='HALF.INNING') %>% 
  mutate(RUNS.ROI = MAX.RUNS - RUNS)
```

```{r}
### Creating the Run Expectancy matrix
data2016 <- data2016 %>% 
  mutate(BASES = paste(ifelse(BASE1_RUN_ID != '', 1, 0),
                       ifelse(BASE2_RUN_ID != '', 1, 0),
                       ifelse(BASE3_RUN_ID != '', 1, 0), sep=''),
         STATE = paste(BASES, OUTS_CT))

data2016 <- data2016 %>% 
  mutate(NRUNNER1 = as.numeric(RUN1_DEST_ID == 1 | BAT_DEST_ID == 1),
         NRUNNER2 = as.numeric(RUN1_DEST_ID == 2 | RUN2_DEST_ID == 2 | BAT_DEST_ID == 2),
         NRUNNER3 = as.numeric(RUN1_DEST_ID == 3 | RUN2_DEST_ID == 3 | RUN3_DEST_ID == 3 | BAT_DEST_ID == 3),
         NOUTS = OUTS_CT + EVENT_OUTS_CT,
         NEW.BASES = paste(NRUNNER1, NRUNNER2, NRUNNER3, sep=''),
         NEW.STATE = paste(NEW.BASES, NOUTS))

data2016 <- data2016 %>% 
  filter((STATE != NEW.STATE) | (RUNS.SCORED > 0))

data2016C <- data2016 %>%
  filter(Outs.Inning == 3)
```

```{r}
RUNS2016 <- data2016C %>% 
  group_by(STATE) %>% 
  summarize(Mean = mean(RUNS.ROI)) %>% 
  mutate(Outs = substr(STATE, 5, 5)) %>% 
  arrange(Outs)

REM2016 <- matrix(round(RUNS2016$Mean, 2), 8, 3)
dimnames(REM2016)[[2]] <- c('0 outs', '1 out', '2 outs')
dimnames(REM2016)[[1]] <- c('000', '001', '010', '011',
                            '100', '101', '110', '111')
REM2016; sum(REM2016)
```

```{r}
# When runner is safe
s10 <- REM2016[3,1] - REM2016[5,1]  # 100 0 outs -> 010 0 outs
s11 <- REM2016[3,2] - REM2016[5,2]  # 100 1 outs -> 010 1 outs
s12 <- REM2016[3,3] - REM2016[5,3]  # 100 2 outs -> 010 2 outs
s10; s11; s12

s20 <- REM2016[2,1] - REM2016[3,1]  # 010 0 outs -> 001 0 outs
s21 <- REM2016[2,2] - REM2016[3,2]  # 010 1 outs -> 001 1 outs
s22 <- REM2016[2,3] - REM2016[3,3]  # 010 1 outs -> 001 1 outs
s20; s21; s22
```

```{r}
# when Runner is out
o10 <- REM2016[1,2] - REM2016[5,1]  # 100 0 outs -> 000 1 outs
o11 <- REM2016[1,3] - REM2016[5,2]  # 100 1 outs -> 000 2 outs
o12 <- 0 - REM2016[5,3]             # 100 2 outs -> 000 0 outs
o10; o11; o12

o20 <- REM2016[1,2] - REM2016[3,1]  # 010 0 outs -> 000 1 outs
o21 <- REM2016[1,3] - REM2016[3,2]  # 010 1 outs -> 000 2 outs
o22 <- 0 - REM2016[3,3]             # 010 2 outs -> 000 0 outs
o20; o21; o22
```

```{r}
SB <- data2016 %>% 
  filter(EVENT_CD == 4)
CS <- data2016 %>% 
  filter(EVENT_CD == 6)
```

```{r}
sb <- nrow(SB)
cs <- nrow(CS)
success_rate <- sb / (sb+cs)
success_rate
```

```{r}
success_rate * (s10 + s11 + s12 + s20 + s21 + s22) / 6 + (1-success_rate) * (o10 + o11 + o12 + o20 + o21 + o22) / 6
```


```{r}
SB2016 <- data2016 %>%
  filter(EVENT_CD == 4)
CS2016 <- data2016 %>% 
  filter(EVENT_CD == 6)
```

```{r}
tab_SB <- SB2016 %>% 
  group_by(STATE, NEW.STATE, RUNS.SCORED) %>% 
  tally()
tab_CS <- CS2016 %>% 
  group_by(STATE, NEW.STATE, RUNS.SCORED) %>%
  tally()
```

```{r}
print(tab_SB, n=38)
print(tab_CS, n=24)
```

```{r}
RED <- rep(NA, nrow(tab_SB))
for(i in 1:nrow(tab_SB)){
  ind <- which(tab_SB$STATE[i] == RUNS2016$STATE)
  new.ind <- which(tab_SB$NEW.STATE[i] == RUNS2016$STATE)
  
  if(substr(tab_SB$NEW.STATE[i], 5, 5) != '3'){
    RED[i] <- RUNS2016$Mean[new.ind] - RUNS2016$Mean[ind]
  } else{
    RED[i] <- 0 - RUNS2016$Mean[ind]
  }
}
tab_SB$RED <- RED

RED <- rep(NA, nrow(tab_CS))
for(i in 1:nrow(tab_CS)){
  ind <- which(tab_CS$STATE[i] == RUNS2016$STATE)
  new.ind <- which(tab_CS$NEW.STATE[i] == RUNS2016$STATE)
  
  if(substr(tab_CS$NEW.STATE[i], 5, 5) != '3'){
    RED[i] <- RUNS2016$Mean[new.ind] - RUNS2016$Mean[ind]
  } else{
    RED[i] <- 0 - RUNS2016$Mean[ind]
  }
}
tab_CS$RED <- RED

tab_SB$RED; tab_CS$RED

tab_SB <- tab_SB %>%
  mutate(rRED = RUNS.SCORED + RED,
         nRED = n * rRED)
tab_CS <- tab_CS %>% 
  mutate(rRED = RUNS.SCORED + RED,
         nRED = n * rRED)

(sum(tab_SB$nRED) + sum(tab_CS$nRED)) / (sum(tab_SB$n) + sum(tab_CS$n))
```



