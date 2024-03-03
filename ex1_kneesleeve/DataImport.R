library(tidyverse)

columns <- c("time","sensor1", "sensor2", "sensor3", "sensor4", "sensor5", 
             "sensor6", "sensor7", "sensor8", "sensor9", "sensor10", "sensor11", 
             "sensor12", "sensor13", "sensor14", "sensor15", "sensor16", "angle")
### Data Import

# calibration files -----
S1.calib01 <- read.csv("S1/S1.calib01.txt", col.names = columns) %>% 
  mutate(time = time - min(time))
S1.calib02 <- read.csv("S1/S1.calib02.txt", col.names = columns) %>% 
  mutate(time = time - min(time))
S2.calib01 <- read.csv("S2/S2.calib01.txt", col.names = columns)  %>% 
  mutate(time = time - min(time))
S3.calib01 <- read.csv("S3/S3.calib01.txt", col.names = columns)  %>% 
  mutate(time = time - min(time))
S3.calib02 <- read.csv("S3/S3.calib02.txt", col.names = columns)  %>% 
  mutate(time = time - min(time))
S4.calib01 <- read.csv("S4/S4.calib01.txt", col.names = columns)  %>% 
  mutate(time = time - min(time))

# flex files -----
S1.flex01 <- read.csv("S1/S1.flex01.txt", col.names = columns) %>% 
  mutate(time = time - min(time))
S1.flex02 <- read.csv("S1/S1.flex02.txt", col.names = columns) %>% 
  mutate(time = time - min(time))
S2.flex01 <- read.csv("S2/S2.flex01.txt", col.names = columns) %>% 
  mutate(time = time - min(time))
S2.flex02 <- read.csv("S2/S2.flex02.txt", col.names = columns) %>% 
  mutate(time = time - min(time))
S3.flex01 <- read.csv("S3/S3.flex01.txt", col.names = columns) %>% 
  mutate(time = time - min(time))
S4.flex01 <- read.csv("S4/S4.flex01.txt", col.names = columns) %>% 
  mutate(time = time - min(time))
S4.flex02 <- read.csv("S4/S4.flex02_end.txt", col.names = columns) %>% 
  mutate(time = time - min(time))
