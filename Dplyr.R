rm(list = ls())

# Pobranie danych
Allegan <- read.csv("http://www.calvin.edu/~stob/data/Allegan.csv")

dim(Allegan)
head(Allegan)

library(dplyr)
library(sqldf)
select <- select(Allegan, Day, Month, Year, PRCP)
select.sql <- sqldf("SELECT Day, Month, Year, PRCP FROM Allegan")

select.drop <- select(Allegan, - starts_with("State"))

select.as <- select(Allegan, precipitation = PRCP)
select.sql.as <- sqldf("SELECT PRCP AS precipitation FROM Allegan")

filtered <- filter(Allegan, 2000 <= Year, Year <= 2005)
select.where <- sqldf("SELECT * FROM Allegan WHERE Year BETWEEN 2000 AND 2005")

filtered.in <- filter(Allegan, Month %in% c(7, 8, 9), PRCP == 0.00)
select.sql.in <- sqldf("SELECT * FROM Allegan WHERE Month IN (7, 8, 9) AND PRCP = 0.00")

mutate.new.column <- mutate(Allegan, TMAX.CEL = (TMAX - 32) * (5 / 9),
                                     TMIN.CEL = (TMIN - 32) * (5 / 9),
                                     AMPL.CEL = TMAX.CEL - TMIN.CEL)

select.order <- arrange(mutate.new.column, desc(AMPL.CEL), TMIN)
select.sql.order <- sqldf("SELECT * FROM 'mutate.new.column' ORDER BY 'AMPL.CEL' DESC, TMIN")

# U¿ywaj¹c ³¹cznika %.% budujemy chain, w którym kolejne operacje zapisujemy "od lewej do prawej"
select.chain <- Allegan %>%
                filter(2000 <= Year, Year <= 2005, Month %in% c(7, 8, 9), PRCP == 0.00) %>%
                mutate(TMAX.CEL = (TMAX - 32) * (5 / 9),
                    TMIN.CEL = (TMIN - 32) * (5 / 9),
                    AMPL.CEL = TMAX.CEL - TMIN.CEL) %>%
                arrange(desc(AMPL.CEL), TMIN)

select.chain.group_by <- Allegan %>%
                        group_by(Day) %>%
                        summarize(mean.JD = mean(JD))

# Usuwamy obserwacje NA 
select.na.omit <- Allegan %>% na.omit()