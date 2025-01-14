# Llibreries
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyverse)

# Importem les dades
companies_cap <- read.csv(file = 'C:/Users/Marc Vila/Documents/UOC/Visualitzaci� de dades/PRAC 2/companies_market_cap.csv')
colnames(companies_cap) <- c("num","company","country","sector","market_cap_bn","market_cap_added","change")

# Definim correctament el tipus de variable de les columnes num�riques
cols.num <- c("market_cap_bn","market_cap_added","change")
companies_cap[cols.num] <- sapply(companies_cap[cols.num],as.numeric)

# Agrupem les dades per països i per sectors
cap_groupped <- companies_cap  %>%
  group_by(country,sector)  %>%
  summarize(bn_sum = sum(market_cap_bn),
            added_sum = sum(market_cap_added),
            change_sum = sum(change))

# Ordenem les dades segons 'added_sum' i seleccionem els primers 25 registres per obtenir
attach(cap_groupped)
cap_50_groupped <- cap_groupped[order(-added_sum),]
cap_50_groupped <- head(cap_50_groupped, 25)
detach(cap_groupped)

# Visualitzaci� pa�sos
ggplot(cap_50_groupped, aes(fill=country, y=added_sum, x=reorder(sector, -added_sum))) + 
  geom_bar(position="stack", stat="identity") +
  labs(title = "Sectors en els que m�s ha augmentatel capital durant la pand�mia:",
       subtitle = "Segons els pa�sos capdavanters") + 
  xlab("Sector Econ�mic") + 
  ylab("Augment de capital") +
  coord_flip()

# Guardem la figura amb el tamany desitjat
ggsave(
  file="Sectors en els que m�s ha augmentat el capital durant la pand�mia: Segons els pa�sos capdavanters.png",
  width=8, height=4.5)
