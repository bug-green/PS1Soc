install.packages("dplyr")
install.packages("purrr")
install.packages("ggplot2")
install.packages("readr")
install.packages("tidyverse")

library(dplyr)
library(purrr)
library(ggplot2)
library(readr)
library(tidyverse)

ps1_data_F2023 <- read_csv("ps1_data_F2023.csv")

nrow(ps1_data_F2023) options(scipen=999)

partiallifetable <- ps1_data_F2023 %>% 
  mutate(nMx = nDx/nNx, 
         n = lead(x)-x, 
         nqx = (n*nMx)/(1+(n-nax)*nMx), 
         npx = 1 - nqx)

View(partiallifetable)

radix <- 100000

lifetable <- partiallifetable %>% 
  mutate(lx = radix*lag(cumprod(npx)))

lifetable$lx[1] <- radix

lifetable <- lifetable %>% 
  mutate(ndx = lx*nqx, nLx=n*lead(lx) + nax*ndx)

lifetable$nLx[19] = lifetable$lx[19]/lifetable$nMx[19]

lifetable <- lifetable %>% 
  mutate(Tx = rev(cumsum(rev(nLx))), ex=Tx/lx, nmx = ndx / nLx)

View(lifetable)

ggplot(data = life_table, aes(x = x, y = lx)) +
  geom_line() +
  labs(
    title = "Number of Survivors at Each Age",
    x = "Age",
    y = "lx (Number of Survivors)"
  ) +
  theme_minimal()

ggplot(data = life_table, aes(x = x, y = ndx)) +
  geom_line() +
  labs(
    title = "Number of Deaths in the Age Interval",
    x = "Age",
    y = "ndx (Number of Deaths)"
  ) +
  theme_minimal()

ggplot(data = life_table, aes(x = x, y = nmx)) +
  geom_line() +
  labs(
    title = "Central Death Rates",
    x = "Age",
    y = "nmx (Central Death Rate)"
  ) +
  theme_minimal()
