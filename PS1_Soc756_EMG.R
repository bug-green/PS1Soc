install.packages("dplyr")
install.packages("purrr")
install.packages("ggplot2")

library(dplyr)
library(purrr)
library(ggplot2)

life_table <- ps1_data_F2023 %>%
  mutate( nMx = nDx / nNx,
          nqx = nMx / (1 + (1 - nax) * nMx),
          nqx = if_else(nqx > 1, 1, nqx),
          npx = 1 - nqx) %>%
  mutate(lx = accumulate(npx, ~ .x * .y, .init = 100000)[-1],
         ndx = lx * nqx) %>%
  mutate(lx_next = lead(lx),
         nLx = case_when(
           is.na(lx_next) ~ lx / nMx,
           TRUE ~ (lx + lx_next) / 2 * (x[2] - x[1]))) %>%
  mutate(Tx = rev(cumsum(rev(nLx))),
         ex = Tx / lx,
         nmx = ndx / nLx) %>%
  select(x, nDx, nNx, nMx, nqx, npx, lx, ndx, nLx, Tx, ex, nmx)

View(life_table)

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
