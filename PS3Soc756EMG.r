library(HMDHFDplus)
library(dplyr)

username <- readline(prompt = "[omitted for security]")
password <- readline(prompt = "[omitted for security]")

data <- readHMDweb("USA", "Mx_1x1", username, password, fixup = TRUE) |>
  filter(Year == 2005)

View(data)

data2 <- data %>%
  mutate(crashprob = 0.062 - 0.000053*(Age^2))

# note that nqx is in the HMD dataset as "Total" and nqx(i) is "crashprob"

View(data2)

data3 <- data2[-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16) ]

View(data3)

data4 <- data3 %>%
  mutate(nqx_all = Total + crashprob)

View(data4)

radix <- 85000

data5 <- data4 %>% 
  mutate(lx = radix*lag(cumprod(1-nqx_all)))

data4$lx[1] <- radix

View(data5)

data6 <- data5 %>% 
  mutate(ndx_T = Total*lx , ndx_i = crashprob*lx) 

View(data6)

# completed calculations manually
