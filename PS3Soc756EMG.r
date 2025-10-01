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

radix <- 85000

data4 <- data3 %>% 
  mutate(lx = radix*lag(cumprod(1-Total)))

data4$lx[1] <- radix

View(data4)

data5 <- data4 %>% 
  mutate(ndx = Total*lx , ndx_i = lx*crashprob) 

View(data5)

# completed calculations manually