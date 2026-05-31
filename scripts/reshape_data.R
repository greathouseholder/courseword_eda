library(readxl)
library(mlogit)
library(dfidx)


cleaned_data_wide = read_excel('cleaned_data_wide.xlsx')

data = reshape(as.data.frame(cleaned_data_wide), direction='long',
               varying = list(
                 provider=6:8,
                 accuracy=10:12,
                 waiting_time=13:15
               ),
               v.names = c(
                 "provider",
                 "accuracy",
                 "waiting_time"
               ),
               timevar = "alt"
)

data$high_trust <- ifelse(data$trust >= 4, 1, 0)
data$visited <- ifelse(data$experience %in% c("Да, много раз", "Да, несколько раз"), 1, 0)

new_order = order(data$ID, data$trial, data$alt)
data = data[new_order, ]
data$choice = as.numeric(data$choice == data$alt)
data$accuracy = as.numeric(gsub(",", ".", data$accuracy))
data$waiting_time = as.numeric(data$waiting_time)

mlogit_data <- mlogit.data(
  data,
  choice = "choice",
  shape = "long",
  alt.var = "alt",
  id.var = "subject"
)

mlogit_data$provider <- as.factor(mlogit_data$provider)

mlogit_data$waiting_time <- factor(mlogit_data$waiting_time)
