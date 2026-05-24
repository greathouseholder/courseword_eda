model <- mlogit(
  choice ~ accuracy + accuracy:visited + waiting_time + provider | 0,
  data = mlogit_data
)

summary(model)