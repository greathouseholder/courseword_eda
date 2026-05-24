model <- mlogit(
  choice ~ accuracy + waiting_time | 1 + high_trust,
  data = mlogit_data,
  reflevel = "2"   # doctor – базовая альтернатива
)
summary(model)