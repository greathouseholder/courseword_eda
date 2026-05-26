library(mlogit)
library(dplyr)
library(ggplot2)

source("visuals_temp.R")
source("reshape_data.R")

model_h4 <- mlogit(
  choice ~ accuracy + accuracy:visited + waiting_time + provider | 0,
  data = mlogit_data
)

coefs_h4 <- coef(model_h4)

df_h4 <- data.frame(
  Group = c("Без опыта", "С опытом"),
  Beta = c(
    coefs_h4["accuracy"],
    coefs_h4["accuracy"] + coefs_h4["accuracy:visited"]
  ),
  color_group = c("Низкий доход", "Высокий доход")
)

p_h4 <- ggplot(df_h4, aes(Group, Beta, fill = color_group)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = format_num(Beta)),
    vjust = -0.5,
    size = 4.3
  ) +
  scale_fill_temp() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.25)),
    limits = c(0, NA)
  ) +
  labs(
    x = NULL,
    y = "Коэффициент β атрибута «Точность»"
  ) +
  theme_temp() +
  theme(axis.text.x = element_text(size = 12))

print(p_h4)

ggsave_temp(
  "hypo4_accuracy_by_experience.png",
  p_h4,
  width = 14,
  height = 10
)


df_h5 <- data.frame(
  Format = c("Только ИИ", "Врач + ИИ"),
  Coefficient = c(-1.125, -0.519),
  color_group = c("ИИ", "Доктор и ИИ")
)

p_h5 <- ggplot(df_h5, aes(Format, Coefficient, fill = color_group)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  
  geom_hline(yintercept = 0, linewidth = 0.6) +
  
  geom_text(
    aes(label = sprintf("%.3f", Coefficient)),
    vjust = 1.5,
    size = 4.2
  ) +
  
  scale_fill_temp() +
  scale_y_continuous(
    limits = c(-1.3, 0.15),
    expand = c(0, 0)
  ) +
  
  labs(
    x = NULL,
    y = "Коэффициент high_trust"
  ) +
  
  theme_temp() +
  theme(
    axis.text.x = element_text(size = 13),
    plot.margin = margin(t = 10, r = 20, b = 20, l = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  )

print(p_h5)

ggsave_temp(
  "hypo5_high_trust_coefficients.png",
  p_h5,
  width = 13,
  height = 11
)