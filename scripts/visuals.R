library(ggplot2)
library(scales)
library(dplyr)

# Цвета
colors <- c(
  "ИИ"            = "#009688", 
  "Доктор"        = "#1B263B", 
  "Доктор и ИИ"   = "#BDBDBD", 
  "Low"           = "#BDBDBD",
  "High"          = "#009688",
  "Высокий доход" = "#009688",
  "Низкий доход"  = "#BDBDBD",
  "Точность"        = "#009688", 
  "Время ожидания"  = "#1B263B", 
  "Тип консультации" = "#BDBDBD"
)

# Метки
labels <- c(
  "Price_Group"  = "Ценовая группа",
  "Income_Group" = "Уровень дохода",
  "AI_Solo"      = "Выбор только ИИ",
  "Choice"       = "Тип консультации",
  "accuracy"     = "Точность диагностики (%)",
  "waiting_time" = "Время ожидания (дни)",
  "price"        = "Стоимость (руб.)"
)

# Функции округления
format_num  <- function(x) sprintf("%.2f", x)
format_perc <- function(x) paste0(sprintf("%.2f", x * 100), "%")

# Тема
theme_temp <- function(base_size = 12) {
  theme_classic(base_size = base_size, base_family = "serif") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line        = element_line(color = "black", linewidth = 0.5),
      axis.ticks       = element_line(color = "black"),
      axis.text        = element_text(color = "black", size = base_size - 1),
      axis.title       = element_text(color = "black", size = base_size, face = "bold"),
      plot.title       = element_blank(),
      plot.subtitle    = element_blank(),
      plot.caption     = element_blank(),
      plot.margin      = margin(t = 5, r = 10, b = 5, l = 5),
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(size = base_size - 1),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.key.size   = unit(0.35, "cm"),
      legend.key.width  = unit(0.35, "cm"),
      legend.key.height = unit(0.35, "cm")
    )
}
theme_set(theme_temp())

# Шкалы
scale_fill_temp  <- function(...) scale_fill_manual(values = colors, ...)
scale_color_temp <- function(...) scale_color_manual(values = colors, ...)

# Функция сохранения
ggsave_temp <- function(filename, plot, width = 16, height = 10) {
  ggsave(filename = filename, plot = plot, width = width, height = height, 
         units = "cm", dpi = 300, bg = "white")
}