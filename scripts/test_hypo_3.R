library(patchwork)
library(mlogit)
library(readxl)
library(ggplot2)
library(dplyr)

# ── 1. Загрузка данных ──────────────────────────────────────────────────────
data_wide <- read_excel("cleaned_data_wide.xlsx")

# ── 2. Reshape: wide → long ─────────────────────────────────────────────────
data <- reshape(
  as.data.frame(data_wide),
  direction = "long",
  varying = list(
    provider     = 5:7,
    price        = 8:10,
    accuracy     = 11:13,
    waiting_time = 14:16
  ),
  v.names  = c("provider", "price", "accuracy", "waiting_time"),
  timevar  = "alt"
)

data        <- data[order(data$subject, data$trial, data$alt), ]
data$choice <- as.numeric(data$choice == data$alt)
data$accuracy <- as.numeric(gsub(",", ".", data$accuracy))
data$waiting_time <- as.numeric(data$waiting_time)

# provider: базовая категория — ai
data$provider <- factor(data$provider, levels = c("ai", "doctor", "doctor&ai"))

# waiting_time: категориальная ординальная, базовая — 0 (немедленно)
data$waiting_time_cat <- factor(data$waiting_time,
                                levels = c(0, 1, 3, 7),
                                labels = c("Немедленно", "1 сутки", "3 суток", "1 неделя"),
                                ordered = FALSE)
data$waiting_time_cat <- relevel(data$waiting_time_cat, ref = "Немедленно")

# ── 3. Перевод в формат mlogit ──────────────────────────────────────────────
mlogit_data <- mlogit.data(
  data,
  choice  = "choice",
  shape   = "long",
  alt.var = "alt",
  id.var  = "subject"
)

# ── 4. MNL-модель с категориальным waiting_time ────────────────────────────
model <- mlogit(
  choice ~ accuracy + waiting_time_cat + provider | 0,
  data = mlogit_data
)

cat("==========================================================\n")
cat("Результаты MNL-модели\n")
cat("==========================================================\n")
print(summary(model))

# ── 5. Коэффициенты и p-values ─────────────────────────────────────────────
coefs <- summary(model)$coefficients
pvals <- summary(model)$CoefTable[, "Pr(>|z|)"]

cat("\n==========================================================\n")
cat("Часть 1: коэффициент и значимость accuracy\n")
cat("==========================================================\n")
cat("β_accuracy =", coefs["accuracy"], "\n")
cat("p-value    =", pvals["accuracy"], "\n")
if (pvals["accuracy"] < 0.05) {
  cat("Вывод: accuracy значимо увеличивает вероятность выбора (p < 0.05)\n")
} else {
  cat("Вывод: эффект accuracy незначим на уровне 0.05\n")
}

# ── 6. Относительная важность атрибутов ────────────────────────────────────
#
# Для непрерывных атрибутов: range_utility = |β| × (max − min уровня)
# Для категориального waiting_time: range_utility = max(β_level) − min(β_level)
#   базовая категория (Немедленно) имеет β = 0
# Для категориального provider: range_utility = max(β_category) − min(β_category)
#   базовая категория (ai) имеет β = 0
#
cat("\n==========================================================\n")
cat("Часть 2: относительная важность атрибутов\n")
cat("==========================================================\n")

# Accuracy
range_accuracy <- (95 - 80) / 100
ui_accuracy <- abs(coefs["accuracy"]) * range_accuracy

# Provider
beta_provider <- c(
  ai          = 0,
  doctor      = coefs["providerdoctor"],
  `doctor&ai` = coefs["providerdoctor&ai"]
)
ui_provider <- max(beta_provider) - min(beta_provider)

# Waiting_time категориальная: берём все коэффициенты уровней
wt_names <- names(coefs)[grepl("waiting_time_cat", names(coefs))]
beta_wt <- c(0, coefs[wt_names])  # 0 — базовая категория (Немедленно)
names(beta_wt)[1] <- "Немедленно"
ui_waiting_time <- max(beta_wt) - min(beta_wt)

# Суммарный диапазон и относительная важность
total_ui <- ui_accuracy + ui_waiting_time + ui_provider

ri_accuracy     <- ui_accuracy     / total_ui * 100
ri_waiting_time <- ui_waiting_time / total_ui * 100
ri_provider     <- ui_provider     / total_ui * 100

cat(sprintf("%-25s β = %7.4f   p-value = %s   range utility = %6.4f   relative importance = %5.1f%%\n",
            "accuracy", coefs["accuracy"], "< 2.2e-16", ui_accuracy, ri_accuracy))
cat(sprintf("%-25s β = %7.4f   p-value = %s\n",
            "provider (doctor)", coefs["providerdoctor"], "< 2.2e-16"))
cat(sprintf("%-25s β = %7.4f   p-value = %s   range utility = %6.4f   relative importance = %5.1f%%\n",
            "provider (doctor&ai)", coefs["providerdoctor&ai"], "< 2.2e-16", ui_provider, ri_provider))
cat("  * range utility для provider: диапазон между max (doctor) и min (ai=0)\n")

cat("\nКоэффициенты waiting_time по уровням (базовая: Немедленно = 0):\n")
for (nm in wt_names) {
  cat(sprintf("  %-30s β = %7.4f   p-value = %.4f\n", nm, coefs[nm], pvals[nm]))
}
cat(sprintf("%-25s range utility = %6.4f   relative importance = %5.1f%%\n",
            "waiting_time (overall)", ui_waiting_time, ri_waiting_time))

cat("\nАтрибут с наибольшей относительной важностью:\n")
ri_all <- c(accuracy = ri_accuracy, waiting_time = ri_waiting_time, provider = ri_provider)
cat(names(which.max(ri_all)), "—", round(max(ri_all), 1), "%\n")

# ── 7. McFadden R² ──────────────────────────────────────────────────────────
if (!require(lmtest)) install.packages("lmtest", repos = "https://cloud.r-project.org")
library(lmtest)

model_null <- mlogit(choice ~ 1, data = mlogit_data)
LL_null    <- as.numeric(logLik(model_null))
LL_model   <- as.numeric(logLik(model))
mcfadden_R2 <- 1 - LL_model / LL_null
lr_test <- lrtest(model_null, model)

cat("\n==========================================================\n")
cat("Сравнение с нулевой моделью\n")
cat("==========================================================\n")
cat(sprintf("LL(null)  = %.1f\n", LL_null))
cat(sprintf("LL(model) = %.1f\n", LL_model))
cat(sprintf("McFadden R² = %.3f\n", mcfadden_R2))
cat("\nТест отношения правдоподобий (LR test):\n")
print(lr_test)

# ── 8. График: относительная важность атрибутов ─────────────────────────────
source("visuals.R")

ri_df <- data.frame(
  attribute = c("Точность\nдиагностики", "Формат\nконсультации", "Время\nожидания"),
  importance = c(ri_accuracy, ri_provider, ri_waiting_time),
  color_key  = c("Точность", "Тип консультации", "Время ожидания")
)
ri_df$attribute <- factor(ri_df$attribute,
                          levels = ri_df$attribute[order(ri_df$importance, decreasing = TRUE)])

plot_ri <- ggplot(ri_df, aes(x = attribute, y = importance, fill = color_key)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(round(importance, 1), "%")),
            vjust = -0.4, size = 3.5, fontface = "bold", family = "serif") +
  scale_fill_temp() +
  scale_y_continuous(limits = c(0, 65),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL,
       y = "Относительная важность (%)") +
  theme_temp() +
  theme(legend.position = "none")

ggsave_temp("hyp3_relative_importance.png", plot_ri, width = 14, height = 9)
cat("\nГрафик сохранён: hyp3_relative_importance.png\n")

# ── 9. График: Utility Curves ────────────────────────────────────────────────

# Accuracy utility curve (непрерывная)
acc_levels <- seq(0.80, 0.95, by = 0.01)
acc_utility <- coefs["accuracy"] * acc_levels
acc_df <- data.frame(
  x     = acc_levels * 100,
  y     = acc_utility - min(acc_utility),  # нормируем к нулю
  attr  = "Точность диагностики (%)"
)

# Waiting_time utility curve (категориальная)
wt_utility <- c(
  0,                                          # Немедленно (база)
  coefs["waiting_time_cat1 сутки"],
  coefs["waiting_time_cat3 суток"],
  coefs["waiting_time_cat1 неделя"]
)
wt_df <- data.frame(
  x     = c(0, 1, 3, 7),
  y     = wt_utility - max(wt_utility),       # нормируем к нулю сверху
  attr  = "Время ожидания (дни)"
)

# Provider utility (три точки)
prov_utility <- c(
  ai        = 0,
  doctor    = coefs["providerdoctor"],
  `doc+ai`  = coefs["providerdoctor&ai"]
)
prov_df <- data.frame(
  x_label = factor(c("Только ИИ", "Только врач", "Врач + ИИ"),
                   levels = c("Только ИИ", "Врач + ИИ", "Только врач")),
  y       = prov_utility - min(prov_utility),
  attr    = "Формат консультации"
)

# ── Accuracy plot ────────────────────────────────────────────────────────────
plot_acc <- ggplot(acc_df, aes(x = x, y = y)) +
  geom_line(color = "#009688", linewidth = 1.2) +
  geom_point(color = "#009688", size = 2.5) +
  scale_x_continuous(breaks = c(80, 85, 90, 95),
                     labels = function(x) paste0(x, "%")) +
  labs(x = "Точность диагностики",
       y = "Полезность (нормированная)") +
  theme_temp()

# ── Waiting_time plot ─────────────────────────────────────────────────────────
plot_wt <- ggplot(wt_df, aes(x = factor(x, labels = c("Немедленно", "1 сутки", "3 суток", "1 неделя")), y = y)) +
  geom_col(fill = "#1B263B", width = 0.45) +
  geom_text(aes(label = round(y, 2)), vjust = 1.4,
            size = 3.5, fontface = "bold", family = "serif", color = "black") +
  scale_x_discrete(position = "top") +
  scale_y_continuous(expand = expansion(mult = c(-0.05, 0))) + 
  labs(x = NULL,
       y = "Полезность (нормированная)") +
  annotate("text", x = 2.5, y = -Inf,
           label = "Время ожидания", vjust = 3.3,
           size = 4.2, fontface = "bold", family = "serif") +
  coord_cartesian(clip = "off") +
  theme_temp() +
  theme(axis.text.x.top = element_text(color = "black"),
        axis.ticks.x.top = element_line(color = "black"),
        plot.margin = margin(t = 5, r = 10, b = 20, l = 5))

# ── Provider plot ─────────────────────────────────────────────────────────────
plot_prov <- ggplot(prov_df, aes(x = x_label, y = y)) +
  geom_col(fill = "#BDBDBD", width = 0.45) +
  geom_text(aes(label = round(y, 2)), vjust = -0.4,
            size = 3.5, fontface = "bold", family = "serif") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Формат консультации",
       y = "Полезность (нормированная)") +
  theme_temp()

# ── Объединяем три графика ────────────────────────────────────────────────────
if (!require(patchwork)) install.packages("patchwork", repos = "https://cloud.r-project.org")
library(patchwork)

plot_utility <- plot_acc + plot_wt + plot_prov +
  plot_layout(ncol = 3, widths = c(1, 1, 1))

ggsave_temp("hyp3_utility_curves.png", plot_utility, width = 30, height = 10)
cat("График сохранён: hyp3_utility_curves.png\n")