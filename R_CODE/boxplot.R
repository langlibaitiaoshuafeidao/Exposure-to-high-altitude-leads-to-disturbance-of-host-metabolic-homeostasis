#1. 加载包
#本部分分析过程涉及的R包
library(survival)
library(ggplot2)
library(ggpubr)
#2. 导入数据
data <- read.csv("GENUS.csv")
str(data)
#3. 箱式图分析R语言代码
# 多组指标两两比较
co <- combn(c(unique(data[["group"]])), 2, simplify = FALSE)
ck <- lapply(co, function(x) {
  x_names <- as.character(x)
  new_vector <- c(x_names[1], x_names[2])
  return(new_vector)
})
ggboxplot(data, "group", "Parabacteroides", add = "none", rug = TRUE, color = "group", fill = "group", alpha = 0.5, width = 0.4, palette = "aaas", size = 0.3) + get("theme_pubr")() + stat_compare_means(method = "t.test", label = "p.format", comparisons = ck) + stat_compare_means(method = "anova", label = "p.format", label.x.npc = "center", labl.y = max(data[["Parabacteroides"]]) * 1.4)
