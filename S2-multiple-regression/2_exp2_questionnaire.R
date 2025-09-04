library(ggplot2)

(wd <- getwd())
df <- read.csv(paste0(wd, "/S2-multiple-regression/", "2_exp2_questionnaire.csv"), fileEncoding = "UTF-8")

# 重回帰 multiple regression

# person with Dimentia
# sink( paste0(wd, "/S2-multiple-regression/", "2-2_PersonWithDimentia.txt") )
pwd_r2 <- numeric()
for( em in 1:6){
  print(qs[em + 1])
  formula_str <- paste(qs[em + 1], "~ age + gender + Q4_privateD + Q5_jobD")
  model <- lm(as.formula(formula_str), data = df)
  smr <- summary(model)
  print(smr)
  #print(smr$r.squared)
  pwd_r2 <- c(pwd_r2, smr$r.squared)
}
print("Multiple R-squared")
print(pwd_r2)
# sink()

#myself with Dimentia
# sink( paste0(wd, "/S2-multiple-regression/", "2-2_MyselfWithDimentia.txt") )
mwd_r2 <- numeric()
for( em in 1:6){
  colN <- em + 1 + 6
  print(qs[colN])
  formula_str <- paste(qs[colN], "~ age + gender + Q4_privateD + Q5_jobD")
  model <- lm(as.formula(formula_str), data = df)
  smr <- summary(model)
  print(smr)
  #print(smr$r.squared)
  mwd_r2 <- c(mwd_r2, smr$r.squared)
}
print("Multiple R-squared")
print(mwd_r2)
# sink()

#others without Dimentia
# sink( paste0(wd, "/S2-multiple-regression/", "2-2_OthersWithoutDimentia.txt") )
wod_r2 <- numeric()
for( em in 1:6){
  colN <- em + 1 + 6*2
  print(qs[colN])
  formula_str <- paste(qs[colN], "~ age + gender + Q4_privateD + Q5_jobD")
  model <- lm(as.formula(formula_str), data = df)
  smr <- summary(model)
  print(smr)
  #print(smr$r.squared)
  wod_r2 <- c(wod_r2, smr$r.squared)
}
print("Multiple R-squared")
print(wod_r2)
# sink()



# まとめてデータフレームに
q123_df <- data.frame(
  value = c(pwd_r2, mwd_r2, wod_r2),
  group = rep(c("PwD", "SwD", "PwoD"), each = 6)
)
q123_df$group <- factor(q123_df$group, levels = c("PwD", "SwD", "PwoD"))

# write.csv(q123_df, paste0(wd, "/S2-multiple-regression/", "2-3_q123_df.csv"), row.names = TRUE)

# 平均と標準誤差を計算
summary_df <- q123_df %>%
  group_by(group) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    se = sd(value) / sqrt(n())
  )


# 棒グラフ＋エラーバー（SDバージョン）
ggplot(summary_df, aes(x = group, y = mean, fill = group)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                width = 0.2, linewidth = 0.8) +
  geom_jitter(data = q123_df, aes(x = group, y = value),
              width = 0.1, size = 1.0, alpha = 0.7, color = "gray") +
  theme_minimal() +
  labs(x = "Appraisal of emotions", y = "Mean R²") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_discrete(guide = FALSE) +  
  theme(
    axis.text = element_text(size = 12),      # 目盛の文字サイズ
    axis.title = element_text(size = 12),      # 軸ラベルの文字サイズ
    #axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)   # x軸ラベルを縦向きに
  )

ggsave( paste0(wd, "/S2-multiple-regression/","2_lmr_perspective_r2_SD.png"), 
       plot = last_plot(),
       width = 7.48/2, height = 7.28/2, units = "in", dpi = 1000, 
       bg = "white", device = ragg::agg_png)
