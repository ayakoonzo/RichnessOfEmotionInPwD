#install.packages("dplyr")
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

(wd <- getwd())
dir <- "/S3-BERT-model/"

#######
thk8e_df <- read.csv( paste0(wd,dir,"aggregated_tohoku_wrime_8emo_n86.csv"), fileEncoding = "UTF-8")

EMOS <- c('score_anger', 'score_disgust', 'score_fear', 
         'score_joy', 'score_sadness', 'score_surprise', 
         'score_trust', 'score_anticipation')

colMeans(thk8e_df[, EMOS], na.rm = TRUE)

# 追加した2感情を含む希望順（EMOSの並び）を使う
order_levels <- sub("^score_", "", EMOS)

sumdf <- thk8e_df |>
  select(all_of(EMOS)) |>
  pivot_longer(everything(), names_to = "emo", values_to = "score") |>
  group_by(emo) |>
  summarise(mean = mean(score, na.rm = TRUE),
            sd   = sd(score,   na.rm = TRUE),
            n    = sum(!is.na(score)),
            .groups = "drop") |>
  mutate(
    se  = sd / sqrt(n),
    emo = sub("^score_", "", emo),
    emo = factor(emo, levels = order_levels)  # ← 並び順を固定
  )

#######

### average ratings for each participant
ekman_df <- read.csv(paste0(wd,dir,"Ekman-evaluators-data-n86.csv"), fileEncoding = "UTF-8")
IDs <- sort(unique(ekman_df$interviewID))

rater_ave_df <- data.frame(
  id = integer(0),
  anger = numeric(0),
  disgust = numeric(0),
  fear = numeric(0),
  happiness = numeric(0),
  sadness = numeric(0),
  surprise = numeric(0)
)

for(id in IDs){
  #print(id)
  d <- ekman_df[ekman_df$interviewID == id,]
  #print(d)
  ms <- data.frame(
    id = id,
    anger = mean(d$anger), 
    disgust = mean(d$disgust), 
    fear = mean(d$fear), 
    happiness = mean(d$happiness),
    sadness = mean(d$sadness),
    surprise = mean(d$surprise) )
  print(ms)
  rater_ave_df <- rbind(rater_ave_df, ms)
}

# 感情の順番
EMOS2 <- c("anger","disgust","fear","happiness","sadness","surprise")

# ロング化
emo_long <- rater_ave_df %>%
  pivot_longer(cols = all_of(EMOS2), names_to = "emo", values_to = "score") %>%
  mutate(emo = factor(emo, levels = EMOS2))

# 平均とSEを計算
sumdf <- emo_long %>%
  group_by(emo) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd   = sd(score, na.rm = TRUE),
    n    = sum(!is.na(score)),
    .groups = "drop"
  ) %>%
  mutate(se = sd / sqrt(n))

################

# 比較する6感情（順序固定）
order_levels <- c("anger","disgust","fear","happiness","sadness","surprise")
EMOS6_BERT   <- paste0("score_", c("anger","disgust","fear","joy","sadness","surprise"))

# 1) BERT（trust/anticipation 除外）
bert_long <- thk8e_df %>%
  select(all_of(EMOS6_BERT)) %>%
  pivot_longer(everything(), names_to = "emo", values_to = "score") %>%
  mutate(
    emo = sub("^score_", "", emo),
    emo = ifelse(emo == "joy", "happiness", emo),  # ラベル整合
    source = "BERT"
  )

# 2) Human（0–4 → 0–1 に正規化）
human_long <- rater_ave_df %>%
  pivot_longer(cols = all_of(order_levels), names_to = "emo", values_to = "score") %>%
  mutate(
    score  = score / 4,
    source = "Human"
  )

# 3) 結合＆因子レベル
combined <- bind_rows(bert_long, human_long) %>%
  filter(emo %in% order_levels) %>%
  mutate(
    emo    = factor(emo, levels = order_levels),
    source = factor(source, levels = c("BERT","Human"))
  )

# 4) 代表値（平均±SE）
sum2 <- combined %>%
  group_by(source, emo) %>%
  summarise(
    mean = mean(score, na.rm = TRUE),
    sd   = sd(score,   na.rm = TRUE),
    n    = sum(!is.na(score)),
    .groups = "drop"
  ) %>%
  mutate(se = sd / sqrt(n))


# 5) 棒グラフ（平均±SE）
# ggplot(sum2, aes(x = emo, y = mean, fill = source)) +
#   geom_col(position = position_dodge(width = 0.8)) +
#   geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = pmin(mean + se, 1)),
#                 position = position_dodge(width = 0.8), width = 0.2) +
#   coord_cartesian(ylim = c(0, 1)) +
#   labs(x = "Emotion", y = "Mean ± SE (0–1, Human normalized)") +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))



###  86 interviews scatter plot for 6 emotions

thk8e_df
rater_ave_df

order_levels <- c("anger","disgust","fear","happiness","sadness","surprise")
EMOS6_BERT   <- paste0("score_", c("anger","disgust","fear","joy","sadness","surprise"))

r_6emo <- data.frame(
  emotion = character(0),
  r = numeric(0),
  pval = numeric(0)
)

for (i in 1:6){
  h_e <- order_levels[[i]]
  b_e <- EMOS6_BERT[[i]]
  ct <- stats::cor.test(rater_ave_df[[ h_e ]], thk8e_df[[ b_e ]], 
                        method = "spearman", exact = FALSE)
  print(ct)
  print(unname(ct$estimate)[[1]])
  r <- data.frame(
    emotion = h_e,
    r = unname(ct$estimate),
    pval = ct$p.value
  )
  print(r)
  r_6emo <- rbind(r_6emo, r)
}
r_6emo
#write.csv(r_6emo, paste0(wd,dir,"5_human-BERT-6emo-correlations.csv"), row.names = TRUE)

plot_cor_emotion <- function(human, bert, emo_label,
                             method = "spearman", r_digits = 3, p_digits = 3,
                             anc_x = 0.5, anc_y = 0.8) {
  stopifnot(length(human) == length(bert))
  df <- data.frame(human = human, bert = bert)
  
  # 相関計算
  ct <- stats::cor.test(df$human, df$bert, method = method)
  r_val <- round(unname(ct$estimate), r_digits)
  p_raw <- ct$p.value
  
  # p値を必ず小数で表示
  if (p_raw < 0.001) {
    p_txt <- "< 0.001"
  } else {
    p_txt <- formatC(p_raw, format = "f", digits = p_digits)
    p_txt <- paste0("= ", p_txt)
  }
  
  # 表示テキスト
  anno <- sprintf("\u03C1 = %.3f\np %s", r_val, p_txt)
  
  # 注記位置（プロット範囲の右上あたり）
  xr <- range(df$human, na.rm = TRUE)
  print(diff(xr))
  yr <- range(df$bert, na.rm = TRUE)
  x_anno <- xr[1] + anc_x * diff(xr)   # 右寄り
  y_anno <- yr[1] + anc_y * diff(yr)   # 上寄り
  
  # プロット
  p <- ggplot2::ggplot(df, ggplot2::aes(x = human, y = bert)) +
    ggplot2::geom_point(size = 1, color = "steelblue") +   # 点を小さく
    ggplot2::geom_smooth(method = "lm", se = FALSE, linetype = "solid", color = "red") +
    ggplot2::annotate("text", x = x_anno, y = y_anno, label = anno,
                      hjust = 0, size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Human", y = "BERT", title = paste0("<", emo_label, ">") ) +
    ggplot2::scale_x_continuous(limits = c(0, 4)) +
    #ggplot2::scale_y_continuous(limits = c(0, 0.4)) +
    theme(
      axis.text = element_text(size = 12),      # 目盛の文字サイズ
      axis.title = element_text(size = 12),      # 軸ラベルの文字サイズ
      #axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)   # x軸ラベルを縦向きに
    )
  
  return(list(plot = p, cor_test = ct))
}

res1 <- plot_cor_emotion(rater_ave_df$anger, thk8e_df$score_anger, "anger", anc_x = 0.85, anc_y = 0.9)
res2 <- plot_cor_emotion(rater_ave_df$disgust, thk8e_df$score_disgust, "disgust", anc_x = 1.0, anc_y = 0.2)
res3 <- plot_cor_emotion(rater_ave_df$fear, thk8e_df$score_fear, "fear", anc_x = 1.0, anc_y = 0.8)
res4 <- plot_cor_emotion(rater_ave_df$happiness, thk8e_df$score_joy, "happiness(joy)",  anc_x = 0.6, anc_y = 0.92)
res5 <- plot_cor_emotion(rater_ave_df$sadness, thk8e_df$score_sadness, "sadness", anc_x = 0.86, anc_y = 0.88)
res6 <- plot_cor_emotion(rater_ave_df$surprise, thk8e_df$score_surprise, "surprise", anc_x = 1.2, anc_y = 0.9)

p <- (res1$plot | res2$plot | res3$plot) /
  (res4$plot | res5$plot | res6$plot) 
p
# ggsave( paste0(wd,dir,"5_hmn-bert_6emo_scatter_3_spearman_lm.png"), 
#        plot = p,
#        width = 7.48, height = 5.40, units = "in", dpi = 1000, 
#        bg = "white", device = ragg::agg_png)

