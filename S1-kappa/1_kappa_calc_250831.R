# Install the 'irr' package
# install.packages("irr")
# install.packages("ggplot2")

# Load the 'irr' package
library(irr)
library(ggplot2)
library(patchwork)
library(dplyr)

(wd <- getwd())
ekman_df <- read.csv( paste0(wd,"/S1-kappa/Ekman-evaluators-data.csv"), fileEncoding = "UTF-8")
emotions <- c("anger", "disgust", "fear", "happiness", "sadness", "surprise")

ev_gr_1 = ekman_df[ekman_df$eval_group == 1 ,]
ev_gr_2 = ekman_df[ekman_df$eval_group == 2 ,]
ev_gr_3 = ekman_df[ekman_df$eval_group == 3 ,]
ev_gr_4 = ekman_df[ekman_df$eval_group == 4 ,]
ev_gr_5 = ekman_df[ekman_df$eval_group == 5 ,]
ev_gr_6 = ekman_df[ekman_df$eval_group == 6 ,]

generate_evaluator_vectors <- function(ev_group_d, emotions, evalN){
  # iterateN represents the interviewee number.
  iterateN <- (nrow(ev_group_d) / evalN )
  vecs <- matrix(0, nrow = iterateN, ncol = evalN * length(emotions))
  #print(dim(vecs))
  #print(head(ev_group_d))
  #em_count = 0
  for(i_em in seq_along(emotions) ){
    for (v in 1:iterateN) {
      for(ev in 1:evalN){
        #print(paste(v, ev + (i_em - 1) * evalN))
        #print(paste((v-1)*evalN + ev, emotions[i_em]))
        vecs[v, ev + (i_em - 1) * evalN] <- ev_group_d[ (v-1)*evalN + ev, emotions[i_em]]
        #print(paste("i_em:", i_em, "ite:", v, "ev", ev) )
        #print(ev_group_d[v + ev - 1,])
      }
    }
    #em_count <- em_count + 1
  }
  print(head(vecs))
  return(vecs)
}

v1 <- generate_evaluator_vectors(ev_gr_1, emotions, 2)
v2 <- generate_evaluator_vectors(ev_gr_2, emotions, 2)
v3 <- generate_evaluator_vectors(ev_gr_3, emotions, 2)
v4 <- generate_evaluator_vectors(ev_gr_4, emotions, 2)
v5 <- generate_evaluator_vectors(ev_gr_5, emotions, 3)
v6 <- generate_evaluator_vectors(ev_gr_6, emotions, 2)

v_list <- list(v1, v2, v3, v4, v5, v6)
for (i in seq_along(v_list)) {
  print(v_list[[i]])
  #write.csv(v_list[[i]], paste0(wd,"/S1-kappa", "/0-ev_gr_", i, "rates_each_emotion.csv"), row.names = TRUE)
}


# Kappa2
initialize_result_df_kappa <- function(){
  # initial dataframe
  result_df_kappa <- data.frame(
    ev_g = integer(0),
    emotion = character(0),
    subjects = integer(0),
    raters = integer(0),
    kappa = numeric(0),
    z = numeric(0),
    p = numeric(0)
  )
  return(result_df_kappa)
}
# Calculate Cohen's Kappa for 2 raters
calc_kappa2 <- function(matrix, result, colN, ev_g){
  colN2 <- (colN -1)*2 + 1
  #print(matrix[,colN2:(colN2+1)])
  #str(matrix)
  # 0–4 の中点で区切って丸める例（しきい値は事前に固定しておく）
  cuts <- c(-Inf, 0.5, 1.5, 2.5, 3.5, Inf)
  print("r1")
  print(matrix[,colN2])
  r1_cat <- cut(matrix[,colN2], breaks = cuts, labels = FALSE, right = FALSE) - 1
  #r1_cat <- matrix[,colN2]
  print(r1_cat)
  
  print("r2")
  print(matrix[,(colN2+1)])
  r2_cat <- cut(matrix[,(colN2+1)], breaks = cuts, labels = FALSE, right = FALSE) - 1
  #r2_cat <- matrix[,(colN2+1)]
  print(r2_cat)
  
  kappa2_result <- kappa2( cbind(r1_cat, r2_cat), weight = "squared")
  print(kappa2_result)
  
  kappa_or_na <- kappa2_result$value
  if(kappa_or_na == 0){
    kappa_or_na = NA
  }
  
  new_kappa <- data.frame(
    ev_g = ev_g,
    emotion = emotions[colN],
    subjects = kappa2_result$subjects,
    raters = kappa2_result$raters,
    kappa = kappa_or_na,#kappa2_result$value,
    z = kappa2_result$statistic,
    p = kappa2_result$p.value
  )
  result <- rbind(result, new_kappa)
  # Display the result
  
  return(result)
}
# Calculate Leight's Chohen's Kappa for 3 raters
calc_leight_kappa <- function(mat, result, colN, ev_g){
  # Calculate leight's Kappa
  i0   <- (colN - 1) * 3 + 1
  cols <- i0:(i0 + 2)
  
  blk  <- mat[, cols, drop = FALSE]
  cuts <- c(-Inf, 0.5, 1.5, 2.5, 3.5, Inf)
  
  r1 <- cut(mat[,i0 + 0], breaks = cuts, labels = FALSE, right = FALSE) - 1
  r2 <- cut(mat[,i0 + 1], breaks = cuts, labels = FALSE, right = FALSE) - 1
  r3 <- cut(mat[,i0 + 2], breaks = cuts, labels = FALSE, right = FALSE) - 1
  print(cbind(mat[,i0 + 0], r1, mat[,i0 + 1], r2, mat[,i0 + 2], r3))  
  
  k2_r12 <- kappa2( cbind(r1, r2), weight = "squared")
  k2_r23 <- kappa2( cbind(r2, r3), weight = "squared")
  k2_r31 <- kappa2( cbind(r3, r1), weight = "squared")
  print(list(k2_r12,k2_r23,k2_r31))  
  
  if(k2_r12$value == 0){
    k2_r12$value = NA
  }
  if(k2_r23$value == 0){
    k2_r23$value = NA
  }
  if(k2_r31$value == 0){
    k2_r31$value = NA
  }
  
  mean_kappa = mean( c(k2_r12$value, k2_r23$value, k2_r31$value), na.rm = TRUE )
  mean_statistic = mean( c(k2_r12$statistic, k2_r23$statistic, k2_r31$statistic) )
  mean_pvalue = mean( c(k2_r12$p.value, k2_r23$p.value, k2_r31$p.value) )
  
  new_kappa <- data.frame(
    ev_g = ev_g,
    emotion = emotions[colN],
    subjects = k2_r12$subjects,
    raters = k2_r12$raters,
    kappa = mean_kappa,
    z = mean_statistic,
    p = mean_pvalue
  )
  result <- rbind(result, new_kappa)
  
  return(result)
}

result_df_kappa <- initialize_result_df_kappa()
#v_list <- list(v1)
v_list <- list(v1, v2, v3, v4)
#v_list <- list(v6)#, v2, v3, v4)
# evaluator group 1-4, 2 raters
for (i in seq_along(v_list)) {
  for(eN in 1:length(emotions)){
    print(emotions[eN])
    result_df_kappa <- calc_kappa2(v_list[[i]], result_df_kappa, eN, i)
    #print(result_df_kappa)
    #write.csv(result_df_kappa, paste0("kappa-",emotions[eN],".csv"), row.names = TRUE)
  }
}
# evaluator group 5, 3 raters
for(eN in 1:length(emotions)){
  print(emotions[eN])
  result_df_kappa <- calc_leight_kappa(v5, result_df_kappa, eN, 5)
  print(result_df_kappa)
  #write.csv(result_df_kappa, paste0("kappa-",emotions[eN],".csv"), row.names = TRUE)
}
# evaluator group 6, 2 raters
for(eN in 1:length(emotions)){
  print(emotions[eN])
  result_df_kappa <- calc_kappa2(v6, result_df_kappa, eN, 6)
  print(result_df_kappa)
  #write.csv(result_df_kappa, paste0("kappa-",emotions[eN],".csv"), row.names = TRUE)
}

#write.csv(result_df_kappa, paste0(wd, "/S1-kappa/", "1_1-kappa.csv"), row.names = TRUE)
result_df_kappa


# coefficient mean per emotion -----------------------------------------------

kappa_summary_df <- result_df_kappa %>%
  filter(!is.nan(z)) %>%
  group_by(emotion) %>%  
  summarise(
    mean_kappa = mean(kappa, na.rm = TRUE),
    se_kappa   = sd(kappa, na.rm = TRUE) / sqrt(sum(!is.na(kappa))),
    sd_kappa   = sd(kappa, na.rm = TRUE),
    n = n()
  )
#write.csv(kappa_summary_df, paste0(wd, "/S1-kappa/", "1_2-kappa-means.csv"), row.names = TRUE)
kappa_summary_df

ggplot(kappa_summary_df, aes(x = emotion, y = mean_kappa)) +
  geom_col(fill = "skyblue", width = 0.6) +                         # bars
  geom_errorbar(aes(ymin = mean_kappa - sd_kappa,
                    ymax = mean_kappa + sd_kappa),
                width = 0.2) +                                      # error bar SDへ
  geom_jitter(data = result_df_kappa %>% filter(!is.nan(z)),        # dots
              aes(x = emotion, y = kappa),
              width = 0.15, alpha = 0.6, color = "black") +
  theme_minimal() +
  labs(y = expression("Mean quadratic weighted " * kappa), x = "Emotions") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))  +
  theme(
    axis.text = element_text(size = 14),      # 目盛の文字サイズ
    axis.title = element_text(size = 14),      # 軸ラベルの文字サイズ
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)   # x軸ラベルを縦向きに
    
  )

# ggsave( paste0(wd,"/S1-kappa/", "1-5_mean_cohens_kappa_qwk_sd.png"), plot = last_plot(),
#        width = 7.48/2, height = 7.28/2, units = "in", dpi = 1000, 
#        bg = "white", , device = ragg::agg_png)



# group 3 anger 0, NaN
#         disgust 0, 0
#         fear    0, NaN
#         happiness 
#         sadness
#         surprise 0, NaN

# group 4 surprise NA, NA
# group 5 surprise -0.03, NaN
# group 5 surprise eID=9:0 only,evID=10:2,1,...,evID=0,0,..1,0

