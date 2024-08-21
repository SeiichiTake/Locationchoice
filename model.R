library(survival)
library(broom)

# 必要な列だけを抜き出す
data <- read.csv('Data/data.csv')
data <- data %>% select(vessel_day_id, year, month, area, samani_km, num_ves, predicted_rev, 平均風速.m.s., 平均気温..., 降水量の合計.mm., L1choice, suke, 漁区中央水深, revenue_suke)
data$choice <- ifelse(is.na(data$suke), 0, 1)

# 新しいデータフレームを作成
data <- rename(data, ave_wind = 平均風速.m.s., ave_temp = 平均気温..., rain = 降水量の合計.mm., id = vessel_day_id, depth = 漁区中央水深, distance = samani_km)
choice_data <- data %>% select(id, choice, area, distance, num_ves, L1choice, predicted_rev, ave_wind, ave_temp, rain, depth)

data_analysis <- data %>% select(id, choice, area, distance, num_ves, L1choice, predicted_rev, ave_wind, ave_temp, rain, depth, revenue_suke)
data_analysis <- na.omit(data_analysis)

# num_vesの値で場合分けしてモデルを集計する
df_1 <- data_analysis %>% filter(num_ves == 1)
df_2 <- data_analysis %>% filter(num_ves == 2)
df_3 <- data_analysis %>% filter(num_ves == 3)
hist(df_1$area, breaks = seq(135, 162, by = 1))
hist(df_2$area, breaks = seq(135, 162, by = 1))
hist(df_3$area, breaks = seq(135, 162, by = 1))

choice_data <- na.omit(choice_data)

# num_vesの値で場合分けしてモデルを集計する
data_1 <- choice_data %>% filter(num_ves == 1)
data_2 <- choice_data %>% filter(num_ves == 2)
data_3 <- choice_data %>% filter(num_ves == 3)

# 同じ日に日高地区で操業した漁船数が自分の漁船だけの時
model_1 <- clogit(choice ~ distance + L1choice + predicted_rev + strata(id), data = data_1)
summary(model_1)
# McFaddenの疑似決定係数 (Psuedo R-squared) の計算と出力
pseudo_r2 <- 1 - (model_1$loglik[2] / model_1$loglik[1])
cat("McFadden's Pseudo R-squared:", pseudo_r2, "\n")

# 同じ日に日高地区で操業した漁船数が自分の漁船＋１の時
model_2 <- clogit(choice ~ distance + L1choice + predicted_rev + strata(id), data = data_2)
summary(model_2)
# McFaddenの疑似決定係数 (Psuedo R-squared) の計算と出力
pseudo_r2 <- 1 - (model_2$loglik[2] / model_2$loglik[1])
cat("McFadden's Pseudo R-squared:", pseudo_r2, "\n")

# 同じ日に日高地区で操業した漁船数が自分の漁船＋２の時
model_3 <- clogit(choice ~ distance + L1choice + predicted_rev + strata(id), data = data_3)
summary(model_3)
# McFaddenの疑似決定係数 (Psuedo R-squared) の計算と出力
pseudo_r2 <- 1 - (model_3$loglik[2] / model_3$loglik[1])
cat("McFadden's Pseudo R-squared:", pseudo_r2, "\n")


# 交差項を考慮したフルモデル
# choice_dataをコピーした新しいデータフレームを作る
choice_data_full <- choice_data
choice_data_full$num_ves <- choice_data_full$num_ves - 1
full_model <- clogit(choice ~ distance + L1choice + predicted_rev + distance:num_ves + L1choice:num_ves + strata(id) + num_ves, data = choice_data_full)
summary(full_model)
# McFaddenの疑似決定係数 (Psuedo R-squared) の計算と出力
pseudo_r2 <- 1 - (full_model$loglik[2] / full_model$loglik[1])
cat("McFadden's Pseudo R-squared:", pseudo_r2, "\n")
