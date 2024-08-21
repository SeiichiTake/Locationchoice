library(tidyverse)
library(randomForest)
# Load the data
data <- read.csv("Data/raw_data.csv")

# 必要な列だけを抜き出す
# vessel_day_id, vessel_id, vessel_name, year, month, day, area, port, suke, revenue_suke, samani_km, urakawa_kmだけを抜き出す
data <- data %>% select(vessel_day_id, vessel_id, vessel_name, year, month, day, area, port, suke, revenue_suke, samani_km, urakawa_km, num_ves, L1choice, 漁区中央水深)

# 気象データを結合
weather2000 <- read.csv("Data/urakawa2000to2009.csv", encoding = "UTF-8")
weather2010 <- read.csv("Data/urakawa2010to2019.csv", encoding = "UTF-8")
weather2020 <- read.csv("Data/urakawa2020.csv", encoding = "UTF-8")

# 縦方向に結合する
weather <- rbind(weather2000, weather2010, weather2020)
# 年月日で列を分ける
weather <- separate(weather, 年月日, into = c("year", "month", "day"), sep = "/")

# dataとweatherを結合
# year, month, dayが一致する行を結合する
# weather データフレームの year 列を整数型に変換
weather$year <- as.integer(weather$year)
# weather データフレームの month 列を整数型に変換
weather$month <- as.integer(weather$month)
# weather データフレームの day 列を整数型に変換
weather$day <- as.integer(weather$day)
data <- left_join(data, weather, by = c("year", "month", "day"))
data <- subset(data, year != 2021)
data <- subset(data, vessel_id == 2)
# 最初の4行だけ削除する
data <- data[-(1:4), ]  # 1行目から4行目までの行を除外

# 漁獲金額予測モデルを作成するためのデータを作成
# 予測に必要な列だけを抜き出す
for_pred_data <- data %>% select(year, month, area, revenue_suke, 平均気温..., 降水量の合計.mm., 平均風速.m.s.)

# 漁獲金額を予測するモデルを作る
# year, month, day, area はダミー変数へ変換する
for_pred_data <- for_pred_data %>% mutate(year = as.factor(year), month = as.factor(month), area = as.factor(area))
for_train_data <- na.omit(for_pred_data)

# ランダムフォレスト回帰モデルの作成
model <- randomForest(revenue_suke ~ ., data = for_train_data, ntree = 500)
# 平均二乗誤差の計算
mse <- mean((for_train_data$revenue_suke - predict(model, for_train_data))^2)
print(mse)

predictions <- predict(model, for_pred_data)

data$predicted_rev <- predictions

data$vessel_day_id <- as.numeric(factor(data$vessel_day_id))

# dataをcsvファイルで保存
write.csv(data, "Data/data.csv", row.names = FALSE)
