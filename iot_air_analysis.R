install.packages("elastic")
install.packages("sqldf")
# 載入elastic
library(elastic)

# 設定es連線 
es <- connect(host = '139.162.75.164', port = 9200, transport_schema = "http")

# 查看資料量
#index_name = 'ltu-iot'
index_name = 'weather'
total_num = Search(es, index = index_name)$hits$total$value

#試連線抓一次資料
res <- Search(es, index = index_name)
res$hits$hits
docs <- lapply(res$hits$hits, "[[", "_source")

#用data.table 解析內容
library(data.table)
doc_tb = rbindlist(docs, fill = TRUE, use.names = TRUE)

## ---------------------------------------------------------
#P67
#資料集介紹
?airquality

# 載入elastic
library(elastic)

# 設定es連線 
es <- connect(host = '139.162.75.164', port = 9200, transport_schema = "http")

#抓全部的資料作分析
res_all <- Search(es, index = index_name, time_scroll = "1m", size = 2000)
print(res_all$`_scroll_id`) 
out_all <- list()
hits <- 1
while(hits != 0){
  tmp1 <- scroll(conn=es, x = res_all$`_scroll_id`, time_scroll = "1m")
  hits <- length(tmp1$hits$hits)
  if(hits > 0){
    out_all <- c(out_all, tmp1$hits$hits)
  }
  if(length(out_all) >= 100000){
    break
  }
}

#length(out_all)

#str(out_all)

#轉成 data table
docs_all <- lapply(out_all, "[[", "_source")
sensor <- rbindlist(docs_all, fill = TRUE, use.names = TRUE)
nrow(sensor)
head(sensor)

head(airquality)

library(sqldf)
air_quility <- sqldf("select * from airquality")

df_sensor <- sqldf("select cast(substr(trim(logtime), 7,1) as int) month,
        cast(substr(trim(logtime), 9, 2) as int) day,
        avg(Humidity) avg_humidity,
        avg(Temperature) avg_temperature
        from sensor
        group by
          cast( substr(trim(logtime),7,1) as int ),
          cast( substr(trim(logtime),9,2) as int )
        having cast( substr(trim(logtime),7,1) as int ) <> 0
      ")

df_allitems <- sqldf(" select a.*,b.avg_temperature,b.avg_humidity
                                            from airquality a
                                            left join df_sensor b
                                            on a.Month=b.month and a.Day = b.day
                                         ")
#預測和建模, 摘要
# Step 3 建置多元回歸模型
lmTrain <- lm(formula = Ozone ~ Solar.R+Wind+avg_temperature+avg_humidity, 
                data = subset(df_allitems, complete.cases(df_allitems))) #排除null

# 模型摘要
summary(lmTrain) 

# Step 4 預測明日臭氧濃度
new_data <-data.frame(Solar.R=200, Wind=12, avg_temperature=32.1, avg_humidity=62.7)

predicted <- predict(lmTrain, newdata = new_data)
predicted = predicted/1000
predicted
