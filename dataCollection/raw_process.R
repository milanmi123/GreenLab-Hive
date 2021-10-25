library(tidyverse)
library(plyr)

extract_data <- function(path_name){
  csvpath <- list.files(path = path_name,
                        recursive = TRUE,
                        pattern = "^Joule.*\\csv",
                        full.names = TRUE)
  
  col_data <- csvpath %>%
    lapply(read_csv) %>%
    bind_rows
  
  return(col_data)
}

app_big <- extract_data("./raw_data/BigCom_App/data/")
web_big <- extract_data("./raw_data/BigCom_Web/data/")
app_sma <- extract_data("./raw_data/SmallCom_App/data/")
web_sma <- extract_data("./raw_data/SmallCom_Web/data/")

app_long <- extract_data("./raw_data/Signal_App/data/")
web_long <- extract_data("./raw_data/Signal_Web/data/")

app_4g <- extract_data("./raw_data/4G_App/data/")
web_4g <- extract_data("./raw_data/4G_Web/data/")

# rename the column name
colnames(app_long) <- c("app")
colnames(app_4g) <- c("app")
colnames(web_long) <- c("web")
colnames(web_4g) <- c("web")

colnames(app_big) <- c("app")
colnames(app_sma) <- c("app")
colnames(web_big) <- c("web")
colnames(web_sma) <- c("web")

# put two platform into one df
df_big <- data.frame(app_big, web_big)
df_sma <- data.frame(app_sma, web_sma)

df_long <- data.frame(app_long, web_long)
df_4g <- data.frame(app_4g, web_4g)

# output csv file (for further analysis)
write.csv(df_long,"long_app_web.csv", row.names = FALSE)
write.csv(df_4g,"4g_app_web.csv", row.names = FALSE)

write.csv(df_big,"big_app_web.csv", row.names = FALSE)
write.csv(df_sma,"small_app_web.csv", row.names = FALSE)
