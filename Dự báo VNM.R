install.packages("readxl")
install.packages("ggplot2")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("rugarch")
install.packages("xts")
install.packages("fGarch")
install.packages("tidyverse")

library(readxl)
library(ggplot2)
library(quantmod)
library(PerformanceAnalytics)
library(rugarch)
library(xts)
library(fGarch)
library(tidyverse)

#Tải dữ liệu
VNM <- read_excel("D:/Downloads/Phân tích dữ liệu bằng R/VNM.xlsx")
head(VNM)

#Vẽ biểu đồ giá cổ phiếu
ggplot(VNM, aes(x = `Date/Time`, y = Close))+
  geom_line()+
  labs(x = "Ngày", y = "Giá đóng cửa", title = "Biểu đồ Giá cổ phiếu VNM")+
  theme_minimal()

#Chuyển đổi dữ liệu thành dạng time-series
VNM_ts <- xts(VNM[, -1], order.by = as.Date(VNM$`Date/Time`, format = "%d/%m/%Y"))
chartSeries(VNM_ts)

#Tỉ suất lợi nhuận hàng ngày (Daily Return)
return <- CalculateReturns(VNM_ts$Close)
return <- return[-1] 
hist(return)

chartSeries(return, theme = 'white')

#Tính độ biến động hàng năm
sd(return)
chart.RollingPerformance(R = return["2015::2023"],
                         width = 252,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Độ biến động hàng năm của VNM")

# Tính dãy lợi suất
returns <- diff(log(VNM$Close))

# Quy chuẩn thời gian
VNM$`Date/Time` <- as.Date(VNM$`Date/Time`, format = "%m/%d/%Y")

# Bổ sung lợi suất ngày đầu tiên là 0
VNM$RETURN <- c(0, returns)

last_date <- VNM$`Date/Time`[length(VNM$`Date/Time`)]
last_close <- VNM$Close[length(VNM$Close)]

#Chạy mô hình giả định dự báo
my_garch <- garchFit(~arma(1,1)+garch(1,1), data = VNM$RETURN, trace = FALSE)
my_garch
df_for <- garchSim(spec = garchSpec(model = coef(my_garch)), n = 300)
df_sim <- tibble(
  i_t = 0:length(df_for$garch),
  ref_date = last_date + i_t,
  sim_log_ret = c(0, df_for$garch),
  sim_arit_ret = exp(sim_log_ret)-1,
  sim_price = last_close*(cumprod(1+sim_arit_ret)))
p1 <- ggplot() +
  geom_line(data = VNM, aes(x = `Date/Time`, y = Close), color = 'black', size = 0.75) +
  geom_line(data = df_sim, aes(x =ref_date, y = sim_price, group = 1),color = 'red', size = 0.35) +
  labs(title = "Dự báo giá cổ phiếu VNM", x = "Năm", y = "Giá")
theme_bw()
p1

# Biểu đồ giá dự báo
ggplot(data = df_sim, aes(x = ref_date, y = sim_price)) +
  geom_line(color = "red", linewidth = 0.5) +
  labs(title = "Dự báo giá Cổ phiếu VNM", x = "Ngày", y = "Giá") +
  theme_bw()