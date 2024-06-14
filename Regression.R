# Các thư viện cần thiết
library(readxl)
library(dplyr)
library(plm)
library(ggplot2)
library(reshape2)
library(stargazer)
library(summarytools)
#Import dữ liệu vào
file_path <- "E:/K214142070.xlsx"
data <- read_excel(file_path)

#Xem qua dữ liệu
head(data)

#Dữ liệu thuộc dạng dữ liệu bảng, cần khai báo không gian và thời gian
pdata <- pdata.frame(data, index = c("NAME", "YEAR"))

#-------------------------------------------------------------------------------
# Tạo biến Phụ thuộc GROWTH là logarit của biến Revenue 
# Tạo biến giải thích CASH là logarit của biến Cash and Short Term Investment
# Xóa  giá trị âm trong cột "Revenue" và "Cash.and.Short.Term.Investments" trước khi logarit
pdata <- pdata[pdata$Revenue > 0 & pdata$Cash.and.Short.Term.Investments > 0 & pdata$Total.Equity>0,  ]
#Tiến hành logarit GROWTH, CASH và tính biến LEV, SIZE 
pdata <- pdata %>%
  mutate(
    YEAR = as.integer(as.character(YEAR)),
    Organization.Founded.Year = as.integer(as.character(Organization.Founded.Year)),
    GROWTH = log(Revenue),
    CASH = (Cash.and.Short.Term.Investments/Total.Assets),
    SIZE = log(Total.Assets),
    LEV = Total.Liabilities / Total.Equity,
    AGE = YEAR - Organization.Founded.Year
  )
# Tạo biến giả COVID19( 2020,2021 là 2 năm Covid ảnh hưởng nặng nhất tại Việt Nam)
pdata <- pdata %>%
  mutate(COVID19 = ifelse(YEAR %in% c(2020, 2021), 1, 0))

#-------------------------------------------------------------------------------


#1.Vẽ biểu đồ Scatterplot cho các biến
ggplot(pdata, aes(x = SIZE, y = GROWTH)) +
  geom_point() +
  labs(x = "SIZE", y = "GROWTH") +
  ggtitle("Đồ thị phân tán quy mô và doanh thu")

ggplot(pdata, aes(x = AGE, y = GROWTH)) +
  geom_point() +
  labs(x = "AGE", y = "GROWTH") +
  ggtitle("Đồ thị phân tán năm hoạt động và doanh thu")

ggplot(pdata, aes(x = CASH, y = GROWTH)) +
  geom_point() +
  labs(x = "CASH", y = "GROWTH") +
  ggtitle("Đồ thị phân tán tỷ lệ nắm giữ tiền mặt và doanh thu")

#BIẾN LEV CẦN XỬ LÝ OUTLIER, Lựa chọn Phân vị thứ 99% (loại bỏ phần trăm cao nhất)
q99 <- quantile(pdata$LEV, 0.99)  # Phân vị thứ 99% (loại bỏ phần trăm cao nhất)
# Loại bỏ outlier
pdata_clean <- pdata[pdata$LEV <= q99, ]
# Vẽ lại biểu đồ scatterplot sau khi loại bỏ outlier
ggplot(pdata_clean, aes(x = LEV, y = GROWTH)) +
  geom_point() +
  labs(x = "LEV", y = "GROWTH") +
  ggtitle("Đồ thị phân tán đòn bẩy và doanh thu")



#-------------------------------------------------------------------------------
#2.MÔ TẢ THỐNG KÊ CÁC BIẾN

dfSummary(pdata[, c("GROWTH", "SIZE", "LEV", "AGE", "CASH", "COVID19")])


#-------------------------------------------------------------------------------
#3. Tính toán ma trận correlation
correlation_matrix <- cor(pdata[, c("SIZE", "LEV", "AGE", "CASH")])
correlation_matrix
correlation_df <- as.data.frame(as.table(correlation_matrix))
ggplot(data = correlation_df, aes(x = Var1, y = Var2, fill = Freq, label = round(Freq, 2))) +
  geom_tile(color = "white") +  # Viền màu trắng cho các ô
  geom_text(color = "black") +  # Thêm nhãn với màu đen
  scale_fill_gradient2(low =  "#00a676", high ="#2c7bb6", mid = "#ffffff", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  labs(title = "Heatmap of Correlation Matrix", x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  coord_fixed()




#-------------------------------------------------------------------------------
#4.MÔ HÌNH HỒI QUY HIỆU ỨNG CỐ ĐỊNH
#Mô hình 1
model_fem1 <- plm(GROWTH ~ SIZE + AGE + LEV+ CASH, 
                  data = pdata, 
                  index = c("NAME", "YEAR"), 
                  model = "within")


#Mô hình 2
model_fem2 <- plm(GROWTH ~ COVID19*SIZE + COVID19*AGE + COVID19*LEV+ COVID19*CASH , 
                  data = pdata, 
                  index = c("NAME", "YEAR"), 
                  model = "within")


# Tạo danh sách các mô hình
models <- list(model_fem1, model_fem2)
# In bảng tóm tắt
stargazer(models, type = "text")







