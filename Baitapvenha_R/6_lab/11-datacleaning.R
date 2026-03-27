# 1  Đọc dữ liệu
clients = read.csv("D:/CODING_DATA/clients.csv", stringsAsFactors = FALSE)

# 2 Định nghĩa hàm tìm Mode
get_mode = function(v) {
  uniqv = unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 3 Xử lý lệch cột
for (i in 1:nrow(clients)) {
  if (grepl("-", clients$Recency[i])) {
    # Kiểu 1 Kéo trái
    clients[i, 4:(ncol(clients)-1)] = clients[i, 5:ncol(clients)]
    clients[i, ncol(clients)] = NA
  } else if (grepl("-", clients$Teenhome[i])) {
    # Kiểu 2 Kéo phải
    clients[i, 6:ncol(clients)] = clients[i, 5:(ncol(clients)-1)]
    clients[i, 5] = NA
  } else if (clients$Marital_Status[i] == "Cycle") {
    # Kiểu 3: Đảo chỗ MS và Income
    clients$Marital_Status[i] = clients$Income[i]
    clients$Income[i] = NA
  }
}

# 4 Ép kiểu số cho các cột cần thiết
numeric_cols = c("Year_Birth", "Income", "Kidhome", "Teenhome", "Recency", "MntWines", "Response")
for(col in numeric_cols) {
  clients[[col]] = as.numeric(clients[[col]])
}

# Điền NA cho các biến định danh (Education, Marital_Status, Response) bằng Mode
clients$Education[is.na(clients$Education)] = "Other"
clients$Marital_Status[is.na(clients$Marital_Status)] = get_mode(clients$Marital_Status)
clients$Response[is.na(clients$Response)] = get_mode(clients$Response)

# Điền NA cho các biến số Income, Year_Birth, MntWines bằng Trung vị
clients$Income[is.na(clients$Income)] = median(clients$Income, na.rm = TRUE)
clients$Year_Birth[is.na(clients$Year_Birth)] = round(median(clients$Year_Birth, na.rm = TRUE))
clients$MntWines[is.na(clients$MntWines)] = median(clients$MntWines, na.rm = TRUE)

# 6 Chuyển đổi Factor
clients$Marital_Status = as.factor(clients$Marital_Status)

clients$Education[is.na(clients$Education)] = "Other"
clients$Education = factor(clients$Education, 
                           levels = c("Basic", "2n", "Graduation", "Master", "PhD"), 
                           ordered = TRUE)

# 7 Kiểm tra
print(paste("Tổng số NA còn lại:", sum(is.na(clients))))
View(clients)
sum(is.na(clients))

# 8 Lưu kết quả
save(clients, file = "D:/CODING_DATA/clientsInR.RData")