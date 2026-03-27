# Tạo các vector với kiểu dữ liệu đa dạng
vector1 = 1:8 # vector số nguyên
vector2 = seq(0, 1, length.out=8) # vector số thực
vector3 = c(T, F, T, T, F, T, F, F)
vector4 = c("A", "B", "C", "D", "E", "F", "G", "H")
vector5 = as.factor(c("Thấp", "Cao", "Trung Bình", "Cao", "Cao", "Thấp", "Thấp", "Trung Bình"))

# Tạo dataframe
mySet1 = data.frame(vector1, vector2, vector3, vector4, vector5)

# a) Hiển thị dòng thứ 5
mySet1[5,]

# b) Thay đổi tên của cột thứ hai thành "column02"
colnames(mySet1)[2] = "column02"

# c) Hiển thị 7 dòng đầu tiên (2 phương pháp)
mySet1[1:7,]      # Phương pháp 1: Sử dụng chỉ số
head(mySet1, 7)   # Phương pháp 2: Sử dụng hàm

# Hiển thị mỗi dòng thứ 3 giữa quan sát 40 và 120
iris[seq(40, 120, 3), ]

# a) Thay đổi kiểu cột đầu tiên thành ký tự
women$height = as.character(women$height)

# b) Thêm hai dòng mới (đảm bảo đồng nhất kiểu dữ liệu)
new_rows = data.frame(
  height = as.character(c(70, 71)),
  weight = c(165, 168)
)
women = rbind(women, new_rows)

# c) Thêm cột shoe_size (số nguyên từ 35 đến 42)
women$shoe_size = floor(runif(nrow(women), min=35, max=43))

# 1. In uptake từ lớn nhất đến nhỏ nhất
sort(CO2$uptake, decreasing = TRUE)

# 2. Lọc Type là Quebec và Treatment là chilled
CO2[CO2$Type == "Quebec" & CO2$Treatment == "chilled", ]

# 3. Uptake > 40 và sắp xếp theo conc tăng dần (viết trên 1 dòng)
CO2[order(CO2$conc), ][CO2[order(CO2$conc), ]$uptake > 40, ]

# 4. Sắp xếp ngẫu nhiên bộ dữ liệu CO2
CO2[order(runif(nrow(CO2))), ]

# Thiết lập dữ liệu thiếu (Chạy code mẫu)
set.seed(123)
missCO2 = CO2
missCO2[c(as.integer(runif(10)*nrow(missCO2)), 14:16), "uptake"] = NA
missCO2[c(as.integer(runif(10)*nrow(missCO2)), 14:16), "conc"] = NA
missCO2$weight = paste0(as.integer(runif(nrow(missCO2))*30), "kg")

# 5. Hiển thị các dòng có ít nhất một giá trị bị thiếu
missCO2[!complete.cases(missCO2), ]

# 6. Điền giá trị uptake bị thiếu bằng 20
missCO2[is.na(missCO2$uptake), "uptake"] = 20

# 7. Điền giá trị conc bị thiếu bằng giá trị trung bình (mean)
missCO2[is.na(missCO2$conc), "conc"] = mean(missCO2$conc, na.rm = TRUE)

# 8. Trích xuất giá trị số từ biến weight vào cột weightNumber
missCO2$weightNumber = as.numeric(sub("kg", "", missCO2$weight))

# Lưu vào CSV
write.csv(missCO2, "missCO2.csv", row.names = TRUE)

# Lưu vào RDS (định dạng R đơn lẻ)
saveRDS(missCO2, "missCO2.rds")

# Lưu nhiều đối tượng vào RData
save(iris, mtcars, file = "multiple_datasets.RData")