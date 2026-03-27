# BÀI TẬP 1: BAR CHART
subjects = c("Toán", "Lý", "Hóa", "Văn", "Anh")
scores = c(8, 7.5, 9, 8.5, 7)

# 1, 2, 3. Vẽ bar chart cơ bản với tiêu đề và màu sắc
# Lưu biểu đồ vào biến 'b' để xác định vị trí đặt text sau này
b = barplot(scores, names.arg = subjects, 
            main = "Điểm thi của bạn", 
            col = c("red", "blue", "green", "orange", "purple"),
            ylim = c(0, 10), ylab = "Điểm số")

# 4. Thêm giá trị điểm lên đầu mỗi cột
text(x = b, y = scores, labels = scores, pos = 3, cex = 1.2, col = "black")

# 5. Vẽ bar chart ngang
barplot(scores, names.arg = subjects, 
        main = "Điểm thi của bạn (Ngang)", 
        col = rainbow(5), 
        horiz = TRUE, las = 1)


# BÀI TẬP 2: HISTOGRAM
set.seed(2024)
exam_scores = rnorm(100, mean = 70, sd = 10)

# 1, 2, 3. Vẽ histogram với 10 bins, tiêu đề và màu xanh lam
hist(exam_scores, breaks = 10, col = "blue", 
     main = "Phân phối điểm thi (10 Bins)", 
     xlab = "Điểm số", ylab = "Tần suất")

# 4. Thêm đường thẳng đứng màu đỏ tại vị trí điểm trung bình
abline(v = mean(exam_scores), col = "red", lwd = 3, lty = 2)

# 5. Vẽ histogram với 20 bins
hist(exam_scores, breaks = 20, col = "skyblue", 
     main = "Phân phối điểm thi (20 Bins)", 
     xlab = "Điểm số", ylab = "Tần suất")
# Nhận xét: Với 20 bins, biểu đồ chi tiết hơn, cho thấy rõ các khoảng trống hoặc đỉnh nhỏ mà 10 bins bỏ qua.


# BÀI TẬP 3: BOX PLOT
# 1, 2, 3. Vẽ box plot cho Petal.Length theo loài (Species)
boxplot(Petal.Length ~ Species, data = iris, 
        col = c("gold", "coral", "lightgreen"),
        main = "So sánh chiều dài cánh hoa giữa các loài",
        xlab = "Loài", ylab = "Chiều dài cánh hoa")

# 4. Trả lời câu hỏi:
# - Loài nào có petal dài nhất? -> Virginica.
# - Loài nào có độ biến thiên lớn nhất? -> Virginica (chiều dài từ râu trên đến râu dưới lớn nhất).
# - Có outliers không? Ở loài nào? -> Có, xuất hiện chấm đen ở loài Setosa và Versicolor.


# BÀI TẬP 4: SCATTER PLOT
# 1, 2. Vẽ scatter plot hp vs mpg, màu theo số cyl
plot(mtcars$hp, mtcars$mpg, 
     col = mtcars$cyl, 
     pch = 19, # Kiểu điểm đặc
     main = "Mối quan hệ giữa Mã lực và Mức tiêu hao nhiên liệu",
     xlab = "Horsepower (hp)", ylab = "Miles per Gallon (mpg)")

# 3. Thêm đường hồi quy tuyến tính (Linear Regression)
abline(lm(mpg ~ hp, data = mtcars), col = "red", lwd = 2)

# 4. Thêm legend
legend("topright", legend = unique(mtcars$cyl), 
       col = unique(mtcars$cyl), pch = 19, title = "Số Cyl")

# 5. Nhận xét: Mối quan hệ tương quan nghịch (Negative correlation). 
# Khi mã lực tăng thì số dặm đi được trên mỗi gallon xăng giảm (xe mạnh thì tốn xăng hơn).


# BÀI TẬP 5: NHIỀU BIỂU ĐỒ (2X2
par(mfrow = c(2, 2)) # Thiết lập khung hình 2 dòng 2 cột

# 1. Histogram của hp
hist(mtcars$hp, col = "lightblue", main = "Histogram của hp", xlab = "hp")

# 2. Box plot của hp
boxplot(mtcars$hp, col = "lightpink", main = "Box plot của hp")

# 3. Box plot so sánh hp theo cyl
boxplot(hp ~ cyl, data = mtcars, col = "lightgreen", main = "hp theo số Cyl")

# 4. Scatter plot hp vs mpg
plot(mtcars$hp, mtcars$mpg, col = "darkgray", pch = 16, main = "Scatter hp vs mpg")

par(mfrow = c(1, 1)) # Reset về khung hình đơn


# BÀI TẬP 6: TỔNG HỢP
# Tạo dữ liệu
Q1 = c(100, 120, 110, 130)
Q2 = c(150, 140, 160, 155)
Q3 = c(180, 170, 190, 185)
Q4 = c(200, 210, 195, 220)
products = c("Sản phẩm A", "Sản phẩm B", "Sản phẩm C", "Sản phẩm D")
sales_matrix = matrix(c(Q1, Q2, Q3, Q4), nrow = 4, byrow = TRUE)

# 1, 2, 3, 4. Tạo figure 2x2
par(mfrow = c(2, 2))

# - Grouped bar chart
barplot(sales_matrix, beside = TRUE, col = terrain.colors(4),
        names.arg = products, main = "Doanh thu theo quý",
        legend.text = c("Q1", "Q2", "Q3", "Q4"),
        args.legend = list(x = "top", bty = "n", horiz = TRUE))

# - Line plot tất cả sản phẩm
matplot(t(sales_matrix), type = "b", pch = 19, lty = 1, col = 1:4,
        xaxt = "n", main = "Xu hướng doanh thu", xlab = "Quý", ylab = "Doanh thu")
axis(1, at = 1:4, labels = c("Q1", "Q2", "Q3", "Q4"))
legend("topleft", legend = products, col = 1:4, pch = 19, cex = 0.6)

# - Pie chart tổng doanh thu mỗi quý
total_per_quarter = rowSums(sales_matrix)
pie(total_per_quarter, labels = c("Q1", "Q2", "Q3", "Q4"), 
    col = cm.colors(4), main = "Tỷ trọng doanh thu Quý")

# - Bar chart tổng doanh thu mỗi sản phẩm
total_per_product = colSums(sales_matrix)
barplot(total_per_product, names.arg = products, 
        col = "orange", main = "Tổng doanh thu sản phẩm")

par(mfrow = c(1, 1))