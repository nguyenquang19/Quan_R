# Unordered Factor
colors = factor(c("red", "green", "blue", "yellow"))

# Ordered Factor (có thứ tự low < medium < high)
ratings = factor(
  c("low", "high", "medium", "high"),
  levels = c("low", "medium", "high"),
  ordered = TRUE
)

str(ratings)

# Ví dụ dữ liệu giáo dục
education = factor(
  c("High School", "Bachelor", "Master", "PhD"),
  levels = c("High School", "Bachelor", "Master", "PhD"),
  ordered = TRUE
)

str(education)
levels(education)

# Tạo dữ liệu điểm số học sinh
grades = factor(
  c("Giỏi", "Khá", "Trung bình", "Giỏi", "Khá", "Yếu"),
  levels = c("Yếu", "Trung bình", "Khá", "Giỏi"),
  ordered = TRUE
)

table(grades)

# Khảo sát mức độ hài lòng khách hàng
satisfaction = factor(
  c("Rất thích", "Thích", "Bình thường", "Không thích",
    "Thích", "Rất thích", "Bình thường"),
  levels = c("Không thích", "Bình thường", "Thích", "Rất thích"),
  ordered = TRUE
)

table(satisfaction)

# Vẽ biểu đồ cột
barplot(table(satisfaction))

# Ví dụ kích cỡ áo
sizes = factor(
  c("M", "L", "S", "XL", "M", "S", "M", "L", "XL", "M"),
  levels = c("S", "M", "L", "XL"),
  ordered = TRUE
)

# Thống kê số lượng từng size
summary(sizes)