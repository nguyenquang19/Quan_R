# Tạo matrix từ vector
mat1 = matrix(1:6, nrow = 2, ncol = 3)
mat1

# Điền theo hàng
mat2 = matrix(1:6, ncol = 3, byrow = TRUE)
mat2

# Matrix rỗng
empty_mat = matrix(NA, nrow = 3, ncol = 5)
empty_mat

# Matrix toàn số 0
zero_mat = matrix(0, nrow = 3, ncol = 3)
zero_mat

# Ví dụ bảng điểm
grades = matrix(
  c(
    8.5, 9, 7.5, 8.8,
    9.2, 7.8, 8.5, 9.0,
    7.6, 8.2, 8.9, 8.5
  ),
  nrow = 3,
  ncol = 4,
  byrow = TRUE
)

rownames(grades) = c("Le Nhat Tung", "TITV", "Le Thi C")
colnames(grades) = c("Toán", "Văn", "Tin", "R")
grades

# Ví dụ dữ liệu bán hàng
sales = matrix(
  c(150, 200, 180,
    120, 160, 140,
    180, 220, 200),
  nrow = 3,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(
    c("SP1", "SP2", "SP3"),
    c("T1", "T2", "T3")
  )
)
sales

# Truy xuất dữ liệu
sales[1,2]
sales[1,]
sales[,2]
sales[1:2,]
sales[,2:3]

# Tính toán
rowSums(sales)
colSums(sales)
rowMeans(sales)
colMeans(sales)

# Vẽ biểu đồ
barplot(rowSums(sales))