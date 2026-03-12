# BÀI TẬP TỔNG HỢP 

# 1. Tạo một factor với các giá trị "a", "b", "c" có độ dài 7
v = c("a", "b", "c", "c", "b", "a", "b")
v_factor = factor(
  v,
  levels = c("a", "b", "c"),
  labels = c("Letter A", "Letter B", "Letter C")
)
summary(v_factor)
str(v_factor)


# 2. Tạo vector số từ 1-4 có độ dài 10 (ngẫu nhiên)
x = sample(1:4, 10, replace = TRUE)

print(x)
class(x)
summary(x)
x_factor = factor(
  x,
  levels = c(1,2,3,4),
  labels = c("low", "medium", "high", "very high"),
  ordered = TRUE
)
summary(x_factor)


# 3. Tạo ma trận 5x2 toàn số 0
table = matrix(0, nrow = 5, ncol = 2)
table[,1] = 3
table[3,2] = 20

print(table[,2])
class(table[,2])

table[4,2] = "twelve"

print(table[,2])
class(table[,2])
class(table[,1])


# 4. Tạo các biến khác kiểu và đưa vào list
vec1 = c("a","b","c")
mat1 = matrix(1:6, nrow = 2)
single_val = 100
vec2 = c(1,2,3)
myList = list(vec1, mat1, single_val, vec2)

myList[[1]] = c(myList[[1]], "d")

myList[[length(myList)+1]] = c(5,6,7,8,9)

myList[[length(myList)]][4]

myList[[length(myList)]][5] = NA