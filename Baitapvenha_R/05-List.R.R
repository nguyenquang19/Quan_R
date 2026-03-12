# Tạo list đơn giản
student = list(
  name = "HELP!!!!",
  age = 18,
  grades = c(8.5, 9.0, 7.5),
  passed = TRUE
)

# Truy cập dữ liệu

# Cách 1: dùng [ ] -> trả về list con
student[1]
class(student[1])

# Cách 2: dùng [[ ]] -> trả về giá trị
student[[1]]
class(student[[1]])

student[[3]][c(1,3)]

# Cách 3: dùng $ -> truy cập theo tên
student$grades

# Thêm giá trị vào list
student$id = "012345678"
print(student)

# Cập nhật giá trị
student$grades = c(10,10,10)
student