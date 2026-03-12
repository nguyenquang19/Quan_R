# 1. Bài tập về chuyển đổi kiểu dữ liệu số
num = 23.67
print(num)
class(num)
num_int = as.integer(num)
class(num_int)

# 2. Bài tập về ghép chuỗi
text1 = "HALO"
text2 = "HOC IT DAU DAU VCLLLLLLLLLLLLL"

help(paste)
help(paste0)

result1 = paste(text1, text2)
result1

result2 = paste(text1, text2, sep = "-")
result2

result3 = paste("a", "b", "c", sep='->')
result3

result4 = paste0(text1, text2)
result4

vectors_s = c("a", "b", "c")
result5 = paste(vectors_s)
result5

result5 = paste(vectors_s, collapse = ", ")
result5

result6 = paste(c("x", "y"), c("1", "2", "3", "4"), sep = "-", collapse = ";")
result6

result7 = paste(c(), c("1", "2", "3", "4"), recycle0 = FALSE)
result7

# 3. Bài tập về xử lý ngày tháng
vecDate = c("09:12:12", "28:02:16", "31:05:22")
class(vecDate)

dates = as.Date(vecDate, format="%d:%m:%y")
print(dates)

today = Sys.Date()
days_diff = today - dates
days_diff

# 4. Tạo vector số
vec1 = c(2:8, 17:30)
vec1

# 5. Sử dụng seq()
vec2 = seq(from=2, to=32, by=6)
vec2

# 6. Lặp chuỗi
result = rep(c("2", "7", "a"), times=3)
result

# 7. Vector 100 số chia hết cho 3
vec3 = seq(from=3, by=3, length.out=100)
vec3

# 8. Mẫu phức tạp
vec3 = rep(seq(from=1, to=9, by=2), each=2, times=3)
vec3

# 9. Số ngẫu nhiên
vec4 = runif(50)
vec4
vec4 = vec4 * 21
vec4
vec4 = floor(vec4)
vec4

# 10. Truy xuất phần tử
vec4[5]
vec4[c(5,10,26)]

# 11. Dãy có quy luật
indices = seq(from=5, to=length(vec4), by=2)
result = vec4[indices]
result