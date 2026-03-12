#Load dữ liệu
alcohol = read.csv("C:/Users/ducse/OneDrive/ドキュメント/data/student-alcohol.csv")
View(alcohol)

# xem 6 dòng đầu 
head(alcohol)

#kiểm tra structure (str)
summary(alcohol)

#loại bỏ cột đầu tiên
head(alcohol[,-1])
alcohol = alcohol[,-1]

#tìm dữ liệu thiếu
alcohol[!complete.cases(alcohol),]

#điền giá trị thiếu
alcohol$Mjob[63] = "other"

#đếm số dòng thiếu
length(alcohol[!complete.cases(alcohol),])

#kiểm tra phân bố tuổi
summary(alcohol$age)

#tính trung vị bỏ qua NA
median(alcohol$age, na.rm=TRUE)

#điền missing value bằng trung vị
alcohol$age[is.na(alcohol$age)] = median(alcohol$age, na.rm=TRUE)

alcohol$age[is.na(alcohol$age)]


#chuyển thành factor
#school: GP/MS
summary(factor(alcohol$school))

alcohol$school = factor(alcohol$school,
                        levels = c("GP","MS"),
                        labels = c("Gabriel Pereia","Mousina da Pireno")
)

#gioi tinh
summary(factor(alcohol$sex))

alcohol$sex = factor(alcohol$sex,
                     levels = c("F","M"),
                     labels = c("Female","Male"))

#noi o
summary(factor(alcohol$address))

alcohol$address <- factor(alcohol$address, 
                          levels = c("R", "U"), 
                          labels = c("rural", "urban"))

#farmsize
summary(factor(alcohol$famsize))

alcohol$famsize <- factor(alcohol$famsize, 
                          levels = c("GT3", "LE3"), 
                          labels = c("more than 3", "less or equal to 3"))

#tinh trang song chung
summary(factor(alcohol$Pstatus))

alcohol$Pstatus <- factor(alcohol$Pstatus, 
                          levels = c("A", "T"), 
                          labels = c("living apart", "living together"))

#Ordinal factors biến có thứ tự
#mother education
summary(factor(alcohol$Medu))
alcohol$Medu = factor(alcohol$Medu,
                      levels = c(0,1,2,3,4),
                      labels = c("none","primary","lower secondary","upper secondary","university"),
                      ordered = TRUE)

#father education
summary(factor(alcohol$Fedu))

alcohol$Fedu <- factor(alcohol$Fedu, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("none", "primary", "primary higher", 
                                  "secondary", "higher"), 
                       ordered = TRUE)

#ly do chon truong
summary(factor(alcohol$reason))

alcohol$reason <- factor(alcohol$reason)

#tự động hóa với lapply
#liệt kê các yes/no
binaryVariables = c ("schoolsup","famsup","paid","activities","nursery","higher","internet","romantic")

#xem dữ liệu
alcohol[,binarVariables]

#kiểm tra từng biến
lapply(alcohol[,binaryVariables], summary)

lapply(alcohol[,binaryVariables], factor)

lapply(alcohol[,binaryVariables], function (x) {summary(factor(x))})

#chuẩn hóa
alcohol$internet[alcohol$internet==0] = "no"
alcohol$internet[alcohol$internet=="NO"] = "no"
alcohol$internet[alcohol$internet==1] = "yes"
alcohol$internet[alcohol$internet=="YES"] = "yes"

#kiem tra la
summary(factor(alcohol$internet))

lapply(alcohol[,binaryVariables]) = lapply(alcohol[,binaryVariables], factor)

#thoi gian di hoc
summary(alcohol$traveltime)
summary(factor(alcohol$traveltime))

# Ý nghĩa: 1 - <15 phút, 2 - 15-30 phút, 3 - 30-60 phút, 4 - >1 giờ
alcohol$traveltime <- factor(alcohol$traveltime, 
                             levels = c(1, 2, 3, 4), 
                             labels = c("0-15 min", "15-30 min",
                                        "30-60 min", "above 60 min"),
                             ordered = TRUE)

#thời gian học mỗi tuần
summary(alcohol$studytime)
summary(factor(alcohol$studytime))

# Ý nghĩa: 1 - <2 giờ, 2 - 2-5 giờ, 3 - 5-10 giờ, 4 - >10 giờ
alcohol$studytime <- factor(alcohol$studytime, 
                            levels = c(1, 2, 3, 4), 
                            labels = c("0-2 hours", "2-5 hours",
                                       "5-10 hours", "above 10 hours"),
                            ordered = TRUE)

#xủ lý dữ liệu bẩn
# Phát hiện vấn đề
lapply(alcohol[, binaryVariables], function(x) {summary(factor(x))})

#kiểm tra tất cả biến binary
lapply(alcohol[, binaryVariables], function(x) {summary(factor(x))})

# Chuyển đổi tất cả cùng lúc
alcohol[, binaryVariables] <- lapply(alcohol[, binaryVariables], 
                                     factor, 
                                     levels = c("no", "yes"))

#xử lý biên mức độ
str(alcohol)

# Các biến có mức độ từ 1-5
leveledVariables <- c("freetime", "goout", "Dalc", "Walc")

# Xem phân bố
lapply(alcohol[, leveledVariables], summary)

## Chuyển đổi
alcohol[, leveledVariables] <- lapply(alcohol[, leveledVariables], 
                                      factor, 
                                      levels = c(1, 2, 3, 4, 5), 
                                      labels = c("very low", "low", "average",
                                                 "high", "very high"),
                                      ordered = TRUE)
#health
summary(alcohol$health)
summary(factor(alcohol$health))

# Ý nghĩa: 1 = rất tệ, 2 = tệ, 3 = trung bình, 4 = tốt, 5 = rất tốt
alcohol$health <- factor(alcohol$health, 
                         levels = c(1, 2, 3, 4, 5), 
                         labels = c("very bad", "bad", "average",
                                    "good", "very good"),
                         ordered = TRUE)

#kiểm tra lại
str(alcohol)
summary(alcohol)
sum(!complete.cases(alcohol))



