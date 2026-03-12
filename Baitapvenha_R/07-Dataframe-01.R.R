# 1. Tạo dataframe
column1 = c(1:3)
column2 = c("Hoc", "IT", "dau dau vclllllllll")
column3 = c(TRUE, TRUE, FALSE)

dataset1 = data.frame(column1, column2, column3)

dataset1
print(dataset1)
View(dataset1)
colnames(dataset1)
colnames(dataset1)[2] = "Name"
dataset1
colnames(dataset1) = c("#", "Name", "Check")
dataset1

# 2. Thêm dòng mới vào dataframe
newRow = c(4, "Nhat Tung", TRUE)
dataset2 = rbind(dataset1, newRow)
dataset2

newRowDF = data.frame(5, "Lisa", FALSE)
names(newRowDF) = c("#", "Name", "Check")

dataset3 = rbind(dataset2, newRowDF)
dataset3

# 3. Thêm cột mới
newColumn = c("a", "b", "c", "d", "f")
dataset4 = cbind(dataset3, newColumn)
dataset4

dataset4$newColumn2 = c(1,2,3,4,5)

# 4. Truy xuất dữ liệu
dataset4[3,2]    
dataset4[3,"Check"]
dataset4["Name"]
dataset4[,"Name"]
dataset4$Name

# 5. Các hàm thường dùng
head(dataset4)     
tail(dataset4)    
str(dataset4)      
summary(dataset4)  

dataset4$Check = as.logical(dataset4$Check)