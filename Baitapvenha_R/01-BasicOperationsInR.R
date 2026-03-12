chieu_dai <- 20
chieu_rong <- 10
don_vi <- "met"

chu_vi <- (chieu_dai+chieu_rong)*2
dien_tich <- chieu_dai*chieu_rong

check_dk <- (dien_tich>150) & (chieu_dai>chieu_rong)

print(paste("Chu vi la: ", chu_vi, don_vi))
ketQua <- paste("Diện tích là: ", dien_tich, don_vi)
ketQua

class(don_vi)
class(check_dk)

class() 
is.numeric() 
is.integer() 
is.logical() 
is.character()
as.numeric()
as.integer()
as.logical()
as.character()
as.Date()

a<-1.5
class(a)

b<-10
class(b)

is.integer(a)
is.integer(b)
is.numeric(b)

b<-6.89
class(b)
b<-as.integer(b)
class(b)
print(b)

class(check_dk) 
check_dk