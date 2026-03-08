#load dataset
test = read.csv("C:/Users/ducse/OneDrive/ドキュメント/data/test.csv")

#xem thử mấy dòng đầu 
head(test)

#xem mấy dòng cuối
tail(test)

#kiểm tra structure
str(test)
View(test)

#thống kê
summary(test)

#hàm tìm giá trị phổ biến nhất (Mode)
get_mode = function(x) {
  ux = unique(x[!is.na(x)])
  ux[which.max(tabulate(match(x, ux)))]
}

#lấy các cột chứa NA
missing_cols = names(test)[colSums(is.na(test)) > 0]

#dùng dput để in ra định dạng c("")
dput(missing_cols)

#MSSubClass:loại hình nhà xây dựng
summary(factor(test$MSSubClass))
test$MSSubClass = factor(test$MSSubClass,
                         levels = c(20, 30, 40, 45, 50, 60, 70, 75, 80, 85, 90, 120, 150, 160, 180, 190),
                         labels = c("1-STORY 1946+", "1-STORY 1945-", 
                                    "1-STORY W/FINISHED ATTIC", "1-1/2 STORY UNFINISHED", 
                                    "1-1/2 STORY FINISHED", "2-STORY 1946+", "2-STORY 1945-", 
                                    "2-1/2 STORY", "SPLIT OR MULTI-LEVEL", "SPLIT FOYER", "DUPLEX", 
                                    "1-STORY PUD", "1-1/2 STORY PUD", "2-STORY PUD", "PUD MULTILEVEL", 
                                    "2 FAMILY CONVERSION"))

#MSZoning:phân vùng quy hoạch dân cư
summary(factor(test$MSZoning))
test$MSZoning = factor(test$MSZoning,
                       levels = c("A", "C (all)", "FV", "I", "RH", "RL", "RP", "RM"),
                       labels = c("Agriculture", "Commercial", "Floating Village Residential", 
                                  "Industrial", "Residential High Density", "Residential Low Density", 
                                  "Residential Low Density Park", "Residential Medium Density"))

#LotFrontage:chiều dài mặt tiền (xử lý NA bằng trung vị)
test$LotFrontage[is.na(test$LotFrontage)] = median(test$LotFrontage, na.rm = TRUE)
summary(test$LotFrontage)

#LotArea:diện tích khu đất
summary(test$LotArea)

#NHÓM 1:MẶT TIỀN 
#Street:loại đường tiếp giáp
summary(factor(test$Street))
test$Street = factor(test$Street, 
                     levels = c("Pave", "Grvl"), 
                     labels = c("Paved", "Gravel"))

#Alley:loại đường hẻm
summary(factor(test$Alley))
test$Alley = factor(test$Alley, 
                    levels = c("Pave", "Grvl", NA), 
                    labels = c("Paved", "Gravel", "NONE"), exclude = NULL)

#LotShape:hình dạng lô đất
summary(factor(test$LotShape))
test$LotShape = factor(test$LotShape, 
                       levels = c("Reg", "IR1", "IR2", "IR3"), 
                       labels = c("Regular", "Slightly Irregular", "Moderately Irregular", "Irregular"))

#LandContour:độ bằng phẳng địa hình
summary(factor(test$LandContour))
test$LandContour = factor(test$LandContour, 
                          levels = c("Lvl", "Bnk", "HLS", "Low"), 
                          labels = c("Near Flat", "Banked", "Hillside", "Depression"))

#Utilities:tiện ích sẵn có
summary(factor(test$Utilities))
test$Utilities = factor(test$Utilities, 
                        levels = c("AllPub", "NoSewr", "NoSeWa", "ELO"), 
                        labels = c("All public utilities", "Electricity Gas Water", "Electricity Gas Only", "Electricity Only"))

#LotConfig:cấu hình lô đất
summary(factor(test$LotConfig))
test$LotConfig = factor(test$LotConfig, 
                        levels = c("Inside", "Corner", "CulDSac", "FR2", "FR3"), 
                        labels = c("Inside lot", "Corner lot", "Cul-de-sac", "Frontage on 2 sides", "Frontage on 3 sides"))

#LandSlope:độ dốc khu đất
summary(factor(test$LandSlope))
test$LandSlope = factor(test$LandSlope, 
                        levels = c("Gtl", "Mod", "Sev"), 
                        labels = c("Gentle slope", "Moderate slope", "Severe slope"))

#Neighborhood:vị trí địa lý 
test$Neighborhood = factor(test$Neighborhood,
                           levels = c("Blmngtn", "Blueste", "BrDale", "BrkSide", "ClearCr", 
                                      "CollgCr", "Crawfor", "Edwards", "Gilbert", "IDOTRR", 
                                      "MeadowV", "Mitchel", "NAmes", "NoRidge", "NPkVill", 
                                      "NridgHt", "NWAmes", "OldTown", "SWISU", "Sawyer", 
                                      "SawyerW", "Somerst", "StoneBr", "Timber", "Veenker"),
                           labels = c("Bloomington Heights", "Bluestem", "Briardale", "Brookside", "Clear Creek", 
                                      "College Creek", "Crawford", "Edwards", "Gilbert", "Iowa DOT and Rail Road", 
                                      "Meadow Village", "Mitchell", "North Ames", "Northridge", "Northpark Villa", 
                                      "Northridge Heights", "Northwest Ames", "Old Town", "South and West of ISU", "Sawyer", 
                                      "Sawyer West", "Somerset", "Stone Brook", "Timberland", "Veenker"))


#NHÓM 2: NGOẠI THẤT VÀ CHẤT LƯỢNG 

#Condition1:điều kiện môi trường 1
summary(factor(test$Condition1))
test$Condition1 = factor(test$Condition1,
                         levels = c("Artery", "Feedr", "Norm", "RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe"),
                         labels = c("Adjacent to Arterial Street", 
                                    "Adjacent to Feeder Street", 
                                    "Normal", 
                                    "Near North South Railroad", 
                                    "Adjacent to North South Railroad", 
                                    "Near Positive Feature", 
                                    "Adjacent to Positive Feature", 
                                    "Near East West Railroad", 
                                    "Adjacent to East West Railroad"))

#Condition2:điều kiện môi trường 2
summary(factor(test$Condition2))
test$Condition2 = factor(test$Condition2,
                         levels = c("Artery", "Feedr", "Norm", "RRNn", "RRAn", "PosN", "PosA", "RRNe", "RRAe"),
                         labels = c("Adjacent to Arterial Street", 
                                    "Adjacent to Feeder Street", 
                                    "Normal", 
                                    "Near North South Railroad", 
                                    "Adjacent to North South Railroad", 
                                    "Near Positive Feature", 
                                    "Adjacent to Positive Feature", 
                                    "Near East West Railroad", 
                                    "Adjacent to East West Railroad"))
#BldgType:kiểu tòa nhà
summary(factor(test$BldgType))
test$BldgType = factor(test$BldgType, 
                       levels = c("1Fam", "2fmCon", "Duplex", "TwnhsE", "Twnhs"), 
                       labels = c("Single Family", "Two Family Conversion", "Duplex", 
                                  "Townhouse End Unit", "Townhouse Inside Unit"))

#HouseStyle:phong cách thiết kế
summary(factor(test$HouseStyle))
test$HouseStyle = factor(test$HouseStyle,
                         levels = c("1Story", "1.5Fin", "1.5Unf", "2Story", "2.5Fin", "2.5Unf", "SFoyer", "SLvl"),
                         labels = c("One story", 
                                    "One and half story finished", 
                                    "One and half story unfinished", 
                                    "Two story", 
                                    "Two and half story finished", 
                                    "Two and half story unfinished", 
                                    "Split Foyer", 
                                    "Split Level"))

#OverallQual:chất lượng vật liệu hoàn thiện
test$OverallQual = factor(test$OverallQual,
                          levels = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
                          labels = c("Very Excellent", "Excellent", "Very Good", "Good", "Above Average", 
                                     "Average", "Below Average", "Fair", "Poor", "Very Poor"))

#OverallCond:tình trạng hiện tại của nhà
summary(factor(test$OverallCond))
test$OverallCond = factor(test$OverallCond,
                          levels = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
                          labels = c("Very Excellent", "Excellent", "Very Good", "Good", "Above Average", 
                                     "Average", "Below Average", "Fair", "Poor", "Very Poor"))

#YearBuilt:năm xây dựng
summary(test$YearBuilt)

#YearRemodAdd:năm cải tạo
summary(test$YearRemodAdd)

#RoofStyle:kiểu mái nhà
summary(factor(test$RoofStyle))
test$RoofStyle = factor(test$RoofStyle)

#RoofMatl:vật liệu mái
summary(factor(test$RoofMatl))
test$RoofMatl = factor(test$RoofMatl,
                       levels = c("ClyTile", "CompShg", "Membran", "Metal", "Roll", "Tar&Grv", "WdShake", "WdShngl"),
                       labels = c("Clay or Tile", 
                                  "Standard Composite Shingle", 
                                  "Membrane", 
                                  "Metal", 
                                  "Roll roofing", 
                                  "Gravel and Tar", 
                                  "Wood Shakes", 
                                  "Wood Shingles"))

#Exterior1st:vật liệu ngoại thất 1
test$Exterior1st = factor(test$Exterior1st,
                          levels = c("AsbShng", "AsphShn", "BrkComm", 
                                     "BrkFace", "CBlock", "CemntBd", 
                                     "HdBoard", "ImStucc", "MetalSd", 
                                     "Other", "Plywood", "PreCast", "Stone", 
                                     "Stucco", "VinylSd", "Wd Sdng", "WdShing" ),
                          labels = c("Asbestos Shingles", "Asphalt Shingles", 
                                     "Brick Common", "Brick Face", "Cinder Block", 
                                     "Cement Board", "Hard Board", "Imitation Stucco", 
                                     "Metal Siding", "Other", "Plywood", "PreCast", 
                                     "Stone", "Stucco", "Vinyl Siding", "Wood Siding", 
                                     "Wood Shingles"))

#Exterior2nd:vật liệu ngoại thất 2
test$Exterior2nd = factor(test$Exterior2nd,
                          levels = c("AsbShng", "AsphShn", "BrkComm", 
                                     "BrkFace", "CBlock", "CemntBd", "HdBoard", 
                                     "ImStucc", "MetalSd", "Other", "Plywood", 
                                     "PreCast", "Stone", "Stucco", "VinylSd", "Wd Sdng", 
                                     "WdShing"),
                          labels = c("Asbestos Shingles", "Asphalt Shingles", 
                                     
                                     "Brick Common", "Brick Face", "Cinder Block", "Cement Board", "Hard Board", 
                                     "Imitation Stucco", "Metal Siding", "Other", "Plywood", 
                                     "PreCast", "Stone", "Stucco", "Vinyl Siding", "Wood Siding", "Wood Shingles")) 

#MasVnrType:loại tường ốp
summary(factor(test$MasVnrType))
test$MasVnrType = factor(test$MasVnrType, 
                         levels = c("BrkCmn", "BrkFace", "CBlock", "None", "Stone", NA), 
                         labels = c("Brick Common", "Brick Face", "Cinder Block", "None", "Stone", "None"), exclude = NULL)

#MasVnrArea:diện tích tường ốp
test$MasVnrArea[is.na(test$MasVnrArea)] = 0
summary(test$MasVnrArea)

#tạo bảng map chất lượng chung
`map quality` = c("Ex" = "Excellent", "Gd" = "Good", "TA" = "Typical Average", "Fa" = "Fair", "Po" = "Poor")

#ExterQual:chất lượng vật liệu ngoại thất
summary(factor(test$ExterQual))
test$ExterQual = factor(test$ExterQual, 
                        levels = names(`map quality`), 
                        labels = `map quality`)

#ExterCond:tình trạng vật liệu ngoại thất
summary(factor(test$ExterCond))
test$ExterCond = factor(test$ExterCond, 
                        levels = names(`map quality`), 
                        labels = `map quality`)

#Foundation:loại móng nhà
summary(factor(test$Foundation))
test$Foundation = factor(test$Foundation,
                         levels = c("BrkTil", "CBlock", "PConc", "Slab", "Stone", "Wood"),
                         labels = c("Brick & Tile", "Cinder Block", "Poured Contrete", "Slab", "Stone", "Wood")) 


#NHÓM 3:TẦNG HẦM

#BsmtExposure:mức độ tiếp xúc ánh sáng hầm
summary(factor(test$BsmtExposure))
test$BsmtExposure = factor(test$BsmtExposure, 
                           levels = c("Gd", "Av", "Mn", "No", NA), 
                           labels = c("Good Exposure", "Average Exposure", 
                                      "Minimum Exposure", "No Exposure", "No Basement"), exclude = NULL)

#BsmtFinType1:mức độ hoàn thiện hầm 1
summary(factor(test$BsmtFinType1))
test$BsmtFinType1 = factor(test$BsmtFinType1, 
                           levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", NA), 
                           labels = c("Good Living Quarters", "Average Living Quarters", 
                                      "Below Average Living Quarters", "Average Rec Room", 
                                      "Low Quality", "Unfinished", "No Basement"), exclude = NULL)

#bsmtQual:chất lượng tầng hầm 
test$BsmtQual = factor(test$BsmtQual,
                       levels = c("Ex", "Gd", "TA", "Fa", "Po", NA),
                       labels = c("Excellent Level", "Good Level", "Typical Level", 
                                  "Fair Level", "Poor Level", "No Basement"),exclude = NULL)

#bsmtcond:tình trạng tầng hầm 
test$Bsmtcond = factor(test$BsmtCond,
                       levels = c("Ex", "Gd", "TA", "Fa", "Po", NA),
                       labels = c("Excellent Level", "Good Level", "Typical Level", 
                                  "Fair Level", "Poor Level", "No Basement"),exclude = NULL)

#DT Ham SF:các diện tích hầm
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] = 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] = 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] = 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] = 0

#BsmtFinType2:mức độ hoàn thiện hầm 2
summary(factor(test$BsmtFinType2))
test$BsmtFinType2 = factor(test$BsmtFinType2, 
                           levels = c("GLQ", "ALQ", "BLQ", "Rec", "LwQ", "Unf", NA), 
                           labels = c("Good Living Quarters", "Average Living Quarters", 
                                      "Below Average Living Quarters", "Average Rec Room", 
                                      "Low Quality", "Unfinished", "No Basement"), exclude = NULL)

#Heating:hệ thống sưởi
summary(factor(test$Heating))
test$Heating = factor(test$Heating,
                      levels = c("Floor", "GasA", "GasW", "Grav", "OthW", "Wall"),
                      labels = c("Floor Furnace", "Gas forced warm air furnace", 
                                 "Gas hot water or steam heat", "Gravity furnace",
                                 "Other hot water or steam heat", "Wall furnace"))

#HeatingQC:chất lượng sưởi
summary(factor(test$HeatingQC))
test$HeatingQC = factor(test$HeatingQC,
                        levels = c("Ex", "Gd", "TA", "Fa", "Po"),
                        labels = c("Excellent", "Good", "Typical/Average", "Fair", "Poor"))

#CentralAir:điều hòa trung tâm
summary(factor(test$CentralAir))
test$CentralAir = factor(test$CentralAir, 
                         levels = c("Y", "N"), 
                         labels = c("Yes", "No"))


#NHÓM 4:NỘI THẤT

#Electrical:hệ thống điện
summary(factor(test$Electrical))
test$Electrical = factor(test$Electrical,
                         levels = c("SBrkr", "FuseA", "FuseF", "FuseP", "Mix"),
                         labels = c("Standard Circuit Breakers", "Fuse Box Average", "Fuse Box Fair", "Fuse Box Poor", "Mixed"))

#Phòng Tắm:số lượng phòng tắm hầm và trên mặt đất 
test$BsmtFullBath[is.na(test$BsmtFullBath)] = 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = 0
summary(test$FullBath)
summary(test$HalfBath)

#phòng tắm đầy đủ dưới hầm có cả bồn rửa mặt, toilet, vòi sen, bồn tắm 
test$BsmtFullBath[is.na(test$BsmtFullBath)] = 0 
test$BsmtFullBath = factor(test$BsmtFullBath, 
                           levels = c(0, 1, 2, 3), 
                           labels = c("0 Room", "1 Room", "2 Room", "3 Room"))

#phòng tắm nửa dưới hầm có toilet, bồn rửa mặt 
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] = 0 
test$BsmtHalfBath = factor(test$BsmtHalfBath, 
                           levels = c(0, 1, 2), 
                           labels = c("0 Room", "1 Room", "2 Room"))

#phòng tắm đầy đủ trên mặt đất có cả bồn rửa mặt, toilet, vòi sen, bồn tắm
test$FullBath = factor(test$FullBath, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("0 Room", "1 Room", "2 Room", "3 Room", "4 Room"))

#phòng tắm nửa trên mặt đất có toilet, bồn rửa mặt
test$HalfBath = factor(test$HalfBath, 
                       levels = c(0, 1, 2), 
                       labels = c("0 Room", "1 Room", "2 Room"))

#đổi các cột có đơn vị feet vuông
cols_sqft <- c("BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", "GrLivArea", "LotArea")

#gán NA bằng 0 và thêm đơn vị
test[cols_sqft] <- lapply(test[cols_sqft], function(x) paste(replace(x, is.na(x), 0), "sqft"))

#đối các cột có đơn vị feet 
test$LotFrontage <- paste(test$LotFrontage, "ft")

#kiểm tra tổng quát NA sau làm sạch
sum(is.na(test))

sum(is.na(test$Exterior2nd))

sum(is.na(test$Exterior1st))

sum(is.na(test$electrical))









