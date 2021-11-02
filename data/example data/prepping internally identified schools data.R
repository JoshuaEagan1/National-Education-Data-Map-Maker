#fixing the internal identifier for the example data

data<-read.xlsx("C:/Users/eaganj1/Desktop/NCES Mapper Tool/app/data/internal identifier district example data.xlsx")
str(data)

table(nchar(data$`State.School.ID.[Public.School].2019-20`))
table(nchar(data$`State.Agency.ID.[Public.School].2019-20`))
table(substr(data$`State.School.ID.[Public.School].2019-20`, 1, 9)==data$`State.Agency.ID.[Public.School].2019-20`)

data$Internal_School_Identifier<-substr(data$`State.School.ID.[Public.School].2019-20`, 11, 19)
names(data)[9]<-"Internal_District_Identifier"

write.xlsx(data, file="C:/Users/eaganj1/Desktop/NCES Mapper Tool/app/data/internal identifier school example data.xlsx")