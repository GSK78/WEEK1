df<-data.frame(name=c("akhil","amar","vallab","gowtham"),
               age=c(20,20,20,20),
               height=c(5.8,6.0,5.7,6.0),
               weight=c(84,60,65,70),
               sex=as.factor(c("m","f","m","m")))
levels(df$sex)
levels(df$sex)<-c("m","f")
df
df1<-data.frame(name=c("akhil","amar","vallab","gowtham"),
                working=c("yes","no","yes","no"))
combined<-cbind(df,df1)
combined
nrow(df)
nrow(df1)
ncol(df)
ncol(df1)
cc<-sapply(df,class)
cc 
cc<-sapply(combined,class)
cc
dd<-class(df)
d<-data.frame(df)
d
x=c(1,2,3,3)
x1<-class(x)
x1
x2<-data.frame(x1)
x2
ak<-data.frame(marks=c(90,100,80),
               cgpa=c(8.95,9.21,9.20),
               crank=c(230,150,170))
ak
sh<-ak[order(ak$marks),]
sh
#...............................................................................
my<-matrix(1:7,nrow=3,ncol=3)
my
d<-data.frame(my)
d
row<-paste("id",1:nrow(d),sep='_')
rownames(d)<-row
col<-paste("variable",1:ncol(d),sep='_')
colnames(d)<-col
d
#................................................................................
data("VADeaths")
VADeaths-as.data.frame(VADeaths)
Total<-rowSums(VADeaths)
Total
Total
VADeaths<-VADeaths[,c(ncol(VADeaths),1:(ncol(VADeaths)-1))]
print(VADeaths)
#............................................................
data(state.x77)
state.x77<-as.data.frame(state.x77)
c<-sum(state.x77$Income<4300)
c
k<-which.max(state.x77$Income)
h<-colnames(state.x77)[k]
h
#...........................................................
data("swiss")
ro<-c(1,2,3,10,11,12,13)
col<-c("Examination","Education","Infant.Mortality")
data<-swiss[ro,col]
data
x<-data[swiss$Infant.Mortality=="Sarine","Infant.Morality"]<-NA
x
tr<-colSums(data[,1:3],na.rm=TRUE)
data<-rbind(data,c("Total",tr))

data
data$Proportion<-data$Examination/data$Total
data
#.............................

data("state.abb")
data("state.area")
data("state.division")
data("state.name")
data("state.region")


statedata <- data.frame(abb = state.abb, area = state.area, division = state.division, name = state.name, region = state.region)


colnames(statedata) <- gsub(".*\\.", "", colnames(statedata))


rownames(statedata) <- statedata$name

statedata <- statedata[, -which(colnames(statedata) == "name")]


print(statedata)
#..................................

combineddata <- cbind(state.x77, statedata)

combineddata <- combineddata[, -which(colnames(combineddata) %in% c("div", "Life Exp", "HS Grad", "Frost", "abb", "are"))]

combineddata$Illiteracy_Level <- cut(combineddata$Illiteracy, breaks = c(0, 1, 2, Inf), labels = c("Low", "Some", "High"), right = FALSE)

west_low_illiteracy <- combineddata[combineddata$region == "West" & combineddata$Illiteracy_Level == "Low", ]


highestincomestate <- west_low_illiteracy[which.max(west_low_illiteracy$Income), ]

cat("State with the highest income in the West and low illiteracy:", highestincomestate$name, "\n")
cat("Income:", highestincomestate$Income, "\n")