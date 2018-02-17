# Same Sex Marriage Vote Map - Data preperation and processing

library(data.table)
library(tools)
library(XML)
library(RCurl)
library(ggplot2)

###### Vote Data ######

# http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1800.02017?OpenDocument
# http://www.abs.gov.au/AUSSTATS/ABS@Archive.nsf/log?openagent&australian_marriage_law_postal_survey_2017_-_response_final.xls&1800.0&Time%20Series%20Spreadsheet&916379DAEE6E960CCA2581F0001A08A9&0&2017&11.12.2017&Latest
DT <- fread("../data/ssmVote.csv", sep=",", header = T)

# Edit names to match abs electorate data
DT[,Area_case:=toupper(Area)]
DT[,Area_case:=gsub("(E)","",Area_case,fixed = T)]
DT[,Area_case:=gsub("(C)","",Area_case,fixed = T)]
DT[,Area_case:=gsub("(D)","",Area_case,fixed = T)]
DT[,Total_pc:=NULL]
#DT[grep(")",Area,fixed = T)]


# Get members details and merge with DT
webaddy <- "https://en.wikipedia.org/wiki/Members_of_the_Australian_House_of_Representatives,_2016%E2%80%932019"

doc.html <- getURL(webaddy)
doc.html <- htmlParse(doc.html)

parlimentTable <- readHTMLTable(doc.html)
parlimentTable<-as.data.table(parlimentTable[[2]])
parlimentTable[,ElecCaps:=toupper(Electorate)]
parlimentTable[,Notes:=NULL]

DT<-merge(DT,parlimentTable[,.(Member,Party,State,ElecCaps,`In office`)], by.x="Area_case", by.y="ElecCaps",all.x=TRUE)


# Write to new text file
fwrite(DT,"../data/ssmVote2.csv", sep=",")

# Import into QGIS and join with abs electorate shapefile.  Downloaded from:
# http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202017
# http://www.abs.gov.au/AUSSTATS/subscriber.nsf/log?openagent&1270055003_ced_2017_aust_shp.zip&1270.0.55.003&Data%20Cubes&9C2197D75933022ACA2581C9001CCF70&0&July%202017&31.10.2017&Latest


# Simplify shapefiles

# Import joined vote shapefile data and simplify
ssmVoteMap <- readOGR("./data","ssmVoteMap")
ssmVoteMapSimp001 <- gSimplify(ssmVoteMap,.001)

# Rejoin text/attribute data to simplified spatial data and save as ESRI shapefile
ssmVoteMapSimp001<-SpatialPolygonsDataFrame(ssmVoteMapSimp001, ssmVoteMap@data)
writeOGR(ssmVoteMapSimp001,"./data","ssmVoteMap",driver="ESRI Shapefile", overwrite_layer=TRUE)

# Import suburb polygon shapefile and rejoin text data
subMap <- readOGR("./data","SSC_2016_AUST")
subMapSimp001 <- gSimplify(subMap,.0011)
subMapSimp001<-SpatialPolygonsDataFrame(subMapSimp001, subMap@data)
writeOGR(subMapSimp001,"./data","SSC_2016_AUST",driver="ESRI Shapefile", overwrite_layer=TRUE)



#################  ADD More Info  ##############

# http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1800.02017?OpenDocument
# http://www.abs.gov.au/AUSSTATS/ABS@Archive.nsf/log?openagent&australian_marriage_law_postal_survey_2017_-_participation_final.xls&1800.0&Time%20Series%20Spreadsheet&830FD42482B309D6CA2581F0001A085B&0&2017&11.12.2017&Latest
participation <- fread("../data/Participation.csv")
participation <- participation[`18-19 years`!=""]
names(participation)[1:2] <- c("Area","Type")


for(i in 1:nrow(participation)) {
    
    if(participation$Area[i] != "") {
        areaName <-participation$Area[i]
    } else { participation$Area[i] <- areaName }
    
}

part_melt <- melt(participation, id.vars = c("Area","Type"), value.name = "Value", variable.name = "Demographic")
participation2 <- dcast(part_melt,Area + Demographic ~ Type)
names(participation2)[3:5] <- c("EligPart","PartRate","TotalPart")
participation2[Demographic=="Age and/or gender nfd.(b)",Demographic:="Not defined"]

cols<-c("EligPart","PartRate","TotalPart")

participation2[, (cols) := lapply(.SD, function(x) gsub(",","",x)), .SDcols=cols]
participation2[, (cols) := lapply(.SD, as.numeric), .SDcols=cols]

participation2[,Area:=gsub("(e)","",Area,fixed = T)]
participation2[,Area:=gsub("(c)","",Area,fixed = T)]
participation2[,Area:=gsub("(d)","",Area,fixed = T)]

fwrite(participation2,"./data/participation.csv")



###### Census Data ########

# https://datapacks.censusdata.abs.gov.au/datapacks/

# Age P1 #

age <- fread("../data/2016_PEP_CED_for_AUS_short-header/2016 Census PEP Commonwealth Electoral Divisions for AUST/2016Census_P01_AUS_CED.csv")
a<-as.numeric(c(1,(grep("_P$",names(age)))))
age <- age[,..a]
a<-as.numeric(c(1,(grep("Age",names(age)))))
age <- age[,..a]
a<-as.numeric(c(1,(grep("_yr_",names(age)))))
age <- age[,..a]
names(age) <- gsub("_P$","",names(age))
age <- melt(age, id.vars = "CED_CODE_2016", value.name = "Persons", variable.name = "Group")

splitNm <- function(x) {
    a<-unlist(strsplit(as.character(x),split="_", fixed=TRUE))
    
    if(a[length(a)]=="yr") {
        return(paste(a[2],"-",a[3]," ",a[4], sep=""))
    } else {
        return(paste(a[2],"+ ",a[3], sep=""))
    }
}

age$GroupNm<-sapply(age$Group,splitNm)

age[, CED_NO:=gsub("CED","",CED_CODE_2016)]
age[,dataset:="Age Breakdown"]

fwrite(age,"./data/age.csv")


ggplot(age,aes(x=Group, y=Persons)) +
    geom_bar(stat="sum") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90),
          legend.position="none")



# Religion P14 #

religion <- fread("../data/2016_PEP_CED_for_AUS_short-header/2016 Census PEP Commonwealth Electoral Divisions for AUST/2016Census_P14_AUS_CED.csv")
a<-as.numeric(c(1,(grep("_P$",names(religion)))))
religion <- religion[,..a]
names(religion) <- gsub("_P$","",names(religion))
religion <- melt(religion, id.vars = "CED_CODE_2016", value.name = "Persons", variable.name = "Group")


#sum(religion[!grep("Tot",Religion)]$Persons)

# fwrite(data.table(ReligionId=unique(religion$Religion)),"../data/religionNm.csv")

religionNm <- fread("../data/religionNm.csv")

# for(i in 1:nrow(religionNm)) {
#     if(i<16) {religionNm$rowNo[i] <-1
#     } else {religionNm$rowNo[i] <- 2}
# }


religion <- merge(religion,religionNm,by.x="Group", by.y="ReligionId", all.x=TRUE)
setnames(religion, "religionNm", "GroupNm")
#setnames(religion, "Religion", "Group")

religion[, CED_NO:=gsub("CED","",CED_CODE_2016)]
religion[,dataset:="Religious Affiliation"]
religion<-religion[!grep("Tot",Group)]

fwrite(religion,"./data/religion.csv")

# Test Graphs
# ggplot(religion[!grep("Tot",Religion)],aes(x=religionNm, y=Persons)) +
#     geom_bar(stat="sum") +
#     facet_wrap("rowNo", scale="free_x", ncol=1) +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle=90),
#           legend.position="none")


# ggplot(religion,aes(x=Group, y=Persons)) +
#     geom_bar(stat="sum") +
#     theme(axis.text.x = element_text(angle=90))

# dcast(religion, Group ~., fun.aggregate = sum, value.var = "Persons")


# Education Level P38 #

# Voluntary Work for an Organisation or Group by Age by Sex P19 #

# Country of Birth of Person by Sex P09 #


# Registered Marital Status by Age by Sex P05 #

marriage1 <- fread("../data/2016_PEP_CED_for_AUS_short-header/2016 Census PEP Commonwealth Electoral Divisions for AUST/2016Census_P05_AUS_CED.csv")
marriage2 <- fread("../data/2016_PEP_CED_for_AUS_short-header/2016 Census PEP Commonwealth Electoral Divisions for AUST/2016Census_P06_AUS_CED.csv")

colF <- grepl("P_",names(marriage1)) & grepl("_Tot_",names(marriage1)) & !grepl("_Tot_Tot",names(marriage1))
marriage1 <- cbind(marriage1[,1],marriage1[,..colF])

colF <- grepl("P_",names(marriage2)) & grepl("_Tot_",names(marriage2)) & !grepl("_Tot_Tot",names(marriage2))
a<-c(1,grep("P_Tot_Marr_a_de_facto_marr",names(marriage2)))
marriage2 <- marriage2[,..a]

marriage <- merge(marriage1,marriage2,by="CED_CODE_2016")

marriage <- melt(marriage, id.vars = "CED_CODE_2016", value.name = "Persons", variable.name = "Group")

marriageNm <- fread("../data/marriageNm.csv")

marriage <- merge(marriage,marriageNm, by="Group")
marriage[, CED_NO:=gsub("CED","",CED_CODE_2016)]
marriage[,dataset:="Marriage Status"]

fwrite(marriage,"./data/marriage.csv")

# Test Graphs
# ggplot(marriage,aes(x=GroupNm, y=Persons)) +
#     geom_bar(stat="sum") +
#     theme(axis.text.x = element_text(angle=90))



# Social Marital Status by Age by Sex P06 #





