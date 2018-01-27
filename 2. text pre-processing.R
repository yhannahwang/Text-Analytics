
####################
#text preprocessing#
####################

# 1. Marina Bay Sands
MBS <- read.csv("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\reviews for Marina Bay Sands_total.csv",stringsAsFactors = FALSE)

content_MBS <- MBS$content_detail
content_MBS <- as.vector(content_MBS)
content_MBS <- gsub("\n","",content_MBS)
content_MBS <- gsub("<U.*B198>","",content_MBS)
content_MBS <- gsub("<U.*0099>","",content_MBS)
content_MBS <- gsub("<U.*B200>"," ",content_MBS)
content_MBS <- gsub("<U.*B209>","",content_MBS)
content_MBS <- gsub("<U.*B207>"," ",content_MBS)
content_MBS <- gsub("<U.*00C2>"," ",content_MBS)
content_MBS <- gsub("N/A","",content_MBS)
content_MBS <- gsub("\\.", "", content_MBS) 
content_MBS <- gsub("[[:digit:]]*", "", content_MBS) 
content_MBS <- gsub("//.*/ "," ",fixed = FALSE,content_MBS)
content_dt <- as.data.frame(content_MBS)
MBS[,6] <- content_dt 

setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\hotel data\\")
write.csv(MBS,"reviews for Marina Bay Sands.csv")


# 2. Crowne Plaza Changi Airport
CPCA <- read.csv("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\reviews for Crowne Plaza Changi Airport_total.csv",stringsAsFactors = FALSE)

content_CPCA <- CPCA$content_detail
content_CPCA <- as.vector(content_CPCA)
content_CPCA <- gsub("\n","",content_CPCA)
content_CPCA <- gsub("<U.*B198>","",content_CPCA)
content_CPCA <- gsub("<U.*0099>","",content_CPCA)
content_CPCA <- gsub("<U.*B200>"," ",content_CPCA)
content_CPCA <- gsub("<U.*B209>","",content_CPCA)
content_CPCA <- gsub("<U.*B207>"," ",content_CPCA)
content_CPCA <- gsub("<U.*00C2>"," ",content_CPCA)
content_CPCA <- gsub("N/A","",content_CPCA)
content_CPCA <- gsub("\\.", "", content_CPCA) 
content_CPCA <- gsub("[[:digit:]]*", "", content_CPCA) 
content_CPCA <- gsub("//.*/ "," ",fixed = FALSE,content_CPCA)
content_dfCPCA <- as.data.frame(content_CPCA)
CPCA[,6] <- content_dfCPCA 
head(CPCA)
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\hotel data\\")
write.csv(CPCA,"reviews for Crowne Plaza Changi Airport.csv")


# 3. PARKROYAL on Beach Road
PBR <- read.csv("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\reviews for PARKROYAL on Beach Road_total.csv",stringsAsFactors = FALSE)

content_PBR <- PBR$content_detail
content_PBR <- as.vector(content_PBR)
content_PBR <- gsub("\n","",content_PBR)
content_PBR <- gsub("<U.*B198>","",content_PBR)
content_PBR <- gsub("<U.*0099>","",content_PBR)
content_PBR <- gsub("<U.*B200>"," ",content_PBR)
content_PBR <- gsub("<U.*B209>","",content_PBR)
content_PBR <- gsub("<U.*B207>"," ",content_PBR)
content_PBR <- gsub("<U.*00C2>"," ",content_PBR)
content_PBR <- gsub("N/A","",content_PBR)
content_PBR <- gsub("\\.", "", content_PBR) 
content_PBR <- gsub("[[:digit:]]*", "", content_PBR) 
content_PBR <- gsub("//.*/ "," ",fixed = FALSE,content_PBR)
content_dfPBR <- as.data.frame(content_PBR)
PBR[,6] <- content_dfPBR
head(PBR)
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\hotel data\\")
write.csv(PBR,"reviews for PARKROYAL on Beach Road.csv")


# 4. Siloso Beach Resort Sentosa
SBRS <- read.csv("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\reviews for Siloso Beach Resort Sentosa_total.csv",stringsAsFactors = FALSE)

content_SBRS <- SBRS$content_detail
content_SBRS <- as.vector(content_SBRS)
content_SBRS <- gsub("\n","",content_SBRS)
content_SBRS <- gsub("<U.*B198>","",content_SBRS)
content_SBRS <- gsub("<U.*0099>","",content_SBRS)
content_SBRS <- gsub("<U.*B200>"," ",content_SBRS)
content_SBRS <- gsub("<U.*B209>","",content_SBRS)
content_SBRS <- gsub("<U.*B207>"," ",content_SBRS)
content_SBRS <- gsub("<U.*00C2>"," ",content_SBRS)
content_SBRS <- gsub("N/A","",content_SBRS)
content_SBRS <- gsub("\\.", "", content_SBRS) 
content_SBRS <- gsub("[[:digit:]]*", "", content_SBRS) 
content_SBRS <- gsub("//.*/ "," ",fixed = FALSE,content_SBRS)
content_dfSBRS <- as.data.frame(content_SBRS)
SBRS[,6] <- content_dfSBRS
head(SBRS)
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\hotel data\\")
write.csv(SBRS,"reviews for Siloso Beach Resort Sentosa.csv")

# 5. Ibis Singapore on Bencoolen
ISB <- read.csv("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\reviews for Ibis Singapore on Bencoolen_total.csv",stringsAsFactors = FALSE)

content_ISB <- ISB$content_detail
content_ISB <- as.vector(content_ISB)
content_ISB <- gsub("\n","",content_ISB)
content_ISB <- gsub("<U.*B198>","",content_ISB)
content_ISB <- gsub("<U.*0099>","",content_ISB)
content_ISB <- gsub("<U.*B200>"," ",content_ISB)
content_ISB <- gsub("<U.*B209>","",content_ISB)
content_ISB <- gsub("<U.*B207>"," ",content_ISB)
content_ISB <- gsub("<U.*00C2>"," ",content_ISB)
content_ISB <- gsub("N/A","",content_ISB)
content_ISB <- gsub("\\.", "", content_ISB) 
content_ISB <- gsub("[[:digit:]]*", "", content_ISB) 
content_ISB <- gsub("//.*/ "," ",fixed = FALSE,content_ISB)
content_dfISB <- as.data.frame(content_ISB)
ISB[,6] <- content_dfISB
head(ISB)
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\hotel data\\")
write.csv(ISB,"reviews for Ibis Singapore on Bencoolen.csv")


# 6. Destination Singapore Beach Road
DSB <- read.csv("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\reviews for Destination Singapore Beach Road_total.csv",stringsAsFactors = FALSE)

content_DSB <- DSB$content_detail
content_DSB <- as.vector(content_DSB)
content_DSB <- gsub("\n","",content_DSB)
content_DSB <- gsub("<U.*B198>","",content_DSB)
content_DSB <- gsub("<U.*0099>","",content_DSB)
content_DSB <- gsub("<U.*B200>"," ",content_DSB)
content_DSB <- gsub("<U.*B209>","",content_DSB)
content_DSB <- gsub("<U.*B207>"," ",content_DSB)
content_DSB <- gsub("<U.*00C2>"," ",content_DSB)
content_DSB <- gsub("N/A","",content_DSB)
content_DSB <- gsub("\\.", "", content_DSB) 
content_DSB <- gsub("[[:digit:]]*", "", content_DSB) 
content_DSB <- gsub("//.*/ "," ",fixed = FALSE,content_DSB)
content_dfDSB <- as.data.frame(content_DSB)
DSB[,6] <- content_dfDSB
head(DSB)
setwd("C:\\Users\\Yhannah\\Desktop\\CLASS\\Analytics Workshop\\text mining\\booking\\hotel data\\")
write.csv(DSB,"reviews for Destination Singapore Beach Road.csv")

