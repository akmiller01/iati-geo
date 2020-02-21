list.of.packages <- c("data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-annual-report-2020/output/"
setwd(wd)

agg <- fread("iati_unfiltered_agg.csv")
dagg <- fread("iati_unfiltered_disagg.csv")

transactions.aggregate <- subset(agg,budget_or_transaction=="Transaction")
budgets.aggregate <- subset(agg,budget_or_transaction=="Budget")

transactions.aggregate.2019 <- subset(transactions.aggregate,year==2019)
trans.2019.tab <- data.table(transactions.aggregate.2019)[,.(value=sum(usd_disbursement,na.rm=TRUE)),by=.(publisher,year)]

budgets.aggregate.19.20.21 <- subset(budgets.aggregate,year %in% c(2019,2020,2021))

bud.19.20.21.tab <- data.table(budgets.aggregate.19.20.21)[,.(value=sum(usd_disbursement,na.rm=TRUE)),by=.(publisher,year)]

iati_members <- c(
  "Ghana"
  ,"Liberia"
  ,"Nigeria"
  ,"Rwanda"
  ,"Sierra Leone"
  ,"Somalia"
  ,"Tanzania"
  ,"Malawi"
  ,"Burkina Faso"
  ,"Burundi"
  ,"Madagascar"
  ,"Benin"
  ,"Democratic Republic of the Congo"
  ,"Congo"
  ,"Guinea"
  ,"Mali"
  ,"Bangladesh"
  ,"Indonesia"
  ,"Myanmar"
  ,"Nepal"
  ,"Viet Nam"
  ,"Colombia"
  ,"Honduras"
  ,"Dominican Republic"
  ,"Montenegro"
  ,"Moldova"
  ,"Lebanon"
  ,"Syrian Arab Republic"
  ,"Yemen"
  ,"Papua New Guinea"
  ,"Sao Tome and Principe"
)

iati_member_codes <- c(
  # Ghana
  "GH" #ISO2
  ,"GHA" #ISO3
  ,"241" #CRS
  # Liberia
  ,"LR"
  ,"LBR"
  ,"251"
  # Nigeria
  ,"NG"
  ,"NGA"
  ,"261"
  # Rwanda
  ,"RW"
  ,"RWA"
  ,"266"
  # Sierra Leone
  ,"SL"
  ,"SLE"
  ,"272"
  # Somalia
  ,"SO"
  ,"SOM"
  ,"273"
  # Tanzania
  ,"TZ"
  ,"TZA"
  ,"282"
  # Malawi
  ,"MW"
  ,"MWI"
  ,"253"
  # Burkina Faso
  ,"BF"
  ,"BFA"
  ,"287"
  # Burundi
  ,"BI"
  ,"BDI"
  ,"228"
  # Madagascar
  ,"MG"
  ,"MDG"
  ,"252"
  # Benin
  ,"BJ"
  ,"BEN"
  ,"236"
  # Democratic Republic of Congo
  ,"CD"
  ,"COD"
  ,"235"
  # Republic of the Congo
  ,"CG"
  ,"COG"
  ,"234"
  # Guinea
  ,"GN"
  ,"GIN"
  ,"243"
  # Mali
  ,"ML"
  ,"MLI"
  ,"255"
  # Bangladesh (Vice Chair)
  ,"BD"
  ,"BGD"
  ,"666"
  # Indonesia
  ,"ID"
  ,"IDN"
  ,"738"
  # Myanmar
  ,"MM"
  ,"MMR"
  ,"635"
  # Nepal
  ,"NP"
  ,"NPL"
  ,"660"
  # Vietnam
  ,"VN"
  ,"VNM"
  ,"769"
  # Colombia
  ,"CO"
  ,"COL"
  ,"437"
  # Honduras
  ,"HN"
  ,"HND"
  ,"351"
  # Dominican Republic
  ,"DO"
  ,"DOM"
  ,"340"
  # Montenegro
  ,"ME"
  ,"MNE"
  ,"65"
  # Moldova
  ,"MD"
  ,"MDA"
  ,"93"
  # Lebanon
  ,"LB"
  ,"LBN"
  ,"555"
  # Syria
  ,"SY"
  ,"SYR"
  ,"573"
  # Yemen
  ,"YE"
  ,"YEM"
  ,"580"
  # Papua New Guinea
  ,"PG"
  ,"PNG"
  ,"862"
  # São Tomé and Príncipe
  ,"ST"
  ,"STP"
  ,"268"
)

recode_iati_members <- function(x){
  return(iati_members[ceiling(which(iati_member_codes==x)/3)])
}

transactions.disaggregate <- subset(dagg,budget_or_transaction=="Transaction")
transactions.disaggregate.17.18.19 <- subset(transactions.disaggregate,year %in% c(2017,2018,2019))

transactions.disaggregate.17.18.19$recipient_code <- toupper(transactions.disaggregate.17.18.19$recipient_code)
transactions.disaggregate.17.18.19 <- subset(transactions.disaggregate.17.18.19,recipient_code %in% iati_member_codes)
transactions.disaggregate.17.18.19$recipient <- sapply(transactions.disaggregate.17.18.19$recipient_code,recode_iati_members)

transactions.disaggregate.17.18.19$publisher[which(transactions.disaggregate.17.18.19$publisher=="usaid")] = "unitedstates"
transactions.disaggregate.17.18.19$publisher[which(transactions.disaggregate.17.18.19$publisher=="ec-echo")] = "ec-devco"
transactions.disaggregate.17.18.19$publisher[which(transactions.disaggregate.17.18.19$publisher=="ec-near")] = "ec-devco"

trans.recip.donor.tab <- data.table(transactions.disaggregate.17.18.19)[,.(value=sum(usd_disbursement,na.rm=TRUE)),by=.(recipient,publisher,year)]

trans.recip.max <- trans.recip.donor.tab[,.(value=max(value)),by=.(recipient,publisher)]

trans.recip.max <- merge(trans.recip.max,trans.recip.donor.tab,all.x=TRUE)
setnames(trans.recip.max,"year","iati.year")
setnames(trans.recip.max,"value","iati.value")

# exclude <- c("abt","akfuk73","dec-uk","palladium","plan_usa","spuk","wwf-uk")
exclude = c("wwf-uk")
trans.recip.max <- subset(trans.recip.max,!(publisher %in% exclude))

crs <- fread("crs.csv")
names(crs) = make.names(names(crs))

crs <- subset(crs,Recipient %in% iati_members & Amount.type=="Current Prices")
crs$value <- crs$Value*1000000
setnames(crs,"Recipient","recipient")
setnames(crs,"Donor","donor")
setnames(crs,"Year","year")
setnames(crs,"Value","value")
keep <- c("recipient","donor","year","value")
crs <- data.table(crs)[,keep,with=F]

vague_donors <- c(
  "All Donors, Total"
  ,"DAC Countries, Total"
  ,"Multilaterals, Total"
  ,"Non-DAC Countries, Total"
  ,"Memo: Private Donors, Total"
  ,"G7 Countries, Total"
  ,"DAC EU Members, Total"
  ,"DAC EU Members + EC, Total"
  ,"Other Multilateral, Total"
  ,"Regional Development Banks, Total"
  ,"Official Donors, Total"
  ,"Private Donors, Total"
  #Duplicates
  ,"International Development Association [IDA]"
  ,"World Bank, Total"
  ,"AsDB Special Funds"
  ,"African Development Fund [AfDF]"
  ,"African Development Bank [AfDB]"
  ,"IMF (Concessional Trust Funds)"
  ,"United Nations, Total"
  ,"IDB Special Fund"
  ,"Asian Development Bank [AsDB]"
  ,"IDB Invest"
  )

crs <- subset(crs,!donor %in% vague_donors)
crs = crs[,.(value=sum(value,na.rm=T)),by=.(recipient,donor,year)]

crs <- crs[order(crs$recipient,-crs$value),]
crs.top15 <- data.table(crs)[,head(.SD,15),by="recipient"]

trans.recip.max <- trans.recip.max[order(trans.recip.max$recipient,-trans.recip.max$iati.value),]
trans.recip.top15 <- trans.recip.max[,head(.SD,15),by="recipient"]

publisher.dict <- c(
  "worldbank"="World Bank Group, Total"    
  ,"asdb"="Asian Development Bank, Total"         
  ,"dfid"="United Kingdom"         
  ,"bmz"="Germany"           
  ,"usaid"="United States"        
  ,"ec-devco"="EU Institutions"     
  ,"ifad"="IFAD"         
  ,"theglobalfund"="Global Fund"
  ,"ausgov"="Australia"       
  ,"minbuza_nl"="Netherlands"   
  ,"unitedstates"="United States" 
  ,"afdb"="African Development Bank, Total"         
  ,"be-dgd"="Belgium"       
  ,"undp"="UNDP"         
  ,"danida"="Denmark"       
  ,"ec-echo"="EU Institutions"      
  ,"gac-amc"="Canada"      
  ,"gavi"="Global Alliance for Vaccines and Immunization [GAVI]"         
  ,"wfp"="WFP"          
  ,"iadb"="Inter-American Development Bank, Total"         
  ,"norad"="Norway"        
  ,"sida"="Sweden"         
  ,"cerf"="Central Emergency Response Fund [CERF]"         
  # ,"fao"="FAO"          
  ,"unfpa"="UNFPA"        
  ,"ec-near"="EU Institutions"      
  ,"afd"="France"          
  ,"ebrd"="European Bank for Reconstruction and Development [EBRD]"         
  # ,"idlo"="IDLO"         
  ,"mfat"="New Zealand"
  ,"gavi"="Global Alliance for Vaccines and Immunization [GAVI]"
  ,"jica"="Japan"
  ,"bmgf"="Bill & Melinda Gates Foundation"
  ,"finland_mfa"="Finland"
  ,"theglobalfund"="Global Fund"
  ,"aics"="Italy"
  ,"odakorea"="Korea"
  ,"ofid"="OPEC Fund for International Development [OFID]"
  ,"uasd"="Romania"
  ,"maec"="Spain"
  ,"sdc_ch"="Switzerland"
  ,"unicef"="UNICEF"
  ,"cif"="Climate Investment Funds [CIF]"
  ,"who"="World Health Organisation [WHO]"
  ,"irishaid"="Ireland"
  ,"ilo"="International Labour Organisation [ILO]"
  ,"cprojects"="Charity Projects Ltd (Comic Relief)"
  ,"slovakaid"="Slovak Republic"
  ,"unaids"="UNAIDS"
  ,"lithuania_mfa"="Lithuania"
  ,"hewlett-foundation"="William & Flora Hewlett Foundation"
  ,"unhcr"="UNHCR"
  ,"iadb"="Inter-American Development Bank [IDB]"
  ,"gggi"="Global Green Growth Institute [GGGI]"
  ,"omidyarnetwork"="Omidyar Network Fund, Inc."
  ,"af"="Adaptation Fund"
)

trans.recip.max$donor = publisher.dict[trans.recip.max$publisher]

setnames(crs,"value","crs.value")
setnames(crs,"year","crs.year")
joint.donors <- merge(crs,trans.recip.max,by=c("recipient","donor"),all=TRUE)
joint.donors$value <- pmax(joint.donors$crs.value,joint.donors$iati.value,na.rm=TRUE)
joint.donors <- joint.donors[order(joint.donors$recipient,-joint.donors$value),]

joint.donors <- joint.donors[order(joint.donors$recipient,-joint.donors$value),]
joint.top15 <- data.table(joint.donors)[,head(.SD,15),by="recipient"]
joint.top15$publishing.to.iati <- as.numeric(!is.na(joint.top15$iati.value))
joint.top15$donor.or.publisher <- joint.top15$donor
joint.top15$donor.or.publisher[which(is.na(joint.top15$donor.or.publisher))] <- joint.top15$publisher[which(is.na(joint.top15$donor.or.publisher))]

write.csv(transactions.aggregate.2019,"transactions_2019.csv",na="",row.names=FALSE)
write.csv(trans.2019.tab,"transactions_2019_by_publisher.csv",na="",row.names=FALSE)
write.csv(budgets.aggregate.19.20.21,"budgets_192021.csv",na="",row.names=FALSE)
write.csv(bud.19.20.21.tab,"budgets_192021_by_publisher.csv",na="",row.names=FALSE)
write.csv(transactions.disaggregate.17.18.19,"transactions_171819_disaggregated.csv",na="",row.names=FALSE)
write.csv(trans.recip.top15,"transactions_by_recipient.csv",na="",row.names=FALSE)
write.csv(crs.top15,"crs_by_recipient.csv",na="",row.names=FALSE)
write.csv(joint.top15,"merged_by_recipient.csv",na="",row.names=FALSE)
