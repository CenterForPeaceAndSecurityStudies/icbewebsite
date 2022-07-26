
flag_folder_small="/mnt/8tb_a/rwd_github_private/icbewebsite/flags_small/"

knitr::opts_chunk$set(echo = F, echo=F, cache=F, results='hide', message=F, warning=F) #Global options

#library(pacman)
library(googlesheets4)
library(tidyverse)
library(janitor)
library(flextable)
library(ggplot2)
library(cowplot)
library(ggtext)
#set_flextable_defaults(fonts_ignore=TRUE) #Warning: fonts used in `flextable` are ignored because the `pdflatex` engine is used and not `xelatex` or `lualatex`. You can avoid this warning by using the `set_flextable_defaults(fonts_ignore=TRUE)` command or use a compatible engine by defining `latex_engine: xelatex` in the YAML header of the R Markdown document.
library(ftExtra)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
options(tidyverse.quiet = TRUE)
options(gargle_oauth_email = TRUE)

#install.packages('stargazer')
#install.packages('kableExtra')
#! sh: 1: pdflatex: not found
#sudo apt-get install texlive-latex-recommended texlive-fonts-recommended

#! LaTeX Error: File `letltxmacro.sty' not found.
#sudo apt-get install texlive-latex-extra

library(pacman)
library(igraph)
library(tidyverse)
library(flextable)
library(ftExtra)

#pip install -U pip setuptools wheel
#pip install -U spacy
#python -m spacy download en_core_web_sm
library(rvest)

icb_long_clean <- readRDS(file=paste0(here::here(), "/../ICBEdataset/replication_data/out/ICBe_V1.1_long_clean.Rds")) #assumes you've pulled ICBEdataset v1.1 to the same main folder

ICBe_events_agreed <- readRDS( paste0(here::here(), "/../ICBEdataset/replication_data/out/ICBe_V1.1_events_agreed.Rds"))
dim(ICBe_events_agreed)

icb_events_long <- readRDS( paste0(here::here(), "/../ICBEdataset/replication_data/out/ICBe_V1.1_events_agreed_long.Rds"))
dim(icb_events_long)

ICBe_crises_markdown <- readRDS( paste0(here::here(), "/../ICBEdataset/replication_data/out/ICBe_V1.1_crises_markdown.Rds"))

ICBe_events_agreed_markdown <- readRDS( paste0(here::here(), "/../ICBEdataset/replication_data/out/ICBe_V1.1_events_agreed_markdown.Rds"))

#oh is it because break is reserved in R? lol
#icb_crises <- read_csv(file=paste0(here::here(), "/data_other_datasets/icb/icb1v14.csv")) %>% janitor::clean_names()
icb_crises  <- read.csv(paste0(here::here(), "/../ICBEdataset/replication_data/in/icb1v14.csv")) %>% janitor::clean_names()
icb_crises$start_simple <- icb_crises$yrtrig+(icb_crises$motrig/12)
icb_crises$end_simple <- icb_crises$yrterm+(icb_crises$moterm/12)

#somehow break of all things is a b roken name
#icb_actors <- read_csv(file=paste0(here::here(), "/data_other_datasets/icb/icb2v14.csv"))
icb_actors <- read.csv(paste0(here::here(), "/../ICBEdataset/replication_paper/data/in/icb2v14.csv"))

#The answer is I have to include it as an image, none of the table options were compiling correctly without error


# Common Functions ----------------

global_check_overlap=F
library(igraph)
library(RColorBrewer)
library(ggraph)
target_file <- paste0(here::here(),"/../ICBEdataset/replication_paper/data/in/icb_manual_recoding_master_sheet.xlsx")
dictionary_actors_labels    <- readxl::read_excel(target_file, sheet="actors") %>%
  dplyr::select(crisno, value_normalized=value_normalized_wikidata_id, value_normalized_label) %>% dplyr::distinct() %>% na.omit()

actor_translator <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1a7Id0Zg41PTKEv74H_KjiJzhMak2jGh0leT7B8oVWdI/edit?usp=sharing",
                                              sheet="actor_translator") #%>% janitor::clean_names()
#Convert to a local file for pushing
#actor_translator    <- readxl::read_excel(target_file, sheet="actor_translator") #Ya we just add colors to the actor translator and we're good
pallet <- c(brewer.pal(n=20, "Dark2"), brewer.pal(n=20, "Set1"), brewer.pal(n=20, "Set2"), brewer.pal(n=20, "Paired")) %>% unique()
library(pals); #install.packages('pals')
pallet <- pals::glasbey()
pallet <- setdiff(pallet, c("#666666", "#999999", "#FFFF99","#FFFF33","#B2DF8A","#B3B3B3","#0000FF"))
library(colorspace)
#cols <- c("#CDE4F3","#E7F3D3","#F7F0C7","#EFCFE5","#D0D1E7")
pallet <- rgb(hex2RGB(pallet)@coords * 0.7)
actor_translator$actor_color = rep(pallet, length=nrow(actor_translator))

crisno_name_years <- icb_crises %>% dplyr::select(crisno, crisname, yrtrig, yrterm)



CAMEO_eventcodes <- read_tsv(paste0(here::here(), "/../ICBEdataset/replication_paper/data/in/CAMEO.eventcodes.txt"), col_types='cccc') %>% janitor::clean_names()

cameo_role_codes <- data.frame(
  stringsAsFactors = FALSE,
  role_code = c("GOV","SPY","OPP",
                "MIL","PTY","REB","EDU","COP","JUD","CRM",
                "BUS","MED","CVL","UAF","AGR","ELI","DEV","ENV",
                "HLH","HRI","LAB","LEG","REF","MOD","RAD"),
  role_code_label = c("Government",
                      "Intelligence agencies",
                      "Opposition groups (often parliamentary)",
                      "Military",
                      "Political party",
                      "Rebels/insurgents",
                      "Education-focused actors",
                      "Law enforcement agencies",
                      "Judicial actors",
                      "Criminal actors",
                      "Business (NOT MNCs)",
                      "Media-related actors/organizations",
                      "Civilians",
                      "Unaffiliated armed forces",
                      "Agriculture-focused actors",
                      "Elites, celebrities",
                      "Development-focused actors",
                      "Environment-focused actors",
                      "Health-focused actors",
                      "Human rights-focused actors",
                      "Labor-focused actors",
                      "Legislative bodies/actors",
                      "Refugee-focused actors",
                      "Moderate/mainstream group",
                      "Radical/extremist/fundamentalist")
) %>% janitor::clean_names()


subject_rejects <- c(
  "ACTIVISM,1" , "ACTIVISM,2" , "AGRICULTURE,1" , "AGRICULTURE,2" , "AGRICULTURE,3" , "AGRICULTURE,4" ,
  "BAILOUT,1" , "BANKRUPTCY,1" ,
  "BLACK_MARKET,1" , "CHECKPOINT,1" ,
  "CIVIL_SOCIETY,1" , "CLIMATE_CHANGE,1" , "COAL,1" , "COAL,2" , "COAL,3" , "COMMON_ROBBERY,1" , "CORRUPTION,1" , "CORRUPTION,2"     ,
  "DISCRIMINATION,1" , "DISEASE,1" , "DISEASE,2" , "DISEASE,3" , "DISEASE,4" ,
  "DRUG_CARTELS,1" , "DRUG_TRADE,1" , "DRUGS,1" , "DRUGS,2" , "DRUGS,3" ,
  "EXCHANGE_RATE,1" , "FOOD_SECURITY,1" ,
  "HOUSING_PRICES,1" ,
  "ELECTRICITY_OUTAGE,1" ,
  "INT_FINANCIAL_INST,1" , "INT_FINANCIAL_INST,2" , "INTEREST_RATES,1" , "INTEREST_RATES,2" , "LABOR_STRIKE,1" ,
  "LABOR_UNIONS,1" , "LABOR_UNIONS,2" , "LABOR_UNIONS,4" , "LANDMINE,1" , "LGBT,1" , "LITERACY,1" , "MEDIA_CENSORSHIP,1" , "MONOPOLY,1" ,
  "MONOPOLY,2" ,
  "ORGANIZED_CRIME,1" ,
  "POSTSECONDARY_EDUCATION,1" , "POSTSECONDARY_EDUCATION,2" , "POSTSECONDARY_EDUCATION,3" , "POVERTY,1" , "PRIVATIZATION,1" , "PRIVATIZATION,2" ,
  "PUBLIC_TRANSIT,1" , "PUBLIC_TRANSIT,2" , "PUBLIC_TRANSIT,3" , "RAPE,1" , "REFUGEES,1" , "REFUGEES,2" , "REFUGEES,3" , "RENEWABLE_ENERGY,1" ,
  "SCHOOLS,1" , "SCHOOLS,2" , "SCHOOLS,3" ,
  "SEXTRANS_DISEASE,1" , "SHORTAGE,1" , "SOCIAL_MOVEMENT,1" , "STUDENTS,1" , "STUDENTS,2" , "STUDENTS,3" , "SUBSIDIES,1" ,
  "TEACHERS,1" , "TEACHERS,2" ,
  "UNEMPLOYMENT,1" , "VACCINATION,1" ,
  "VACCINATION,2" , "YOUTH_MOVEMENT,1"
)

cameo_actor_codes <- data.frame(
  stringsAsFactors = FALSE,
  code = c("ABN","ABW","ACC",
           "ADB","AEA","AEU","AFB","AFG",
           "AFGGOVTAL","AFGREBTAL","AFR","AFU",
           "AGO","AGOCAB","AGOREBUNI","AGR",
           "AGR","AIA","AIE","ALA","ALB",
           "ALQ","AMN","AMU","AND","ANT",
           "APE","ARB","ARB","ARBBTH","ARE",
           "ARG","ARL","ARM","ASA","ASA",
           "ASF","ASM","ASN","ATG","ATH",
           "ATH","AUS","AUT","AZE","BAH",
           "BCA","BDI","BEL","BEN","BFA",
           "BGD","BGR","BHR","BHS","BIH","BIH",
           "BIHBHF","BIHSRP","BIS","BLK",
           "BLR","BLZ","BMU","BOL","BRA",
           "BRB","BRN","BTN","BUD","BUD",
           "BUS","BUS","BUS","BWA","CAF",
           "CAN","CAS","CAU","CEM","CEU",
           "CFR","CHE","CHL","CHN","CHNTIC",
           "CHR","CHR","CHRCPT","CHRCTH",
           "CHRDOX","CHRJHW","CHRLDS","CHRMRN",
           "CHRPRO","CHRRAD","CIS","CIV",
           "CIV","CMN","CMR","CNY","COD",
           "COD Bangladesh","COE","COG","COG",
           "COK","COL","COL","COM","CON",
           "CON","COP","CPC","CPV","CRB",
           "CRI","CRM","CRM","CRO","CSK",
           "CSS","CTH","CUB","CVL","CVL",
           "CVL","CWN","CYM","CYP","CYPGRK",
           "CYPTRK","CZA","CZE","DDR","DEU",
           "DEV","DEV","DJI","DMA","DNK",
           "DOM","DZA","DZAGOVFLN",
           "DZAGOVMSP","DZAGOVRND","DZAOPPENN",
           "DZAOPPFIS","DZAOPPFLN","DZAOPPMSP",
           "DZAREBFIS","DZAREBGIA","DZAREBGSP",
           "EAC","EAF","EBR","ECU","EDU",
           "EDU","EEC","EEU","EFT","EGY",
           "EGYREBMBR","EIN","EIN","ELI","ELI",
           "ENV","ENV","ENV","ERI","ESH",
           "ESP","ESPBSQ","EST","ETH",
           "EUR","FID","FIN","FJI","FLK","FLK",
           "FRA","FRO","FRY","FRYKSV",
           "FRYMTN","FRYSRB","FRYVVD","FSM",
           "FSM","GAB","GBR","GBRREBIRA",
           "GEO","GGY","GHA","GIB","GIN","GLP",
           "GMB","GME","GMW","GNB","GNQ",
           "GOS","GOV","GOV","GRC","GRD",
           "GRL","GTM","GUF","GUM","GUY",
           "GYP","GZA","HCH","HCR","HIN",
           "HIN","HKG","HKG","HLH","HLH",
           "HMS","HND","HRI","HRV","HRV",
           "HRW","HTI","HUN","HUT","IAD","IAE",
           "ICC","ICG","ICJ","ICO","IDB",
           "IDN","IGC","IGO","IGO",
           "IGOAFRAFU","IGOAFRAGRIAC","IGOAFRBUSCES",
           "IGOAFRBUSCFA","IGOAFRDEVAFB",
           "IGOAFRDEVATD","IGOAFRDEVNEP",
           "IGOAFROAU","IGOAFRPAP","IGOAGRCPA",
           "IGOAGRCPC","IGOAGRICO","IGOAGRIGC",
           "IGOARBAPE","IGOARBDEVABD",
           "IGOBUSBIS","IGOBUSGOE","IGOBUSGOS",
           "IGOBUSGSS","IGOBUSHIP","IGOBUSIMF",
           "IGOBUSOPC","IGOBUSPRC","IGOBUSWTO",
           "IGOCAFBCA","IGOCAFCEM",
           "IGOCAFECA","IGOCASCIS","IGOCOPITP",
           "IGOCWN","IGOEAFDEVIAD","IGOEAFEAC",
           "IGOEURBUSEFT","IGOEURCOE",
           "IGOEURDEVEBR","IGOEUREEC","IGOEURSCE",
           "IGOJUDICC","IGOLEGIPU","IGOMEAACC",
           "IGOMEAAEU","IGOMEAAMF",
           "IGOMEAAMU","IGOMEAARL","IGOMOSDEVIDB",
           "IGOMOSOIC","IGONAFCSS","IGONON",
           "IGOOAS","IGOPGSGCC","IGOPKO",
           "IGOSAFDEVSAD","IGOSASSAA","IGOSEAASN",
           "IGOSEADEVADB","IGOSEASOT","IGOUNO",
           "IGOUNOAGRFAO","IGOUNOAIE",
           "IGOUNODEVWBK","IGOUNOHLHWHO",
           "IGOUNOHRIHCH","IGOUNOIAE","IGOUNOJUDICJ",
           "IGOUNOJUDWCT","IGOUNOKID",
           "IGOUNOLABILO","IGOUNOREFHCR","IGOUNOWFP",
           "IGOWAFDEVWAM","IGOWAFUEM",
           "IGOWAFWAD","IGOWAFWAS","IGOWEU",
           "IGOWSTNAT","IHF","ILO","IMF","IMG",
           "IMGMOSALQ","IMGSEAMOSASF",
           "IMGSEAMOSJMA","IMY","IND","INDKAS",
           "INS","INT","IPU","IRC","IRL",
           "IRN","IRQ","IRQBAG","IRQKURKDP",
           "ISI","ISL","ISR","ISRGOVCMN",
           "ISRGOVLBA","ISRGOVLKD","ISRGOVMRZ",
           "ISRGOVSHA","ISROPPLBA","ISROPPLKD",
           "ISROPPMRZ","ISROPPSHA","ISRSET",
           "ITA","ITP","JAM","JAN","JAN",
           "JEW","JEW","JEWHSD","JEWUDX",
           "JMA","JOR","JOROPPIAF","JPN",
           "JUD","JUD","JUR","KAS","KAZ","KEN",
           "KGZ","KHM","KHMREBKMR","KID",
           "KIR","KNA","KNA","KOR","KOR",
           "KSV","KUR","KWT","LAB","LAB",
           "LAM","LAO","LBN","LBNREBAML",
           "LBNREBASL","LBNREBHEZ","LBR",
           "LBRBOM","LBRBON","LBRCAP","LBRGBA",
           "LBRGGC","LBRGOVLAP","LBRGOVNDP",
           "LBRGOVNPF","LBRGOVUPP","LBRKRH",
           "LBRKRU","LBRLOF","LBRMAN","LBRMNT",
           "LBRMRG","LBRMRY","LBRNIM",
           "LBROPPALC","LBROPPLAP","LBROPPNDM",
           "LBROPPNDP","LBROPPUPP","LBRREBAFL",
           "LBRREBINP","LBRREBLPC",
           "LBRREBLUR","LBRREBNPF","LBRREBULM",
           "LBRRVC","LBRSIN","LBY","LCA","LEG",
           "LEG","LIE","LKA","LKAREBJVP",
           "LSO","LTU","LUX","LVA","MAC",
           "MAR","MARREBPLS","MCO","MDA","MDE",
           "MDG","MDT","MDV","MEA","MED",
           "MED","MEX","MHL","MIL","MIL",
           "MKD","MLI","MLT","MMR","MMR",
           "MNC","MNC","MNE","MNG","MNP",
           "MOD","MOD","MOS","MOS","MOSALE",
           "MOSDRZ","MOSRAD","MOSSFI",
           "MOSSHI","MOSSUN","MOZ","MRT","MSF",
           "MSR","MTN","MTQ","MUS","MWI",
           "MYS","MYT","NAF","NAM","NAT",
           "NCL","NER","NFK","NGA","NGAABI",
           "NGAABU","NGAADA","NGAAKI","NGAANB",
           "NGABAU","NGABAY","NGABIA",
           "NGABNU","NGABOR","NGACRR","NGADEL",
           "NGAEBO","NGAEKI","NGAENU",
           "NGAGOM","NGAHAU","NGAIBO","NGAIJW",
           "NGAIMO","NGAJIG","NGAKAD","NGAKAN",
           "NGAKAT","NGAKEB","NGAKOG",
           "NGAKWA","NGALAG","NGANAS","NGANDR",
           "NGANGR","NGANNG","NGAOGO",
           "NGAOGU","NGAOND","NGAOPPANP",
           "NGAOPPCFD","NGAOPPNDC","NGAOSU","NGAOYO",
           "NGAPLA","NGAREBMAD","NGARIV",
           "NGASOK","NGATAR","NGATIV","NGAYOB",
           "NGAYRB","NGAZAM","NGM",
           "NGMGRP","NGO","NGO","NGOCHRCSI",
           "NGOHLHCRC","NGOHLHIRC","NGOHLHMSF",
           "NGOHLHRCR","NGOHRIAMN","NGOHRIFID",
           "NGOHRIHRW","NGOHRIIHF","NGOICG",
           "NGOJUDJUR","NGOREFIOM","NGOUAJ",
           "NGOWEF","NGOXFM","NIC","NIU",
           "NIU","NLD","NMR","NOR","NPL",
           "NRU","NZL","OAS","OAU","OCC",
           "OIC","OMN","OPC","OPP","OPP","PAG",
           "PAK","PAKKAS","PAL","PALPLO",
           "PALREBANO","PALREBPLF","PAN",
           "PCN","PER","PGS","PHL","PLW",
           "PNG","PNGBOU","POL","PPL","PRI",
           "PRK","PRK","PRO","PRT","PRY",
           "PSE","PSE","PSEGOVFTA","PSEGOVHMS",
           "PSEGZS","PSEREBAAM","PSEREBDFL",
           "PSEREBHMS","PSEREBISJ",
           "PSEREBPFL","PSEWSB","PTY","PYF","QAT",
           "RAD","RAD","REB","REB","REB",
           "REF","REF","REL","REU","ROM",
           "ROU","RUS","RUSCNY","RWA",
           "RWAGOVRPF","RWAUAFRPF","SAA","SAD",
           "SAF","SAM","SAS","SAU","SCG","SCG",
           "SCGKSV","SCGMTN","SCGSRB",
           "SCGVVD","SCN","SDN","SDNDFR",
           "SDNREBNDA","SDNREBSPL","SEA","SEN",
           "SENREBMDF","SER","SGP","SHI",
           "SHN","SHN","SIK","SIK","SJM","SLA",
           "SLB","SLE","SLEREBKAM",
           "SLEREBRUF","SLV","SMR","SNL","SOM",
           "SOT","SPM","SPY","SPY","SRB",
           "SRB","SRBKSV","SRBVVD","SRP","SSD",
           "STP","SUN","SUR","SVK","SVN",
           "SWE","SWZ","SYC","SYR","TAM",
           "TAM","TAO","TAO","TCA","TCD",
           "TER","TGO","THA","TJK","TKL",
           "TKM","TMP","TMP","TON","TRG",
           "TRK","TTO","TUN","TUR","TURANK",
           "TURGOVAKP","TURGOVANP","TURGOVCHP",
           "TURGOVDSP","TURGOVDYP",
           "TURGOVMHP","TURGOVREP","TURIST","TURIZM",
           "TUROPPAKP","TUROPPANP",
           "TUROPPCHP","TUROPPDSP","TUROPPDTP",
           "TUROPPDYP","TUROPPFAZ","TUROPPHDP",
           "TUROPPMHP","TUROPPREP","TURREBDSL",
           "TURREBPKK","TURSOE","TUT","TUV",
           "TWN","TZA","UAF","UAF","UER",
           "UGA","UGAREBADF","UGAREBLRA",
           "UIG","UIG","UIS","UKR","UNO",
           "UNR","URY","USA","USR","UZB",
           "VAT","VCT","VEN","VGB","VIR","VIR",
           "VNM","VNM","VUT","WAD","WAF",
           "WAS","WBK","WCT","WEF","WFP",
           "WHO","WLF","WSM","WST","WST",
           "WTO","XFM","YEM","YMN","YMS",
           "YUG","YUG","YUGBSN","YUGCTA",
           "YUGKSV","YUGMCD","YUGMTN","YUGSLN",
           "YUGSRB","YUGVVD","ZAF","ZMB",
           "ZRO","ZWE"),
  name = c("ethnic Albanian",
           "Aruba","Arab Cooperation Council",
           "Asian Development Bank",
           "Association of Southeast Asian Nations Defense Ministers",
           "Arab Economic Unity Council","African Development Bank",
           "Afghanistan","Taliban (d.r.)",
           "Taliban (d.r.)","Africa","African Union",
           "Angola","Cabinda Enclave",
           "National Union for the Total Independence of Angola (UNITA)",
           "Agriculture (secondary role code)",
           "Agriculture-focused actors","Anguilla",
           "International Energy Agency",
           "˚Aland Islands","Albania","Al Qaeda",
           "Amnesty International",
           "Arab Maghreb Union","Andorra",
           "Netherlands Antilles",
           "Org of Arab Petroleum Exporting Countries","Arab (ethnic group)",
           "Arab","Baath Party",
           "United Arab Emirates","Argentina","Arab League",
           "Armenia","Asia (region)","Asia",
           "Abu Sayyaf","American Samoa",
           "Association of Southeast Asian Nations","Antigua and Barbuda",
           "Agnostic/Atheist","Atheist/Agnostic",
           "Australia","Austria","Azerbaijan",
           "Bahai",
           "Bank of Central African States","Burundi","Belgium","Benin",
           "Burkina Faso","Bangladesh",
           "Bulgaria","Bahrain","Bahamas",
           "Bosnia and Herzegovina (d.r.)",
           "Bosnia and Herzegovina",
           "Bosniak/Croat Federation of Bosnia and Herzegovina (d.r.)",
           "Bosnian Serb Republika Srpska (d.r.)",
           "Bank for International Settlements","Balkans","Belarus",
           "Belize","Bermuda","Bolivia","Brazil",
           "Barbados","Brunei Darussalam",
           "Bhutan","Buddhist","Buddhism",
           "Business (secondary role code)",
           "Business (NOT MNCs)","Business",
           "Botswana","Central African Republic",
           "Canada","Central Asia","Caucasus",
           "Cmn Mrkt for East and Southern Africa","Central Europe",
           "Central Africa","Switzerland","Chile","China",
           "Tibet","Christian",
           "Christianity","Coptic","Catholic","Orthodox",
           "Jehovah’s Witnesses",
           "Latter Day Saints","Maronite","Protestant",
           "“fundamentalist” Christian",
           "Commonwealth of Independent States",
           "Cte d’Ivoire (Ivory Coast)",
           "Cote d'Ivoire","Communist Party","Cameroon",
           "Chechnya",
           "Democratic Republic of the Congo (Kinshasa)",
           "Congo, Democratic Republic of",
           "Council of Europe",
           "People’s Republic of the Congo (Brazzaville)",
           "Congo, Republic of","Cook Islands","Colombia",
           "Columbia","Comoros","Confucian",
           "Confucianism",
           "Law enforcement agencies",
           "Association of Coffee Producing States","Cape Verde","Caribbean",
           "Costa Rica",
           "Criminal (secondary role code)","Criminal actors",
           "ethnic Croat","Czechoslovakia",
           "Community of Sahel-Saharan States",
           "Catholic","Cuba",
           "Civilian (secondary role code)","Civilians",
           "Civilian individuals/groups",
           "Commonwealth of Nations","Cayman Islands",
           "Cyprus","Greek Cypriot",
           "Turkish Cypriot","Czechoslovakia",
           "Czech Republic","East Germany","Germany",
           "Development (secondary role code)",
           "Development-focused actors","Djibouti",
           "Dominica","Denmark",
           "Dominican Republic","Algeria",
           "National Liberation Front (FLN)",
           "Movement of the Society for Peace",
           "Democratic National Rally","Ennahda Movement",
           "Islamic Salvation Front",
           "National Liberation Front (FLN)",
           "Movement of the Society for Peace",
           "Islamic Salvation Army",
           "Armed Islamic Group (GIA)","Salafist Group",
           "East African Community","Eastern Africa",
           "European Bank for Reconstruct and Dev","Ecuador",
           "Education (secondary role code)",
           "Education-focused actors","European Union",
           "Eastern Europe",
           "European Free Trade Association","Egypt","Muslim Brotherhood",
           "East Indies (region)",
           "East Indies","Elites (secondary role code)",
           "Elites, celebrities",
           "Environment (secondary role code)",
           "Environment-focused actors","Environmental",
           "Eritrea","Western Sahara","Spain",
           "Basque","Estonia","Ethiopia",
           "Europe",
           "International Federation of Human Rights","Finland","Fiji",
           "Falkland Islands (Malvinas)",
           "Falkland Islands","France",
           "Faeroe Islands",
           "Federal Republic of Yugoslavia","Kosovo (d.r.)",
           "Montenegro (d.r.)","Serbia (d.r.)",
           "Vojvodina (d.r.)","Micronesia",
           "Micronesia, Federated States of","Gabon",
           "United Kingdom","Irish Republican Army",
           "Georgia","Guernsey","Ghana",
           "Gibraltar","Guinea","Guadeloupe",
           "Gambia",
           "Democratic Republic of Germany (East Berlin)",
           "Federal Republic of Germany (Bonn)","Guinea-Bissau",
           "Equatorial Guinea",
           "Group of seven","Government (primary role code",
           "Government","Greece","Grenada",
           "Greenland","Guatemala",
           "French Guiana","Guam","Guyana","Gypsy",
           "Gaza Strip",
           "UN High Commission for Human Rights",
           "UN High Commission for Refugees","Hindu","Hinduism",
           "Hong Kong (Special Administrative Region of China)","Hong Kong",
           "Health (secondary role code)",
           "Health-focused actors","Hamas","Honduras",
           "Human rights-focused actors",
           "Croatia (d.r.)","Croatia",
           "Human Rights Watch","Haiti","Hungary",
           "Hutu (ethnic group)",
           "Intergovernmental Auth. on Development",
           "International Atomic Energy Agency",
           "International Criminal Court",
           "International Crisis Group",
           "International Court of Justice",
           "International Cocoa Organization",
           "Islamic Development Bank","Indonesia",
           "International Grains Council",
           "Inter-governmental organizations",
           "Inter-Government Organizations","African Union",
           "Inter-African Coffee Organization (IACO)",
           "Common Market for Eastern and Southern Africa",
           "Franc Zone Financial Community of Africa",
           "African Development Bank",
           "Eastern and Southern African Trade and Development Bank",
           "New Economic Partnership for Africa’s Development",
           "Organization of African Unity (OAU)",
           "Pan African Parliament",
           "Cocoa Producer’s Alliance",
           "Association of Coffee Producing Countries",
           "International Cocoa Organization (ICCO)",
           "International Grains Council",
           "Organization of Arab Petroleum Exporting Countries (OAPEC)",
           "Arab Bank for Economic Development in Africa",
           "Bank for International Settlements",
           "Group of Eight (G-8)","Group of Seven (G-7)",
           "Group of Seventy-Seven (G-77)",
           "Highly Indebted Poor Countries (HIPC)",
           "International Monetary Fund (IMF)",
           "Organization of Petroleum Exporting Countries (OPEC)","Paris Club",
           "World Trade Organization (WTO)",
           "Bank of Central African States (BEAC)",
           "Monetary and Economic Community of Central Africa (CEMAC)",
           "Economic Community of Central African States",
           "Commonwealth of Independent States","Interpol",
           "Commonwealth of Nations",
           "Intergovernmental Authority on Development (IGAD)",
           "East African Community",
           "European Free Trade Association","Council of Europe",
           "European Bank for Reconstruction and Development","European Union",
           "Council of Security and Cooperation in Europe (OSCE)",
           "International Criminal Court",
           "Inter-Parliamentary Union","Arab Cooperation Council",
           "Arab Economic Unity Council",
           "Arab Monetary Fund for Economic and Social Development","Arab Maghreb Union",
           "Arab League",
           "Islamic Development Bank",
           "Organization of Islamic Conferences (OIC)",
           "Community of Sahel-Saharan States (CENSAD)",
           "Organization of Non-Aligned Countries",
           "Organization of American States",
           "Gulf Cooperation Council",
           "Peacekeeping force (organization unknown)",
           "Southern African Development Community",
           "South Asian Association",
           "Association of Southeast Asian Nations (ASEAN)","Asian Development Bank",
           "Southeast Asia Collective Defense Treaty (SEATO)","United Nations",
           "United Nations Food and Agriculture Organization",
           "International Energy Agency","The World Bank",
           "World Health Organization (WHO)",
           "United Nations High Commission for Human Rights (OHCHR)",
           "International Atomic Energy Agency (IAEA)",
           "International Court of Justice (ICJ)",
           "International War Crimes Tribunals",
           "United Nations Children’s Fund (UNICEF)",
           "International Labor Organization",
           "United Nations High Commission for Refugees (OHCR)","World Food Program",
           "West Africa Monetary and Economic Union",
           "Economic and Monetary Union of West Africa (UEMOA)",
           "West Africa Development Bank",
           "Economic Community of West African States (ECOWAS)",
           "Western European Union",
           "North Atlantic Treaty Organization (NATO)",
           "Internat. Helsinki Fed for Human Rights",
           "International Labor Organization","International Monetary Fund",
           "International Militarized Group",
           "Al Qaeda","Abu Sayyaf",
           "Jemaah Islamiya","Isle of Man","India",
           "Indian-controlled Kashmir",
           "Insurgents",
           "Ambiguous international or transnational actor",
           "Inter-Parliamentary Union",
           "International Red Cross","Ireland","Iran","Iraq",
           "Baghdad",
           "Kurdish Democratic Party (KDP)","ISIS/ISIL","Iceland","Israel",
           "Israeli Communist Party (d.r.)",
           "Israeli Labor Party (d.r.)",
           "Likud Party (d.r.)","Meretz Party (d.r.)",
           "Shas Party (d.r.)",
           "Israeli Labor Party (d.r.)",
           "Likud Party (d.r.)","Meretz Party (d.r.)",
           "Shas Party (d.r.)","Israeli Settlers",
           "Italy","Interpol","Jamaica","Jain",
           "Jainism","Jew","Judaism",
           "Hasidic Jew","Ultra-Orthodox Jew",
           "Jemaah Islamiya","Jordan",
           "Islamic Action Front","Japan",
           "Judiciary (primary role code)","Judicial actors",
           "International Commission of Jurists",
           "Kashmir","Kazakhstan","Kenya",
           "Kyrgyzstan","Cambodia",
           "Khmer Rouge",
           "United Nations Children's Fund","Kiribati","Saint Kitts-Nevis",
           "Saint Kitts and Nevis",
           "Republic of Korea (Seoul)",
           "Korea, Republic of","Kosovo","Kurd (ethnic group)",
           "Kuwait",
           "Labor (secondary role code)","Labor-focused actors",
           "Latin America","Laos","Lebanon",
           "Amal Militia","South Lebanon Army",
           "Hezbullah","Liberia","Bomi (Liberia)",
           "Bong (Liberia)",
           "Grand Cape Mount (Liberia)",
           "Grand Bassa (Liberia)","Grand Gedeh (Liberia)",
           "Liberia Action Party (d.r.)",
           "National Democratic Party of Liberia (d.r.)",
           "National Patriotic Front of Liberia (NPFL) (d.r.)",
           "United People’s Party (d.r.)","Krahn (ethnic group)",
           "Grand Kru (Liberia)",
           "Lofa (Liberia)",
           "Mandingo, Mandingoe (ethnic group)","Montserrado (Liberia)",
           "Margibi (Liberia)","Maryland (Liberia)",
           "Nimba (Liberia)",
           "All Liberia Coalition Party",
           "Liberia Action Party (d.r.)","New Deal Movement",
           "National Democratic Party of Liberia (d.r.)",
           "United People’s Party (d.r.)","Armed Forces of Liberia (d.r.)",
           "Independent NPFL",
           "Liberia Peace Council",
           "Liberians United for Reconciliation and Democracy (LURD)",
           "National Patriotic Front of Liberia (NPFL) (d.r.)",
           "United Liberation Front for Democracy",
           "Rivercess (Liberia)","Sino (Liberia)","Libya",
           "Saint Lucia",
           "Legislature (secondary role code)",
           "Legislative bodies/actors","Liechtenstein","Sri Lanka",
           "People’s Liberation Front",
           "Lesotho","Lithuania","Luxembourg",
           "Latvia",
           "Macao (Special Administrative Region of China)","Morocco",
           "Polisario Guerillas","Monaco",
           "Moldova","Middle East","Madagascar",
           "Mediterranean","Maldives",
           "Middle East","Medical (secondary role code)",
           "Media-related actors/organizations","Mexico","Marshall Islands",
           "MIlitary (primary role code)",
           "Military","Macedonia","Mali","Malta",
           "Myanmar (Burma)","Myanmar",
           "Multi-national corporation",
           "Multinational Corporation","Montenegro",
           "Mongolia","Northern Mariana Islands",
           "Moderate (tertiary role code)",
           "Moderate/mainstream group","Muslim",
           "Islam","Alewi","Druze",
           "“Fundamentalist,” “radical,” “extremist” Muslim","Sufi","Shia","Sunni",
           "Mozambique","Mauritania",
           "Doctors Without Borders","Montserrat",
           "Montenegro","Martinique","Mauritius",
           "Malawi","Malaysia","Mayotte",
           "North Africa","Namibia",
           "North Atlantic Treaty Organization","New Caledonia",
           "Niger","Norfolk Island",
           "Nigeria","Abia (Nigeria)",
           "Abuja (Nigeria)","Adamawa (Nigeria)",
           "Akwa Ibom (Nigeria)","Anambra (Nigeria)",
           "Bauchi (Nigeria)",
           "Bayelsa (Nigeria)","Biafra (Nigeria)",
           "Benue (Nigeria)","Borno (Nigeria)",
           "Cross River (Nigeria)","Delta (Nigeria)",
           "Edo (Nigeria)","Ekiti (Nigeria)",
           "Enugu (Nigeria)","Gombe (Nigeria)",
           "Hausa (ethnic group)",
           "Ibo, Igbo (ethnic group)",
           "Ijaws (ethnic group)","Imo (Nigeria)",
           "Jigawa (Nigeria)","Kaduna (Nigeria)",
           "Kano (Nigeria)","Katsina (Nigeria)",
           "Kebbi (Nigeria)","Kogi (Nigeria)",
           "Kwara (Nigeria)","Lagos (Nigeria)",
           "Nassarawa (Nigeria)",
           "Niger Delta Region (Nigeria)","Niger (Nigeria)",
           "North Nigeria (Nigeria)",
           "Ogoni (ethnic group)","Ogun (Nigeria)",
           "Ondo (Nigeria)",
           "All Nigeria People’s Party","Campaign for Democracy",
           "National Democratic Coalition of Nigeria (NADECO)","Osun (Nigeria)",
           "Oyo (Nigeria)","Plateu State (Nigeria)",
           "Movement for the Advancement of Democracy (MAD)","Rivers (Nigeria)",
           "Sokoto (Nigeria)",
           "Taraba (Nigeria)","Tiv (ethnic group, language)",
           "Yobe (Nigeria)",
           "Yoruba (ethnic group)","Zamfara (Nigeria)",
           "Non-governmental movements","Greenpeace",
           "Non-governmental organizations",
           "Non-Governmental Organization",
           "Christian Solidarity International",
           "International Fed. of Red Cross and Red Crescent (ICRC)","Red Cross",
           "Medecins Sans Frontieres (Doctors Without Borders)","Red Crescent",
           "Amnesty International",
           "International Federation of Human Rights (FIDH)",
           "Human Rights Watch",
           "International Helsinki Federation for Human Rights","International Crisis Group",
           "International Commission of Jurists",
           "International Organization for Migration","Union of Arab Journalists",
           "World Economic Forum","Oxfam",
           "Nicaragua","Niue","Niue Island",
           "Netherlands","North America",
           "Norway","Nepal","Nauru","New Zealand",
           "Organization of American States",
           "Organization of African Unity",
           "Occupied",
           "International Cocoa Organization","Oman",
           "Org. of Petroleum Exporting Countries",
           "Opposition (primary role code)",
           "Opposition groups (often parliamentary)",
           "Animist/Pagan","Pakistan",
           "Pakistani-controlled Kashmir","Palestinian",
           "Palestine Liberation Organization",
           "Abu Nidal Organization",
           "Palestine Liberation Front","Panama","Pitcairn",
           "Peru","Persian Gulf",
           "Philippines","Palau","Papua New Guinea",
           "Bougainville","Poland","Papel",
           "Puerto Rico",
           "Democratic People’s Rep. of Korea (Pyongyang)",
           "Korea, Democratic People's Republic of",
           "Protestant","Portugal","Paraguay",
           "Palestinian Occupied Territories",
           "Palestine","Fatah (d.r.)",
           "Hamas (d.r.)","Gaza Strip",
           "Al Aqsa Martyrs Brigade",
           "Democratic Front for the Liberation of Palestine (DFLP)",
           "Hamas (d.r.)",
           "Palestinian Islamic Jihad",
           "People’s Front for the Liberation of Palestine (PFLP)",
           "West Bank","Political party",
           "French Polynesia","Qatar",
           "Radical (tertiary role code)",
           "Radical/extremist/fundamentalist",
           "Rebel (primary role code)","Rebels/insurgents",
           "Rebel / insurgents",
           "Refugee (secondary role code)","Refugee-focused actors",
           "Unspecified religious","Runion",
           "Romania","Romania","Russia",
           "Chechnya","Rwanda",
           "Rwandan Patriotic Front (d.r.)",
           "Rwandan Patriotic Front (d.r.)",
           "South Asian Association",
           "Southern African Development Community","Southern Africa",
           "South America","South Asia",
           "Saudi Arabia","Serbia and Montenegro (d.r.)",
           "Serbia and Montenegro",
           "Kosovo (d.r.)","Montenegro (d.r.)",
           "Serbia (d.r.)","Vojvodina (d.r.)",
           "Scandinavia","Sudan","Darfur",
           "National Democratic Alliance",
           "Sudan People’s Liberation Army","Southeast Asia",
           "Senegal",
           "Movement of Democratic Forces of Casamance","ethnic Serb",
           "Singapore","Shia",
           "Saint Helena","Shintoism","Sikh","Sikhism",
           "Svalbard and Jan Mayen Islands",
           "Slav","Solomon Islands",
           "Sierra Leone","Kamojor militia",
           "Revolutionary United Front","El Salvador",
           "San Marino","Sinhalese (ethnic group)",
           "Somalia",
           "SE Asia Collective Defense Treaty",
           "Saint Pierre and Miquelon","Spy (primary role code)",
           "Intelligence agencies",
           "Serbia (d.r.)","Serbia","Kosovo (d.r.)",
           "Vojvodina (d.r.)","Republica Srpska",
           "South Sudan",
           "Sao Tome and Principe","Soviet Union","Suriname",
           "Slovakia","Slovenia","Sweden",
           "Swaziland","Seychelles","Syria",
           "Tamil (ethnic group)","Tamil","Taoist",
           "Taoism","Turks and Caicos Islands",
           "Chad",
           "Terai (region in northern India/southern Nepal)","Togo",
           "Thailand","Tajikistan","Tokelau",
           "Turkmenistan",
           "East Timor (Timor-Leste)","East Timor","Tonga",
           "Tuareg (ethnic group)","ethnic Turk",
           "Trinidad and Tobago","Tunisia",
           "Turkey","Ankara",
           "Justice and Development Party (AKP) (d.r.)",
           "Motherland Party (ANAP) (d.r.)",
           "Republican People’s Party (CHP) (d.r.)",
           "Democratic Left Party (DSP) (d.r.)",
           "True Path Party (DYP) (d.r.)",
           "National Action Party (MHP) (d.r.)",
           "Welfare Party (Refah) (d.r.)","Istanbul",
           "Izmir",
           "Justice and Development Party (AKP) (d.r.)",
           "Motherland Party (ANAP) (d.r.)",
           "Republican People’s Party (CHP) (d.r.)",
           "Democratic Left Party (DSP) (d.r.)",
           "Democratic Society Party (DTP)",
           "True Path Party (DYP) (d.r.)",
           "Virtue Party (Fazilet)",
           "Democratic People’s Party (DEHAP/HADEP)",
           "National Action Party (MHP) (d.r.)",
           "Welfare Party (Refah) (d.r.)","Dev-Sol",
           "Kurdistan Workers’ Party (PKK)",
           "Southeast Turkey","Tutsi (ethnic group)",
           "Tuvalu","Taiwan","Tanzania",
           "Unidentified Armed Force (tertiary role code)","Unaffiliated armed forces",
           "International Academy of Architecture","Uganda",
           "Allied Democratic Forces","Lord’s Resistance Army",
           "Uighur (Chinese ethnic minority)",
           "Uighur","Unidentified state actors",
           "Ukraine","United Nations",
           "Unrecognized","Uruguay","United States",
           "Union of Soviet Socialist Republics (USSR)","Uzbekistan",
           "Holy See (Vatican City)",
           "Saint Vincent and the Grenadines","Venezuela",
           "British Virgin Islands",
           "U.S. Virigin Islands","U.S. Virgin Islands",
           "Vietnam","Viet Nam","Vanuatu",
           "West African Development Bank","West Africa",
           "Econ Community of West African States","World Bank",
           "International War Crimes Tribunals",
           "World Economic Forum","World Food Program",
           "World Health Organization",
           "Wallis and Futuna Islands","Samoa",
           "“the West”","the West",
           "World Trade Organization","Oxfam","Yemen",
           "North Yemen","South Yemen",
           "Socialist Federal Republic of Yugoslavia (d.r.)",
           "Yugoslavia",
           "Yugoslavia’s Republic of Bosnia (d.r.)",
           "Yugoslavia’s Republic of Croatia (d.r.)",
           "Kosovo (d.r.)",
           "Yugoslavia’s Republic of Macedonia (d.r.)",
           "Montenegro (d.r.)",
           "Yugoslavia’s Republic of Slovenia (d.r.)",
           "Yugoslavia’s Republic of Serbia (d.r.)","Vojvodina (d.r.)",
           "South Africa","Zambia",
           "Zoroastrian","Zimbabwe")
)

cameo_actor_codes <- cameo_actor_codes %>% filter(!duplicated(code)) %>%
  mutate(name = name %>%
           str_replace_all("\\(secondary role code\\)|\\(primary role code\\)","") %>%
           str_replace_all("\\(secondary role code\\)|\\(secondary role code|\\(primary role code|\\(primary role code\\)","")   %>%
           str_replace_all(" \\)","")   %>%
           trimws()
  )
dim(cameo_actor_codes)


# Sentence Tables ----------------


for(crisis in unique(ICBe_events_agreed_markdown$crisno)){
  print(crisis)
  temp <- ICBe_events_agreed_markdown %>%
          filter(crisno==crisis) %>%
          dplyr::select(S=sentence_number_int_aligned, ICB=sentence_span_text,ICBe=value_markdown ) %>% distinct() %>%
          mutate(ICB=ifelse(row_number()==1 | ICB!=lag(ICB), ICB, '' )) %>%
          mutate(S=ifelse(row_number()==1 | S!=lag(S), S, '' ))

  ft_sentence_table=  temp  %>%
          flextable::as_flextable() %>%
          flextable::width( j = 1, width=0.5)  %>%
          flextable::width( j = 2:3, width=4.0)  %>%
          flextable::fontsize(size = 9, part = "all") %>%
          valign(j = 1:3, valign = "top", part = "body") %>%
          flextable::line_spacing( space = 1.0, part = "all") %>%
          padding( padding = 1, part = "all") %>%
          bg( i = which( ((temp$S %>% as.numeric) %% 2 )==1 ) , part = "body", bg = "#EFEFEF") %>%
          ftExtra::colformat_md(j = 3, part="body")

  ft_sentence_table %>% saveRDS( paste0(here::here(),"//images/tables/sentence_tables/ft_sentence_table_",crisis,".Rds"))
}



# ICBe plots --------------------------------------------


global_check_overlap=F
library(igraph)
library(RColorBrewer)
library(ggraph)
#target_file <- paste0(here::here(),"/replication_paper/data/in/icb_manual_recoding_master_sheet.xlsx")
#dictionary_actors_labels    <- readxl::read_excel(target_file, sheet="actors") %>%
#  dplyr::select(crisno, value_normalized=value_normalized_wikidata_id, value_normalized_label) %>% dplyr::distinct() %>% na.omit()

dictionary_actors_labels <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1a7Id0Zg41PTKEv74H_KjiJzhMak2jGh0leT7B8oVWdI/edit?usp=sharing",
                                              sheet="actors")  %>%
  dplyr::select(crisno, value_normalized=value_normalized_wikidata_id, value_normalized_label) %>% dplyr::distinct() %>% na.omit()

actor_translator <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1a7Id0Zg41PTKEv74H_KjiJzhMak2jGh0leT7B8oVWdI/edit?usp=sharing",
                                              sheet="actor_translator") #%>% janitor::clean_names()
#Convert to a local file for pushing
#actor_translator    <- readxl::read_excel(target_file, sheet="actor_translator") #Ya we just add colors to the actor translator and we're good
pallet <- c(brewer.pal(n=20, "Dark2"), brewer.pal(n=20, "Set1"), brewer.pal(n=20, "Set2"), brewer.pal(n=20, "Paired")) %>% unique()
library(pals); #install.packages('pals')
pallet <- c(pals::glasbey(),  pals::alphabet2())
pallet <- setdiff(pallet, c("#666666", "#999999", "#FFFF99","#FFFF33","#B2DF8A","#B3B3B3","#0000FF"))
library(colorspace)
#cols <- c("#CDE4F3","#E7F3D3","#F7F0C7","#EFCFE5","#D0D1E7")
pallet <- rgb(hex2RGB(pallet)@coords * 0.7)
actor_translator$actor_color = rep(pallet, length=nrow(actor_translator))

icbe_crisismaps <- function(thiscrisis=471, prune_rare=T){

  icb_events_subset <-  ICBe_events_agreed %>%
    dplyr::filter(crisno==thiscrisis) %>%  #417 196
    dplyr::filter(!is.na(think_actor_a) | !is.na(say_actor_a) | !is.na(do_actor_a))   %>%
    dplyr::mutate(decade=date_earliest_decade %>% as.numeric() ) %>%
    dplyr::mutate(year=date_earliest_year %>% as.numeric() ) %>%
    dplyr::mutate(month= date_earliest_month %>% as.numeric() ) %>%
    dplyr::mutate(day= date_earliest_day %>% as.numeric()  ) %>%
    dplyr::arrange(desc(do_timing %in% 'long before crisis'), sentence_number_int_aligned) %>%
    tidyr::fill(decade, .direction="downup") %>%
    tidyr::fill(year, .direction="downup") %>%
    tidyr::fill(month, .direction="downup") %>%
    tidyr::fill(day, .direction="downup") %>%
    dplyr::mutate(point_in_time_year = year %>% as.numeric())  %>%
    dplyr::mutate(point_in_time_month = month %>% as.numeric()) %>%
    arrange(decade, year,month,day) %>%
    mutate(sent= cumsum((sentence_number_int_aligned == lag(sentence_number_int_aligned) ) %in% FALSE) ) #this is now our new order
  if(nrow(icb_events_subset)==0){return()}

  icb_events_subset_diad <- icb_events_subset %>%
    mutate(think_actor_a = strsplit(as.character(think_actor_a), ";")) %>% unnest(think_actor_a, keep_empty=T) %>%
    mutate(say_actor_a = strsplit(as.character(say_actor_a), ";")) %>% unnest(say_actor_a, keep_empty=T) %>%
    mutate(say_actor_b = strsplit(as.character(say_actor_b), ";")) %>% unnest(say_actor_b, keep_empty=T) %>%
    mutate(do_actor_a = strsplit(as.character(do_actor_a), ";")) %>% unnest(do_actor_a, keep_empty=T) %>%
    mutate(do_actor_b = strsplit(as.character(do_actor_b), ";")) %>% unnest(do_actor_b, keep_empty=T) %>%
    mutate(dokind=paste(act_deescalate,act_escalate,act_uncooperative,act_cooperative, interact_deescalate,
                        interact_escalate, interact_decreasecoop, interact_increasecoop, sep=";")) %>%
    mutate(thinkkind = strsplit(as.character(thinkkind), ";")) %>% unnest(thinkkind, keep_empty=T) %>%
    mutate(sayintkind = strsplit(as.character(sayintkind), ";")) %>% unnest(sayintkind, keep_empty=T) %>%
    mutate(dokind = strsplit(as.character(dokind), ";")) %>% unnest(dokind, keep_empty=T) %>%
    mutate(dokind=ifelse(dokind=='',NA,dokind)) %>%
    filter(!is.na(thinkkind) | !is.na(sayintkind) | !is.na(dokind)) %>%
    dplyr::select(sentence_number_int_aligned, #You have to keep this to remove dupe monads from the same sentence
                  sent,
                  think_actor_a,say_actor_a,say_actor_b,do_actor_a,do_actor_b,
                  thinkkind, sayintkind, dokind,
                  point_in_time_year, point_in_time_month
    ) %>% distinct() %>%
    filter(!duplicated(paste(sent,think_actor_a, say_actor_a, say_actor_b, do_actor_a, do_actor_b, thinkkind, sayintkind, dokind)))

  notes=NULL
  if(prune_rare==T){
    actor_sent_counts <-
      bind_rows(
        icb_events_subset %>% dplyr::select(sent=sentence_number_int_aligned,actor=think_actor_a),
        icb_events_subset %>% dplyr::select(sent=sentence_number_int_aligned,actor=say_actor_a),
        icb_events_subset %>% dplyr::select(sent=sentence_number_int_aligned,actor=say_actor_b),
        icb_events_subset %>% dplyr::select(sent=sentence_number_int_aligned,actor=do_actor_a),
        icb_events_subset %>% dplyr::select(sent=sentence_number_int_aligned,actor=do_actor_b)
      ) %>%
      na.omit() %>%
      mutate(actor = strsplit(as.character(actor), ";")) %>% unnest(actor) %>% distinct() %>% count(actor) %>% arrange(n %>% desc() )

    #New pruning rule, either singletons or actors with less than 10% of the max actor
    singleton_actors <- actor_sent_counts %>% mutate(share_of_max=n/max(n)) %>% filter(n==1 | share_of_max<=0.1) %>% pull(actor)
    left_over_count <- nrow(actor_sent_counts) - length(singleton_actors)
    if(left_over_count<3){
      #Skip pruning if it leaves less than 3 actors
    } else {

      actor_map <- dictionary_actors_labels %>% filter(crisno==thiscrisis) %>% distinct() %>% rename(actor=value_normalized)
      actor_singleton_labels <- actor_map$value_normalized_label
      names(actor_singleton_labels) <- actor_map$actor
      actor_singleton_labels <- actor_singleton_labels[singleton_actors]

      notes <- paste("Not shown: ", paste( singleton_actors %>% sort(), collapse="; " )) %>% stringi::stri_wrap(width=80) %>% paste(collapse="\n")

      #Easiest thing is to just prune the original data
      icb_events_subset <- icb_events_subset %>%
        filter(!(
          think_actor_a %in% singleton_actors | #This doesn't work for joint ones
            say_actor_a %in% singleton_actors |
            say_actor_b %in% singleton_actors |
            do_actor_a %in% singleton_actors |
            do_actor_b %in% singleton_actors
        ) )

      #recreate diads again using the new sentence numbers
      icb_events_subset_diad <- icb_events_subset %>%
        mutate(think_actor_a = strsplit(as.character(think_actor_a), ";")) %>% unnest(think_actor_a, keep_empty=T) %>%
        mutate(say_actor_a = strsplit(as.character(say_actor_a), ";")) %>% unnest(say_actor_a, keep_empty=T) %>%
        mutate(say_actor_b = strsplit(as.character(say_actor_b), ";")) %>% unnest(say_actor_b, keep_empty=T) %>%
        mutate(do_actor_a = strsplit(as.character(do_actor_a), ";")) %>% unnest(do_actor_a, keep_empty=T) %>%
        mutate(do_actor_b = strsplit(as.character(do_actor_b), ";")) %>% unnest(do_actor_b, keep_empty=T) %>%
        mutate(dokind=paste(act_deescalate,act_escalate,act_uncooperative,act_cooperative, interact_deescalate,
                            interact_escalate, interact_decreasecoop, interact_increasecoop, sep=";")) %>%
        mutate(thinkkind = strsplit(as.character(thinkkind), ";")) %>% unnest(thinkkind, keep_empty=T) %>%
        mutate(sayintkind = strsplit(as.character(sayintkind), ";")) %>% unnest(sayintkind, keep_empty=T) %>%
        mutate(dokind = strsplit(as.character(dokind), ";")) %>% unnest(dokind, keep_empty=T) %>%
        mutate(dokind=ifelse(dokind=='',NA,dokind)) %>%
        filter(!is.na(thinkkind) | !is.na(sayintkind) | !is.na(dokind)) %>%
        dplyr::select(sentence_number_int_aligned, #You have to keep this to remove dupe monads from the same sentence
                      sent,
                      think_actor_a,say_actor_a,say_actor_b,do_actor_a,do_actor_b,
                      thinkkind, sayintkind, dokind,
                      point_in_time_year, point_in_time_month
        ) %>% distinct() %>%
        filter(!duplicated(paste(sent,think_actor_a, say_actor_a, say_actor_b, do_actor_a, do_actor_b, thinkkind, sayintkind, dokind))) %>%
        filter(!(
          think_actor_a %in% singleton_actors | #This doesn't work for joint ones
            say_actor_a %in% singleton_actors |
            say_actor_b %in% singleton_actors |
            do_actor_a %in% singleton_actors |
            do_actor_b %in% singleton_actors
        ) ) %>%
        arrange(sent) %>%
        mutate(sent= cumsum((sent == lag(sent) ) %in% FALSE) ) #
    }
  }

  g <- make_empty_graph(n = 0, directed = TRUE)
  #graph_from_data_frame(vertices=nodes) #relations, directed=TRUE,


  #Order actors by how many interactions they have together
  temp <- bind_rows(
    icb_events_subset_diad  %>%
      dplyr::select(say_actor_a, say_actor_b, sent) %>% na.omit()  %>% dplyr::rename(actor_1=say_actor_a, actor_2=say_actor_b),
    icb_events_subset_diad %>%
      filter(is.na(sayintkind)) %>%  dplyr::select(do_actor_a, do_actor_b, sent) %>% na.omit() %>% dplyr::rename(actor_1=do_actor_a, actor_2=do_actor_b)
  )  %>% arrange(actor_1, actor_2) %>% distinct()

  #new plan is to minimize the number of crossovers
  temp2 <- bind_rows(
    temp,
    temp %>% setNames(c("actor_2", "actor_1", "sent"))
  ) %>% count(actor_1, actor_2) %>% arrange(actor_1, actor_2) %>% arrange(n) %>%
    rowwise() %>%
    mutate(actor_unordered= ifelse(actor_1>actor_2, paste(actor_1, actor_2),paste(actor_2, actor_1) ) ) %>%
    arrange(desc(n))


  temp2_wide <- temp2 %>% tidyr::pivot_wider(id_cols=c(actor_1), names_from=actor_2, values_from=n) %>% mutate_if(is.numeric, tidyr::replace_na, 0) %>% as.data.frame()
  rownames(temp2_wide) <- temp2_wide$actor_1
  temp2_wide$actor_1 <- NULL
  temp2_wide <- temp2_wide %>% as.matrix()
  temp2_wide <- 1 -   (temp2_wide/max(temp2_wide))
  temp2_wide <- temp2_wide[sort(rownames(temp2_wide)), sort(colnames(temp2_wide))]
  hc <- hclust(temp2_wide %>% as.dist(), method="single")
  diag(temp2_wide) <- 0
  temp2_wide_pca <- prcomp(temp2_wide, center = F,scale. = F, retx=F, rank=1) #just run PCA on the distance matrix and order along the first eigenvalue

  #ideal_ordering <- hc$labels[hc$order] #There's a bug here, put all the isolated nodes by themselves first
  ideal_ordering <- temp2_wide_pca$rotation %>% as.data.frame() %>% arrange(PC1) %>% rownames() #This all works but it's not optimizing what we want which is to minimize the number of cross over events

  #####Nodes
  icb_events_subset_nodes <-
    bind_rows(
      icb_events_subset_diad %>% dplyr::select(sent, actor=think_actor_a, point_in_time_year, point_in_time_month) ,
      icb_events_subset_diad %>% dplyr::select(sent, actor=say_actor_a, point_in_time_year, point_in_time_month) ,
      icb_events_subset_diad %>% dplyr::select(sent, actor=say_actor_b, point_in_time_year, point_in_time_month) ,
      icb_events_subset_diad %>% dplyr::select(sent, actor=do_actor_a, point_in_time_year, point_in_time_month) ,
      icb_events_subset_diad %>% dplyr::select(sent, actor=do_actor_b, point_in_time_year, point_in_time_month)
    )  %>%
    filter(!is.na(actor)) %>%
    distinct()  %>% arrange(actor,sent) %>% mutate(actor_sent= actor %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>% #don't na omit this one
    mutate(x=actor %>% factor(levels=actor %>% unique() %>% setdiff(ideal_ordering) %>% c(ideal_ordering)) %>% as.numeric() *100 ,
           y=sent*-1 *100)


  icbe_actor_to_qcode <- icb_events_long %>%
    dplyr::filter(crisno==thiscrisis  ) %>%
    filter(varname_normalized %>% str_detect("actor")) %>%
    dplyr::select(value_normalized,value_qcode) %>%
    na.omit() %>% distinct()

  actor_colors_df <- icbe_actor_to_qcode %>%
    left_join(actor_translator %>% dplyr::select(value_qcode=QCode, actor_color))
  actor_colors <- actor_colors_df$actor_color
  names(actor_colors) = actor_colors_df$value_normalized

  date_sineposts <- icb_events_subset_nodes  %>%
    mutate(x=max(x)+50) %>%
    dplyr::select(sent, point_in_time_year, point_in_time_month,x,y) %>%
    na.omit() %>%
    distinct() %>%
    mutate(sent=as.numeric(sent)) %>% arrange(sent) %>%
    mutate(year_float=point_in_time_year+(point_in_time_month/12)) %>%
    group_by(sent) %>%
    filter(year_float==min(year_float)) %>%
    ungroup() %>% as.data.frame() %>%
    filter( row_number()==1 |  ( year_float > lag(year_float)  ) )  %>%
    filter(!duplicated(year_float ) ) %>%
    mutate(label=paste0(point_in_time_year,"-",month.abb[point_in_time_month]))

  ####Actor levels
  icb_events_subset_nodes_min <-
    icb_events_subset_nodes %>% group_by(actor) %>% filter(sent==sent %>% min()) %>% ungroup() %>%
    left_join(dictionary_actors_labels %>% filter(crisno==thiscrisis) %>% distinct() %>% rename(actor=value_normalized) ) %>%
    #rename(qcode=actor, actor=value_normalized_label) %>% #we used to normalize to qcode and n
    rowwise() #%>%
  #mutate(actor=actor %>% stringi::stri_wrap(width=15) %>% paste0(collapse='\n')) #do at plot time so I don't break colors


  for(i in 1:nrow(icb_events_subset_nodes)){
    g <- g %>%  add_vertices(1, name=icb_events_subset_nodes$actor_sent[i],
                             actor = icb_events_subset_nodes$actor[i],
                             sent = icb_events_subset_nodes$sent[i]  ) #, type = nodes$type[i] #, leaf = nodes$leaf[i]
  }

  xy <- cbind(icb_events_subset_nodes$x, icb_events_subset_nodes$y )


  #####Time Edges
  icb_events_subset_nodes_edges_time <- cbind(icb_events_subset_nodes, bind_rows(icb_events_subset_nodes[-1,],icb_events_subset_nodes[1,] ) ) %>% janitor::clean_names() %>% filter(actor==actor_2 & sent_2>sent) %>% distinct()

  g <- g %>%
    add_edges(icb_events_subset_nodes_edges_time[,c('actor_sent','actor_sent_2')] %>% as.matrix() %>% t() %>% as.vector(),
              actor=icb_events_subset_nodes_edges_time$actor, timeedge=T, leaf='', eventedge=F, bundle_count=1)


  ######Monads
  icb_events_subset_monad <-
    bind_rows(
      icb_events_subset_diad %>% dplyr::select(sent, actor=think_actor_a,leaf=thinkkind) ,
      icb_events_subset_diad %>% filter(!is.na(say_actor_a) & is.na(say_actor_b)) %>% dplyr::select(sent,actor=say_actor_a,leaf=sayintkind) ,
      icb_events_subset_diad %>% filter(!is.na(do_actor_a) & is.na(do_actor_b)) %>% dplyr::select( sent,actor=do_actor_a,leaf=dokind)
    ) %>%
    na.omit() %>%
    distinct() %>%
    filter(!duplicated(paste(sent,actor,leaf))) %>%
    mutate(actor_sent= actor %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>%
    mutate(leaf = strsplit(as.character(leaf), ";")) %>% unnest(leaf) %>%
    distinct() %>%
    group_by(sent,actor,actor_sent) %>%
    summarise(leaf=paste(leaf, collapse="\n") ) %>%
    left_join( icb_events_subset_nodes %>% dplyr::select(actor_sent,x,y))


  ######Directed
  icb_events_subset_directed <-
    #We need to enumerate all the possibilities
    bind_rows(
      #Do alone
      icb_events_subset_diad %>% filter(is.na(thinkkind) & is.na(sayintkind) & !is.na(dokind)) %>% dplyr::select(sent,actor_1=do_actor_a, actor_2=do_actor_b, leaf=dokind) ,
      #Say alone
      icb_events_subset_diad %>% filter(is.na(thinkkind) & !is.na(sayintkind) & is.na(dokind)) %>% dplyr::select(sent,actor_1=say_actor_a, actor_2=say_actor_b, leaf=sayintkind) ,
      #Say Do
      icb_events_subset_diad %>% filter(is.na(thinkkind) & !is.na(sayintkind) & !is.na(dokind)) %>%
        mutate(leaf=paste0(sayintkind,"-",dokind)) %>% dplyr::select(sent,actor_1=say_actor_a, actor_2=say_actor_b, leaf=leaf),
      #Think Do
      icb_events_subset_diad %>% filter(!is.na(thinkkind) & is.na(sayintkind) & !is.na(dokind)) %>%
        mutate(leaf=paste0(dokind)) %>% dplyr::select(sent,actor_1=do_actor_a, actor_2=do_actor_b, leaf=leaf)
    ) %>%
    na.omit() %>%
    distinct() %>%
    #mutate(leaf = strsplit(as.character(leaf), ";")) %>% unnest(leaf) %>% distinct() %>%
    mutate(actor_sent_1= actor_1 %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>%
    mutate(actor_sent_2= actor_2 %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>%
    arrange(sent, actor_sent_1,leaf) %>%
    group_by(sent,actor_1, actor_sent_1,actor_sent_2) %>%
    summarise(leaf=paste(leaf, collapse="\n") ) %>%
    #It is actually easier just to rejex out some dupes
    mutate(leaf = leaf %>%
             str_replace("accept\naccept","accept") %>%
             str_replace("appeal\nappeal","appeal") %>%
             str_replace("reject\nreject","reject") %>%
             str_replace("express intent\nexpress intent","express intent")
    ) %>%
    arrange(sent, actor_sent_1,leaf) %>%
    filter(actor_sent_1!=actor_sent_2)

  #debug
  icb_events_subset_directed <- icb_events_subset_directed  %>%
    group_by(sent, leaf) %>%
    mutate(bundle_count=n()) %>%
    mutate(leaf2=ifelse(bundle_count>1 & row_number()>1,'',leaf)) %>%
    ungroup()

  g <- g %>% add_edges(icb_events_subset_directed[,c('actor_sent_1','actor_sent_2')] %>% as.matrix() %>% t() %>% as.vector(),
                       timeedge=F,
                       leaf=icb_events_subset_directed$leaf2,
                       eventedge=T,
                       actor=icb_events_subset_directed$actor_1,
                       bundle_count=icb_events_subset_directed$bundle_count
  )

  ######Put it all together
  wrap_names <- function(x){sapply(x, FUN=function(x) x %>% stringi::stri_wrap(width=15) %>% paste0(collapse='\n') ) }
  p_map <- g %>%
    ggraph("manual",x=xy[,1],y=xy[,2]) +  #geom_edge_parallel

    #Event edges
    #geom_path(data = hbundle, aes(x, y, group = group), col = "#9d0191", size = 0.05) +
    geom_edge_arc(aes(
      colour=actor,
      label=leaf,
      filter=eventedge==T & bundle_count==1
    ),
    label_colour=NA,
    vjust = -0.75,
    lineheight = .75,
    sep = unit(1, 'mm'),
    edge_width=0.25,
    show.legend = F,
    arrow = arrow(length = unit(2, 'mm')),
    label_size=3,
    check_overlap=global_check_overlap,
    start_cap = circle(3, 'mm'),
    end_cap = circle(3, 'mm'),
    angle_calc = 'along' #,
    #label_push=unit(20, units='mm') #This shifts the curved labels toward the target
    ) +
    #Grouped Edges
    geom_edge_parallel(aes(
      colour=actor,
      label=leaf,
      filter=eventedge==T & bundle_count>1
    ),
    vjust = -0.75,
    lineheight = .75,
    sep = unit(5, 'mm'),
    edge_width=0.4,  #0.25
    show.legend = F,
    arrow = arrow(length = unit(1, 'mm')),
    label_size=4,
    check_overlap=global_check_overlap,
    start_cap = circle(3, 'mm'),
    end_cap = circle(3, 'mm'),
    angle_calc = 'along'
    ) +

    #Time edges
    geom_edge_link(aes(
      edge_colour=actor,
      filter=timeedge==T
    ),
    edge_width=0.1,
    show.legend = F,
    check_overlap=global_check_overlap
    ) +
    geom_node_point(aes(color=actor),size=1,stroke=0.5) +
    #Fatalities
    #geom_text(data=fatalities %>% filter(interact_fatalities>1), aes(x=x+4,y=y, size=interact_fatalities+1), color='black', label='☠', angle=0, check_overlap=global_check_overlap) +

    geom_text(data=icb_events_subset_monad, aes(x=x,y=y, label=leaf), nudge_y=40, angle=0, lineheight = .75, size=3, check_overlap=global_check_overlap) +
    #Add Actor Names
    geom_text(data=icb_events_subset_nodes_min, aes(x=x,y=y, label=actor  %>% wrap_names() , color=actor),
              nudge_y=150, angle=0, size=4, lineheight = .75, fontface = "bold",
              check_overlap=global_check_overlap) + #-45
    geom_text(data=date_sineposts, aes(x=x,y=y, label=label), color="grey", nudge_y=0, angle=0, size=4, lineheight = .75, fontface = "bold",check_overlap=global_check_overlap) + #-45
    scale_color_manual(values = actor_colors)  +
    scale_edge_color_manual(values = actor_colors) +
    #theme_bw() +
    #labs( title = plot_title ) +
    theme_graph() +
    theme(legend.position = "none") #+
  #ggimage::geom_image(data= units, aes(x=x+offeset,y=y-30, image = IMAGE), size = 0.015)

  if(prune_rare==T & !is.null(notes)){
    p_map <- p_map + labs(caption = notes)
  }

  p_map$sentence_count <- (range(c(icb_events_subset_nodes_edges_time$y_2, icb_events_subset_nodes_edges_time$y)) %>% diff() %>% abs())/100
  return(p_map)
}


for(crisis in icb_crises %>% pull(crisno) %>% unique() ){
  print(crisis)
  tictoc::tic()
  save_file_p <- paste0(here::here(), "//images/ggplots/p_icbe_plot_",crisis,".Rds")
  icbe_crisismaps(crisis) %>% saveRDS(save_file_p)
  tictoc::toc()
}


# Dyadic ICB  ----------------

#Note yemen 679 doesn't show up in this data
icb_df <- data.frame(
  stringsAsFactors = FALSE,
  icb_name = c("Afghanistan","Albania",
               "Algeria","Andorra","Angola","Antigua & Barbuda",
               "Argentina","Armenia","Australia","Austria","Azerbaijan",
               "Bahamas","Bahrain","Bangladesh","Barbados","Belarus",
               "Belgium","Belize","Benin (Dahomey)","Bhutan","Bolivia",
               "Bosnia","Botswana","Brazil","Brunei","Bulgaria",
               "Burkina Faso (Upper Volta)","Burma (see Myanmar)",
               "Burundi","Cambodia","Cameroon","Canada","Cape Verde",
               "Central African Republic","Chad","Chile","China",
               "China","Colombia","Comoros","Congo Brazzaville",
               "Congo, Democratic","Costa Rica","Cote D’Ivoire","Croatia",
               "Cuba","Cyprus","Czech Republic","Czechoslovakia",
               "Dahomey (see Benin)","Denmark","Djibouti (Somalia Fr.)",
               "Dominica","Dominican Republic","Ecuador","Egypt (UAR)",
               "El Salvador","Equatorial Guinea","Eritrea","Estonia",
               "Ethiopia","Federated States of Micronesia","Fiji",
               "Finland","Formosa","France","French West Africa",
               "Gabon","Gambia","Georgia",
               "German Democratic Republic (East Germany)",
               "Germany (German Federal Republic, West Germany)","Germany (Prussia)","Ghana",
               "Great Britain (see United Kingdom)","Greece","Grenada","Guatemala",
               "Guinea","Guinea Bissau","Guyana","Haiti","Hijaz (Hejaz)",
               "Honduras","Hungary","Iceland","India","Indonesia",
               "Iran","Iraq","Ireland (Eire)","Israel","Italy",
               "Ivory Coast (see Cote D’Ivoire)","Jamaica","Japan",
               "Jordan","Kazakhstan","Kenya","Korea","Kosovo","Kuwait",
               "Kyrgyz Republic","Laos","Latvia","Lebanon","Lesotho",
               "Liberia","Libya","Liechtenstein","Lithuania",
               "Luxemberg","Macedonia","Madagascar (Malagasy Republic)",
               "Malawi","Malaysia","Maldives","Mali","Malta",
               "Marshall Islands","Mauritania","Mauritius","Mexico","Moldova",
               "Monaco","Mongolia","Morocco","Mozambique",
               "Myanmar (Burma)","Najd (Nejd)","Namibia (South West Africa)",
               "Nepal","Netherlands","Nevis","New Zealand",
               "Nicaragua","Niger","Nigeria",
               "North Korea (People’s Republic of Korea)","Norway","Oman","Pakistan","Palau","Panama",
               "Papua and New Guinea","Paraguay","Peru",
               "Philippines","Poland","Portugal","Principe","Qatar",
               "Republic of (Congo Kinshasa) (Zaire)","Rhodesia (see Zimbabwe)",
               "Rumania","Russia (Soviet Union)","Rwanda",
               "San Marino","Sao Tome","Saudi Arabia","Senegal",
               "Serbia (see Yugoslavia)","Seychelles","Sierra Leone","Singapore",
               "Slovakia","Slovenia","Solomons","Somalia",
               "Somalia Fr. (see Djibouti)","South Africa",
               "South Korea (Republic of Korea)","South Sudan",
               "South West Africa (see Namibia)","Spain","Spanish Sahara (see Western Sahara)",
               "Sri Lanka (Ceylon)","St. Kitts","St. Lucia",
               "St. Vincent & The Grenadines","Sudan","Surinam","Swaziland",
               "Sweden","Switzerland","Syria","Taiwan (China",
               "Tajikistan","Tanzania","Thailand","Tibet","Togo",
               "Trinidad & Tobago","Tunisia","Turkey","Turkmenistan","Uganda",
               "Ukraine","United Arab Emirates",
               "United Kingdom (Great Britain)","Upper Volta (see Burkina Faso)","Uruguay",
               "USA","USSR (see Russia)","Uzbekistan","Vanuatu",
               "Venezuela","Vichy France",
               "Vietnam, Democratic Republic of (North Vietnam)","Vietnam, Republic of (South Vietnam)",
               "Western Sahara (Spanish Sahara)","Western Samoa",
               "Yemen (Arab Republic of Yemen, North Yemen)",
               "Yemen, People’s Republic of (South Yemen)","Yugoslavia (Serbia)",
               "Zambia","Zanzibar","Zimbabwe (Rhodesia)"),
  icb_letters = c("AFG","ALB","ALG","AND",
                  "ANG","AAB","ARG","ARM","AUL","AUS","AZE","BHM",
                  "BAH","BNG","BAR","BLR","BEL","BLZ","BEN","BHU","BOL",
                  "BOS","BOT","BRA","BRU","BUL","BFO",NA,"BUI",
                  "CAM","CAO","CAN","CAP","CEN","CHA","CHL",NA,"CHN",
                  "COL","COM","CON","DRC","COS","CDI","CRO","CUB",
                  "CYP","CZR","CZE",NA,"DEN","DJI","DMA","DOM",
                  "ECU","EGY","SAL","EQG","ERI","EST","ETH","FSM","FIJ",
                  "FIN","TAW","FRN","FWA","GAB","GAM","GRG","GDR",
                  "GFR","GMY","GHA",NA,"GRC","GRN","GUA","GUI",
                  "GNB","GUY","HAI","HIJ","HON","HUN","ICE","IND",
                  "INS","IRN","IRQ","IRE","ISR","ITA",NA,"JAM","JPN",
                  "JOR","KZK","KEN","KOR","KOS","KUW","KYR","LAO",
                  "LAT","LEB","LES","LBR","LIB","LIE","LIT","LUX",
                  "MAC","MAG","MAW","MAL","MAD","MLI","MLT","MSI",
                  "MAA","MAS","MEX","MLD","MNC","MON","MOR","MZM","MYA",
                  "NAJ","NAM","NEP","NTH","SKN","NEW","NIC","NIR",
                  "NIG","PRK","NOR","OMA","PAK","PAL","PAN","PNG",
                  "PAR","PER","PHI","POL","POR","STP","QAT","DRC",
                  NA,"RUM","RUS","RWA","SNM",NA,"SAU","SEN",NA,
                  "SEY","SIE","SIN","SLO","SLV","SOL","SOM",NA,"SAF",
                  "ROK","SSD",NA,"SPN",NA,"SRI",NA,"SLU","SVG",
                  "SUD","SUR","SWA","SWD","SWZ","SYR",NA,"TAJ","TAZ",
                  "THI","TBT","TOG","TRI","TUN","TUR","TKM","UGA",
                  "UKR","UAE","UKG",NA,"URU","USA",NA,"UZB","VAN",
                  "VEN","VFR","DRV","RVN","SPA","WSM","YEM","YPR",
                  "YUG","ZAM","ZAN","ZIM"),
  icb_code = c("700","339","615","232",
               "540","058","160","371","900","305","373","031",
               "692","771","053","370","211","080","434","760","145",
               "346","571","140","835","355","439",NA,"516",
               "811","471","020","402","482","483","155",NA,"710",
               "100","581","484","490","094","437","344","040",
               "352","316","315",NA,"390","522","054","042",
               "130","651","092","411","531","366","530","987","950",
               "375","713","220","480","481","420","372","265",
               "260","255","452",NA,"350","55","090","438",
               "404","110","041","671","091","310","395","750","850",
               "630","645","205","666","325",NA,"051","740",
               "663","705","501","730","347","690","703","812",
               "367","660","570","450","620","223","368","212",
               "343","580","553","820","781","432","338","983","435",
               "590","070","359","221","712","600","541","775",
               "672","565","790","210","060","920","093","436",
               "475","731","385","698","770","986","095","910",
               "150","135","840","290","235","403","694","490",NA,
               "360","365","517","331",NA,"670","433",NA,"591",
               "451","830","317","349","940","520",NA,"560",
               "732","626",NA,"230",NA,"780",NA,"056","057","625",
               "115","572","380","225","652",NA,"702","510",
               "800","711","461","052","616","640","701","500",
               "369","696","200",NA,"165","002",NA,"704","935",
               "101","219","816","817","605","990","678","680",
               "345","551","511","552")
)

df_IWCA <- data.frame(
  stringsAsFactors = FALSE,
  IWCA = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L),
  IWCA_label = c("not an intra-war crisis (IWC)",
                 "Entry of a major actor into an ongoing war",
                 "Perceived high probability of a major actor entering a war",
                 "Exit of a major actor",
                 "Perceived high probability of major power exiting a war",
                 "Technological escalation of a war","Major non-technological escalation",
                 "Defeat in a significant battle","Internal deterioration","Other")
)

df_break <-
  data.frame(
    stringsAsFactors = FALSE,
    BREAK = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L),
    BREAK_label = c("Verbal act","Political act",
                    "Economic act","External change",
                    "Other non-violent act",
                    "Internal verbal or physical challenge to regime or elite","Non-violent military act","Indirect violent act",
                    "Violent act")
  )


#icbdy_20 <- read_tsv(here::here(), "/../ICBEdataset/replication_paper/data/in/icbdy_20.dat")
icbdy_20 <- readRDS(paste0(here::here(), '/../ICBEdataset/replication_paper/data/in/icbdy_20.Rds'))

icbdy_20_clean <- icbdy_20 %>%
  left_join( icb_df %>% dplyr::select(STATEA=icb_code, actor_1=icb_name ) %>% mutate(STATEA=STATEA %>% as.numeric())  ) %>%
  left_join( icb_df %>% dplyr::select(STATEB=icb_code, actor_2=icb_name ) %>% mutate(STATEB=STATEB %>% as.numeric())  ) %>%
  left_join(df_IWCA %>% dplyr::select(IWCA=IWCA, IWCA_label=IWCA_label )  ) %>%
  left_join(df_IWCA %>% dplyr::select(IWCB=IWCA, IWCB_label=IWCA_label )  )

icb_diadic <- icbdy_20_clean %>% dplyr::select(CRISNO, NAMEA, NAMEB,  STATEA, STATEB, actor_1,actor_2, IWCA_label,IWCB_label) #%>% filter(is.na(actor_1) | is.na(actor_2))


icb2v14 <- read_csv(paste0(here::here(), "/../ICBEdataset/replication_paper/data/in/icb2v14.csv"))


df_trigger <- data.frame(
  stringsAsFactors = FALSE,
  triggr = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L),
  triggr_label = c("Verbal act","Political act","Economic act",
                   "External change","Other non-violent act",
                   "Internal verbal or physical challenge to regime or elite",
                   "Non-violent military act","Indirect violent act","Violent act")
)

df_majres <- data.frame(
  stringsAsFactors = FALSE,
  majres = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L),
  majres_label = c("No response-inaction","Verbal act",
                   "Political act","Economic act",
                   "Other non-violent act",
                   "Non-violent military act",
                   "Multiple including non-violent military act",
                   "Violent military act",
                   "Multiple including violent military act")
)

df_outcome <-
  data.frame(
    stringsAsFactors = FALSE,
    outcom = c(1L, 2L, 3L, 4L, 5L),
    outcom_label = c("Victory", "Compromise", "Stalemate", "Defeat", "Other")
  )

df_outfor <-
  data.frame(
    stringsAsFactors = FALSE,
    outfor = c(1L,2L,3L,4L,5L,6L,7L,8L,
               9L,10L,11L,12L,13L,14L,15L),
    outfor_label = c("Formal agreement - voluntary",
                     "Semi-formal agreement - voluntary",
                     "Tacit understanding","Unilateral - self","Unilateral - ally",
                     "Unilateral - adversary","Compliance","Imposed - imposer",
                     "Imposed - imposee","Spillover",
                     "Other - global organization intervention","Other - ally",
                     "Other - internal or non-state actor","Other - misc.","Faded")
  )

icb2v14_clean <- icb2v14 %>%
  left_join(icb_df %>% dplyr::select(cracid=icb_code, actor_label=icb_name ) %>% mutate(cracid=cracid %>% as.numeric()) ) %>%  #you can't actually match on the letter codes you have to match on the numbers, uhhhhhg
  left_join( df_trigger  ) %>%
  left_join( df_majres ) %>%
  left_join( df_outcome ) %>%
  left_join( df_outfor )

icb_monadic <-
  bind_rows(
    icb2v14_clean%>% mutate(sent= yrtrig + tidyr::replace_na((motrig/12),0) + tidyr::replace_na(datrig/31/12,0) ) %>% mutate(type='start') %>% dplyr::select(year=yrtrig, month=motrig, crisno,type, sent, actor_label, leaf=triggr_label),
    icb2v14_clean %>% mutate(sent= yerres + tidyr::replace_na((monres/12),0) + tidyr::replace_na(dayres/31/12,0) ) %>% mutate(type='mid') %>% dplyr::select(year=yerres, month=monres, crisno,type, sent, actor_label, leaf=majres_label),
    icb2v14_clean %>% mutate(sent= yrterm + tidyr::replace_na((moterm/12),0) + tidyr::replace_na(daterm/31/12,0) ) %>% mutate(type='end') %>% dplyr::select(year=yrterm, month=moterm, crisno,type, sent, actor_label, leaf=outfor_label)
  ) %>% as.data.frame()


library(igraph)
library(ggraph)
#ICB Function {Plots
icb_crisismaps <- function(thiscrisis=196, prune_rare=T){

  print(thiscrisis)
  save_icb_file <- paste0(here::here(), "//images/ggplots/p_icb_plot_",thiscrisis,".Rds")

  icb_diadic_subset <- icb_diadic %>% filter(CRISNO==thiscrisis) #Not all of them are diadic apparently
  icb_monadic_subset <- icb_monadic %>% rename(actor=actor_label) %>% filter(crisno==thiscrisis) %>%
    mutate(sent=sent %>% rank() %>% round()) %>% mutate(actor_sent= actor %>% paste0("_", str_pad(sent, 2, pad = "0") ))   %>% arrange(sent, actor)

  g <- make_empty_graph(n = 0, directed = TRUE)
  #graph_from_data_frame(vertices=nodes) #relations, directed=TRUE,

  icb_events_subset_diad <- bind_rows(
    icb_diadic_subset %>% dplyr::select(actor_1,actor_2) %>% left_join(icb_monadic_subset %>% filter(type=="start") %>%
                                                                         mutate(actor_1=actor) %>% dplyr::select(year, month,sent, actor_1,leaf) ) %>%
      rename(actor_2=actor_1,actor_1=actor_2),
    icb_diadic_subset %>% dplyr::select(actor_1,actor_2) %>% left_join(icb_monadic_subset %>% filter(type=="start") %>%
                                                                         mutate(actor_2=actor) %>% dplyr::select(year, month,sent, actor_2,leaf) ),

    icb_diadic_subset %>% dplyr::select(actor_1,actor_2) %>% left_join(icb_monadic_subset %>% filter(type=="mid") %>%
                                                                         mutate(actor_1=actor) %>% dplyr::select(year, month,sent, actor_1,leaf) ) %>%
      rename(actor_2=actor_1,actor_1=actor_2),
    icb_diadic_subset %>% dplyr::select(actor_1,actor_2) %>% left_join(icb_monadic_subset %>% filter(type=="mid") %>%
                                                                         mutate(actor_2=actor) %>% dplyr::select(year, month,sent, actor_2,leaf) ),

    icb_diadic_subset %>% dplyr::select(actor_1,actor_2) %>% left_join(icb_monadic_subset %>% filter(type=="end") %>%
                                                                         mutate(actor_1=actor) %>% dplyr::select(year, month,sent, actor_1,leaf) ) %>%
      rename(actor_2=actor_1,actor_1=actor_2),
    icb_diadic_subset %>% dplyr::select(actor_1,actor_2) %>% left_join(icb_monadic_subset %>% filter(type=="end") %>%
                                                                         mutate(actor_2=actor) %>% dplyr::select(year, month,sent, actor_2,leaf) ),

  ) %>% dplyr::select(year, month,sent, actor_1, actor_2, leaf) %>% distinct() %>% na.omit()

  if(nrow(icb_events_subset_diad)==0){
    print("No dyadic icb rows skipping")
    return()
  }

  #Order actors by how many interactions they have together
  temp <- icb_events_subset_diad  %>% dplyr::select(-year, -month) %>% arrange(actor_1, actor_2) %>% distinct()

  temp2 <- bind_rows(
    temp,
    temp %>% setNames(c("story_date", "actor_2", "actor_1" ))
  ) %>% count(actor_1, actor_2) %>% arrange(actor_1, actor_2) %>% arrange(n)

  temp2_wide <- temp2 %>% pivot_wider(id_cols=c(actor_1), names_from=actor_2, values_from=n) %>% mutate_if(is.numeric, tidyr::replace_na, 0) %>% as.data.frame()
  rownames(temp2_wide) <- temp2_wide$actor_1
  temp2_wide$actor_1 <- NULL
  temp2_wide <- temp2_wide %>% as.matrix()
  temp2_wide <- 1 - (temp2_wide/max(temp2_wide))
  temp2_wide <- temp2_wide[sort(rownames(temp2_wide)), sort(colnames(temp2_wide))]
  hc <- hclust(temp2_wide %>% as.dist(), method="single")

  temp_list <- list()
  for(i in 1:nrow(temp2_wide)){
    temp_list[[as.character(i)]] <- cutree(hc, k=i)
  }

  #ideal_ordering <- bind_rows(temp_list) %>% t() %>% as.data.frame() %>% apply(1, paste, collapse='') %>% sort() %>% names() #this will break with more than 9
  #  arrange(across(starts_with("V")))
  ideal_ordering <- hc$labels[hc$order] #There's a bug here, put all the isolated nodes by themselves first

  #####Nodes
  icb_events_subset_nodes <-
    bind_rows(
      icb_events_subset_diad %>% dplyr::select(year, month,sent=sent, actor=actor_1) ,
      icb_events_subset_diad %>% dplyr::select(year, month,sent=sent, actor=actor_2)
    ) %>% na.omit() %>% distinct()  %>% arrange(actor,sent) %>% mutate(actor_sent= actor %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>% #don't na omit this one
    mutate(x=actor %>% factor(levels=actor %>% unique() %>% setdiff(ideal_ordering) %>% c(ideal_ordering)) %>% as.numeric() *100 ,
           y=sent*-1 *100)

  #we centralize everything in qcodes so colors should be consistent across plots for the same actors
  icbe_actor_to_qcode <- icb_events_long %>%
    dplyr::filter(crisno==thiscrisis  ) %>%
    filter(varname_normalized %>% str_detect("actor")) %>%
    dplyr::select(value_normalized,value_qcode) %>%
    na.omit() %>% distinct()

  actor_colors_df <- icbe_actor_to_qcode %>%
    left_join(actor_translator %>% dplyr::select(value_qcode=QCode, actor_color))
  actor_colors <- actor_colors_df$actor_color
  names(actor_colors) = actor_colors_df$value_normalized


  actors=icb_events_subset_nodes$actor %>% unique() %>% as.factor()
  actor_colors = c(brewer.pal(n=11, "Dark2"), brewer.pal(n=11, "Set1"))[1:length(actors)] #ok so pick a bewer pallet with lots of values and then assign it to specific values
  names(actor_colors) = levels(actors)

  date_sineposts <- icb_events_subset_nodes  %>%
    mutate(x=max(x)+50) %>%
    dplyr::select(sent, point_in_time_year=year, point_in_time_month=month,x,y) %>%
    na.omit() %>%
    distinct() %>%
    mutate(sent=as.numeric(sent)) %>% arrange(sent) %>%
    mutate(year_float=point_in_time_year+(point_in_time_month/12)) %>%
    group_by(sent) %>%
    filter(year_float==min(year_float)) %>%
    ungroup() %>% as.data.frame() %>%
    filter( row_number()==1 |  ( year_float > lag(year_float)  ) )  %>%
    filter(!duplicated(year_float ) ) %>%
    mutate(label=paste0(point_in_time_year,"-",month.abb[point_in_time_month]))

  ####Actor levels
  icb_events_subset_nodes_min <-
    icb_events_subset_nodes %>% group_by(actor) %>% filter(sent==sent %>% min()) %>% ungroup() %>%
    #left_join(dictionary_actors_labels %>% filter(crisno==thiscrisis) %>% distinct() %>% rename(actor=value_normalized) ) %>% #only need to do for our q codes
    #rename(qcode=actor, actor=value_normalized_label) %>%
    rowwise() %>%
    mutate(actor=actor %>% stringi::stri_wrap(width=15) %>% paste0(collapse='\n'))


  for(i in 1:nrow(icb_events_subset_nodes)){
    g <- g %>%  add_vertices(1, name=icb_events_subset_nodes$actor_sent[i],
                             actor = icb_events_subset_nodes$actor[i],
                             sent = icb_events_subset_nodes$sent[i]  ) #, type = nodes$type[i] #, leaf = nodes$leaf[i]
  }

  xy <- cbind(icb_events_subset_nodes$x, icb_events_subset_nodes$y )


  #####Time Edges
  icb_events_subset_nodes_edges_time <- cbind(icb_events_subset_nodes, bind_rows(icb_events_subset_nodes[-1,],icb_events_subset_nodes[1,] ) ) %>%
    janitor::clean_names() %>% filter(actor==actor_2 & sent_2>sent) %>% distinct()

  g <- g %>%
    add_edges(icb_events_subset_nodes_edges_time[,c('actor_sent','actor_sent_2')] %>% as.matrix() %>% t() %>% as.vector(),
              actor=icb_events_subset_nodes_edges_time$actor, timeedge=T, leaf='', eventedge=F)


  ######Directed
  icb_events_subset_directed <-
    icb_events_subset_diad %>%
    dplyr::select(sent=sent, actor_1=actor_1, actor_2=actor_2, leaf ) %>% distinct() %>%
    mutate(actor_sent_1= actor_1 %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>%
    mutate(actor_sent_2= actor_2 %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>%
    group_by(sent,actor_1, actor_sent_1,actor_sent_2) %>%
    summarise(leaf=paste(leaf, collapse="\n") %>% tolower() ) #%>%
  #filter(actor_sent_2 %>% str_detect("Q30") | actor_sent_1 %>% str_detect("Q30"))

  g <- g %>% add_edges(icb_events_subset_directed[,c('actor_sent_1','actor_sent_2')] %>% as.matrix() %>% t() %>% as.vector(),
                       timeedge=F,
                       leaf=icb_events_subset_directed$leaf,
                       eventedge=T,
                       actor=icb_events_subset_directed$actor_1)


  p_icb_map <- g %>%
    ggraph("manual",x=xy[,1],y=xy[,2]) +  #geom_edge_parallel

    #Time edges
    geom_edge_link(aes(
      edge_colour=actor,
      filter=timeedge==T
    ),
    edge_width=0.1,
    show.legend = F
    ) +
    geom_node_point(aes(color=actor),size=1,stroke=0.5) +

    #Event edges
    #geom_path(data = hbundle, aes(x, y, group = group), col = "#9d0191", size = 0.05) +
    geom_edge_arc(aes(
      colour=actor,
      label=leaf,
      filter=eventedge==T #& bundle_count==1
    ),
    vjust = -0.75,
    lineheight = .75,
    sep = unit(1, 'mm'),
    edge_width=0.25,
    show.legend = F,
    arrow = arrow(length = unit(2, 'mm')),
    label_size=3,
    check_overlap=global_check_overlap,
    start_cap = circle(3, 'mm'),
    end_cap = circle(3, 'mm'),
    angle_calc = 'along'
    ) +

    #Not in phoenix
    #geom_text(data=fatalities %>% filter(interact_fatalities>1), aes(x=x+4,y=y, size=interact_fatalities+1), color='black', label='☠', angle=0) +
    #Not in phoenix
    #geom_text(data=icb_events_subset_monad, aes(x=x,y=y, label=leaf), nudge_y=40, angle=0, lineheight = .75, size=3) +
    geom_text(data=date_sineposts, aes(x=x,y=y, label=label), color="grey", nudge_y=0, angle=0, size=4, lineheight = .75, fontface = "bold",check_overlap=global_check_overlap) + #-45
    geom_text(data=icb_events_subset_nodes_min, aes(x=x,y=y, label=actor, color=actor), nudge_y=150, angle=0, size=4, lineheight = .75, fontface = "bold") + #-45
    scale_color_manual(values = actor_colors)  +
    scale_edge_color_manual(values = actor_colors) +
    #theme_bw() +
    #labs( title = plot_title ) +
    theme_graph() +
    theme(legend.position = "none") #+
  #Not in phoenix
  #ggimage::geom_image(data= units, aes(x=x+offeset,y=y-30, image = IMAGE), size = 0.015)


  p_icb_map$sentence_count <- icb_events_subset_nodes$sent %>% unique() %>% length()
  p_icb_map$actor_colors <- actor_colors
  p_icb_map$icb_monadic_subset <- icb_monadic_subset

  p_icb_map %>% saveRDS(save_icb_file)

}

for(crisis in unique(ICBe_events_agreed_markdown$crisno)){
  print(crisis)
  icb_crisismaps(crisis)
}

# MIDS ---------------------------------------------

mid4_df <- data.frame(
  stringsAsFactors = FALSE,
  act = c(1L,2L,3L,4L,5L,6L,7L,8L,
          9L,10L,11L,12L,13L,14L,15L,16L,17L,18L,19L,
          20L,21L,22L),
  act_label = c("1. None (1)",
                "2. Threat to use force (2)",
                "3. Threat to blockade (2)",
                "4. Threat to occupy terr. (2)",
                "5. Threat to declare war (2)",
                "6. Threat to join war (2)",
                "7. Show of troops (3)",
                "8. Show of ships (3)",
                "9. Show of planes (3)",
                "10. Alert (3)",
                "12. Mobilization (3)",
                "13. Fortify border (3)",
                "14. Border violation (4)",
                "15. Blockade (4)",
                "16. Occupation of territory (4)",
                "17. Seizure (4)",
                "18. Clash (4)",
                "19. Raid (4)",
                "20. Declaration of war (4)",
                "22. Begin interstate war (5)",
                "23. Join interstate war (5)",
                "24. Use CBR Weapons (5)")
) %>% mutate(act_label= act_label %>% str_replace('[0-9]{1,2}\\. ','') %>% str_replace('\\([1-9]\\)','') )

mid_df <- data.frame(
  stringsAsFactors = FALSE,
  act = c(0L,1L,2L,3L,4L,5L,6L,7L,
          8L,9L,10L,11L,12L,13L,14L,15L,16L,17L,18L,19L,
          20L,21L),
  act_label = c("No militarized action (1)",
                "Threat to use force (2)","Threat to blockade (2)",
                "Threat to occupy terr. (2)","Threat to declare war (2)",
                "Threat to use CBR weapons","Threat to join war (2)",
                "Show of force (3)","Alert (3)","Nuclear alert (3)",
                "Mobilization (3)","Fortify border (3)","Border violation (4)",
                "Blockade (4)","Occupation of territory (4)",
                "Seizure (4)","Attack (4)","Clash (4)","Declaration of war (4)",
                "Use of CBR weapons (4)","Begin interstate war (5)",
                "Join interstate war (5)")
)  %>% mutate(act_label= act_label %>% str_replace('[0-9]{1,2}\\. ','') %>% str_replace('\\([1-9]\\)','') )


mid_df2 <- data.frame(
  stringsAsFactors = FALSE,
  outcome = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L),
  outcome_label = c("0. Ongoing MID",
                    "1. Victory for State A","2. Victory for state B",
                    "3. Yield by State A","4. Yield by State B","5. Stalemate",
                    "6. Compromise","7. Released (for seizures)","8. Unclear",
                    "-9 Missing")
) %>% mutate(outcome_label= outcome_label %>% str_replace('[0-9]{1,2}\\. ','') %>% str_replace('\\([1-9]\\)','') )

dyadic_mid <- read_csv(paste0(here::here(), "/../ICBEdataset/replication_paper/data/in/dyadic_mid_4.02.csv"))

dyadic_mid_clean <- dyadic_mid %>% distinct() %>%
  left_join(actor_translator %>% dplyr::select(statea=COWcode, actor_1=QCode_name, actor_1_qcode=QCode)) %>% #I think we add color here
  left_join(actor_translator %>% dplyr::select(stateb=COWcode, actor_2=QCode_name, actor_2_qcode=QCode)) %>%
  left_join(mid_df  %>% dplyr::select( mid5hiacta=act , leaf_1=act_label )) %>%
  left_join(mid_df  %>% dplyr::select( mid5hiactb=act , leaf_2=act_label )) %>%
  left_join(mid_df2  %>% dplyr::select( outcome=outcome , outcome_label=outcome_label ))


mids_crisismaps <- function(thiscrisis=196, prune_rare=T){

  print(thiscrisis)
  #thiscrisis=471
  plot_title <- crisno_name_years %>% filter(crisno==thiscrisis) %>% mutate(title="Crisis #" %>% paste0(crisno, " ", crisname, " (", yrtrig, "-",yrterm,")")) %>% pull(title)
  save_mids_file <- paste0(here::here(), "//images/ggplots/p_mids_plot_",thiscrisis,".Rds")

  year_start <-icb_crises[icb_crises$crisno==thiscrisis,]$yrtrig
  year_end   <- icb_crises[icb_crises$crisno==thiscrisis,]$yrterm

  start_simple <- icb_crises[icb_crises$crisno==thiscrisis,]$start_simple
  end_simple <- icb_crises[icb_crises$crisno==thiscrisis,]$end_simple

  actor_sent_counts <- icb_events_long %>%
    dplyr::filter(crisno==thiscrisis  ) %>%
    filter(varname_normalized %>% str_detect("actor")) %>%
    dplyr::select(sent=sentence_number_int_aligned,value_normalized,value_qcode) %>%
    na.omit() %>%
    count(value_normalized,value_qcode) %>% arrange(n %>% desc() )

  dyadic_mid_clean_subset <- dyadic_mid_clean %>%
    filter( actor_1_qcode %in% actor_sent_counts$value_qcode & actor_2_qcode %in% actor_sent_counts$value_qcode ) %>%
    filter(strtyr>=year_start & endyear<=year_end) %>% arrange(strtyr, strtmnth, endday)
  dim(dyadic_mid_clean_subset) #
  if(nrow(dyadic_mid_clean_subset)==0 |
     (c(dyadic_mid_clean_subset$namea,dyadic_mid_clean_subset$nameb) %>% na.omit() %>% unique() %>%  length())<2  #Reject if not 2 actors
  ){
    print("No mids events for this crisis")
    return()
  }

  dyadic_mid_clean_dyad <-
    bind_rows(
      dyadic_mid_clean_subset %>% dplyr::select(year=strtyr,month=strtmnth,day=strtday, actor_1,actor_2, leaf=leaf_1) , #
      dyadic_mid_clean_subset %>% dplyr::select(year=endyear,month=endmnth,day=endday, actor_1,actor_2, leaf=outcome_label)
    ) %>% mutate(leaf=ifelse(leaf=="Yield by State A" , "Yield to",leaf)) %>%
    filter(!leaf %in% "Yield by State B") %>%
    mutate(sent= ( year+(month/12)+(day/31/1000) ) %>% rank() %>% round())



  g <- make_empty_graph(n = 0, directed = TRUE)
  #graph_from_data_frame(vertices=nodes) #relations, directed=TRUE,

  icb_events_subset_diad <- dyadic_mid_clean_dyad %>% dplyr::select(year, month, sent=sent, actor_1=actor_1, actor_2=actor_2 ) %>% distinct()
  #Order actors by how many interactions they have together
  temp <- icb_events_subset_diad  %>% dplyr::select(-year, -month) %>% arrange(actor_1, actor_2) %>% distinct()

  temp2 <- bind_rows(
    temp,
    temp %>% setNames(c("story_date", "actor_2", "actor_1" ))
  ) %>% count(actor_1, actor_2) %>% arrange(actor_1, actor_2) %>% arrange(n)

  temp2_wide <- temp2 %>% pivot_wider(id_cols=c(actor_1), names_from=actor_2, values_from=n) %>% mutate_if(is.numeric, tidyr::replace_na, 0) %>% as.data.frame()
  rownames(temp2_wide) <- temp2_wide$actor_1
  temp2_wide$actor_1 <- NULL
  temp2_wide <- temp2_wide %>% as.matrix()
  temp2_wide <- 1 - (temp2_wide/max(temp2_wide))
  temp2_wide <- temp2_wide[sort(rownames(temp2_wide)), sort(colnames(temp2_wide))]
  hc <- hclust(temp2_wide %>% as.dist(), method="single")

  temp_list <- list()
  for(i in 1:nrow(temp2_wide)){
    temp_list[[as.character(i)]] <- cutree(hc, k=i)
  }

  #ideal_ordering <- bind_rows(temp_list) %>% t() %>% as.data.frame() %>% apply(1, paste, collapse='') %>% sort() %>% names() #this will break with more than 9
  #  arrange(across(starts_with("V")))
  ideal_ordering <- hc$labels[hc$order] #There's a bug here, put all the isolated nodes by themselves first

  #####Nodes
  icb_events_subset_nodes <-
    bind_rows(
      icb_events_subset_diad %>% dplyr::select(year, month, sent=sent, actor=actor_1) ,
      icb_events_subset_diad %>% dplyr::select(year, month, sent=sent, actor=actor_2)
    ) %>% na.omit() %>% distinct()  %>% arrange(actor,sent) %>% mutate(actor_sent= actor %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>% #don't na omit this one
    mutate(x=actor %>% factor(levels=actor %>% unique() %>% setdiff(ideal_ordering) %>% c(ideal_ordering)) %>% as.numeric() *100 ,
           y=sent*-1 *100)

  #we centralize everything in qcodes so colors should be consistent across plots for the same actors
  icbe_actor_to_qcode <- icb_events_long %>%
    dplyr::filter(crisno==thiscrisis  ) %>%
    filter(varname_normalized %>% str_detect("actor")) %>%
    dplyr::select(value_normalized,value_qcode) %>%
    na.omit() %>% distinct()

  actor_colors_df <- icbe_actor_to_qcode %>%
    left_join(actor_translator %>% dplyr::select(value_qcode=QCode, actor_color))
  actor_colors <- actor_colors_df$actor_color
  names(actor_colors) = actor_colors_df$value_normalized


  actors=icb_events_subset_nodes$actor %>% unique() %>% as.factor()
  actor_colors = c(brewer.pal(n=11, "Dark2"), brewer.pal(n=11, "Set1"))[1:length(actors)] #ok so pick a bewer pallet with lots of values and then assign it to specific values
  names(actor_colors) = levels(actors)




  date_sineposts <- icb_events_subset_nodes  %>%
    mutate(x=max(x)+50) %>%
    dplyr::select(sent, year, month,x,y) %>%
    na.omit() %>%
    distinct() %>%
    mutate(sent=as.numeric(sent)) %>% arrange(sent) %>%
    mutate(year_float=year+(month/12)) %>%
    group_by(sent) %>%
    filter(year_float==min(year_float)) %>%
    ungroup() %>% as.data.frame() %>%
    filter( row_number()==1 |  ( year_float > lag(year_float)  ) )  %>%
    filter(!duplicated(year_float ) ) %>%
    mutate(label=paste0(year,"-",month.abb[month]))


  ####Actor lavels
  icb_events_subset_nodes_min <-
    icb_events_subset_nodes %>% group_by(actor) %>% filter(sent==sent %>% min()) %>% ungroup() %>%
    rowwise() %>%
    mutate(actor=actor %>% stringi::stri_wrap(width=15) %>% paste0(collapse='\n'))


  for(i in 1:nrow(icb_events_subset_nodes)){
    g <- g %>%  add_vertices(1, name=icb_events_subset_nodes$actor_sent[i],
                             actor = icb_events_subset_nodes$actor[i],
                             sent = icb_events_subset_nodes$sent[i]  ) #, type = nodes$type[i] #, leaf = nodes$leaf[i]
  }

  xy <- cbind(icb_events_subset_nodes$x, icb_events_subset_nodes$y )


  #####Time Edges
  icb_events_subset_nodes_edges_time <- cbind(icb_events_subset_nodes, bind_rows(icb_events_subset_nodes[-1,],icb_events_subset_nodes[1,] ) ) %>% janitor::clean_names() %>% filter(actor==actor_2 & sent_2>sent) %>% distinct()

  g <- g %>%
    add_edges(icb_events_subset_nodes_edges_time[,c('actor_sent','actor_sent_2')] %>% as.matrix() %>% t() %>% as.vector(),
              actor=icb_events_subset_nodes_edges_time$actor, timeedge=T, leaf='', eventedge=F)


  ######Directed
  icb_events_subset_directed <-
    dyadic_mid_clean_dyad %>%
    dplyr::select(sent=sent, actor_1=actor_1, actor_2=actor_2, leaf ) %>% distinct() %>%
    mutate(actor_sent_1= actor_1 %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>%
    mutate(actor_sent_2= actor_2 %>% paste0("_", str_pad(sent, 2, pad = "0") )) %>%
    group_by(sent,actor_1, actor_sent_1,actor_sent_2) %>%
    summarise(leaf=paste(leaf, collapse="\n") %>% tolower() ) #%>%
  #filter(actor_sent_2 %>% str_detect("Q30") | actor_sent_1 %>% str_detect("Q30"))

  g <- g %>% add_edges(icb_events_subset_directed[,c('actor_sent_1','actor_sent_2')] %>% as.matrix() %>% t() %>% as.vector(),
                       timeedge=F,
                       leaf=icb_events_subset_directed$leaf,
                       eventedge=T,
                       actor=icb_events_subset_directed$actor_1)

  p_mids_map <- g %>%
    ggraph("manual",x=xy[,1],y=xy[,2]) +  #geom_edge_parallel

    #Time edges
    geom_edge_link(aes(
      edge_colour=actor,
      filter=timeedge==T
    ),
    edge_width=0.1,
    show.legend = F
    ) +
    geom_node_point(aes(color=actor),size=1,stroke=0.5) +

    #Event edges
    #geom_path(data = hbundle, aes(x, y, group = group), col = "#9d0191", size = 0.05) +
    geom_edge_arc(aes(
      colour=actor,
      label=leaf,
      filter=eventedge==T #& bundle_count==1
    ),
    vjust = -0.75,
    lineheight = .75,
    sep = unit(1, 'mm'),
    edge_width=0.25,
    show.legend = F,
    arrow = arrow(length = unit(2, 'mm')),
    label_size=3,
    check_overlap=global_check_overlap,
    start_cap = circle(3, 'mm'),
    end_cap = circle(3, 'mm'),
    angle_calc = 'along'
    ) +

    #Not in phoenix
    #geom_text(data=fatalities %>% filter(interact_fatalities>1), aes(x=x+4,y=y, size=interact_fatalities+1), color='black', label='☠', angle=0) +
    #Not in phoenix
    #geom_text(data=icb_events_subset_monad, aes(x=x,y=y, label=leaf), nudge_y=40, angle=0, lineheight = .75, size=3) +
    geom_text(data=date_sineposts, aes(x=x,y=y, label=label), color="grey", nudge_y=0, angle=0, size=4, lineheight = .75, fontface = "bold",check_overlap=global_check_overlap) + #-45
    geom_text(data=icb_events_subset_nodes_min, aes(x=x,y=y, label=actor, color=actor), nudge_y=150, angle=0, size=4, lineheight = .75, fontface = "bold") + #-45
    scale_color_manual(values = actor_colors)  +
    scale_edge_color_manual(values = actor_colors) +
    #theme_bw() +
    #labs( title = plot_title ) +
    theme_graph() +
    theme(legend.position = "none") #+
  #Not in phoenix
  #ggimage::geom_image(data= units, aes(x=x+offeset,y=y-30, image = IMAGE), size = 0.015)


  p_mids_map$sentence_count <- icb_events_subset_nodes$sent %>% unique() %>% length()
  p_mids_map$actor_colors <- actor_colors
  p_mids_map$dyadic_mid_clean <- dyadic_mid_clean


  p_mids_map %>% saveRDS(save_mids_file)
}

for(crisis in unique(ICBe_events_agreed_markdown$crisno)){
  print(crisis)
  mids_crisismaps(crisis)
}

# Phoenix ------------------

#[@althausClineCenterHistorical2020a]
#https://databank.illinois.edu/datasets/IDB-2796521

library(tidyverse)
library(countrycode)
library(igraph)
library(ggraph)

#8,491,556
library(stringr)
fromscratch=F
if(fromscratch){

  convert_aid = function(x) { x %>% mutate(aid = aid %>% format( scientific = FALSE)) %>% mutate_all(as.character) }
  meta1_nyt2 <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/MetaDataBLN-NYT_1980-2018.csv", col_types=rep('c',50)  %>% paste0(collapse='')) #%>% convert_aid()
  meta2_swb <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/MetaDataBLN-SWB_1979-2019.csv", col_types=rep('c',50)  %>% paste0(collapse='')) #%>% convert_aid()
  meta_fbis <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/MetaDataFBIS_1995-2004.csv", col_types=rep('c',50)  %>% paste0(collapse='')) #%>% convert_aid()
  meta_nyt <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/MetaDataNYT_1945-2005.csv", col_types=rep('c',50)  %>% paste0(collapse='')) #%>% convert_aid()
  meta_wsj <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/MetaDataWSJ_1945-2006.csv", col_types=rep('c',50)  %>% paste0(collapse='')) #%>% convert_aid()

  events_nyt2 <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/PhoenixBLN-NYT_1980-2018.csv", col_types=rep('c',50)  %>% paste0(collapse='')) # %>% convert_aid()
  events_swb <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/PhoenixBLN-SWB_1979-2019.csv", col_types=rep('c',50) %>% paste0(collapse='')) #%>% convert_aid()
  events_fbis <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/PhoenixFBIS_1995-2004.csv", col_types=rep('c',50) %>% paste0(collapse='')) #%>% convert_aid()
  events_nyt <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/PhoenixNYT_1945-2005.csv", col_types=rep('c',50) %>% paste0(collapse='')) #%>% convert_aid()
  events_wsj <- read_csv("/mnt/8tb_a/rwd_github_private/icbewebsite/git_ignore_folder/DOI-10-13012-b2idb-0647142_v3/PhoenixWSJ_1945-2006.csv", col_types=rep('c',50) %>% paste0(collapse='')) #%>% convert_aid()

  setdiff(events_nyt2$aid , meta1_nyt2$aid) %>% length() #0 #316,201 #yup that was it if you import as numeric it converts to scientific internally and breaks everything
  setdiff(meta1_nyt2$aid , events_nyt2$aid) %>% length() #0 #613,981 #Have to keep as a string throughout

  phoenix <- bind_rows(events_nyt2 %>% left_join(meta1_nyt2),
                       events_swb %>% left_join(meta2_swb),
                       events_fbis %>% left_join(meta_fbis),
                       events_nyt %>% left_join(meta_nyt),
                       events_wsj %>% left_join(meta_wsj)
  )

  target_file <- paste0(here::here(),"/../ICBEdataset/replication_paper/data/in/icb_manual_recoding_master_sheet.xlsx")
  actor_translator    <- readxl::read_excel(target_file, sheet="actor_translator")

  root_codes <- phoenix$root_code %>% unique()

  #library("remotes")
  #remotes::install_github("andybega/icews")
  library(icews)
  data("cameo_codes") #we're going to grab the cameo codes from the ICEWS package because they're a nightmare
  cameo_codes_goldstein <- cameo_codes %>% left_join(goldstein_mappings) %>% mutate(name=name %>% tolower())

  phoenix_clean <- phoenix %>%
    #head(10000) %>%
    left_join(cameo_codes_goldstein %>% dplyr::select(code=cameo_code , cameo_event_label=name,
                                                      cameo_penta_category=penta_category, cameo_quad_category=quad_category,
                                                      cameo_goldstein=goldstein, cameo_lvl0=lvl0 ) ) %>%
    left_join(    cameo_codes_goldstein %>% filter(level==0) %>% dplyr::select(cameo_lvl0=lvl0, cameo_lvl0_label=name)) %>% #
    #
    left_join(cameo_actor_codes %>% dplyr::select(source=code, source_label=name)  )  %>%
    left_join(cameo_actor_codes %>% dplyr::select(target=code, target_label=name)  )  %>%
    #
    left_join(cameo_actor_codes %>% dplyr::select(source_root=code, source_label=name)  )  %>%
    left_join(cameo_actor_codes %>% dplyr::select(target_root=code, target_label=name)  )  %>%
    #
    left_join(cameo_actor_codes %>% dplyr::select(source_agent=code, source_agent_label=name) %>% distinct() %>% na.omit() ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(target_agent=code, target_agent_label=name) %>% distinct() %>% na.omit() ) %>%
    #
    separate(source_others, c("source_others1", "source_others2", "source_others3"), sep = ";", remove=F, extra="drop") %>%
    separate(target_others, c("target_others1", "target_others2", "target_others3"), sep = ";", remove=F, extra="drop") %>%
    left_join(cameo_actor_codes %>% dplyr::select(source_others1=code, source_others1_label=name) %>% distinct() %>% na.omit() ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(target_others1=code, target_others1_label=name) %>% distinct() %>% na.omit() )  %>%
    left_join(cameo_actor_codes %>% dplyr::select(source_others2=code, source_others2_label=name) %>% distinct() %>% na.omit() ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(target_others2=code, target_others2_label=name) %>% distinct() %>% na.omit() )  %>%
    left_join(cameo_actor_codes %>% dplyr::select(source_others3=code, source_others3_label=name) %>% distinct() %>% na.omit() ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(target_others3=code, target_others3_label=name) %>% distinct() %>% na.omit() )  %>%

    mutate(date_simple=as.numeric(year) + (as.numeric(month)/12) ) %>%

    mutate(joined_issues=ifelse( joined_issues=='',";", joined_issues)  ) %>% #Apparently unnesting an empty string just gets dropped
    mutate(joined_issues = strsplit(as.character(joined_issues), ";") ) %>%  unnest(joined_issues)  %>% #This drops if the row is missing, I did not know that was it
    #
    left_join(actor_translator %>% dplyr::select(source_root=phoenix_2020, source_root_label=QCode_name, source_root_qcode=QCode) %>% distinct() %>% na.omit() ) %>%
    left_join(actor_translator %>% dplyr::select(target_root=phoenix_2020, target_root_label=QCode_name, target_root_qcode=QCode) %>% distinct() %>% na.omit() ) %>%
    mutate(month_label = lubridate::month(month %>% as.numeric(),label=TRUE,abbr=F)) %>%
    mutate(source_agent_label = source_agent_label %>% tolower()) %>%
    mutate(target_agent_label = target_agent_label %>% tolower()) %>%
    mutate(source_others1_label=source_others1_label %>% tolower()) %>%
    mutate(target_others1_label=target_others1_label %>% tolower()) %>%
    mutate(source_others2_label=source_others2_label %>% tolower()) %>%
    mutate(target_others2_label=target_others2_label %>% tolower()) %>%
    mutate(source_others3_label=source_others3_label %>% tolower()) %>%ssa
    mutate(target_others3_label=target_others3_label %>% tolower()) %>%
    unite(text,
          source_root_label, source_agent_label, source_others1_label, source_others2_label, source_others3_label,
          cameo_event_label,
          target_agent_label, target_root_label, target_others1_label, target_others2_label, target_others3_label,
          sep=" ", remove = F, na.rm = T) %>%
    mutate(text = ifelse(!is.na(placename),paste0(text, " in ", placename),text) ) %>%
    mutate(text = ifelse(!is.na(joined_issues),paste0(text, " on the topic of ", joined_issues %>% tolower() %>% str_replace(",[0-9]","") ),text) ) %>%
    mutate(text = paste0("On ", month_label, " ",day %>% as.numeric()," ",year, ", ", text)) %>%
    mutate(text=text %>% str_replace(", not specified below","")) %>%
    arrange(year, month, day)  %>% mutate(date = paste(day,"-",month,"-",year) %>% lubridate::dmy()) %>%
    unite('dyad_a', source_root_label, source_agent_label, source_others1_label, source_others2_label, source_others3_label, na.rm = TRUE, remove = FALSE) %>%
    unite('dyad_b', target_root_label, target_agent_label, target_others1_label, target_others2_label, target_others3_label, na.rm = TRUE, remove = FALSE) %>%
    mutate(dyad=paste0(dyad_a,"->",dyad_b)) %>%
    mutate(dyad_event=paste0(dyad, "_", cameo_event_label)) %>%
    mutate(dyad_penta_category=paste0(dyad, "_", cameo_penta_category)) %>%
    mutate(dyad_lvl0_label=paste0(dyad, "_", cameo_lvl0_label))

  dim(phoenix_clean) #8,495,402 #3,721,110 #2,813,748 #2912035 #3,508,495 #4,854,787
  phoenix_clean %>% arrow::write_parquet(here::here("git_ignore_folder",  "phoenix_clean.parquet") )

}



library(ggtext)
phoenix_barmaps <- function(thiscrisis=196, topactor=F){

  #print(thiscrisis)
  plot_title <- crisno_name_years %>% filter(crisno==thiscrisis) %>% mutate(title="Crisis #" %>% paste0(crisno, " ", crisname, " (", yrtrig, "-",yrterm,")")) %>% pull(title)
  save_file_lv0 <- paste0(here::here(), "//images/ggplots/p_phoenix_plot_",crisis,".Rds")
  #paste0(here::here(), "/../ICBEdataset/replication_datafigures/p_phoenix_plot_lv0_",thiscrisis,".Rds")

  date_start <- lubridate::as_date(paste0(icb_crises[icb_crises$crisno==thiscrisis,]$yrtrig , "-", icb_crises[icb_crises$crisno==thiscrisis,]$motrig, "-01"))
  date_end <- lubridate::as_date(paste0(icb_crises[icb_crises$crisno==thiscrisis,]$yrterm , "-", icb_crises[icb_crises$crisno==thiscrisis,]$moterm, "-01"))
  date_end <- date_end %>% lubridate::ceiling_date() %>% lubridate::as_date()

  year_start <-icb_crises[icb_crises$crisno==thiscrisis,]$yrtrig
  year_end   <- icb_crises[icb_crises$crisno==thiscrisis,]$yrterm

  actors <- icb_events_long %>% filter(crisno %in% thiscrisis) %>% filter(varname_normalized %>% str_detect('actor')) %>%
    dplyr::select(value_normalized,value_qcode) %>% distinct()

  #tictoc::tic()
  phoenix_clean_subset <- phoenix_clean_small %>%
    #mutate(date = story_date %>% lubridate::mdy()) %>% #
    filter(date>=(date_start-lubridate::years(1)) & date<=(date_end+lubridate::years(1)))  %>%

    #unite('temp', everything() , na.rm = TRUE, remove = FALSE) %>%
    filter(text %>% str_detect(actors$value_normalized %>% paste0(collapse="|")) ) #%>%
    #dplyr::select(-temp)
  if(nrow(phoenix_clean_subset)==0){return()}
  dim(phoenix_clean_subset)
  #tictoc::toc()

  names_to_qcodes <- actor_translator %>% dplyr::select(QCode, contains("name")) %>%
    pivot_longer(-c(QCode)) %>% na.omit() %>% dplyr::select(-name) %>% distinct() %>%
    mutate(markdown = glue::glue("![]({flag_folder_small}/{QCode}.png)") )
  names_to_qcodes <- bind_rows(names_to_qcodes, names_to_qcodes %>% mutate(value= value %>% tolower() )) %>% distinct() %>%
    arrange(desc(nchar(value))) #sort by length so you never accidentally replace a subset

  #cameo_lvl0_label
  dyad_event_counts <-     phoenix_clean_subset  %>%
                            #mutate(story_date=paste(month, day, year, sep="/")  %>% lubridate::mdy() )  %>%
                            #oh fun there's an edge case with events that have a single day #switching to give it a two week grace period on either side
                            filter(date>=((date_start %>% as.Date())-lubridate::days(14)) & date<=((date_end %>% as.Date() )+lubridate::days(14)  )) %>%
                            count(cameo_lvl0_label, dyad_lvl0_label) %>%
                            group_by(cameo_lvl0_label) %>%
                            arrange(cameo_lvl0_label,  desc(n) ) %>%
                            filter(row_number()<=10) %>% ungroup()

  phoenix_clean_subset_top10 <- phoenix_clean_subset %>%
    filter(dyad_lvl0_label %in% dyad_event_counts$dyad_lvl0_label  ) %>%
    mutate(dyad_markdown=dyad)
  for(i in 1:nrow(names_to_qcodes)){
    phoenix_clean_subset_top10 <- phoenix_clean_subset_top10 %>%
      dplyr::mutate(dyad_markdown = dyad_markdown %>% str_replace_all(names_to_qcodes$value[i], names_to_qcodes$markdown[i]  ))
  }
  library(ggtext)
  phoenix_clean_subset_top10 <- phoenix_clean_subset_top10 %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("government","gov")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("law enforcement agencies","law enforcement")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("\\(tertiary role code\\)","")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("unidentified armed force"," unknown force"))

  phoenix_clean_subset_top10_day <- phoenix_clean_subset_top10 %>%
                                    filter(!is.na(cameo_lvl0_label)) %>%
                                    group_by(dyad_markdown, cameo_lvl0_label, date) %>%
                                    summarise(mean_goldstein=mean(cameo_goldstein), n=n())

  p_phoenix_lv0 <- phoenix_clean_subset_top10_day %>%
    group_by(cameo_lvl0_label, dyad_markdown) %>% mutate(n_all=sum(n, na.rm=T)) %>% ungroup() %>%
    mutate(dyad_markdown = tidytext::reorder_within(dyad_markdown, n_all, within = cameo_lvl0_label)) %>%
    ggplot( aes(x = date, y = dyad_markdown, fill = mean_goldstein)) +
    geom_tile() + theme_bw() +
    tidytext::scale_y_reordered() +
    geomtextpath::geom_textvline(xintercept = as.Date(date_start), label="Crisis Start (ICB)" , linetype="dotted", color = "black", size=3) +
    geomtextpath::geom_textvline(xintercept = as.Date(date_end), label="Crisis End (ICB)" , linetype="dotted", color = "black", size=3) +
    facet_wrap(~cameo_lvl0_label, scale="free_y", ncol=1)  +
    theme(plot.margin = unit(c(0,0,0,0), "cm")) + theme( axis.text.y = element_markdown(size = 12, lineheight = 0.1)  ) +
    scale_fill_gradient2() +
    theme(legend.position="none") +
    ylab("") + xlab("")

  p_phoenix_lv0 %>% saveRDS(save_file_lv0)

}


phoenix_clean_small <- arrow::read_parquet(here::here("git_ignore_folder", "phoenix_clean.parquet") ,
                                           col_select=c('date','dyad','cameo_lvl0_label','dyad_lvl0_label','text','cameo_goldstein'))
dim(phoenix_clean_small) #8495402      64
for(crisis in  icb_crises %>% filter(yrtrig>=1945) %>% pull(crisno) %>% unique()){
  print(crisis)
  tictoc::tic()
  phoenix_barmaps(crisis)
  tictoc::toc()
}

# Terrier ------------------

#https://osf.io/4m2u7/

terrier_agent_labels <- data.frame(
  stringsAsFactors = FALSE,
  agent = c("GOV","SPY","OPP",
            "MIL","PTY","REB","EDU","COP","JUD","CRM",
            "BUS","MED","CVL","UAF","AGR","ELI","DEV","ENV",
            "HLH","HRI","LAB","LEG","REF","MOD","RAD"),
  agent_label = c("Government",
                  "Intelligence agencies",
                  "Opposition groups (often parliamentary)",
                  "Military",
                  "Political party",
                  "Rebels/insurgents",
                  "Education-focused actors",
                  "Law enforcement agencies",
                  "Judicial actors",
                  "Criminal actors",
                  "Business (NOT MNCs)",
                  "Media-related actors/organizations",
                  "Civilians",
                  "Unaffiliated armed forces",
                  "Agriculture-focused actors",
                  "Elites, celebrities",
                  "Development-focused actors",
                  "Environment-focused actors",
                  "Health-focused actors",
                  "Human rights-focused actors",
                  "Labor-focused actors",
                  "Legislative bodies/actors",
                  "Refugee-focused actors",
                  "Moderate/mainstream group",
                  "Radical/extremist/fundamentalist")
) %>% janitor::clean_names() %>%
  mutate(agent_label = agent_label %>% tolower() )

#if(file.exists(paste0(here::here(), "/../ICBEdataset/replication_paper/data/ignore/terrier/largegeolocateddata//"))){
#  terrier_fromscratch <-readline(prompt="Load original Terrier data? (T/F)")
#}
#terrier_fromscratch <- terrier_fromscratch %in% "T"

#CAMEO_eventcodes <- read_tsv(paste0(here::here(), "/../ICBEdataset/replication_paper/data/in/CAMEO.eventcodes.txt"), col_types='cc' ) %>% janitor::clean_names()
#codes_used <-terrier_clean %>% count(cameo_code)

#Switching to data table with dplyr wrapper
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
terrier_fromscratch <- F
if(terrier_fromscratch){
  files <- list.files(path = paste0(here::here(), "/git_ignore_folder/largegeolocateddata/"), pattern = ".tsv$", full.names = T)
  terrier <- rbindlist(lapply(files, FUN=function(x) fread(x,header=F, colClasses=paste0(rep('character',50), collapse='')  )  ) ) #%>% janitor::clean_names()
  dim(terrier) #28,438,029 #28,370,448       28

  actor_translator_terrier <- actor_translator %>% dplyr::select( terrier,  QCode, QCode_name) %>% filter(!duplicated(terrier)) %>% as.data.table() %>% na.omit()

  #library("remotes")
  #remotes::install_github("andybega/icews")
  library(icews)
  data("cameo_codes") #we're going to grab the cameo codes from the ICEWS package because they're a nightmare
  cameo_codes_goldstein <- cameo_codes %>% left_join(goldstein_mappings) %>% mutate(name=name %>% tolower())

  library(tictoc)
  tictoc::tic()
  dim(terrier) #28,438,029
  #terrier_clean_temp %>% as.data.table() %>% dim()
  #Maybe it's just ISO3 country codes?
  terrier_clean_temp <- terrier %>%
    #head(100000) %>%
    as_tibble() %>%
    dplyr::select(
      #id=V1,
      hash=V2,
      cameo_code=V3,
      src_country=V8,
      src_agent=V9,
      src_other_agent=V10,

      target_country=V12 ,
      target_agent=V13,
      target_other_agent=V14,

      year=V17,
      month=V18,
      day=V19,
      where=V21#,
      #source=V7
      #random_text=x28
    ) %>%
    left_join(cameo_codes_goldstein %>% mutate(cameo_code=cameo_code ) %>% dplyr::select(cameo_code=cameo_code, cameo_label= name, cameo_goldstein=goldstein,
                                                                                         cameo_penta_category=penta_category, cameo_lvl0=lvl0)  ) %>% #
    left_join(    cameo_codes_goldstein %>% filter(level==0) %>% dplyr::select(cameo_lvl0=lvl0, cameo_lvl0_label=name)) %>% #
    mutate( cameo_label= cameo_label %>% str_replace(', not specified below','')) %>%

    mutate( cameo_label= cameo_label %>% str_replace(', not specified below','')) %>%
    tidyr::separate(target_other_agent, c("target_other_agent1", "target_other_agent2", "target_other_agent3",
                                          'trash1','trash2','trash3','trash4','trash5','trash6','trash7','trash8','trash9','trash10'), sep = ";", remove=F, extra="drop") %>%
    dplyr::select(-starts_with("trash")) %>%
    tidyr::separate(src_other_agent   , c("src_other_agent1"   , "src_other_agent2"   , "src_other_agent3",
                                          'trash1','trash2','trash3','trash4','trash5','trash6','trash7','trash8','trash9','trash10','trash11'), sep = ";", remove=F, extra="drop") %>%
    dplyr::select(-starts_with("trash")) %>%

    left_join(cameo_actor_codes %>% dplyr::select(src_country=code, src_country_label= name) %>% as.data.table()  ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(target_country=code, target_country_label= name) %>% as.data.table()  ) %>%

    left_join(cameo_actor_codes %>% dplyr::select(src_agent=code, src_agent_label= name) %>% as.data.table()  ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(src_other_agent1=code, src_other_agent1_label= name) %>% as.data.table()  ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(src_other_agent2=code, src_other_agent2_label= name) %>% as.data.table()  ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(src_other_agent3=code, src_other_agent3_label= name) %>% as.data.table()  ) %>%

    left_join(cameo_actor_codes %>% dplyr::select(target_agent=code, target_agent_label= name) %>% as.data.table()   ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(target_other_agent1=code, target_other_agent1_label= name) %>% as.data.table()  ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(target_other_agent2=code, target_other_agent2_label= name) %>% as.data.table()  ) %>%
    left_join(cameo_actor_codes %>% dplyr::select(target_other_agent3=code, target_other_agent3_label= name) %>% as.data.table()  ) %>%

    left_join(actor_translator_terrier %>% dplyr::select(src_country   = terrier, actor_1_qcode = QCode, actor_1_label=QCode_name) %>% as.data.table() %>% na.omit() ) %>%
    left_join(actor_translator_terrier %>% dplyr::select(target_country= terrier, actor_2_qcode = QCode, actor_2_label=QCode_name) %>% as.data.table() %>% na.omit()  ) %>%
    mutate(month_label = lubridate::month(month %>% as.numeric(),label=TRUE,abbr=F)) %>%
    mutate(cameo_label = cameo_label %>% tolower() ) %>%
    mutate(year= year %>% as.numeric() ) %>%
    mutate(month= month %>% as.numeric() ) %>%
    mutate(day= day %>% as.numeric() )  %>%
    mutate(story_date=paste(month, day, year, sep="/")  %>% lubridate::mdy() ) %>%
    unite('dyad_a', src_country_label,src_agent_label,src_other_agent1_label,src_other_agent2_label,src_other_agent3_label, na.rm = TRUE, remove = FALSE) %>%
    unite('dyad_b', target_country_label, target_agent_label,target_other_agent1_label,target_other_agent2_label,target_other_agent3_label, na.rm = TRUE, remove = FALSE) %>%
    unite('temp_text', src_country_label,src_agent_label,src_other_agent1_label,src_other_agent2_label,src_other_agent3_label,
          target_country_label, target_agent_label,target_other_agent1_label,target_other_agent2_label,target_other_agent3_label,
          where,
          na.rm = TRUE, remove = FALSE) %>%
    mutate(dyad=paste0(dyad_a,"->",dyad_b)) %>%
    mutate(dyad_event=paste0(dyad, "_", cameo_label)) %>%
    mutate(dyad_penta_category=paste0(dyad, "_", cameo_penta_category)) %>%
    mutate(dyad_lvl0_category=paste0(dyad, "_", cameo_lvl0_label)) %>%
    as_tibble()
  tictoc::toc() #313.74
  dim(terrier_clean_temp) #28,438,029  #31,117,198 #39322909

  #terrier_clean_temp %>% saveRDS( here::here("replication_paper", "data", "in", "terrier_clean.Rds"))
  library(arrow) #you have to actually load the library
  terrier_clean_temp  %>%
    arrow::write_parquet(here::here("git_ignore_folder", "terrier_clean.parquet") )
}


library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

terrier_barmaps <- function(thiscrisis=471, topactor=F){

  print(thiscrisis)
  plot_title <- crisno_name_years %>% filter(crisno==thiscrisis) %>% mutate(title="Crisis #" %>% paste0(crisno, " ", crisname, " (", yrtrig, "-",yrterm,")")) %>% pull(title)
  #save_file_lv0 <- paste0(here::here(), "/../ICBEdataset/replication_datafigures/p_terrier_plot_lv0_",thiscrisis,".Rds")
  save_file_lv0 <- paste0(here::here(), "///images/ggplots/p_terrier_plot_",thiscrisis,".Rds")

  date_start <- lubridate::as_date(paste0(icb_crises[icb_crises$crisno==thiscrisis,]$yrtrig , "-", icb_crises[icb_crises$crisno==thiscrisis,]$motrig, "-01"))
  date_end <- lubridate::as_date(paste0(icb_crises[icb_crises$crisno==thiscrisis,]$yrterm , "-", icb_crises[icb_crises$crisno==thiscrisis,]$moterm, "-01"))
  date_end <- date_end %>% lubridate::ceiling_date() %>% lubridate::as_date()

  year_start <-icb_crises[icb_crises$crisno==thiscrisis,]$yrtrig
  year_end   <- icb_crises[icb_crises$crisno==thiscrisis,]$yrterm

  actors <- icb_events_long %>% filter(crisno %in% thiscrisis) %>% filter(varname_normalized %>% str_detect('actor')) %>%
    dplyr::select(value_normalized,value_qcode) %>% distinct()

  terrier_clean_subset <- terrier_clean_small %>%
                          filter(story_date>=((date_start %>% as.Date())-lubridate::years(1)) & story_date<=((date_end %>% as.Date() )+lubridate::years(1)  )) %>%
                          filter(temp_text %>% str_detect(actors$value_normalized %>% paste0(collapse="|")))
  dim(terrier_clean_subset) #928,640
  if(nrow(terrier_clean_subset)==0){return()}

  names_to_qcodes <- actor_translator %>% dplyr::select(QCode, contains("name")) %>%
    pivot_longer(-c(QCode)) %>% na.omit() %>% dplyr::select(-name) %>% distinct() %>%
    mutate(markdown = glue::glue("![]({flag_folder_small}/{QCode}.png)") )
  names_to_qcodes <- bind_rows(names_to_qcodes, names_to_qcodes %>% mutate(value= value %>% tolower() )) %>% distinct() %>%
    arrange(desc(nchar(value))) #sort by length so you never accidentally replace a subset

  #_lvl0
  dyad_event_counts <-     terrier_clean_subset  %>%
                            filter(story_date>=((date_start %>% as.Date())-lubridate::days(14)) & story_date<=((date_end %>% as.Date() )+lubridate::days(14)  )) %>%
                            count(cameo_lvl0_label, dyad_lvl0_category) %>%
                            group_by(cameo_lvl0_label) %>%
                            arrange(cameo_lvl0_label,  desc(n) ) %>%
                            filter(row_number()<=10) %>% ungroup()
  if(nrow(dyad_event_counts)==0){return()}

  terrier_clean_subset_top10 <- terrier_clean_subset %>%
                                  filter(dyad_lvl0_category %in% dyad_event_counts$dyad_lvl0_category  ) %>%
                                  mutate(dyad_markdown=dyad)

  terrier_clean_subset_top10_day <- terrier_clean_subset_top10 %>%
    rename(date=story_date) %>%
    filter(!is.na(cameo_lvl0_label)) %>%
    group_by(dyad_markdown, cameo_lvl0_label, date) %>%
    summarise(mean_goldstein=mean(cameo_goldstein), n=n())

  for(i in 1:nrow(names_to_qcodes)){
    #print(i)
    terrier_clean_subset_top10_day <- terrier_clean_subset_top10_day %>%
      dplyr::mutate(dyad_markdown = dyad_markdown %>% str_replace_all(names_to_qcodes$value[i], names_to_qcodes$markdown[i]  ))
  } #This is a bottleneck now and needs to move to after the group
  library(ggtext)
  terrier_clean_subset_top10_day <- terrier_clean_subset_top10_day %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("Inter-governmental organizations","IGOs")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("government[^a-z]|Government[^a-z]","gov")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("law enforcement agencies","law enforcement")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all(fixed("(tertiary role code)"),"")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("Unidentified Armed Force","Unknown Mil."))

  p_terrier_lv0 <-  terrier_clean_subset_top10_day %>%
    group_by(cameo_lvl0_label, dyad_markdown) %>% mutate(n_all=sum(n, na.rm=T)) %>% ungroup() %>%
    mutate(dyad_markdown = tidytext::reorder_within(dyad_markdown, n_all, within = cameo_lvl0_label)) %>%
    ggplot( aes(x = date, y = dyad_markdown, fill = mean_goldstein)) +
    geom_tile() + theme_bw() +
    tidytext::scale_y_reordered() +
    geomtextpath::geom_textvline(xintercept = as.Date(date_start), label="Crisis Start (ICB)" , linetype="dotted", color = "black", size=3) +
    geomtextpath::geom_textvline(xintercept = as.Date(date_end), label="Crisis End (ICB)" , linetype="dotted", color = "black", size=3) +
    facet_wrap(~cameo_lvl0_label, scale="free_y", ncol=1)  +
    theme(plot.margin = unit(c(0,0,0,0), "cm")) + theme( axis.text.y = element_markdown(size = 12, lineheight = 0.1)  ) +
    scale_fill_gradient2() +
    theme(legend.position="none") +
    ylab("") + xlab("")

  p_terrier_lv0 %>% saveRDS(save_file_lv0)

}

#done
#install.packages("arrow")
terrier_clean_small <-  arrow::read_parquet( here::here("git_ignore_folder", "terrier_clean.parquet"),
                                       col_select=c('story_date','dyad','cameo_lvl0_label','dyad_lvl0_category','temp_text','cameo_goldstein') )
#203mb
dim(terrier_clean_small) #28,438,029 #39,322,909 #7,847,463

for(crisis in  icb_crises %>% filter(yrtrig>=1977) %>% pull(crisno) %>% unique()){
  print(crisis)
  tictoc::tic()
  terrier_barmaps(crisis)
  tictoc::toc()
}


# ICEWS ---------

fromscratch=F
if(fromscratch){
  #accidentally loaded the pdfs
  files <- list.files(path = paste0(here::here(), "/git_ignore_folder/dataverse_files/"), full.names = T, pattern = ".tab$|.tsv$") #there's both tab and tsv for some reason
  icews <- bind_rows(lapply(files, FUN=function(x) read_tsv(x, show_col_types = FALSE) )  ) %>% janitor::clean_names() %>% distinct()
  dim(icews) #18,403,260 #15,751,586

  codes_used <-icews %>% count(cameo_code)

  actor_translator_icews <- actor_translator %>%  dplyr::select(ICEWS_Actor.Name, ICEWS_Country, QCode, QCode_name)
  #%>% mutate(icews=ifelse(ICEWS_Country=="International Government Organizations", ICEWS_Actor.Name, ICEWS_Country))

  #Going to heavily filter on source sectors and source name
  #c(icews$source_sectors,icews$source_name) %>% table()

  #This doesn't totally work because India has 3 copies. The lower thing gets a different q code than the higher thing, so we first have to try to match on the lower, and if that fails match on the higher kinda
  #Indian Administered Kashmir		India

  #library("remotes")
  #remotes::install_github("andybega/icews")
  library(icews)
  data("cameo_codes") #we're going to grab the cameo codes from the ICEWS package because they're a nightmare
  cameo_codes_goldstein <- cameo_codes %>% left_join(goldstein_mappings) %>% mutate(name=name %>% tolower())

  dim(icews) #18,403,260 #15,751,586
  icews_clean <- icews %>%
    #filter(event_id==20776790) %>%
    #head(10000) %>%
    mutate_if(is.character, na_if, "null") %>%
    mutate_if(is.character, na_if, "NULL") %>%
    #head(1000) %>%
    mutate(where = coalesce(city, district,province)) %>%
    left_join(cameo_codes_goldstein %>% mutate(cameo_code=cameo_code ) %>%
                dplyr::select(cameo_code=cameo_code, cameo_label= name, cameo_goldstein=goldstein, cameo_penta_category=penta_category,
                              cameo_lvl0=lvl0)  ) %>% #it's that insane cameo code bug
    left_join(    cameo_codes_goldstein %>% filter(level==0) %>% dplyr::select(cameo_lvl0=lvl0, cameo_lvl0_label=name)) %>% #it's that insane cameo code bug
    mutate( cameo_label= cameo_label %>% str_replace(', not specified below','')) %>%

    left_join( actor_translator_icews %>% dplyr::select(source_name=ICEWS_Actor.Name,  source_q_code_actor=QCode, source_q_label_actor=QCode_name) %>%
                 distinct() %>% na.omit() ) %>%
    left_join( actor_translator_icews %>% filter(is.na(ICEWS_Actor.Name)) %>%
                 dplyr::select(source_country=ICEWS_Country,  source_q_code_country=QCode, source_q_label_country=QCode_name) %>%
                 distinct() %>% na.omit() ) %>%
    mutate(source_q_code=ifelse(!is.na(source_q_code_actor) , source_q_code_actor, source_q_code_country)) %>%
    mutate(source_label=ifelse(!is.na(source_q_code_actor) , source_q_label_actor, source_q_label_country)) %>%

    left_join( actor_translator_icews %>% dplyr::select(target_name=ICEWS_Actor.Name,  target_q_code_actor=QCode, target_q_label_actor=QCode_name) %>%
                 distinct() %>% na.omit() ) %>%
    left_join( actor_translator_icews %>% filter(is.na(ICEWS_Actor.Name)) %>%
                 dplyr::select(target_country=ICEWS_Country,  target_q_code_country=QCode, target_q_label_country=QCode_name) %>%
                 distinct() %>% na.omit() ) %>%
    mutate(target_q_code=ifelse(!is.na(target_q_code_actor) , target_q_code_actor, target_q_code_country)) %>%
    mutate(target_label=ifelse(!is.na(target_q_code_actor) , target_q_label_actor, target_q_label_country)) %>%

    mutate(source_target_q_code= paste0(source_q_code,"_",target_q_code)) %>%

    left_join( actor_translator_icews %>% filter(is.na(ICEWS_Actor.Name)) %>% dplyr::select(country=ICEWS_Country,  country_q_code=QCode) %>% distinct() %>% na.omit() ) %>%
    mutate(year = event_date %>% lubridate::year()  )  %>%
    mutate(month = event_date %>% lubridate::month()  )  %>%
    mutate(day = event_date %>% lubridate::day()  )  %>%
    mutate(month_label = lubridate::month(month %>% as.numeric(),label=TRUE,abbr=F)) %>%
    mutate(source_sectors = source_sectors %>% tolower()) %>%
    mutate(target_sectors = target_sectors %>% tolower()) %>%
    mutate(cameo_label = cameo_label %>% str_replace(", not specified below","") ) %>%
    mutate(who_a = coalesce(source_name,source_sectors %>% tolower(), source_label ) ) %>%
    mutate(who_b = coalesce(target_name,target_sectors %>% tolower(), target_label ) ) %>%
    mutate(who_a = ifelse(who_a %in% source_country, NA, who_a)) %>%
    mutate(who_b = ifelse(who_b %in% target_country, NA, who_b)) %>%
    unite(text,
          source_country,who_a,
          cameo_label,
          who_b, target_country, sep=" ", remove = F, na.rm = T) %>%
    mutate(text = ifelse(!is.na(where),paste0(text, " in ", where),text) ) %>%
    mutate(text = paste0("On ", month_label, " ",day," ",year, ", ", text)) %>%
    mutate(date=paste(month, day, year, sep="/")  %>% lubridate::mdy() )   %>%
    unite('dyad_a', source_name ,  na.rm = TRUE, remove = FALSE) %>%
    unite('dyad_b', target_name ,  na.rm = TRUE, remove = FALSE) %>%
    mutate(dyad=paste0(dyad_a,"->",dyad_b)) %>%
    mutate(dyad_event=paste0(dyad, "_", cameo_label)) %>%
    mutate(dyad_penta_category=paste0(dyad, "_", cameo_penta_category)) %>%
    mutate(dyad_lv10=paste0(dyad, "_", cameo_lvl0_label))

  dim(icews_clean) #17,541,701 #1,965,573 it's still too many but it's just close enough now #1,965,573
  icews_clean %>% arrow::write_parquet(here::here("git_ignore_folder", "icews_clean.parquet") )


}


library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

icews_barmaps <- function(thiscrisis=471, topactor=F){

  print(thiscrisis)
  plot_title <- crisno_name_years %>% filter(crisno==thiscrisis) %>% mutate(title="Crisis #" %>% paste0(crisno, " ", crisname, " (", yrtrig, "-",yrterm,")")) %>% pull(title)
  save_file_lv0 <-  paste0(here::here(), "//images/ggplots/p_icews_plot_",thiscrisis,".Rds")

  date_start <- lubridate::as_date(paste0(icb_crises[icb_crises$crisno==thiscrisis,]$yrtrig , "-", icb_crises[icb_crises$crisno==thiscrisis,]$motrig, "-01"))
  date_end <- lubridate::as_date(paste0(icb_crises[icb_crises$crisno==thiscrisis,]$yrterm , "-", icb_crises[icb_crises$crisno==thiscrisis,]$moterm, "-01"))
  date_end <- date_end %>% lubridate::ceiling_date() %>% lubridate::as_date()

  year_start <- icb_crises[icb_crises$crisno==thiscrisis,]$yrtrig
  year_end   <- icb_crises[icb_crises$crisno==thiscrisis,]$yrterm

  start_simple <- icb_crises[icb_crises$crisno==thiscrisis,]$start_simple
  end_simple <- icb_crises[icb_crises$crisno==thiscrisis,]$end_simple

  actors <- icb_events_long %>% filter(crisno %in% thiscrisis) %>% filter(varname_normalized %>% str_detect('actor')) %>%
    dplyr::select(value_normalized,value_qcode) %>% distinct()
  #library(re2r) #didn't speed it up and enabling parallel was actually slower, not sure what's going on usually very fast
  library(tictoc)
  tictoc::tic()
  icews_clean_subset <- icews_clean_small %>%
    filter(date>=(date_start-lubridate::years(1)) & date<=(date_end+lubridate::years(1))) %>%
    filter(text %>% str_detect(actors$value_normalized %>% paste0(collapse="|")))  #this is slow and maybe replacing with re2r will fix
  tictoc::toc() #85.212
  dim(icews_clean_subset) #441,588 #586,406
  if(nrow(icews_clean_subset)==0){return()}

  #lv0
  dyad_event_counts <-     icews_clean_subset  %>%
                          filter(date>=((date_start %>% as.Date())-lubridate::days(14)) & date<=((date_end %>% as.Date() )+lubridate::days(14)  )) %>%
                          count(cameo_lvl0_label, dyad_lv10) %>%
                          group_by(cameo_lvl0_label) %>%
                          arrange(cameo_lvl0_label,  desc(n) ) %>%
                          filter(row_number()<=10) %>% ungroup()

  names_to_qcodes <- actor_translator %>% dplyr::select(QCode, contains("name")) %>%
    pivot_longer(-c(QCode)) %>% na.omit() %>% dplyr::select(-name) %>% distinct() %>%
    mutate(markdown = glue::glue("![]({flag_folder_small}/{QCode}.png)") )
  names_to_qcodes <- bind_rows(names_to_qcodes, names_to_qcodes %>% mutate(value= value %>% tolower() )) %>% distinct() %>%
    arrange(desc(nchar(value))) #sort by length so you never accidentally replace a subset

  icews_clean_subset_top10 <- icews_clean_subset %>%
    filter(dyad_lv10 %in% dyad_event_counts$dyad_lv10  ) %>%
    mutate(dyad_markdown=dyad)

  dyad_unique <-     bind_rows(
    icews_clean_subset_top10 %>% dplyr::select(dyad_unique=dyad_a) %>% distinct(),
    icews_clean_subset_top10 %>% dplyr::select(dyad_unique=dyad_b) %>% distinct()
  ) %>% distinct() %>% mutate(dyad_unique_markup = dyad_unique)

  for(i in 1:nrow(names_to_qcodes)){
    #print(i)
    dyad_unique <- dyad_unique %>% dplyr::mutate(dyad_unique_markup = dyad_unique_markup %>% str_replace_all(names_to_qcodes$value[i], names_to_qcodes$markdown[i]  ))
    #print(head(dyad_unique,2))
  }

  icews_clean_subset_top10_day <- icews_clean_subset_top10 %>%
    left_join(dyad_unique %>% dplyr::select(dyad_a=dyad_unique, dyad_a_markup=dyad_unique_markup) ) %>%
    left_join(dyad_unique %>% dplyr::select(dyad_b=dyad_unique, dyad_b_markup=dyad_unique_markup) ) %>%
    mutate(dyad_markdown=paste0(dyad_a_markup,"->",dyad_b_markup)) %>%
    filter(!is.na(cameo_lvl0_label)) %>%
    group_by(dyad_markdown, cameo_lvl0_label, date) %>%
    summarise(mean_goldstein=mean(cameo_goldstein), n=n())

  library(ggtext)
  icews_clean_subset_top10_day <- icews_clean_subset_top10_day %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("government|Government","gov")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("law enforcement agencies","law enforcement")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("(tertiary role code)","")) %>%
    mutate(dyad_markdown = dyad_markdown %>% str_replace_all("International Committee of the Red Cross","Red Cross"))

  p_icews_lv0 <-  icews_clean_subset_top10_day %>%
    group_by(cameo_lvl0_label, dyad_markdown) %>% mutate(n_all=sum(n, na.rm=T)) %>% ungroup() %>%
    mutate(dyad_markdown = tidytext::reorder_within(dyad_markdown, n_all, within = cameo_lvl0_label)) %>%
    ggplot( aes(x = date, y = dyad_markdown, fill = mean_goldstein)) +
    geom_tile() + theme_bw() +
    tidytext::scale_y_reordered() +
    geomtextpath::geom_textvline(xintercept = as.Date(date_start), label="Crisis Start (ICB)" , linetype="dotted", color = "black", size=3) +
    geomtextpath::geom_textvline(xintercept = as.Date(date_end), label="Crisis End (ICB)" , linetype="dotted", color = "black", size=3) +
    facet_wrap(~cameo_lvl0_label, scale="free_y", ncol=1)  +
    theme(plot.margin = unit(c(0,0,0,0), "cm")) +
    theme( axis.text.y = element_markdown(size = 12, lineheight = 0.1)  ) +
    scale_fill_gradient2() +
    theme(legend.position="none") +
    ylab("") + xlab("")


  p_icews_lv0 %>% saveRDS(save_file_lv0)


}

icews_clean_small <-  arrow::read_parquet( here::here("git_ignore_folder", "icews_clean.parquet"),
                                             col_select=c('date','dyad','cameo_lvl0_label','dyad_lv10','text','cameo_goldstein','dyad_a','dyad_b') )
dim(icews_clean_small) #18403260

for(crisis in  icb_crises %>% filter(yrtrig>=1995) %>% pull(crisno) %>% unique()){
  print(crisis)
  tictoc::tic()
  icews_barmaps(crisis)
  tictoc::toc()
}




