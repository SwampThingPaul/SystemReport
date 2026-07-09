## scripts/01_fetch_data.R
## Stage 1 of the pipeline: download everything, write raw data + a manifest.
## Run: Rscript scripts/01_fetch_data.R
##
## Outputs (all under data/raw/):
##   Qdat.rds            structure discharge (long, DBKEY attached)
##   stg_dat.rds         stage (long)
##   sal_dat.rds         DBHYDRO salinity (long)
##   sle_sal_usgs.rds    USGS SLE salinity
##   flab_crk.rds        USGS coastal creek discharge
##   fetch_manifest.csv  one row per DBKEY/site: status, n_rows, max_date, message
##
## REPORT.Rmd then does *no* network I/O: it just readRDS()s these files.
## The stage/discharge site tables below are ported unchanged from the Rmd —
## when a DBKEY changes, this is now the only file to edit.

suppressPackageStartupMessages({
  library(AnalystHelper)
  library(dataRetrieval)
})
source("src/report_helpers.R")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# Dates ------------------------------------------------------------------------
CurWY      <- WY(date.fun(Sys.time()))
Start.Date <- date.fun(paste(CurWY - 4, "05-01", sep = "-"))
End.Date   <- date.fun(Sys.Date() - 1)
YEST       <- End.Date
dates      <- c(Start.Date, End.Date)
flab.dates <- c(date.fun(paste(CurWY - 15, "05-01", sep = "-")), End.Date)
LOK.sdate  <- date.fun("2007-05-01")

manifests <- list()


## 1. Structure discharge (DBHYDRO Insights) -------------------------------
# --- site/dbkey table: ported verbatim from REPORT.Rmd 'Structure Q data' ---
Qdbkeys <- data.frame(loc = c("FECSR70","S84","S84X","S71","S72","S65E","S65EX1",
                              "S77","S79","S354","S351","S352","S271",
                              "GORDY","S49","S48","S80","S308",
                              "S127","S129","S131","S133","S135","S4",
                              "S44","S155","S41","S155A",
                              "S12A","S12B","S12C","S12D","S333N","S333","S334","S335","S355A","S355B","S355B","S356",
                              "S332D","S332DX1","S328","G737","S200","S332","S175",
                              "G300","G301","G251","G310","S362",
                              "G338","G782","G339","G335","G436","S7","S150","S8","G407",
                              "G393A","G393B","G393C","G354A","G354B","G354C","G352A",
                              "G352B","G352C","G344A","G344B","G344C","G344D","G344E",
                              "G344F","G344G","G344H","G344I","G344J",
                              "S77","S78","S78","S79"),
                      DA = c("88210","91687","91686","91668","91675","91656","AL760",
                             "15635","00865","91513","91508","91510","65409",
                             "AS188","91607","91606","DJ238","DJ239",
                             "91370","91373","91376","15637","91379","91608",
                             "91602","91404","91601","91403",
                             "01313","00610","00621","01310","40371","15042","FB752","91489","MQ895","AM173","MQ896","64136",
                             "91485","91484","AN558","AN674","91437","15753","91421",
                             "90939",
                             "90940","90934","90973","91517","91012","AS397","91013",
                             "91008","91209","91681","91395","91689","91192","91179",
                             "91180","91181","91081","91082","91083","91075","91076",
                             "91077","91051","91052","91053","91054","91055","91056",
                             "91057","91058","91059","91060",
                             "DJ235","00857","DJ236","DJ237"),
                      DA_P = c(rep("P1",80),"P2","P1","P2","P2"),
                      BK = c(NA,"90817","90816","90803","90805","90791","AL761",
                             NA,NA,"95111","65106","65108","65408",
                             rep(NA,5),
                             "64827","64830","64833","64834","64836","90743",
                             rep(NA,16),rep(NA,44)))
Qdbkeys[Qdbkeys$loc=="S271","DA"] <- "02855"
Qdbkeys[Qdbkeys$loc=="S333","DA"] <- "91487"
Qdbkeys[Qdbkeys$loc=="S334","DA"] <- "91488"
Qdbkeys <- subset(Qdbkeys, !(loc %in% c("S127","S129","S131","S133","S135","S4")))
Qdbkeys$EffDate <- NA; Qdbkeys$InaDate <- NA
Qdbkeys <- Qdbkeys[Qdbkeys$loc != "S352", ]
Qdbkeys <- Qdbkeys[Qdbkeys$loc != "G338", ]

# EAA
Qdbkeys2 <- data.frame(loc = c("S352", "S2", "S3", "S5AS5AW", "S6", 
                               "G136", "G250", "G600", "G328", "G370", "G372", "G371", "G373", 
                               "G434", "G435", "G722", "C10", "C12A", "C12", "C4A", "S236", 
                               "EPD07", "PC15MD", "G302", "G311", "S319", "S361", "G328I", "G338", 
                               "G342A", "G342B", "G342C", "G342D", "G342E", "G342F", "G353A", 
                               "G353B", "G353C", "G601", "G602", "G603", "G508", "G396A", "G396B", 
                               "G396C","S2_h","S2_n","S5A","S5AW"),
                       DA = c("15068", "15021", "15018", "15031", "15034", 
                              "15195", "16222", "GG955", "J0718", "TA438", "TA437", "TS261", 
                              "TS260", "90327", "90328", "AM015", "15645", "15647", "15646", 
                              "15648", "15644", "AM706", "66021", "90941", "90974", "91476", 
                              "91516", "90978", "91012", "91016", "91017", "91018", "91019", 
                              "91020", "91021", "91078", "91079", "91080", "91247", "91248", 
                              "91249", "91231", "91183", "91184", "91185","00351","00436","91623","91621"),
                       DA_P = "P1",
                       BK = NA,
                       EffDate = c("1978-10-01", 
                                   "1978-10-01", "1978-10-01", "1978-10-01", "1978-10-01", "1978-10-01", 
                                   "1994-01-25", "1997-03-06", "2000-04-01", "2003-10-01", "2003-10-01", 
                                   "2006-02-01", "2006-02-15", "2012-11-01", "2013-05-17", "2015-08-28", 
                                   "2018-05-01", "2018-05-01", "2018-05-01", "2018-05-01", "2018-05-01", 
                                   "2018-05-01", "2021-04-01", "1999-07-01", "2005-10-01", "2004-10-01", 
                                   "2004-10-01", "2005-03-01", "2002-03-01", "1999-06-23", "1998-11-21", 
                                   "1999-05-19", "1998-11-20", "2008-04-23", "2008-03-10", "2008-05-01", 
                                   "2008-05-01", "2008-05-01", "2007-05-01", "2007-05-01", "2007-05-01", 
                                   "2012-12-01", "2008-05-01", "2008-05-01", "2008-05-01","1957-03-22","1957-03-22","1978-10-01","1978-10-01"), 
                       InaDate = c(NA, NA, NA, NA, NA, NA, "1999-07-10", "2005-04-30", NA, NA, NA, NA, 
                                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                   NA, NA, NA, NA, NA, "2012-04-30", "2012-04-30", "2012-04-30", 
                                   "2012-04-30", "2012-04-30", "2008-04-30", "2008-04-30", "2008-04-30", 
                                   NA, "2012-04-30", "2012-04-30", "2012-04-30",NA,NA,NA,NA))
Qdbkeys2[Qdbkeys2$DA==15068,];# HGS5X "PREF" DBKEY
Qdbkeys2[Qdbkeys2$DA==15068,"DA"] <- "91510"

Qdbkeys2[Qdbkeys2$loc=="S6","DA"] <- "00357" # 15034 is PREF
Qdbkeys2[Qdbkeys2$loc=="S3","DA"] <- "91599" # 15018 is PREF 
Qdbkeys2[Qdbkeys2$loc=="G328","DA"] <- "90979" # J0718 is PREF 
Qdbkeys2[Qdbkeys2$loc=="G370","DA"] <- "91094" # TA438 is PREF 
Qdbkeys2[Qdbkeys2$loc=="G372","DA"] <- "91105" # TA437 is PREF 
Qdbkeys2[Qdbkeys2$loc=="G371","DA"] <- "91095" # TS261 is PREF 
Qdbkeys2[Qdbkeys2$loc=="G373","DA"] <- "91106" # TS260 is PREF 
Qdbkeys2[Qdbkeys2$loc=="G434","DA"] <- "91202" # 90327 is PREF 
Qdbkeys2[Qdbkeys2$loc=="G435","DA"] <- "91208" # 90328 is PREF 
Qdbkeys2[Qdbkeys2$loc=="G722","DA"] <- "64174" # AM015 is PREF 

Qdbkeys3 <- data.frame(loc = c("S10A","S10C","S10D","G94A","G94B","G94C","S39","G338",
                               "S11A","S11B","S11C","S143","S144","S145","S146",
                               "G404","S140_P","S140_S","S190",
                               "S332D","S332DX1","S328","G737","S200","S332","S175","S18C",
                               "S344","S343A","S343B","S31","S337","S9","S9A"),
                       DA = c("15261","15262","15263","91281","91282","91283","91598","91012",
                              "15258","15259","15260","91388","92215","92216","91391",
                              "91190","91384","91385","91428",
                              "91485","91484",'AN558',"AN674","91437","91486","15752","91427",
                              "91504","91502","91503","91477","91491","91695","91692"),
                       DA_P = "P1",
                       BK =NA,
                       EffDate = NA,
                       InaDate = NA)

Qdbkeys3 <- rbind(Qdbkeys3,
                  data.frame( loc ="S38",DA = "91594", DA_P = "P1",
                              BK = "90729",EffDate = NA, InaDate =NA))

Qdbkeys <- rbind(Qdbkeys, Qdbkeys2, Qdbkeys3)
Qdbkeys$EffDate <- date.fun(Qdbkeys$EffDate)
Qdbkeys$InaDate <- date.fun(Qdbkeys$InaDate)
Qdbkeys <- Qdbkeys[!duplicated(Qdbkeys), ]
saveRDS(Qdbkeys, "data/raw/qdbkeys.rds")

## long-term sites (need the 15-yr record)
redline_locs <- c("G300","G301","G251","G310","S362","G338","G782",
                  "G339","G335","G436","S7",
                  "S150","S8","L28U","G407","G393A","G393B","G393C",
                  "G354A","G354B","G354C","G352A","G352B","G352C",
                  paste0("G344", LETTERS[1:10]))
ENP_SRS_locs <- c("S12A","S12B","S12C","S12D","S333N","S333","S334","S335",
                  "S355A","S355B","S356")
LT_locs <- c(redline_locs, ENP_SRS_locs)

message("Fetching structure discharge (", nrow(Qdbkeys), " DBKEYs)...")
q_res <- fetch_many(
  keys      = Qdbkeys,
  fetch_fun = function(sdate, edate, dbkey) insight_fetch_daily(sdate, edate, dbkey),
  sdate_fun = function(i) if (Qdbkeys$loc[i] %in% LT_locs) flab.dates[1] else dates[1],
  edate     = dates[2],
  key_col   = "DA"
)
saveRDS(q_res$data, "data/raw/Qdat.rds")
manifests$Q <- transform(q_res$manifest, group = "discharge",source = "DBHYDRO")

## 2. Stage (DBHYDRO Insights) ---------------------------------------------
stg_dbkeys_list <- list(
  LOK  = data.frame(loc = "LOK", DA = c("06832","N3466","94832"), BK = NA,
                    Priority = c("P3","P2","P1"),
                    datum = c("NGVD29","NGVD29","NAVD88"), datum_conv = 1.25),
  EAA  = data.frame(loc = c("S2_H","S3_H","S352","L8.441"),
                    DA = c("06562","06633","FF580","02854"), BK = NA, Priority = NA,
                    datum = "NGVD29", datum_conv = NA),
  WCA1 = data.frame(loc = c("CA1-8T","CA1-9","CA1-7"),
                    DA = c("15809","15811","15808"), BK = NA, Priority = NA,
                    datum = "NGVD29", datum_conv = NA),
  WCA2 = data.frame(loc = c("S11B_H","CA217"), DA = c("WN126","16531"),
                    BK = NA, Priority = NA, datum = "NGVD29", datum_conv = NA),
  WCA3 = data.frame(loc = c("CA3-62","CA3-63","CA3-64","CA3-65"),
                    DA = c("16536","16532","16537","16538"), BK = NA, Priority = NA,
                    datum = "NGVD29", datum_conv = NA),
  ENP  = data.frame(loc = c("NESRS2","NP201","S334_HW","S333N_TW","S333_TW"),
                    DA = c("01218","06719","DJ184","40367","AJ015"),
                    BK = c(rep("NA",2),"IY636","40368","AJ016"), Priority = NA,
                    datum = "NGVD29", datum_conv = NA)
)
stg_dbkeys <- do.call(rbind.data.frame, c(stg_dbkeys_list, make.row.names = FALSE))

message("Fetching stage (", nrow(stg_dbkeys), " DBKEYs)...")
## Stage is fetched per-row (not via fetch_many) because `datum` varies by row.
stg_rows <- lapply(seq_len(nrow(stg_dbkeys)), function(i) {
  fetch_daily_safe(
    function(s, e, k) insight_fetch_daily(s, e, k, datum = stg_dbkeys$datum[i]),
    sdate = if (stg_dbkeys$loc[i] == "LOK") LOK.sdate else
            date.fun(paste((CurWY - 3) - 1, "05-01", sep = "-")),
    edate = dates[2],
    dbkey = stg_dbkeys$DA[i]
  )
})
stg_manifest <- cbind(stg_dbkeys,
  status   = vapply(stg_rows, `[[`, character(1), "status"),
  n_rows   = vapply(stg_rows, `[[`, integer(1),  "n"),
  max_date = vapply(stg_rows, `[[`, character(1), "max_date"),
  message  = vapply(stg_rows, `[[`, character(1), "message"))
stg_dat <- do.call(rbind, Map(function(r, k) {
  d <- r$data; if (is.data.frame(d) && nrow(d)) { d$DBKEY <- k; d } else NULL
}, stg_rows, stg_dbkeys$DA))
if (!is.null(stg_dat)) {
  stg_dat <- merge(stg_dat, stg_dbkeys[, c("loc","DA","Priority","datum_conv")],
                   by.x = "DBKEY", by.y = "DA", all.x = TRUE)
  stg_dat$Data.Value.corr <- with(stg_dat,
    ifelse(!is.na(datum_conv) & units == "ft NAVD88",
           Data.Value + datum_conv, Data.Value))
}
saveRDS(stg_dat, "data/raw/stg_dat.rds")
manifests$stage <- transform(stg_manifest, group = "stage",source = "DBHYDRO")

## 3. Salinity (DBHYDRO) ---------------------------------------------------
# changed site to loc 
# changed DBKEY to DA (for daily DBKEY)

sal_dbkeys_ls <- list()
sal_dbkeys_ls[["FLAB"]] <- data.frame(loc=paste0("ENP",c("TB","GB","BK","MK","JK","WB",
                                                          "LM","JB","LS","TC","BN","BS","DK",
                                                          "BA","LR","PK",
                                                          "WP","LO","BR")),
                                      param = "SAL",
                                      depth="bottom",
                                      DA=c("63683","63617","63578","63667","63634","63695",
                                              "63642","AN690","63663","63687","63581","63588","63608",
                                              "63572","63654","63675",
                                              "63703","63650","63584"),
                                      region = c(rep("FLAB",16),rep("WEST",3)))

sal_dbkeys_ls[["CRE"]] <- data.frame(loc=c(rep("VALI75",2),rep("FORTMYERSM",2),rep("CCORAL",2),rep("MARKH",2),rep("SANIB2",2)),
                                     param = rep(c("WT","SPC"),5),
                                     depth="bottom",
                                     DA=c("UL030","UL026","88288","88291","UO832","AJ012","WZ152","WZ156","WN375","WN377"),
                                     region = "CRE")

sal_dbkeys_ls[["SLE"]] <- data.frame(loc=c(rep("HR1",2)),
                                     param = rep(c("WT","SPC"),1),
                                     depth="bottom",
                                     DA=c("IX674","IX679"),
                                     region = "SLE")
sal_dbkeys_ls[["LWL"]] <- data.frame(loc=c(rep("LWL19",2),rep("LWL20A",2),rep("LWL20",2)),
                                     param = rep(c("WT","SPC"),3),
                                     depth="bottom",
                                     DA=c("39343","39347","39450","39452","WZ749","WZ753"),
                                     region = "LWL")

sal_dbkeys <- do.call(rbind,sal_dbkeys_ls)
rownames(sal_dbkeys) <- NULL

message("Fetching salinity (and parameters) (", nrow(sal_dbkeys), " DBKEYs)...")
sal_res <- fetch_many(
  keys      = sal_dbkeys,
  fetch_fun = function(sdate, edate, dbkey) insight_fetch_daily(sdate, edate, dbkey),
  sdate_fun = function(i) if (sal_dbkeys$region[i]=="FLAB") flab.dates[1] else Start.Date,
  edate     = YEST,
  key_col   = "DA"
)
# saveRDS(sal_res$data, "data/raw/sal_dat.rds")
# manifests$sal <- transform(sal_res$manifest, group = "salinity")


## 4. USGS SLE Salinity ----------------------------------------------------
usgs_fetch_daily <- function(sdate, edate, site_id, pcodes) {
  tmp <- read_waterdata_daily(
    monitoring_location_id = site_id,
    parameter_code         = pcodes,
    time                   = c(as.Date(sdate), as.Date(edate)),
    skipGeometry           = TRUE
  )
  if (is.data.frame(tmp) && nrow(tmp) > 0) {
    tmp$Date       <- date.fun(tmp$time)   # manifest + downstream expect Date
    tmp$Data.Value <- tmp$value            # harmonize with DBHYDRO naming
  }
  tmp
}

sle_usgs_keys <- data.frame(
  loc   = c("STL_RIVER", "STL_STPT"),
  DA  = paste0("USGS-", c("02277100", "02277110")),
  param  = NA,          # long data carries multiple params per site
  depth  = "bottom",
  region = "SLE"
)
pCode2     <- c("00010", "00095", "00480")            # WT, SPC, SAL
pCode2_def <- data.frame(parameter_code = pCode2,
                         param = c("WT", "SPC", "SAL"))

message("Fetching salinity (USGS) (", nrow(sle_usgs_keys), " DBKEYs)...")
sle_sal_res <- fetch_many(
  keys      = sle_usgs_keys,
  fetch_fun = usgs_fetch_daily,
  sdate_fun = function(i) Start.Date,
  edate     = YEST,
  key_col   = "DA",
  pcodes    = pCode2          # extra args pass straight through `...`
)

## Harmonize both sources to one minimal long schema and save a single file.
sal_cols  <- c("loc", "region", "param", "depth", "Date", "Data.Value",
               "DA", "source")
sal_parts <- list()

if (!is.null(sal_res$data)) {
  tmp <- merge(sal_res$data, sal_dbkeys,
               by.x = "DBKEY",by.y = "DA")
  names(tmp)[names(tmp)=="DBKEY"]<-"DA"
  tmp$source <- "DBHYDRO"
  sal_parts$dbhydro <- tmp[, sal_cols]
}

if (!is.null(sle_sal_res$data)) {
  tmp <- merge(sle_sal_res$data, pCode2_def, "parameter_code")   # adds param
  tmp <- merge(tmp, sle_usgs_keys[, c("DA", "loc", "depth", "region")],
               by.x = "monitoring_location_id",by.y = "DA")
  names(tmp)[names(tmp)=="monitoring_location_id"]<-"DA"
  tmp$source <- "USGS"
  sal_parts$usgs <- tmp[, sal_cols]
}

sal_dat <- do.call(rbind, sal_parts)     # NULL-safe: skips failed sources
if (!is.null(sal_dat)) saveRDS(sal_dat, "data/raw/sal_dat.rds")

manifests$sal      <- transform(sal_res$manifest,     group = "salinity",
                                source = "DBHYDRO")
manifests$sal_usgs <- transform(sle_sal_res$manifest, group = "salinity",
                                source = "USGS")


##  5. USGS coastal creek discharge (Florida Bay + west coast) -----------------
usgs_west <- data.frame(site_no = c("02290888", "02290918", "02290878"),
                        loc     = c("Chatham", "Lost", "Broad"),
                        loc_abb = c("ENPWP", "ENPLO", "ENPBR"))

usgs_sites <- data.frame(
  site_no = c("251241080385300",   # Taylor River (upstream)
              "251127080382100",   # Taylor River (mouth)
              "251355080312800",   # Joe Bay
              "251253080320100",   # Trout Creek
              "251209080350100",   # Mud Creek
              "251003080435500",   # McCormick
              "251433080265000",   # West Highway
              "251032080473400"),  # Alligator Creek
  loc     = c("Taylor River (upstream)", "Taylor River (mouth)", "Joe Bay",
              "Trout", "Mud", "McCormick", "W Hwy", "Alligator"),
  loc_abb = c("TRUP", "TRM", "JB", "TC", "MUD", "MCC", "HWY", "ALLI")
)
usgs_sites <- rbind(usgs_sites, usgs_west)
usgs_sites <- usgs_sites[usgs_sites$loc != "Joe Bay", ]
usgs_sites$DA <- paste0("USGS-", usgs_sites$site_no)

pCode     <- c("00060", "72137")   # Q, tidally filtered Q
pCode_def <- data.frame(parameter_code = pCode,
                        param = c("Q", "Q_tidefilt"))

creek_locs <- tryCatch(
  read_waterdata_monitoring_location(usgs_sites$DA) |>
    merge(usgs_sites, by.x = "monitoring_location_id",by.y = "DA"),
  error = function(e) {
    message("Creek site metadata fetch failed: ", conditionMessage(e))
    NULL
  }
)
if (!is.null(creek_locs)) {
  saveRDS(creek_locs, "data/raw/usgs_creek_locs.rds")
} else if (!file.exists("data/raw/usgs_creek_locs.rds")) {
  warning("No creek site metadata available (fetch failed, no cached copy).")
}

message("Fetching coastal creek discharge (", nrow(usgs_sites), " sites)...")
crk_res <- fetch_many(
  keys      = usgs_sites,
  fetch_fun = usgs_fetch_daily,
  sdate_fun = function(i) flab.dates[1],   # 15-yr record for percentiles
  edate     = dates[2],
  key_col   = "DA",
  pcodes    = pCode
)

if (!is.null(crk_res$data)) {
  crk_dat <- merge(crk_res$data, pCode_def, "parameter_code") |>
    merge(usgs_sites, by.x = "monitoring_location_id",by.y = "DA", all.x = TRUE)
  names(crk_dat)[names(crk_dat)=="monitoring_location_id"]<-"DA"
  saveRDS(crk_dat, "data/raw/flab_crk.rds")
}
manifests$creeks <- transform(crk_res$manifest, group = "creek_Q",source = "USGS")


# Write manifest & summarize --------------------------------------------
manifest <- do.call(rbind, lapply(manifests, function(m) {
  m[, intersect(c("group","loc","DA","DBKEY","status","n_rows","max_date","message"),
                names(m)), drop = FALSE]
}))
rownames(manifest) <- NULL
write.csv(manifest, "data/raw/fetch_manifest.csv", row.names = FALSE)

n_fail <- sum(manifest$status != "ok")
message(sprintf("Fetch complete: %d ok, %d failed. Manifest: data/raw/fetch_manifest.csv",
                sum(manifest$status == "ok"), n_fail))
if (n_fail > 0) {
  print(manifest[manifest$status != "ok",
                 intersect(c("group","loc","DA","DBKEY","message"), names(manifest))])
}


