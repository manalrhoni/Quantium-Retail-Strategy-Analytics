# -------------------------------------------------------------------
# TASK 2: START FROM SCRATCH
# -------------------------------------------------------------------

# 1. Install libraries (ila ma-kanouch)
if (!require("data.table")) install.packages("data.table")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")

library(data.table)
library(ggplot2)
library(tidyr)

# 2. Chargiw l-Data (QVI_data.csv li sifti)
# Hna ghanst3mlo file.choose() bach t-khtari l-fichier b yddk w thannay
print("3afak khtari l-fichier 'QVI_data.csv'...")
data <- fread(file.choose())

# 3. Nqado t-Tarikh (Date) w nzidou 'YEARMONTH'
# Masalan: 2018-10-17 --> 201810
data[, DATE := as.Date(DATE)]
data[, YEARMONTH := as.numeric(format(DATE, "%Y%m"))]

# 4. Nchoufo wach tzad l-colonne
print("---- Data Check (Top rows) ----")
head(data)

# -------------------------------------------------------------------
# MARHALA 2: Tjmya3 l-Data b Chhoura (Monthly Aggregation)
# -------------------------------------------------------------------

# Bach nqarNo l-magazat, khassna n3rfo 'Adae' (Performance) dyalhom f kola chhar
measureOverTime <- data[, .(
  totSales = sum(TOT_SALES),             # 1. Chhal dakhel dyal l-flous
  nCustomers = uniqueN(LYLTY_CARD_NBR),  # 2. Chhal mn client ja
  nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR), # 3. Chhal mn merra kaychri l-wahed
  nChipsPerTxn = sum(PROD_QTY)/uniqueN(TXN_ID),          # 4. Chhal mn bakiya f kola chriya
  avgPricePerUnit = sum(TOT_SALES)/sum(PROD_QTY)         # 5. Taman l-moyenne
), by = .(STORE_NBR, YEARMONTH)]

# Nrttbohom
setorder(measureOverTime, STORE_NBR, YEARMONTH)

# 5. N-Filtréw: Nkhlliw ghir Stores li 3ndhom 12 chhar kamla dyal Data
# Hit ma-ymknch nqarNo magaza khdmat 3am m3a whda yallah bdAt
storeCounts <- measureOverTime[, .N, by = STORE_NBR]
storesWithFullObs <- storeCounts[N == 12, STORE_NBR]

measureOverTime <- measureOverTime[STORE_NBR %in% storesWithFullObs]

print("---- Monthly Metrics (Top 6 rows) ----")
head(measureOverTime)


# -------------------------------------------------------------------
# MARHALA 3: Hisab Correlation (Chkun li kaychbh lina?)
# -------------------------------------------------------------------

# 1. Hada Code bach nsawbo Fonction kat7sb Correlation
# (Ghir diruliha Run, ma-tbdlou fiha walo)
calculateCorrelation <- function(baseTable, metricCol, storeComparison) {
  outputDataFrame <- data.table()
  
  # Nkhdmo ghir b magazat li 3ndhom data kamla
  stores <- unique(baseTable$STORE_NBR)
  
  for (i in stores) {
    # Njibo data dyal Trial Store (li baghin nqarNo bih)
    calcMeasure <- baseTable[STORE_NBR == storeComparison, eval(metricCol)]
    # Njibo data dyal Store 'i' (li baghin ntestiwh)
    compMeasure <- baseTable[STORE_NBR == i, eval(metricCol)]
    
    # N7sbo Correlation
    calculatedMeasure <- data.table(Store1 = storeComparison, 
                                    Store2 = i, 
                                    corr_measure = cor(calcMeasure, compMeasure))
    
    outputDataFrame <- rbind(outputDataFrame, calculatedMeasure)
  }
  return(outputDataFrame)
}

# 2. Daba n-appliqiwha 3la Stores dyalna (77, 86, 88)
# N7sbo Correlation d SALES
corr_nSales <- rbind(
  calculateCorrelation(measureOverTime, quote(totSales), 77),
  calculateCorrelation(measureOverTime, quote(totSales), 86),
  calculateCorrelation(measureOverTime, quote(totSales), 88)
)

# N7sbo Correlation d CUSTOMERS
corr_nCustomers <- rbind(
  calculateCorrelation(measureOverTime, quote(nCustomers), 77),
  calculateCorrelation(measureOverTime, quote(nCustomers), 86),
  calculateCorrelation(measureOverTime, quote(nCustomers), 88)
)

# 3. Nchoufo Natija (Top Correlations)
print("---- Top Correlations (Sales) ----")
# Nchoufo ghir li 3ndhom correlation tal3a (qrib l 1)
head(corr_nSales[order(-corr_measure)])


# -------------------------------------------------------------------
# MARHALA 4: Ikhtiyar Best Control Store (Correlation + Magnitude)
# -------------------------------------------------------------------

# 1. Fonction bach n7sbo Magnitude (Chhal l-farq f l-mabi3at)
calculateMagnitudeDistance <- function(baseTable, metricCol, storeComparison) {
  outputDataFrame <- data.table()
  stores <- unique(baseTable$STORE_NBR)
  
  for (i in stores) {
    calcMeasure <- baseTable[STORE_NBR == storeComparison, eval(metricCol)]
    compMeasure <- baseTable[STORE_NBR == i, eval(metricCol)]
    
    # N7sbo l-farq (Distance)
    dists <- abs(calcMeasure - compMeasure)
    # Normalisewh (nrddoh bin 0 w 1)
    distMeasure <- data.table(Store1 = storeComparison, 
                              Store2 = i, 
                              mag_measure = 1 - (mean(dists) - min(dists)) / (max(dists) - min(dists)))
    
    outputDataFrame <- rbind(outputDataFrame, distMeasure)
  }
  return(outputDataFrame)
}

# 2. N-appliqiwha 3la Sales
mag_nSales <- rbind(
  calculateMagnitudeDistance(measureOverTime, quote(totSales), 77),
  calculateMagnitudeDistance(measureOverTime, quote(totSales), 86),
  calculateMagnitudeDistance(measureOverTime, quote(totSales), 88)
)

# 3. Njm3o Scores bjuj: Correlation (0.5) + Magnitude (0.5)
# Hna ghanst3mlo ghir Sales bach n-simplifiyew l-omour (hit hwa l-mouhim)
total_score <- merge(corr_nSales, mag_nSales, by = c("Store1", "Store2"))
total_score[, combined_score := 0.5 * corr_measure + 0.5 * mag_measure]

# 4. Nchoufo Rabe7 l-kbir (The Winner) l kola Store
# Ghadi nakhdo Top 1 l kola Trial Store
control_stores <- total_score[Store1 != Store2][order(-combined_score)]
control_stores <- control_stores[!duplicated(Store1)] # Hada kaykhlli ghir l-lwwal

print("---- THE WINNERS (Control Stores) ----")
print(control_stores[, .(Trial_Store = Store1, Control_Store = Store2, Score = combined_score)])


# -------------------------------------------------------------------
# MARHALA 5: Visualization (Trial 77 vs Control 233)
# -------------------------------------------------------------------

# 1. N3zlo ghir Data dyal 77 w 233
plotData <- measureOverTime[STORE_NBR %in% c(77, 233)]

# 2. Nzidou colonne bach nfrrqo binathom f l-lwan (Trial vs Control)
plotData[, Store_Type := ifelse(STORE_NBR == 77, "Trial Store 77", "Control Store 233")]

# 3. Nrssmo Graph
ggplot(plotData, aes(x = YEARMONTH, y = totSales, color = Store_Type)) +
  geom_line(size = 1) +
  # Nzidou khet 3amoudi f bdayt t-tajriba (Feb 2019 = 201902)
  geom_vline(xintercept = 201902, linetype = "dashed", color = "red") +
  labs(title = "Total Sales: Trial Store 77 vs Control Store 233",
       subtitle = "L-khet l-7mar kaybayen bdayt t-tajriba",
       x = "Month", y = "Total Sales") +
  theme_bw() +
  scale_color_manual(values = c("Trial Store 77" = "blue", "Control Store 233" = "orange"))

# -------------------------------------------------------------------
# MARHALA 6: Visualization (Stores 86 & 88)
# -------------------------------------------------------------------

# 1. Graph dyal Store 86 vs Control 155
plotData86 <- measureOverTime[STORE_NBR %in% c(86, 155)]
plotData86[, Store_Type := ifelse(STORE_NBR == 86, "Trial Store 86", "Control Store 155")]

p1 <- ggplot(plotData86, aes(x = YEARMONTH, y = totSales, color = Store_Type)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 201902, linetype = "dashed", color = "red") +
  labs(title = "Store 86 vs Control 155", x = "Month", y = "Total Sales") +
  theme_bw() +
  scale_color_manual(values = c("Trial Store 86" = "blue", "Control Store 155" = "orange"))

# 2. Graph dyal Store 88 vs Control 237
plotData88 <- measureOverTime[STORE_NBR %in% c(88, 237)]
plotData88[, Store_Type := ifelse(STORE_NBR == 88, "Trial Store 88", "Control Store 237")]

p2 <- ggplot(plotData88, aes(x = YEARMONTH, y = totSales, color = Store_Type)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 201902, linetype = "dashed", color = "red") +
  labs(title = "Store 88 vs Control 237", x = "Month", y = "Total Sales") +
  theme_bw() +
  scale_color_manual(values = c("Trial Store 88" = "blue", "Control Store 237" = "orange"))

# 3. Afficher les Graphes
print(p1)
print(p2)