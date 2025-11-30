# ---------------------------------------------------------
# MARHALA 1: Installation dyal l-maktabat (Libraries)
# ---------------------------------------------------------
# Hado khasshom ydarou ghir merra wehda. Ila deja drtihom, tqdri t-passihom.
if (!require("data.table")) install.packages("data.table")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggmosaic")) install.packages("ggmosaic")
if (!require("readr")) install.packages("readr")
if (!require("readxl")) install.packages("readxl") # Hada darouri l Excel

# ---------------------------------------------------------
# MARHALA 2: Chargement dyal Libraries
# ---------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(readxl)

# ---------------------------------------------------------
# MARHALA 3: Jiban l-Data (Loading Data)
# ---------------------------------------------------------

# 1. Transaction Data (Excel)
# Ghadi t-tla3 lik fenetre, siri khtari l-fichier EXCEL "QVI_transaction_data.xlsx"
print("Please select the Transaction Data Excel file...")
transactionData <- read_excel(file.choose())

# Mohim: N-7wlouh l 'data.table' bach ykon bhal l-fichier lakhor
setDT(transactionData)

# 2. Customer Data (CSV)
# Ghadi t-tla3 fenetre tanya, khtari l-fichier CSV "QVI_purchase_behaviour.csv"
print("Please select the Customer Behavior CSV file...")
customerData <- fread(file.choose())

# ---------------------------------------------------------
# MARHALA 4: T2kkod (Checking Data)
# ---------------------------------------------------------

print("---- Transaction Data (Top 6 rows) ----")
head(transactionData)

print("---- Customer Data (Top 6 rows) ----")
head(customerData)

print("---- Structure dyal Transaction Data ----")
str(transactionData)

# ---------------------------------------------------------
# MARHALA 5: Data Cleaning & Summary
# ---------------------------------------------------------

# 1. Nssal7o l-Date
# Excel kay7sb l-iyam mn 1899-12-30, khassna nwrriw l R hadchi
transactionData$DATE <- as.Date(transactionData$DATE, origin = "1899-12-30")

# 2. Nchoufo l-Date wach tqadat
print("---- Date jdida (Top 6 rows) ----")
head(transactionData$DATE)

# 3. Nchoufo Summary (Khoulassa) dyal Transaction Data
# Hada ghadi y3tina l-Moyenne (Mean), Max, Min dyal kola colonne
print("---- Summary Statistics ----")
summary(transactionData)

# ---------------------------------------------------------
# MARHALA 6: Enquêter et Supprimer l'Outlier (L-moghrrb)
# ---------------------------------------------------------

# 1. Ajina nchoufo hada li chra 200 bakiya chkun hwa
print("---- L-Client l-Mochtabah fih (Outlier) ----")
outlier_transaction <- transactionData[PROD_QTY == 200, ]
print(outlier_transaction)

# 2. Nchoufo wach fayt lih chra chi haja okhra
# (Ghadi nst3mlo LYLTY_CARD_NBR dyalo li lqina l-foq)
print("---- Tarikh chira2at dyal had l-client ----")
transactionData[LYLTY_CARD_NBR == 226000, ]

# 3. Mlli t2kkdna annahou "Outlier", khassna n7yduh
# Nkhlliw ghir transaction li fiha QTY sgher mn 200
transactionData <- transactionData[PROD_QTY < 200, ]

# 4. N3awdo n-checkiw
print("---- Summary jdid (Max khasso ykon sgher daba) ----")
summary(transactionData$PROD_QTY)

# ---------------------------------------------------------
# MARHALA 7: Vérification des Dates
# ---------------------------------------------------------

# 1. N7sbo chhal mn nhar 'unique' 3ndna f l-data
print(paste("3adad l-ayam li kayna f l-Data:", uniqueN(transactionData$DATE)))

# 2. Nqado tableau sghir fih 'Nhar' w 'Chhal tba3 fih' (Total Transactions)
transactions_by_day <- transactionData[, .N, by = DATE]

# 3. Nkhlqo 'Silsila' (Sequence) dyal 3am kamel (mn 01-07-2018 tal 30-06-2019)
# Bach nqarNouha m3a l-data dyalna
all_dates <- data.table(DATE = seq(as.Date("2018-07-01"), as.Date("2019-06-30"), by = "day"))

# 4. Njam3ohom (Merge) bach yban lina nhar li naqess (ila kan)
transactions_by_day <- merge(all_dates, transactions_by_day, by = "DATE", all.x = TRUE)

# 5. Nrssmo Graph bach nchoufo l-mabi3at kif ghada
# Ghadi yban lik wahed l-hbtA kbira f wahed l-wqt
ggplot(transactions_by_day, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions over time") +
  theme_bw()

# ---------------------------------------------------------
# MARHALA 8: Zoom sur Décembre (Zoom 3la chhar 12)
# ---------------------------------------------------------

# 1. N3zlo ghir l-data dyal chhar 12
december_data <- transactions_by_day[month(DATE) == 12, ]

# 2. Nrssmo Graph jdid ghir dyal had chhar
ggplot(december_data, aes(x = DATE, y = N)) +
  geom_line() +
  labs(x = "Day", y = "Number of transactions", title = "Transactions in December") +
  scale_x_date(date_labels = "%d", date_breaks = "1 day") + # Bach ybano l-iyam kamlin
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) # Dwrna l-ktba bach tban

# ---------------------------------------------------------
# MARHALA 9: Feature Engineering (Pack Size & Brand)
# ---------------------------------------------------------

# 1. Njbou 'Pack Size' (L-wzn)
# had 'parse_number' katqllb 3la arqam wst l-ktba w katjbdhom
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

# 2. Njbou 'Brand' (L-marka)
# Hna kanakhdo ghir l-kalma l-lwla mn PROD_NAME
transactionData[, BRAND := tstrsplit(PROD_NAME, " ")[[1]]]

# 3. Nchoufo n-natija
print("---- Nchoufo Colonnes Jdad (Top 6) ----")
head(transactionData[, .(PROD_NAME, PACK_SIZE, BRAND)])

# 4. Nchoufo achno huma l-awzan (Sizes) li kaynin bzaf (Histogram)
print("---- Graph dyal Awzan l-Bakiya ----")
ggplot(transactionData, aes(x = PACK_SIZE)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Distribution of Pack Sizes", x = "Pack Size (g)", y = "Count") +
  theme_bw()

# ---------------------------------------------------------
# MARHALA 10: Correction des noms de marques (Brand Cleaning)
# ---------------------------------------------------------

# 1. Nbdlo s-smiyat l-mkhtasra awla l-ghalta
transactionData[BRAND == "Red", BRAND := "RRD"]
transactionData[BRAND == "Smith", BRAND := "Smiths"]
transactionData[BRAND == "Dorito", BRAND := "Doritos"]
transactionData[BRAND == "Snbts", BRAND := "Sunbites"]
transactionData[BRAND == "Infzns", BRAND := "Infuzions"]
transactionData[BRAND == "Ww", BRAND := "Woolworths"]
transactionData[BRAND == "Grain", BRAND := "Grnwves"]
transactionData[BRAND == "Ncc", BRAND := "Natural"]

# 2. Nchoufo s-smiyat l-mqada
print("---- Brands Nqyine ----")
unique(transactionData$BRAND)

# ---------------------------------------------------------
# MARHALA 11: Jmi3 d-Data (Merging)
# ---------------------------------------------------------

# Daba ghadi njm3o 'Transaction Data' m3a 'Customer Data'
# Bach nwelliw n3rfo kola chariya, chkoun moulaha (Wach 3azabi, Mjwj, Premium...)
data <- merge(transactionData, customerData, all.x = TRUE)

# Nchoufo wach kayna chi haja naqsa (Missing values) mlli jma3nahom
print("---- Wach kayn chi client malqinach ma3loumato? ----")
colSums(is.na(data))

# Nchoufo t-tableau l-kbir l-majmo3
print("---- Data Kamla (Top 6) ----")
head(data)


# ---------------------------------------------------------
# MARHALA 12: Tahlil l-Mabi3at (Sales Analysis)
# ---------------------------------------------------------

# 1. Njam3o l-mabi3at (Total Sales) 3la hsab kola groupe
sales_summary <- data[, .(TOT_SALES = sum(TOT_SALES)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

# 2. Nrssmo Graph bach nqarNo binathom
ggplot(sales_summary, aes(x = LIFESTAGE, y = TOT_SALES, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Sales by Customer Segment", 
       x = "Lifestage", 
       y = "Total Sales",
       fill = "Customer Type") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # Dwrna l-ktba bach tban


# ---------------------------------------------------------
# MARHALA 13: 3lach hado kaychriw bzaf? (Drivers of Sales)
# ---------------------------------------------------------

# 1. N7sbo 3adad les clients f kola segment
customers_summary <- data[, .(UNIQUE_CUSTOMERS = uniqueN(LYLTY_CARD_NBR)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

# 2. Nrssmo Graph dyal 3adad l-Kliyan
ggplot(customers_summary, aes(x = LIFESTAGE, y = UNIQUE_CUSTOMERS, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Customers by Segment", 
       x = "Lifestage", 
       y = "Number of Customers",
       fill = "Customer Type") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# ---------------------------------------------------------
# MARHALA 14: Mow3ddal l-bakiyat (Units per Customer)
# ---------------------------------------------------------

# 1. N7sbo chhal mn bakiya kaychri l-wahed f l-moyenne
avg_units <- data[, .(AVG = sum(PROD_QTY)/uniqueN(LYLTY_CARD_NBR)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

# 2. Nrssmo Graph
ggplot(avg_units, aes(x = LIFESTAGE, y = AVG, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Units per Customer", 
       x = "Lifestage", 
       y = "Average Units",
       fill = "Customer Type") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# ---------------------------------------------------------
# MARHALA 15: Taman l-Mowattassit (Average Price per Unit)
# ---------------------------------------------------------

# 1. N7sbo taman bakiya wehda f l-moyenne (Total Sales / Total Quantity)
avg_price <- data[, .(AVG = sum(TOT_SALES)/sum(PROD_QTY)), by = .(LIFESTAGE, PREMIUM_CUSTOMER)]

# 2. Nrssmo Graph
ggplot(avg_price, aes(x = LIFESTAGE, y = AVG, fill = PREMIUM_CUSTOMER)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Price per Unit", 
       x = "Lifestage", 
       y = "Average Price",
       fill = "Customer Type") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# ---------------------------------------------------------
# MARHALA 16: T-Test (Statistical Difference)
# ---------------------------------------------------------

# 1. N-creew groupe jdid fih ghir Young w Midage Singles/Couples
target_segment <- data[LIFESTAGE %in% c("YOUNG SINGLES/COUPLES", "MIDAGE SINGLES/COUPLES")]

# 2. Nfrrqohom l Jouj groups: 
# Group 1: Mainstream
# Group 2: Others (Budget + Premium)
mainstream <- target_segment[PREMIUM_CUSTOMER == "Mainstream", TOT_SALES / PROD_QTY]
other <- target_segment[PREMIUM_CUSTOMER != "Mainstream", TOT_SALES / PROD_QTY]

# 3. Ndiro T-Test
t_test_result <- t.test(mainstream, other, alternative = "greater")

# 4. Nchoufo Natija
print(t_test_result)


# ---------------------------------------------------------
# MARHALA 17: Brand Affinity (Achmen marka kayfddlo?)
# ---------------------------------------------------------

# 1. N3zlo l-Groupe dyalna (Young Singles - Mainstream)
segment_data <- data[LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream", ]
other_data <- data[!(LIFESTAGE == "YOUNG SINGLES/COUPLES" & PREMIUM_CUSTOMER == "Mainstream"), ]

# 2. N7sbo chhal % kola Marka kat-tba3 3ndna vs 3nd lokhrin
quantity_segment <- sum(segment_data$PROD_QTY)
quantity_other <- sum(other_data$PROD_QTY)

brand_proportions_segment <- segment_data[, .(target_ratio = sum(PROD_QTY)/quantity_segment), by = BRAND]
brand_proportions_other <- other_data[, .(other_ratio = sum(PROD_QTY)/quantity_other), by = BRAND]

# 3. Njam3ohom bach nqarNo
brand_affinity <- merge(brand_proportions_segment, brand_proportions_other, by = "BRAND")

# 4. N7sbo Affinity (Target / Other)
brand_affinity[, affinity := target_ratio / other_ratio]

# 5. Nrttbohom bach nchoufo Top Brands
brand_affinity <- brand_affinity[order(-affinity)]

print("---- Top Brands li kayfddlohom Chbab (Mainstream) ----")
head(brand_affinity)

# ---------------------------------------------------------
# MARHALA 18: Pack Size Affinity (Achmen hajm kayfddlo?)
# ---------------------------------------------------------

# 1. N7sbo % d l-mabi3at 3la hsab l-Hajm (Size)
pack_proportions_segment <- segment_data[, .(target_ratio = sum(PROD_QTY)/quantity_segment), by = PACK_SIZE]
pack_proportions_other <- other_data[, .(other_ratio = sum(PROD_QTY)/quantity_other), by = PACK_SIZE]

# 2. Merge & Affinity Calculation
pack_affinity <- merge(pack_proportions_segment, pack_proportions_other, by = "PACK_SIZE")
pack_affinity[, affinity := target_ratio / other_ratio]
pack_affinity <- pack_affinity[order(-affinity)]

# 3. Nchoufo Natija
print("---- Top Pack Sizes ----")
head(pack_affinity)