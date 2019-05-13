
convertYtoM = function(string) {
  string = str_split(string, "-")
  if (str_detect(string[[1]][1], "[GL]")) {
    string[[1]][1] = as.numeric(str_extract(string[[1]][1], "^[0-9]+"))*12
  }
  if (str_detect(string[[1]][2], "[GL]")) {
    string[[1]][2] = as.numeric(str_extract(string[[1]][2], "^[0-9]+"))*12
  }
  string = paste0(string[[1]][1], "-", string[[1]][2])
  return(string)
}

addAge = function(df) {

# Select Age from Nielsen's SKU description, pattern: S-digit-letter
df[, Age := str_extract(str_sub(SKU, start = -12),
                       "/S[0-9]+[-MESDNL]*")]
# Select all the rest options
df[is.na(Age), Age := str_extract(str_sub(SKU, start = -12), 
                       "[/S]?[ ]?[0-9]+[ ]?[-MESDNL]*[0-9]*[LMG]*")]

# Remove all spaces
df[, Age2 := str_replace_all(Age, " ", "")]

# Remove all unnecessary letters and symbols
# Dot can be in the middle of the age, additional check needs to be implemented
df[, Age2 := str_replace_all(Age2, "[/MESN\\.]", "")]
# a[, Age2 := str_replace_all(Age2, "\d+", "")][]

# Assign NA to Age
df[, Age := NA]

# Only month is mentioned
df[str_detect(Age2, "^[0-9]+[M]?$"), 
  Age := paste0(str_extract(Age2, "^[0-9]+"), "+")]

# From the 1st day
df[str_detect(Age2, "^[0-9]{1}[D]$"), Age := "0+"]

# From N years
df[str_detect(Age2, "^[0-9]{1}[GL]$"), 
  Age := paste0(as.numeric(str_extract(Age2, "^[0-9]+"))*12, "+")]

# Months & hyphen, letter M is optionally
df[str_detect(Age2, "^[0-9]*-[0-9]*$"), Age := Age2]

# Range with years
df[str_detect(Age2, "-") & is.na(Age), Age := mapply(convertYtoM, Age2)]

# Assign NA to the rest
df[is.na(Age), Age := "NA"]

# Remove unnecessary columns
df[, Age2 := NULL]

}

addAge2 = function(df) {
  
  # Remove spaces
  df[, SKU2 := str_replace_all(SKU, " ", "")]
  df[, SKU2 := str_replace_all(SKU2, "ML", "XX")]
  df[, SKU2 := str_sub(SKU2, start = -12)]
  
  #d-d
  df[, Age2 := str_extract(SKU2, "[0-9]{1,2}-[0-9]{1,2}$")]
  
  #d[MES]-d[MES]
  df[is.na(Age2), 
     Age2 := str_extract(SKU2, "[0-9]{1,2}[M(?=ES).]*-[0-9]{1,2}[M(?=ES).]*$")]
  
  #Years
  # df[is.na(Age2), 
  #    Age2 := str_extract(SKU2, "[0-9]{1,2}[M(?=ES)\\.]*[GL]*-[1-9]{1,2}[M(?=ES).]*[GL]*$")][]
  
  df[is.na(Age2), 
     Age2 := str_extract(SKU2, "[0-9]{1,2}[M(?=ES)\\.GL]*-[1-9]{1,2}[GL]*$")]
  
  #SdDN
  # df[is.na(Age2), 
  #    Age2 := str_extract(SKU2, "[S]{0,1}[0-9]{1,2}[D(?=N).]*$")]
 
  df[is.na(Age2), 
     Age2 := str_extract(SKU2, "[S]{0,1}[0-9]{1,2}DN?\\.?$")]
  
  df[is.na(Age2), 
     Age2 := str_extract(SKU2, "[S]{0,1}[0-9]{1,2}DN?\\.?")]
  
  #SdMES
  df[is.na(Age2), 
     Age2 := str_extract(SKU2, "[S]{0,1}[0-9]{1,2}\\.*[0-9]*[M(?=ES)]*\\.?$")]
  
  #SdM in the end
  df[is.na(Age2), 
     Age2 := str_extract(SKU2, "[S]{0,1}[0-9]{1,2}[M]")]
  
  #SdY in the end
  df[is.na(Age2), 
     Age2 := str_extract(SKU2, "[S]{0,1}[1-9]{1}[GL]")]
  
  #d in the end
  df[is.na(Age2), 
     Age2 := str_extract(SKU2, "(?<=/)[1-9]{1,2}[(?=OLD)]*$")]
  
  
  # Delete dots, but not in the digits
  df[, Age2 := str_replace_all(Age2, "((?<![0-9])\\.)|(\\.(?![0-9]))", "")]
  
  # Delete unnecessary symbols
  df[, Age2 := str_replace_all(Age2, "[/MESN]", "")]
  df[, Age2 := str_replace_all(Age2, "=OLD", "")]
  
  ### Transform Ages to months
  
  # Assign NA to Age
  df[, Age := NA]
  df$Age = as.character(df$Age)
  
  # Only month is mentioned
  df[str_detect(Age2, "^[0-9]+\\.*[0-9]*[M]?$"), 
     Age := paste0(str_extract(Age2, "^[0-9]+\\.*[0-9]*"), "+")]
  
  # From the 1st day
  df[str_detect(Age2, "^[0-9]{1}[D]$"), Age := "0+"]
  
  # From N years
  df[str_detect(Age2, "^[0-9]{1}[GL]$"), 
     Age := paste0(as.numeric(str_extract(Age2, "^[0-9]+"))*12, "+")]
  
  # Months & hyphen, letter M is optionally
  df[str_detect(Age2, "^[0-9]+[M]*-[0-9]*[M]*$"), Age := Age2]
  
  # Range with years
  df[str_detect(Age2, "-") & is.na(Age), Age := mapply(convertYtoM, Age2)]
  
  # 1-3L
  df[str_detect(Age2, "1-3L?$"), Age := "12-36"]
  
  # Assign NA to the rest
  df[is.na(Age), Age := "NA"]
  
  # Remove unnecessary columns
  df[, Age2 := NULL]
  df[, SKU2 := NULL]
  
  # Add exclusions
  df1 = fread("dictAgeExceptions.csv")
  # SKUAgeExceptions = df1[, SKU]
  # df[SKU %in% SKUAgeExceptions][df1, on = c(SKU = "SKU"), Age := i.Age]
  df[df1, on = "SKU", Age := i.Age]
}