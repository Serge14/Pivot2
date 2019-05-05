
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
df[str_detect(Age2, "-") & is.na(Age), Age := convertYtoM(Age2)]

# Assign NA to the rest
df[is.na(Age), Age := "NA"]

# Remove unnecessary columns
df[, Age2 := NULL]

}
