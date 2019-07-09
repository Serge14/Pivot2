addForm = function(df){
  
  df[grepl("PYURE", SKU), PRODUCT.FORM := "Pure"]
  df[, Form := str_to_title(PRODUCT.FORM)]
  # df[, PRODUCT.FORM := NULL]
  df[Form == "Not Applicable", Form := NA]
  
}

addSize = function(df) {
  
  # Size
  df[, Size := str_extract(SKU, "[0-9]+[GML]+")]
  df[, Size := str_replace(Size, "G", "Gr")]
  df[, Size := str_replace(Size, "ML", "Ml")]
  
}

addDates = function(df){
  
  # Dates
  dictMonth = data.table(
    Month = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
              "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"),
    Mnb = 1:12
  )
  
  # df[, Ynb := str_extract(variable, "[0-9]+")]
  # df[, Mnb := str_extract(variable, "[A-Za-z]+")]
  
  df[, Ynb := str_extract(Period, "[0-9]+")]
  df[, Mnb := str_extract(Period, "[A-Za-z]+")]
  
  df[dictMonth, on = .(Mnb = Month), Mnb := i.Mnb]
  
}

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
  # df1 = fread("dictAgeExceptions.csv")
  # SKUAgeExceptions = df1[, SKU]
  # df[SKU %in% SKUAgeExceptions][df1, on = c(SKU = "SKU"), Age := i.Age]
  df[dictAgeExceptions, on = "SKU", Age := i.Age]
}

correctSegments = function(df){
  
  df[SKU == "NUTRILON 2 P400GJT GIPPOALL/MOL/6M",
     `:=`(PS = "Hypoallergenic",
          PS3 = "Specials")]
  
  df[SKU == "NUTRILON/1 PRONU+ P400GJT GIPPOAL/MOL/1D",
     `:=`(PS = "Hypoallergenic",
          PS3 = "Specials")]
  
  df[SKU == "NUTRILON/1 PRONUTRA+P800GJC M/GIPOAL/0-6",
     `:=`(PS = "Hypoallergenic",
          PS3 = "Specials")]
  
  df[SKU == "NUTRILON/A-REFL.P400GJT MOL/SIND.SRYG/1D",
     `:=`(PS = "Anti Reflux")]
  
  df[SKU == "Similac_Nizkolaktoznyi_375Gr_0+_N/S_IMF_IF_Base_BIF_Abbott Laboratories",
     `:=`(PS = "Digestive Comfort",
          PS3 = "Specials")]
  
  df[SKU == "Semper_Lemolac_650Gr_0-6_N/S_Foods_Wet Food_Specials_Anti Reflux_Hero Ag",
     `:=`(PS0 = "IMF", PS2 = "IF")]
  
  df[SKU == "Similac_Nizkolaktoznyi_375Gr_0+_N/S_IMF_IF_Specials_Hypoallergenic_Abbott Laboratories",
     `:=`(PS = "DR-NL")]
  
  df[SKU == "Gerber_Do Re Mi_350Gr_12-36_N/S_IMF_Gum_Base_Gum Base_Nestle",
     `:=`(PS0 = "IMF", PS = "Gum Base")]
  
}

addPriceSegments = function(df){
  
  # Price Segments local
  df[.(PS0 = "IMF", dictPriceSegments[Segment == "IMF"]),
     on = c(Brand = "Brand", PS0 = "Segment"), 
     PriceSegment := i.PriceSegment]
  
  df[.(PS2 = "Dry Food", dictPriceSegments[Segment == "Dry Food"]),
     on = c(Brand = "Brand", PS2 = "Segment"), 
     PriceSegment := i.PriceSegment]
  
  dictPriceSegments[Segment == "Puree", Segment := "Fruits"]
  df[.(PS3 = "Fruits", dictPriceSegments[Segment == "Fruits"]),
     on = c(Brand = "Brand", PS3 = "Segment"), 
     PriceSegment := i.PriceSegment]
  
  dictPriceSegments[Segment == "Fruits", Segment := "Savoury Meal"]
  df[.(PS3 = "Savoury Meal", dictPriceSegments[Segment == "Savoury Meal"]),
     on = c(Brand = "Brand", PS3 = "Segment"), 
     PriceSegment := i.PriceSegment]
  
  # Price Segments global
  df[.(PS0 = "IMF", dictPriceSegments[Segment == "IMF"]),
     on = c(Brand = "Brand", PS0 = "Segment"), 
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  df[.(PS2 = "Dry Food", dictPriceSegments[Segment == "Dry Food"]),
     on = c(Brand = "Brand", PS2 = "Segment"), 
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  # dictPriceSegments[Segment == "Fruits", Segment := "Savoury Meal"]
  df[.(PS3 = "Savoury Meal", dictPriceSegments[Segment == "Savoury Meal"]),
     on = c(Brand = "Brand", PS3 = "Segment"), 
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  dictPriceSegments[Segment == "Savoury Meal", Segment := "Fruits"]
  df[.(PS3 = "Fruits", dictPriceSegments[Segment == "Fruits"]),
     on = c(Brand = "Brand", PS3 = "Segment"), 
     GlobalPriceSegment := i.GlobalPriceSegment]
  
  # print("Price Segments (global, local):")
  # print(df[is.na(PriceSegment), .N])
  # print(df[is.na(GlobalPriceSegment), .N])
  
}

add_EC_AC = function(df) {
  
  # Extrapolation & correction
  
  # Extrapolation coefficients
  df[dictEC, on = c("Channel", "PS3"), EC := i.EC]
  df[PS == "Digestive Comfort" & Channel == "MT", EC := 1.1]
  df[PS == "Digestive Comfort" & Channel == "PHARMA", EC := 1.1]
  df[PS == "Hypoallergenic" & Channel == "MT", EC := 1.1]
  df[PS == "Hypoallergenic" & Channel == "PHARMA", EC := 1.1]
  df[is.na(EC), .N]
  
  # Additional corrections
  
  df[, AC := 1]
  
  # Old problem, Pharma * 1.3
  df[Ynb == 2016 & Mnb %in% c(3:9) & 
       Channel == "PHARMA" & 
       PS3 != "Specials", 
     AC := 1.3]
  
  df[Ynb == 2017 & Mnb %in% c(7:12) & Channel == "PHARMA" & 
       PS == "Digestive Comfort", 
     AC := 1.3] # was 1.2
  
  df[Ynb == 2018 & Mnb %in% c(1:6) & Channel == "PHARMA" & 
       PS == "Digestive Comfort", 
     AC := 1.3] # was 1.2
  
  df[Ynb == 2019 & Mnb %in% c(1:2) & Channel == "PHARMA" & 
       PS == "Digestive Comfort", 
     AC := 0.89]
  
  df[Ynb == 2017 & Mnb %in% c(11:12) & 
       PS == "Hypoallergenic", 
     AC := 1.2]
  
  df[Ynb == 2018 & Mnb %in% c(1:9) & 
       PS == "Hypoallergenic", 
     AC := 1.2]
  
  df[Ynb >= 2019 & 
       PS == "Hypoallergenic", 
     AC := 0.95]
  
  df[Ynb == 2017 & Channel == "PHARMA" & 
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic", 
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb %in% c(1, 2, 3, 5) & Channel == "PHARMA" & 
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic", 
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb %in% c(1, 2, 3, 5) & 
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic", 
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb %in% c(10, 11, 12) & 
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic", 
     AC := 0.95]
  
  df[Ynb >= 2019 & 
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic", 
     AC := 0.95]
  
  df[Ynb == 2018 & Mnb %in% c(3, 5, 6) & Channel == "PHARMA" & 
       (PS3 == "Base" | PS3 == "Plus"),
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb == 12 & Channel == "PHARMA" & 
       (PS3 == "Base" | PS3 == "Plus"),
     AC := 0.985]
  
  df[Ynb >= 2019 & Channel == "PHARMA" & 
       (PS3 == "Base" | PS3 == "Plus"),
     AC := 0.985]
  
  df[Ynb == 2019 & Mnb >= 5 & Channel == "PHARMA" & 
       (PS3 == "Fruits" | PS3 == "Savoury Meal"),
     AC := 1.2]
  
  # Pharma lost 300 & 600 Gr SKUs, had to increase sales of old 400 & 800 Gr
  df[Ynb == 2019 & Mnb == 5 & Channel == "PHARMA" & 
       (SKU == "Nutrilon_Komfort 1_400Gr_0-6_N/S_IMF_IF_Specials_Digestive Comfort_Nutricia" |
        SKU == "Nutrilon_Komfort 1_800Gr_0-6_N/S_IMF_IF_Specials_Digestive Comfort_Nutricia" |
        SKU == "Nutrilon_Komfort 2_400Gr_6-12_N/S_IMF_FO_Specials_Digestive Comfort_Nutricia"),
     AC := 1.4]

}

addPS2 = function(df){
  
  #IF from 0 to 6, but actually from less than 6 months
  df[PS0 == "IMF" & as.numeric(str_extract(Age, "[0-9]{1,2}")) < 6, 
     PS2 := "IF"]
  
  # FO (6-12) starts from 6 to less than 12
  df[PS0 == "IMF" & 
       as.numeric(str_extract(Age, "[0-9]{1,2}")) >= 6 &
       as.numeric(str_extract(Age, "[0-9]{1,2}")) < 12, 
     PS2 := "FO"]
  
  # GUM 12+
  df[PS0 == "IMF" & as.numeric(str_extract(Age, "[0-9]{1,2}")) >= 12, 
     PS2 := "Gum"]
  
  # Others are not stated
  df[PS0 == "IMF" & is.na(PS2), PS2 := "N/S"]
  
  # Dry Food
  DryFoodSegments = c("Instant Cereals",
                      "Cereal Biscuits",
                      "Ready To Eat Cereals",
                      "Liquid Cereals")
  
  df[PS3 %in% DryFoodSegments, PS2 := "Dry Food"]
  
  # Wet Food
  WetFoodSegments = c("Fruits",
                      "Drinks",
                      "Savoury Meal",
                      "Dairy/desserts")
  
  df[PS3 %in% WetFoodSegments, PS2 := "Wet Food"]
  
  # All others
  df[is.na(PS2), PS2 := ""]
  
}
