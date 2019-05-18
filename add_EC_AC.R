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
  
  df[Ynb == 2018 & Mnb %in% c(1, 2, 3, 5) & Channel == "PHARMA" & 
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic", 
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb %in% c(1, 2, 3, 5) & Channel == "PHARMA" & 
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic", 
     AC := 1.1]
  
  df[Ynb == 2018 & Mnb %in% c(10, 11, 12) & Channel == "PHARMA" & 
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic", 
     AC := 0.95]
  
  df[Ynb >= 2019 & Channel == "PHARMA" & 
       PS3 == "Specials" &
       PS != "Digestive Comfort" & PS != "Hypoallergenic", 
     AC := 0.95]
  
}