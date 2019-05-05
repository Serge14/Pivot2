# Pivot 2

library(data.table)
library(stringr)

source(/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/addAge.R)
setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201903")
df = fread("N_Y2018-Y19_M03.csv", check.names = TRUE)

setwd("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2")
dictCompanyBrand = fread("dictCompanyBrand.csv")
dictSegments = fread("dictSegments.csv")
dictEC = fread("dictEC.csv")
dictPriceSegments = fread("/home/sergiy/Documents/Work/Nutricia/Rework/Dictionaries/PriceSegments.csv")

df[grepl("PYURE", SKU), PRODUCT.FORM := "Pure"]

# Age
df = addAge(df)


cols = c("SKU", "BRAND", "BRAND.OWNER",
          "DANONE.SEGMENT", "DANONE.SUB.SEGMENT",
          "PRODUCT.FORM", "TYPE...BABY.PRODUCT")

df = melt.data.table(df, id.vars = cols)

# Brand - Company
df[dictCompanyBrand, on = c(BRAND = "NielsenBrand"), Brand := i.RTRIBrand]
df[dictCompanyBrand, on = c(Brand = "RTRIBrand"), Company := i.RTRICompany]

# Segments
df[dictSegments, 
   on = c(DANONE.SUB.SEGMENT = "NielsenSegment"), 
   `:=`(PS = i.PS, PS3 = i.PS3, PS0 = i.PS0)] # this is wrong due to PS2

DryFoodSegments = c("Instant Cereals",
                    "Cereal Biscuits",
                    "Ready To Eat Cereals",
                    "Liquid Cereals")

df[PS3 %in% DryFoodSegments, PS2 := "Dry Food"]

# Size
df[, Size := str_extract(SKU, "[0-9]+[GML]+")]
df[, Size := str_replace(Size, "G", "Gr")]
df[, Size := str_replace(Size, "ML", "Ml")]




# Dates
dictMonth = data.table(
  Month = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
            "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"),
  Mnb = 1:12
)

df[, Ynb := str_extract(variable, "[0-9]+")]
df[, Mnb := str_extract(variable, "[A-Za-z]+")]
df[dictMonth, on = .(Mnb = Month), Mnb := i.Mnb]

# Sales
df[is.na(value), value := 0]
df[, Volume := value*1000]

df = df[, .(Volume = sum(Volume)), 
        by = .(SKU, Ynb, Mnb, 
               Brand, Size, Company, 
               PS0, PS3, PS2, PS, PRODUCT.FORM)]

df[, Channel := "MT"]
df[, Form := str_to_title(PRODUCT.FORM)]
df[, PRODUCT.FORM := NULL]



# Merge with RTRI data

df1 = fread("/home/sergiy/Documents/Work/Nutricia/1/Data/df.csv")
df1 = df1[Ynb >= 2018 & Channel == "PHARMA",
          .(SKU, Ynb, Mnb, 
            Brand, Size, Company, 
            PS0, PS2, PS3, PS, Form, 
            Channel,
            Volume)]

df1 = df1[, .(Volume = sum(Volume)),
          by = .(SKU, Ynb, Mnb, 
                 Brand, Size, Company, 
                 PS0, PS2, PS3, PS, Form, 
                 Channel)]

df = df[, .(SKU, Ynb, Mnb, 
            Brand, Size, Company, 
            PS0, PS2, PS3, PS, Form, 
            Channel,
            Volume)]

all(names(df) == names(df1))

df = rbindlist(list(df[Volume > 0], df1))

# Price Segments
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

# Extrapolation

# Extrapolation coefficients
df[dictEC, on = c("Channel", "PS3"), EC := i.EC]
df[PS == "Digestive Comfort" & Channel == "MT", EC := 1.1]
df[PS == "Digestive Comfort" & Channel == "PHARMA", EC := 1.0041891]
df[PS == "Hypoallergenic" & Channel == "MT", EC := 1.1]
df[PS == "Hypoallergenic" & Channel == "PHARMA", EC := 1.1]
df[is.na(EC), .N]

df[, VolumeC := Volume*EC]

df = df[, .(SKU, Ynb, Mnb,
            Brand, Size, Company,
            PS0, PS2, PS3, PS,
            Form, Channel, 
            PriceSegment, GlobalPriceSegment,
            Volume, EC, VolumeC)]

setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201903")
fwrite(df, "df_MT_PH_SKU.csv", row.names = FALSE)
