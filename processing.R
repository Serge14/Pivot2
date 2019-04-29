# Pivot 2

library(data.table)
library(stringr)

setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201903")
df = fread("N_Y2018-Y19_M03.csv", check.names = TRUE)

setwd("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2")
dictCompanyBrand = fread("dictCompanyBrand.csv")
dictSegments = fread("dictSegments.csv")
dictPriceSegments = fread("/home/sergiy/Documents/Work/Nutricia/Rework/Dictionaries/PriceSegments.csv")

df[grepl("PYURE", SKU), PRODUCT.FORM := "Pure"]

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
        by = .(Ynb, Mnb, Brand, Company, PS0, PS3, PS, PRODUCT.FORM)]

df[, Channel := "MT"]
df[, Form := str_to_title(PRODUCT.FORM)]
df[, PRODUCT.FORM := NULL]

df1 = fread("/home/sergiy/Documents/Work/Nutricia/1/Data/df.csv")
df1 = df1[Ynb >= 2018 & Channel == "PHARMA",
          .(Ynb, Mnb, Brand, Company, 
            PS0, PS3, PS, Form, 
            PriceSegment, Channel,
            Volume)]

df1 = df1[, .(Volume = sum(Volume)),
          by = .(Ynb, Mnb, Brand, Company, 
                 PS0, PS3, PS, Form, 
                 PriceSegment, Channel)]

df = df[, .(Ynb, Mnb, Brand, Company, PS0, PS3, PS, Form, 
            PriceSegment, Channel,
            Volume)]
df = rbindlist(list(df[Volume > 0], df1))

fwrite(df, "df_MT_PH.csv", row.names = FALSE)
