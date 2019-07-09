# Pivot 2

library(data.table)
library(stringr)

source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/addAge.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/addPS2.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/addSubBrand.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/correctSegments.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/add_EC_AC.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/addPriceSegments.R")

setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201903")

df = fread("N_Y2015-Y19_M03.csv", check.names = TRUE, na.strings = "NA")
df1 = fread("N_Y2015-Y19_M03_value.csv", check.names = TRUE, na.strings = "NA")

setwd("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2")
dictCompanyBrand = fread("dictCompanyBrand.csv")
dictSegments = fread("dictSegments.csv")
dictEC = fread("dictEC.csv")
dictAgeExceptions = fread("dictAgeExceptions.csv")
dictPriceSegments = fread("/home/sergiy/Documents/Work/Nutricia/Rework/Dictionaries/PriceSegments.csv")


cols = c("SKU", "BRAND", "BRAND.OWNER",
          "DANONE.SEGMENT", "DANONE.SUB.SEGMENT",
          "PRODUCT.FORM", "TYPE...BABY.PRODUCT")

df = melt.data.table(df, id.vars = cols)
df1 = melt.data.table(df1, id.vars = cols)

names(df)[9] = "Volume"

df[df1, on = c("SKU", "variable"), Value := i.value]

df[grepl("PYURE", SKU), PRODUCT.FORM := "Pure"]

# df$value = as.numeric(df$value)

# Age
# df = addAge(df)
df = addAge2(df)

# Brand - Company
df[dictCompanyBrand, on = c(BRAND = "NielsenBrand"), Brand := i.RTRIBrand]
df[dictCompanyBrand, on = c(Brand = "RTRIBrand"), Company := i.RTRICompany]

# Segments PS0 & PS3
df[dictSegments, 
   on = c(DANONE.SUB.SEGMENT = "NielsenSegment"), 
   `:=`(PS = i.PS, PS3 = i.PS3, PS0 = i.PS0)] # this is wrong due to PS2

df = correctSegments(df)
df = addPS2(df)
# df[, SubBrand := mapply(addSubBrand2, SKU)]
#df = addSubBrand(df)
df[, SubBrand := NA]

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
# df[is.na(value), value := 0]
df = df[Value != "NA"]
df[is.na(Volume), Volume := 0]
df[, `:=`(Volume = Volume*1000,
          Value = Value*1000)]

df = df[, .(Volume = sum(Volume),
            Value = sum(Value)), 
        by = .(SKU, Ynb, Mnb, 
               Brand, SubBrand, Size, Age, Company, 
               PS0, PS3, PS2, PS, PRODUCT.FORM)]

df[, Channel := "MT"]
df[, Form := str_to_title(PRODUCT.FORM)]
df[, PRODUCT.FORM := NULL]



# Merge with RTRI data

df1 = fread("/home/sergiy/Documents/Work/Nutricia/1/Data/df.csv")
# df1 = df1[Ynb >= 2018 & Channel == "PHARMA",
#           .(SKU, Ynb, Mnb, 
#             Brand, SubBrand, Size, Age, Company, 
#             PS0, PS2, PS3, PS, Form, 
#             Channel,
#             Value)]
# 
# df1 = df1[, .(Value = sum(Value)),
#           by = .(SKU, Ynb, Mnb, 
#                  Brand, SubBrand, Size, Age, Company, 
#                  PS0, PS2, PS3, PS, Form, 
#                  Channel)]

# df1 = df1[Ynb >= 2018 & Channel == "PHARMA",
#           .(SKU, Ynb, Mnb, 
#             Brand, SubBrand, Size, Age, Company, 
#             PS0, PS2, PS3, PS, Form, 
#             Channel,
#             Volume, Value)]

df1 = df1[Ynb >= 2015 & Channel == "PHARMA",
          .(Volume = sum(Volume),
            Value = sum(Value)),
          by = .(SKU, Ynb, Mnb, 
                 Brand, SubBrand, Size, Age, Company, 
                 PS0, PS2, PS3, PS, Form, 
                 Channel)]

df = df[, .(SKU, Ynb, Mnb, 
            Brand, SubBrand, Size, Age, Company, 
            PS0, PS2, PS3, PS, Form, 
            Channel,
            Volume, Value)]

all(names(df) == names(df1))

df = rbindlist(list(df, df1))

# df = rbindlist(list(df[Value > 0], df1))

# Price Segments
df = addPriceSegments(df)


df = add_EC_AC(df)

df[, `:=`(VolumeC = Volume*EC*AC,
          ValueC = Value*EC*AC)]
# df[, ValueC := Value*EC]

df = df[, .(SKU, Ynb, Mnb,
            Brand, SubBrand, Size, Age, Company,
            PS0, PS2, PS3, PS,
            Form, Channel, 
            PriceSegment, GlobalPriceSegment,
            Volume, Value, EC, AC, VolumeC, ValueC)]

# df = df[, .(SKU, Ynb, Mnb,
#             Brand, SubBrand, Size, Age, Company,
#             PS0, PS2, PS3, PS,
#             Form, Channel, 
#             PriceSegment, GlobalPriceSegment,
#             Value, EC, ValueC)]

setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201903")
fwrite(df, "df_MT_PH_SKU_from15.csv", row.names = FALSE)
