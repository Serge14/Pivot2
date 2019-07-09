# Pivot 2
# converter

library(data.table)
library(stringr)

source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/functions.R")
source("/home/sergiy/Documents/Work/Nutricia/Scripts/Pivot2/dictionaries.R")

setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201905")

df = fread("N_201905.csv", check.names = TRUE, na.strings = "NA")

df = fread("N_Y19_M04.csv", check.names = TRUE, na.strings = "NA")
df1 = fread("N_Y19_M04_value.csv", check.names = TRUE, na.strings = "NA")

str(df)
str(df1)

use.Existing.Attributes = TRUE

cols = c("SKU", "BRAND", "BRAND.OWNER",
         "DANONE.SEGMENT", "DANONE.SUB.SEGMENT",
         "PRODUCT.FORM", "TYPE...BABY.PRODUCT", "PRODUCT.BASE")

df = melt.data.table(df, id.vars = cols)
df1 = melt.data.table(df1, id.vars = cols)

names(df)[10] = "Volume"
df[df1, on = c("SKU", "variable"), Value := i.value]

if (use.Existing.Attributes == TRUE) {
  
  SKU.Matrix = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201905/SKU Matrix.csv")
  # SKU.Matrix = SKU.Matrix[Channel == "MT,
  #                         .(SKU = unique(SKU)),
  #                         by = 
  #                           .(Brand, SubBrand, Size, Age,
  #                             PS0, PS2, PS3, PS,
  #                             Form, Company)]
  
  df[SKU.Matrix, on = "SKU",
     `:=`(Brand = i.Brand,
          SubBrand = i.PreSubBrand,
          Size = i.Size,
          Age = i.Age,
          Scent = i.PreScent,
          PS0 = i.PS0,
          PS2 = i.PS2,
          PS3 = i.PS3,
          PS = i.PS,
          Form = i.Form,
          Company = i.Company)]
  
  df.existing = df[!is.na(Brand)]
  df = df[is.na(Brand)]
  
}

# Brand - Company
df[dictCompanyBrand, on = c(BRAND = "NielsenBrand"), Brand := i.RTRIBrand]
df[dictCompanyBrand, on = c(Brand = "RTRIBrand"), Company := i.RTRICompany]

df[is.na(Brand), .N]
df[is.na(Company), .N]

# SubBrand
#df = addSubBrand(df)
# df[, SubBrand := NA]

df = addSize(df)
df = addForm(df)
df = addAge2(df)

# PS0, PS2, PS3
df[dictSegments, 
   on = c(DANONE.SUB.SEGMENT = "NielsenSegment"), 
   `:=`(PS = i.PS, PS3 = i.PS3, PS0 = i.PS0)] # this is wrong due to PS2
df = addPS2(df)


# Add Scent

df[, Scent := 
     sapply(str_split(PRODUCT.BASE, "&"), function(x) paste(sort(x), collapse = "-"))]

df[, Scent := str_to_title(str_trim(Scent))]


# Merge datasubsets
all(names(df) == names(df.existing))
df = rbindlist(list(df.existing, df))

# Some data may be incorrect, needs to be corrected
df = correctSegments(df)


df = addDates(df)




df[is.na(Volume), Volume := 0]
df[is.na(Value), Value := 0]
# df = df[Value != "NA" | !is.na(Value)]
df = df[(Value + Volume) > 0]

df[, `:=`(Volume = Volume*1000,
          Value = Value*1000)]

df = df[, .(Volume = sum(Volume),
            Value = sum(Value)), 
        by = .(SKU, Ynb, Mnb, 
               Brand, SubBrand, Size, Age, Scent, Company, 
               PS0, PS3, PS2, PS, Form)]

df[, Channel := "MT"]

## Checks
df[is.na(Age) | Age == "", unique(SKU), by = PS3]
df[is.na(Form) | Form == "", unique(SKU), by = PS3]

df[is.na(PS0) | PS0 == "", unique(SKU), by = PS3]

df[is.na(Form) | Form == "", Form := "Not Applicable"]

# df[is.na(PriceSegment) & (PS2 == "Dry Food" | PS0 == "IMF" |
#                             (PS3 == "Fruits" | PS3 == "Savoury Meal")), 
# unique(Brand), by = PS]
df[is.na(PS2) | PS2 == "", unique(SKU), by = PS3]

# Merge with RTRI data, pharma channel

df1 = fread("/home/sergiy/Documents/Work/Nutricia/1/Data/df_Y19M05.csv")

df1 = df1[Ynb == 2019 & Mnb == 5 & Channel == "PHARMA",
          .(Volume = sum(Volume),
            Value = sum(Value)),
          by = .(SKU, Ynb, Mnb, 
                 Brand, SubBrand, Size, Age, Scent, Company, 
                 PS0, PS2, PS3, PS, Form, 
                 Channel)]

setcolorder(df, names(df1))

all(names(df) == names(df1))

df = rbindlist(list(df, df1))

# Add price Segments
df = addPriceSegments(df)

# Check Price Segments
df[(is.na(PriceSegment) | PriceSegment == "") & (PS2 == "Dry Food" | PS0 == "IMF" |
                            (PS3 == "Fruits" | PS3 == "Savoury Meal")), 
   unique(Brand), by = PS]

df[(is.na(GlobalPriceSegment) | GlobalPriceSegment == "") & 
     (PS2 == "Dry Food" | PS0 == "IMF" | (PS3 == "Fruits" | PS3 == "Savoury Meal")), 
   unique(Brand), by = PS]


# Extrapolate (check 1.2 in Puree)
df = add_EC_AC(df)
df[, `:=`(VolumeC = Volume*EC*AC,
          ValueC = Value*EC*AC)]

# Merge with historical data

df1 = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201904/df_N_SB.csv")
setcolorder(df, names(df1))
all(names(df) == names(df1))

df = rbindlist(list(df1, df))

df = df[Volume + Value > 0]

setwd("/home/sergiy/Documents/Work/Nutricia/Rework/201905")
fwrite(df, "df_N_4.csv", row.names = FALSE)

df.SKU = df[Channel == "MT", .(SKU = unique(SKU)), by = .(Brand, SubBrand, Size,
                         Age, Scent, Company,
                         PS0, PS2, PS3, PS, Form)]
df.SKU[df, on = "SKU", 
       `:=`(BRAND = i.BRAND,
            BRAND.OWNER = i.BRAND.OWNER,
            DANONE.SEGMENT = i.DANONE.SEGMENT,
            DANONE.SUB.SEGMENT = i.DANONE.SUB.SEGMENT,
            PRODUCT.FORM = i.PRODUCT.FORM,
            TYPE...BABY.PRODUCT = i.TYPE...BABY.PRODUCT,
            PRODUCT.BASE = i.PRODUCT.BASE)]


df[, ScentType2 := 
     sapply(str_split(ScentType, "-"), function(x) paste(sort(x), collapse = "-"))]

df[, Scent := str_to_title(str_trim(ScentType2))]

df = fread("SKU Matrix with cereals.csv")

df[, ScentType2 := 
     sapply(str_split(ScentType, "-"), function(x) paste(sort(x), collapse = "-"))]

df[, ScentType2 := str_to_title(str_trim(ScentType2))]

df[, Scent21 := 
     sapply(str_split(Scent2, "-"), function(x) paste(sort(x), collapse = "-"))]

df[, Scent21 := str_to_title(str_trim(Scent21))]

dict = df[, unlist(unique(Scent))]
dict2 = sapply(str_split(dict, "-"), function(x) sort(unique(x)))
dict2 = sort(unique(unlist(dict2, use.names = FALSE)))

dict.s2 = df[, unlist(unique(Scent21))]
dict.s2 = sapply(str_split(dict.s2, "-"), function(x) sort(unique(x)))
dict.s2 = sort(unique(unlist(dict.s2, use.names = FALSE)))

# write.csv(df, "Matrx2.csv", row.names = FALSE)
dict.s2[!(dict.s2 %in% dict2)]

dict.s3 = df[, unlist(unique(ScentType2))]
dict.s3 = sapply(str_split(dict.s3, "-"), function(x) sort(unique(x)))
dict.s3 = sort(unique(unlist(dict.s3, use.names = FALSE)))

# write.csv(df, "Matrx2.csv", row.names = FALSE)
dict.s3[!(dict.s3 %in% dict2)]

sku.matrix = fread("SKU Matrix2.csv")
sku.matrix[df, on = "SKU", `:=`(Scent2 = i.Scent21, ScentType = i.ScentType2)]
write.csv(sku.matrix, "SKU Matrix2.csv", row.names = FALSE)
