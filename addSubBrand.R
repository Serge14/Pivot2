addSubBrand = function(df) {
  
  df[, SubBrand := mapply(addSubBrand2, SKU)]
  
}

addSubBrand2 = function(sku) {
  
  SKUBrand = df[SKU == sku, unique(Brand)]
  
  if (length(SKUBrand) > 1) {
    print("More than one brand are assigned to SKU")
  }
  
  SubBrands = df[Brand == SKUBrand & !is.na(SubBrand) & SubBrand != "NA",
                 unique(SubBrand)]
  
  if (length(SubBrands) > 0) {
  
    sku = str_remove(sku, "[0-9]+[GML]+")
    sku = str_squish(sku)
  
  
  similarity = stringsim(sku,
                        toupper(SubBrands),
                        method = c("lv"),
                        q = 4)
  maxSimilarity = which(similarity == max(similarity))
  
  } else {SubBrand = NA}
  
  if (length(maxSimilarity) > 1) {
    print(sku)
    print(SubBrands[maxSimilarity])
    SubBrand = NA
    
  } else {
    SubBrand = SubBrands[maxSimilarity]
    
  }
  return(SubBrand)
}