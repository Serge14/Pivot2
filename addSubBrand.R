addSubBrand = function(df){
  
  df[, SubBrand := str_extract(SKU, "[^0-9]{2,}")]
  df[, SubBrand := str_trim(SubBrand)]
  df[, SubBrand := str_remove(SubBrand, " P$")]
  df[, SubBrand := str_remove(SubBrand, " L$")]
  df[, SubBrand := str_remove(SubBrand, " C$")]
  df[, SubBrand := str_remove(SubBrand, " CW$")]
  df[, SubBrand := str_remove(SubBrand, " CM$")]
  df[, SubBrand := str_remove(SubBrand, "/$")]
  df[, SubBrand := str_trim(SubBrand)]
  df[, SubBrand := str_to_title(SubBrand)]
  
}





