
raw <- c("demo_mwi2024_central.pjnz",
         "demo_mwi2024_northern.pjnz", 
         "demo_mwi2024_southern.pjnz")

write_pjn_region_name <- function(pjnz, new_region_name) {
  pjn_file <- grep("PJN$", unzip(pjnz, list = TRUE)$Name, ignore.case = TRUE, value = TRUE)
  pjn <- readLines(unz(pjnz, pjn_file))
  reg_tag_line <- grep("^<Projection Parameters - Subnational Region Name2>", pjn)

  ## Note: This will not work correctly if there is an existing region name
  pjn[reg_tag_line+2] <- sub("^,,,", paste0(",,,", new_region_name), pjn[reg_tag_line+2])
  
  writeLines(pjn, pjn_file)
  zip(pjnz, pjn_file)
  file.remove(pjn_file)
  naomi::read_spectrum_region_name(pjnz)
}
  
write_pjn_region_name("demo_mwi2024_central.pjnz", "Central Region")
write_pjn_region_name("demo_mwi2024_northern.pjnz", "Northern Region")
write_pjn_region_name("demo_mwi2024_southern.pjnz", "Southern Region")

new <- sub(".pjnz", "_small.pjnz", raw)
file.copy(raw, new)

library(zip)

lapply(new,
       \(x) {
         f <- zip_list(x)$filename
         f_delete <- grep("DP$|PJN$|shiny90$", f, value = TRUE, invert = TRUE)
         utils::zip(x, f_delete, flags="-d")
       })

zip("demo_mwi2024_region-pjnz.zip", new)

