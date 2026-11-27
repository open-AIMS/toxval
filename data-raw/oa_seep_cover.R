# Source: Australian Institute of Marine Science data record
# https://apps.aims.gov.au/metadata/view/76c40b3c-8535-43b6-bf5f-ad26d6e1ad92
# Downloaded 2026-10-23. Licence: Creative Commons Attribution 3.0 Australia.

source_data <- utils::read.csv("data-raw/Benthic_cover.csv")
columns <- c(
  "Reef",
  "OmAr.mean",
  "OmAr.msem",
  "pH.mean",
  "ACROPORA",
  "CRUSTOSE.CORALLINE.ALGAE"
)
oa_seep_cover <- source_data[columns]
dir.create("data", showWarnings = FALSE)
save(oa_seep_cover, file = "data/oa_seep_cover.rda", compress = "xz")
