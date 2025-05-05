source("scripts/utils.R")

# Fetch from folder https://drive.google.com/drive/folders/1u1nIiBlMtTsP_-u6wzd6Rbs1d_q4WEW1
# IMERSS Research Projects/Community Research Projects/2021-2025 - Janszen Legacy Project/Datasets

downloadGdriveFolder("1u1nIiBlMtTsP_-u6wzd6Rbs1d_q4WEW1", "tabular_data/Catalogues", skip_if_exists = FALSE)
