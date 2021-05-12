# # Reading WFS from Open Data Berlin. See: https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/ for tutorial
# wfs_bwk = "https://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/s_lor_plr_2021"
# wfs = WFSClient$new(wfs_bwk, serviceVersion = "2.0.0")
# ogrinfo(wfs)
# lor = wfs$getFeatures("fis:s_lor_plr_2021") # fetching the dataset from the url
# lor <- lor %>% mutate(PLR_ID = str_pad(PLR_ID, width = 8, side = "left", pad = "0")) %>% 
#   arrange(PLR_ID)
