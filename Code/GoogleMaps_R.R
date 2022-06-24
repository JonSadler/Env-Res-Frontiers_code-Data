# GOOGLE MAPS API

# KEY
# AIzaSyC5yX39DNG6L_L4ATWEecBjnohC_MRMmaw

# this sets your google map for this session
register_google(key = "[AIzaSyC5yX39DNG6L_L4ATWEecBjnohC_MRMmaw]")

# Register permanently
register_google(key = "[AIzaSyC5yX39DNG6L_L4ATWEecBjnohC_MRMmaw]", write = TRUE)

qmap('Birmingham')

map <- get_map(c("Birmingham, London"))
ggmap(map)
