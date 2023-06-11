# load library
library(data.table)

# custom function
matlab2POS = function(x, timez = "UTC") {
  days = x - 719529 	# 719529 = days from 1-1-0000 to 1-1-1970
  secs = days * 86400 # 86400 seconds in a day
  # This next string of functions is a complete disaster, but it works.
  # It tries to outsmart R by converting the secs value to a POSIXct value
  # in the UTC time zone, then converts that to a time/date string that 
  # should lose the time zone, and then it performs a second as.POSIXct()
  # conversion on the time/date string to get a POSIXct value in the user's 
  # specified timezone. Time zones are a goddamned nightmare.
  return(as.POSIXct(strftime(as.POSIXct(secs, origin = '1970-1-1', 
                                        tz = 'UTC'), format = '%Y-%m-%d %H:%M:%S', 
                             tz = 'UTC', usetz = FALSE), tz = timez))
}

# load data
dt_tdr = fread("../seal_predator_avoidance_strategy/data/2010023_iknos_raw_data.csv")
dt_dr = readRDS("../seal_predator_avoidance_strategy/export/data_dive.rds")

# convert dt_dr into data.table
setDT(dt_dr)

# keep some column + format date
dt_tdr = dt_tdr[,.(
  id = 2010023,
  depth = CorrectedDepth,
  datetime = matlab2POS(time)
)]

# filter and rename
dt_dr = dt_dr[id == 2010023, .(
  id,
  datetime = date, 
  dive_number = DiveNumber,
  dive_type = DiveTypeName
)]

# setkey before merge
setkey(dt_tdr, id, datetime)
setkey(dt_dr, id, datetime)

# merge
dt = dt_dr[dt_tdr, roll = T]

# remove the beginning
dt = dt[!is.na(dive_number),]

# plot
n = 1600
dt[dive_number >= n & dive_number < n + 10,] 
  data_tdr %>% 
  ggplot(aes(x = datetime, 
             y = depth,
             group = id))+
  geom_path(aes(col = dive_type)) +
  # geom_path(col = "black") +
  # everything below is optional
  scale_y_reverse(name = "Depth (m)") +
  scale_x_datetime(name = "Date", 
                   date_breaks = "20 mins",
                   date_labels = "%H:%M",
                   position = "top") +
  labs(col = "Dive Type") +
  # choose a theme
  theme_classic() +
  # and tweak it
  theme(
    # legend position
    legend.position = "top",
    # add arrow for x axis
    axis.line.x = element_line(arrow = arrow(
      ends = "last",
      type = "closed",
      length = unit(0.25, "npc"))),
    # add arrow for y axis
    axis.line.y = element_line(arrow = arrow(
      ends = "first",
      type = "closed",
      length = unit(0.25, "npc"))))

# export
saveRDS(
  as.data.frame(
    dt[dive_number >= n & dive_number < n + 10,] %>% 
      .[, dive_number := dive_number - 1599]),
        "./export/data_tdr.rds")
