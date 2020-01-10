
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eplusbuildr

[![Travis-CI Build
Status](https://travis-ci.com/hongyuanjia/eplusbuildr.svg?branch=master)](https://travis-ci.com/hongyuanjia/eplusbuildr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/hongyuanjia/eplusbuildr?branch=master&svg=true)](https://ci.appveyor.com/project/hongyuanjia/eplusbuildr)
[![codecov](https://codecov.io/gh/hongyuanjia/eplusbuildr/branch/master/graph/badge.svg)](https://codecov.io/gh/hongyuanjia/eplusbuildr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/eplusbuildr)](https://cran.r-project.org/package=eplusbuildr)
[![CRAN
Checks](https://cranchecks.info/badges/summary/eplusbuildr)](https://cranchecks.info/pkgs/eplusbuildr)
[![CRAN Download
Badge](https://cranlogs.r-pkg.org/badges/eplusbuildr)](https://cran.r-project.org/package=eplusbuildr)

> A collection of useful functions to work with eplusr.

## Installation

Currently eplusbuidr is not on CRAN yet. You can install the development
version from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("hongyuanjia/eplusbuildr")
```

## Get started

``` r
library(eplusr)
library(eplusbuildr)

# read model
idf <- read_idf(file.path(eplus_config(8.8)$dir, "ExampleFiles", "RefBldgLargeHotelNew2004_Chicago.idf"))
#> IDD v8.8.0 has not been parsed before.
#> Try to locate `Energy+.idd` in EnergyPlus v8.8.0 installation folder '/usr/local/EnergyPlus-8-8-0'.
#> IDD file found: '/home/hongyuanjia/.local/EnergyPlus-8-8-0/Energy+.idd'.
#> Start parsing...
#> Parsing completed.

# Remove not-used objects
purge_object(idf, "Material")
#> Object(s) below has been purged:
#>  #1| Object 'Wood Siding' [ID: 55] in class 'Material'
#>  #2| Object 'Metal Siding' [ID: 59] in class 'Material'
#>  #3| Object 'Metal Roofing' [ID: 64] in class 'Material'
#>  #4| Object 'Std AC02' [ID: 66] in class 'Material'

# Merge same objects into one
merge_object(idf, "Schedule:Compact")
#> Objects in class 'Schedule:Compact' below have been merged into object 'Kitchen_Exhaust_SCH' [ID: 428]:
#>  #1| Object 'MinOA_Sched' [ID: 796]
#> 
#> Objects in class 'Schedule:Compact' below have been merged into object 'Guest Room Water Equipment Hot Supply Temp Sched' [ID: 986]:
#>  #1| Object 'Guest Room Water Equipment Temp Sched' [ID: 987]
#> 
#> Objects in class 'Schedule:Compact' below have been merged into object 'Kitchen Water Equipment Hot Supply Temp Sched' [ID: 988]:
#>  #1| Object 'Kitchen Water Equipment Temp Sched' [ID: 989]
#> 
#> Objects in class 'Schedule:Compact' below have been merged into object 'Laundry Water Equipment Hot Supply Temp Sched' [ID: 990]:
#>  #1| Object 'Laundry Water Equipment Temp Sched' [ID: 991]
#>  #2| Object 'SWHSys1 Water Heater Setpoint Temperature Schedule Name' [ID: 993]
#>  #3| Object 'SWHSys1-Loop-Temp-Schedule' [ID: 994]

# Rename objects using function
# purrr-like function definition supported
idf$object_name("ZoneHVAC:FourPipeFanCoil")
#> $`ZoneHVAC:FourPipeFanCoil`
#> [1] "Room_1_Flr_3 fan coil"        "Room_1_Flr_6 fan coil"       
#> [3] "Room_2_Flr_3 fan coil"        "Room_2_Flr_6 fan coil"       
#> [5] "Room_3_Mult19_Flr_3 fan coil" "Room_3_Mult9_Flr_6 fan coil" 
#> [7] "Room_4_Mult19_Flr_3 fan coil" "Room_5_Flr_3 fan coil"       
#> [9] "Room_6_Flr_3 fan coil"

rename_per_fun(idf, "ZoneHVAC:FourPipeFanCoil",
    ~gsub("fan coil", "four-pipe fan coil", .x)
)

idf$object_name("ZoneHVAC:FourPipeFanCoil")
#> $`ZoneHVAC:FourPipeFanCoil`
#> [1] "Room_1_Flr_3 four-pipe fan coil"       
#> [2] "Room_1_Flr_6 four-pipe fan coil"       
#> [3] "Room_2_Flr_3 four-pipe fan coil"       
#> [4] "Room_2_Flr_6 four-pipe fan coil"       
#> [5] "Room_3_Mult19_Flr_3 four-pipe fan coil"
#> [6] "Room_3_Mult9_Flr_6 four-pipe fan coil" 
#> [7] "Room_4_Mult19_Flr_3 four-pipe fan coil"
#> [8] "Room_5_Flr_3 four-pipe fan coil"       
#> [9] "Room_6_Flr_3 four-pipe fan coil"

# Rename objects based on class and fielf values
idf$object_name("People")
#> $People
#>  [1] "Banquet_Flr_6 People"       "Basement People"           
#>  [3] "Cafe_Flr_1 People"          "Corridor_Flr_3 People"     
#>  [5] "Corridor_Flr_6 People"      "Dining_Flr_6 People"       
#>  [7] "Kitchen_Flr_6 People"       "Laundry_Flr_1 People"      
#>  [9] "Lobby_Flr_1 People"         "Retail_1_Flr_1 People"     
#> [11] "Retail_2_Flr_1 People"      "Room_1_Flr_3 People"       
#> [13] "Room_1_Flr_6 People"        "Room_2_Flr_3 People"       
#> [15] "Room_2_Flr_6 People"        "Room_3_Mult19_Flr_3 People"
#> [17] "Room_3_Mult9_Flr_6 People"  "Room_4_Mult19_Flr_3 People"
#> [19] "Room_5_Flr_3 People"        "Room_6_Flr_3 People"       
#> [21] "Storage_Flr_1 People"

rename_per_field(idf, "People", "Zone or ZoneList Name")

idf$object_name("People")
#> $People
#>  [1] "People-Banquet_Flr_6"       "People-Basement"           
#>  [3] "People-Cafe_Flr_1"          "People-Corridor_Flr_3"     
#>  [5] "People-Corridor_Flr_6"      "People-Dining_Flr_6"       
#>  [7] "People-Kitchen_Flr_6"       "People-Laundry_Flr_1"      
#>  [9] "People-Lobby_Flr_1"         "People-Retail_1_Flr_1"     
#> [11] "People-Retail_2_Flr_1"      "People-Room_1_Flr_3"       
#> [13] "People-Room_1_Flr_6"        "People-Room_2_Flr_3"       
#> [15] "People-Room_2_Flr_6"        "People-Room_3_Mult19_Flr_3"
#> [17] "People-Room_3_Mult9_Flr_6"  "People-Room_4_Mult19_Flr_3"
#> [19] "People-Room_5_Flr_3"        "People-Room_6_Flr_3"       
#> [21] "People-Storage_Flr_1"
```

## Author

Hongyuan Jia

## License

The project is released under the terms of MIT License.

Copyright © 2019 Hongyuan Jia
