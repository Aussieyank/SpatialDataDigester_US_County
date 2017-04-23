##############################################
# Geospatial Data Digester Prototyped in R -- 
# A Shiny Project @ NYC Data Science Academy
# 
#
# Chao Shi
# chao.shi.datasci@gmail.com
# 4/23/2017
##############################################


# ======= final polishing wishlist during the weekend - brain dump ========

# done # final edit of ui.R and server.R

# --> # modify some column names in global.R, this would beautify 2 plots 1) pie chart 2) coor matrix plot

# --> # save data as .rda, separate global.R into dataPrep.R and global.R
#        -- in dataPrep.R,    read, clean and merge data sets
#        -- in global.R, load libraries (some require the devtool version)

# --> # readme.md   and     insight.md

# try upload to shinyapp.io

# 10-slide presentation file

# git to bootcamp project folder



# education data

# done #  !! a non-quantile color pallete is needed when plotting lving cost and Inc/Cost data alone (when filtering towards the high cost end)
# --> when the amount of data is very limited, breaking into quantiles doesn't make sense either
# --> maybe add a data size check, if smaller than a threshold, switch away from quantile color scale

# done # !! population is not normalized correctly (range overflow)  -> new york is a good place to check if this is fixed on population
# --> added a round(,2) step in global.R, to limit numeric values to num_decimal = 2

# done #    window title / tab title when oened in a browser

# TRIED, but... for some reason it didn't work for me # create popup on markers, turn on mouse hover for markers

# done #  add score to a column in tab 2 (data table, so users can sort wrt score, then drop markers)

# ON HOLD -- seems to need some GeoJSON code, not a quick since I'm only getting lat lon from mouse click
# I assume if I assign an ID to each polygon, that might be helpful, but I only have 1 day to finalize now 
# Desire: seems click on a county on the map, radar chart should show info about this county too

# done # for 1 variable case (for example politics), row filtering works now (bug fix)

# done # color bar, red on top, edit text <-- right now it seems hard to let leaflet legend plot bigger numbers on top
# the 'reverse' option in palletes flips color, but not the numbers
# therefore for this project I use 'colors' and 'label' to manually define the legend I want

# DONE #   check words on the sliders, units

#  categorical data example -- climate zone data


# > ?reactiveFileReader
# > ?reactivePoll
# > ?reactiveFileReader
# ======================================
# Spec
mypallete_spec_11 = c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')


mypallete_spec_10 = c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')
labels_spec_10    = c('90-100','80-90','70-80','60-70','50-60','40-50','30-40','20-30','10-20','0-10')


# RdBu
mypallete_RdBu_7 = c('#b2182b','#ef8a62','#fddbc7','#f7f7f7','#d1e5f0','#67a9cf','#2166ac')
mypallete_RdBu_11 = c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')


mypallete_RdBu_10 = c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')
labels_RdBu_10    = c('R led by 80-100%', 'R led by 60-80%', 'R led by 40-60%', 'R led by 20-40%', 'R led by 0-20%',
                      'D led by 0-20%', 'D led by 20-40%', 'D led by 40-60%', 'D led by 60-80%', 'D led by 80-100%')

