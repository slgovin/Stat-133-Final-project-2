# =========================================================
# Stat133: Final Project - Skeleton
# Names: Hanna Haddad and Swati Govindaraju
# SIDs: 22922603 and 22380774
# Date: 08/14/15
# Description: Directory Set Up and File Organization
# Data: NA
# =========================================================

# ============================================
# Create Folders and Rearrange Files
# ============================================

# create the folders needed
dir.create('code')
dir.create('rawdata')
dir.create('data')
dir.create('resources')
dir.create('report')
dir.create('images')

# move the existing files into the correct folders
file.rename(from = 'cleaning.R', to = 'code/cleaning.R')
file.rename(from = 'analysis.R', to = 'code/analysis.R')
file.rename(from = 'visualization.R', to = 'code/visualization.R')
file.rename(from = 'report.Rmd', to = 'report/report.Rmd')
