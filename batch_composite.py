
import arcpy, os, glob, csv
from arcpy import env

# read csv for image names
csv_file = "O:\\PRIV\\NERL_ORD_CYAN\\Sentinel2\\Images\\composited\\0day_may_jul\\img_list.csv"
csv_reader = csv.reader(csv_file, delimiter=',')

with open(csv_file) as csvfile:
    data = list(csv.reader(csvfile))

# set workspace for image output
env.workspace = "O:\\PRIV\\NERL_ORD_CYAN\\Sentinel2\\Images\\composited\\0day_may_jul\\composited"

# perform composite for each image
for row in range(1,14):
    print("Processing #" + str(row) + " at " + datetime.datetime.now().strftime("%Y-%m-%d %H:%M"))
    
    # assign get granule dir and set workspace there
    folder = data[row][1]
    granule_id = data[row][2]
    granule_dir = "D:\\s2\\raw\\" + folder + ".SAFE\\GRANULE\\" + granule_id + "\\IMG_DATA"
    #arcpy.env.workspace = granule_dir

    # assign band names; convert to string for input
    b2 = ''.join(glob.glob(granule_dir + "\\*B02.jp2"))
    b3 = ''.join(glob.glob(granule_dir + "\\*B03.jp2"))
    b4 = ''.join(glob.glob(granule_dir + "\\*B04.jp2"))
    
    bands_str = [b2, b3, b4]
    in_rasts = '; '.join(bands_str)

    # composite
    print("Compositing image #" + str(row))
    arcpy.CompositeBands_management(in_rasts, granule_id + "_" + str(row) + ".tif")

    print("Done with image #" + str(row))

print("All done!!!")
