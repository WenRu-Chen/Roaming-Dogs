import pandas as pd 
from shapely.geometry import Point
import geopandas as gpd
import numpy as np
from collections import Counter

def Csv_to_Shp(dt,coor,crs_choise = {'init': 'epsg:3826'}): # 預設為TWD97 ，epsg:3826
    
    geom = [Point(xy) for xy in zip(dt[coor[0]], dt[coor[1]])]
    crs = crs_choise
    geodt = gpd.GeoDataFrame(dt, crs=crs, geometry=geom)
    
    return geodt

'''
常用的座標分類：
 
- WMS,WMTS等常使用的Web Mercator :EPSG:3857
- kml, geojson默認使用的WGS84:EPSG:4326 (x = '經度', y = "緯度")
- TWD97 121分帶:EPSG:3826
- TWD97 119分帶:EPSG:3825
- TWD67 121分帶(台灣舊系統):EPSG:3828
- TWD67 119分帶(台灣舊系統):EPSG:3827
'''

# Function for counting the point
# df_polygon.crs need to equal to df_point.crs

