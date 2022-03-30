import pandas as pd 
from shapely.geometry import Point
import geopandas as gpd
import numpy as np
from collections import Counter

def HowManyPoint(df_polygon, df_point,x):  # x: gathering 的單位（里、區......）
    if df_polygon.crs != df_point.crs:
        df_polygon = df_polygon.to_crs(df_point.crs)
        
    aggr = gpd.sjoin(left_df=df_point ,
                right_df=df_polygon,
                op='intersects') \
            .groupby(x) \
            .size()
   
    return aggr

def HowMuch(df_polygon, df_point,x):  # x: gathering 的單位（里、區......）
    if df_polygon.crs != df_point.crs:
        df_polygon = df_polygon.to_crs(df_point.crs)
        
    aggr = gpd.sjoin(left_df=df_point ,
                right_df=df_polygon,
                op='intersects') \
            .groupby(x) \
            .sum()
    return aggr