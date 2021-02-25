import geopandas
import pandas as pd
import rtree

nyc = geopandas.read_file('https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Community_Districts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson')
zoning = geopandas.read_file('https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nyzd/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson')

residential_zones = [zone for zone in zoning.ZONEDIST.unique().tolist() 
                     if (zone.find(r'R') != -1 or zone.find('C') != -1) # People can live in either R or C zones
                     and zone not in ['PLAYGROUND', 'PARK', 'PUBLIC PLACE', 'C7', 'C8', 'BPC']] # filter out parks, prisons and C7/8 where living is prohibited

joined_files = geopandas.sjoin(zoning, nyc, how="right", op = "intersects") # change to intersects to be more correct

livable_area = joined_files[joined_files['ZONEDIST'].isin(residential_zones)].groupby('BoroCD').sum()[["Shape__Area_x", "Shape__Area_y"]]

livable_area["pct_livable"] = livable_area['Shape__Area_x']/livable_area['Shape__Area_y']
