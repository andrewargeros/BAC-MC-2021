# -*- coding: utf-8 -*-
import geopandas
import rtree
import requests
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from sodapy import Socrata
from shapely.geometry import shape

nyc = geopandas.read_file('https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Community_Districts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson')

zoning = geopandas.read_file('https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nyzd/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson')

zoning.head()

residential_zones = [zone for zone in zoning.ZONEDIST.unique().tolist() 
                     if (zone.find(r'R') != -1 or zone.find('C') != -1) and zone not in ['PLAYGROUND', 'PARK', 'PUBLIC PLACE', 'C7', 'C8']]

residential_zones

joined_files = geopandas.sjoin(zoning, nyc, how="left", op = "intersects")

livable_area = joined_files[joined_files['ZONEDIST'].isin(residential_zones)].groupby('BoroCD').sum()[["Shape__Area_left", "Shape__Area_right"]]

livable_area.head()

livable_area["pct_livable"] = livable_area['Shape__Area_left']/livable_area['Shape__Area_right']
livable_area['l_pct_livable'] = np.log(livable_area.pct_livable)
livable_area.to_csv('test.csv')

livable_area

average(livable_area.pct_livable)

ny2 = nyc.merge(livable_area, on="BoroCD")

joined_files['residential'] = joined_files['ZONEDIST'].isin(residential_zones)

joined_files

zoning['ZONEDIST'].isin(residential_zones)

ny2.plot(column = "l_pct_livable")

zcta = geopandas.read_file('/content/nyu-2451-34509-geojson.json')
zcta_boro = geopandas.sjoin(zcta, nyc, how="right", op = "intersects")

covid = pd.read_csv('https://raw.githubusercontent.com/nychealth/coronavirus-data/master/totals/data-by-modzcta.csv')

covid['zctaa'] = covid.MODIFIED_ZCTA

zcta_boro.to_file("zcta_borocd_merged.geojson", driver='GeoJSON')

zcta_boro['zctaa'] = zcta_boro.zcta.astype('int64')

f, ax = plt.subplots(1)
zcta_boro.merge(covid, on = 'zctaa').plot(column = 'COVID_CASE_COUNT', 
                                          cmap = "Reds", 
                                          scheme='naturalbreaks',
                                          linewidth=0.5, edgecolor='0.2',
                                          figsize=(8, 6))
ax.set_axis_off()
plt.axis('equal')
fig1 = plt.gcf()
plt.show()
plt.draw()
fig1.savefig('tessstttyyy.png', dpi=100,  bbox_inches='tight', tight_layout=True, pad_inches=0, frameon=None)

zcta_boro.merge(covid, on = 'zctaa').to_file("zcta_borocd_merged2.geojson", driver='GeoJSON')

response = requests.get('https://data.cityofnewyork.us/resource/viz9-mrjz.json', params = {'limit': '2000000', 'app_token': 'pJ5vqJnhqF11A9ZSKQwuJJYO7'})

data = response.json()
for d in data:
    d['the_geom'] = shape(d['the_geom'])

gdf = geopandas.GeoDataFrame(data).set_geometry('the_geom')
gdf.head()

client = Socrata("data.cityofnewyork.us", 'pJ5vqJnhqF11A9ZSKQwuJJYO7')

results = client.get("viz9-mrjz", limit=1700000)

# Convert to pandas DataFrame
results_df = pd.DataFrame.from_records(results)

for d in results:
    d['the_geom'] = shape(d['the_geom'])

gdf = geopandas.GeoDataFrame(results).set_geometry('the_geom')
gdf.head()

gdf.head()

gdf['shape_volume'] = gdf.shape_area.astype(float)*gdf.heightroof.astype(float)

buildings = geopandas.sjoin(joined_files.drop('index_left', axis = 1), gdf, how="left", op = "intersects")

buildings