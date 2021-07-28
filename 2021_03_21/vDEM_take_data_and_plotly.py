import pandas as pd
import geopandas as gpd
import json
import plotly.express as px

df = pd.read_excel('DemocracyScores_lf.xlsx')

SHAPEFILE = 'geo_data/ne_110m_admin_0_countries.shp'
gdf = gpd.read_file(SHAPEFILE)
gdf = gdf[['ADMIN','geometry']]

gdf['ADMIN'].replace({'Myanmar':'Burma/Myanmar',
                      'Czechia':'Czech Republic',
                      'eSwatini':'Eswatini',
                      'Macedonia':'North Macedonia',
                      'Republic of Serbia':'Serbia',
                      'United Republic of Tanzania':'Tanzania',
                      'Gambia':'The Gambia',
                      'East Timor':'Timor-Leste'}, inplace = True)

gdf = gdf.merge(df, left_on = 'ADMIN', right_on = 'Country')
gdf.drop(['ADMIN'], axis=1, inplace=True)

gdf_2010 = gdf[gdf['Year'] == 2010]
json_2010 = gdf_2010.to_json()
json_2010 = json.loads(json_2010)

int_fig = px.choropleth_mapbox(
    mapbox_style='open-street-map',
    data_frame = gdf,
    geojson = json_2010,
    featureidkey = 'properties.Country',
    locations = 'Country',
    color = 'Democracy_Score',
    center = {"lat": 41.8719, "lon": 12.5674},
    zoom = 1,
    animation_frame='Year',
    animation_group='Country',   
    color_continuous_scale="RdBu",
    range_color = (0, 1),
    color_continuous_midpoint = .5,
    title = "vDEM Democracy Map of the World between 2010 - 2019")

int_fig.write_html("world_dem_map_interactive.html", include_plotlyjs='cdn')  