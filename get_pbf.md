# get osm.pbf for oberbayern
-tutorial at 

- download file for Oberbayern
wget https://download.geofabrik.de/europe/germany/bayern-latest.osm.pbf

- cut the file with osmium-tool (sudo apt-get install osmium-tool)

- get bbox at https://boundingbox.klokantech.com/ (CSV format)

- cut with 
osmium extract --strategy complete_ways --bbox 10.5641,47.7834,12.5636,48.5855 bayern-latest.osm.pbf -o cropped.osm.pbf

- filter with

osmium tags-filter cropped.osm.pbf w/highway w/public_transport=platform w/railway=platform w/park_ride=yes r/type=restriction -o filtered.osm.pbf -f pbf,add_metadata=false
