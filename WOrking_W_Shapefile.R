library(GISTools)
library(rgdal)
hell <- readOGR(dsn="C:/lab/crimedata/sub/500x300_Sub", layer="Grid_Clipped")
hmerge <- merge(hell, hbz, by.x="id", by.y="id",
                all.x=T, all.y=T)
writeOGR(hmerge, dsn="C:/tmp", "Grid_Clipped", driver="ESRI Shapefile")
