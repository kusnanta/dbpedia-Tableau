# dependency
library(SPARQL)
library(ggplot2)
library(ggrepel)
library(plotly)

endpoint <- "http://localhost:3030/pi/query"
query <-
  "
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX geo: <http://www.w3.org/2003/01/geo/wgs84_pos#>
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX dbp: <http://dbpedia.org/property/>
PREFIX dbr: <http://dbpedia.org/resource/>
PREFIX dbc: <http://dbpedia.org/resource/Category:>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX wil: <http://example.com/jumlahdanrasiodokterperawatterhadappuskesmas2015#>
PREFIX wi: <http://purl.org/ontology/wi/core#>
PREFIX db: <http://dbpedia.org/>

SELECT ?name ?puskes ?doktergigi ?dokterumum ?perawat ?bidan ?lat ?long ?area ?pop
{
  ?temp a wil:wilayah .
  ?temp owl:sameAs ?out .
  ?temp wil:label ?name .
  ?temp wil:jumlah_puskesmas ?puskes .
  ?temp wil:dokter_umum_puskesmas ?dokterumum .
  ?temp wil:dokter_gigi_puskesmas ?doktergigi .
  ?temp wil:perawat_puskesmas ?perawat .
  ?temp wil:bidan_puskesmas ?bidan .
  SERVICE <http://dbpedia.org/sparql>
  {SELECT * WHERE{
  ?out dbo:populationTotal ?pop .
  ?out dbp:areaTotalKm ?area .
  ?out geo:lat ?lat .
  ?out geo:long ?long .
  }}
}
"

# query from sparql endpoint
quedata <- SPARQL(endpoint,query)
# parsing result
puskesmas = quedata$results
df=puskesmas
puskesmasMatrix=as.matrix(cbind(df$puskes, df$pop),ncol=2)

# execute
fit <- kmeans(puskesmasMatrix, 5)
pus <- ggplot(data=df, aes(x=pop, y=puskes, color = factor(fit$cluster), label= puskesmas$name)) + geom_point() 
pus + geom_segment(aes(x = 0, y = 0, xend = 3600000, yend = 1200, colour = "segment"))
ggplotly(label= puskesmas$name)

# from csv
# puskesmas = read.csv("jumlahdanrasiodokterperawatterhadappuskesmas2015.csv")
# df=puskesmas
# puskesmasMatrix=as.matrix(cbind(df$Jumlah_Puskesmas2015, df$Dokter_Umum_puskesmas2015, df$Dokter_Gigi_puskesmas2015, df$Perawat_puskesmas2015, df$Bidan_puskesmas2015, df$Rasio_dokter_umum, df$Rasio_dokter_gigi, df$Rasio_perawat, df$Rasio_bidan),ncol=9)

# fit <- kmeans(puskesmasMatrix, 6)
# ggplot(puskesmas, aes(Jumlah_Puskesmas2015,Dokter_Umum_puskesmas2015, label= puskesmas$Wilayah)) + geom_point() + geom_text()
# ggplot(data=df, aes(x=Jumlah_Puskesmas2015, y=Dokter_Umum_puskesmas2015, color = factor(fit$cluster), label= puskesmas$Wilayah)) + geom_point() + geom_text_repel(aes(Jumlah_Puskesmas2015,Dokter_Umum_puskesmas2015, label= puskesmas$Wilayah))
# pus <- ggplot(data=df, aes(x=Jumlah_Puskesmas2015, y=Dokter_Umum_puskesmas2015, color = factor(fit$cluster), label= puskesmas$Wilayah)) + geom_point() + geom_text_repel(aes(Jumlah_Puskesmas2015,Dokter_Umum_puskesmas2015, label= puskesmas$Wilayah)) + scale_color_manual(values = c("purple", "black", "blue", "green", "yellow", "gray", "red" ))
# pus + geom_segment(aes(x = 0, y = 0, xend = 1000, yend = 1000, colour = "segment"))

# end of csv code