require(SPARQL)

#### EUROVOC ####

# Sparql request
endpoint <- "http://publications.europa.eu/webapi/rdf/sparql"
query <-
  "
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX ns3: <http://purl.org/dc/terms/>
  SELECT DISTINCT
  ?eurovoc_id
  ?eurovoc_label
  ?mt_id
  ?mt_label_id
  ?mt_label
  ?domain_id
  ?domain_label_id
  ?domain_label
  ?uf
  from <http://eurovoc.europa.eu/100141>
  where{
  ?eurovoc_id a skos:Concept .
  ?eurovoc_id skos:prefLabel ?eurovoc_label.
  ?eurovoc_id skos:inScheme ?mt_id .
  ?mt_id skos:prefLabel ?mt_label.
  ?mt_id ns3:identifier ?mt_label_id.
  ?mt_id ns3:subject ?domain_id.
  ?domain_id skos:prefLabel ?domain_label.
  ?domain_id ns3:identifier ?domain_label_id.
  ?eurovoc_id skos:altLabel ?uf.
  filter (str(?mt_id) != 'http://eurovoc.europa.eu/100141')
  filter (lang(?eurovoc_label) = 'en')
  filter (lang(?mt_label) = 'en')
  filter (lang(?domain_label) = 'en')
  filter (lang(?uf) = 'en')
  }"

eurovoc_sparql <- SPARQL::SPARQL(endpoint,query,format)$results

# clean ids
eurovoc_sparql$eurovoc_id <- gsub(".*/|>.*","",eurovoc_sparql$eurovoc_id)
eurovoc_sparql$mt_id <- gsub(".*/|>.*","",eurovoc_sparql$mt_id)
eurovoc_sparql$domain_id <- gsub(".*/|>.*","",eurovoc_sparql$domain_id)

# clean labels
eurovoc_sparql$eurovoc_label <- gsub("\"","",eurovoc_sparql$eurovoc_label)
eurovoc_sparql$eurovoc_label <- gsub("@en","",eurovoc_sparql$eurovoc_label)

eurovoc_sparql$mt_label <- gsub("\"","",eurovoc_sparql$mt_label)
eurovoc_sparql$mt_label <- gsub("@en","",eurovoc_sparql$mt_label)

eurovoc_sparql$domain_label <- gsub("\"","",eurovoc_sparql$domain_label)
eurovoc_sparql$domain_label <- gsub("@en","",eurovoc_sparql$domain_label)

eurovoc_sparql$uf <- gsub("\"","",eurovoc_sparql$uf)
eurovoc_sparql$uf <- gsub("@en","",eurovoc_sparql$uf)


save(eurovoc_sparql, file= "00_data/eurovoc_sparql.Rdata")