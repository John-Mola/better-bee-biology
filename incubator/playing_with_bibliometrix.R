# assumes 01a_bibtex_wrangling has been run in environment

results <- biblioAnalysis(df_bib_wrangled, sep = ";")
S <- summary(object = results, k = 10, pause = FALSE)
plot(x = results, k = 10, pause = FALSE)


#metaTagExtraction(results)
topAU <- authorProdOverTime(df_bib_wrangled, k = 10, graph = TRUE)

# NetMatrix <- biblioNetwork(df_bib_wrangled, analysis = "coupling", network = "authors", sep = ";")
# 
# net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)

NetMatrix <- biblioNetwork(df_bib_wrangled, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

CS <- conceptualStructure(df_bib_wrangled,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)
