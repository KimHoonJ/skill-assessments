# Get the directory of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the script's directory
setwd(script_dir)

library(tidyverse)
library(DESeq2)
se <- readRDS("EwS.rds")

countMat <- assay(se)
rownames(countMat) <- gsub(rownames(countMat), pattern = "\\..+", replacement = "")
countMat <- countMat[rowSums(countMat != 0) > 0, ]
sampleInfo <- as.data.frame(colData(se))
dds <- DESeqDataSetFromMatrix( countData = countMat,
                               colData = sampleInfo,
                               design = ~ condition)

dds <- DESeq(dds)
rds <- rlog(dds)
plotPCA(rds)+
  theme(aspect.ratio=1/2)

res <- results(dds)
plotMA(res)
resNorm <- lfcShrink(dds=dds, res=res, type="normal", coef=2)
plotMA(resNorm, ylim=c(-10,10))
#resNorm2 <- lfcShrink(dds=dds, res=res, type="apeglm", coef=2)
#plotMA(resNorm2, ylim=c(-10,10))
#resNorm3 <- lfcShrink(dds=dds, res=res, type="ashr")
#plotMA(resNorm3, ylim=c(-10,10))


library(EnsDb.Hsapiens.v86)
library(AnnotationDbi)
anno <- AnnotationDbi::select(EnsDb.Hsapiens.v86, keys = keys(EnsDb.Hsapiens.v86),
                              columns = c("SYMBOL"))

colnames(anno)[1]<-"ENSEMBLE"
resFrame <- res %>% as.data.frame() %>%
  rownames_to_column("ENSEMBLE")
resFrame <- inner_join(anno, resFrame)

write.csv(resFrame, "results.csv")

library(EnhancedVolcano)
EnhancedVolcano(resFrame, lab=resFrame$SYMBOL, x="log2FoldChange", y="padj",
                pCutoff = 10e-5, FCcutoff=2)

topRes <- resFrame %>% arrange(padj)

topRes$padj[topRes$padj==0] = .Machine$double.xmin
# apparently extremely small value that is closest to 0 except 0.
posRes <- topRes[topRes$log2FoldChange>0,]
negRes <- topRes[topRes$log2FoldChange<=0, ]
posRes <- posRes[1:11, ]
# the top 11 had the same padj value (extremely close to 0 that R couldn't discern)
negRes <- negRes[1:10, ]

posRes <- arrange(posRes, desc(log2FoldChange))
posRes <- posRes[1:10, ]
# additional arrangement so that only take highets log2FoldChange among the top 11.

annotation <- as.data.frame(colData(rds)[, c("condition", "run")])
mat <- assay(rds)
id1 <- posRes$ENSEMBLE
id2 <- posRes$SYMBOL
topDE <- mat[id1,]
rownames(topDE) <- id2

library(pheatmap)
pheatmap(topDE, scale = "row", clustering_distance_rows = "correlation", 
         annotation_col = annotation, main="Top 10 Over Expressed Genes")

id3 <- negRes$ENSEMBLE
id4 <- negRes$SYMBOL
lowDE <- mat[id3, ]
rownames(lowDE) <- id4

pheatmap(lowDE, scale="row", clustering_distance_rows="correlation",
         annotation_col=annotation, main="Top 10 Under Expressed Genes")

topSymbol <- posRes$SYMBOL
botSymbol <- negRes$SYMBOL

write.csv(posRes, "topsymbol.csv")
write.csv(negRes, "botsymbol.csv")

library(enrichR)

websiteLive <- getOption("enrichR.live")
if (websiteLive) {
  listEnrichrSites()
  setEnrichrSite("Enrichr") # Human genes   
}

#if (websiteLive) dbs <- listEnrichrDbs()
#if (websiteLive) head(dbs)
dbs <- c("KEGG_2021_Human")

if (websiteLive) {
  enriched <- enrichr(topSymbol, dbs)
}

write.csv(enriched[[1]], "10OverEnrichR.csv")

if (websiteLive) {
  plotEnrich(enriched[[1]], showTerms = 20, numChar = 40, y = "Count", orderBy = "P.value", title="Top 10 Over-Expressed Genes\nEnrichment Analysis")
}

if (websiteLive){
  enriched2 <- enrichr(botSymbol, dbs)
}

if (websiteLive){
  plotEnrich(enriched2[[1]], showTerms=20, numChar=40, y="Count", orderBy="P.value", title="Top 10 Under-Expressed Genes\nEnrichment Analysis")
}

write.csv(enriched2[[1]], "10UnderEnrichR.csv")
