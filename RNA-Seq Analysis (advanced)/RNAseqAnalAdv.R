# Get the directory of the current script
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# Set the working directory to the script's directory
setwd(script_dir)

library(recount)
library(tidyverse)

project_info <- abstract_search(query="glioma")

#SRP064317
#' This study compared control glioma initiating cells(GIC)
#' GICs seem to play a critical role in glioblastoma.
#' 
#' Over expression of KDM1B delays the cell growth under hypoxia.
#' knocking down of KDM1B promotes survival and tumorgenic abilities.
#' I have not read the paper yet (before analysis)
#' I would like to discover genes associated with KDM1B and how they
#' may be potentially beneficial/detrimental to tumorgenisis.


study <- download_study("SRP064317")

load("SRP064317/rse_gene.Rdata")

se <- rse_gene

rowname <- rownames(se)
rownames(se) <- gsub(rowname, pattern = "\\..+", replacement = "")

mat <- as.data.frame(assay(se))
mat <- rownames_to_column(mat, "ENSEMBL")
mat <- mat[!duplicated(mat$ENSEMBL), ]
mat <- column_to_rownames(mat, "ENSEMBL")

se$condition <- c(rep("control",3), rep("shKDM1B",3))
cold <- as.data.frame(colData(se))
library(DESeq2)

dds <- DESeqDataSetFromMatrix(countData= mat, colData = cold, design = ~ condition)

dds <- DESeq(dds)


res <- results(dds)

DEScsv <- as.data.frame(res) %>%
  rownames_to_column("GENEID")
write_csv(DEScsv,"DESeq2_results.csv")

#resNorm1 <- lfcShrink(dds=dds, res=res, type="normal", coef=2)
resNorm2 <- lfcShrink(dds=dds, res=res, type="apeglm", coef=2)
#resNorm3 <- lfcShrink(dds=dds, res=res, type="ashr", contrast=c("condition", "shKDM1B", "control"))

DESeq2::plotMA(res, ylim=c(-30,30))
DESeq2::plotMA(resNorm2, ylim=c(-7,7))

rds <- rlog(dds)
plotPCA(rds)


library(AnnotationDbi)
library(org.Hs.eg.db)
library(EnsDb.Hsapiens.v86)

anno <- AnnotationDbi::select(EnsDb.Hsapiens.v86, keys = keys(EnsDb.Hsapiens.v86),
                                 columns = c("SYMBOL"))

resTable <- as.data.frame(resNorm2)
resTable <- rownames_to_column(resTable, "GENEID")
resTable <- inner_join(anno, resTable)


library(EnhancedVolcano)

EnhancedVolcano(resTable, lab=resTable$SYMBOL, x="log2FoldChange", y="padj")

library(pheatmap)


TOres <- resTable %>% 
  arrange(padj) %>% 
  dplyr::filter(log2FoldChange > 2) %>%
  dplyr::filter(baseMean > 20) %>%
  dplyr::filter(padj<0.0001)
OSigres <- TOres
TOres <- TOres[1:10, ]


TUres <- resTable %>%
  dplyr::filter(log2FoldChange < -2) %>%
  arrange(padj) %>%
  dplyr::filter(baseMean > 20) %>%
  dplyr::filter(padj<0.0001)

USigres <- TUres
TUres <- TUres[1:10, ]

Sigres <- resTable %>%
  dplyr::filter(abs(log2FoldChange)>2) %>%
  arrange(padj) %>%
  dplyr::filter(baseMean > 20) %>%
  dplyr::filter(padj<0.0001)

pmat <- as.data.frame(assay(rds))
TOgenes <- TOres$GENEID
TOmat <- pmat[TOgenes,]
annotation <- as.data.frame(colData(rds)[,c("condition", "sample")])
pheatmap(mat=TOmat, scale="row",
         clustering_distance_rows="correlation", 
         annotation_col=annotation,
         labels_row = TOres$SYMBOL,
         main="Top 10 Over-Expressed Genes")
TOres


TUgenes <- TUres$GENEID
TUmat <- pmat[TUgenes, ]
pheatmap(mat=TUmat, scale="row",
         clustering_distance_rows="correlation",
         annotation_col=annotation,
         labels_row = TUres$SYMBOL,
         main="Top 10 Under-Expressed Genes")


#' Performing GSEA on under-expressed genes
library(msigdbr)
library(clusterProfiler)
gene_sets <- msigdbr(species = "Homo sapiens", category = "C2")
gene_sets <- gene_sets %>%
  dplyr::select(gs_name, gene_symbol)


USYMBOL<- USigres$SYMBOL
OSYMBOL <- OSigres$SYMBOL
SSYMBOL <- Sigres$SYMBOL

egmt <- enricher(gene = USYMBOL,
                 TERM2GENE = gene_sets)

edf <- as.data.frame(egmt)

# Plot results with clusterProfiler
dotplot(egmt)
#barplot(egmt)


resTable2 <- as.data.frame(res) %>%
  drop_na() %>%
  rownames_to_column("GENEID") %>%
  inner_join(anno) %>%
  arrange(desc(stat))

# Don't need to deal with INF values.
#resTable2 <- resTable2 %>%
#  mutate(padj = case_when(padj == 0 ~ .Machine$double.xmin,
#                          TRUE ~ padj)) %>%
#  mutate(gsea_metric = -log10(padj) * sign(log2FoldChange))
ranks <- resTable2 %>%
  select(SYMBOL, stat) %>%
  distinct(SYMBOL, .keep_all = TRUE) %>%
  deframe()


gseares <- GSEA(geneList = ranks, 
                TERM2GENE = gene_sets,
                pvalueCutoff=0.05)

gsearesdf <- as.data.frame(gseares)
gseaplot(gseares, geneSetID = gsearesdf[1,1],
         title = gsearesdf[1,1])

gseaplot(gseares, geneSetID = gsearesdf[2,1],
         title = gsearesdf[2,1])

gseaplot(gseares, geneSetID = gsearesdf[3,1],
         title = gsearesdf[3,1])

gseaplot(gseares, geneSetID = gsearesdf[4,1],
         title = gsearesdf[4,1])

gseaplot(gseares, geneSetID = gsearesdf[5,1],
         title = gsearesdf[5,1])
