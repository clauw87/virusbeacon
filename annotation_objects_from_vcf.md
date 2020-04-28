# annotation_objects_from_vcf
1. Annotation tool and Version will go to Variant annotation endpoint object **annotationToolVersion**  (pending, Dietmar will provide in next iteration)

2. From EFF>
- [ ] the field  **Effect** goes to the Variant Annotation endpoint object **moleculareffect**  (it contains the info of effect of variant at nucleotide level i.e “SYNONYMOUS_CODING”, “NON-SYNONYMOUS_CODING”, “STOP_GAINED”)
- [ ] the field **Functional_Class** goes to the Variant annotation endpoint object **molecularConsequence**  (it contains the info of consequence at protein level of variant “silent”, “missense variant”, “nonsense”), 
- [ ] the field **Amino_Acid_Change** goes to the Variant annotation endpoint object **aminoacidChange**
- [ ] genomicRegion  is a compound object which contains a list of genomic regions affected by the variant (their class and their name/id), for example in these vcf we find annotation of genes and transcripts, these are two genomic region classes, and they have a class and an actual id
- [ ] the field **Gene_Name** goes to the Variant annotation endpoint  objects **genomicRegion featureClass** (the tag “gene”) and **genomicRegion featureID** (actual value in the field) (the values here are names such as “orf1ab”, this needs to be mapped to NCBI refseqs for beacon endpoint, although this names are right-away usable as alias for users). 
- [ ] the field **Transcript_ID** goes to the Variant annotation endpoint  objects **genomicRegion  featureID** as well (appended to list) See example below
- [ ] the field **Transcript_BioType** goes to the schema object **genomicRegion featureClass**as well (appended to list), in the form “transcript”: value in the field. See example below

This would be an example from real life:
Table from dietmar:
variant | Gene_Name| Transcript_BioType| Gene_Coding | Transcript_id
171:C>T |  Orf1ab | protein_coding| CODING | GU280_gp01

This would go to two elements in list, each with a class  and a featureId
genomicRegion: 
      class: gene
	featureId: Orf1ab
genomicRegion: 
      class: transcript: protein coding
	featureId: GU280_gp01


- [ ] Note:  I guess the noncoding will have only info in Gene_Coding but there aren’t any non-coding in this vcf; if it has Gene_Coding=coding, we are interested also in Transcript_BioType.If f Gene_Coding is set to something else, i.e. non-coding stuff, I guess Gene_Coding will then contain the info on whether info about it being untranslated region or intergenic, etc)

genomic regions classes I expect, but I have to check SnpEff specification: 
non-coding (intergenic, intronic, upstream)
coding (transcript: protein coding , transcript: ncRNA, transcript: miRNA)

2. From LOF and NMD> Not for now. These would go together in one single column if anything, that could be named predictedEffect  (bit it’s not yet in schema, so don’t use for now)