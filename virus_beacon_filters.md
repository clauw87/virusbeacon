# virus_beacon_filters

1. For query by variant response
query: variant descriptor> refseq:nt:ref:alt

* Variant in sample
- [ ] Mutation rate (categories: polymorphic_hypervariable vs conserved regions or ranges 0/0-0.03 /0.3-0.01/0.01-0.1_>0.3)

* Virus 
- [ ] taxon_id
- [ ] strain
- [ ] sequence length (nt)> all/>29000nt

* Biosample
- [ ] collection date
- [ ] sample type/source

* Host
- [ ] geo location
- [ ] ethnicity
- [ ] age
- [ ] sex
- [ ] disease course >asymptomatic, mild, severe, fatal 
- [ ] disease outcome
- [ ] clinical_findings



2. For query by annotation response (genomic regions)
query: genomic region> all, 3UTR, ORF1a, ..

query by variant filters and, additionally:

* Variant annotation 
- [ ] variant type: SNP, indel, structural variant
- [ ] variant effect: silent_synonymous_non-synonymous:missense variant, etc



4. Query by functional annotation  (positions based on functional annotation)
query: functional region/domain > structural protein, non-structural protein, accessory protein, receptor binding site (RDB), RNA methylation site (e.g AAGAAlike), Hypervariable Region 

query by variant filters and, additionally:

* Variant annotation 
- [ ] variant type: SNP, del, 
- [ ] variant effect: silent_synonymous_non-synonymous:missense variant



