# The Red Queen trap in science funding

Figure code and frozen, derived inputs for Gábor Várbíró's manuscript. This companion redraws six main and two supplementary figures. It is not a raw-data replication of the entire historical research programme. The plotting code and all seven frozen data files are byte-identical to those in Online Resource 2 (ESM_2.zip) accompanying the Higher Education submission, manuscript version 1.4.

From the repository root, run `Rscript make_figures.R` using R with Cairo graphics and an Arial or compatible sans-serif font. The script uses base R graphics and utilities, reads `data/` and writes 24 files (SVG, EPS and PNG for each figure) in `figures/`. Font rendering can differ by platform. Compare the results with the reference SVGs in `examples/`. Run `python verify_inputs.py` to compare all seven original CSV byte streams with their frozen SHA-256 hashes.

Figures 2–4 show NSF review counts and competition, MSCA qualification versus initial selection, and NIH/Taiwan decision or scoring patterns. Figure S1 shows the preserved NSF descriptive component and its earlier rank sensitivity; Figure S2 uses conditional portfolio-cost scenarios. Figures 1, 5 and 6 are conceptual. Figure 6 proposes mechanisms; no result here estimates the effect of funding refugia on research capacity. A funding rate is not an hours measure. Programme-level records do not stand for whole countries.

`source_manifest.csv` gives sources, locators and input hashes. Where a row has no primary URL, it is a previously computed sensitivity or model output, not an additional observation. See `CITATION.cff` for authorship and version. No journal acceptance, public DOI or repository release is asserted. Source data retain their original terms; see `RIGHTS.md`.



