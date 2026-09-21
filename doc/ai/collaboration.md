# Human–AI collaboration and scientific ownership

Existing audits, golden tests and documentation remain useful evidence for the properties they examine. Separate three questions: does the implementation follow its specification; can the computation be repeated from declared inputs; and is that specification scientifically appropriate? None implies the other two.

| AI fluency competency | Observable practice | Evidence left behind |
|---|---|---|
| Delegation | State task scope; humans own scientific specifications. Continue routine authorized work. Use specialist agents only for explicitly requested independent work. | Task brief and named decision owner |
| Description | Identify input contracts, estimand, sample, year, geography, missingness, weights and acceptance criteria before changing methods. | Method/specification link and handoff |
| Discernment | Challenge outputs with independent calculations, boundary cases and revision-matched numerical comparisons. Review underlying plot data and rendering. | Tests, comparisons, unresolved counterexamples |
| Diligence | Preserve source data; record provenance, review status, limitations and appropriate AI assistance disclosure. | Run report and evidence index |

These practices adapt the [AI Fluency Framework](https://aifluencyframework.org/). The framework describes responsible collaboration; it does not certify a scientific result. [Sandve et al.](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003285) motivate traceable computational steps and preserved input versions. [Wilson et al.](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005510) motivate practical organization, automation and verification. [DCAS](https://datacodestandard.org/) and [AEA guidance](https://www.aeaweb.org/journals/data/data-code-policy) inform the data-access and replication-package checklist. The repository has not received certification from these bodies.

Before an analytical change, distinguish a human-approved specification from an assistant's proposal. Ask only when an unresolved choice changes methods, source data or task scope. Preserve city-source confirmation and processing-script integration boundaries in the workflows. An audit is report-only until implementation is requested. Historical implementation authorization is not standing authorization for future tasks. Never infer that a past audit finding is fixed merely because today's code looks different.

A handoff is concise, factual and free of credentials/restricted records. All harnesses update these same documents. Native wrappers are routing metadata, not separate copies of the project policy. Record accepted methodological choices in evidence.md; leave unresolved model suggestions open. Recommend only reviewed, sanitized audit excerpts for a human-created commit. Follow [Git safety](rules/git-safety.md); implementation approval does not authorize staging, committing, or pushing. Local exploratory reports stay under ignored doc/audits/.
