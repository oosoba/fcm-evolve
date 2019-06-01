FCM Coding - JDMS simulations
-----------------------------
- Using al Qaeda specialization of PSOT model (aom-pkd2013:Fig2.4, pkd-el2012:FigS.2)
- PSOT models: 
	- PSOT v0: Pure factor tree model, no cross links
	- PSOT v1: cross links
	- PSOT v2: cross links, PSOT self-excitation, explicit L->R temporal link

- hsucc, prsk: largest jump in betweenness centrality from v0->v2:
- MOTV, PLEG: top betw cent in v2
- ACR: top betw in v0
- ACR: highest eigv-centrality in both v0,v2

## Record of Examples
> Eg #1: {"lead", "pkg", "opp", "pres", "MOTV", "intl", "cprop", "desp", "PLEG", "intm", "lvic", "id", "mgtc"}

> Eg #2: {"lead", "pkg", "opp", "pres", "MOTV", "intl", "rvng", "desp", "intm", "lvic", "id", "mgtc", "hfail"} - {"prsk", "scst"} (linactvn needs 16-cprop to tip over)

> Eg #3: {"lead", "pkg", "pres", "MOTV", "intl", "rvng", "desp", "intm", "lvic", "id", "mgtc", "prop", "hfail"} - {"rsrc", "opp", "prsk", "scst"}


