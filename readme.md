# Animator of rebecaOS

Animator of the [Rebeca language](http://rebeca-lang.org/), as a possible entry point for newcomers to Rebeca and sandbox for new language experiments.

A compiled version of this project can be used at

 - https://fm-dcc.github.io/rebecaos

# Tutorial

A snapshot of this project with tag [v0.2](https://github.com/FM-DCC/rebecaos/releases/tag/v0.2) has been archived in Zenodo at https://zenodo.org/records/14947780.
This version in Zenodo has been extended with:
 - a detailed `readme.md` explaining how to compile and modify this code, and how to replicate examples from a published article;
 - a `Dockerfile` which can be used to recompile the code using Docker, without relying on JVM nor sbt.

# Publications

  - [BP25a] M. H. ter Beek, J. Proença, _Animating Rebeca_, in: E. A. Lee, M. R.
  Mousavi, C. Talcott (Eds.), Rebeca for Actor Analysis in Action, Vol. 15560
  of LNCS, Springer, 2025, pp. 182--194. https://doi.org/10.1007/978-3-031-85134-6_8
  - [PB25] J. Proença, M. H. ter Beek, _RebeCaos_, in: C. Di Giusto, A. Ravara
  (Eds.), Proceedings of the 27th IFIP WG 6.1 International Conference on
  Coordination Models and Languages (COORDINATION 2025), Vol. 15731 of LNCS,
  Springer, 2025, pp. 219--229. https://doi.org/10.1007/978-3-031-95589-1_11


# Caos

This project uses and the Caos's framework, placed at `lib/caos`. More information on it can be found online:

 - Caos' GitHub page: https://github.com/arcalab/CAOS
 - Caos' tutorial: https://arxiv.org/abs/2304.14901
 - Caos' demo video: https://youtu.be/Xcfn3zqpubw 

The project can also be included as a submodule, as explained in the documentation of Caos.

## Requirements

- JVM (>=1.8)
- sbt

## Compilation

You need to compile this project using the ScalaJS plug-in, following the steps below.
The result will be a JavaScript file that is already being imported by an existing HTML file. 

1. `sbt fastLinkJS`
2. open the file `lib/tool/index.html`
