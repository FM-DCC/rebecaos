# Animator of rebecaOS

Animator of the [Rebeca language](http://rebeca-lang.org/), as a possible entry point for newcomers to Rebeca and sandbox for new language experiments.

A compiled version of this project can be used at

 - https://fm-dcc.github.io/rebecaos


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
