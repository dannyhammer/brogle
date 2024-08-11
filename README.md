# Brogle

Brogle (pronounced like 'broccoli') is a UCI-compatible chess engine.

This repository is a _huge_ work-in-progress.
Once the code reaches a state that I am content with, I will release a version 1.0.

## Layout

The [engine](brogle/) is where the source code for Brogle actually lives.
Inside you can find the components for UCI-compatibility, search, evaluation, and more.

The [core library](brogle_core/) is a home-rolled implementation of all the rules of chess.
I intend to separate this into it's own repository later and publish it on [crates.io](https://crates.io).
Why? Because I want to.

The [tools](brogle_tools/) crate is a collection of tools I used during the development of this engine.
These are mostly meant for me, so don't judge me if they're poorly-written or outdated.

## Features

-   Compatible with the [Universal Chess Interface](https://backscattering.de/chess/uci/) protocol
-   [Magic Bitboards](https://www.chessprogramming.org/Magic_Bitboards) for sliding piece movement (rook/bishop/queen)
-   Search

    -   [Negamax](https://www.chessprogramming.org/Negamax) with [alpha-beta pruning](https://www.chessprogramming.org/Alpha-Beta)
    -   [Iterative Deepening](https://www.chessprogramming.org/Iterative_Deepening) with [MVV-LVA](https://www.chessprogramming.org/MVV-LVA) [move ordering](https://www.chessprogramming.org/Move_Ordering)
    -   [Quiescence Search](https://www.chessprogramming.org/Quiescence_Search)

-   Evaluation
    -   [HCE](https://www.chessprogramming.org/Evaluation) (hand-crated evaluation)
    -   Basic [material](https://www.chessprogramming.org/Material) difference evaluation

More features to be implemented as I find time!

##

### Acknowledgements:

Special thanks to all

-   [Sebastian Lague](https://www.youtube.com/@SebastianLague), for his [chess programming series](https://www.youtube.com/watch?v=_vqlIPDR2TU&list=PLFt_AvWsXl0cvHyu32ajwh2qU1i6hl77c) on YouTube that ultimate inspired me to do this project.
-   The [Chess Programming Wiki](https://www.chessprogramming.org/), and all those who contribute to free, open-source knowledge.
-   The folks over at the [Engine Programming Discord](https://discord.com/invite/F6W6mMsTGN), for their patience with my silly questions and invaluable help overall.
-   [Analog-Hors](https://github.com/analog-hors), for her excellent [article on magic bitboards](https://analog-hors.github.io/site/magic-bitboards/)
