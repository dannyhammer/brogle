# **Deprecation**

This repo was my first foray into chess programming, and it taught me a lot!
I've since split apart the various pieces here into their own repositories.
The primary ones I'd have you visit are [`chessie`](https://github.com/dannyhammer/chessie), my chess move generation library, and [`toad`](https://github.com/dannyhammer/toad), the engine that succeeded `brogle`.

I'm leaving this repository up for now, but may remove it in the future.

# Brogle

Brogle (pronounced 'brog-lee' (like 'broccoli')) is a UCI-compatible chess engine.

This repository is a _huge_ work-in-progress.
Once the code reaches a state that I am content with, I will release a version 1.0.

## Layout

The [engine](brogle/) is where the source code for Brogle actually lives.
Inside you can find the components for UCI-compatibility, search, evaluation, and more.

The [core library](chessie/) is a home-rolled implementation of all the rules of chess.
I intend to separate this into it's own repository later and publish it on [crates.io](https://crates.io).

The [tools](brogle_tools/) crate is a collection of tools I used during the development of this engine.
These are mostly meant for me, so don't judge me if they're poorly-written or outdated.

## Features

-   Compatible with the [Universal Chess Interface](https://backscattering.de/chess/uci/) protocol
-   [Magic Bitboards](https://www.chessprogramming.org/Magic_Bitboards) for sliding piece movement (rook/bishop/queen)
-   Search

    -   [Negamax](https://www.chessprogramming.org/Negamax) with [alpha-beta pruning](https://www.chessprogramming.org/Alpha-Beta)
    -   [Principal Variation Search](https://www.chessprogramming.org/Principal_Variation_Search)
    -   [Iterative Deepening](https://www.chessprogramming.org/Iterative_Deepening)
    -   [MVV-LVA](https://www.chessprogramming.org/MVV-LVA) [move ordering](https://www.chessprogramming.org/Move_Ordering)
    -   [Quiescence Search](https://www.chessprogramming.org/Quiescence_Search)
    -   [Transposition Table](https://www.chessprogramming.org/Transposition_Table) for move ordering and basic pruning

-   Evaluation
    -   [HCE](https://www.chessprogramming.org/Evaluation) (hand-crated evaluation)
    -   Basic [material](https://www.chessprogramming.org/Material) difference evaluation
    -   [Piece-Square tables](https://www.chessprogramming.org/Piece-Square_Tables) with valued copied from the [Simplified Evaluation Function](https://www.chessprogramming.org/Simplified_Evaluation_Function)

More features to be implemented as I find time!

## Building and Running

This project is written in Rust. Since I (presently) don't have any precompiled binaries, you need to build from source with [Cargo](https://doc.rust-lang.org/cargo/).
Once you've downloaded the code, a simple `cargo run --release --bin brogle` will launch the engine.

## Acknowledgements:

Special thanks to all

-   [Sebastian Lague](https://www.youtube.com/@SebastianLague), for his [chess programming series](https://www.youtube.com/watch?v=_vqlIPDR2TU&list=PLFt_AvWsXl0cvHyu32ajwh2qU1i6hl77c) on YouTube that ultimate inspired me to do this project.
-   The [Chess Programming Wiki](https://www.chessprogramming.org/), and all those who contribute to free, open-source knowledge.
-   The folks over at the [Engine Programming Discord](https://discord.com/invite/F6W6mMsTGN), for their patience with my silly questions and invaluable help overall.
-   [Analog-Hors](https://github.com/analog-hors), for her excellent [article on magic bitboards](https://analog-hors.github.io/site/magic-bitboards/)
