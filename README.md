# The Orbis Project

The Orbis Project is an effort, led by [Ardana](https://ardana.org/), to create a ZK rollups layer 2 solution for Cardano. This project aims to address scalability challenges on Cardano, particularly to support decentralized finance (DeFi) applications such as those which Ardana has promised to deliver. In order to compete with the world of centralized finance, these applications must support massive transaction throughput, on the order of thousands of transactions per second. The Orbis Project aims to deliver this level of transaction throughput, and ultimately any desired level of transaction throughput, for decentralized applications (dapps) built on Cardano. The goal is to deliver for Cardano something comparable to ZK rollups solutions for Ethereum, like [zkSync](https://zksync.io/), [StarkNet](https://starkware.co/starknet/), and [Loopring](https://loopring.org/#/).

The Orbis Project is in design and early implementation phases. In this repo you can see the work in progress. Here is a breakdown of what is here:

 * `architecture`: the TeX source code (and Nix build code) for the current system architecture proposal.
 * `specs/on-chain-tx-pricing`: the TeX source code (and Nix build code) for the current on-chain transaction pricing model proposal.
 * `specs/uplc2c`: the design document for UPLC2C, a WIP Untyped Plutus Core to C compiler.
 * `uplc2c`: the code for UPLC2C compiler and runtime system.
 * `hs2halo2`: some rudiments for a way of expressing Halo 2 circuits in Haskell.
