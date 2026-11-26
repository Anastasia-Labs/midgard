# Isolated-chain devnet

A private Cardano chain (one pool) with Ogmios, Kupo and Postgres. The watcher
journeys, the contrib campaign `isolated-chain` stages and `devnet-stack` run on it.
Every run gets a unique Compose project, database, filesystem tree, Cardano
chain, Kupo index and deployment manifest. It refuses known public network
magic values and does not read the repository `.env`.

## Generate without starting services

```bash
export MIDGARD_ISOLATED_CHAIN_RUN_DIR=/tmp/midgard-isolated-chain-example
export MIDGARD_ISOLATED_CHAIN_RUN_ID=example
devnet/isolated-chain/scripts/generate.sh
```

Generation uses `cardano-cli latest genesis create-testnet-data` with exactly
one pool and renders Compose configuration, but starts no service. Defaults are
Ogmios `127.0.0.1:2337`, Kupo `127.0.0.1:2442`, and Postgres
`127.0.0.1:5544`. Projects and databases begin with
`midgard_isolated_chain_`; signing keys and run credentials are mode 0600. In a
linked git worktree the compose project also carries the checkout's hash and
the default ports shift by the same checkout-specific offset
(`scripts/lib/worktree-identity.mjs` at the repository root), so two
checkouts' devnets can run at once; the main checkout uses the defaults above.
Generation prints the project and ports it chose, and explicit
`MIDGARD_ISOLATED_CHAIN_*_PORT` values win.

## Start and stop

`scripts/up.sh <run-dir>` starts the run's L1 services and log followers;
`scripts/down.sh <run-dir>` stops them and keeps every durable tree, so `up.sh`
can start the run again. Both read every setting from the run's `run.env`.

## Chain parameters

Because the devnet is preprod-matched, generation pins the Shelley
genesis protocol major to 11, matching the target preprod ledger. This matters beyond enabling Plutus V3: the current Aiken bundle
uses builtins such as `CountSetBits` that major 9 rejects as malformed.
`validate-custom-chain-config.sh` fails closed unless the genesis protocol
major matches exactly and the node config forces Conway at epoch zero. Treat a
target-network protocol upgrade as an explicit fixture update, and never reuse
a run generated under a different major.

The shared generator reads `../preprod/configuration.json`: a dated profile
verified against the connected Preprod node through both cardano-cli and Ogmios,
with consensus values corroborated by the official Shelley genesis. It applies
all active protocol parameters, execution limits, and cost models before hashing
genesis. The watcher wrapper only supplies host paths; it does not override a
second set of protocol parameters. The isolated chain retains its own magic,
start time, keys, genesis funding and initial delegation.

Consensus uses one-second slots, `activeSlotsCoeff=0.25` (the isolated chain's
overlay; Preprod's is 0.05), `securityParam=2160`,
`epochLength=432000`, and `PraosMode`. The node has no peers, so `GenesisMode`
must not be used: its limit on eventual chain selection never lifts without a
caught-up transition and the chain freezes after exactly `securityParam` blocks.
Blocks are probabilistic. Timeouts and confirmation checks must count actual
blocks, not treat one slot as one block.
The generated initial committee terms obey the current maximum term length.

The ledger forecast horizon with these parameters is `ceil(3k/f)=25920` slots
(7.2 hours). This bounds how far a frozen isolated node can forecast; it is
**not** a maximum supported outage for a follower of a continuing network. See
the [consensus forecast explanation](https://ouroboros-consensus.cardano.intersectmbo.org/docs/references/miscellaneous/hard_won_wisdom/).

Cardano-node 11.1.3, Ogmios v7.0.0, Kupo v2.11.0, and Postgres 15.15 are
fixed to official immutable image digests. The node pin includes the genesis
cost-model override fix. The generated `run.env` records them.

The workspace uses Lucid Evolution 0.8.9, whose provider package natively
supports Ogmios v7's canonical `maxReferenceScriptsSizePerTransaction`
protocol-parameter field. No local provider patch is applied. The provider
fails closed when the value is missing or malformed. Verify it with:

```bash
pnpm --dir ../midgard-node exec vitest run tests/kupmios-ogmios-v7-protocol-parameters.test.ts
```

Run the static tests with:

```bash
pnpm run test:isolated-chain:assets
```

The fresh-chain funding policy is shared with `devnet-stack` in
[`preprod/funding-policy.json`](../preprod/funding-policy.json): a
1-billion-ADA spendable genesis UTxO (2 billion total, 1 billion delegated).
This is local test funding only. The 180-day, fivefold-margin fee model and
all fourteen stack roles are documented in the
[tools README](../../README.md#devnet-wallet-fee-runway).
