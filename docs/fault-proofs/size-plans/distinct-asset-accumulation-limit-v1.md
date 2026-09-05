# `distinctAssetAccumulationLimit` V1 size plan

Category `00000035` is a six-validator linear proof thread. It owns only the
three `E_ASSET_COUNT` coordinates produced by the canonical `ValueAndMint`
machine: resolved spend-input assets, transaction-output assets, and mint
assets. No value-preservation, minimum-Ada, decoding, or unrelated rejection
predicate is reachable from an applied script.

| Physical applied validator                             | State transition                                                                       | Imported semantic engine                                                                  | Maximum dynamic evidence                                                                 | Planned fit test                                                                            |
| ------------------------------------------------------ | -------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------- |
| `distinct_asset_accumulation_limit_step_01.main.spend` | initial thread -> typed input/output/mint coordinate                                   | proof-thread accepted/forced subject bind                                                 | one maximum-depth accepted inclusion or forced-leaf membership proof                     | complete signed accepted/forced start transactions                                          |
| `distinct_asset_accumulation_limit_step_02.main.spend` | bound coordinate -> authenticated accumulator initialization                           | canonical `ValueAndMintControlV1` work-root and validation-trace membership checks        | one retained trace proof plus the full fixed-size native/value control                   | signed maximum-control authentication transaction                                           |
| `distinct_asset_accumulation_limit_step_03.main.spend` | initialized -> resolved-input fold checkpoint                                          | narrow twin of the `ValueAndMint` replay-asset membership and canonical MPF mutation rule | one maximum output-descriptor asset proof and one maximum-depth resolution-schedule node | signed worst-case input-asset crossing/boundary transaction                                 |
| `distinct_asset_accumulation_limit_step_04.main.spend` | input checkpoint -> output fold checkpoint                                             | narrow twin of the `ValueAndMint` output-asset membership and canonical MPF mutation rule | one maximum output-descriptor asset proof                                                | signed worst-case output-asset crossing/boundary transaction                                |
| `distinct_asset_accumulation_limit_step_05.main.spend` | output checkpoint -> mint fold checkpoint                                              | narrow twin of the `ValueAndMint` mint membership and canonical MPF mutation rule         | one maximum-depth mint membership proof                                                  | signed worst-case mint-asset crossing/boundary transaction                                  |
| `distinct_asset_accumulation_limit_step_06.main.spend` | exact first crossing or complete contradiction -> thread burn and permanent proof mint | proof-thread `terminal_contradiction_v1` over the authenticated canonical mutation result | no unbounded evidence                                                                    | signed terminal mint, every-stage cancel, and leased target/descendant removal transactions |

The carried state is domain separated by transaction identity, verdict source
and direction, typed coordinate, validation-trace descriptor, program counter,
canonical `ValueAndMint` work root, current fold domain, accumulator root,
seen/nonzero asset counts, cursor, and the next expected script. The narrow
twins are necessary because importing the transaction-wide semantic resolvers
produced applied scripts of 20,873--24,540 raw bytes. Each fold step
accepts only its matching coordinate kind; non-target domains advance through
an authenticated fixed checkpoint and the target domain applies the same
MPF insertion/update predicate as the canonical machine. The accepted-invalid direction requires the
authenticated target mutation to produce the first `> 16,384` crossing. The
wrongful-rejection direction requires the same authenticated coordinate to
produce an ordinary successor at the exact boundary or below, contradicting
the forced `E_ASSET_COUNT` leaf.

Maximum evidence uses 16,384 distinct assets before the selected mutation,
the maximum 32-byte asset name, the maximum Merkle sibling depth admitted by
the native transaction bounds, and the branch that creates a new accumulator
leaf. The adjacent vector leaves `seen_asset_count == 16,384`; the over-bound
vector attempts `16,385`. Checkpoints are fixed-size and cannot be transplanted
between input, output, or mint folds.

The family fit test uses the fresh `testnet` blueprint with the pinned
compiler, publish all six applied reference scripts under the 15,872-byte
reliability target, and records signed bytes, memory, CPU, and positive margins
for publications, direct/certified carriage, all three reason arms in both
directions, every physical cancel, restart after a committed fold checkpoint,
permanent proof mint, and mutation-leased target plus descendant removal. It
fails closed on `oversized`, raised protocol limits, or disabled local UPLC
evaluation and writes the deterministic ledger to
`distinct-asset-accumulation-limit-v1-fit-ledger.json`.

## Completed lifecycle and recovery evidence

The test matrix contains all three typed coordinates in both verdict directions,
each with a baseline, a maximum-envelope case, and an honest refusal: 18 real
Lucid lifecycles. Each successful proof starts from the registered catalogue,
authenticates the selected source and retained control, traverses all six
physical scripts, mints the permanent proof, and removes the leased target and
descendant. All six cancellation positions are exercised in both directions.
Input and output decisions survive the remaining non-target checkpoints; those
checkpoints refuse a missing prior decision. Honest boundary/crossing opposites
also refuse deliberately forged terminal diagnostic evidence. Corrupted work
roots and delta-presence witnesses fail before the valid continuation.

The maximum envelope combines a 64-step source MPF proof, 32 validation-trace
siblings, 14 asset/mint siblings, a 16-step delta insertion proof, a 32-byte asset
name and maximum-width signed quantity. Input/output descriptors include a
57-byte address, maximum uint64 lovelace, the 5,000-byte Cardano Value bound,
and complete reference-script hash/length/commitment fields. Source compact
integers use their maximum widths. The retained native control simultaneously
fills all 15 possible frontier heights below the 32,768-byte field ceiling in
every nonselected domain, plus full compact/witness fields, maximum declared
field lengths, and a conservative context envelope with uint64 integers.
This simultaneous frontier shape conservatively exceeds any one canonical
transaction's aggregate field allocation.

Maximum source and accumulator proofs use explicitly constructed operator
subtree commitments. The selected source, descriptor, control work root,
coordinate and mutation are authenticated by the real validators; the test does
not allocate hidden trees or claim their unrelated leaves are valid canonical
transactions. The 16,384-leaf asset frontier likewise stresses its admitted
membership shape. Complete field bytes are not proof inputs: the family opens
fixed output descriptors and selected assets, never a full output/script/value.

The installed family actuator rehydrates retained DA between maximum-case
stages. A fresh directory-journal reader resumes after each durable intent and
confirmation. Prepared witness bytes are compared on every turn; changed
artifacts and publication recovery payloads refuse admission. An accepted
64-step proof exceeds the direct transaction size because it is carried twice.
The installed runner catches only the exact release-bound `maxTxSize` refusal,
publishes shared proof chunks, journals their intended hash and exact outputs,
reconciles them through the authenticated publication observer, and resumes the
same step with chunked verification. The forced source fits directly. No manual
certificate input or increased ledger limit is used.

The shared `buildVanRossemFitLedger`/`writeVanRossemFitLedger` writer records all
255 signed transactions: 111 publications (108 applied-script publications and
three automatic proof-chunk publications) and 144 transactions with nonzero
execution units. The verifier reconstructs every margin and digest, checks all
18 shapes and named arms, and hashes the current `realBlueprintPath`; rebuilding
the blueprint requires regenerating this evidence.

Reproduce with the declared Node/pnpm toolchain, after a fresh pinned testnet
Aiken build:

```sh
MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/distinct-asset-accumulation-limit-lifecycle.test.ts
pnpm --dir demo/midgard-fault-proofs exec vitest run tests/distinct-asset-accumulation-limit-fit-ledger.test.ts tests/distinct-asset-accumulation-limit-publication-fit.test.ts tests/distinct-asset-accumulation-limit.test.ts tests/distinct-asset-accumulation-recovery.test.ts
```

The current ledger records maxima of 15,204 signed bytes, 5,656,307 memory units, and 2,153,991,585 CPU units. All margins are positive; the largest reference publication leaves 668 bytes beneath the 15,872-byte reliability target.
