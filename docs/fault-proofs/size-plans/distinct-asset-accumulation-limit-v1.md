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

The family fit test will build the fresh `testnet` blueprint with the pinned
compiler, publish all six applied reference scripts under the 15,872-byte
reliability target, and record signed bytes, memory, CPU, and positive margins
for publications, direct/certified carriage, all three reason arms in both
directions, every physical cancel, restart after a committed fold checkpoint,
permanent proof mint, and mutation-leased target plus descendant removal. It
will fail closed on `oversized`, raised protocol limits, or disabled local UPLC
evaluation and write the deterministic ledger to
`distinct-asset-accumulation-limit-v1-fit-ledger.json`.

## Current compiled fit evidence

The fresh `testnet` blueprint built with Aiken `v1.1.23+5adf783` at SHA-256
`5a131c16641da7b254a01f0fa739114172c3583e60ff0f1231677bad5dd35de3`
contains the following fully applied raw programs. These figures are a compile
gate; the signed-transaction and measured execution-unit ledger is recorded
below.

| Applied validator    | Raw compiled bytes | Margin to 15,872 bytes |
| -------------------- | -----------------: | ---------------------: |
| `step_01.main.spend` |             14,822 |                  1,050 |
| `step_02.main.spend` |             10,647 |                  5,225 |
| `step_03.main.spend` |             12,281 |                  3,591 |
| `step_04.main.spend` |             12,130 |                  3,742 |
| `step_05.main.spend` |              8,229 |                  7,643 |
| `step_06.main.spend` |              1,872 |                 14,000 |

The real Van Rossem emulator publication transactions, completed and signed
with the freshly applied scripts, measure as follows. Reference publication
executes no validator, so its memory and CPU columns are canonically zero.

| Applied validator    | Signed publication bytes | Margin to 15,872 bytes | Memory | CPU |
| -------------------- | -----------------------: | ---------------------: | -----: | --: |
| `step_01.main.spend` |                   15,204 |                    668 |      0 |   0 |
| `step_02.main.spend` |                   10,995 |                  4,877 |      0 |   0 |
| `step_03.main.spend` |                   12,630 |                  3,242 |      0 |   0 |
| `step_04.main.spend` |                   12,479 |                  3,393 |      0 |   0 |
| `step_05.main.spend` |                    8,577 |                  7,295 |      0 |   0 |
| `step_06.main.spend` |                    2,265 |                 13,607 |      0 |   0 |

The real Lucid/Van Rossem lifecycle starts the registered category-35 thread,
restarts it from each committed out-ref, exercises every physical cancel,
authenticates the retained `ValueAndMint` state, advances the input and output
checkpoints, proves the mint-domain first crossing, mints the permanent proof,
and removes the mutation-leased target plus descendant. The table retains the
maximum observed execution units across repeated signed runs (address-dependent
hash comparisons can select different bounded branches). All execution-unit
margins are against 16,500,000 memory units and 10,000,000,000 CPU units.

| Lifecycle transaction          | Signed bytes |    Memory |           CPU |
| ------------------------------ | -----------: | --------: | ------------: |
| cancel step 1                  |          611 |   124,808 |    42,452,566 |
| cancel step 2                  |          611 |   113,576 |    40,640,424 |
| cancel step 3                  |          611 |   111,876 |    40,368,424 |
| cancel step 4                  |          611 |   111,876 |    40,368,424 |
| cancel step 5                  |          611 |   111,376 |    40,288,424 |
| cancel step 6                  |          611 |   111,876 |    40,368,424 |
| init                           |        1,497 |   685,608 |   235,025,974 |
| step 1 accepted source         |        2,030 | 1,349,088 |   459,158,245 |
| step 2 retained authentication |        2,004 |   983,928 |   465,786,728 |
| step 3 input checkpoint        |        1,064 |   133,910 |    58,383,591 |
| step 4 output checkpoint       |        1,064 |   133,910 |    58,383,591 |
| step 5 mint first crossing     |        1,126 |   325,812 |   126,582,342 |
| step 6 permanent proof mint    |          916 |   271,347 |    98,742,003 |
| mutation-leased removal        |        2,060 | 3,090,203 | 1,050,218,245 |

The deterministic ledger is
`distinct-asset-accumulation-limit-v1-fit-ledger.json`, SHA-256
`eddd4b4f310dcda08f70501eb163fb3f9e3c1f8fac6eef0a110e2b9284b5ac96`.

The retained-DA prerequisite is now precise: step 2 consumes the authenticated
`ValueAndMint` pre-state/control and proves its phase, stage, work root and
trace membership. Steps 3--5 consume only the retained negative-coordinate
`valueInputAsset`, `valueOutputAsset`, or `valueMintAsset` auxiliary selected by
the typed coordinate. No complete transaction-wide auxiliary stream is
required by this family.
