# Value-not-preserved resumable conservation size plan

Category `00000019` retains the existing accepted single-asset predicate. The
installed maximum path uses a canonical input/output/mint union fold. Accepted
proofs still name an Ada/token unit and inflation/deflation direction. Forced
proofs authenticate the exact `ValueNotPreserved` verdict and prove conservation
for every unit, including Ada and fee. Original submitted-valid DA bytes remain
available independently of the operator-adjudicated rejected source bytes.

The signed-delta MPF starts empty. Every authenticated asset occurrence applies
one signed addition. A zero result deletes the unit; nonzero results remain.
Completing every domain with an empty map therefore proves universal token
conservation. A separate scalar proves Ada conservation. A prover cannot stop
at a favorable unit or skip an occurrence.

## Physical and transaction boundaries

The existing accepted four-script route stays available. New source doors feed
one shared resumable engine. Each physical program targets a signed reference
publication of at most 15,872 bytes. Each evaluated transaction targets at most
16,384 signed bytes with positive Van Rossem CPU/memory margins.

| Physical responsibility | Frozen state / transition                                       | Maximum dynamic witness                       |
| ----------------------- | --------------------------------------------------------------- | --------------------------------------------- |
| Accepted source         | Original asset/sign claim and native identity                   | One counted source proof, direct or published |
| Forced source           | Exact forced key, rejected source and reason                    | One counted forced source proof               |
| Event coordinate        | Exact source event to transition index                          | One event-root MPF proof                      |
| Transition pre-state    | Bind the event's actual ledger pre-state                        | One transition-root MPF proof                 |
| Input selection         | Canonical field-0 cursor to input out-ref                       | At most two authenticated field chunks        |
| Input descriptor        | Ledger membership and immutable value facts                     | One ledger proof plus fixed descriptor        |
| Input assets            | Exhaustive asset frontier cursor                                | One leaf proof, unit and quantity             |
| Field grammar           | Certify exact variable-width count/end                          | Bounded shared grammar steps and field chunks |
| Output selection        | Authenticated next item extent                                  | Bounded envelope header; no whole output copy |
| Output scan             | Canonical bounded output parser to asset frontier               | Current chunk plus immediate successor        |
| Output assets           | Exhaustive output asset frontier cursor                         | One leaf proof, unit and quantity             |
| Mint selection          | Canonical field-5 cursor to signed asset quantity               | One bounded mint item and field chunks        |
| Union update            | Authenticate old delta and apply exact signed contribution      | One maximum MPF mutation proof                |
| Terminal                | Exhausted domains, exact accepted sign or universal forced zero | Ada scalar or one selected token membership   |

Selection and MPF mutation use separate transactions. The output selector freezes
the authenticated field length, chunk digests and exact item extent; the scanner
reads a 132-byte window for each of four semantic steps. Certified field
publications carry raw chunks of at most 15,148 bytes independently of the
roughly 9KB maximum MPF mutation proof. The
selector freezes its own authenticated script hash and continuation before
calling the union-update script. That script returns only to the frozen hash,
with the uniquely calculated root and unchanged continuation. This removes
parameter cycles while preserving authenticated routing.

Every physical stage supports cancellation. The installed durable workflow must
rehydrate exact retained evidence, publish/reconcile required field/proof
carriage automatically, survive restart after intent and confirmation, reach
permanent proof mint and leased removal, and refuse changed artifacts, source
coordinates, roots, quantities, cursors, or publication outputs.

The evidence gate includes Ada-only, token, mint, burn, multi-policy maximum,
maximal source/input proofs and field windows, and adjacent/honest mutations in
both directions. Measurements use the shared Van Rossem ledger writer and bind
the current real testnet blueprint digest. Physical cuts may be refined only
with measured script and transaction evidence, without relaxing ledger limits.

## Measured implementation

The new chain has 13 physical programs plus the existing entry. The largest
new raw program is the output scanner (13,585 raw bytes). Its reference
publication fits below the 15,872-byte target after the field-anchor work moved
to output selection. The existing accepted constructor remains constructor 0;
the conservation launch occupies constructor 1 inside the ordinary Continue arm.

The lifecycle suite exercises both directions through catalogue registration,
permanent proof and removal, exact 32,768-byte aggregate native fields, signed
mint and burn, and cancel/reinitialize at every visited physical program. The
installed field prerequisite is the same function used by the manifest-bound
workflow, rebuilding publication/certificate actions from admitted retained DA
and reconciling them across fresh instances. Durable adapter tests use the
fsynced directory journal for pre-submit intent and exact mutation-lease recovery.

Regenerate measured transactions with:

```sh
MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json" MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs exec vitest run tests/value-conservation-lifecycle.test.ts
MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json" pnpm --dir demo/midgard-fault-proofs exec vitest run tests/value-not-preserved-fit-ledger.test.ts
```

The verifier recomputes the standard ledger using the current real blueprint
SHA-256; it does not accept a saved digest independently of the current build.

The 1,304-asset lifecycle exposed two canonical MPF exclusion discrepancies:
terminal-fork compression omitted the shared skipped prefix, and a nonterminal
leaf with a skip used the neighbor nibble before the skip. The shared Aiken
mutation helpers and resumable TS/Aiken fold now agree with the pinned off-chain
trie. Exact insertion/deletion regression vectors bind both roots, including a
negative vector for the dropped prefix. This shared repair changes dependent
script hashes, so evidence from older blueprints must be regenerated. Both
environments pin the repaired published-proof verifier hash
`1dbd8f5ad1314ebf9e4bcbf814fb7f44eea574ed20b3a5b42c7e0767`; the
accepted maximum-proof lifecycle exercises that withdrawal binding.

Retained-DA replay checks also spend an output created by an earlier forced
transaction in the same block. They establish the selected event's pre-state
and reject an inconsistent prior committed effect root, independently of the
block's initial ledger root.
