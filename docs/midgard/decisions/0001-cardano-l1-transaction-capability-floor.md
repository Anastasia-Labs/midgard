# 0001 — Cardano L1 transaction capability is Midgard's minimum

- Status: Accepted
- Date: 2026-07-24
- Scope: canonical V1 and every subsequently deployable Midgard consensus
  profile
- Supersedes: any provisional Midgard limit that is more restrictive than the
  Cardano mainnet capability floor defined below

## Context

Midgard is a Cardano L2. A user should not need to split, simplify, or remove a
transaction merely because Midgard's fault proof is easier to implement with a
smaller transaction, value, script, redeemer set, or cardinality.

Canonical V1 uses independently authenticated field and program preimages and
has no independent aggregate 8KiB transaction limit. When this decision was
adopted, provisional V1 limits were nevertheless below what a Cardano mainnet
transaction can use. Those historical limits included a 4,095-byte output
preimage, a 9,215-byte general field preimage, 64-element
input/output/witness limits, 16 script executions, 128 distinct assets, and
low native-script depth/node limits. The current compiled values that replace
them are recorded below and checked into the canonical profile documentation.

Those restrictions came from fitting a complete witness into one L1 proof
transaction. They are proof-layout constraints, not acceptable user-facing L2
capability constraints.

## Cardano mainnet parameter snapshot

The following values were observed for Cardano mainnet Conway epoch 645 on
2026-07-24:

| Parameter                       |           Mainnet value | Transaction effect                                   |
| ------------------------------- | ----------------------: | ---------------------------------------------------- |
| protocol version                |                  `11.0` | Identifies the active ledger rules                   |
| `maxTxSize`                     |            16,384 bytes | Maximum complete serialized transaction              |
| `maxValueSize`                  |             5,000 bytes | Maximum serialized `Value` in an output              |
| `maxTxExecutionUnits.memory`    |              16,500,000 | Aggregate Plutus memory per transaction              |
| `maxTxExecutionUnits.steps`     |          10,000,000,000 | Aggregate Plutus CPU steps per transaction           |
| `maxCollateralInputs`           |                       3 | Maximum collateral inputs                            |
| `collateralPercentage`          |                    150% | Required collateral relative to the fee              |
| `txFeePerByte`                  |             44 lovelace | Linear transaction-size fee coefficient              |
| `txFeeFixed`                    |        155,381 lovelace | Fixed transaction fee component                      |
| execution price, memory         |    0.0577 lovelace/unit | Plutus memory fee                                    |
| execution price, steps          | 0.0000721 lovelace/unit | Plutus CPU fee                                       |
| `utxoCostPerByte`               |          4,310 lovelace | UTxO storage charge                                  |
| `minFeeRefScriptCoinsPerByte`   |             15 lovelace | Reference-script fee input                           |
| `maxBlockBodySize`              |            90,112 bytes | Block-level context, not a per-transaction allowance |
| `maxBlockExecutionUnits.memory` |              72,000,000 | Block-level Plutus memory                            |
| `maxBlockExecutionUnits.steps`  |          20,000,000,000 | Block-level Plutus CPU steps                         |

The live snapshot was read from:

- `https://api.koios.rest/api/v1/epoch_params?limit=1&order=epoch_no.desc`
- `https://api.koios.rest/api/v1/tip`

Koios is useful reproducible observation evidence, but it is not the
activation authority. A release must query the target network through its
trusted Cardano node/provider and bind the resulting parameter snapshot into
the exact deployment profile and release evidence.

Parameter meanings come from the official
[Cardano protocol-parameter guide](https://docs.cardano.org/about-cardano/explore-more/parameter-guide),
the [formal ledger specification](https://intersectmbo.github.io/formal-ledger-specifications/cardano-ledger.pdf),
and [CIP-28](https://cips.cardano.org/cip/CIP-28). Reference-script semantics
come from [CIP-33](https://cips.cardano.org/cip/CIP-0033). Cardano parameters
are governable; the current values must not be confused with the wider
[constitutional guardrails](https://cardano.org/constitution/), such as the
32KiB upper guardrail for `maxTxSize`.

## Comparison with canonical V1

| Dimension                           |                                                                                   Cardano mainnet |                                                                            Midgard canonical V1 | Comparison                                              |
| ----------------------------------- | ------------------------------------------------------------------------------------------------: | ----------------------------------------------------------------------------------------------: | ------------------------------------------------------- |
| Total transaction bytes             |                                                                                            16,384 |                                      295,041 derived from all bounded fields plus fixed framing | Midgard higher                                          |
| Serialized output preimage          |                                          Transaction-size constrained; `Value` alone may be 5,000 |                                                                                          16,384 | Equal byte floor                                        |
| General dynamic field               |                                                   May consume nearly the remaining L1 transaction |                                        32,768, consumed through authenticated 4,095-byte chunks | Midgard higher                                          |
| Aggregate transaction execution     |                                                                          16.5M memory / 10B steps |                16.5M memory / 10B steps capability floor; execution may span proof transactions | Equal floor                                             |
| Spend inputs                        |                                                No separate ledger count; transaction-size limited |                                      16,384 one-byte-floor guardrail; field bytes are effective | No lower count for an L1-sized transaction              |
| Reference inputs                    |                                                No separate ledger count; transaction-size limited |                                      16,384 one-byte-floor guardrail; field bytes are effective | No lower count for an L1-sized transaction              |
| Outputs                             |                                                No separate ledger count; transaction-size limited |                                      16,384 one-byte-floor guardrail; field bytes are effective | No lower count for an L1-sized transaction              |
| Vkey witnesses and required signers |                                                No separate ledger count; transaction-size limited |                                                          16,384 each; field bytes are effective | No lower count for an L1-sized transaction              |
| Script executions and redeemers     |                                                       Transaction-size and execution-unit limited |                                   16,384 one-byte-floor guardrail plus matching execution floor | No lower independent count                              |
| Distinct assets                     |                                                              5,000-byte serialized output `Value` |                                     16,384 one-byte-floor guardrail plus 5,000-byte Value bound | No lower independent count                              |
| Native-script complexity            |                                                            Primarily transaction-size constrained |                                                                Depth and node guardrails 16,384 | No lower bound for an L1-sized transaction              |
| Reference scripts                   | 200 KiB aggregate use limit in Conway ledger rules; creation is transaction-size/UTxO constrained | 50-byte executable envelope plus up to 67,108,418 bytes of content-addressed material within DA | Midgard higher; individual proof reveals remain bounded |

“No separate count” does not mean unbounded. Cardano's complete transaction
size, canonical encoding, ledger rules, and execution budget still impose a
finite maximum. It means Midgard may not choose an unrelated lower round-number
cap. Midgard must derive and test a count that admits every applicable shape
which can fit under the Cardano constraints.

The comparison is capability-based, not a claim that Cardano and Midgard have
identical transaction schemas. Governance actions, certificates, staking
operations, and other Cardano features outside Midgard's transaction model do
not become Midgard features through this decision. For every feature present
in both models, however, Midgard's resource and cardinality limits must be no
more restrictive.

## Decision

**Every deployable Midgard consensus profile must provide transaction
capability equal to or greater than the Cardano mainnet capability in effect
for that profile's target deployment.**

For an upper capacity limit, “equal or greater” means that the Midgard maximum
is at least the corresponding Cardano maximum. For a required minimum,
economic charge, collateral rule, or storage rule, it means Midgard must not
reject an otherwise supported transaction by imposing a stricter requirement
without a separate protocol-semantic reason unrelated to proof convenience.

Concretely:

1. Midgard must accept at least 16,384 bytes of canonical transaction data.
2. Midgard must accept every supported output value shape whose canonical
   Cardano serialized `Value` is at most 5,000 bytes.
3. A supported scripted transaction must receive at least 16.5M aggregate
   memory units and 10B aggregate CPU steps.
4. Input, reference-input, output, signer, witness, redeemer, script,
   native-script, and asset limits must admit every applicable Cardano-valid
   shape that fits the mainnet byte and execution limits.
5. Inline datums, reference scripts, mint/burn, and other supported dynamic
   fields may not receive a lower arbitrary cap merely because one
   self-contained fraud-proof transaction cannot carry them.
6. Normal and forced Midgard transactions use the same capability floor.
7. A profile must identify the exact Cardano parameter snapshot used for its
   comparison and include machine-checkable parity evidence.

Proof complexity does not override this rule.

## Accepted proof decomposition tradeoff

It is explicitly acceptable for a proof or forced-submission path to require
more Cardano transactions in order to preserve Cardano-level L2 transaction
capacity.

Permitted techniques include:

- publishing field evidence once and referencing it from later proof steps;
- splitting large evidence into ordered, hash-bound chunks;
- certifying content-addressed script or value nodes separately;
- using more bisection rounds or one-step transitions;
- spreading CEK execution across multiple bounded dispute transactions;
- proving large input, output, asset, witness, or redeemer collections through
  authenticated incremental folds; and
- separating publication, receipt, order, challenge, and settlement
  transactions.

Each individual L1 transaction must still satisfy the live Cardano limits.
The complete path must be deterministic, trustless, bounded in transaction
count, affordable under documented assumptions, and able to finish with
substantial margin inside the block maturity window.

Reducing the number of proof transactions is an optimization objective. It is
not a valid reason to impose a lower Midgard transaction constraint.

## Profile lifecycle

Cardano protocol parameters can change. Midgard therefore cannot encode
“mainnet parity” as a one-time comparison against this document's numbers.

Every release must:

1. read the target network's effective and pending protocol parameters;
2. produce a canonical parameter snapshot and digest;
3. generate adversarial maximum-shape fixtures from that snapshot;
4. prove that every Midgard constraint is equal or less restrictive;
5. bind the snapshot digest, profile digest, and validator hashes together;
6. refuse activation when the comparison is missing or fails; and
7. monitor ratified Cardano parameter changes early enough to deploy a new
   exact Midgard profile before a more permissive L1 parameter becomes
   effective.

An immutable Midgard profile must not silently raise its limits. If Cardano
becomes more permissive than the active profile, a new versioned profile is
required. Continuing to advertise the old profile as Cardano-capability
equivalent is forbidden.

## Consequences for canonical V1

The canonical V1 source now uses independent field commitments,
content-addressed program material, deterministic validation traces, and
interactive one-step disputes. Its compiled capability values remove the lower
round-number limits identified when this decision was adopted.

Before canonical V1 is production-activated:

- generate a trusted effective-and-pending target-network snapshot;
- generate Cardano boundary and adjacent-boundary fixtures for every mapped
  capability;
- execute complete normal and forced proof paths for those fixtures;
- construct and measure actual applied/parameterized publication, resolution,
  and settlement transactions; and
- bind the resulting validator hashes, parameters, measurements, DA framing,
  fees, transaction counts, and timing evidence into the release digest.

The existing fail-closed proof-profile release gate must remain closed until
that evidence is complete.

## Acceptance criteria

A profile satisfies this decision only when CI produces all of the following:

1. a trusted target-network protocol-parameter snapshot;
2. a checked mapping from every applicable Cardano transaction constraint to
   its Midgard counterpart;
3. generated boundary transactions at the Cardano maximums and immediately
   adjacent rejecting cases;
4. proof and forced-submission paths for maximum-size fields, outputs, values,
   scripts, redeemer sets, inputs, outputs, witnesses, and asset collections;
5. measured byte, memory, CPU, fee, transaction-count, and wall-clock bounds
   for every path;
6. normal/forced and valid/invalid adversarial tests;
7. cross-language TypeScript/Aiken agreement for every incremental proof
   transition; and
8. a validator-hash- and parameter-snapshot-bound release-evidence digest.

Unknown parameters, an unsupported comparison, missing evidence, an
out-of-date snapshot, or any lower Midgard capability must fail closed.
