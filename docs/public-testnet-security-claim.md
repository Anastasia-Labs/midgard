# P1 public testnet security claim

P1 is the planned stage-0 public Preprod deployment of Midgard. It uses curated
operators, allow-listed fault provers and DA challengers, and manual proof
submission by the team. This document describes that limited security model;
the release manifest and deployed profile identify an actual instance.

## Participation and trust

Users can submit normal L2 transactions through the node API. Deposits,
withdrawal requests and supported native-only forced transactions enter through
L1. Normal L2 transactions can execute Plutus. The operator set is curated for
P1, and only the listed keys can initiate the supported fraud proofs and
availability challenges.

Users rely on curated operators for block production and for correctness outside
the enabled proof catalogue. A dishonest operator can commit some invalid states
that P1 cannot prove, including incorrect Plutus verdicts and fabricated forced
transactions. A dishonest operator can also censor a valid forced transaction by
assigning a rejection that P1 cannot challenge. These limitations prevent a
general promise that users can always exit without operator cooperation.

Allow-listed provers prepare and submit supported proofs using authenticated
chain and payload evidence. Proof preparation does not establish that a proof
has landed. An enabled proof can remove a block or slash a bond only when its
on-chain checks accept the authenticated fault. Manual operation and sufficient
L1 submission capacity are required.

The DA committee attests availability and retains the exact payload. Its
signature is not proof that the ledger transition is valid. Users rely on
timely retrieval, the configured Cardano finality assumptions and the applicable
availability challenge process.

## Supported paths and their limits

Key-held L2 funds can use the L1 withdrawal path, subject to the protocol's
event processing and maturity rules. The public profile uses a seven-day block
maturity. A withdrawal request is not an immediate payout or a universal escape
hatch.

Forced transactions are native-only. They cannot carry Plutus witnesses,
redeemers, auxiliary data, an L2 validity upper bound or output reference scripts.
Canonical and noncanonical inline datums are permitted at L1 admission. Passing L1 admission does not make
the submitted L2 transaction valid; its ledger checks and verdict still apply.
Native scripts must be supplied through the supported witness path.

A valid forced transaction spending a key-owned output, or a satisfiable native
script without relying on an expiry branch, can use this path. Funds requiring
Plutus execution or a native script's expiry branch need normal L2 execution.
The censorship limitation above still applies. The SDK screens admission rules and
applies a measured memory estimate before building the L1 order. The final L1
transaction still has to fit Cardano's size and execution limits with its
witnesses and material carriage. Passing the estimate is not proof of full
signed-transaction fit, and no single output or policy count is a universal limit.

Supported omission and fault proofs depend on authenticated inclusion, the
deployment's finality and timing rules, the enabled catalogue, retained evidence,
and successful L1 submission. They do not supply permissionless proof coverage
for every invalid state. The catalogue and excluded categories are published
with the release manifest.

## Other P1 limitations

- Validation-trace states and the validation dispute game are unavailable.
  The excluded proof families remain outside P1 coverage.
- Proof submission and re-drive after the safe-start deadline are manual.
- Strikes are permissionless and rotate operator shifts. They can still rotate
  shifts during catch-up and slow its progress. The public P1 ejection threshold
  is set out of reach, so strikes do not provide an operator ejection mechanism.
- P1 Plutus cost accounting differs from Cardano-exact word accounting.
  The node's evaluation endpoint reports advisory P1 budgets; evaluation does
  not guarantee admission or execution.
- Some otherwise valid Cardano shapes are refused: the interim 9,215-byte Data
  caps, the 2^512 integer refusal, and native scripts supplied only by reference
  inputs remain. The legacy rejection-code format also remains.
- An unsigned withdrawal order can block an output reference (issue #819).
- No authenticated bootstrap state snapshot is supplied. New verifiers replay
  from genesis and must join while the required retained payload history is
  available. The public deployment is required to retain payloads for 120 days.
- Escape hatch and settlement claims are unsupported.

P1 is a testnet. A later protocol phase requires a separately announced
regenesis; P1 state will not automatically carry over. A sunset notice must
identify the actual schedule and user actions. A planned date does not establish
that a release or migration has happened.

## Disclosure

Send private or urgent security reports to [info@anastasialabs.io](mailto:info@anastasialabs.io).
The project owner handles intake. The response targets, encrypted reporting
process and good-faith research terms are in [SECURITY.md](../SECURITY.md).
