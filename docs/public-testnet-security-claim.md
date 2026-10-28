# P1 public testnet security claim

P1 is the planned stage-0 public Preprod deployment of Midgard. It uses curated
operators; fault provers and DA challengers are permissionless, and every
watcher submits the proofs it finds with its own key. This document describes
that limited security model; the release manifest and deployed profile identify
an actual instance.

## Participation and trust

Users can submit normal L2 transactions through the node API. Deposits,
withdrawal requests and supported native-only forced transactions enter through
L1. Normal L2 transactions can execute Plutus. The operator set is curated for
P1 and fixed for its life; registration admits only allow-listed operator keys.
Any key can initiate the enabled fraud proofs and availability challenges.

Users rely on curated operators for block production and for correctness outside
the enabled proof catalogue. A dishonest operator can commit some invalid states
that P1 cannot prove, including fabricated forced transactions and faults in the
categories the profile excludes. A dishonest operator can also censor a valid
forced transaction by assigning a rejection that P1 cannot challenge. These
limitations prevent a general promise that users can always exit without
operator cooperation.

A Plutus verdict is challengeable through the interactive validation dispute
game. The game proves each execution step that fits in a single L1
transaction. A step that does not fit cannot be proven, so a wrong verdict
that depends on such a step relies on curated-operator trust.

A watcher detects supported faults from authenticated chain and payload
evidence and submits the proof transactions itself; a normal challenge needs no
operator command. Proof preparation does not establish that a proof has landed.
An enabled proof can remove a block or slash a bond only when its on-chain
checks accept the authenticated fault; a proof against an honest block is
refused. Sufficient L1 submission capacity is required.

The DA committee attests availability and retains the exact payload. Its
signature is not proof that the ledger transition is valid. Users rely on
timely retrieval, the configured Cardano finality assumptions and the applicable
availability challenge process.

## Supported paths and their limits

Key-held L2 funds can use the L1 withdrawal path, subject to the protocol's
event processing and maturity rules. The public profile uses a seven-day block
maturity. A withdrawal request is not an immediate payout or a universal escape
hatch.

A forced transaction enters as an L1 order whose own validity range is closed
and at most ten minutes long. Forced transactions are native-only. They cannot
carry Plutus witnesses, redeemers, auxiliary data, an L2 validity upper bound
or output reference scripts, and each output's encoded value is at most 5,000
bytes. Canonical and
noncanonical inline datums are permitted at L1 admission. Passing L1 admission
does not make the submitted L2 transaction valid; its ledger checks and verdict
still apply.

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
and successful L1 submission. They do not supply proof coverage for every
invalid state. The nine trace-relative fault-proof families prove their
faults directly from the committed transaction, without validation-trace
states. The current `preprod-public` profile enables all nine and the Plutus
dispute game, `validationTraceDispute`, and excludes no category. The catalogue
and excluded categories of an instance are published with its release
manifest.

## Other P1 limitations

- DA payloads carry neither validation-trace states nor the UTxO set.
- A watcher does not start a proof after that proof's latest safe start. It
  holds the objective visibly, and no command at this release re-drives it.
  Any other challenger may still prove the fault while the challenge window is
  open.
- Strikes are permissionless and rotate operator shifts. They can still rotate
  shifts during catch-up and slow its progress. The public P1 ejection threshold
  is set out of reach, so strikes do not provide an operator ejection mechanism.
- L2 Plutus scripts are evaluated with Cardano's protocol-version-11 budget
  rules and the pinned PlutusV3 cost model. The node's evaluation endpoint
  reports advisory budgets; evaluation does not guarantee admission or
  execution.
- Rejections committed for forced transactions carry typed rejection reasons.
  The node's transaction status API reports its own diagnostic rejection
  codes.
- An unsigned withdrawal order can block an output reference (issue #819).
- No authenticated bootstrap state snapshot is supplied. New verifiers replay
  from genesis and must join while the required retained payload history is
  available. The public profile configures 120-day payload retention.
- Escape hatch and settlement claims are unsupported.

P1 is a testnet. A later protocol phase requires a separately announced
regenesis; P1 state will not automatically carry over. A sunset notice must
identify the actual schedule and user actions. A planned date does not establish
that a release or migration has happened.

## Disclosure

Send private or urgent security reports to [info@anastasialabs.io](mailto:info@anastasialabs.io).
The project owner handles intake. The response targets, encrypted reporting
process and good-faith research terms are in [SECURITY.md](../SECURITY.md).
