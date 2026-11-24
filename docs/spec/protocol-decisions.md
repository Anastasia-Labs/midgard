# Protocol decisions, margin thresholds and the proof-family contract

This document records canonical V1 protocol decisions that the component
specifications and the technical specification build on, the exact margin
thresholds every proof path is measured against, and what every fault-proof
family consists of. Field formats, commitments and the three carriage tiers
are defined in [`midgard-tx.md`](midgard-tx.md); economic values and deadlines
are fixed by the
[economics and margin decision record](../midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md).

## Protocol decisions

### Features stay enabled

Scripts, redeemers, reference inputs and scripts, script credentials,
protected outputs, observers, Values, mint/burn and valid effectful forced
transactions are all enabled in canonical V1. None is disabled to reduce proof
scope. Every Cardano-valid shape the target parameter snapshot admits stays
representable and independently disputable.

### Complete items first

Large Values and Data are carried as complete items. Exact signed quantities,
policy and asset identity, map key and value identity, child order, counts and
terminal summaries are preserved. The complete canonical Value or Data is
accepted through tier-1 direct carriage or the tier-2 single raw-UTxO input or
reference input ([`midgard-tx.md`](midgard-tx.md) §8) whenever the applied path
fits. Builders and validators use the simplest tier that fits; tier-3 chunked
publication, and stateful incremental traversal beyond it, are used only for a
family whose final applied validators are measured not to fit the simpler
tiers within the [margin thresholds](#margin-thresholds). Every tier of the
same item authenticates the same flat commitment and reaches the same result.

Authenticated offset-and-slice access over the flat-committed field preimage,
under the enveloped preimage grammar and the access invariants of
[`midgard-tx.md`](midgard-tx.md) §5 and §7, is that fallback. The dispute-side
mechanisms that realize it (the resumable walk and its checkpoints, the Value
bookmark, the Canonical-Data Acceptor) are defined in its §§10–11. Its §7
invariants, including positions rather than bytes in resumable state, bind
every consumer. No flattened, lossy or round-number substitute is allowed.

### Duplicate vkey hashes reject

Duplicate vkey hashes are noncanonical. Signer and witness collections use the
sole canonical V1 encoding and strict ordering and deduplication semantics
on-chain and off-chain.

### Fees and min-Ada

Fees and min-Ada are parameterized rules, not constants or stubs. The minimum
fee proof implements the exact Phase A formula from the bound target parameter
snapshot. Output min-Ada uses the applicable target Cardano ledger rule over
the canonical output bytes. No helper returns zero in place of either rule.

### Availability challenge

Data withheld after attestation is answered by a bond-backed availability
challenge. Opening a timely challenge blocks merge. The accountable DA signers
publish the complete committed payload and proof material through the fewest
authenticated L1 inline-datum outputs that fit. A complete single-output
publication is preferred; ordered multi-output publication is used only where
the complete item cannot fit in one publication transaction. Every publication
binds offsets, lengths, item or chunk hashes, terminal accumulator, payload
identity, header identity, deployment identity and deadline. Anyone can
reconstruct the complete response from L1; a private endpoint or a fresh
signature is insufficient. Exact publication closes only the availability
challenge and does not excuse malformed or root-mismatched content. Timeout
authorizes deterministic header and descendant correction and a DA slash of
the one DA bond pool that backs the whole committee.

The pool:

- One pool UTxO holds the `MIDGARD_DA_BOND_POOL` NFT, in state `Bonded` or
  `Withdrawing{unlock_at}`. Its backing is `max(0, lovelace − floor)`.
- An attestation applies only while the pool is `Bonded` with backing of at
  least `da_bond`. No bond is posted per block.
- Anyone may top up the pool, by at least the minimum top-up. BeginWithdraw,
  CancelWithdraw and CompleteWithdraw need the DA params owner quorum.
  `unlock_at` is the BeginWithdraw validity upper bound plus
  `da_bond_withdraw_delay`, which outlasts the latest timely slash of any block
  the pool backs.
- Timeout spends the pool, even at zero backing:
  `taken = min(da_bond, backing)`, `fee_part = min(penalty, taken)` and
  `payout = taken − fee_part`. The pool keeps `pool − taken` with the same
  datum and NFT, and `tx.fee = fee_part + c` with `0 ≤ c ≤ max_timeout_fee`.
  The challenger receives one merged output: its remaining reserve less `c`,
  plus the challenge record lovelace, plus `payout`.

### Retention

DA and proof stores retain proof-critical data for at least canonical maturity
plus the measured worst-case proof time and margin. The deployment identity
binds the window. Pruning never removes evidence that is still challengeable,
and an approaching retention deadline raises an alert.

### Proof selection

The rule bundle contains a total, versioned violation-priority table. Proof
selection picks the earliest invalid transition or event, then the table's
stable family order. No provable fault is hidden by a later or less specific
diagnostic.

### Economics

Bond, slash, inactivity penalty, prover reward, fee and collateral
requirements and deadline margins are nonzero, come from the
[economics and margin decision record](../midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md),
and are part of the deployment and release identity. The pooled DA bond
amounts and timing are among them, as deployment-profile and availability
`ParametersV1` values.

### Finality

Finality is depth- and chain-point-based. A deployment declares exactly one
L1-source mode:

- `local_node`: a watcher-operated Cardano full node is the chain-consensus
  authority, and chain-sync supplies roll-forward and rollback events. Ogmios,
  Kupo/Kupmios or db-sync may query or index that same node. Those services are
  one authority, need not be operationally independent, and require no second
  provider. Their network and query/index chain point match the local node;
  stale or mismatched results fail closed.
- `external_providers`: the watcher operates no Cardano node and uses services
  such as Blockfrost or Koios. At least two operationally independent external
  providers with compatible network, chain-point and content observations are
  required. Services sharing one backend or authority do not count twice;
  disagreement quarantines protocol decisions.

Both modes use an explicit confirmation depth and keep state below that depth
pending, so a rollback rewinds it. A rollback of finalized state, possible up
to Cardano's security parameter k (2,160 blocks), is an incident with
automated rewind and replay recovery, not manual state surgery. Indexed
records bind to the actual transaction, output and datum bytes observed
through the selected source mode. Cardano consensus and the deployed
validators establish L1 transition validity; the watcher decodes and indexes
accepted L1 state rather than reimplementing those validators. Fault proofs
adjudicate dishonest L2 operators.

### Block application

Block application follows the canonical deployed V1 rule bundle. The watcher
imports that rule; it keeps no second ordering of its own.

### Capability floor

The capability comparison never uses a weaker testnet ceiling. Fixture
generation uses the accepted Cardano mainnet capability floor
([decision 0001](../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md))
and any more permissive applicable target-network parameter. A smaller testnet
value serves deployment validation and never lowers canonical V1.

## Margin thresholds

Wherever a document requires "measured margin", "substantial margin",
"maturity-window margin" or "bounded resources", the pass/fail thresholds are:

1. **Byte fit:** every serialized publication or proof transaction is at or
   below the deployment's measured `maxTxSize`.
2. **Execution fit:** execution memory and CPU are at or below the deployment's
   measured protocol limits with at least a 20% reserve
   ([`docs/consensus-profile-v1.md`](../consensus-profile-v1.md) §10).
3. **Maturity fit:** the complete measured worst-case correction path (DA
   fetch, evidence construction, every proof step including 32 interactive
   rounds where applicable, settlement, L1 confirmations, rollback allowance
   and removal) completes inside half the canonical maturity window under the
   configured response deadlines.
4. **Resource fit:** live acceptance runs inside the container memory, CPU and
   PID ceilings fixed by the economics and margin decision record. Those
   ceilings contain local acceptance runs only; the same record separately
   sets the production hardware floor, and the acceptance caps are not
   production sizing.

A path that fits its raw limit but not the applicable reserve fails.

### Execution basis

Execution fit is judged against one declared basis: **13,200,000 memory
units** and **8,000,000,000 CPU units**, the 20% reserve off the Cardano
mainnet `maxTxExUnits` caps of 16,500,000 memory and 10,000,000,000 CPU. The
cap comes from the mainnet parameter snapshot in
[decision 0001](../midgard/decisions/0001-cardano-l1-transaction-capability-floor.md),
carried as the supported floor
`MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxMemoryUnits`
(`demo/midgard-core/src/consensus-profile.ts`) and in
[`docs/consensus-profile-v1.md`](../consensus-profile-v1.md) §10. The live cap
is pinned for a release by its target-network parameter binding. In TypeScript
the basis is `SCRIPT_BUDGET_BASIS` in `demo/midgard-core/src/script-budgets.ts`.

### Maturity window

The canonical maturity window for the maturity-fit threshold is the production
block maturity of 7 days (604,800,000 ms), a half-maturity budget of
302,400,000 ms. A scaled acceptance or test window never stands in for it;
short drill windows are drill configuration.

### Economic sufficiency

Bond, slash, inactivity penalty, prover reward, finality and confirmation
depths and availability deadlines come from the economics and margin decision
record and are never chosen per change. The economic sufficiency of a
correction path (fees and collateral against the prover reward) is judged
against the production launch economics (§2.1 of that record, with its
75,000-ADA `fraud_prover_reward`), never the scaled §2.2 drill profile.

### Proof-transaction count cap

No fault proof requires more than 5,000 proof transactions. The ceiling of a
proof's measured cost over the per-transaction usable budget (the
[execution basis](#execution-basis)) stays at or below that cap on both the
memory and the CPU axis.

## Proof-family contract

A fault-proof family is one independently closable proof of one violation.

### Family outputs

A family consists of:

1. A normative rule and violation identifier.
2. A canonical evidence schema with strict TypeScript/Aiken codec agreement.
3. The exact flat blake2b-256 field commitments over canonical enveloped
   preimage bytes and the canonical native codec, per
   [`midgard-tx.md`](midgard-tx.md). No legacy PlutusData or root path, no
   witness-encoding split and no counted field-commitment path remains.
4. Aiken proof steps with positive tests and negative controls showing that
   mutated evidence and a valid block both reject.
5. A maximum and adversarial proof-fit fixture.
6. A catalogue identifier, first-step hash, membership proof, and deployment
   and reference-script records.
7. A DA-first evidence builder using retained authenticated material. Builders
   consume verified `DaPayloadV1` and proof bundles and authenticated L1
   observations, never operator-private REST, DB or file inputs except as
   labelled diagnostics.
8. One resumable prepare/submit command with no hidden manual state.
9. An emulator lifecycle: init, every step, the permanent proof token,
   removal of the fraudulent header or claim, and the correct slash and
   reward.

### Structural N/A

A rule that is structurally enforced elsewhere has no proof family. Instead it
has an adversarial executable test of that enforcement, no unreachable proof
surface, and an `N/A` row in the coverage matrix. Prose alone does not
establish it.
