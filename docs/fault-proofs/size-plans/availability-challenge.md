# Size-fit plan: `availability_challenge.availability_challenge`

Follows [`00-primer.md`](00-primer.md). This is the one contract in the
programme whose validator is a hand-written multi-arm mint/spend script, so it
takes the primer's pattern 4 (redesign) in the shape of the state-queue
precedent: keep a small mint/spend dispatcher and move every minting arm into
an authenticated withdraw-zero rewarding validator. The spend arms stay in the
dispatcher; §3 explains why.

## 1. Identity

| Field                         | Value                                                                                                                                                                                                                                                               |
| ----------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Blueprint titles              | `availability_challenge.availability_challenge.mint`, `.spend`, `.else` (one compiled body)                                                                                                                                                                         |
| Files                         | `onchain/aiken/validators/availability-challenge.ak` (1,529 lines), `onchain/aiken/lib/midgard/availability-challenge.ak` (799 lines)                                                                                                                               |
| Raw size (unapplied)          | 19,927 bytes (reproducible 2026-09-01 build, re-measured in the probe copy)                                                                                                                                                                                         |
| Applied size                  | 20,017 bytes, script hash `8cd5ef370e2dd7e5af3ad3f7d80729cd79bed9c14e0e9f2ce79f47c4` under the node test parameters (`demo/midgard-node/tests/availability-challenge-publication-admission-v1.test.ts`)                                                             |
| Signed publication (measured) | 20,524 / 20,522 bytes (public_testnet_readiness.md Q58 blocker bullet); the ledger rejects at 16,384                                                                                                                                                                |
| Applied parameters            | `hub_oracle_policy_id: PolicyId`, `parameters: availability.ParametersV1` (`response_geometry`, `da_bond_lovelace`, `challenger_bond_lovelace`, five fee ceilings)                                                                                                  |
| Arms                          | mint: `MintBondFromAttestation`, `OpenChallenge`, `SettleTranche`, `CloseChallenge`, `TimeoutChallenge`; spend: `AdvanceTranche`, `ConsumeCarrier`, `Coordinate`                                                                                                    |
| Role names today              | `AvailabilityChallengeSpend`, `AvailabilityChallengeMint` (`REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` in `demo/midgard-sdk/src/reference-scripts.ts`; `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES` in `demo/midgard-core/src/deployment-manifest-identity-v1.ts`) |
| Deployment entries today      | `availabilityChallengeSpend`, `availabilityChallengeMint` (`DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE`; node `deployment-manifest-v1.ts`, `midgard-contracts.ts` `REAL_AVAILABILITY_CHALLENGE_SCRIPT_TITLES`, `contract-deployment-info.ts`)         |
| Publication targets today     | `nodeRuntimeReferenceScriptTargets` publishes both roles; the `da` scope publishes the minting role again (`demo/midgard-node/src/transactions/reference-scripts.ts` lines 1491–1512, 1676–1682, both carrying the `#649` oversize caveat)                          |
| Consumers of the applied hash | `correction_lock.spend` (parameter), `state_queue.mint` (`availability_policy_id`), `state_queue.spend`, `state_queue_yields.remove_unavailable`, `da_attestation` (mint+spend), hub-oracle datum (runtime)                                                         |

Spec anchors: GOAL_SPEC §3.1(5) (bond-backed availability challenge), rows
Q58 (semantics), Q59 (tooling and lifecycle), Q61 (attestation-timeout
removal, sibling path through the same state-queue yields), Q63 (governor
bounds; the bond output value is `da_bond_lovelace`, unaffected here), W30
(typed adapter for the availability challenge), C52 (5,000-transaction cap).
`docs/fault-proofs/architecture.md` §"Trust and release boundaries" and the
readiness Q58 blocker bullet both record the current unpublishability.

## 2. Why it is this size

Aiken inlines the library into a single body, so the monolith pays once for
shared helpers but every arm's exclusive code adds up. Probes were built in
`/tmp/size-probe-ac` (copy deleted afterwards) with the pinned
`v1.1.23-org-5adf7837` compiler, `--env testnet`, after marking the validator's
`fn validate_*` functions `pub` so throwaway `withdraw` validators under
`validators/probe/` could call one arm each. Every probe takes the same three
parameters (`hub_oracle_policy_id`, `ParametersV1`, `own_policy_id`) and an
opaque `Data` redeemer decoded into a small record, so the 168-byte empty
probe is the fixed overhead.

| Probe                                                                                               | Raw bytes | Notes                                                                                             |
| --------------------------------------------------------------------------------------------------- | --------: | ------------------------------------------------------------------------------------------------- |
| baseline monolith `availability_challenge` (mint = spend = else)                                    |    19,927 | matches the blueprint index                                                                       |
| `p0_empty` (parameters + record decode only)                                                        |       168 | fixed probe overhead                                                                              |
| `p1_mint_bond` → `validate_mint_bond_from_attestation`                                              |     5,745 | hub datum + DA attestation datum decode + `commitment_is_canonical_v1` + queue transition         |
| `p2_open` → `validate_open_challenge`                                                               |     7,847 | largest arm: `tranche_initial_lovelaces_v1`, N tranche outputs, terminal output, queue transition |
| `p3_settle` → `validate_settle_tranche`                                                             |     6,945 | `validate_settlement_status` (both `TrancheDatumV1` arms) + `timeout_carrier_lovelace` + fold     |
| `p4_close` → `validate_close_challenge`                                                             |     6,418 | hub datum + queue transition + `published_terminal_commitment_v1`                                 |
| `p5_timeout` → `validate_timeout_challenge`                                                         |     5,841 | hub datum + state-queue mint-redeemer binding                                                     |
| `p6_advance` → `validate_advance_tranche`                                                           |     5,717 | `publication_advances_active_tranche_v1` (Merkle frontier + step accumulator) + carrier chain     |
| `p7_consume` → `validate_consume_carrier`                                                           |       721 |                                                                                                   |
| `p8_coordinate` → `validate_coordinate_spend`                                                       |     1,486 | full `MintRedeemerV1` decoder                                                                     |
| `p9_canonical` → `commitment_is_canonical_v1` alone                                                 |     1,596 | shared by bond, open (via `tranche_initial_lovelaces_v1`), settle, close, timeout                 |
| `p10_hub_and_sq` → `hub.get_datum` + `validate_state_queue_status_transition`                       |     2,811 | shared by bond, open, close                                                                       |
| `p11_yield_handshake` → `state_queue_yield.require_authenticated_zero_yield`                        |       716 | cost of the dispatcher handshake                                                                  |
| `pa_dispatcher_a`: all five mint arms yielded (opaque redeemer), all three spend arms inline        |     7,906 |                                                                                                   |
| `pc_dispatcher_real`: as `pa` but decoding the real `MintRedeemerV1` in the mint handler            |     7,963 | **chosen dispatcher shape**                                                                       |
| `pb_dispatcher_b`: all mint arms and `AdvanceTranche` yielded; `ConsumeCarrier`/`Coordinate` inline |     2,614 | rejected, see §3                                                                                  |
| `pd_bond_timeout`: bond + timeout in one yield                                                      |     8,439 | rejected pairing                                                                                  |
| `pe_settle_close`: settle + close in one yield                                                      |    11,080 | rejected pairing                                                                                  |

Reading the table: the eight arms sum to 40,720 bytes against a 19,927-byte
monolith, so roughly half of every arm is shared library code
(`commitment_is_canonical_v1` with `descriptors_are_canonical_v1`, the
hub-oracle/state-queue transition pair, the asset-name derivations, the
`BondDatumV1`/`TrancheDatumV1`/`TerminalAccumulatorDatumV1` decoders and the
`hash_domain_and_data` accumulators). No single function dominates; the size
is the union of five mint arms that each need a different subset. That rules
out pruning as a sufficient fix and makes per-arm yields the natural cut: each
yield pays the shared code once and stays between 5.7 and 7.9 KB.

Six private functions in the validator are unreachable today
(`present_carrier_indices`, `indices_are_unique_nonnegative`,
`validate_timeout_tranche_inputs`, `contiguous_input_range_contains_ref`,
`indexed_inputs_contain_ref`, `optional_indexed_inputs_contain_ref`). Aiken
does not compile them, so they contribute nothing to the 19,927; they should be
deleted during the split so the file stops suggesting a per-tranche timeout
path that the redeemer no longer exposes.

## 3. Options considered

1. **Prune reachable code.** Rejected as a primary fix. The only redundancy
   found is `commitment_is_canonical_v1` being evaluated twice in `OpenChallenge`
   (once directly through `tranche_initial_lovelaces_v1`, once via the
   `Attested → Challenged` state-queue transition's caller) and the duplicated
   `MintRedeemerV1` decode in `Coordinate`; both are execution costs, not code
   size. Removing the dead functions saves nothing. No arm can be dropped: all
   five mint arms and all three spend arms are required by §3.1(5). Pruning
   cannot recover the ~5,000 bytes needed.
2. **Withdraw-zero yield split (chosen).** Directly mirrors
   `validators/state-queue.ak` + `state-queue-yields.ak`. Each mint arm already
   receives everything it needs as explicit arguments
   (`hub_oracle_policy_id`, `parameters`, `own_policy_id`, `tx`, indices), so
   moving it into a rewarding validator is a parameter-passing exercise with no
   semantic change. The dispatcher keeps the multipurpose mint/spend script so
   the policy id, the bond/tranche/carrier address and every asset-name
   derivation stay identical in shape.
   - _Which arms to yield._ Measured: all mint arms yielded with spend arms
     inline gives a 7,963-byte dispatcher; also yielding `AdvanceTranche` gives
     2,614. Both fit. `AdvanceTranche` stays inline because the publication
     transaction is the byte-critical one: it carries a 14,020-byte chunk inline
     datum (`q58_maximum_14020_byte_publication_with_merkle_proof_applied_fits`)
     and is sized to the 16,384-byte envelope. A yield would add a withdrawal
     entry, a withdraw redeemer and a reference input to that transaction, which
     would force the response geometry (`chunkByteLength`) to be re-measured and
     re-signed as release data, and it would make every one of up to ~4,800
     publications reference two scripts instead of one. Keeping it inline
     changes nothing about the publication transaction except that the referenced
     dispatcher shrinks from 19,927 to ~8,000 bytes.
   - _How many yields._ One per mint arm (five). Pairing was measured and
     rejected: `settle+close` is 11,080 and `bond+timeout` 8,439, both still
     under 15,000, but pairing forces every settlement transaction to reference
     a script 4 KB larger than it needs, and it breaks the one-role-per-arm
     property that makes cross-arm substitution fail on the role name alone.
     Five roles is what the state-queue precedent already deploys.
3. **Multi-transaction chaining.** Rejected. Every arm is already a single
   transaction whose execution fits (largest measured 10.4 M mem, §5); the
   problem is bytes of code, not budget. Chaining would add transactions
   inside the response window for no benefit.
4. **Redesign of the arm boundaries.** Not needed beyond the split. The Q58
   semantics (bond from attestation, open, ordered chunk publication with
   carriers, per-tranche settlement into a terminal accumulator, close, timeout
   with DA-bond slash) are stable and tested; the boundaries stay.

## 4. Chosen design

### New validator list

| Blueprint title                                               | File                                               | Purpose                                                                                                              | Parameters                                                                                      | Role name (auth NFT)                                                  | Manifest contract key                                     | Human target name                           |
| ------------------------------------------------------------- | -------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- | --------------------------------------------------------------------- | --------------------------------------------------------- | ------------------------------------------- |
| `availability_challenge.availability_challenge.mint`/`.spend` | `validators/availability-challenge.ak` (rewritten) | dispatcher: mint arms authenticate one yield; spend arms `AdvanceTranche`, `ConsumeCarrier`, `Coordinate` run inline | `hub_oracle_policy_id`, `parameters: ParametersV1`, **`reference_script_auth_policy_id`** (new) | `AvailabilityChallengeSpend`, `AvailabilityChallengeMint` (unchanged) | `availabilityChallengeSpend`, `availabilityChallengeMint` | unchanged                                   |
| `availability_challenge_yields.bond.withdraw`                 | `validators/availability-challenge-yields.ak`      | `validate_mint_bond_from_attestation`                                                                                | `availability_policy_id`, `hub_oracle_policy_id`, `parameters`                                  | `AvailabilityChallengeBondYield`                                      | `availabilityChallengeBondWithdraw`                       | `availability-challenge bond withdrawal`    |
| `availability_challenge_yields.open.withdraw`                 | same                                               | `validate_open_challenge`                                                                                            | same                                                                                            | `AvailabilityChallengeOpenYield`                                      | `availabilityChallengeOpenWithdraw`                       | `availability-challenge open withdrawal`    |
| `availability_challenge_yields.settle.withdraw`               | same                                               | `validate_settle_tranche`                                                                                            | same                                                                                            | `AvailabilityChallengeSettleYield`                                    | `availabilityChallengeSettleWithdraw`                     | `availability-challenge settle withdrawal`  |
| `availability_challenge_yields.close.withdraw`                | same                                               | `validate_close_challenge`                                                                                           | same                                                                                            | `AvailabilityChallengeCloseYield`                                     | `availabilityChallengeCloseWithdraw`                      | `availability-challenge close withdrawal`   |
| `availability_challenge_yields.timeout.withdraw`              | same                                               | `validate_timeout_challenge`                                                                                         | same                                                                                            | `AvailabilityChallengeExpiryYield`                                    | `availabilityChallengeTimeoutWithdraw`                    | `availability-challenge timeout withdrawal` |

Role-name constants live in a new `lib/midgard/availability-challenge-yield.ak`
(`pub const bond_role`, `open_role`, `settle_role`, `close_role`,
`timeout_role`) next to a re-exported handshake, exactly as
`lib/midgard/state-queue-yield.ak` does for the state queue; the SDK/core
token-name tables must match these strings byte for byte.

The five arm functions move from the validator file into
`lib/midgard/availability-challenge.ak` as `pub fn` (or into a new
`lib/midgard/availability-challenge-arms.ak` if the 799-line library should
stay type-and-accumulator only), unchanged in body. `validate_state_queue_status_transition`,
`validate_da_apply_binding`, `expected_tranche_mint_pairs`,
`validate_initial_tranche_outputs`, `validate_initial_terminal_accumulator_output`,
`validate_settlement_status`, `timeout_carrier_lovelace` and
`validate_state_queue_timeout_binding` move with them; `validate_advance_tranche`,
`validate_consume_carrier` and `validate_coordinate_spend` stay private to the
dispatcher.

### Redeemer and datum ABI deltas

- `MintRedeemerV1`: every constructor gains `yield_to_ref_input_index: Int` as
  its **first** field (same position as the state-queue `MintRedeemer`).
  Constructor indices and all other fields are unchanged, so
  `Coordinate`, `state_queue.spend.availability_status_update_is_authorized_v1`
  and `da_attestation.ApplyToStateQueue` (all of which decode
  `MintRedeemerV1` with `..` patterns) recompile without source changes; their
  compiled decoders change, which is one of the hash ripples in §10.
- New `YieldRedeemerV1 { YieldAvailabilityChallengeV1 }` (fieldless) for the
  five rewarding validators, encoded off-chain as `Data.void()` like
  `encodeStateQueueYieldRedeemerV1`.
- `SpendRedeemerV1`, `BondDatumV1`, `TrancheDatumV1`, `PublicationDatumV1`,
  `TerminalAccumulatorDatumV1`, `StateQueueStatusV1`, every asset-name
  derivation and every accumulator domain string are **unchanged**. The
  watcher's decoded state (`daAvailability` in
  `production-state-queue-observation-v1.ts`, `da-availability-state-v1.ts`)
  is unaffected.

### Handshake (kept exactly as the primer requires)

Dispatcher `mint(redeemer: MintRedeemerV1, own_policy_id, tx)`:

```
let (role, yield_index) = when redeemer is {
  MintBondFromAttestation { yield_to_ref_input_index, .. } -> (yield.bond_role, yield_to_ref_input_index)
  OpenChallenge { .. }          -> (yield.open_role, ..)
  SettleTranche { .. }          -> (yield.settle_role, ..)
  CloseChallenge { .. }         -> (yield.close_role, ..)
  TimeoutChallenge { .. }       -> (yield.timeout_role, ..)
}
expect _ = state_queue_yield.require_authenticated_zero_yield(
  tx.reference_inputs, tx.withdrawals, tx.redeemers,
  reference_script_auth_policy_id, role, yield_index)
True
```

Each yield `withdraw(_r: YieldRedeemerV1, _cred, tx)`:

```
expect OpenChallenge { hub_oracle_ref_input_index, bond_input_index, .. }: MintRedeemerV1
  = utils.get_unique_mint_redeemer(tx.redeemers, availability_policy_id)
arms.validate_open_challenge(hub_oracle_policy_id, parameters, availability_policy_id, tx, ...)
```

1. **Dispatch uniqueness.** `get_unique_mint_redeemer` requires exactly one
   `Mint(availability_policy_id)` redeemer in the transaction, so one
   withdrawal discharges exactly one availability mint; `require_authenticated_zero_yield`
   requires exactly one withdrawal for the yield's script hash with a unique
   withdraw redeemer. The spend side is unchanged: `Coordinate` still binds
   every availability input to the single mint redeemer by index and requires
   its own out-ref to be one of the declared inputs, and `ConsumeCarrier`
   binds the carrier to the `AdvanceTranche` thread redeemer.
2. **Role authentication.** The indexed reference input must carry exactly one
   NFT under `reference_script_auth_policy_id` whose name equals the arm's role
   and must expose the script hash that the zero withdrawal names; nothing is
   trusted from the redeemer except the index.
3. **Cross-arm substitution.** An `OpenChallenge` redeemer with a settle yield
   fails in the dispatcher on `candidate_role == open_role`; the yields
   themselves also fail closed because each `expect`s its own constructor.
4. **Script substitution.** A withdrawal from a different script that happens
   to carry the right role NFT fails on `Script(candidate_hash) == script_hash`
   in the handshake; role NFTs are only minted by the deployment publication.
5. **Output-state re-derivation.** Every arm already recomputes the expected
   continuation datum (`Available`/`ChallengedBond`, every `Active` tranche
   datum and the `TerminalAccumulatorDatumV1` on open, the folded terminal
   accumulator on settle, the `Published { terminal_commitment }` queue status
   on close) and compares by exact `Data` equality, and every arm checks the
   exact mint pairs for `own_policy_id`. Because `own_policy_id` is now the
   yield parameter `availability_policy_id`, the yield can only be satisfied by
   the dispatcher whose hash it was applied with.
6. **What an attacker gains if a yield is omitted.** Nothing: the dispatcher
   mint handler returns `True` only after the handshake, and there is no
   fallback arm. If a _deployment_ omits publishing one yield, that arm is
   simply unavailable (the lifecycle stalls, funds stay locked under the
   dispatcher), which the publication-fit test and manifest verification make
   impossible to miss. If the yield reference UTxO is later spent (auth policy
   timelock), the arm stops working until republished; this is the same
   operational property the state-queue yields already have.
7. **Parameter direction.** The dispatcher carries the auth policy id and role
   names in; the yields carry the dispatcher hash in. Deployment order is
   therefore hub oracle → dispatcher (needs the auth policy id already known)
   → five yields → `correction_lock`, `state_queue.*`, `da_attestation` (all
   parameterised by the dispatcher hash), matching the existing order in
   `midgard-contracts.ts` where `buildRealAvailabilityChallengeValidator` runs
   before `buildRealCorrectionLockValidator`.

## 5. Size and budget projection

Sizes are raw unapplied; parameter application adds ~73 bytes per script
(three parameters here, so allow ~100). Expected yield sizes are the probe
sizes plus the delta between an opaque record decode and
`get_unique_mint_redeemer` + a full `MintRedeemerV1` decode; the same delta in
the dispatcher measured 57 bytes (`pa` → `pc`), the state-queue yields pay
roughly 300 bytes for it, so the projection allows +400.

| Script                    | Measured basis | Projected raw | Fits ≤ 15,000 |
| ------------------------- | -------------: | ------------: | ------------- |
| dispatcher (mint + spend) |          7,963 |        ~8,000 | yes           |
| bond yield                |          5,745 |        ~6,150 | yes           |
| open yield                |          7,847 |        ~8,250 | yes           |
| settle yield              |          6,945 |        ~7,350 | yes           |
| close yield               |          6,418 |        ~6,800 | yes           |
| timeout yield             |          5,841 |        ~6,250 | yes           |
| **family total**          |                |       ~42,800 |               |

Referenced script bytes per transaction (raw, from the current blueprint:
`state_queue.mint` 4,828, `state_queue.spend` 1,657,
`state_queue_yields.remove_unavailable` 5,710, `correction_lock.spend` 5,821,
`da_attestation` 9,121; hub oracle and DA params are datum reference inputs,
not scripts):

| Transaction                                    | Today (monolith) | After split                                                | Fee band (25 KiB tiers, `minFeeRefScriptCostPerByte`) |
| ---------------------------------------------- | ---------------: | ---------------------------------------------------------- | ----------------------------------------------------- |
| bond mint = DA attestation apply               |           35,533 | 8,000 + 6,150 + 9,121 + 4,828 + 1,657 ≈ **29,760**         | tier 2 (was tier 2); ~5.8 KB cheaper                  |
| open                                           |           21,584 | 8,000 + 8,250 + 1,657 ≈ **17,900**                         | tier 1 (was tier 1)                                   |
| publish (`AdvanceTranche` + `ConsumeCarrier`)  |           19,927 | **8,000**                                                  | tier 1                                                |
| settle (three `Coordinate` spends + mint)      |           19,927 | 8,000 + 7,350 ≈ **15,350**                                 | tier 1                                                |
| close                                          |           21,584 | 8,000 + 6,800 + 1,657 ≈ **16,460**                         | tier 1                                                |
| timeout + `RemoveUnavailableBlockAfterTimeout` |           37,943 | 8,000 + 6,250 + 4,828 + 1,657 + 5,710 + 5,821 ≈ **32,270** | tier 2 (was tier 2); ~5.7 KB cheaper                  |

Every transaction references fewer bytes than today because the 19,927-byte
monolith was referenced by all of them. Nothing approaches the 200 KiB
`maxRefScriptSizePerTx` cap.

Execution budget. `aiken check -m q58_` in the probe copy (46 tests, all
pass) reports the following upper bounds; they include the test's fixture
construction, and the "applied topology" tests run the mint arm plus every
`Coordinate` spend of the same transaction:

| Shape (existing test)                                                     |     mem |    cpu |
| ------------------------------------------------------------------------- | ------: | -----: |
| `q58_maximum_16_tranche_open_applied_topology_fits`                       |  8.71 M | 3.74 G |
| `q58_maximum_16_tranche_published_settlement_applied_topology_fits`       | 10.12 M | 3.69 G |
| `q58_maximum_16_tranche_partial_timeout_settlement_applied_topology_fits` | 10.41 M | 3.92 G |
| `q58_maximum_16_tranche_final_close_applied_topology_fits`                |  5.16 M | 2.34 G |
| `q58_maximum_16_tranche_final_timeout_applied_topology_fits`              |  4.01 M | 1.41 G |
| `q58_maximum_14020_byte_publication_with_merkle_proof_applied_fits`       |  1.31 M | 0.58 G |
| `q58_mint_bond_accepts_exact_attestation_and_queue_transition`            |  1.74 M | 0.90 G |

The split adds, per transaction, one handshake (a 716-byte function: one
reference-input lookup, one token-map projection, one withdrawal filter, one
redeemer filter) and one extra `MintRedeemerV1` parse in the yield; the
state-queue yields show this at well under 0.5 M mem. Projected worst case is
the 16-tranche partial-timeout settlement at ≈ 10.9 M mem, inside the
13,200,000-unit §3.3 basis (16.5 M less the 20 % reserve) with ≈ 2.3 M
headroom, and far inside the 10 G cpu limit. The open transaction additionally
runs `state_queue.spend` (`AvailabilityStatusUpdate`, one redeemer decode) and
the bond-mint transaction runs `da_attestation` and `state_queue.mint`; both
are already measured in today's emulator flows and are unchanged. The
projection must be replaced by the emulator measurement in §7 before the plan
is marked done.

## 6. Off-chain work

What exists today (verified by reading the sources):

- **Datums, redeemers, planning.** `demo/midgard-sdk/src/availability-challenge-v1.ts`
  (2,585 lines) has every schema (`DaAvailabilityMintRedeemerV1Schema`,
  `DaAvailabilitySpendRedeemerV1Schema`, bond/tranche/terminal/publication
  datums), the commitment builder and canonical asserts, tranche layout and
  funding planners (`deriveDaAvailabilityTrancheLayoutV1`,
  `planDaAvailabilityTrancheFundingV1`, `buildDaAvailabilityChallengeDatumPlanV1`,
  `planDaAvailabilityPublicationsV1`, `advanceDaAvailabilityTrancheV1`,
  `planDaAvailabilitySettlementV1`, `planDaAvailabilityTerminalRefundV1`,
  `assertDaAvailabilityTerminalReceiptsV1`,
  `reconstructDaAvailabilityPayloadV1`, ...). These are pure helpers; none
  builds a transaction.
- **One transaction builder**, for `MintBondFromAttestation` only, embedded in
  `incompleteApplyDaAttestationToStateQueueTxProgram`
  (`demo/midgard-sdk/src/da-attestation.ts` lines 727–1002), driven by the
  node's `attestStateQueueOnceProgram` (`demo/midgard-node/src/transactions/da-attestation.ts`)
  and by `demo/da-committee-node/src/coordinator/tx-builders.ts`. It resolves
  the minting reference script through `fetchDaAttestationReferenceScripts`
  (`DaAttestationReferenceScripts.availabilityChallengeMinting`).
- **No builders** for `OpenChallenge`, `AdvanceTranche`/`ConsumeCarrier`,
  `SettleTranche`, `CloseChallenge`, `TimeoutChallenge`, nor for the
  state-queue `RemoveUnavailableBlockAfterTimeout` that the timeout arm
  requires in the same transaction (`demo/midgard-sdk/src/state-queue.ts`
  only has the redeemer schema at line 200; the unattested-removal builders
  `incompletePruneTimedOutBlockDescendantTxProgram` and
  `incompleteRemoveUnattestedHeadAfterTimeoutTxProgram` are the templates).
- **No node commands** for challenge/respond/settle/close/timeout (Q59). The
  node's `retention-check` hardcodes `availabilityChallengeState: "not_deployed"`,
  `daPayloadTerminalOutcomes.ts` pins `availabilityChallengeCapability: "missing"`,
  and `e2e-state-correction-local-authority.ts` returns
  `availabilityChallengeCapability: "missing"`, so the
  `availability_challenge_readiness` gate in `e2e-state-correction-acceptance.ts`
  is always `blocked`.
- **Watcher.** `deployment-identity.ts` authenticates the manifest's
  `availabilityChallenge` parameters (`watcherDeploymentAvailabilityChallengeAuthorityV1`),
  `production-state-queue-observation-v1.ts` decodes the node's
  `daAvailability` status, and `production-prover-funding-calculation-v1.ts`
  accepts a `da_availability_lifecycle` funding scope (defined in
  `demo/midgard-fault-proofs/src/workflow/production-funding-requirements-v1.ts`
  with the `availability_carrier` semantic role and admitted by
  `admitProductionAvailabilityFundingRequirementsV1`). There is **no**
  availability adapter: nothing indexes bond/tranche/carrier UTxOs, decides to
  open, publishes chunks, settles, closes or times out. The state-queue
  observation already exposes `Challenged` blocks (merge is refused by
  `daAvailabilityStateQueueStatusPermitsMergeV1`).
- **Roles/manifest/inspection.** Two roles wired everywhere listed in §1;
  `demo/midgard-node/tests/deployment-manifest-v1.test.ts`,
  `contract-deployment-info.test.ts`, `midgard-contracts.test.ts` and
  `demo/midgard-core/tests/deployment-manifest-identity-v1.test.ts` pin the
  current role vectors; `demo/midgard-fault-proofs/tests/support/emulator/contracts.ts`
  uses `base.escapeHatch.policyId` as a stand-in availability policy and
  `validators.ts` maps `availabilityChallenge` to an always-succeeds script.
- **Emulator.** No Lucid Evolution lifecycle test for the challenge exists in
  `demo/midgard-sdk/tests`, `demo/midgard-node/tests` or
  `demo/midgard-fault-proofs/tests`. The bond mint is exercised through
  `attestQueuedStateQueueHeader` in `demo/midgard-node/tests/deposit-flow-emulator-shared.ts`
  with `maxTxSize` raised to 65,536 (also in `initialization-emulator.test.ts`);
  `availability-challenge-publication-admission-v1.test.ts` asserts the
  current _failure_ to publish; `scratch-cg1-publication-fit.test.ts` is
  `it.skip` pending #649.

Work items (all new unless marked "change"):

1. **SDK contracts.** `demo/midgard-sdk/src/common.ts`: add
   `AvailabilityChallengeYieldValidatorsV1 { bond, open, settle, close, timeout: WithdrawalValidator }`
   and `AvailabilityChallengeValidatorV1 = AuthenticatedValidator & { yields }`;
   change `MidgardValidators.availabilityChallenge` to it. Node
   `midgard-contracts.ts` (change): `buildRealAvailabilityChallengeValidator`
   takes `referenceScriptAuthPolicyId` as third parameter and builds the five
   yields with `buildYield` under new
   `REAL_AVAILABILITY_CHALLENGE_SCRIPT_TITLES.{bondYield,…}`; `authenticatedValidatorFromManifest`
   path adds five `withdrawalValidatorFromManifest` entries. Fault-proofs
   emulator `contracts.ts`/`validators.ts` (change): keep the stand-in but
   give it a `yields` shape so the type compiles.
2. **Reference-script roles.** Add the five human names → token names to
   `REFERENCE_SCRIPT_AUTH_TOKEN_NAMES` (SDK) and
   `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_TOKEN_NAMES` (core), the five
   contract keys to `DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE`
   (core and node), the node publication targets in
   `nodeRuntimeReferenceScriptTargets` (both the node-runtime list and the
   `da` scope, which must also publish the bond yield) and the
   `contract-deployment-info.ts` records; remove the two `#649` caveat
   comments. Update the pinned vectors in the four tests listed above.
3. **Reward-account registration.** Withdraw-zero requires the yield's stake
   credential to be registered on a real ledger. Generalise
   `ensurePhasMembershipRewardAccountRegisteredProgram`
   (`demo/midgard-node/src/transactions/phas-membership-registration.ts`) into
   an `ensureScriptRewardAccountRegisteredProgram` and run it for the five new
   credentials during initialization. **Unverified:** no registration exists
   for the five state-queue yields either and their emulator suites pass, so
   either the Lucid emulator does not enforce it or the state-queue path has
   the same latent gap; the plan's emulator scenario must register explicitly
   and assert idempotence so the real-L1 behaviour is covered.
4. **Builders** in a new `demo/midgard-sdk/src/availability-challenge-tx-v1.ts`,
   each returning an incomplete `TxBuilder` in the style of
   `incompleteApplyDaAttestationToStateQueueTxProgram` and each attaching its
   yield with `.readFrom([yieldRef])` plus `.withdraw(scriptRewardAddress(network, yield.withdrawalScript), 0n, Data.void())`
   and setting `yield_to_ref_input_index` via `requireReferenceInputIndex`:
   - `incompleteOpenAvailabilityChallengeTxProgram(lucid, contracts, { hubOracleRefInput, bondUtxo, challengerUtxo, stateQueueTarget, challenger, referenceScripts, validityRange })`
     — three inputs exactly, `bond_output`, N tranche outputs from
     `planDaAvailabilityTrancheFundingV1`, the terminal accumulator output, the
     `Coordinate` spend redeemer on the bond and `AvailabilityStatusUpdate` on
     the queue node, `.addSignerKey(challenger)`, fee ≤ `max_open_fee_lovelace`
     asserted from the completed transaction.
   - `incompletePublishAvailabilityTrancheChunkTxProgram(lucid, contracts, { trancheUtxo, previousCarrierUtxo?, publication: DaAvailabilityPublicationDatumV1, referenceScripts })`
     — `AdvanceTranche` on the thread, `ConsumeCarrier` on the previous
     carrier, two outputs, no mint, `validTo ≤ response_deadline`.
   - `incompleteSettleAvailabilityTrancheTxProgram(lucid, contracts, { bondRefInput, terminalUtxo, trancheUtxo, carrierUtxo?, referenceScripts, validityRange })`
     — `SettleTranche` mint, `Coordinate` on every availability input, one
     output from `planDaAvailabilitySettlementV1`.
   - `incompleteCloseAvailabilityChallengeTxProgram(...)` and
     `incompleteTimeoutAvailabilityChallengeTxProgram(...)`; the timeout
     builder composes with a new
     `incompleteRemoveUnavailableBlockAfterTimeoutTxProgram` in
     `state-queue.ts` (both `RemoveTimedOutHead` and
     `PruneTimedOutBlockDescendant` approaches, the `correction_lock`
     acquire, the `remove_unavailable` state-queue yield witness, and
     `state_queue_mint_redeemer_index` bound into the availability redeemer).
   - Change `incompleteApplyDaAttestationToStateQueueTxProgram` to attach the
     bond yield; extend `DaAttestationReferenceScripts` with
     `availabilityChallengeBondYield` and `fetchDaAttestationReferenceScripts`
     accordingly; mirror in `da-committee-node/src/coordinator/tx-builders.ts`.
5. **Submit commands** (Q59) in `demo/midgard-node/src/transactions/availability-challenge.ts`
   and `src/index.ts`: `da-challenge-open --header-hash`,
   `da-challenge-respond --header-hash [--tranche]` (publishes the next
   chunk from the retained `DaPayloadV1`, resumable),
   `da-challenge-settle --header-hash`, `da-challenge-close --header-hash`,
   `da-challenge-timeout --header-hash` (timeout + removal). Each resolves
   reference scripts by the new names, refuses when the manifest lacks the
   roles, and replaces the `"not_deployed"` / `"missing"` capability pins in
   `retention-check.ts`, `daPayloadTerminalOutcomes.ts` and
   `e2e-state-correction-local-authority.ts` with authenticated manifest
   presence (`authenticated_deployed`) so the acceptance gate can be satisfied.
6. **Funding requirements.** Add concrete rows to
   `production-funding-requirements-v1.ts` for the `da_availability_lifecycle`
   scope: challenger bond + `max_open_fee_lovelace`; per-publication
   `max_publication_fee_lovelace` × `maximumDaAvailabilityPublicationCountV1`;
   settlement, close and timeout fee ceilings; collateral; and the five yield
   reference inputs' min-Ada as deployment (not prover) cost. The watcher's
   `production-prover-funding-calculation-v1.ts` already consumes this scope.
7. **Watcher adapter** (W30, "observes but does not own liveness" per Q61
   applies only to attestation timeout; the challenge is a watcher action):
   `demo/midgard-watcher/src/availability-challenge-indexer-v1.ts` (bond,
   tranche, carrier and terminal UTxO index keyed by `challenge_asset_name`,
   decoded with the SDK parsers, rollback-safe like `state-queue-indexer.ts`)
   and `production-availability-challenge-adapter-v1.ts` (decides open when
   retained-DA retrieval fails after attestation, drives settle/close/timeout,
   never publishes chunks — that is the accountable DA signers' job). Wire
   through `production-fault-proof-supervisor-v1.ts` and the operations HTTP
   surface; no operator-local inputs.
8. **Codec changes.** None in `midgard-core`/`midgard-validation`; the only
   ABI delta is the SDK `DaAvailabilityMintRedeemerV1Schema` field.
9. **Docs.** Update the readiness Q58 bullet, `architecture.md` trust
   paragraph, the `#649` comments in `reference-scripts.ts`,
   `deposit-flow-emulator-shared.ts`, `initialization-emulator.test.ts`,
   `scratch-cg1-publication-fit.test.ts`, and `docs/fault-proofs/size-plans/README.md`
   strategy cell.

## 7. Emulator scenario tests

New file `demo/midgard-node/tests/availability-challenge-lifecycle-v1.test.ts`
(node package because `loadRealMidgardContractsForTest` and the real
attestation flow live there), using `VAN_ROSSEM_TRANSACTION_LIMITS`
(`maxTxSize` 16,384, `maxTxExMem` 16,500,000, `maxTxExSteps` 10,000,000,000)
with **no** raised envelope, and `TEST_AVAILABILITY_PARAMETERS_V1`
(`chunkByteLength` 14,020, `trancheByteLength` 4 MiB, `maxTrancheCount` 16,
bonds 10,000 ADA). Fixtures: a 2-of-2 committee as in
`deposit-flow-emulator-shared.ts`; payload A of 3 chunks (≈ 42 KB, one
tranche, small response class, 1 h window); payload B of two tranches (4 MiB +
1 chunk) whose second tranche is left partially published; commitment C with
16 descriptors (payload_byte_length 64 MiB) whose bytes are never published.

1. **Publication fit** — `it("publishes the dispatcher and every yield under the real L1 envelope")`:
   `publishPlainReferenceScriptUtxo` for all six scripts without `oversized`,
   assert `completeSignedBytes < 16_384` and
   `SDK.assertReferenceScriptRawBodiesFitL1EnvelopeV1(nodeRuntimeReferenceScriptTargets(contracts))`
   does not throw. Invert `availability-challenge-publication-admission-v1.test.ts`
   (rename to "…admits…", pin the six new hashes and sizes). Un-skip
   `scratch-cg1-publication-fit.test.ts` and restore `maxTxSize` to
   `PROTOCOL_PARAMETERS_DEFAULT.maxTxSize` in `deposit-flow-emulator-shared.ts`
   and `initialization-emulator.test.ts`.
2. **Positive lifecycle through award (payload A)** — commit → attest
   (`attestStateQueueOnceProgram`, bond minted through the bond yield; assert
   queue status `Attested`) → open (assert 1 tranche + terminal outputs,
   status `Challenged`, merge refused by `merge-readiness`) → three
   `respond` publications (assert `Receipt`, carrier chain) → settle (assert
   `folded_terminal_accumulator` equals `foldDaAvailabilityTerminalAccumulatorV1`)
   → close (assert DA refund to `bond_owner`, challenger refund equals
   `remaining_challenger_lovelace − fee`, status `Published { terminal_commitment }`,
   merge permitted). Record `mem/cpu` of every transaction via the harness
   measurement and assert each ≤ 13,200,000 mem.
3. **Timeout with DA-bond slash (payload B)** — open two tranches, publish
   tranche 0 fully, publish one chunk of tranche 1, advance the clock past
   `response_deadline`, settle tranche 0 (Receipt) and tranche 1
   (`TimedOutTranche` with the partial carrier), then `timeout` +
   `RemoveUnavailableBlockAfterTimeout` in one transaction: assert both
   `da_bond_lovelace` and the challenger refund go to the challenger, three
   burns, the block leaves the queue and the correction lock releases.
4. **Maximum shape (commitment C)** — open with 16 tranches (19 outputs;
   assert the completed transaction bytes < 16,384 and mem ≤ 13.2 M), settle
   all 16 as unpublished timed-out tranches (no carriers), timeout. This is
   the emulator counterpart of the four `q58_maximum_16_tranche_*` Aiken
   tests and pins the worst-case referenced bytes and fee of §5.
5. **Valid-block negative at the same frontier** — after payload A is fully
   published and settled, a `timeout` attempt rejects (`has_timed_out_tranche == False`),
   and a second `open` on the `Published` block rejects
   (`Attested → Challenged` transition unavailable).
6. **Handshake negatives** (each expected to fail on-chain via
   `expectOnchainRefusalV1`): open with the settle yield's reference input and
   withdrawal (cross-arm role); open with the correct role NFT but a
   withdrawal from `state_queue_yields.remove_unavailable` (script
   substitution); open with the yield reference input but no withdrawal
   (omitted yield); withdrawal of 1 lovelace (non-zero); two availability
   mints in one transaction (unique mint redeemer); publication with a wrong
   `chunk_hash`, wrong `chunk_index`, and past the deadline; settle with a
   substituted carrier out-ref; `Coordinate` spending an undeclared bond
   input.
7. **Cancel/resume** — the family has no cancel; resume is covered by
   restarting `da-challenge-respond` mid-tranche (the next chunk is derived
   from the on-chain `Active` datum, not local state) and by re-running the
   watcher indexer from genesis over the emulator ledger and asserting the same
   `challenge_asset_name` → state map (Q59 restart/reconcile).

Existing helpers to extend: `publishStateQueueYieldReferenceScriptV1`
(`demo/midgard-fault-proofs/tests/support/emulator/reference-scripts.ts`) gets
an availability sibling `publishAvailabilityChallengeYieldReferenceScriptV1`;
`state-queue-yield-publication-admission-v1.test.ts` gets a sibling
`availability-challenge-yield-publication-admission-v1.test.ts` in the same
package once the fault-proofs emulator stops using the always-succeeds
stand-in.

## 8. Aiken tests

`validators/availability-challenge.test.ak` (34 tests today) keeps every
`q58_*` test; the helpers `validates_close(tx, redeemer)`,
`validates_coordinate_input(s)` and `validates_maximum_publication` change so
that a mint arm is exercised by calling the yield's `withdraw` handler with the
transaction carrying the dispatcher's mint redeemer, and the dispatcher's
`mint` handler with a fixture reference input (role NFT + `reference_script`
hash) and zero withdrawal. New tests:

- `q58_dispatcher_accepts_every_arm_with_its_own_yield` (five arms, one each).
- `q58_dispatcher_rejects_cross_arm_role_substitution` `fail` (open redeemer,
  settle role NFT).
- `q58_dispatcher_rejects_withdrawal_script_substitution` `fail`.
- `q58_dispatcher_rejects_missing_zero_withdrawal` `fail` and
  `q58_dispatcher_rejects_nonzero_withdrawal` `fail`.
- `q58_dispatcher_rejects_second_yield_redeemer` `fail` (two withdraw
  redeemers for the same script).
- `q58_yield_rejects_second_availability_mint_redeemer` `fail`
  (`get_unique_mint_redeemer` singleton).
- `q58_yield_rejects_wrong_constructor` `fail` per yield (five).
- `q58_yield_rejects_substituted_availability_policy` `fail` (yield applied
  with another policy id sees mismatched mint pairs).
- Property test `q58_yield_index_out_of_range_rejects` over
  `yield_to_ref_input_index` with `fuzz.int_between`.
- Re-run the four `q58_maximum_16_tranche_*_applied_topology_fits` and record
  the new `mem/cpu` in the plan.

New `validators/availability-challenge-yields.test.ak` holds the fixtures
shared by those tests (`yield_reference_input(role, script_hash)`,
`zero_withdrawal(script_hash)`), modelled on the tests at the bottom of
`lib/midgard/state-queue-yield.ak`.

## 9. Verification commands

Run from `onchain/aiken` on the working tree (not a probe copy):

```bash
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken build --env testnet
node -e 'const b=require("./plutus.json");for(const v of b.validators)if(/^availability_challenge/.test(v.title)&&!/\.else$/.test(v.title))console.log(v.title,Buffer.from(v.compiledCode,"hex").length)'
# expected: 7 lines (dispatcher mint+spend, five yields), every size <= 15000, dispatcher ~8,000
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check --env testnet -m q58_
# expected: 46 existing + >=16 new tests, 0 failures
/home/gumbo/.aiken/versions/v1.1.23-org-5adf7837/bin/aiken check --env testnet -m authenticated_zero_yield
# expected: 4 passing (unchanged handshake)
```

From `demo` (pnpm, Node `/home/gumbo/.nvm/versions/node/v22.22.2/bin`):

```bash
pnpm --filter @al-ft/midgard-core test -- deployment-manifest-identity-v1        # role vectors updated
pnpm --filter @al-ft/midgard-sdk test -- reference-scripts availability-challenge-v1 da-attestation
pnpm --filter midgard-node test -- availability-challenge-publication-admission-v1 scratch-cg1-publication-fit availability-challenge-lifecycle-v1 deployment-manifest-v1 contract-deployment-info midgard-contracts initialization-emulator
# expected: scratch-cg1 no longer skipped; admission test asserts fit; lifecycle 7 scenarios pass under 16,384 / 16.5M / 10G
pnpm --filter @al-ft/midgard-fault-proofs test -- zz610-compiled-script-arity state-queue-yield-publication-admission-v1
# expected: arity table sees availability_challenge with 3 params and five 3-param yields
pnpm --filter midgard-watcher test -- availability-challenge-indexer-v1 production-state-queue-observation-v1
```

Blueprint identity: one regeneration; `deployment-manifest-v1.test.ts` and
`midgard-contracts.test.ts` pin the new blueprint SHA-256, applied hashes and
catalogue root once (§10).

## 10. Ordering and dependencies

- **Shared entry points.** Reuses `state_queue_yield.require_authenticated_zero_yield`
  and `utils.get_unique_mint_redeemer` unchanged; no library shared with the
  validation-trace or transition-trace plans is touched, so this plan is
  independent of all 49 others at the source level.
- **Parameter ripple.** The dispatcher hash changes (new code and a third
  parameter), so every consumer of `availability_policy_id` re-applies:
  `correction_lock.spend`, `state_queue.mint`, `state_queue.spend`,
  `state_queue_yields.remove_unavailable`, `da_attestation` (mint+spend). The
  `MintRedeemerV1` field addition also changes the compiled decoders in
  `state_queue.spend` and `da_attestation` independently of parameters. The
  hub-oracle datum carries these hashes at runtime, and the catalogue root is
  re-pinned with the whole programme's single blueprint regeneration, as the
  primer requires; do not re-pin for this plan alone.
- **Landing order inside the programme.** No other plan depends on this one.
  It must land in the same blueprint as the others only to share the one
  catalogue-root re-pin. Within this plan: Aiken split → SDK types/roles/
  builders → node contracts/manifest/publication → tests un-skipped →
  commands → watcher adapter. The watcher adapter and the funding rows can
  follow the on-chain landing in a later change without blocking the size fix.
- **External coordination.** `da-committee-node` consumes the changed
  attestation-apply builder and must be released together with the SDK.

## 11. Risks

- **Budget overrun.** The 16-tranche partial-timeout settlement is the tightest
  shape at ≈ 10.4 M mem before the handshake; projected ≈ 10.9 M against the
  13.2 M basis. If the emulator measurement exceeds the basis, the mitigation
  is to drop the duplicate `commitment_is_canonical_v1` evaluation in the
  settle yield (the bond datum was already validated at open) — a pure
  execution saving with no ABI change. The open transaction at 16 tranches
  (19 outputs) must also be re-measured for bytes: it carries 17 inline datums
  and was previously only measured in Aiken, not as a signed transaction.
- **Publication transaction bytes.** Untouched by design (§3), but any
  reviewer proposing to yield `AdvanceTranche` must re-measure the 14,020-byte
  chunk publication against 16,384 first.
- **ABI churn.** `yield_to_ref_input_index` in every mint constructor changes
  the SDK schema, the attestation-apply builder in three packages, the
  state-queue spend decoder and the DA-attestation decoder. All are in one
  repository except the committee node's consumption of the SDK, which is
  versioned with it. No retained data (bond/tranche/carrier datums, queue
  status) changes, so no migration.
- **Reward-account registration** (§6 item 3) is unverified for the emulator
  and required on L1; the plan makes the lifecycle test register explicitly so
  a missing registration fails locally rather than on preprod.
- **Deployment surface.** Five more reference-script UTxOs (min-Ada each,
  timelocked auth NFTs) and five more stake registrations (2 ADA deposit
  each) in the initialization budget; `l1-provider-preflight` and the
  funding-target calculators must account for seven availability scripts.
- **Spec conflicts.** None found: §3.1(5) and Q58 constrain semantics, not
  script layout; Q59 explicitly asks for the commands, watcher adapter and
  emulator lifecycle this plan adds; W30 forbids operator-local inputs, which
  the adapter design respects. The readiness document and `architecture.md`
  must be updated in the same change so they stop describing the monolith as
  unpublishable.
- **Dead code removal.** Deleting the six unreachable timeout-path helpers is
  size-neutral and safe, but must not be confused with removing the
  per-tranche timeout semantics, which live in `validate_settlement_status`
  and stay.

Done criterion (primer): all six bodies ≤ 15,000 raw; each publishes in the
emulator without `oversized` and passes
`assertReferenceScriptRawBodiesFitL1EnvelopeV1`; the seven lifecycle scenarios
pass under the Van Rossem limits; `scratch-cg1-publication-fit.test.ts` runs
un-skipped; the catalogue root is re-pinned once with the programme.
