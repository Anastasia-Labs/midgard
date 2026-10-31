# Public testnet parameters

Status: Reference and review. Reviewed 2026-10-27 against the
`preprod-public` profile. The value table is generated from the profile and
checked in CI; the prose is reviewed by hand.

This page lists every parameter of the P1 public preprod deployment
(`preprod-public`): its value and unit, where it is set, what it governs, and
the decision or measurement behind it. It covers the deployment profile
[`preprod-public.yaml`](../config/deployments/preprod-public.yaml) and the
values set outside it (node defaults and deploy-time manifest values). The
derivations and the validation rules each profile must pass are in the
[deployment profiles guide](../config/deployments/README.md); the compiled
consensus limits are in the [consensus profile](consensus-profile-v1.md).
Launch readiness is tracked in [public-testnet readiness](public_testnet_readiness.md).

To change a value, edit the profile, then regenerate the environments and this
table:

```sh
# From the repository root; preprod-testing stays the selected profile.
pnpm --dir demo deployment:generate preprod-testing
pnpm --dir demo/midgard-core run docs:public-testnet-parameters:sync
```

The sync command fails when this page's prose never names a parameter the
profile sets, so a new parameter needs its entry below.

## Profile values

Durations are in milliseconds and amounts in lovelace (1 ADA = 1,000,000
lovelace); the readable column converts them. "Set at" links the profile line.

<!-- BEGIN PUBLIC_TESTNET_PARAMETERS_GENERATED: do not edit -->

| Parameter                                            | Value                                                      | Unit              | Readable    | Set at                                                 |
| ---------------------------------------------------- | ---------------------------------------------------------- | ----------------- | ----------- | ------------------------------------------------------ |
| `name`                                               | `preprod-public`                                           | text              |             | [L1](../config/deployments/preprod-public.yaml#L1)     |
| `network`                                            | `Preprod`                                                  | text              |             | [L2](../config/deployments/preprod-public.yaml#L2)     |
| `l1_finality.confirmation_depth`                     | `30`                                                       | blocks            |             | [L4](../config/deployments/preprod-public.yaml#L4)     |
| `l1_finality.commit_event_depth`                     | `22`                                                       | blocks            |             | [L7](../config/deployments/preprod-public.yaml#L7)     |
| `l1_finality.security_parameter`                     | `2160`                                                     | blocks            |             | [L9](../config/deployments/preprod-public.yaml#L9)     |
| `l1_finality.active_slot_coeff`                      | `0.05`                                                     | ratio             |             | [L10](../config/deployments/preprod-public.yaml#L10)   |
| `l1_finality.slot_length_ms`                         | `1000`                                                     | ms                | 1 s         | [L11](../config/deployments/preprod-public.yaml#L11)   |
| `timing.block_maturity_ms`                           | `604800000`                                                | ms                | 7 d         | [L13](../config/deployments/preprod-public.yaml#L13)   |
| `timing.dispute_side_clock_ms`                       | `86400000`                                                 | ms                | 1 d         | [L24](../config/deployments/preprod-public.yaml#L24)   |
| `timing.dispute_max_validity_range_ms`               | `120000`                                                   | ms                | 2 min       | [L25](../config/deployments/preprod-public.yaml#L25)   |
| `timing.dispute_resolution_allowance_ms`             | `21600000`                                                 | ms                | 6 h         | [L26](../config/deployments/preprod-public.yaml#L26)   |
| `timing.operator_shift_ms`                           | `3600000`                                                  | ms                | 1 h         | [L27](../config/deployments/preprod-public.yaml#L27)   |
| `timing.registration_ms`                             | `30000`                                                    | ms                | 30 s        | [L28](../config/deployments/preprod-public.yaml#L28)   |
| `timing.event_wait_ms`                               | `1800000`                                                  | ms                | 30 min      | [L29](../config/deployments/preprod-public.yaml#L29)   |
| `timing.user_events_negligence_timeout_ms`           | `1200000`                                                  | ms                | 20 min      | [L30](../config/deployments/preprod-public.yaml#L30)   |
| `timing.new_shift_inactivity_grace_period_ms`        | `480000`                                                   | ms                | 8 min       | [L31](../config/deployments/preprod-public.yaml#L31)   |
| `timing.max_validity_range_ms`                       | `480000`                                                   | ms                | 8 min       | [L32](../config/deployments/preprod-public.yaml#L32)   |
| `timing.max_event_validity_range_ms`                 | `600000`                                                   | ms                | 10 min      | [L33](../config/deployments/preprod-public.yaml#L33)   |
| `timing.da_attestation_timeout_ms`                   | `3600000`                                                  | ms                | 1 h         | [L34](../config/deployments/preprod-public.yaml#L34)   |
| `timing.da_small_response_window_ms`                 | `3600000`                                                  | ms                | 1 h         | [L35](../config/deployments/preprod-public.yaml#L35)   |
| `timing.da_full_response_window_ms`                  | `172800000`                                                | ms                | 2 d         | [L36](../config/deployments/preprod-public.yaml#L36)   |
| `timing.da_challenge_window_ms`                      | `86400000`                                                 | ms                | 1 d         | [L42](../config/deployments/preprod-public.yaml#L42)   |
| `timing.da_slash_grace_ms`                           | `172800000`                                                | ms                | 2 d         | [L43](../config/deployments/preprod-public.yaml#L43)   |
| `timing.da_bond_withdraw_delay_ms`                   | `778080000`                                                | ms                | 9 d 8 min   | [L47](../config/deployments/preprod-public.yaml#L47)   |
| `da_transport.retention_days`                        | `120`                                                      | days              |             | [L50](../config/deployments/preprod-public.yaml#L50)   |
| `da_bond.da_bond_lovelace`                           | `100000000000`                                             | lovelace          | 100,000 ADA | [L52](../config/deployments/preprod-public.yaml#L52)   |
| `da_bond.da_slash_penalty_lovelace`                  | `25000000000`                                              | lovelace          | 25,000 ADA  | [L53](../config/deployments/preprod-public.yaml#L53)   |
| `da_bond.da_bond_min_top_up_lovelace`                | `1000000000`                                               | lovelace          | 1,000 ADA   | [L54](../config/deployments/preprod-public.yaml#L54)   |
| `da_bond.da_bond_pool_floor_lovelace`                | `5000000`                                                  | lovelace          | 5 ADA       | [L55](../config/deployments/preprod-public.yaml#L55)   |
| `da_bond.challenge_record_lovelace`                  | `27000000`                                                 | lovelace          | 27 ADA      | [L61](../config/deployments/preprod-public.yaml#L61)   |
| `limits.max_bisection_rounds`                        | `32`                                                       | rounds            |             | [L63](../config/deployments/preprod-public.yaml#L63)   |
| `limits.max_inactivity_strikes`                      | `1000000`                                                  | strikes           | 1,000,000   | [L65](../config/deployments/preprod-public.yaml#L65)   |
| `limits.coins_per_utxo_byte`                         | `4310`                                                     | lovelace per byte |             | [L66](../config/deployments/preprod-public.yaml#L66)   |
| `limits.max_dispute_move_fee_lovelace`               | `1200000`                                                  | lovelace          | 1.2 ADA     | [L70](../config/deployments/preprod-public.yaml#L70)   |
| `limits.fraud_proof_output_lovelace`                 | `2000000`                                                  | lovelace          | 2 ADA       | [L74](../config/deployments/preprod-public.yaml#L74)   |
| `economics.profile`                                  | `public-preprod-launch-v1`                                 | text              |             | [L76](../config/deployments/preprod-public.yaml#L76)   |
| `economics.requiredBondLovelace`                     | `100000000000`                                             | lovelace          | 100,000 ADA | [L77](../config/deployments/preprod-public.yaml#L77)   |
| `economics.slashingPenaltyLovelace`                  | `25000000000`                                              | lovelace          | 25,000 ADA  | [L78](../config/deployments/preprod-public.yaml#L78)   |
| `economics.inactivitySlashingPenaltyLovelace`        | `10000000000`                                              | lovelace          | 10,000 ADA  | [L79](../config/deployments/preprod-public.yaml#L79)   |
| `economics.fraudProverRewardLovelace`                | `75000000000`                                              | lovelace          | 75,000 ADA  | [L80](../config/deployments/preprod-public.yaml#L80)   |
| `economics.proverCollateralFloorLovelace`            | `5000000`                                                  | lovelace          | 5 ADA       | [L81](../config/deployments/preprod-public.yaml#L81)   |
| `protocol_treasury_native_script.type`               | `atLeast`                                                  | text              |             | [L88](../config/deployments/preprod-public.yaml#L88)   |
| `protocol_treasury_native_script.required`           | `2`                                                        | signatures        |             | [L89](../config/deployments/preprod-public.yaml#L89)   |
| `protocol_treasury_native_script.scripts[0].type`    | `sig`                                                      | text              |             | [L91](../config/deployments/preprod-public.yaml#L91)   |
| `protocol_treasury_native_script.scripts[0].keyHash` | `f1fb1abc10fc4d90b367c7f994b87aeeb4db60fa8a42ee43a159256c` | payment key hash  |             | [L92](../config/deployments/preprod-public.yaml#L92)   |
| `protocol_treasury_native_script.scripts[1].type`    | `sig`                                                      | text              |             | [L93](../config/deployments/preprod-public.yaml#L93)   |
| `protocol_treasury_native_script.scripts[1].keyHash` | `9ac54f4b18dcdbaeb981b9acbb4e79b590315eb508a08b3a50509cb9` | payment key hash  |             | [L94](../config/deployments/preprod-public.yaml#L94)   |
| `protocol_treasury_native_script.scripts[2].type`    | `sig`                                                      | text              |             | [L95](../config/deployments/preprod-public.yaml#L95)   |
| `protocol_treasury_native_script.scripts[2].keyHash` | `65a0683b5ca28e4a8fed2b5a443210741faad5ed43802d20fd329a53` | payment key hash  |             | [L96](../config/deployments/preprod-public.yaml#L96)   |
| `allow_lists.operators[0]`                           | `041388eef29da389e7b9dac19509ef8ac95a398a9909991a4326d424` | payment key hash  |             | [L101](../config/deployments/preprod-public.yaml#L101) |
| `allow_lists.operators[1]`                           | `4189f341bddd0c74ecb4f5a1ddae8653ee204be022fe530e2f65a6a6` | payment key hash  |             | [L102](../config/deployments/preprod-public.yaml#L102) |

<!-- END PUBLIC_TESTNET_PARAMETERS_GENERATED -->

## What each parameter governs

Below, N is `user_events_negligence_timeout_ms`, L is `max_validity_range_ms`,
W is `event_wait_ms`, d is `commit_event_depth` and M is `block_maturity_ms`.

### Identity

- `name` and `network`: the profile name and the Cardano network. Both are
  part of the profile digest that the finalized deployment manifest binds.
- `coins_per_utxo_byte` (4,310 lovelace per byte): the minimum-ADA rate of L2
  outputs, the Cardano Preprod Conway value. It is compiled into the consensus
  profile, so changing it is a redeploy.

### L1 finality

- `confirmation_depth` (30 blocks): how deep an L1 transaction must be before
  the off-chain parties act on it. Validation budgets it at twice the 20-second
  mean block time (20 minutes). Accepted for public launch on the condition
  that a deeper rollback, within k, is recovered automatically
  ([economics record](midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md) §3).
- `commit_event_depth` (d = 22 blocks): an event is committed only once it is d
  blocks below the view a commit is planned at. The profile binds it (owner,
  2026-10-09). 22 is the largest d all the bounds admit; the production bound
  W − L ≥ 3d·slot/f is the tight one.
- `security_parameter` (k = 2,160), `active_slot_coeff` (f = 0.05) and
  `slot_length_ms` (1 s): the Cardano Preprod consensus parameters the depth
  and timing bounds assume. Recorded facts, not choices. One block takes at
  most 3/f slots, 60 seconds, under the guaranteed chain-growth bound.

### Block maturity and the execution dispute

- `block_maturity_ms` (7 days): how long a committed block stays open to fraud
  proofs before it can merge. The canonical V1 maturity, kept at 7 days with
  the execution dispute on at launch (owner, 2026-10-21). It must be at least
  2 D_case + 1 = 420,960,001 ms, and an execution-dispute case must be able to
  open by M / 2 = 302,400,000 ms, after the latest DA answer at 259,680,000 ms.
- `dispute_side_clock_ms` (24 h): each side's total time in an execution
  dispute, a chess clock (owner, 2026-10-13). This is the real move timer.
- `dispute_max_validity_range_ms` (2 min): the widest validity range a dispute
  move may carry.
- `dispute_resolution_allowance_ms` (6 h): time after the game for the
  one-step resolution chain. Measured on the emulator (2026-10-21): the longest
  chain is 19 transactions, 3,420 s per thread, so 6 h covers six threads
  played one move at a time.
- `max_bisection_rounds` (32): the most bisection rounds of an execution
  dispute.
- `max_dispute_move_fee_lovelace` (1.2 ADA): the most a dispute move may take
  from its thread for its fee; larger one-step hops are paid by the wallet.
  At least twice the largest measured reveal (377,315 lovelace at depth 32),
  kept at 1.2 ADA after measurement (2026-10-21).
- `fraud_proof_output_lovelace` (2 ADA): the lovelace of an execution-dispute
  fraud-proof output, above its measured minimum ADA (1,327,480 lovelace).

### Operators and shifts

- `operators` (two payment key hashes): the curated P1 operator allow-list.
  Only these keys may register (owner input I1, 2026-10-13). Fault provers and
  DA challengers are permissionless (owner, 2026-10-13).
- `operator_shift_ms` (1 h): the length of one operator's shift, the one-hour
  shift the node schedules.
- `registration_ms` (30 s): how long after its registration an operator
  becomes active (`registration_duration` in
  `onchain/aiken/validators/operator-directory/registered-operators.ak`).
- `user_events_negligence_timeout_ms` (N = 20 min): how long a deposit,
  withdrawal or forced transaction may wait for inclusion before the shift's
  operator can be struck. A strike always cites such an event (owner,
  2026-10-09). N must be shorter than the shift and at least L.
- `new_shift_inactivity_grace_period_ms` (8 min): the earliest a new shift's
  operator can be struck for an event already overdue at the shift boundary.
  Must be at least L (owner, 2026-10-09); set equal to L.
- `max_inactivity_strikes` (1,000,000): the strike count that ejects an
  operator. Set out of reach, so in P1 a strike skips the shift but never
  ejects (owner, 2026-10-12, two-stage launch).

### Event timing

- `event_wait_ms` (W = 30 min): the delay between a user event's L1 validity
  end and its L2 inclusion time. The worst case for 30 blocks, 3N/f slots
  (owner, 2026-09-26 and 2026-09-27). Deposits and orders become eligible on
  L2 that much later.
- `max_validity_range_ms` (L = 8 min): the widest validity range of a commit,
  deposit or withdrawal.
- `max_event_validity_range_ms` (10 min): the widest closed validity range a
  tx-order may carry, enforced at the tx-order mint
  (`onchain/aiken/validators/user-events/tx-order-v1.ak`). About 10 minutes, so
  orders do not go invalid in flight (owner, 2026-10-03).

The commit-timing relations hold on this profile: N ≥ L (20 ≥ 8 min);
W + N = 50 min ≥ 3(d + 1)·slot/f + L = 31 min; W − L = 22 min = 3d·slot/f;
grace = L.

### Data availability timing

- `da_attestation_timeout_ms` (1 h): how long a committed block may stay
  unattested before timeout removal. Timeout removal does not slash.
- `da_small_response_window_ms` (1 h) and `da_full_response_window_ms` (2 d):
  the committee's deadline to answer an availability challenge for a small
  payload (up to 64 KiB) or a full one, counted from the challenge's open.
- `da_challenge_window_ms` (24 h): an availability challenge must open within
  this window after the block's end time (owner, 2026-10-15). It exceeds the
  attestation timeout by 23 h, more than the required 36 min.
- `da_slash_grace_ms` (2 d): how long the challenger has to land the timeout
  slash once it is possible. See [open items](#open-items).
- `da_bond_withdraw_delay_ms` (9 d 8 min): the delay between the two steps of a
  committee bond withdrawal. Derived, not chosen: L + max(challenge window +
  full response window, M) + slash grace = 480,000 + 604,800,000 +
  172,800,000 ms. It covers the latest possible slash of any block the pool
  still backs.
- `retention_days` (120 days): how long the committee retains DA payloads.
  Participants bootstrap by replaying retained payloads from genesis, with no
  state snapshot in P1 (owner, 2026-10-12). It exceeds both maturity and the
  withdrawal delay, and the 15-day minimum the node enforces. Node
  housekeeping takes its retention from the manifest (owner, 2026-10-01).

### Data availability bond

One pooled bond backs every block the committee attests (owner, 2026-09-27);
an attestation applies only while the pool holds at least one DA bond of
backing.

- `da_bond_lovelace` (100,000 ADA): one DA bond, the backing the pool must hold.
- `da_slash_penalty_lovelace` (25,000 ADA): burned as fee by each lost
  availability challenge; the rest of one DA bond goes to the challenger.
- `da_bond_min_top_up_lovelace` (1,000 ADA): the smallest permissionless top-up.
- `da_bond_pool_floor_lovelace` (5 ADA): stays in the pool UTxO for its minimum
  ADA and never counts as backing.
- `challenge_record_lovelace` (27 ADA): the exact lovelace of a challenge
  record, the measured minimum ADA of the largest admitted record, rounded up
  to a whole ADA.

The bond, slash penalty and minimum top-up are owner-approved (2026-10-22).

### Operator economics

- `profile` (`public-preprod-launch-v1`): the economics schedule identifier,
  recorded in the manifest.
- `requiredBondLovelace` (100,000 ADA): the bond an operator posts to register;
  slashing penalty plus prover reward.
- `slashingPenaltyLovelace` (25,000 ADA): the part of a fraudulent operator's
  bond that is not paid to the prover.
- `fraudProverRewardLovelace` (75,000 ADA): what a successful fault prover is
  paid from the slashed bond.
- `inactivitySlashingPenaltyLovelace` (10,000 ADA): the penalty of a forced
  retirement for inactivity. Dormant in P1, because strikes never eject.
- `proverCollateralFloorLovelace` (5 ADA): the least collateral a fault prover
  reserves for a script transaction.

These figures are owner-set
([economics record](midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md)
§2.1) and approved again on 2026-10-22.

### Protocol treasury

- `protocol_treasury_native_script`: where forfeited execution-dispute bonds
  go, separate from the reserve (owner, 2026-10-13). Its `type` is `atLeast`,
  `required` 2 of the three listed Preprod treasury keys, the deployment's
  owner quorum (owner, 2026-10-13). The generator derives its hash.

### Excluded fault-proof families

- `excluded_fraud_proof_categories`: the families left out of the fraud-proof
  catalogue; Computation-Thread Init refuses them. The list is empty, so the
  table above has no row for it: every category is in the catalogue. The
  execution dispute (`validationTraceDispute`) is in it at launch (owner,
  2026-10-21). The nine trace-relative families joined as each was rebuilt to
  prove its fault from the committed transaction (owner, 2026-10-25), and each
  has an installed off-chain workflow.

## Values set outside the profile

Line numbers are as of this review; the symbol names are the stable reference.

| Parameter                       | Value                                                                                                                                                                                                                    | Set at                                                                                                                                                                                                                                                                                       | Governs and why                                                                                                                                                                                                                                                                   |
| ------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Block ex-units budget           | 33,000,000 memory, 20,000,000,000 steps per block                                                                                                                                                                        | `DEFAULT_BLOCK_EX_UNITS_BUDGET`, `demo/midgard-node/src/workers/utils/commit-block-planner.commit-scheduler-evidence-key.ts` line 101; overrides `BLOCK_EX_UNITS_BUDGET_MEMORY` and `BLOCK_EX_UNITS_BUDGET_STEPS` (`demo/midgard-node/src/services/config.block-ex-units-budget.ts` line 22) | The execution units the node packs into one block: twice the consensus per-transaction cap, so a block holds at least two transactions at the cap. A node-local planning bound, not consensus. Forced transactions are never budget-gated.                                        |
| Import-check opener margin      | 600,000 ms on `preprod-public`                                                                                                                                                                                           | `OPENER_MARGIN_MS_BY_PROFILE`, `demo/midgard-node/src/services/config.import-check.ts` line 80; override `IMPORT_CHECK_OPENER_MARGIN_MS`                                                                                                                                                     | Room before the case open deadline (block end plus half the block maturity) for a challenge opener's Init, Open and VerifySource transactions to land. Node-local and alarm-only: the import check's backlog alarms when it cannot finish an entry this long before its deadline. |
| HTTP rate limits                | Per peer: health 20 burst / 5 per s; admin 30 / 10; read 30 / 10; batch and write 4 / 1; evaluate 2 / 0.2. Global: read 200 / 100; batch and write 16 / 4; evaluate 8 / 1. Each client table holds 8,192 keys, idle 60 s | `DEFAULT_HTTP_RATE_LIMITS`, `HTTP_PEER_MAX_ENTRIES`, `HTTP_PEER_IDLE_MS`, `demo/midgard-node/src/services/http-rate-limits.ts` lines 62, 39, 41; overrides `HTTP_RATE_LIMIT_<CLASS>` and `HTTP_RATE_LIMIT_ENABLED` (`demo/midgard-node/src/services/config.http-proxy-peers.ts`)             | The public node's request limits. The global buckets can create at most 6,780 keys per idle window, within the 8,192-key table. Body and page limits are in the [public HTTP guide](../demo/midgard-node/PUBLIC_HTTP.md).                                                         |
| Availability challenger bond    | 10,000 ADA                                                                                                                                                                                                               | `MIDGARD_DA_AVAILABILITY_CHALLENGER_BOND_LOVELACE`, `demo/midgard-node/.env.example` line 194, read at `demo/midgard-node/src/environment.ts` line 93                                                                                                                                        | A deploy-time manifest value. A DA challenger posts it, and it alone must cover the committee's maximum publication, settlement and terminal fees. The execution-dispute bond equals it (owner, 2026-10-12); the figure is owner-approved (2026-10-22).                           |
| DA fee caps                     | open 0.5 ADA, publication 0.5 ADA, settlement 0.5 ADA, close 1 ADA                                                                                                                                                       | `MIDGARD_DA_AVAILABILITY_MAX_*_FEE_LOVELACE`, `demo/midgard-node/.env.example` lines 195–198                                                                                                                                                                                                 | Deploy-time manifest values: the most each availability-challenge transaction may pay in fees.                                                                                                                                                                                    |
| DA retention minimum            | 15 days                                                                                                                                                                                                                  | `DA_TRANSPORT_LIMITS.minimumRetentionDays`, `demo/midgard-core/src/da-transport.da-libp2p-runtime-manifest.ts` line 50                                                                                                                                                                       | The least retention a profile or manifest may declare; maturity plus worst-case proof time plus margin. The public profile declares 120 days.                                                                                                                                     |
| Consensus limits                | Per-transaction execution floor 16,500,000 memory and 10,000,000,000 CPU; at most 10,000 L2 transactions, withdrawals, forced transactions and deposits per block                                                        | `MIDGARD_CONSENSUS_LIMITS`, `demo/midgard-core/src/consensus-profile.ts` lines 281–287 and 326–331                                                                                                                                                                                           | Compiled into the consensus profile and its digest; the [consensus profile](consensus-profile-v1.md) lists them all.                                                                                                                                                              |
| Installed fault-proof workflows | none uninstalled                                                                                                                                                                                                         | `UNINSTALLED_FRAUD_PROOF_CATEGORIES`, `demo/midgard-fault-proofs/src/workflow/installed-categories.ts` line 11                                                                                                                                                                               | Catalogue families with no off-chain workflow. Empty: every catalogue family has one.                                                                                                                                                                                             |

## Open items

These need an owner decision or a follow-up; none blocks the table above.

- `da_slash_grace_ms` (2 d) and `da_bond_pool_floor_lovelace` (5 ADA) have no
  recorded owner confirmation. `da_bond_withdraw_delay_ms` follows from the
  grace.
- The challenger bond and the DA fee caps are deploy-time manifest values, not
  profile values, so profile validation does not pin them.
- The block ex-units budget is to be re-measured on an idle machine.
