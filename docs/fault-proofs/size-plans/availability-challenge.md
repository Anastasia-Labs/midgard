# Availability-challenge publication plan

Status: The publication and registered contract-emulator slice passes on the
2026-09-28 tree, with the pooled DA committee bond. The [operational implementation](../availability-challenge-operations.md)
also supplies shared builders, independent actors and durable recovery. Live
release acceptance remains open.

## Current implementation slice

The 2026-09-08 review retains this decomposition. This P0 covers validator
extraction, ABI/role/parameter applications, DA apply and registration integration,
and real registered emulator lifecycle/budget scenarios. Open/respond/settle/close/
timeout construction is exercised by the emulator harness. Reusable operational
builders, commands, watcher/indexer integration and actor recovery were delivered
in the subsequent operational implementation; its plan records separate evidence.
The lifecycle fixture starts with declared hub/queue/DA-parameter genesis state
and then executes real attestation, DA bond pool and challenge transactions. Existing
initialization scenarios separately verify the applied deployment dependencies.
All availability publication evidence uses signed transactions with a 512-byte
size reserve.

The emulator profile is pinned to mainnet epoch 654, protocol 11, including the
complete 350-entry Plutus V3 cost model, in
[`mainnet-protocol-parameters.json`](../../../demo/midgard-node/tests/fixtures/mainnet-protocol-parameters.json).
It uses Scalus 0.1.3 with explicit protocol 11 and local evaluation enabled.
Lucid's bundled 297-entry model predates the mainnet update; its default Aiken
WASM evaluator miscosts Ed25519 verification with the complete protocol-11 model.
The cost-model test verifies that changing a supplied coefficient changes the
measured execution charge. The lifecycle tests exercise real committee signatures.
The snapshot comes from the [mainnet ledger parameters](https://api.koios.rest/api/v1/epoch_params?_epoch_no=654);
[Intersect documents the cost-model and protocol-11 activation](https://intersectmbo.org/news/cardano-upgrade-van-rossem-hard-fork).

The explicit acceptance fixture uses a 12-billion-lovelace challenger bond, fee
ceilings of 2 million for open/publication/settlement/close and 3 million for
timeout. The original 500,000-lovelace publication ceiling was below the base
fee of a maximum chunk. The challenger bond covers all 4,800 maximum-fee chunk
publications plus live carrier min-Ada and settlement working capital across
16 tranches. These are explicit fixture/deployment parameters, not implicit
production defaults. The pooled DA committee bond (#685) replaced the per-block
DA bond; its amounts (bond, slash penalty, minimum top-up, pool floor and
challenge record lovelace) come from the deployment profile.

The realistic multi-tranche scenario also found an SDK encoding mismatch:
Aiken's tranche asset-name suffix is a two-byte little-endian index. SDK indices
above zero previously used big endian. Matching Aiken/SDK literal vectors now
cover indices 0, 1, 15 and 63 without changing the on-chain token derivation.

## Problem and design boundary

The former availability validator combined five mint arms with three spend arms.
Its earlier recorded unapplied body was 19,927 raw bytes. The previous node
admission test pinned the applied fixture to 20,029 bytes; both exceeded the
complete signed 16,384-byte transaction ceiling. These are saved measurements, not a fresh build
result. Rebuild with the pinned testnet compiler before
using a size projection as implementation evidence.

Retain a mint/spend dispatcher with `AdvanceTranche`, `ConsumeCarrier`, and
`Coordinate` inline. Move each mint arm into a separate authenticated
zero-withdrawal validator. Preserve challenge-record, tranche, publication,
terminal and state-queue semantics; this work changes physical verification
boundaries.

| Mint arm | Role token                         | Manifest contract key                  |
| -------- | ---------------------------------- | -------------------------------------- |
| Open     | `AvailabilityChallengeOpenYield`   | `availabilityChallengeOpenWithdraw`    |
| Settle   | `AvailabilityChallengeSettleYield` | `availabilityChallengeSettleWithdraw`  |
| Close    | `AvailabilityChallengeCloseYield`  | `availabilityChallengeCloseWithdraw`   |
| Timeout  | `AvailabilityChallengeExpiryYield` | `availabilityChallengeTimeoutWithdraw` |

The split originally had a fifth arm that minted a per-block bond at DA
attestation. The pooled DA committee bond (#685) removed it: Apply checks the
one pool instead, and a Timeout slashes the pool.

The Timeout yield takes the pool as a mandatory input, even at zero backing,
and checks the slash exactly. With `backing` the pool's lovelace above its
floor, `taken = min(da_bond_lovelace, backing)`,
`fee_part = min(da_slash_penalty_lovelace, taken)` and
`payout = taken − fee_part`. The pool output keeps its address, datum and NFT
and holds `taken` less lovelace. The transaction fee is exactly
`fee_part + c`, where `c` is the challenger's own fee contribution,
`0 ≤ c ≤ max_timeout_fee_lovelace`. The challenger receives one merged output
of `remaining − c + challenge_record_lovelace + payout`: a separate reward
output with `0 < payout <` min-UTxO would make the Timeout unbuildable, while
the merged output always carries at least the record lovelace. The builder sets
`c` to the part of the required ledger fee that `fee_part` does not cover, so
with a full pool `c = 0` and the fee is exactly the penalty.

The existing `AvailabilityChallengeSpend` and `AvailabilityChallengeMint` roles
remain on the dispatcher. Match role bytes between Aiken, SDK and core tables;
all asset names must fit 32 bytes.

## Alternatives and consequences

Pruning dead helpers cannot remove the reachable cost of five independent mint
arms. Earlier isolated probes put the chosen dispatcher near 8 KB and separate
mint arms near 6–8 KB; these are design estimates, not publication acceptance.

Keep `AdvanceTranche` inline because its transaction carries the large payload
chunk. Adding another withdrawal/reference input would change the measured
response geometry and charge an additional script across thousands of
publications. Re-measure the maximum 14,020-byte chunk publication if this choice
changes. Pairing mint arms reduces deployed roles but increases referenced bytes
per action and obscures one-role-per-arm authentication. Chaining mint arms adds
response-window transactions without addressing the original code-size cause.

The split adds four reference-script UTxOs and reward-account registrations.
Budget their min-Ada, registration deposits and publication fees separately from
challenger action funding. Include aggregate referenced bytes and reference-script
fees, not only the signed transaction size.

## Authentication and ABI

Add `reference_script_auth_policy_id` to the dispatcher parameters. Each yield
is applied with the availability policy id, hub-oracle policy id and availability
parameters. Build the dispatcher before yields so applied hashes do not cycle.

Each mint constructor gains `yield_to_ref_input_index` as its first field. Keep
its constructor tag and other fields. The rewarding redeemer is fieldless.
Spend redeemers, stored datums, asset-name derivations, and accumulator domains
stay unchanged. Recompile all consumers that decode the mint redeemer.

The dispatcher chooses the role from its mint constructor and requires the exact
role NFT, script reference and unique zero withdrawal. Each yield opens the unique
mint redeemer for its applied availability policy, requires its own constructor,
and runs that arm's complete predicate. It rederives exact outputs, mint pairs,
queue status and accumulators. A missing, wrong-role, duplicate or substituted
yield must refuse. `Coordinate` continues to bind each declared spend to that
mint redeemer; carrier consumption stays bound to `AdvanceTranche`.

Follow [withdraw-zero delegation](../../agents/withdraw-zero-yielding.md).
Missing role publication or reward registration must fail readiness before an
operator/challenger depends on that action.

## Implementation work

1. Factor mint-arm functions into the availability library, add the four yields,
   and preserve all existing spending predicates. Remove unreachable helper code
   only where it is actually dead; retain per-tranche timeout semantics.
2. Update SDK schemas, contract types/application, role tables, manifest entries,
   inspection and publication targets together. Reapply correction-lock,
   state-queue and its unavailable-removal yield, DA-attestation and hub identities
   from the resulting blueprint. Every affected parameter application needs real
   happy-path and refusal emulator scenarios.
3. Keep the DA-attestation apply builder's pooled-bond check in step with the
   node and committee coordinator consumers. Implement open, chunk
   publication, settlement, close and timeout transaction builders. Timeout must
   compose with correction-lock acquisition and unavailable-block removal.
4. Publish all roles in the appropriate runtime/DA scopes and register reward
   credentials idempotently during initialization. Verify registration on the
   target ledger rather than relying on emulator permissiveness.
5. Add operational open/respond/settle/close/timeout commands and resumable chunk
   publication from authenticated retained payloads. Replace hardcoded missing
   capability reports only after deployed manifest and live references authenticate
   the capability.
6. Add rollback-safe challenge-record/tranche/carrier/terminal indexing and a watcher adapter
   that opens after post-attestation retrieval failure, then settles, closes or
   times out. Accountable DA signers publish response chunks; the challenger
   adapter must not depend on operator-local data.
7. Complete action-specific funding: challenger bond, bounded opening/publication/
   settlement/close/timeout fees, collateral and the maximum publication count.
   Preserve intent-first journaling, exact out-ref recovery and mutation fencing.

Current source authorities are the [validator](../../../onchain/aiken/validators/availability-challenge.ak),
[availability library](../../../onchain/aiken/lib/midgard/availability-challenge.ak),
[SDK schemas/planners](../../../demo/midgard-sdk/src/availability-challenge.ts),
[attestation builder](../../../demo/midgard-sdk/src/da-attestation.ts), and
[node publication admission test](../../../demo/midgard-node/tests/availability-challenge-publication-admission.test.ts).
Pure planners and a capability schema do not establish executable lifecycle wiring.

## Acceptance

Build the normal testnet blueprint through the pinned Aiken build workflow. Every
fully applied publication must fit the signed envelope and 512-byte reserve;
all executing scripts in each transaction share the Van Rossem budget. Preserve
the 20% execution basis. No oversized route, raised limit, disabled evaluator,
skipped publication scenario, or arity-only test can establish completion.

Exercise complete registered scenarios for:

- attestation to `Attested{commitment_hash}` status under a backed DA bond pool;
- open through ordered chunk/carrier publication, per-tranche settlement,
  terminal accumulation and close;
- no-response timeout with the exact pool slash, refund and queue removal;
- partial-response timeout and maximum-tranche settlement;
- invalid/cross-arm/omitted-role/reference substitutions and honest refusal;
- restart, repeated requests, already-submitted intent, rollback and concurrent
  actor recovery;
- maximum chunk and maximum tranche shapes, including 16-tranche opening with
  its 19 outputs, then publication/settlement through completion.

Keep original Q58 predicate tests and add yield-conjunction/refusal tests. Run
SDK schema/planner/attestation tests, core role/manifest identity, node contract
application/publication/initialization, and the new watcher adapter and lifecycle
scenarios. Require nonzero collection and record exact results against the current
blueprint. Update the readiness checklist from those results.

Publication closure may land before operational tooling, but the availability
remedy is complete only when independently operated parties can challenge,
respond, recover and settle or time out under the authenticated deployment within
the configured window and transaction cap. [Release acceptance](../execution-plan.md)
remains the completion authority.

## Verified publication and emulator results

These measurements were taken on 2026-09-28 against the pooled DA committee
bond (#685), under the `preprod-testing` profile. The pinned testing blueprint
was built with the exact CI-pinned compiler `aiken v1.1.23+5adf783` (binary MD5
`ea9b39054f166e94771f838a662712f7`); its SHA-256 is
`3ddd74900b586e3b471e2c668a70dc46e1e864566e3fc5f620d98b274bf5f463`.
The [measurement summary](availability-challenge-fit.json) records that digest,
the mainnet profile source, applied publication identities, fees,
reference-script bytes, and per-scenario maxima. Signed publications are
submitted and their live role NFTs and script references checked; reward
registration is ledger-queried and repeat registration submits no transaction.

| Availability reference role | Applied script bytes | Signed publication bytes |
| --------------------------- | -------------------: | -----------------------: |
| Spending dispatcher         |                7,921 |                    8,463 |
| Minting dispatcher          |                7,921 |                    8,461 |
| Open yield                  |                8,011 |                    8,561 |
| Settle yield                |                6,773 |                    7,327 |
| Close yield                 |                6,472 |                    7,024 |
| Timeout yield               |                7,434 |                    7,988 |
| DA bond pool (spending)     |                4,364 |                    4,882 |
| DA bond pool (minting)      |                4,364 |                    4,880 |

The DA bond pool is one applied multivalidator published under its spending and
minting roles. All eight publications fit the 15,872-byte signed publication
target. The broader roster gate separately signed and submitted all 528
node-runtime targets under 16,384 bytes; its largest unrelated target is 16,132
bytes. The 512-byte reserve claim above applies to the availability and pool
reference scripts.

Three real lifecycle scenarios passed under the testing windows, with 88
signed/submitted transactions including fixture publication and registration,
plus ten confirmed on-chain refusals. The largest lifecycle transaction was
15,890 bytes (494 bytes below the limit). Across all scenarios, peak aggregate
memory was 4,902,699 units and peak CPU was 2,015,954,521 units, leaving
margins of 11,597,301 memory and 7,984,045,479 CPU units below the ledger
limits. Every transaction is checked against both the ledger limits and a 20%
execution reserve; budgets are summed across all script purposes in the
transaction.

- The small happy path verifies two real committee signatures, Apply against a
  Bonded pool reference input, challenge opening with its record output,
  ordered publication with carrier consumption, settlement, the exact challenger
  refund including the record lovelace, the Published terminal commitment, and
  complete challenge-token cleanup. The pool is untouched.
- The maximum-first-chunk case opens all 16 tranches with exactly 19 authored
  outputs, publishes one maximum chunk of the first tranche with the
  full-tranche proof and the second tranche's partial response, settles all 16,
  then times out and removes the unavailable queue head. It does not claim full
  64 MiB publication.
- The no-response case refuses settlement one slot before an upper-anchored
  deadline, then performs timeout with a full pool slash, correction-lock
  validation and queue removal after the deadline. The pool gives up one bond;
  the slash penalty is the whole fee and the rest merges into the one
  challenger output with the record lovelace.
- Refusals cover an Apply whose commitment is not the attested preimage, an
  Open whose commitment does not hash to the node's `commitment_hash`, absent
  challenger signature, omitted/wrong-action yield, incorrect chunk
  authentication, omitted predecessor carrier, premature settlement, redirected
  close refund and redirected slash payout. The assertions require a Scalus CEK
  execution failure of the expected script and purpose, not a planner, balance
  or missing-witness error.

The maximum timeout references 48,735 script bytes across nine reference inputs
(40,814 unique script bytes after shared dispatcher deduplication). Its fee is
the 100,000,000-lovelace slash penalty taken from the pool, with no challenger
fee share. Registration deposits, publication fees and reference UTxO min-Ada
are separately funded. Atomic initialization registers the four yields and
funds the pool to its floor plus one bond. Startup, node DA apply and committee
DA apply also check reward-account readiness on the ledger.

Verification completed on 2026-09-28 (node, `NODE_ENV=emulator`, fresh
`preprod-testing` blueprint): the four availability lifecycles
(`availability-challenge-lifecycle`, `-pool-slash-lifecycle`,
`-responder-lifecycle`, `-sdk-lifecycle`), publication admission and the
full-roster publication fit, 20 tests passed and the complete-response scenario
skipped.

## Current scope under testing windows

The lifecycle suite takes its windows from the selected deployment profile. The
testing profiles (`preprod-testing`, `local-devnet-testing`) and the generated
`testnet` environment carry a 720 s small and 840 s full response window. The
emulator lands one transaction per 20 s block. Under those windows:

- the complete-response scenario (301 chunks plus one settlement, 302 blocks) is
  skipped, with the derived reason in its title, because 302 × 20 s exceeds
  840 s. It runs only on a long-window profile;
- the maximum-commitment scenario still opens all 16 tranches with 19 outputs,
  then publishes one maximum chunk of tranche 0 with the full-tranche proof and
  the tranche-1 partial, settles all 16 and times out with queue removal. It no
  longer publishes the whole 300-chunk tranche. Its fit report key is
  `maximum-first-chunk`, and the committed
  [measurement summary](availability-challenge-fit.json) records it under that
  name. The summary has no `full-response` entry.

Under this document's Acceptance rule, a skipped publication scenario does not
establish completion. The complete-response fit, and with it the settlement of
a completed 300-carrier tranche (its first tranche), is measured only with a
long-window profile (`preprod-public` or `mainnet`, 1 h small and 48 h full
windows):

```sh
pnpm --dir demo deployment:build preprod-public
# rebuild midgard-core, lucid-midgard, midgard-sdk and midgard-node, then:
NODE_ENV=emulator MIDGARD_DEPLOYMENT_PROFILE=preprod-public \
  MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json" \
  pnpm --dir demo/midgard-node exec vitest run \
  tests/availability-challenge-lifecycle.test.ts
pnpm --dir demo deployment:build preprod-testing   # restore the selection
```

The node test environment defaults `MIDGARD_DEPLOYMENT_PROFILE` to
`preprod-testing`, and global setup refuses a profile that differs from the
compiled one, so the long-window run must export the profile it was built for.
Without it vitest reports "No test files found" and measures nothing.

Reproduce the primary gates from the repository root (the testing profile skips
the complete-response scenario; see above):

```sh
pnpm --dir demo deployment:build preprod-testing
NODE_ENV=emulator MIDGARD_REAL_BLUEPRINT_PATH="$PWD/onchain/aiken/plutus.json" \
  pnpm --dir demo/midgard-node exec vitest run \
  tests/availability-challenge-publication-admission.test.ts \
  tests/availability-challenge-readiness.test.ts \
  tests/availability-challenge-lifecycle.test.ts \
  tests/mainnet-protocol-parameters.test.ts \
  tests/initialization-emulator.test.ts \
  tests/scratch-cg1-publication-fit.test.ts
pnpm --dir demo/midgard-node run typecheck
```

Set `MIDGARD_AVAILABILITY_FIT_REPORT_DIR` to an existing output directory to retain
all lifecycle transaction measurements, and `MIDGARD_CG1_EMIT` to an output file
for the complete signed reference-publication roster. Saved summaries do not
replace running these gates on the final release blueprint.
