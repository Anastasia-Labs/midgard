# Availability-challenge publication plan

Status: The publication and registered contract-emulator slice passes on the
2026-09-08 tree. The [operational implementation](../availability-challenge-operations.md)
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
and then executes real attestation, bond and challenge transactions. Existing
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

The explicit acceptance fixture uses matching 12-billion-lovelace bonds, fee
ceilings of 2 million for open/publication/settlement/close and 3 million for
timeout. The original 500,000-lovelace publication ceiling was below the base
fee of a maximum chunk. The larger bond covers all 4,800 maximum-fee chunk
publications plus live carrier min-Ada and settlement working capital across
16 tranches. These are explicit fixture/deployment parameters, not implicit
production defaults.

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
zero-withdrawal validator. Preserve bond, tranche, publication, terminal and
state-queue semantics; this work changes physical verification boundaries.

| Mint arm                   | Role token                         | Manifest contract key                  |
| -------------------------- | ---------------------------------- | -------------------------------------- |
| Mint bond from attestation | `AvailabilityChallengeBondYield`   | `availabilityChallengeBondWithdraw`    |
| Open                       | `AvailabilityChallengeOpenYield`   | `availabilityChallengeOpenWithdraw`    |
| Settle                     | `AvailabilityChallengeSettleYield` | `availabilityChallengeSettleWithdraw`  |
| Close                      | `AvailabilityChallengeCloseYield`  | `availabilityChallengeCloseWithdraw`   |
| Timeout                    | `AvailabilityChallengeExpiryYield` | `availabilityChallengeTimeoutWithdraw` |

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

The split adds five reference-script UTxOs and reward-account registrations.
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

1. Factor mint-arm functions into the availability library, add the five yields,
   and preserve all existing spending predicates. Remove unreachable helper code
   only where it is actually dead; retain per-tranche timeout semantics.
2. Update SDK schemas, contract types/application, role tables, manifest entries,
   inspection and publication targets together. Reapply correction-lock,
   state-queue and its unavailable-removal yield, DA-attestation and hub identities
   from the resulting blueprint. Every affected parameter application needs real
   happy-path and refusal emulator scenarios.
3. Extend the existing DA-attestation apply builder with the bond yield and mirror
   the change in node and committee coordinator consumers. Implement open, chunk
   publication, settlement, close and timeout transaction builders. Timeout must
   compose with correction-lock acquisition and unavailable-block removal.
4. Publish all roles in the appropriate runtime/DA scopes and register reward
   credentials idempotently during initialization. Verify registration on the
   target ledger rather than relying on emulator permissiveness.
5. Add operational open/respond/settle/close/timeout commands and resumable chunk
   publication from authenticated retained payloads. Replace hardcoded missing
   capability reports only after deployed manifest and live references authenticate
   the capability.
6. Add rollback-safe bond/tranche/carrier/terminal indexing and a watcher adapter
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

- attestation to bonded available status;
- open through ordered chunk/carrier publication, per-tranche settlement,
  terminal accumulation and close;
- no-response timeout with exact slash/refund and queue removal;
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

The normal testnet blueprint was built with the exact CI-pinned compiler
`aiken v1.1.23+5adf783` (binary MD5 `ea9b39054f166e94771f838a662712f7`).
The [measurement summary](availability-challenge-fit.json) records its SHA-256,
mainnet profile source, applied publication identities, fees, reference-script
bytes, and per-scenario maxima. Signed publications are submitted and their live
role NFTs and script references checked; reward registration is ledger-queried
and repeat registration submits no transaction.

| Availability reference role | Applied script bytes | Signed publication bytes |
| --------------------------- | -------------------: | -----------------------: |
| Spending dispatcher         |                8,135 |                    8,642 |
| Minting dispatcher          |                8,135 |                    8,640 |
| Bond yield                  |                5,966 |                    6,481 |
| Open yield                  |                7,893 |                    8,408 |
| Settle yield                |                7,186 |                    7,705 |
| Close yield                 |                6,540 |                    7,057 |
| Timeout yield               |                6,024 |                    6,543 |

All seven fit the 15,872-byte signed publication target. The restored broader
roster gate separately signed and submitted all 513 node-runtime targets under
16,384 bytes; its largest unrelated target is 16,032 bytes. The 512-byte reserve
claim above applies to the availability reference scripts.

Four real lifecycle scenarios passed, with 704 signed/submitted transactions
including fixture publication and registration, plus nine on-chain refusal
attempts. The largest lifecycle transaction was 15,949 bytes (435 bytes below
the limit). Across all scenarios, peak aggregate memory was 4,156,825 units and
peak CPU was 1,726,095,109 units. Every transaction is checked against both the
ledger limits and a 20% execution reserve; budgets are summed across all script
purposes in the transaction.

- The small happy path verifies two real committee signatures, retained bond
  minting, challenge opening, ordered publication with carrier consumption,
  settlement, exact refunds, the Published terminal commitment, and complete
  challenge-token cleanup.
- The complete response case publishes and reassembles all 301 chunks of a
  4 MiB + 1 byte payload across two tranches, settles both, and closes.
- The maximum-commitment case opens all 16 tranches with exactly 19 authored
  outputs, fully publishes the first 300-chunk tranche, partially publishes the
  second, settles all 16, then times out and removes the unavailable queue head.
  It does not claim full 64 MiB publication.
- The no-response case refuses early settlement, then performs timeout, exact
  slash/refund, correction-lock validation and queue removal after the deadline.
- Refusals cover substituted bond ownership, absent challenger signature,
  omitted/wrong-action yield, incorrect chunk authentication, omitted predecessor
  carrier, premature settlement, redirected close refund and redirected slash.
  The assertions require a Scalus CEK execution failure with spent-budget evidence,
  not a planner, balance or missing-witness error.

The maximum timeout references 41,084 script bytes across eight reference inputs
(32,949 unique script bytes after shared dispatcher deduplication), and pays the
explicit 3-million-lovelace fixture ceiling. Registration deposits, publication
fees and reference UTxO min-Ada are separately funded. Atomic initialization now
registers the five yields; startup, node DA apply and committee DA apply also
check reward-account readiness on the ledger.

Verification completed:

- 96 focused Aiken tests: 43 availability predicates/conjunctions, 16 DA
  attestation, 25 queue-removal, 11 correction-lock, and one cross-language
  tranche-index vector test. Exact selectors used `run-focused-check.mjs` and
  required nonzero collection.
- 70 node tests across publication, mainnet cost-model consumption, readiness,
  full runtime roster, contract application/deployment identity, role parity,
  atomic initialization, operator activation, and the four availability lifecycles.
- SDK schema/planner/attestation/reference-role tests, core manifest identity
  tests, committee deployment/resolver/coordinator/transaction tests, package
  typechecks and scoped formatting/lint checks.

Reproduce the primary gates from the repository root:

```sh
(cd onchain/aiken && aiken build --env testnet)
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
