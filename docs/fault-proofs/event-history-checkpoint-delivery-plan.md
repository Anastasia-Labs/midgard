# Authenticated event-history delivery checkpoints

Baseline: pushed commit `89427ff467736dc02c9084199920a5998fb0717f`.
User-authorized bounded delivery, 2026-09-24. The full original protocol goal
remains incomplete. Recovery checkpoint6 remains deferred.

## Delivery objective

Reproduce the integrated authenticated deposit AND withdrawal implementation
from a clean checkout; verify the fabricated/transition proofs and measured
sorted-list readiness gates; prepare a state-preserving deployment and live
prerequisites in parallel; then run one coordinated live campaign using one
verified deployment. Preserve event IDs, existing work/state, production limits
and acceptance assertions. Root independently verifies integration.

The goal API rejected this bounded replacement because the previous unfinished
goal is registered as paused. This document is the durable authorized objective;
the app goal record was not replaced or falsely marked complete.

## Single critical-path checklist

| Checkpoint | Implementation/preparation | Integrated verification | Live verification |
| --- | --- | --- | --- |
| 1. Reproducibility | Complete: exact committed archive, fresh pinned frozen install, native/runtime builds | PASS: fresh build, dependency provenance, native hash reproduction, node typecheck and actual owner D/W journey | Not applicable |
| 2. Proof acceptance | Complete for bounded checkpoint: arbitrary absent IDs, substitution, original Value CBOR evidence, direct honest Stage03 refusals; existing timing/raw/retired-ID coverage retained | PASS: fabricated16, opening/transition/installed62, retired-ID2; independent signed evidence checks | Pending deployment/public witness/live proof evidence; broader recovery remains open |
| 3. Sorted-list readiness | Local gates implemented and exercised: authentication, accounting, retained-data references, reclaim authorization, no backdating, frontier retirement and bounded protection | PASS: full109, capture14, actual owner journey1; combined maximum3 and signed fit/value checks | ABI/deployment freeze remains pending actual provider timing, contention and final parameter identity |
| 4. Deployment preparation | Provider repair complete on Docker Desktop; pinned provider-only project preserves existing databases; fresh deployment/state and DA preparation remain | PASS: running images/config/genesis, synchronized matching Kupo/Ogmios tip, built node provider preflight, four funded distinct wallet roles | Provider services running; no protocol deployment or transaction |
| 5. One live campaign | Gated on remaining fresh deployment and DA prerequisites | Docker/provider blocker resolved; configured second DA signer unavailable, fresh manifest/state and public retrieval pending | NOT STARTED:0 campaigns,0 live transactions,0 new deployments |

## Implemented and verified flow

The actual production-owner emulator journey streams authenticated public raw
D/W observations through ingestion, durable submission/reconciliation, native
commitment, confirmation, mature merge, invalid withdrawal refund, deposit reserve
absorption, withdrawal payout and retained-data reclamation. Its independent
receipt/owner checks preserve final native/SQL agreement. Transport genesis,
ancestry and observations are synthetic; this is not live consensus evidence.

New fabricated-family cases traverse init and stages01–04 through permanent
evidence and removal for arbitrary absent IDs and mismatched content, including
withdrawal signature-only substitution. Honest inline/external D/W challenges
reach genuine Stage03 inputs and fail actual Spend evaluation; original CT,
header, order, Value and retained data remain unchanged. Signature-content
fidelity is separate from L2 cryptographic validity. Existing raw Data, original
multiasset Value, timing and retired-ID reuse assertions remain intact.

Selected proposed history profile:120000ms protection,512 inline bytes,5000
complete payload bytes,512 Data nodes. Production has no implicit test default.
The owner test now passes this protection value explicitly. Applied maximum
cases combine exact payload/node bounds, maximum inline predecessor,64-level
membership and nine distinct native policies with32-byte names and maximum
positive signed64 quantities. Deposit Value reaches the reserve unchanged;
withdrawal target Value reaches payout initialization, or its ADA-only order
is refunded when classified invalid. External data is then reclaimed.
Maximum reserve-add/final-payout spend is not established by those list fixtures;
the complete representative payout is established separately by the owner journey.

The first wide withdrawal attempt failed at17029bytes because the raw fixture
used token-bearing fee/collateral inputs. Production already forbids those inputs.
The fixture now uses genuine split pure-ADA UTxOs and the production selectors,
with unchanged maximum shapes, assertions and limits. Unrelated token UTxOs must
remain untouched. The original failure is retained. Four malformed-key fixtures
were corrected to reach script evaluation, and fixed protection waits now derive
from authenticated deadlines. No production validator or limit was changed.

## Evidence and exact check scope

Commands and exit codes are retained in `artifacts/event-history/parallel/`:

| Evidence record | Commands/results |
| --- | --- |
| `cp1-handoff.md`, `cp1-fresh-journey-run.json` | Pinned frozen install with zero reused packages; declared workspace/native builds; node `tsc --noEmit`; production D/W1/1 PASS |
| `cp23-v3-final-run.json` | Fault-proof `tsc --noEmit` PASS; fabricated D/W16/16 PASS; initial combined2/3 retained as a diagnosed fixture failure |
| `cp23-milestone-run.json` | Five opening/transition/installed files62/62; capture/output-claims25/25 PASS; generated fit ledger preserved and independently verified |
| `cp23-retired-reuse-run.json` | Dedicated watcher-journeys config and retired-ID selector:2/2 PASS,8 unrelated cases filtered |
| `cp23-v4-final-run.json` | Fault-proof typecheck PASS; combined3/3; all seven list/retention/public/reclaim/user-flow/asset files109/109 PASS |
| `cp23-v2-owner-run.json` | Node typecheck and actual owner D/W1/1 PASS at120000ms; independent owner/raw receipt verifiers PASS |
| `cp23-v3-broad-run.json` | `pnpm run test:tx-prep:sdk`: Lucid175/SDK642 PASS; `pnpm run test:tx-prep:node`: node125/tools51 PASS; `pnpm --filter midgard-node run test:tx-prep:emulator`:64 PASS |
| `cp23-final-docs-links.log` | From repository root, `pnpm --dir docs-site run check:links`:563 Markdown/MDX files PASS |
| `cp23-final-runbook-check.json`, `cp23-final-live-skill-check.log` | Current live runbook and skill validators PASS |

No claim is made that the entire fault-proof package or complete workspace test
inventory was rerun. The named node emulator command is the node part of the
broader tx-preparation emulator gate; the affected fault-proof selections are
listed separately. Previously pinned Aiken build/history195 results are retained
for the unchanged contract sources/compiler/blueprint, not invented as new runs.

Root independently rechecked163 family receipts,1340 installed workflow receipts,
22 retired-ID receipts, owner receipts and all six emitted final list/builder
measurement bundles. Two signed user-flow InputConflict attempts are explicitly
rejected, separately decoded and never counted as accepted transactions.
The combined maximum42 signed receipts have maxima15596bytes,12528579memory,
5995600251steps and2380742lovelace fee. Maxima can belong to different transactions;
these are local protocol-model measurements. Limits remain16384bytes,
16500000memory and10000000000steps. The complete list run separately distinguishes
399 selected-profile receipts from diagnostic larger-payload recipes.

Snapshot4 source identity:
`bcf15995ee5c0a3205edd0a9c36b2f1bab6bc4a2f3531baba67c46b4a3c620bc`.
Blueprint SHA256:
`7b7a0abae2db62e3ecb9e26e7907bbc1d4793eeee1a522470cc1c1014d1c8d74`.
Compiler:`aiken v1.1.23+5adf783`; Node22.22.2/pnpm9.15.4.
`cp23-root-final-code-application.json` proves all eleven integrated code files
match the verified snapshot; the separately verified generated fit ledger is
recorded in `cp23-root-fit-ledger-application.json`. Every snapshot and failure
log remains preserved. Build-input provenance and per-file hashes accompany
these records. No operator archive is treated as L1 authority.

## Concrete remaining prerequisites and limits

1. Resolved: Docker Desktop was started externally. Root verified no running
   Midgard producer, preserved the legacy provider containers, disabled their
   restart policies and replaced only provider services with the pinned
   `midgard-cp5-provider` project. Existing Cardano/Kupo data is preserved.
2. Resolved for provider readiness: actual images/configuration and independent
   genesis pin pass; Kupo/Ogmios are healthy and agree on the current tip. Old
   Kupo used pruning, so fresh activation must follow non-pruning bring-up;
   deleted historical outputs are not recovered or treated as authoritative.
3. Select fresh/attach/resume only from actual identity/state evidence. The old
   40-contract manifest lacks history recipes; the configured database is absent
   on the host route, which does not prove old Docker state absent. A distinct
   fresh identity must have matching SQL/native paths; never rewrite old migration
   checksums or pair clean local state with the old chain deployment.
4. Verify funded distinct wallet roles and one unspent hub nonce, finalize the
   current manifest/parameters/references, configure DA threshold/listeners and
   prove public retained-data retrieval before producer readiness.
5. Run the single live D/W campaign and retain signed receipts, observed fees,
   budgets, timing/contention, final reconciliation and unmodified finalizer
   verdicts. Declared emulator visibility/inclusion budgets are not live guarantees.

`cp5-desktop-providers.compose.yaml` records the active provider-only route;
`cp5-provider-repair-result.json` records passing live provider checks. The earlier
isolated-engine route and blocker records remain historical evidence. No protocol
state reset, deployment replacement or ambiguous submission retry was performed. A failed/ambiguous live submission must be reconciled before any
retry; this plan does not authorize silently restarting the campaign.

The full live finalizer still requires its broader proof-family and recovery
matrix. CP6 remains deferred at the user's boundary, and NIFP-04–09 are unchanged.
Neither this local checkpoint nor consistent saved artifacts close full protocol
acceptance. The final ABI/deployment freeze remains open until live readiness and
release-identity gates are satisfied.


## Provider repair and runtime realization (2026-09-24)

Root applied the prepared pinned Cardano11.1.0/Ogmios7.0.0/Kupo2.11.0 provider
configuration through Desktop after reviewing exact mounts and compatibility.
Cardano/Ogmios stopped cleanly; old Kupo exceeded its60second shutdown timeout
(exit137), so root preserved its stopped database and WAL and required a read-only
SQLite quick_check before reopening. It passed. Both Kupo revisions have the same
10 SQL migrations; no schema conversion/reset was needed. New Kupo runs without
pruning and with installed query indexes. Original containers and all protocol
state remain preserved; no bootstrap or producer was started.

The original node CLI initially resolved stale compiled SDK artifacts despite
matching source. Root coordinated restoration of189 differing/missing outputs
from the verified CP1 build, backing up126 replaced files. All701 runtime outputs
now match CP1, and all5093 recorded source files remain unchanged. The actual
built CLI help and `l1-provider-preflight --json` pass. Read-only preflight uses
explicit loopback endpoints, independently verified genesis pin and the existing
`bounded-acceptance-v1` economics profile; the legacy private environment was not
rewritten. Operator/merge/reference-publisher/user wallets are distinct and funded.
SQL administrative capability is available for a separate fresh database; no
new database, migration or on-chain deployment has been created.

Exact commands/results, image/genesis identities, backups and initial failures
are retained in `cp5-provider-repair-progress.md`, `cp5-provider-repair-result.json`,
`cp5-runtime-realization-handoff.md` and their referenced artifacts. A passing
provider preflight is not full checkpoint5 acceptance.


### Remaining concrete quorum gate

The configured DA committee contains two public keys, but `DA_THRESHOLD=1`.
Root independently evaluated the current SDK governed floor: the required
threshold is2. Only member0 signing capability has been established. Member1
public key is `d03e7e18910e54e2114f8ef0f5165460d7d086b3c5a1bba6111024b79fd513f0`.
The old single-member producer manifest cannot substitute for this configuration.

At the user's instruction, root and the existing builder agent searched relevant
local configuration/run directories, staging worktrees, evidence archives and
historical session references. Candidate private values were compared in memory
against the public key; no secret values were printed or committed. Original
checkout82 candidate files, archive122 files, the protected4096-wallet fixture,
and older matching Codex records yielded no matching signer. The worktree pass
inspected3047 bounded candidate files and also found no match. This establishes
only the searched scope, not that the key cannot exist elsewhere. Exact search
scope/results are retained under `cp5-*-signer-search.json` and the DA readiness
review. The additional bounded Claude archive search also found no matching
credential; its earliest relevant archives postdate the original June24 run.
No committee membership, quorum or private environment was changed.

The campaign remains unstarted pending usable signing capability for the
configured second member (local credential or functioning remote signer), or
an explicit new committee identity for the already-required fresh deployment.
A replacement committee is a governance identity change, not recovery of the
missing key. Remaining DA writer/reader roles, dedicated transport identities,
fresh manifests/state and the one coordinated campaign follow that disposition.
