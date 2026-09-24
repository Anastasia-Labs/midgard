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
| 4. Deployment preparation | Prepared: legacy identity/state review, distinct matching state route, isolated engine/provider-only commands, genesis-pin provenance, DA/public reader/funding checklist | Static checks PASS; actual engine/provider/deployment checks blocked | No service/config/database/deployment mutation performed |
| 5. One live campaign | Prepared and gated on1–4 | Prerequisite check BLOCKED: isolated Docker socket absent; privileged installation unavailable | NOT STARTED:0 campaigns,0 live transactions,0 new deployments |

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

1. Provision the prepared isolated local Docker engine with dedicated socket,
   Docker/containerd state and provider-only project. `sudo -n true` requires a
   password; rootless uidmap is absent. Desktop remains stopped to avoid legacy
   producer autostart. The final read-only Docker probe still finds no socket.
2. Verify actual local Kupmios images/configuration, independent genesis pin,
   retained history coverage and current tip. Preserve existing Cardano/Kupo data.
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

`cp4-live-isolated-route.md` contains the prepared operational commands;
`cp5-live-prerequisite-disposition.json` records the current blocker and zero live
attempts. No unsafe fallback, reset, deployment replacement or ambiguous retry
was performed. A failed/ambiguous live submission must be reconciled before any
retry; this plan does not authorize silently restarting the campaign.

The full live finalizer still requires its broader proof-family and recovery
matrix. CP6 remains deferred at the user's boundary, and NIFP-04–09 are unchanged.
Neither this local checkpoint nor consistent saved artifacts close full protocol
acceptance. The final ABI/deployment freeze remains open until live readiness and
release-identity gates are satisfied.
