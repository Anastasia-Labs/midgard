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
| 4. Deployment preparation | Fresh reset authorized; backup restore verified, exact Midgard SQL/native state reset and migrated; both DA seeds saved, sorted threshold2, isolated DA stores provisioned and response wallet funded | PASS: pinned Aiken build, provider/SQL identities, current SDK/node checks; mixed dependency graph repaired and six unchanged regression cases pass; full emulator gate PASS64 node +619 fault-proof submission | Fresh nonce and response funding confirmed; reference publication active under one identity; empty-genesis startup regression and DA bond top-up verified; init waits on references |
| 5. One live campaign | Run `cp5-live-20260924T172449Z` active; production owner/DA/public reader configuration prepared | Nine runtime consumers share dependency identities; previously failing selection and full payout emulator groups now pass; full emulator gate PASS; final manifests pending | One campaign/deployment attempt in progress; reference transactions confirming. Canonical block maturity is seven days, so final live merge/settlement/payout cannot complete today. No live deposit/withdrawal or complete acceptance verdict yet. Independent watcher/full-finalizer prerequisites remain open |

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


### Missing signer origin established (2026-09-24)

The later Windows-host archive search found the original command in the task
**Assess LIBp2p plan readiness** (`019eea71-ade6-7cd3-ac6a-7174946a4ad9`). On
2026-06-22 at15:47:40UTC, a Node command generated `producerDaKeyPair` using
`generateKeyPairSync('ed25519')`, exported only its public key and printed the
committee configuration. That public key exactly matches missing member1.
The command never accessed/exported the private key or wrote it to a file, and
its process exited0. The following successful patch saved the public committee
configuration to the node environment, not the missing signing key.

This supersedes the earlier search-only diagnosis: the keypair was generated,
but its private key was not persisted by its creation command. The separately
saved producer/watcher libp2p seeds are distinct keys; independent derivation
confirms neither produces the missing DA public key. Member0's existing signer
and the L1 submitter files remain present and unchanged. No replacement signer,
committee membership or threshold was installed. Continuing a fresh deployment
requires replacing the unavailable member while preserving governed quorum.
Root and a second agent independently reviewed the command, exact public output,
normal process exit and subsequent patch. Public-only provenance, record line
numbers and hashes are in `cp5-missing-da-key-origin.json`; raw archival records
are not copied into the repository because they contain other credentials.


### Component credential files (2026-09-24)

**Implementation:** Added a shared YAML loader and wired node/node-tools,
committee, public retained reader and watcher executable entrypoints. Local
`config.yaml` files now hold the existing node wallets and member 0 signer/L1
submitter credentials; incomplete files identify missing member 1, watcher and
public reader credentials. Private files are mode 0600, git-ignored and excluded
from Docker build contexts. Original environment/seed files remain intact for
existing shell tooling. No wallet, committee, threshold or deployed service was
changed. See [component configuration](../agents/component-configuration.md).

**Integrated verification:** Root independently reran the focused loader tests
(24 passed) and node environment tests (9 passed). Full core, node, committee
and watcher typechecks passed. Full builds passed for core, committee, watcher,
node and node tools. The first node build failed with TS2742 declaration errors:
its Effect dependency pointed into the transition worktree. A frozen, offline,
ignore-scripts pnpm install for the node restored checkout-local dependency
resolution; the unmodified full build then passed, including worker declarations.
No checks or assertions were weakened. Compiled service CLI checks confirm
malformed YAML fails before service startup and does not expose test secrets.
The copied credential values exactly match the previous sources and all five
private files pass permission/ignore checks. ESLint and documentation links pass.
Commands, logs and final file hashes are retained in
`artifacts/event-history/parallel/component-yaml-verification/`.

**Live verification:** Not run; CP5 remains incomplete. Missing member 1 signing
capability, watcher/availability credentials, transport configuration and the
fresh deployment remain prerequisites. Existing Compose deployments still use
their configured secret/env sources; YAML runtime mounts and matching watcher
JSON references must be configured before using these files in containers.
This credential organization does not close protocol acceptance gates.


### Replacement DA member 1 wallet saved (2026-09-24)

User explicitly authorized creating the replacement wallet. Its mnemonic is
persisted in the git-ignored mode0600 file
`demo/da-committee-node/run/member-1/config.yaml` under `DA_SIGNER_KEY_SOURCE`
with the supported `cardano-seed:` prefix. `DA_SIGNER_INDEX` remains1.
The file was written atomically and fsynced; reloading reproduces the address
and public key and passes an Ed25519 sign/verify check. No seed is recorded here.

- Network: Preprod.
- Public key: `865925c87aa511735bff3b5ee76072210f7f9e4f8b830d8f454a109f59315384`.
- Funding address: `addr_test1qz7pw7fketaqfkk0ecvnwty0mepfu3n4d577a8fqpjjkfa6ywfdtv2rrjwfjj6vv6zcr33dpelmsnws3jxhukwysqdps8nt3pe`.
- Public-only receipt: `artifacts/event-history/parallel/da-member-1-replacement-wallet.json`.

The configured/deployed committee has not been changed. This is a replacement
for the unavailable historical member, not recovery of that member's key. The
new public identity must be applied consistently during the already-required
fresh deployment, preserving governed quorum. Funding is not yet verified.
Watcher/availability credentials and remaining service/deployment prerequisites
remain open; no live campaign or transaction was run.


### DA signer credentials consolidated (2026-09-24)

At the user's direction, both DA signer seeds now reside in the existing
`demo/da-committee-node/config.yaml`, under `DA_SIGNER_KEY_SOURCE_0` and
`DA_SIGNER_KEY_SOURCE_1`. Each committee process chooses a source using
`DA_SIGNER_INDEX`; an explicit process setting overrides the YAML default0.
The committee config parser rejects mixed single/indexed sources, missing
selected keys and malformed indexed settings. Existing single-source process
configuration remains supported for callers that provide one signer directly.

The main file was atomically replaced and fsynced, its two signer credentials
and L1 submitter credential were verified against their original values, and
an independent process verified the moved seed before deleting the redundant
member1 file and empty directory. Mode0600 and Git exclusion remain in place.
The replacement wallet's funding address and public key are unchanged; its
public receipt now names the consolidated file and indexed setting. No running
service, deployment identity or committee threshold changed.


Verification for indexed signer selection includes18 new positive/negative
cases and all30 existing DA configuration tests, plus6 deployment-fixture tests.
The test run exposed an obsolete public DA fixture: four current retention/
retirement contracts and role-specific history metadata were absent. The
existing opt-in generator refreshed it from the current parameterized blueprint;
no Aiken build or generator changes were needed. The fixture helper now validates
and preserves exact role-specific metadata with the existing core parsers and
derives the initialization nonce from the generated recipe. Unknown/missing
field checks and production deployment verification remain intact. The source
preservation assertion now checks all source fields, including the new metadata.
Build, typecheck and focused verification evidence is retained under
`artifacts/event-history/parallel/da-indexed-config-verification/`. This does not
represent a deployment or live acceptance pass.

## Fresh live campaign — 2026-09-24, cp5-live-20260924T172449Z

User authorized a complete matching Midgard DB reset and on-chain redeployment. Preserve provider stores, all unrelated databases/worktrees, keys and old evidence. CP6 recovery variants remain deferred. Root owns all service, SQL, deployment and transaction actions.

- Implementation/configuration: sorted two-member committee now uses replacement865925… index0 and existing94427… index1, threshold2; wallet addresses unchanged. Both seeds remain in the main DA YAML. Distinct transport and availability-response credentials persisted privately. Docker PG moved to55433 and system ID7661255891566018595 verified; both existing mounts preserved. Two new isolated DA writer databases and read-only reader role provisioned.
- Integrated verification: pinned testnet Aiken build PASS, blueprint7b7a0aba… unchanged. Current Lucid175/SDK642 PASS; current node/tool checks running. Previous production-owner and emulator checkpoint evidence retained; this run does not relabel those as fresh live results.
- Live verification: original33-table Midgard dump restored successfully into an isolated temporary database, which was then removed. Original Midgard DB not yet reset. Provider preflight PASS. Fresh nonce40d92d4d7bf1f2e66c05d8007caf426cfef80ed2e67eaf198bc0e4eb8bba6bb9 submitted, awaiting confirmation. Exact state under artifacts/event-history/cp5-live-20260924T172449Z and node logs with same run ID.
- Remaining gates: confirm nonce, matching reset+migration+references+init, runtime manifests, both DA signers/public reader/producer readiness, real deposit and withdrawal through automatic merge and payout with retained evidence. Independent watcher signed release bundles, funding profiles, historical-script providers and native helper restoration remain open; full54-family/22-drill finalizer is not a claimed outcome of this bounded campaign.

Fresh campaign update17:44Z: confirmed nonce, reset exactly `midgard`, migrated current schema with no missing tables/indexes, archived old native/deployment paths. Initial empty root-owned db directory blocked rename; corrected ownership of that exact empty directory and finished archive/reset. All44 other current Docker DBs preserved (42 old plus2 newly provisioned DA databases). Current node125/tools51 and docs581 PASS. Fresh reference publication started with527 targets/132 planned batches; complete init and user journeys remain pending.

17:50Z: DA availability responder funded100ADA (95+5 plain outputs), tx6ae46d39ed4c30fa73d71353d0afc8dff90db80ebccd8740b1f7117a24a71d4c confirmed/exact outputs visible. Signed CBOR persisted privately before submit;367bytes/171705lovelace fee/zero script execution. Preparation helper initially requested Kupo health without JSON Accept header and failed before signing; corrected content negotiation without changing health assertions. Native architecture_g binary pinned to CP1 SHA6f2fe78b…; fresh manifest paths configured. Sequential emulator gate running separately from network-bound reference publication.

17:55Z lower-layer gate: current emulator run observed9 failures (3 external-data retirement serialization,3 operator activation datum casts,2 selection,1 merge/payout). Root stopped only this test process group (exit143), preserving raw log. Read-only agents found node using original Lucid/Effect while SDK resolves candidate-worktree modules; fixtures and SDK outputs match the prior passing snapshot. No fixture assertions changed. Dependency graph repair and unchanged narrow/broad reruns pending. Reference publication continues with already-loaded process; init/user transactions held until verification.

18:00Z: coherent offline workspace relink PASS (all11 importers, lock unchanged); independent pure runtime probe confirms shared Constr/Data/CML/Effect across9 runtime consumers. Six previously failing retirement/operator cases PASS unchanged. Full emulator rerun progressing: reserve26/operator16/selection3 PASS so far; no whole-suite pass claimed yet. Public-reader host profile will bind127.0.0.1 but announce dns4/localhost for unchanged Preprod client address grammar; this is local retrieval evidence, not internet ingress. Source and build identities remain recorded separately from dependency repair.

18:08Z: full unchanged node emulator lane64/64 PASS across8files after graph repair (681.64s), including all previously failing cases. Same workspace gate now running fault-proof submit-init emulator selections; no complete broader gate verdict yet. Current deployment reference publisher remains active under original fresh nonce/policy.

18:17Z stable verification milestone: current `pnpm run test:tx-prep:emulator` PASS(exit0),64 node tests/8files plus619 fault-proof submit-init tests/117files. This broader named gate now exceeds the earlier node-only emulator run. No assertions or production source were changed; repaired duplicate dependency instances only. Raw prior interrupted failure retained. Initialization held only for fresh reference completion/reconciliation.

### Fresh campaign: authenticated genesis startup correction

Bounded preflight found runtime Preprod config injecting six synthetic genesis UTxOs even though atomic initialization commits an empty ledger root. Native startup correctly refuses this mismatch. Root removed synthetic runtime balances/seed requirements and the production-owner fixture override that masked the defect. The strict on-chain root comparison remains unchanged. Explicit synthetic fixtures remain harness-owned. Configuration regressions pass13/13 across empty Preprod/Preview/Mainnet genesis and existing DA/MPF configuration. Native-owner integration, rebuilt node and final manifest genesis digest verification remain gates before initialization. Reference publication continues under the same nonce/policy and unchanged contract parameters.

### Fresh campaign: genesis verification and DA bond budget

Node typecheck/build pass after the empty-genesis correction. The integrated rerun passes103 tests (including all64 required node tx-preparation emulator cases) and fails one native startup/restart fixture with scheduler-window/end-time disagreement. Preserve the failed log and repair the bounded fixture only if its clocks are wrong; no assertion weakening. A read-only review confirms the existing reference publisher never finalizes a manifest and does not use genesis balances. First finalization must use canonical empty genesis digest `4f53cda18c2baa0c0354bb5f9a3ecbe5ed12ab4d8e11ba873c2f11161202b945`. Node YAML now pins the exact current campaign run-state path.

The DA L1 submitter funds each12000ADA availability bond. Normal merge does not release an Available bond and the current responder has no unchallenged automatic close path. Four sequential D/A/B/W headers require48000ADA locked plus fees/collateral; idle polling does not create additional headers. Root prepared a40000ADA top-up from existing campaign operator22000, merge9000 and replacement signer9000 wallets, targeting about50181ADA in the submitter while preserving other role budgets and the reserved init nonce. Each top-up retains a signed intent before one submission and exact-output reconciliation. Funding is pending confirmation; this is not a new bond-release implementation or a claim that locked capital is reclaimed.

Fresh campaign gate results: all103 integrated tests passed except the preserved native fixture clock failure; its additive scheduler alignment correction then passes the unchanged native startup/restart assertions, and the complete production owner D/W journey passes again (2/2 total). Tools typecheck/build and docs links590 pass. Root rechecks signed receipts, native/journal/cache equality and source/build identity separately. All3 DA top-ups are confirmed and exact outputs re-read; total fees518987lovelace, total funding40000000000lovelace. Submitter now has50181107260lovelace and the init nonce remains unspent. Reference publication continues; no live user event or acceptance verdict yet.

### Live maturity gate — preserve the canonical seven-day window

The shared consensus profile sets block maturity to604800000ms (seven days). SDK protocol parameters and deployment manifests derive that same value. This prevents today's freshly committed headers from reaching automatic merge and payout today. The wait is a protocol gate, not a provider fault; shortening it would invalidate the selected proof-window acceptance. Root will finish independent live admissions, canonical descendant commitments and threshold2 DA, record each actual header's earliest merge time, and retain automatic merge/settlement/payout as incomplete until observed. Existing automatic final-tail handling can drain one mature header below queue threshold once pending work is empty; no manual merge or force configuration is needed.
