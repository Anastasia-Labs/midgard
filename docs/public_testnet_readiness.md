# Public Testnet Readiness Checklist

Status: Active — no-go for an open public preprod deployment.

Source, GitHub and worktree reconciliation: 2026-10-02, 06:35 UTC
(01:35 CDT). Last full readiness review: 2026-09-01.
This update does not rerun launch acceptance or close any release gate.

This is the release acceptance checklist for an externally reachable deployment.
It covers adversarial safety, deployment identity, recovery, user and operator
lifecycles, public challenger access, and operations. Each requirement appears
once below; implementation inventories belong in their maintained domain docs.

The honest-path pipeline and watcher source catalogue exist. Source installation,
unit tests, and local emulator success do not establish public readiness. Use the
[fault-proof catalogue](fault-proofs/catalogue-status.md),
[coverage matrix](fault-proofs/coverage-matrix.md), and
[testing status](fault-proofs/testing-status.md) for current coverage.

Close a gate only with evidence from the release revision: owner/reviewer, exact
command and environment, artifact location, result, and known limitations.
Previously implemented portions of a combined gate still need release verification.

## Evidence and integration snapshot

The canonical filename is `docs/public_testnet_readiness.md`, as named in
[issue #724](https://github.com/Anastasia-Labs/midgard/issues/724).
"Public preprod" means an externally reachable deployment using the public
claim and approved public profile. `preprod-testing` and
`local-devnet-testing` are bounded testing profiles, with fault proofs explicitly
accepted as non-functional; they cannot satisfy that claim. See the
[deployment profiles](../config/deployments/README.md#live-testing-profiles).

Use these status terms independently:

| Status             | Required evidence                                                                                                                         |
| ------------------ | ----------------------------------------------------------------------------------------------------------------------------------------- |
| Implemented        | Behavior exists in the named source snapshot; a dirty patch is identified as uncommitted.                                                 |
| Committed / pushed | Exact commit and branch; a local commit alone is not published.                                                                           |
| Merged             | Named destination and ancestry or merged PR. Inclusion in a topic branch is not default-branch integration.                               |
| Tested             | Named command, revision, environment, executed count, result and artifacts. Historical or owner-reported results keep that qualification. |
| In progress        | Open work, incomplete review or integration, including committed implementations with unfinished acceptance.                              |
| Unverified         | Evidence is missing, stale, incomplete, skipped or from another revision/deployment. This cannot close a checkbox.                        |

The inventory below was read without modifying the implementation worktrees.
It supersedes older descriptions of the lifecycle lane as entirely uncommitted.
Dirty-path counts exclude the generated `onchain/aiken/plutus.json` and are
observations at this snapshot, not locks on other sessions' work.

| Source / worktree                                                        | Observed revision and integration                                                                                                                             | Readiness implication                                                                                                                                             |
| ------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| GitHub default branch `staging`                                          | `41284d897`; the checkpoint and reliability tips are not its ancestors.                                                                                       | Default-branch launch readiness is not established by the topic implementations.                                                                                  |
| Published checkpoint `colll78/canonical-v1-watcher-l1-source-checkpoint` | `dae1120a5`; [PR #471](https://github.com/Anastasia-Labs/midgard/pull/471) is OPEN into `tx-validation`, with no checks attached in the inspected rollup.     | DA pool, automatic settlement and earlier watcher work are integrated into this topic, not merged through #471.                                                   |
| Main checkout `midgard`                                                  | `3027be19c`, one committed module-splitting change beyond the published checkpoint, plus dirty node/watcher/funding/retention/startup and full-stack work.    | Source inspection and the October 1 local test reports do not establish committed or release-bound acceptance.                                                    |
| Reliability worktree `midgard-lifecycle`                                 | `colll78/devnet-lifecycle`: published `ef0c0f1a3`, local `f87d2c3f1`; 17 commits after `3027be19c`, six beyond the published tip, and 110 dirty source paths. | [Epic #696](https://github.com/Anastasia-Labs/midgard/issues/696) is the current reliability tracker. The program is not merged into the checkpoint or `staging`. |
| Verification mirror `midgard-lc-verify`                                  | Detached `3027be19c`, 517 dirty source paths.                                                                                                                 | Older mirror; its tests cannot certify the current lifecycle tree.                                                                                                |
| Early run `midgard-lc2`                                                  | Detached `4f6f3786c`, one dirty source path; [#706](https://github.com/Anastasia-Labs/midgard/issues/706) reports the isolated journey/drills in progress.    | This precedes later fixes. No completed final-run receipt was found in the ticket.                                                                                |

Eight other distinct local correctness/stack tips remain outside both published
checkpoint and reliability histories, and outside `staging`. Their 79 commits
after `3027be19c` are implementation inventory, not 79 accepted launch changes.
No corresponding published branch was found in the fetched origin refs.

| Worktree / branch                                                                   | Tip; new commits; dirty source paths | Integration or verification still needed                                                                                               |
| ----------------------------------------------------------------------------------- | ------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------- |
| `midgard-deepdata` / `deep-data-iterative`                                          | `c976edff5`; 7; 15                   | Pending Cardano-builtin parity work has a reported program-layout mismatch; current resolution is unverified.                          |
| `midgard-la` / `la-exact-reason`                                                    | `022207346`; 23; 0                   | Exact reasons and proof coverage need combined integration with deep-data and canonical redeemers.                                     |
| `midgard-f3port` / `f3-canonical-redeemer-data-split`                               | `921634ecf`; 12; 0                   | Current redeemer port; distinct from the DA-frame step-down issue #719. Do not also import the older pre-split copy.                   |
| `midgard-e2estack` / `e2e-stack`                                                    | `48b41e52a`; 15; 0                   | Separate stack implementation, to reconcile with `devnet-stack` in the lifecycle lane and the main checkout's dirty `full-stack` work. |
| `midgard-r0spec` / `r0-spec-pack`                                                   | `7495ed976`; 5; 0                    | Specification work does not prove corresponding runtime or validator implementation.                                                   |
| `midgard-t161`, `midgard-r1b` / `t161-input-resolution`, `r1b-pin-live-script-refs` | Both `299f12824`; 2 total; 0 each    | Same commits, count once. Input resolution is not proof that live script-material retention is complete.                               |
| `midgard-f1v3` / `f1-v3-terminal`                                                   | `d1ad70a7a`; 5; 2                    | MPF/encoding and artifact work; dependency edits still require reproducible packaging and combined rebuild/redeploy evidence.          |
| `midgard-rlo` / `redeemer-ledger-order`                                             | `5d3d84846`; 10; 1                   | Redeemer/one-successor work and an untracked probe; outstanding proof/review work remains unverified.                                  |

Two follow-up tickets filed after the earlier snapshot remain OPEN and deferred:
[#728](https://github.com/Anastasia-Labs/midgard/issues/728) assesses the pinned
MPF encoding's collision/preimage assumptions after reproducible dependencies,
supported transaction-shape fit and missing proof directions. No practical
exploit or observed preprod/devnet failure is demonstrated in its evidence;
a reachable construction and measured work are required before classifying it
as a launch blocker. [#729](https://github.com/Anastasia-Labs/midgard/issues/729)
tracks pure-move splits of the oversized committee and MPF process modules after
T161's necessary behavior changes. The cleanup is not evidence of behavior
repair or acceptance, and neither ticket closes an existing launch gate.

### Reliability epic #696

Ticket state, implementation and acceptance are separate. In particular, a
closed inventory ticket means the investigation finished, not that its defects
were repaired. These are the live GitHub states inspected for this update;
local commits below may be ahead of the ticket text.

| Tickets                                                                                                                                                                                                                                                                                                              | Observed state and evidence                                                                                                                                              | Remaining gate                                                                                                                                                                                                           |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| [#700](https://github.com/Anastasia-Labs/midgard/issues/700)                                                                                                                                                                                                                                                         | CLOSED; shared Ogmios code classification pushed in `ef0c0f1a3`. Closing comment reports 215 node, 31 core and 163 committee tests plus three typechecks.                | Batched review #714 and final integration/live evidence; those reports were not rerun here.                                                                                                                              |
| [#705](https://github.com/Anastasia-Labs/midgard/issues/705), [#710](https://github.com/Anastasia-Labs/midgard/issues/710)                                                                                                                                                                                           | CLOSED inventories. #710's body still says in progress; its final closing comment completes the sweep.                                                                   | Findings are open in #698, #713 and #725–#727. Neither closure establishes rollback recovery.                                                                                                                            |
| [#697](https://github.com/Anastasia-Labs/midgard/issues/697)                                                                                                                                                                                                                                                         | OPEN, in progress. `79e71b24f` contains displaced signed-intent recovery, but the ticket identifies a test-only production-routing gap and a post-CAS history-wait race. | Production-path regression, soundness review and restart evidence.                                                                                                                                                       |
| [#698](https://github.com/Anastasia-Labs/midgard/issues/698)                                                                                                                                                                                                                                                         | OPEN, implemented patch under adversarial review/refinement in the dirty lifecycle tree.                                                                                 | Retire instead of delete at confirmation, retain reservations to the safe boundary, rebroadcast identical bytes, remove the halt latch, and test v1/v2→v3 migrations.                                                    |
| [#699](https://github.com/Anastasia-Labs/midgard/issues/699)                                                                                                                                                                                                                                                         | OPEN; locally committed in `68d1ded92`, beyond the published lifecycle tip. Signed retained bytes remain servable during quarantine.                                     | Review and publication. The responder still refuses new L1 response submission under quarantine; authority recovery belongs to #713.                                                                                     |
| [#701](https://github.com/Anastasia-Labs/midgard/issues/701), [#719](https://github.com/Anastasia-Labs/midgard/issues/719)                                                                                                                                                                                           | OPEN. Base-aware budgeting/step-down exists in pushed `d16c007ff`; follow-up patch/review is in progress.                                                                | Kill surviving mutants, prove non-speculative SQL effects/readiness/hold budgets and measured ceilings; handle non-monotonic candidate size safely.                                                                      |
| [#702](https://github.com/Anastasia-Labs/midgard/issues/702), [#703](https://github.com/Anastasia-Labs/midgard/issues/703)                                                                                                                                                                                           | OPEN, in progress. #702 has local lease-reclaim, tracked-control-plane-hold and journal-lock commits `d33b650a2`, `e9d8067d0`, `60068c567`.                              | Publish and review; prove socket cleanup, bounded observation/removal stores and both-polarity regressions.                                                                                                              |
| [#704](https://github.com/Anastasia-Labs/midgard/issues/704)                                                                                                                                                                                                                                                         | OPEN; local `aad43c2a2` selects 10 confirmations for live testing, 3 for emulator, 30 for public/mainnet.                                                                | Its response-budget test counts 700 s of the 720 s small testing window, but A1 retrieval, A7 rebuild and B6 retry terms remain TODO tests. Complete-budget acceptance is unverified.                                    |
| [#707](https://github.com/Anastasia-Labs/midgard/issues/707), [#726](https://github.com/Anastasia-Labs/midgard/issues/726)                                                                                                                                                                                           | OPEN. #707's manifest-derived housekeeping patch is under review; #726 is not started in the ticket.                                                                     | Preserve incomplete/challenge-relevant records, serialize prunes under history authority, and retain removed-header DA bytes until removal is more than `k = 2160` blocks deep. Time retention alone is insufficient.    |
| [#708](https://github.com/Anastasia-Labs/midgard/issues/708)                                                                                                                                                                                                                                                         | OPEN, queued.                                                                                                                                                            | Measured per-role genesis funding/runway for months unattended; owner ruling is no refill loop.                                                                                                                          |
| [#709](https://github.com/Anastasia-Labs/midgard/issues/709), [#712](https://github.com/Anastasia-Labs/midgard/issues/712), [#713](https://github.com/Anastasia-Labs/midgard/issues/713)                                                                                                                             | OPEN, queued recovery work.                                                                                                                                              | Replace restart loops with evidenced holds/recovery; cover history owner, reversible correction, committee quarantine, watcher visibility, supervisor exit classification and the 65,534-parameter revival query limit.  |
| [#711](https://github.com/Anastasia-Labs/midgard/issues/711), [#714](https://github.com/Anastasia-Labs/midgard/issues/714), [#715](https://github.com/Anastasia-Labs/midgard/issues/715), [#720](https://github.com/Anastasia-Labs/midgard/issues/720), [#721](https://github.com/Anastasia-Labs/midgard/issues/721) | OPEN review, regression and cleanup obligations. The sticky-verdict, readiness-clearing and watcher-rewind commits are pushed.                                           | Independent review and fiber-level clearing regression; formatting/module limits and supervisor exit 78 handling remain obligations.                                                                                     |
| [#716](https://github.com/Anastasia-Labs/midgard/issues/716), [#717](https://github.com/Anastasia-Labs/midgard/issues/717), [#718](https://github.com/Anastasia-Labs/midgard/issues/718)                                                                                                                             | OPEN, ticket says not started. Main checkout has an uncommitted early HTTP listener/readiness patch, while lifecycle startup gaps remain tracked.                        | Bounded owning-operation retries; bind health before waits and expose the actual stage; report stopped commit loops, verified genesis/tip cadence and staged frame pressure. A constant `starting` reason is incomplete. |
| [#725](https://github.com/Anastasia-Labs/midgard/issues/725), [#727](https://github.com/Anastasia-Labs/midgard/issues/727)                                                                                                                                                                                           | OPEN, not started in the tickets; identified by #710.                                                                                                                    | Confirmation-depth terminals/abandonment, released funding, quiet-block history gaps, offline rollback and sticky watcher quarantine must recover within `k`, with separate adversarial review.                          |
| [#706](https://github.com/Anastasia-Labs/midgard/issues/706), [#722](https://github.com/Anastasia-Labs/midgard/issues/722), [#723](https://github.com/Anastasia-Labs/midgard/issues/723)                                                                                                                             | OPEN; early lc2 run in progress, four previously unrun drills being attempted; final proof blocked on fix lanes.                                                         | Fresh final-dist deployment, exact payouts, unattended full journey and all drills. `restart-cardano-node`, `stop-kupo`, `stop-ogmios`, `kill-public-retained-da` lack completed ticket evidence.                        |
| [#724](https://github.com/Anastasia-Labs/midgard/issues/724)                                                                                                                                                                                                                                                         | OPEN; this change reconciles the readiness checklist only.                                                                                                               | Deposit/withdraw guide and watcher README remain separate documentation work; this update does not close the whole issue.                                                                                                |

### What has actually been tested

- Historical watcher evidence now records **52 accepted family results** on
  2026-09-16: 34 preserved and 18 on a replacement deployment. The
  [journey record](fault-proofs/automatic-watcher-journeys.md#retained-live-status)
  names `/var/tmp/midgard-watcher-completion-20260916/accepted-family-index.json`
  and the recovered-run limitations. It is not a clean single-revision run of
  all 55 installed categories, nor proof of the newer rollback/reliability fixes.
- The [pooled DA bond live record](fault-proofs/automatic-watcher-journeys.md#live-status-and-process-evidence)
  reports all six steps on 2026-09-29, about 91 minutes, on a fresh
  `local-devnet-testing` deployment with real committee readiness/metrics and
  real CLI evidence. It was the fifth attempt. This remains historical local
  acceptance, not public-profile or final reliability acceptance.
- Automatic reserve settlement/payout is integrated in topic commit
  `98bf75197`. Its [verification ledger](../demo/midgard-node/docs/automatic-settlement-verification.md)
  records focused SQL/native/emulator successes and broad-gate failures,
  including a stale fit-ledger/blueprint identity. It explicitly lacks live
  node/watcher soak and performance acceptance. Manual CLI verbs are no longer
  the only implementation, but public payout acceptance remains open.
- October 1 reports in the main checkout's **untracked** <!-- doc-links:external -->
  `docs/exec-plans/public-testnet-decisions-2026-10-01/` describe focused
  funding, archive, startup and transaction-preparation passes on dirty source,
  with broad fault-proof and node database gates still failed. They used the
  superseded 12-confirmation patch and are not final-tree or 10-confirmation
  release evidence. No full-suite pass is inferred from their focused reruns.
- GitHub returned no workflow runs for `colll78/devnet-lifecycle` in the
  inspected branch query, and no attached checks for #471's current head.
  Earlier checkpoint Node/Watcher CI failures at other SHAs do not test these
  tips. CI for the combined release revision is **unverified**.

This reconciliation read source, commit ancestry, dirty worktrees and GitHub
tickets/comments/PR checks. It did not execute application tests, revalidate
historical run artifacts, deploy, reset state, run chaos drills or measure
throughput. The unchecked gates below remain release requirements.

## October 31 delivery target

The owner requires readiness this month: **2026-10-31**, with acceptance sign-off
targeted for Friday, October 30. There are 21 weekdays from October 2 through
October 30, inclusive. This is a deadline plan, not a measured completion forecast
or evidence that the release is ready. The current no-go and all requirements
below still apply.

At this snapshot there are **65 unchecked acceptance gates** and **28 open child
tickets out of 31** in #696. They are different units: gates combine implementation,
integration, review and release evidence; several tickets contribute to one gate.
The three closed tickets include two inventories. The October 1 batch of 17 local
reliability commits is not 17 accepted tasks per day. There is insufficient
observed completion history to extrapolate days remaining at a measured pace.

The planning assumption is five parallel workstreams with independent review
capacity throughout the month. Capacity and named owners are **unconfirmed**;
this document neither assigns contributors nor dispatches their existing work.
By October 4, reconcile every gate to an owner, current implementation, missing
evidence or decision, dependencies and a dated next result. Missing public
release/operations evidence must be investigated immediately, alongside #696.
For example, this checkout has no root `SECURITY.md`; package publication,
artifact provenance and backup/restore acceptance remain unverified. Do not
assume reliability ticket closure covers those requirements.

| Parallel workstream           | Required outcome and main dependencies                                                                                                                                                                                                                                                                                                          |
| ----------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Rollback and funds            | Complete #698, #713 and #725–#727: durable reservations, retained DA, reversible terminals/corrections and watcher recovery within `k`. Independent review must precede final live drills.                                                                                                                                                      |
| Contracts and execution       | Resolve [#683](https://github.com/Anastasia-Labs/midgard/issues/683) and [#695](https://github.com/Anastasia-Labs/midgard/issues/695), both open and unassigned at this inspection. Reconcile the correctness branches, builtin/redeemer parity, reproducible dependencies and multi-operator execution before rebuilding the release identity. |
| Runtime and clients           | Integrate admission, frame sizing, lease/socket lifecycle, funding runway, bounded retries, startup/readiness and supervisor fixes from #696. Verify deposit/withdraw/operator paths and the public SDK, including outstanding #724 guides.                                                                                                     |
| Public release and operations | Establish approved parameters/participation, manifest and references, TLS/custody, supported hardware and retention, immutable artifacts/package publication, provenance, alerts, incident contacts and demonstrated fresh-host restore. Audit existing infrastructure before estimating missing implementation.                                |
| Integration and acceptance    | Select one maintained integration branch without overwriting worktrees; combine reviewed changes once, publish its exact SHA and run full required suites/CI. Complete #711/#714/#715 review and #706/#722/#723 final-run evidence with the public release profile.                                                                             |

| Date, America/Chicago | Required result                                                                                                                                                                                                                       |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| October 2–4           | Audit all 65 gates, confirm workstream/reviewer capacity, name owners and identify every unassigned blocker. Choose the integration destination and reconcile duplicate stack/correctness implementations before merging.             |
| By October 5          | Resolve public participation, supported non-canonical claims, parameter/economic approvals, signer custody and funding decisions so the candidate can have one approved identity.                                                     |
| By October 8          | Demonstrate the rollback repairs and contract/execution approach with focused adversarial evidence. Surface any unresolved correctness or operations work that cannot fit before candidate freeze.                                    |
| By October 12         | Produce an integrated candidate with complete required suites and no unexplained failures; finish public ingress, artifacts and recovery infrastructure needed for acceptance. Historical focused passes cannot substitute.           |
| October 13–14         | Finish independent review, rebuild reproducibly and freeze the candidate's source, images, SDK packages, manifest, blueprint, references and public parameters. Prepare the scenario dependency schedule and funded roles.            |
| By October 15         | Deploy the fresh public-profile acceptance candidate and begin eligible lifecycle scenarios. Record exact event times and projected completion times for every long timer; deployment time alone starts none of those clocks.         |
| October 15–27         | Execute the complete public user/operator, fraud-proof, DA, multi-operator and recovery acceptance; collect exact payouts and authority/rollback evidence. Run independent release/operations verification in parallel.               |
| October 28–30         | Finish evidence review and applicable reruns, publish approved artifacts and operational documentation, and close each gate against the final release identity. This period is not enough for a new full public bond-withdrawal wait. |
| October 31            | Release only after all gates have current accepted evidence. Any remaining required gate means no-go; report the specific blocker, owner and revised earliest evidence date.                                                          |

The [public profile](../config/deployments/preprod-public.yaml) currently sets
seven-day block maturity and **nine days plus eight minutes** for DA bond
withdrawal, alongside a three-day DA challenge window, two-day full response
window and two-day slash grace. These are lifecycle-dependent waits, not a
single deployment soak. Start each scenario as soon as its prerequisites allow;
its schedule must account for sequential waits and L1 confirmation. A withdrawal
wait begun on October 18 finishes on October 27 plus eight minutes at the
earliest, before confirmation and evidence review. Shorter testing-profile
timings cannot close a public-profile gate.

Use the maintained
[live acceptance runbook](../.agents/skills/midgard-e2e-acceptance/references/live-acceptance.md)
and its finalizer. The runbook currently requires 22 recovery drills; #722's four
previously unrun drills are a subset. The 52 historical watcher results are not
release evidence for all 55 installed source categories. Require each applicable
proof direction and payout, exact deployment identity, and both successful
functional and clean-run verdicts, with no remaining safe action. Check the
runtime catalogue against the frozen release rather than treating these counts
as permanent authority.

Escalate missing owners/capacity on October 4, unresolved correctness approaches
on October 8, an incomplete integrated candidate on October 12, or an inability
to start the public acceptance scenarios on October 15. Recalculate the earliest
completion from actual event times whenever a repair or redeployment invalidates
evidence; do not assume a nine-day rerun fits the final three days. Keep deferred
#728/#729 behind required acceptance unless evidence establishes a reachable
defect that blocks a gate. This schedule does not defer canonical functionality,
security, recovery or required operational readiness to meet the date.

## Public claim and protocol scope

- [ ] Publish one security claim across announcements, SDK, operator, and challenger
      docs: supported features, challenge windows, trust assumptions, and limitations.
      Accept the entire canonical V1 surface, including tx-order, script transactions,
      mint/burn, observers, redeemers, reference scripts, and withdrawal categories.
      Configuration cannot disable normative features to bypass acceptance.
- [ ] Choose permissionless or explicitly curated operator participation. Curated
      access uses deliberate controls; permissionless access cannot depend on key
      ordering accidents. Document operator-halt and fund-recovery assumptions.
- [ ] For genuinely non-canonical scope, either accept the escape-hatch lifecycle
      (real mint/spend validators, initialization, trigger, reduced bonds, grace and
      penalties, CLI, recovery, tests) or explicitly mark it unsupported in public
      docs and the manifest. Likewise accept settlement resolution claims or declare
      them unsupported: claims, deposit/withdrawal/tx-order disproof, economic slashing,
      matured removal, restart, valid-claim maturity, and no-slash tests are required.
- [ ] Forced (tx-order) submissions use the immutable-submission format from the
      [forced-submission decision](midgard/decisions/forced-inclusion-submission-verdict.md):
      the L1 order carries no validity field and `OperatorVerdictV1` is the only
      committed operator claim. Its encoding is bound into the consensus profile
      as `forcedTransactionSourceEncoding`, so a manifest published before it
      fails deployment identity and must be republished. The format is integrated
      into the checkpoint topic and has local verification; #471 remains open.
      No current public-profile, release-bound live acceptance is established here.
- [ ] Publish a threat model covering runtime, APIs, key/admin compromise, L1
      provider compromise and rate limits, spam, read amplification, mempool behavior,
      operational DoS, and incident assumptions. Publish `SECURITY.md` with scope,
      reporting/encryption options, acknowledgement/fix targets, safe harbor,
      exclusions, and an emergency contact.

## Deployment identity and contract release

- [ ] Reproduce the testnet blueprint with the compiler pinned by `aiken.toml`
      and CI. Record source revision, compiler/fork, flags, `aiken.lock`, blueprint,
      script bytes/hashes, protocol parameters, Cardano era/protocol version, runtime
      rule bundle, and release hashes. Enforce local version/rebuild checks as well
      as CI checks; old prose hash pins do not authorize deployment.
- [ ] Publish a signed manifest and complete parameterization graph: blueprint
      title/purpose, unapplied hash, ordered typed JSON/Data-CBOR parameters,
      dependency links, applied CBOR/hash/policy id, and catalogue root. Define signing
      authority, threshold, independent verification, rotation, and revocation.
- [ ] Generate one canonical script/reference registry. Classify every validator
      as deployed, excluded, or internal/test-only; fail on unclassified entries.
      Every included proof category has a complete ordered chain, step hashes,
      parameter links, membership CBOR, and reference requirements. Verify hub oracle,
      state queue, scheduler, operator lists, reserve, payout, and proof chains against
      the manifest at startup.
- [ ] Required public reference scripts have non-null records: address, outref,
      script hash, CBOR hash, lovelace, publisher tx hash, and verification time.
      The registry covers initialization, operator lifecycle, state queue, user
      events, reserve/payout, settlement, and every proof category.
- [ ] Review each public parameter's rationale and redeploy impact: registration
      and maturity durations, bond, slash/inactivity penalties, prover reward,
      reserve/outbox values, and required protocol assets. Isolate demo parameters;
      zero economics require an explicit signed rationale. Reject empty delegated
      hashes/placeholders unless a permitted excluded feature is provably unreachable.
      Require live value-conservation evidence, not merely nonzero constants.
- [ ] Bind every Postgres, MPF/local, and DA store to the deployment. Persist a
      genesis marker containing network, one-shot outref, policy ids, manifest hash,
      and schema version. Fail before serving on missing/mismatched identity or
      manifest read/write/verification failure. Test readiness identity checks and
      reset coupling: wiping local DB/MPF state requires a fresh on-chain deployment.

## Public runtime, ingress, and key custody

- [ ] Provide a distinct public profile exposing only intended ingress through
      TLS. Keep Postgres, Grafana, Prometheus, Loki, Tempo, cAdvisor, Ogmios, Kupo, and
      Cardano services internal or authenticated; disable anonymous Grafana admin.
      Remove Docker socket/host filesystem mounts or use hardened collectors.
      Verify rendered compose/deployment port bindings.
- [ ] Fail before HTTP binding on missing public settings, blank admin keys,
      demo seeds, default DB credentials, mutable images, exposed internal services,
      missing identity, or unapproved placeholder economics. Define image and Mithril
      snapshot policy, CPU/memory/disk/DB/indexer sizing, and retention capacity.
      The owner-set production hardware floor from the
      [economics decision](midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md)
      is accepted and carried here verbatim:

      | Role                                         | Floor                                                                                                             |
      | -------------------------------------------- | ----------------------------------------------------------------------------------------------------------------- |
      | midgard-node (operator)                      | ≥ 32 GiB RAM, ≥ 16 vCPU (2026 gaming-PC class), NVMe storage                                                      |
      | DA committee node, midgard-watcher, Postgres | sized from C74/C86 measured usage plus ≥ 2× headroom; the §5.1 ceilings are containment caps, not recommendations |

      C86 bounded-stress results refine the non-node role sizing but cannot
      lower the node floor.

- [ ] Use mounted secrets/secret files and prove startup errors/logs redact them.
      Separate operator, merge, reference-script, admin, provider, and release key
      owners, balances, privileges, signer isolation, and rotation/revocation.
      Decide hot versus external/HSM/manual signing. Disable seed phrases in process
      arguments and shell history; public user commands never select operational
      wallets by default and reject that custody mistake before submission.
- [ ] Define an authoritative action map: public HTTP/CLI, operator-only CLI,
      admin HTTP, or internal. Test the route graph. Protect `/init`, `/commit`,
      `/merge`, `/stateQueue`, `/logBlocksDB`, and `/logGlobals`. Admin mutations use POST, scoped attributable signed or mTLS
      identities, key ids, rotation/revocation, replay protection, and idempotency.
- [ ] Bound request bodies/content types at proxy and app, per-IP/global rates,
      admission concurrency, and read query cost. Paginate tx/UTxO/block/status/batch
      APIs with stable cursors, result and response-byte limits. Load tests prove
      bounded memory, DB growth, and bandwidth under spam and large accounts/blocks.
      Document CORS, timeout/abort, structured errors, and retry behavior for
      200/202/409/413/415/422/429/503 and provider failures.
- [ ] Distinguish `/healthz` liveness from `/readyz` readiness. Readiness checks
      provider freshness, Kupo coverage, Ogmios connection, node sync, deployment
      identity, successful first iterations of required workers, and recovery state.
      A temporary provider failure alone must not trigger restart loops.
- [ ] Test SIGTERM drain: immediately become non-ready, stop admission and new
      commit/merge work, finish or time out handlers, cancel loops, safely release
      renewable leases, close DB/MPF/metrics, flush logs, and exit within stop grace.
      Leave durable jobs recoverable. Provide container healthchecks.

## L1 authority, finality, time, and funding

- [ ] Select exactly one source mode without inference or silent fallback.
      `local_node` uses one watcher-operated Cardano full node and chain-sync as
      authority; its Ogmios/Kupo/db-sync services are not independent quorum members.
      Validate their network and compatible canonical chain point. In
      `external_providers`, require at least two operationally independent operators
      agreeing on network and compatible chain points; quarantine disagreement or
      lost independence. Decode actual chain bytes deterministically, and propagate
      rollback through every index without replaying Cardano validator semantics.
- [ ] Apply source authority/agreement to commit, merge, deposit, withdrawal,
      reserve/payout, scheduler, operator, proof, and init observations. Fault-proof
      actions proceed on authenticated inclusion at fixed depth 1; their configured
      release depth governs stable evidence.
      Preserve unrelated lifecycle finality requirements. Persist transaction and
      chain-point identities, source authority, and observed depth. On rollback,
      invalidate cached authority, re-observe canonical state, reconcile outstanding
      submissions, then retry suitable signed bytes or rebuild under fresh authority.
      Test provisional terminal disappearance, restart, unresolved wallet inputs,
      and post-finalization incident recovery.
      Confirmation depth (10/12/30 in the inspected trees) is a liveness threshold,
      not Cardano finality. The automatic recovery bound is `k = 2160`; #698,
      #713 and #725–#727 track premature irreversible transitions and sticky holds.
- [ ] Anchor deposit/withdrawal events and commit barriers to stable indexed
      chain points. Test appearance, disappearance, reappearance at another point,
      and conflicting same-event payloads before projection/finalization. Readiness
      exposes network, era, tip hash/slot, indexed-through point, and maximum drift;
      logs retain observation authority and provenance.
- [ ] Use chain time for validity windows with one audited slot/time conversion
      module across all lifecycle builders. Bound local-clock skew and fail/degrade
      readiness on skew or unavailable conversion. Test stale slots, epoch boundaries,
      upper-bound inclusivity, and public isolation from Custom/emulator fallbacks.
- [ ] Centralize fee/collateral selection across protocol transactions. Inputs
      must be pure ADA, owned by the intended wallet, above configured minimums,
      unconsumed locally, and neither protocol/reference-script nor datum-bearing
      UTxOs. Verify collateral return, total/max collateral inputs, fee funding, and
      min-UTxO after balancing. Expose balances, fragmentation, refill thresholds,
      and recovery procedures.

## Admission, validation, and durable recovery

- [ ] Verify canonical native-CBOR admission, locally derived and response-checked
      tx ids, durable bytes/payload hashes, and status transitions. Same-id different
      bytes must fail across every insert/replay path. Every conflict-ignore
      transition needs exact-count checks or same-payload reconciliation.
- [ ] Make backlog limits concurrency-safe with a documented bounded tolerance.
      Validate using deterministic public-verifier data; distinguish permanent
      validation rejects from retryable infrastructure failures. Preserve reason
      metrics and tie adversarial Phase A/B fixtures to proof-data expectations.
- [ ] Test validation lease/batch restart, including a crash after rejections
      persist but before acceptances: rejects remain terminal, candidates retry or
      accept exactly once, and spent inputs never resurrect.
- [ ] Exercise commit/confirmation/merge crash boundaries with persistent DB/MPF:
      no duplicate blocks, lost mempool txs, unbounded finalization, or divergence.
      Verify transaction-root reset/replay after DB commit and exact canonical payload
      transitions between mempool, processed, latest, and confirmed tables.
- [ ] Accept shared verified history and deterministic foreign-block import across
      at least two independently operated nodes. Authenticate every header root,
      count and execution result before extending it; verify rotation, restart,
      rollback, peer outages and finalization without a fabricated local signed
      journal. Root-only foreign-base matching is not this acceptance (#695).
- [ ] Provide authenticated checkpoint/rejoin after an outage beyond hot retention,
      including handled events, deposit consumption, settlement and deployment
      identity. Verify complete recent replay and atomic installation; matching a
      claimed state root alone does not prove valid execution. No accepted importer
      or multi-operator catch-up run is established by the inspected work.
- [ ] Recover a submitted header before its local submitted marker, including
      deposit-only/user-event-only commits. Resolve by canonical header hash; do not
      build a competitor on the same base. Preserve the pending journal and finalize
      or abandon deterministically. Projection assignment and pending-status updates
      must be atomic or replay-safe.
- [ ] Recover merge confirmation before local job start and local DB commit before
      job completion. Detect queue advancement, prove completed effects or replay
      safely, and reconcile without manual DB edits. Preserve canonical DA payloads
      and verify their roots/counts before completing finalization.
- [ ] Expose lease/pending-finalization state and ages, mutation job ids/ages,
      processed depth, mempool overlap, missing immutable payload counts, unresolved
      submission age, merge failures, and queue length. Document manual recovery for
      pending finalization, leases, scheduler, merge, and intentionally abandoned
      commits, including how public watchers interpret them.

## Users, withdrawals, and operators

- [ ] Deposit build responses provide unsigned CBOR, event id, nonce outref/unit,
      auth/expected event units, valid-to, and expected inclusion/settlement timing.
      Document wallet funding, nonce selection, validity/retry, and public status
      discovery. Test duplicates, late deposits, malformed datums, and insufficient
      funding; monitor fetch/projection lag, divergence, and consumed-event conflicts.
- [ ] Declare whether withdrawal orders use L1 CLI/API, L2 submission, or both.
      Provide public build/status and external-wallet unsigned-CBOR/event metadata,
      or explicitly document local CLI custody limitations. Use shared production
      L1 submission/recovery, local UPLC evaluation, script-data-hash repair, timeout,
      and confirmation behavior.
- [ ] Classify exact L1 payout payability for `l2_value`, `l1_address`, and
      `l1_datum`. Persist diagnostic evidence; route `UnpayableWithdrawalValue` to
      invalid refund. Test min-ADA, token bundles, datum/address, and multi-asset
      edges. Wire production invalid-refund submission for the public lifecycle.
- [ ] Accept deposit reserve absorption, payout initialization, reserve funding,
      conclusion, and withdrawal finality from a clean public/preprod deployment.
      Expose payout phase, shortfall, next action, stuck/expired/invalid states, and
      alerts. Document reserve funding, batching, fee recovery, latency, and maturity.
      Make inclusion/settlement proof artifacts verifiable externally or explicitly
      describe privileged proof-resolution access. Complete tx-order lifecycle
      coverage under the canonical scope gate above.
      Automatic settlement/payout is implemented in `98bf75197`; retain the
      focused evidence and limits in its verification ledger above. The final
      real-stack exact-payout journey is still owed under #723.
- [ ] Provide register/activate/deactivate/deregister/status and bond/slash docs.
      Runbook: `docs-site/content/docs/operators/node/operator-lifecycle.mdx`.
      Permissionless activation must support empty/head/middle/tail insertion and
      stale-anchor retry; curated activation must use explicit access controls.
      Preflight per-wallet funding for bond, lifecycle, scheduler, commit/merge,
      reference scripts, collateral, and recovery. Define Sybil-resistant faucet or
      manual funding that still permits legitimate onboarding.
- [ ] Enforce missed commitments and neglected deposit/withdrawal/tx-order events
      through watchdog/scheduler takeover and inactivity strikes (runbook:
      `docs-site/content/docs/operators/node/operator-lifecycle.mdx`). Test no-event and
      neglected-event inactivity, single-operator strike limit, multi-operator
      takeover, and partial-slash retirement. Expose operator/shift age, missed age,
      strike count, next takeover time, and last takeover tx.
- [ ] Prevent or slash duplicate registration/activation/retirement combinations;
      provide removal tooling and prove duplicates cannot lock scheduler advance,
      rewind, retirement, or inactivity slashing. Verify deterministic slash/reward
      flows for active, retired, registered, duplicate, bad-state, bad-settlement,
      and partially inactivity-slashed operators.
      [#683](https://github.com/Anastasia-Labs/midgard/issues/683) remains an open
      on-chain attribution blocker: each descendant removal must bind the slashed
      operator to that removed header, with matching builder and refusal tests.
      [#695](https://github.com/Anastasia-Labs/midgard/issues/695) separately tracks
      complete foreign-header execution/commitment verification before extending
      it; a matching UTxO root or DA signature alone is insufficient. Neither
      repair nor a live exploit reproduction is established by this review.
- [ ] Accept safe rekey preserving bonds/scheduler semantics or explicitly require
      retire, unlock, recover, and re-register. Ruling 2026-09-15: no rekey; the
      runbook documents retire → unlock → recover → re-register. Test behavior during bond holds,
      pending shifts/commitments, and retirement. Document compromised-current-key
      response: stop signing, prevent unsafe commits, preserve audit state, recover
      or slash.

## Fraud proofs and public data availability

- [ ] Complete current signed publication/lifecycle checks and the availability
      challenge size plan in the [execution plan](fault-proofs/execution-plan.md).
      Every canonical family needs deployment-bound emulator, watcher, and preprod
      evidence; a first-family milestone cannot close the complete launch gate.
      The [local emulator scheduling measurements](fault-proofs/testing-status.md#emulator-gate-performance)
      preserve test scope and do not satisfy live acceptance.
      Availability reference publication and real contract lifecycles now pass the
      pinned mainnet protocol-11 size/execution gates; see the
      [measured scope and remaining operational work](fault-proofs/size-plans/availability-challenge.md).
- [ ] With real deployed validators and public data, accept invalid-block
      detection, init/steps/conclusion, permanent proof-token mint, fraudulent block
      removal, and operator/slashing effects. Prove valid blocks cannot be challenged.
      Cover size/budget bounds and maximum shapes, cancellation, retry, and resume.
- [ ] Pooled DA committee bond ([decision record](midgard/decisions/da-committee-bond-pool.md),
      spec #685): implemented in #686–#691. An attestation applies only while
      one `Bonded` pool holds at least one DA bond of backing; an availability
      timeout slashes the pool exactly (penalty as the fee, the rest to the
      challenger in one output); permissionless top-up, the owner-quorum
      two-step withdrawal and low-backing/`Withdrawing` alerts are in place.
      Liability is one bond per withholding episode. Fault proofs remain
      accepted as non-functional on the testing profiles. The public-profile
      bond figures await owner confirmation in F04; the earlier 500 ADA ruling
      was infeasible only while the DA bond had to equal the challenger bond.
      Live devnet evidence: the full journey (all six steps) passed on
      2026-09-29 on a fresh `local-devnet-testing` process devnet. The run took
      about 91 minutes and produced the real committee node's `/readyz` and
      `da_bond_pool_*` evidence and the real `midgard-node da-bond` CLI
      evidence.
- [ ] Bind every append/header to verifiable L1-visible DA commitment/attestation
      covering full tx payloads, opened preimages, proof metadata, and member counts.
      Independent committee/storage nodes validate the exact header/payload relation,
      attest it, and serve retained data independently of the producer.
      Prove bounded complete-frame construction, including the resulting ledger,
      scripts and traces, at release sizes. Step-down mitigates oversized candidate
      selection but does not remove a full-state frame ceiling. The live testing
      profiles also lack response time for a complete 4 MiB tranche; their
      availability behavior is not public-profile size acceptance.
- [ ] Persist versioned proof bundles independent of transient node internals:
      header, root role/schema/root/count, canonical key/value CBOR, membership and
      applicable non-membership/deletion CBOR, field preimages, source payload hash,
      and verifier ABI. Store exact committed tx-root proofs and verify recomputation
      after restart.
- [ ] Provide stable public retrieval APIs or artifact exports for bundles, root
      members, witnesses, family/hash metadata, pagination, and retention guarantees.
      A raw payload alone is not challenger-grade retrieval. External challengers
      must reconstruct and submit without privileged DB access.
- [ ] Accept a single watcher process per supported workflow: watch headers, fetch
      public evidence, detect invalidity, simulate, submit, resume, and observe final
      resolution. Persist attempt id, thread UTxO, submitted hashes, confirmation,
      next action, and safe recovery semantics. Verify the
      [challenger](fault-proofs/challenger-runbook.md) and
      [manual recovery](fault-proofs/manual-recovery-runbook.md) runbooks externally.
      The [automatic watcher journeys](fault-proofs/automatic-watcher-journeys.md)
      harness is the acceptance vehicle for this gate on a real devnet. As of
      2026-09-16 the maintained journey record reports 52 accepted families across
      two preserved/recovered deployments, replacing the older ten-family count.
      Preserve their receipts and provenance; they do not establish clean
      final-revision acceptance, the new within-`k` recovery behavior, or all
      canonical categories. Watcher installation covers all 55 source categories;
      installation is not acceptance. See the evidence snapshot above and #725–#727.
- [ ] Expose machine-readable rule/reject-code to family, script hashes, DA needs,
      and supported/disabled/unsupported status. Status cannot excuse missing
      normative coverage. Public tooling must not rely on incompatible-output or
      old-layout flags. Monitor proof-data publication/retrieval lag.

## SDK and public clients

- [ ] Verify intentional public exports and no accidental internal/test symbols.
      Test packed tarball ESM import, CJS require, and NodeNext types, plus a public
      API contract gate. Publish packages through the release channel below.
- [ ] Fail explicitly on invalid UTxOs/partial wallet state rather than silently
      discarding data. Keep protocol-info fallback explicit and unavailable by
      default for normal public use.
- [ ] Test cancellation throughout await/status/protocol calls, hung fetch and
      body reads, body failures, timeout classification, and user abort for protocol
      info, UTxO, submit, and status. Publish typed provider codes, retryability, and
      expected 400/409/413/415/422/429/503/5xx payload contracts.
- [ ] Document terminal/nonterminal status transitions, default await targets,
      `pending_commit`, and `awaiting_local_recovery`. Provide runnable browser/public
      endpoint examples for deposits, submit, withdrawal, polling, abort, and safe
      retries using published package versions.

## Release verification

- [ ] Run frozen install, builds, typechecks, and tests for core, SDK, Lucid,
      validation, node, fault proofs, DA, and watcher plus the pinned Aiken checks,
      native MPF, and relevant throughput gates. Retain exact commands/results.
- [ ] Provide one clean acceptance command sequence: fresh on-chain deployment
      and local state, operator onboarding, deposit, L2 transfers, commit, confirm,
      merge, withdrawal/reserve/payout, and restart. Verify DB/chain/API state without
      manual edits; retain tx hashes, queue state, balances, health/readiness, logs,
      and DB artifacts. Compose smoke must verify startup and safe port exposure.
      `devnet-stack` is implemented on the reliability branch in `84000fe08`;
      reconcile the separate `e2e-stack` branch and dirty `full-stack` work before
      selecting the release harness. #706 is an early run; #723 requires final
      dist and all fixes. A harness implementation is not a passing receipt.
- [ ] Accept persistent restart after projection, admission, submission,
      confirmation, merge, and payout; apply the crash cases above. Set pass/fail
      thresholds for spam, valid/invalid throughput, provider throttling, DB
      saturation, and API latency. Retain live proof acceptance evidence.
- [ ] Build public artifacts from a tagged revision with immutable SHAs/digests
      for actions, base/service/compose images, and release inputs. Fail release CI
      on mutable runner/image/action selections, including `ubuntu-latest` and tags.
- [ ] Generate CycloneDX/SPDX SBOMs for images, npm packages, and contract bundles.
      Scan vulnerabilities/licenses; block untriaged high/critical or prohibited
      licenses. Sign images by digest, publish npm provenance and contract
      in-toto/SLSA attestations; verify source, builder, locks, and command.
- [ ] Define coordinated versions or an explicit dependency graph, registry,
      `publishConfig`, CI publishing, and post-publish registry smoke tests. Reject
      `latest`, local file dependencies, unapproved runtime ranges, unresolved
      workspace specifiers, and locks from unapproved package-manager versions.

## Operations, retention, and public support

- [ ] Alert and dashboard readiness, stale workers, admission age/depth/rejects,
      provider lag, DB saturation, disk, unresolved finalization, merge failures,
      deposit/withdrawal/payout state, DA/proof lag, and public error spikes.
- [ ] Retain audit trails for admission, validation, commitments, merge, payouts,
      and admin actions. Enforce the current minimum DA availability window; pruning
      cannot remove data needed for proofs, recovery, payouts, or support.
- [ ] Classify on-chain/DA/canonical tx/index/log/support/backup data as immutable,
      retained, or deletable. State that public on-chain/DA data cannot be deleted.
      Define status minimum/terminal retention, pruning/expiry responses,
      `retainedUntil`, support windows, and consistent DB/log/backup policy.
- [ ] Remove raw CBOR/body logging on public routes; hash/truncate addresses and
      query values unless necessary. Test redaction of seeds, keys, credentials,
      requests, provider responses, tx CBOR, addresses, and support identifiers.
      Define log access audits, deletion/anonymization, and legal holds without
      corrupting proof reconstruction or recovery.
- [ ] Provide stable response correlation ids, support identifiers and safe
      diagnostic exports (tx/event/header/request ids, status history, log spans).
      Define severity, ownership, escalation, and response targets. Publish a public
      status/explorer surface for health, limitations, confirmed/merged blocks,
      user/payout status, incidents, maintenance, and resolution updates.
- [ ] Drill encrypted offsite backup/restore of Postgres and MPF on a fresh host,
      PITR/WAL archiving, manifest consistency, provider/indexer rebuild, key recovery,
      and failover. Set RPO/RTO and the exact stop-and-redeploy boundary.
- [ ] Publish operator setup/deploy/onboarding/upgrade/recovery and user
      wallet/protocol-info/deposit/submit/status/withdrawal runbooks. Include provider
      and DB outage, invalid block, stuck queue/payout, scheduler, backup, reset,
      and incident communication procedures with impact and resolution criteria.

Launch remains no-go until these gates have release-bound acceptance evidence
and the public claims match the accepted deployment.
