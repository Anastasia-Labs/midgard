# Engineering Midgard for reliable agent contribution

Report date: October 2, 2026, America/Chicago. Transcript snapshot: approximately
11:18 p.m. CDT. Repository inspected: `48b41e52aea75a2d4248419dea755816da277df7`.

Midgard's largest opportunity is to make **the correct working environment,
artifact identity, resource ownership, and verification result properties of the
tools themselves**. The transcripts repeatedly show contributors reconstructing
these properties with shell commands, temporary Python programs, copied files,
hash manifests, and lengthy coordination. More instructions alone will leave
most of this work in place.

The highest-value first changes are a guarded focused-test runner, complete
build provenance, coordination of build and test resources, and an executable
deployment publication gate. These address demonstrated mistakes and blockers.
Next come reusable recovery fixtures, machine-readable run receipts, and a small
workspace/program inventory. Protocol decisions and adversarial review remain
judgment tasks; deterministic tools should supply their evidence.

This report proposes changes. It adds no runtime behavior, installed skill,
deployment, dependency, issue, or commit. Priorities below concern contribution
infrastructure, rather than replacing either program's delivery priorities.

## Scope and evidence

I located **Offchain fixes** and **Resume devnet reliability program** through
the app and inspected the local root transcripts to recover the earlier history
omitted by the app's latest-item view. The captured prefixes contain 121 and 235
human-visible message records respectively, and 1,550 and 1,820 completed command
records. These are record counts, not distinct tasks, defects, or successful
tests. Both chats were active when captured.

[EVIDENCE.json](EVIDENCE.json) records source paths, captured prefix lengths and
hashes, timestamps, 54 selected observations, and limitations. The evidence
register below links important observations to their original transcript lines.
I also inspected existing repository tools and three retained reliability audit
artifacts. I did not decrypt private reasoning, exhaustively inspect every child
transcript, or rerun historical behavior tests.

The retained independent reliability audit classified the original 31 issue
entries as two concerning one observed stall, seven with existing-code
reproductions, twenty derived from inspection/safety/coverage requirements, and
two missing acceptance evidence. These are the audit's historical classifications,
not a fresh evaluation of each issue. Defects introduced by proposed recovery
code are separate. The historical full-check receipt records 49 of 52 checks
passing and exit 1; it does not establish current program completion. [E1, E2]

### Existing foundations to extend

| Existing capability                                                                                                                                 | What it already supplies                                                                    | Gap relevant to these transcripts                                                                       |
| --------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------- |
| [doctor](../../../scripts/doctor.mjs)                                                                                                               | Read-only prerequisite checks, JSON and actionable fixes                                    | Several dist checks use timestamps; diagnosis does not prepare or own an invocation                     |
| [preflight](../../../scripts/preflight.mjs)                                                                                                         | Derived check selection, dependency closure, capability probes, structured results          | No shared protection against another process mutating build outputs; some required checks remain manual |
| [blueprint setup](../../../demo/midgard-test-support/blueprint-stamp-setup.js) and [stamp library](../../../demo/scripts/lib/blueprint-stamp.mjs)   | Refusal of mismatched blueprint inputs/compiler/profile                                     | Does not establish freshness of every TS/native artifact or every generator's imported SDK              |
| [local test environment skill](../../../.agents/skills/local-test-environment/SKILL.md)                                                             | Known setup, package environment, shard naming and dist pitfalls                            | Much of the recipe is still executed by hand; focused vitest can bypass pretest                         |
| [devnet skill](../../../.agents/skills/running-the-devnet/SKILL.md) and [acceptance skill](../../../.agents/skills/midgard-e2e-acceptance/SKILL.md) | Stack selection, ownership guidance, ready-versus-working distinction, existing e2e harness | Ownership across sessions and shared host resources is partly a prose convention                        |
| [artifact channel registry](../../../.agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json)                                         | Generator/input/output inventory and coverage limitations                                   | Some channels are historical or unchecked; invoking a generator can still use stale compiled inputs     |
| [publication fit verifier](../../../demo/scripts/verify-canonical-v1-cg1-control-publication-fit.mjs)                                               | Validates a recorded roster, arithmetic and available blueprint basis                       | Its documented contract verifies a receipt; it does not rerun signed publications                       |
| [safe commit runner](../../../.agents/skills/committing-safely/scripts/commit-paths.mjs)                                                            | Explicit path selection and protection of unrelated staged work                             | Integration packets and source hashes are still assembled with bespoke scripts                          |
| [source facets](../../../scripts/lib/source-facets.mjs)                                                                                             | Shared support for readers of split implementation files                                    | More filename guessing and source-text consumers remain; runtime entrypoints need independent checks    |

The recommendation is to extend these owners. Creating a second doctor,
acceptance harness, artifact registry, or safe-commit implementation would
increase the surface contributors have to learn.

## Repeated problems and the engineering changes they justify

### 1. A focused test command does not reliably identify the code being tested

The offchain thread regenerated ABI fixtures against an older SDK build; six
failures disappeared after the correct build was used. The reliability thread
also lost a compiled test fixture when a package build ran concurrently. Both
are verification orchestration problems with direct transcript evidence.
[O3612, R12714]

The repository already documents the cause: vitest normally resolves workspace
source while plain Node children resolve dist, and focused vitest skips pretest.
Doctor checks several dist directories through timestamps. Moving sources
backward in time can defeat that check. An override blueprint path can also
downgrade stamp failure to a warning. These current code facts do not prove every
historical test was affected, but they identify where enforcement belongs.

**Recommendation:** add a focused-test entrypoint owned by repository tooling.
It should resolve the package-local runner, declared environment, selected files,
compiled-child prerequisites, blueprint profile, native binaries, and test
database identity before execution. It should print a plan and write a receipt.
The CLI takes structured file/name selectors instead of passing arbitrary shell
text through package scripts. Zero executed tests, missing reports and setup
failure remain explicit non-pass outcomes.

Build provenance should cover the entire build input closure: sources, exports,
tsconfig, bundler config, dependency lock, compiler/runtime versions, native
sources and applicable generated inputs. Extend the existing core digest pattern
to SDK, validation, node, tools and native consumers. Validate the subprocess
artifact actually loaded, rather than merely the directory believed to contain
it. Clean builds and cache hits should carry the same identity.

**Proof of improvement:** edit a worker source without building; the test runner
rebuilds or refuses before running. Swap an SDK dist from another checkout, move
source timestamps backward, change an export condition, or select no tests; each
produces a specific non-pass result. A source-only suite that needs no dist still
runs without an unnecessary full-workspace build.

### 2. Isolated checkouts still contend for shared resources

Separate worktrees do not isolate a Docker daemon, host ports, test Postgres,
shared dependency storage, or a build directory used by another test. The
transcripts contain repeated manual allocation of compiler owners and heavy-build
queues, as well as the acknowledged build/test conflict. The later lease-failure
cluster was still being investigated at capture time; do not label all those
failures production lease leaks. [R12714, E2]

**Recommendation:** give each invocation a run ID and resource declaration. Use
exclusive leases for mutable build destinations and database reset operations,
and shared leases for consumers of frozen artifacts. Build off to the side and
publish a complete artifact atomically where the platform permits it. Prefer
immutable build paths for live services and compiled test children, so a later
build cannot replace the files beneath them.

Extend the existing worktree identity helper with an invocation suffix for
same-package concurrent test runs. For devnets, allocate and record ports and
compose project identity together; a generated worktree offset alone does not
isolate two runs from the same checkout. A process receipt records PID, process
start identity, owned children, output limits and shutdown result. Cleanup acts
only on registered resources and does not infer ownership from a process name.

A small local admission queue can limit concurrent memory-heavy builds based on
declared resource classes. Queueing and allocation are deterministic; the tool
does not need to predict exact memory use to prevent two exclusive writers.

**Proof:** run two test invocations in one checkout without shard collisions;
start a build while a compiled-child test holds its artifact; verify orderly
queueing or immutable consumption. Cancel a run and prove all owned children
join. A second run's services, chain and files remain intact.

### 3. Initialization and cold proof routes fail after component tests pass

The offchain transcript reports 5,145 Aiken tests and 876 SDK tests passing,
followed by initialization failing to publish a validator. Later publication
inventory found additional oversized scripts, and a cold canonical observation
measured 16,554 bytes against a 16,384-byte transaction limit. Production manifest
bindings also omitted scripts needed by that canonical route. These are distinct
deployment and delivery failures, rather than contradictions of the component
tests. [O3968, O7900, O8856, O9613]

**Recommendation:** make publication and route coverage an executable release
gate derived from the production contract binder and role roster. For every
enabled parameterized role, build the complete publication transaction through
the real builder, sign, evaluate/submit in the appropriate local fixture, and
record complete bytes, execution units, margins and applied script hash. Keep
the existing receipt verifier as a fast consumer of those measurements.

The gate also walks enabled proof routes and checks that required roles are
bound and available. Exercise cold startup, prepared references that have been
spent, repeated identical chunks, direct carriage, reference fallback and
restart. Distinguish the chain's maximum size from an intentionally stricter
publication margin. Parameter combinations affecting applied script size need
declared coverage; one typical binding does not prove all bindings fit.

**Proof:** increase a role beyond its signed envelope or remove an enabled route's
binding; the relevant gate fails before a broad initialization journey. Restoring
the binding or reducing the script makes the same gate pass against new hashes.
Its receipt expires on any relevant blueprint, binder or protocol-limit change.

### 4. Handwritten fixtures drift from schemas and protocol state

New witness fixtures omitted schema fields and failed in serialization before
testing the intended behavior. Other budget-refusal cases failed in fixture
construction. A shared recovery-depth change broke unrelated cases. Migration
assertions also retained assumptions about an earlier schema set. [O2618,
R13506; command history and current global setup]

**Recommendation:** put typed canonical fixture builders in shared test support.
Construct witnesses through production encoders/converters and use fully typed
values. Provide explicit mutation methods for deliberately malformed bytes;
do not route valid fixture construction through unchecked casts. An error inside
setup is a setup failure, not evidence that a validator refused a fraudulent
transaction.

Use a deterministic chain fixture with separate block height, slot, hash,
ancestry, provider observation, confirmation count, recovery distance, and
rollback operations. Synthetic point identifiers stay labeled synthetic. Fixtures
should expose the manifest-derived migration set and protocol profile rather
than hiding assumptions such as version 1 or a hardcoded seven-day interval.
Keep tests of a fixed migration upgrade pinned to that historical schema; do not
make every migration test derive its expected answer from the implementation.

**Proof:** fixtures validate before the behavior test begins; failure assertions
name the layer expected to reject. Cover shallow/deep boundaries, inclusion-block
counting and divergent provider branches with explicit coordinates. Preserve
the original scenario when correcting its expectation.

### 5. Database setup is isolated by worker, but state can leak between files

The current global setup provisions and migrates one database per worker. Some
fixture cleanup uses enumerated TRUNCATE statements. This creates an engineering
hazard when a new lease or recovery table is omitted, although the active
reliability investigation had not established every failure's cause at capture.

**Recommendation:** introduce a scoped database fixture with a namespace or
database unique to a suite invocation and an explicit per-test cleanup contract.
Assert no unexpected leases, plans or active sessions remain after the owning
scope closes. Reset only databases attested as disposable test resources. New
migration tables should be covered by a reset inventory or a scratch database
strategy instead of relying on scattered cleanup lists.

Database-backed tests need meaningful ordering controls: execute the same affected
files in isolation and in reversed/seeded order, preserving the order seed in the
receipt. Fixing cleanup is preferable to widening lease timeout tolerances.
Dedicated SQL-boundary tests should verify batching before PostgreSQL parameter
limits, rather than discovering the same error across unrelated integration
cases.

**Proof:** seed a leftover lease in one suite and run the next through its normal
fixture; it receives a clean state or an explicit leak failure. Repeat after
adding a table. A concurrent invocation's database is neither dropped nor cleared.

### 6. Recovery tests need a common executable state model

The programs repeatedly distinguish shallow confirmation from recovery-safe
retirement, prove ancestry rather than borrowing depth from another provider,
reopen completed workflows after rollback, and preserve journals named by
compensation plans. New recovery work also introduced fresh-plan identity races.
[R1945, R2143, R2984; E1]

The obsolete family runner cleanup illustrates why a shared path needs scenario
coverage. Initial focused passes did not cover two or more descendants or rewards
from distinct operators. Review found both gaps. The owner then explicitly
requested deletion of the obsolete orchestration while retaining evidence checks,
builders and validators. [O9637, O10214, O10321, O10758]

**Recommendation:** create a small reference model and reusable scenario runner
for the shared workflow engine. Represent submitted, included, confirmed,
recovery-final, orphaned and contradictory observations separately. Persisted
plans carry the journal identity/generation they were derived from. Resource
release follows the protocol's authenticated terminal condition, not a generic
"completed" boolean.

Parameterize proof direction, descendant count, operator identity, reward
destination, caller/prover identity, prepared/signed/submitted stage, restart
point, and rollback depth. Use a compact set of required scenarios plus targeted
pairwise combinations; an enormous Cartesian suite would become another source
of latency. Exercise the same cases against real storage and the production
engine, not only the reference model.

**Proof:** both directions, zero/one/multiple descendants, distinct operators,
already-slashed cleanup, restart between removal steps and rollback from a
previous completion. Negative controls retain signed resources when status is
ambiguous and reject contradictory evidence. The live acceptance gate remains
separate from synthetic/emulator coverage.

### 7. Cross-process string conventions are protocol interfaces

The strongest observed reliability incident was the mismatch between a parent
emitting `node-commit:` lease owners and a worker accepting `commit:`. It stopped
commitments while readiness remained healthy. [R270, R11213]

**Recommendation:** centralize serialized worker messages and owner identities in
a shared boundary module with constructors and parsers. Consumers should receive
a typed owner kind and identifier rather than independently interpreting prefixes.
Validate the actual serialized form at the process boundary.

Add a compiled parent-to-worker integration test and expose a deterministic
boundary conformance command for other TypeScript/Go/Rust seams. Ordinary unit
tests resolving src imports cannot prove that the compiled worker accepts the
message emitted by its actual caller.

**Proof:** the original owner mismatch fails the compiled contract test and
produces an actionable reason. A successful compiled round trip also performs
the intended state transition; a process merely accepting the message is weaker
evidence.

### 8. Cancellation and ownership are implemented repeatedly

The reliability transcript describes a former file-store owner writing after
takeover, shutdown children surviving, cancellation allowing work to continue,
unbounded child output, and holds clearing after supporting files changed. Several
of these were defects in newly proposed recovery code, not original incidents.
[R3090, R11691; later root history]

**Recommendation:** consolidate proven process/network lifecycle primitives:
scope signal propagation, bounded stdout/stderr, request IDs, monotonic deadlines,
fenced writes, atomic file replacement and joined cleanup. Start with the repeated
adapters already present; avoid introducing a new lifecycle framework. Hold
clearance consumes a freshly checked evidence identity after asynchronous waits.
Wall-clock timestamps belong in logs; elapsed-time limits use monotonic time.

A helper's unit tests need a small set of real consumers as conformance cases:
CLI startup, history recorder, DA drain and native retry. Routine dependency
outages should become bounded retries or accurately reported holds where safe.
Integrity failures retain their strict refusal path.

**Proof:** SIGTERM during startup and active I/O, timeout followed by a late
response, takeover while an old writer pauses, backward clock step, and changed
configuration during an asynchronous proof. All owned children/sockets join and
no expired owner mutates state. Do not infer caller coverage from helper tests.

### 9. Readiness is disconnected from useful progress

The original lc2 incident remained healthy while 885 lease-owner failures stopped
commitment work. New DA drain code also logged failure without changing readiness.
[R11213; reliability root history]

**Recommendation:** make progress and pending obligations explicit inputs to
service status. Report pending work, last successful transition, recent dependency
failure, recovery stage and evidence required to clear a hold. A quiet chain is
not a stall: only require progress when work is eligible, and account for normal
cadence, maturity and provider lag.

Provide a read-only diagnostics command that packages effective configuration,
artifact identities, service state, provider tips, pending obligations and
recovery reasons. Use read-only database/journal APIs; opening a journal that
migrates it is unsuitable for observational tooling. Redact wallet credentials
and sensitive transaction material by default. Existing inventory concerns that
failed reproduction stay deferred; this proposal does not promote them to an
observed bug.

**Proof:** pending deposits plus repeated worker failure yields an unhealthy or
blocked working status with the responsible reason. Recovery clears that reason
through successful work. Empty queues remain healthy. Collecting diagnostics
changes no durable file or schema.

### 10. Optional policy can unintentionally restrict ordinary operation

A proposed timing profile required a different storage mode and limited fresh
signing to roughly once every twelve minutes. A new store limit then rejected
ordinary PostgreSQL row 513 while JSON accepted it. These were reproduced
regressions in the proposed improvements. [R12176, R14839]

**Recommendation:** model ordinary mode and explicitly selected policies as
separate configuration states. Bind persisted adoption to the selected policy
identity so restart cannot silently drop it. Test ordinary JSON, ordinary
PostgreSQL, selected policy, and restart with missing/changed policy. Shared
runtime adapters must not smuggle calibration thresholds into ordinary mode.

Timing/capacity analysis should follow actual causal paths and serialized
resources. The transcript's simple sum of waits crossed unrelated roles; later
analysis kept that original budget failure visible. A successful local builder
measurement is not a worst-case provider/network guarantee. [R712, R13718]

**Proof:** unchanged ordinary configuration still admits the same valid workload;
an adopted profile refuses unsupported obligations without withdrawing existing
promises. Measurements name the workload, machine state and date. Correct safety
refusal is reported as a limit, not described as a recovered crash.

### 11. Dependency and artifact regeneration need clean reproducibility

The offchain program recovered a dependency commit unavailable remotely and
verified its preserved archive before later publishing the pin. Declaration
builds also needed more heap. Generated fixtures and execution ledgers drifted
after composed changes. [O1798, O1896, O5502, O6885]

**Recommendation:** add a clean-checkout reproducibility lane that resolves every
pinned Git dependency, performs frozen installs, builds native and TS boundaries,
generates declared artifacts, and verifies them using the selected compiler and
profile. Local caches should improve speed without being necessary for correctness.
Prohibit absolute temporary tarball dependencies in deliverable manifests.

Extend the existing artifact registry with executable prerequisites and input
digests. A generator rebuilds or refuses a stale SDK, writes outputs to a temporary
location, validates them, and publishes only owned paths. Compose shared ABI
changes before regeneration; independently generated branch outputs do not become
consistent merely by choosing one side of conflicts.

**Proof:** a fresh environment builds without an author's private cache. Changing
an encoder invalidates its fixture receipt. Artifact verification identifies
exact stale inputs. Recorded benchmark variability and historical snapshots stay
distinct from release gates; do not turn every noisy measurement into an equality
check.

### 12. Splitting files affects readers and runtime topology

The repository's module-size report documents ESM initialization, facade-only
source guards, missing scenario registrations and incomplete source pins after
extraction. The current transcripts also repeatedly guess filenames that no
longer exist. These are discoverability and composition issues, not a reason to
abandon cohesive module boundaries.

**Recommendation:** keep one authoritative package export/runtime-role catalogue
and extend existing facet traversal where a tool genuinely needs source closure.
Use runtime imports or AST reachability for executable registries. Use typed
registries instead of reading source text when evaluating behavior is possible.
Check root ESM imports and built CLI help after boundary changes. Preserve state
owners and evaluation order; a line-count split alone does not prove correctness.

An optional symbol/entrypoint map can be generated from exports and existing
registries. It should help a contributor find the current worker, role, generator
and test owner, rather than becoming another manually maintained inventory.

**Proof:** mutate an imported implementation facet and observe the relevant
source guard or pin invalidate. An orphan test title does not satisfy scenario
coverage. Type-only imports do not create executable coverage. Root modules load
without initialization cycles.

### 13. Verification receipts are still authored rather than emitted

The offchain root explicitly caught a build receipt whose command only asserted
the compiler version. Other passages distinguish failing-before tests from
guard-removal mutations, filtered tests from actual executions, and narrow passes
from broad red runs. That distinction should survive the next resume without a
reader reconstructing logs. [O10950, E2]

**Recommendation:** emit receipts at execution time. Record structured argv, cwd,
source/input identity, tool versions, start/end, exit/signal, exact executed,
failed, skipped and filtered counts, setup errors, log/report hashes, and artifact
identities. Label evidence kinds: original incident, baseline reproduction,
candidate pass, causal guard mutant, model/inspection, and live acceptance.

A version command cannot satisfy a build step. A stale JSON report cannot satisfy
a new run. A failed setup or zero-test selector cannot become a pass. Receipt
validation can prove execution identity and result consistency; it cannot prove
that a test asserted the correct protocol property. That remains review work.

Use the existing preflight result schema as a parent ledger and attach per-run
receipts. Incremental reuse is allowed only when all relevant inputs still match.
Changing an ABI, dependency or shared fixture invalidates its dependent receipts;
passing one corrected file does not replace a required final full gate.

**Proof:** truncate a run, substitute an old report, edit sources mid-run, or
replace a build with a version probe. Receipt validation refuses the completion
claim and preserves the original failure.

### 14. Resuming a program requires reconstructing ownership and scope

Both chats began by locating Claude transcripts, matching worktrees, recovering
unpublished commits, reconciling reviews and enumerating tasks. The offchain root
later corrected its seven-package summary to include omitted obligations and
recovered 154 numbered tasks. An updated owner ruling also changed the deployment
scope. [O4930, O5319; initial root history]

**Recommendation:** keep a small durable program manifest with stable task IDs,
dependency edges, exact versus related issue links, source ownership, base and
candidate identity, author/review/integration state, accepted decisions, and
required receipts. Render human task lists and handoffs from it. States such as
implemented, reviewed, integrated, published and accepted stay separate.

A read-only workspace inventory derives branch ancestry, dirty/staged paths,
overlap, registered run resources and available receipts. A reviewed integration
packet contains base identity, allowlisted paths and before/after hashes and can
be previewed before application. Extend the safe-commit runner for the eventual
commit, rather than recreating Git index handling.

Store consequential decisions close to their domain: permissionless caller versus
reward recipient, single redeployment prerequisites, timing policy adoption and
retention semantics. Link superseded decisions explicitly. A skill can route to
the current decision; it should not freeze a temporary branch name or a chat's
historical issue status.

**Proof:** a fresh session identifies the same authoritative candidate, known
dirty work, pending reviews and required gates using the manifest and inventory.
Applying a packet with a changed base/file refuses before writing. Issue closure
remains an authorized external action with evidence, not a side effect of local
tests.

### 15. Maximum supported inputs and first-fault order need early engineering gates

One proposed signature-ordering correction expanded to roughly 102,000 trace
steps and over 3 GB of repeated witness data. Later transport work reduced an
observer payload from 79 MB to 3.24 MB. Other review found that a later malformed
witness could prevent proving an earlier oversized-output fault. These are
different dimensions of correctness and feasibility. [O3429, O6885, O8526]

**Recommendation:** define supported input envelopes and test them before
integrating a new proof route. Record maximum event/field sizes, signatures,
trace steps, unique retained bytes, encoded DA bytes, reference publications,
transaction bytes and execution units. Deduplicate immutable witness material
by authenticated identity; preserve trace coordinates and commitments rather
than dropping necessary evidence. Avoid deep object comparisons of huge fixtures
when a linear comparison of their exact serialized bytes proves the property.

Add a compact first-fault matrix across admission, evaluator, trace writer,
watcher classifier, dispute builder and validator: normal/forced origin, each
proof direction, honest/refused input, earlier/later competing faults, malformed
prefix/later child, and boundary-sized carriage. Generate cases from the existing
family/phase catalogue where possible, but assert the actual terminal refusal or
success rather than just a registered test title. A supported input that cannot
be represented by its proof workflow is an explicit gap, not a guessed verdict.

**Proof:** maximum supported fixtures complete within recorded storage and L1
limits; one step over a declared bound fails at the intended boundary. An earlier
fault remains provable despite later malformed material. Cross-language tests
agree on the exact reason and coordinate, and weakening the responsible guard
changes the result. Measurements remain local engineering evidence until the
relevant live acceptance runs.

## Deterministic command surface

The names below are **proposed interfaces**, not commands available today. A thin
repository CLI can route to current implementations; it should not duplicate
them. Development/test tooling belongs in repository scripts or node-tools;
actual operator commands belong in the operator CLI according to its existing
boundary rules.

| Proposed interface                            | Owner to extend                                | Stable inputs and outputs                                                        | Completion contract                                                                              |
| --------------------------------------------- | ---------------------------------------------- | -------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| `contrib prepare --package <name> --plan`     | Doctor plus local test environment             | Package/profile/capabilities → structured setup plan and artifact identities     | Explicit execution prepares only authorized local prerequisites; planning writes no state        |
| `contrib test --package <name> --file <path>` | Preflight runner plus package test support     | Typed selectors and invocation ID → log, report and receipt                      | Applies package environment/pretest, verifies inputs, requires executed tests and joins children |
| `contrib artifacts --check`                   | Existing artifact channels and stamps          | Input closure → stale-channel list or verification receipts                      | No rewriting in check mode; generation builds fresh prerequisites first                          |
| `contrib resources list`                      | Worktree identity and run receipts             | Host resource registry → owner, state and safe action description                | Observation does not signal processes or alter deployments                                       |
| `contrib workspace inspect`                   | Git/worktree inventory and safe commit tooling | Selected program/worktree roots → ancestry, dirty paths, overlap and evidence    | No merge, reset, prune, or branch mutation                                                       |
| `contrib packet verify`                       | Safe commit tooling and input hashes           | Base + path/hash manifest → applicable/refused result                            | Refuses ambiguous source ownership before integration                                            |
| `contrib receipts verify`                     | Preflight structured results                   | Run/source/artifact identities → fresh/stale/incomplete evidence                 | A reported pass requires consistent execution, counts and source identity                        |
| `node-tools deployment-fit`                   | Existing publication fit builders/verifier     | Real role bindings + blueprint + protocol profile → signed envelope measurements | Every enabled role and required route accounted for                                              |
| `node-tools recovery-scenarios`               | Shared workflow and chain fixtures             | Scenario matrix + seed + storage adapter → transition/resource trace             | Covers required restart/rollback/polarity cases, names synthetic evidence                        |
| `node-tools acceptance <existing options>`    | Existing e2e-stack/phase4 harness              | Owned run + frozen dist/deployment → journey, drill and payout receipts          | Actual progress, exact payouts and required drills against the final candidate                   |

Across these interfaces, use argument arrays and schema-validated configuration,
bounded output, meaningful exit states, and a plan mode. Configuration/receipt
output omits secrets. Unknown, unavailable and skipped are distinct from pass.
Never make reset/redeploy a hidden recovery action. Mutation commands can consume
a reviewed plan identity so a changed target cannot silently inherit approval.

## What belongs in a skill

A skill is valuable when it selects a workflow or explains a repository-specific
decision. Its executable steps should call the stable tools above. Arbitrary
shell assembly, process polling, source copying and manual result parsing should
leave the skill body once the corresponding tooling exists.

| Skill treatment                                                   | Reason and content                                                                                                                                                       |
| ----------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Extend **local-test-environment**                                 | Route focused runs through guarded preparation/test commands; keep the diagnosis explanation and exceptional prerequisites                                               |
| Extend **running-the-devnet** and **midgard-e2e-acceptance**      | Use resource/run identities, immutable dist and exact receipt checks; keep stack selection and relaunch/redeploy judgment                                                |
| Extend **regenerating-goldens-and-ledgers**                       | Keep artifact ownership and interpretation; execute registry-derived prerequisites/generation/checks                                                                     |
| Extend **writing-reports-and-prs**                                | Render the evidence ledger from receipts, then add interpretation and limits; do not infer acceptance from test counts                                                   |
| Extend **committing-safely**                                      | Consume verified integration packets and retain exact-path/index protections                                                                                             |
| Add **resuming-a-work-program**, after manifest tooling exists    | Locate the authoritative manifest, inspect overlaps/resources, resume pending evidence and review, preserve accepted decisions                                           |
| Add **diagnosing-runtime-progress**, after diagnostics exist      | Decide whether a problem is process death, dependency outage, valid safety hold, or stalled eligible work; reproduce through the real caller                             |
| Add **changing-a-cross-process-contract** if the pattern persists | Locate all producers/consumers, use shared boundary schemas, verify compiled round trips and failure behavior                                                            |
| Add **retiring-obsolete-orchestration** if repeated               | Map retained users and responsibilities, prove the shared replacement through required cases, delete unused runners/adapters without rebuilding undeployed compatibility |

Four conditional new skills are enough for this corpus. Do not add a separate
skill for every issue, transient branch or one-time fixture error. Existing
consensus-review and writing-tests guidance still owns adversarial reasoning and
the choice of meaningful tests. Keep always-loaded instructions narrow and link
to the tool's generated help/registry rather than duplicating its command syntax.

## Delivery order and acceptance

Effort labels are relative scope judgments, not estimates derived from measured
implementation time: S = one owner and bounded behavior; M = several consumers;
L = cross-service or protocol integration.

| Order | Deliverable                                                         | Effort | Why first / acceptance                                                                                                                            |
| ----- | ------------------------------------------------------------------- | ------ | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1     | Guarded focused-test runner and basic receipts                      | M      | Eliminates wrong runner/cwd/env/pretest/zero-test/report mistakes; demonstrate refusal controls and a real compiled-child suite                   |
| 2     | Complete dist/generator provenance                                  | M–L    | Catches the demonstrated old-SDK fixture problem; prove stale dependency/artifact rejection and a fresh generator run                             |
| 3     | Build/test resource leases and immutable consumer outputs           | M      | Prevents the observed fixture deletion; prove concurrent same-checkout runs and cancellation cleanup                                              |
| 4     | Executable full publication/route and supported-envelope gates      | L      | Detects initialization, cold-route and witness-amplification blockers early; bind signed measurements and first-fault cases to real enabled roles |
| 5     | Scoped DB fixtures and deterministic chain model                    | M–L    | Reduces re-triage of contamination and protocol-depth assumptions; prove isolation and shallow/deep/ancestry cases                                |
| 6     | Shared recovery matrix and lifecycle conformance fixtures           | L      | Catches multi-descendant, rotation, restart, funding and cancellation seams; validate production consumers                                        |
| 7     | Read-only workspace/program inventory and verified packets          | M      | Reduces resume/integration reconstruction; prove stale packet refusal and preservation of unrelated work                                          |
| 8     | Progress diagnostics and ordinary/optional policy matrix            | M      | Keeps ordinary operation usable and status accurate; verify pending-work failure and policy persistence                                           |
| 9     | Clean dependency/regeneration lane and wider gate registry coverage | M      | Makes checkpoints reproducible outside the author's machine; migrate manually required checks into preflight                                      |
| 10    | Thin skills, generated contribution guide and measured pilot        | S–M    | Teach the stable path after it exists; compare outcomes using the existing contribution benchmark design                                          |

Ship each increment through one real consumer before broadening it. For example,
start the guarded runner with the node's compiled-child tests, add the SDK
generator, then generalize to other packages. Start the resource manager with
build-output exclusion and test DB identity; Docker ownership can follow. Keep
the tooling program bounded so it does not delay the demonstrated reliability
fixes with another large framework project.

### Measurement

Use the existing [contribution benchmark design](../agent-contribution-benchmark.md)
and add replayable historical footgun tasks. Suitable tasks include a stale SDK
fixture, the compiled owner-prefix mismatch, a build colliding with a child test,
a missing canonical route binding, a two-descendant recovery, and a database
fixture that leaks a lease. Separate disclosed practice tasks from unseen holdouts.

Measure successful reviewable contributions, human corrections, setup/build
retries, invalid verification attempts, resource conflicts, time to the first
valid failing reproduction, and final acceptance gaps. Record model/tool budget,
machine contention and source identity. Report infrastructure failures separately
while retaining them in attempted-run totals. No success-rate, cost saving or
token saving was measured for this report.

## Evidence register

IDs refer to captured observations in EVIDENCE.json. Transcript statements about
tests and current work are historical reports, not independently rerun results.
Links below are local source records; their captured prefix hashes preserve the
snapshot basis even though the active files can grow.

<!-- doc-links:external -->

| ID                               | Evidence pointer                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             | What it supports                                                     |
| -------------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------- |
| R270 / R11213                    | [Owner mismatch](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:270), [incident scope](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:11213)                                                                                                                                                                                                                                                                                                   | Observed commitment stall and false readiness                        |
| R712                             | [Budget assessment](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:712)                                                                                                                                                                                                                                                                                                                                                                                                                                       | Role/path budgeting and retention of the original failed calculation |
| R1945 / R2143 / R2984            | [Depth semantics](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:1945), [provider ancestry](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:2143), [compensation retention](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:2984)                                                                                                                                                 | Common recovery fixture/model requirements                           |
| R3090 / R11691                   | [Stale-owner race](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:3090), [new-code distinction](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:11691)                                                                                                                                                                                                                                                                                          | Fencing/lifecycle controls; provenance limits                        |
| R11497                           | [Compiled readiness timeout](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:11497)                                                                                                                                                                                                                                                                                                                                                                                                                            | Need to exercise real consumers and complete readiness work          |
| R12176 / R14839                  | [Ordinary signing regression](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:12176), [row 513 regression](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:14839)                                                                                                                                                                                                                                                                                | Ordinary/optional configuration matrix                               |
| R12714                           | [Concurrent build removed fixture](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:12714)                                                                                                                                                                                                                                                                                                                                                                                                                      | Demonstrated build/test resource conflict                            |
| R13506 / R13718                  | [Shared fixture regression](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:13506), [profile limit](/home/gumbo/.codex/sessions/2026/10/01/rollout-2026-10-01T22-39-07-01a0fab1-bbf8-7ee0-af61-216cd8454bbb.jsonl:13718)                                                                                                                                                                                                                                                                                       | Narrow fixture repair and workload-specific measurements             |
| O1798 / O1896 / O5502            | [Dependency blocker](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:1798), [preserved source verification](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:1896), [checkpoint pin](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:5502)                                                                                                                                          | Clean dependency reproducibility                                     |
| O2618 / O3612                    | [Refusal fixture failed early](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:2618), [older SDK generated fixtures](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:3612)                                                                                                                                                                                                                                                                       | Setup versus refusal distinction and generator provenance            |
| O3968 / O7900 / O8856            | [Initialization blocker](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:3968), [wider publication inventory](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:7900), [529 constructed publications](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:8856)                                                                                                                          | Why full signed publication coverage is its own gate                 |
| O8526 / O9613                    | [Earlier-fault replay](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:8526), [cold route size/bindings](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:9613)                                                                                                                                                                                                                                                                                   | Boundary coverage beyond happy-path proof tests                      |
| O9637 / O10214 / O10321 / O10758 | [Owner cleanup instruction](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:9637), [multiple descendants](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:10214), [distinct rewards](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:10321), [rotation limit](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:10758) | Shared-engine migration coverage and remaining dynamic gap           |
| O4930 / O5319                    | [Recovered owner ruling](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:4930), [corrected program scope](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:5319)                                                                                                                                                                                                                                                                                  | Durable task and decision manifest                                   |
| O10950                           | [Compiler-version receipt error](/home/gumbo/.codex/sessions/2026/10/02/rollout-2026-10-02T00-44-48-01a0fb24-cd4c-7a12-88a8-ee933024922f.jsonl:10950)                                                                                                                                                                                                                                                                                                                                                                                                                        | Execution-time receipts rather than manually asserted build evidence |
| E1                               | [Independent provenance audit](/tmp/midgard-reliability-20261002/evidence/original-696-provenance-independent/AUDIT.md)                                                                                                                                                                                                                                                                                                                                                                                                                                                      | Historical classification of original issue evidence                 |
| E2                               | [Full-check result ledger](/tmp/midgard-reliability-20261002/evidence/final-root-verification-19d2fbb9/ROOT-RESULT-LEDGER.json)                                                                                                                                                                                                                                                                                                                                                                                                                                              | Historical 49/52 result and nonzero exit                             |

## Verification and limits of this report

This is a transcript and source review. No protocol runtime tests, Aiken/native
build, dependency install, golden regeneration, fault drill or live acceptance
was run for it. Documentation verification includes the docs-site build and its
declared core/Lucid build prerequisites. Historical test counts retain their
original limited scope. Candidate
work outside the inspected checkout may already implement parts of these
recommendations; inspect its exact source and evidence before scheduling duplicate
work. No pending issue or program is declared complete here.

The existing dirty .gitignore, earlier triage directory and untracked blueprint
are outside this change. All verification below ran from the repository root on
October 2, 2026 CDT. These are document/source checks, with no behavioral test
pass count implied.

| Command                                                                                                                                                                         | Result                                                                                                                                                                                                                                 |
| ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `node scripts/preflight.mjs --list --json --base HEAD`                                                                                                                          | Exit 0 after a sandbox Git subprocess refusal was retried outside the sandbox; eight checks planned, zero executed, no uncovered paths. Includes existing user-owned changes, so this is a planning receipt, not a full preflight pass |
| `timeout 45s node scripts/agents/check-doc-links.mjs`                                                                                                                           | Exit 0; 193 tracked documentation files checked. Earlier sandboxed attempt stopped with exit 130 after failing to return                                                                                                               |
| `node scripts/agents/check-enforcement-tags.mjs`                                                                                                                                | Exit 0; 33 instruction files clean                                                                                                                                                                                                     |
| `node scripts/agents/check-agent-config.mjs`                                                                                                                                    | Exit 0; two configuration files clean                                                                                                                                                                                                  |
| `node scripts/preflight.mjs --check-docs`                                                                                                                                       | Exit 0; generated required-check registry documentation is current                                                                                                                                                                     |
| `node docs-site/scripts/check-docs-links.mjs`                                                                                                                                   | Exit 0; 583 Markdown/MDX files checked                                                                                                                                                                                                 |
| `node /tmp/midgard-agent-workflow-review/validate-links.mjs`                                                                                                                    | Exit 0; one new Markdown file, zero unresolved repository references, including untracked report files supplied explicitly                                                                                                             |
| `python3 /tmp/midgard-agent-workflow-review/validate-report.py`                                                                                                                 | Exit 0; two transcript prefix hashes, 54 observation anchors, 33 absolute local links, owned-file whitespace and report conflict markers checked                                                                                       |
| `demo/node_modules/.bin/prettier --check docs/exec-plans/agent-workflow-improvements-2026-10-02/REPORT.md docs/exec-plans/agent-workflow-improvements-2026-10-02/EVIDENCE.json` | Exit 0; both report files formatted                                                                                                                                                                                                    |
| `pnpm --dir docs-site run build`                                                                                                                                                | Exit 0; optimized production build completed, including declared core/Lucid build prerequisites and docs link check                                                                                                                    |
| `pnpm --dir docs-site run types:check`                                                                                                                                          | Exit 0; MDX generation, route type generation and TypeScript check completed, including declared core/Lucid build prerequisites                                                                                                        |
| `git diff --name-only --diff-filter=U`                                                                                                                                          | Exit 0; no unmerged index paths; report marker validation separately checks the new prose                                                                                                                                              |
| `git diff --check`                                                                                                                                                              | Exit 2; pre-existing `.gitignore:37` blank line at EOF. Owned-file whitespace is checked explicitly because unstaged untracked report files are not covered by this command                                                            |

The merge-tree planning check was not executed: this report creates untracked
documents and performs no merge. The unmerged-index and report marker checks
cover its relevant conflict surface. The broader runtime preflight and live
acceptance gates were not run because no protocol implementation changed. Local
transcript links and temporary validation receipts are machine-specific;
EVIDENCE.json keeps their captured identity and excerpts without publishing raw
transcripts or credentials.
