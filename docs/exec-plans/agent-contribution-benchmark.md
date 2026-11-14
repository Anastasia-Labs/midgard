# Agent contribution benchmark

Status: experiment design, 2026-09-28. No contribution success rate has been
measured. Run the baseline after the DA integration has a stable checkpoint;
use this design to compare repository changes at a fixed model and tool budget.
The earlier qualitative engineering assessment is not a benchmark result.

## What to measure

The primary outcome is a correct, reviewable contribution without human repair.
A successful run satisfies every task-specific acceptance criterion, preserves
unrelated work, runs the required verification, and reports failures and skips
accurately. A plausible patch or a large test count is insufficient.

Record separately: completion rate, human correction count and minutes, elapsed
minutes, input/output tokens, tool calls, cost at the recorded price, changed
lines, and tests actually executed. Report infrastructure failures separately
from contributor failures, with both in the denominator of attempted runs.
For review tasks, score exact findings and false positives separately; topic
similarity is not a finding. This first pack evaluates contributions, not
protocol vulnerability discovery.

## Frozen pilot tasks

All pilot tasks start at commit
`c240d955a` (resolve and record its full SHA before each run). This commit exists
locally and predates the changes being assessed. Give the contributor only the
prompt and the repository at that commit. Keep this rubric and solution commits
in the evaluator's checkout. The tasks are public practice cases: agents that
have seen this work must not count as blind trials.

| ID             | Prompt supplied to the contributor                                                                                                                                                                                           | Acceptance oracle held by evaluator                                                                                                                                                                                                                                                                           |
| -------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| selection      | “Make preflight select the existing docs-site link/build/type checks, specification build, and phase4 devnet asset tests for changes that affect them. Keep required-checks documentation derived from the registry.”        | Assert positive selection for each actual input path and negative selection for unrelated paths; inspect commands against package scripts/Makefile; verify prerequisite ordering; absent dependencies must not yield a pass; generating docs with or without an untracked blueprint produces identical bytes. |
| diagnosis      | “Extend doctor to diagnose missing docs-site dependencies separately from the demo dependencies, and report whether the specification build's Nix prerequisite is available. Diagnostics must give an actionable next step.” | Separate installed/missing/error fixtures for both workspaces and Nix. A denied or failed probe is unknown, not available or missing. Fixture tests must run without installing those tools. Check final diagnostic exit status as well as printed text.                                                      |
| split          | “Split the independent stress-wallet command module into cohesive modules while preserving its behavior and public API. Follow the repository's split skill and verify every consumer.”                                      | Run the pure-move verifier against the frozen commit; compare public exports; resolve the package subpath; typecheck, lint, package tests, and CLI build. Inspect relative imports and evaluation order independently. Baseline lint sites must move without adding or dropping exceptions.                   |
| bounded-change | “Correct the stale statement that Node CI is one serial job in the CI debugging skill. Validate the replacement against the workflows and report what was checked.”                                                          | The description matches the jobs and gate at the frozen commit; no copied count without a date or derivation; the patch stays within that documentation concern. Skill/link checks pass. An unrelated tracked sentinel edit and staged sentinel, inserted by the evaluator, are unchanged.                    |

The evaluator prepares the sentinel fixture in a disposable checkout, records
its contents and index blob IDs, and gives no instruction to change it. Keep
sentinels outside the task's files. Capture the starting dirty state in the
run record so accidental edits can be attributed.

## Run protocol

1. Freeze the task commit, task prompt, evaluator checks and environment image
   before collecting results. Record the full SHA, lockfile digests, Node/pnpm/
   Aiken versions, available services and dependency-cache state. For the split
   task, compile a fresh stamped blueprint and record its profile; use isolated
   test database prefixes and no live deployment credentials.
2. Establish the untouched task baseline: run the oracle on the start commit.
   A functional task must expose the requested gap; a refactor must begin with
   its behavior tests passing. Save every existing failure by test name and
   message. A changed pre-existing failure requires investigation. An unusable
   baseline is an infrastructure-blocked attempt, never a contributor failure
   or a silently excluded trial.
3. Compare the control with the proposed agent-support changes at the same
   frozen product code. Record the exact support patch in each arm; exclude a
   task's solution from its support patch. For example, the preflight solution
   cannot be present in the treatment arm for the selection task. Instruction
   changes may be compared independently from tooling changes.
4. Use a fresh context and checkout per run. Fix model version, reasoning
   setting, enabled tools, network policy, 45-minute wall limit and 60,000-token
   limit in both arms. Record unavailable token accounting as null. Randomize
   arm order; start with five repetitions per task per arm (40 attempts).
   This is a pilot budget, not a statistical guarantee.
5. Save the final diff, command transcript, environment record and contributor
   report outside the evaluated checkout. Run the evaluator checks after the
   contributor stops. A reviewer who does not know the arm checks behavior,
   API preservation, scope and evidence. Disagreements receive a second human
   review, with both initial judgments retained.
6. Report successes/attempts by task and arm, uncertainty intervals for rates,
   median and range for time/cost, and every timeout, skip and infrastructure
   failure. A small pilot does not establish superiority over other repos.
   Repeat on unpublished tasks before using the result to set a release gate.

Complete when each attempted run has an auditable record and every aggregate
can be recalculated from those records. A run reaching its time/token limit is
incomplete; the budget cannot be converted into success.

## Run record

Use one JSON object per attempt in an evaluator-owned JSONL file. These fields
are required; null denotes an unavailable measurement, not zero:

```json
{
  "task": "split",
  "arm": "control",
  "repetition": 1,
  "startCommit": "FULL_SHA",
  "supportPatchSha256": "SHA256_OR_NULL",
  "promptSha256": "SHA256",
  "environmentRecord": "environment.json",
  "model": "EXACT_MODEL_VERSION",
  "reasoning": "RECORDED_SETTING",
  "status": "success|failed|incomplete|infrastructure-blocked",
  "acceptance": {
    "behavior": false,
    "scope": false,
    "verification": false,
    "report": false
  },
  "elapsedSeconds": null,
  "inputTokens": null,
  "outputTokens": null,
  "toolCalls": null,
  "cost": null,
  "humanCorrectionCount": 0,
  "humanCorrectionMinutes": 0,
  "tests": { "passed": 0, "failed": 0, "skipped": 0 },
  "artifacts": {
    "patch": "final.patch",
    "transcript": "commands.jsonl",
    "oracle": "oracle.json",
    "review": "review.json"
  }
}
```

Freeze a separate holdout set after the pilot checks are debugged. It should
include an unfamiliar package change, a failing environment diagnosis, a
cross-package API change and a bounded review. Commit rubric hashes before
runs; keep task solutions and rubric text outside contributor access. Do not
reuse a task for headline results after its solution has entered training or
session context. Past-incident reviewer evaluation (W6.4) remains a separate
experiment; completing this design does not complete W6.4.
