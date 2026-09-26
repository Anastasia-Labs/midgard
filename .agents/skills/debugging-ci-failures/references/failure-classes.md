# Failure classes seen in Midgard CI

Read this when a failing step's log does not obviously match a row of the
classification table in [SKILL.md](../SKILL.md), when you need the exact log
text a class produces, or when you want to re-count the classes.

All counts are as of 2026-09-26 and come from the last 60 failed runs of
`aiken-ci.yml` (2026-08-10 to 2026-09-04) and `midgard-node-ci.yml`
(2026-08-28 to 2026-09-22), and all 46 failed runs of `midgard-watcher-ci.yml`
(2026-08-31 to 2026-09-22; that workflow has never passed). Each run is
counted once, by the log of its first failing step. Every one of these runs
was a `pull_request` run from the checkpoint branch (PR #471) or its
predecessors. No run from any of the three workflows exists after 2026-09-22.

## How the counts were taken

Read-only, from the repository root of any clone:

```bash
REPO=Anastasia-Labs/midgard
for wf in aiken-ci.yml midgard-node-ci.yml midgard-watcher-ci.yml; do
  gh run list -R "$REPO" --workflow "$wf" --status failure --limit 60 \
    --json databaseId --jq '.[].databaseId' > "ids-$wf.txt"
done
# Failing job and step per run:
while read -r id; do
  gh run view "$id" -R "$REPO" --json jobs \
    --jq '.jobs[] | select(.conclusion == "failure") | .steps[] | select(.conclusion == "failure") | .name'
done < ids-midgard-node-ci.yml.txt | sort | uniq -c | sort -rn
# Log of the failing job only:
gh run view <id> -R "$REPO" --log-failed > <id>.log
```

`--log-failed` prints every step of the failed job, not only the failing step,
and labels each line `UNKNOWN STEP`. The failing step's output is the text
between the last `##[group]Run ` before the first `##[error]` and that
`##[error]` line. Search there, not the whole file: earlier steps print
passing test names such as "rejects the same commitment mismatch", which match
naive `mismatch|drift|error` greps.

## aiken-ci.yml (60 failed runs)

| Class               | Runs | Dates                    | Example run   |
| ------------------- | ---: | ------------------------ | ------------- |
| Exec-ledger drift   |   33 | 2026-08-23 to 2026-09-04 | `33875522276` |
| Formatter drift     |   17 | 2026-08-12 to 2026-08-16 | `31948084630` |
| Aiken test failure  |    7 | 2026-08-10 to 2026-09-01 | `33542590564` |
| Silent compile exit |    3 | 2026-08-17 to 2026-08-31 | `33393576516` |

Over the last 200 completed runs (2026-08-04 to 2026-09-04), 74 failed and 126
passed. The last green run was 2026-08-22.

**Exec-ledger drift.** Steps named `Pin the … execution ledger`. The Q1x
ledger failed 20 times, Q21 7 and §8.10 6. Signal:

```text
midgard/native-tx-carriage-v1.test: 'tier3_corner_open_only' drifted — ledger mem=640875 cpu=299825903, measured mem=642673 cpu=300391006
§8.10 carriage execution ledger: 9 drift(s). If the re-take is legitimate, re-run with --update and move docs/spec/midgard-tx.md §8.10 in the same commit.
```

Q21 produced a different shape, which is not a re-takeable number:

```text
... 'accepts_valid_l2_transaction_no_op_transition_fault' is recorded 'within' but measured mem=N cpu=N exceeds the basis mem=N cpu=N
Q21 transition-trace execution ledger: 1 structural failure(s). These are not re-takeable numbers; resolve them in the source or in the ledger.
```

The messages come from `onchain/aiken/scripts/verify-*-exec-ledger-v1.mjs` and
`onchain/aiken/scripts/exec-ledger-within-basis-v1.mjs:275`. "If the re-take is
legitimate" is the question the verdict must answer, not an instruction.

**Formatter drift.** Step `Run normalized Aiken auto-formatter check`
(`.github/workflows/aiken-ci.yml:177`). The log is the output of
`git diff --name-only --exit-code -- '*.ak'`: a bare list of `.ak` paths, then
`##[error]Process completed with exit code 1.` No run has failed here since
2026-08-16, but no aiken-ci run exists after 2026-09-04 either. Reproducing
the step on a scratch copy of `cfa726174`'s 1,193 tracked `.ak` files with
`aiken v1.1.23+5adf783` finds 15 files that differ (as of 2026-09-26), among
them `onchain/aiken/validators/state-queue.ak` and
`onchain/aiken/lib/midgard/state-queue.ak`. The step would be red, and it runs
before `aiken check` and every ledger pin.

**Aiken test failure.** `aiken check` prints a JSON report; a failing test has
`"status": "fail"` and the top-level `"summary"` has `"failed": N`. The tail of
the log is usually a _passing_ module's traces, so read the summary and grep
for the failing titles:

```bash
grep -n -B8 '"status": "fail"' <id>.log | grep '"title"'
```

**Silent compile exit.** The step prints `Compiling …` lines for the
dependencies and then `##[error]Process completed with exit code 1.` with no
diagnostic. In run pair `33393576516` (aiken-ci) and `33393576640` (node CI),
the same commit `1efa9157f` failed this way in both workflows, so it is
deterministic on that commit. The cause is not in the log. Reproduce with the
pinned fork at that commit before naming one.

## midgard-node-ci.yml (60 failed runs)

| Class                            | Runs | Dates                    | Example run   |
| -------------------------------- | ---: | ------------------------ | ------------- |
| Generated profile doc stale      |   31 | 2026-09-12 to 2026-09-22 | `35676544721` |
| Golden drift                     |   12 | 2026-09-01 to 2026-09-12 | `34704951462` |
| Evidence-stamp drift (gate gone) |    9 | 2026-08-28 to 2026-08-29 | `33224722495` |
| Test assertion                   |    6 | 2026-08-29 to 2026-09-03 | `33542590514` |
| Silent compile exit (blueprint)  |    2 | 2026-08-29 to 2026-08-31 | `33393576640` |

Over the last 200 completed runs (2026-08-05 to 2026-09-22), 198 failed. The
only two green runs were both on 2026-08-21.

**Generated profile doc stale.** Step `Check canonical V1 profile
documentation` (`.github/workflows/midgard-node-ci.yml:237`), which runs
`pnpm --dir demo/midgard-core run docs:consensus-profile-v1:check`. Signal,
from `demo/midgard-core/scripts/sync-consensus-profile-doc-v1.mjs:45`:

```text
Error: docs/consensus-profile-v1.md is stale; run the profile documentation sync command
```

It was the first red step of every node CI run from 2026-09-12 to the last
run on 2026-09-22, so the 28 steps after it measured nothing for ten days.

**Golden drift.** Any `Check … golden vectors` or `Check … Aiken constants`
step. Signal, from `demo/midgard-core/scripts/golden-channel.mjs:220`:

```text
Error: stale generated artifact: onchain/aiken/lib/midgard/native-tx-field-items-v1-golden.test.ak
```

The path names the artifact the generator would rewrite.

**Evidence-stamp drift.** `a declared input changed (or the stamp/artifact
moved) since the last regeneration` from the resolver proof-fit sweep test.
The message no longer exists anywhere under `demo/` at `cfa726174`; this class
is historical.

**Test assertion.** Vitest `AssertionError` in the SDK and core suites, for
example `expected [ 'doubleSpend', …(27) ] to deeply equal [ 'doubleSpend',
…(24) ]` (runs of 2026-08-29): the list grew by three entries and the pinned
expectation did not. Whether the code or the expectation is wrong is the verdict to earn.

**Postgres service.** The `postgres:16-alpine` service
(`.github/workflows/midgard-node-ci.yml:91-93`) reported healthy in all 60 runs.

## midgard-watcher-ci.yml (46 failed runs, all of them)

| Class             | Runs | Dates                    | Example run   |
| ----------------- | ---: | ------------------------ | ------------- |
| Missing blueprint |   30 | 2026-09-10 to 2026-09-22 | `35676544697` |
| Lint              |    7 | 2026-09-13               | `34750734415` |
| Typecheck         |    5 | 2026-08-31 to 2026-09-10 | `34437739272` |
| Test assertion    |    4 | 2026-09-03 to 2026-09-04 | `33875522271` |

**Missing blueprint.** The watcher TypeScript suite ran in this workflow,
which never built the Aiken blueprint:

```text
Error: ENOENT: no such file or directory, open '/home/runner/work/midgard/midgard/onchain/aiken/plutus.json'
```

`aa68fdeeb` (2026-09-24) removed the suite from this workflow; node CI runs it
after its `Build testnet Aiken blueprint` step. No CI run has confirmed the fix,
because none has run since.

**Lint.** ESLint prints `<line>:<col>  error  <message>  <rule>`, for example
`2:1  error  Run autofix to sort these imports!  simple-import-sort/imports`.

**Typecheck.** `error TS2307: Cannot find module '@al-ft/midgard-sdk'` in runs
of 2026-08-31 to 2026-09-01, and a missing test-support module on 2026-09-10.

## Infrastructure

Across all 166 runs above: zero service-container failures, zero runner
shutdowns or lost-communication messages, zero exit-137 kills, zero
out-of-disk messages. An infrastructure verdict in this repository therefore
needs direct evidence in the log; it is not the likely explanation for a red
run.
