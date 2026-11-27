# Failure classes in Midgard CI

Read this when a failing step's log does not obviously match a row of the
classification table in [SKILL.md](../SKILL.md), when you need the exact log
text a class produces, or when you want to count the classes over recent runs.

## Counting failures by class

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

Count each run once, by the log of its first failing step, and date the count.

`--log-failed` prints every step of the failed job, not only the failing step,
and labels each line `UNKNOWN STEP`. The failing step's output is the text
between the last `##[group]Run ` before the first `##[error]` and that
`##[error]` line. Search there, not the whole file: earlier steps print
passing test names such as "rejects the same commitment mismatch", which match
naive `mismatch|drift|error` greps.

## Aiken CI

**Script budget over ceiling.** Every execution cost is a ceiling in
`demo/midgard-core/src/script-budgets.json`, judged `measured <= ceiling` by
`onchain/aiken/scripts/script-budgets.mjs` in the shard-collector job's step
`Hold every script budget to its ceiling`. Signal:

```text
over         midgard/user_events/tx_order_v1.test/<test> - <control>  read 5340508 mem / 1580803513 cpu  ceiling 5340507 mem / 1580803513 cpu  above its ceiling on mem
A script budget failed. Shrink the script, or if the cost is accepted run `node onchain/aiken/scripts/script-budgets.mjs raise <name> --reason <why>`.
```

`missing` means a budgeted test or its control did not run (renamed or
deleted). `now-fits` means an `aboveBasis` entry's reading fits the basis, so
its infeasibility claim is stale. `out-of-band` means a growth claim's two
budgets moved apart. "If the cost is accepted" is the question the verdict must
answer, not an instruction.

**Formatter drift.** Step `Run normalized Aiken auto-formatter check` in
`.github/workflows/aiken-ci.yml`. The log is the output of
`git diff --name-only --exit-code -- '*.ak'`: a bare list of `.ak` paths, then
`##[error]Process completed with exit code 1.`

**Aiken test failure.** `aiken check` prints a JSON report; a failing test has
`"status": "fail"` and the top-level `"summary"` has `"failed": N`. The tail of
the log is usually a _passing_ module's traces, so read the summary and grep
for the failing titles:

```bash
grep -n -B8 '"status": "fail"' <id>.log | grep '"title"'
```

**Silent compile exit.** The step prints `Compiling …` lines for the
dependencies and then `##[error]Process completed with exit code 1.` with no
diagnostic. The cause is not in the log. Reproduce with the pinned fork at
that commit before naming one.

## Midgard Node CI

**Generated profile doc stale.** Step `Check canonical V1 profile
documentation`. Signal, from
`demo/midgard-core/scripts/sync-consensus-profile-doc-v1.mjs`:

```text
Error: docs/consensus-profile-v1.md is stale; run the profile documentation sync command
```

**Golden drift.** Any `Check … golden vectors` or `Check … Aiken constants`
step. Signal, from `goldenChannelEmitter` in
`demo/midgard-core/scripts/golden-channel.mjs`:

```text
Error: stale generated artifact: onchain/aiken/lib/midgard/native-tx-field-items-v1-golden.test.ak
```

The path names the artifact the generator would rewrite.

**Test assertion.** Vitest `AssertionError`, for example `expected
[ 'doubleSpend', …(27) ] to deeply equal [ 'doubleSpend', …(24) ]`: the list
grew by three entries and the pinned expectation did not. Whether the code or
the expectation is wrong is the verdict to earn.

**Missing blueprint.** A job that reads the blueprint without the artifact
from the `Testnet blueprint` job fails with:

```text
Error: ENOENT: no such file or directory, open '/home/runner/work/midgard/midgard/onchain/aiken/plutus.json'
```

## Lint and typecheck

**Lint.** ESLint prints `<line>:<col>  error  <message>  <rule>`, for example
`2:1  error  Run autofix to sort these imports!  simple-import-sort/imports`.

**Typecheck.** `error TS…` lines, for example
`error TS2307: Cannot find module '@al-ft/midgard-sdk'` when a workspace
dependency was not built first.

## Infrastructure

An infrastructure verdict needs direct evidence in the log: a service
container failure, a runner shutdown or lost-communication message, an
exit-137 kill or an out-of-disk message. Without one, it is not the
explanation for a red run.
