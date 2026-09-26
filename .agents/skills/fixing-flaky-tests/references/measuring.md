# Measuring a failure rate

Commands checked against `Anastasia-Labs/midgard` on 2026-09-25. All of them
are read-only; never rerun, cancel or comment on a CI run from this skill.

## From CI history

Workflows and step names live in `.github/workflows/*.yml`. The test steps
are coarse: one step, such as `Test Midgard node` or `Build, typecheck, and
test fault-proof tooling`, runs a whole package.

List recent runs of a workflow:

```sh
gh run list -R Anastasia-Labs/midgard --workflow midgard-node-ci.yml -L 50 \
  --json databaseId,headSha,headBranch,conclusion,attempt,createdAt \
  --jq '.[] | [.databaseId, .headSha[0:9], .headBranch, .attempt, .conclusion, .createdAt] | @tsv'
```

A workflow's conclusion is not a step's. Get one step's outcome per run:

```sh
STEP='Test Midgard node'
for id in $(gh run list -R Anastasia-Labs/midgard --workflow midgard-node-ci.yml -L 30 --json databaseId --jq '.[].databaseId'); do
  gh run view "$id" -R Anastasia-Labs/midgard --json jobs,headSha,attempt \
    --jq ".headSha[0:9] as \$sha | .attempt as \$a | .jobs[].steps[] | select(.name == \"$STEP\") | \"$id \\(\$sha) attempt=\\(\$a) \\(.conclusion)\""
done
```

Read the output this way:

- **`skipped` means CI never looked.** Every step after the first failure in a
  job is skipped, so a run that failed on a documentation check says nothing
  about the node tests. Leave skipped runs out of the denominator.
- **The same SHA failing and then passing** (a second `attempt`, or a second
  run on the same `headSha`) is the strongest flake evidence CI gives: the
  code did not change.
- **Many consecutive failures across different SHAs** is a regression, not a
  flake. Find the first failing SHA and read what it changed.

Find the failing test in a run's log:

```sh
gh run view <run-id> -R Anastasia-Labs/midgard --log-failed | grep -nE ' FAIL |AssertionError|Error:' | head -40
```

A test that failed in CI but is not named in the log (a worker crash, a
wasm trap, a job timeout) is still a data point. Record it by symptom.

Write the rate down with its source: "`Test Midgard node`: 3 failures in 22
runs where the step ran, run ids ..., 2 of them on SHAs that later passed
unchanged".

## Locally

Use [scripts/rerun.mjs](../scripts/rerun.mjs) around the narrowest command
that still fails. Narrow the command first; the focused command for each test
level is in
[test-levels.md](../../writing-tests/references/test-levels.md).

```sh
node .agents/skills/fixing-flaky-tests/scripts/rerun.mjs --times 20 --timeout-s 900 -- \
  pnpm --dir demo/midgard-node exec vitest run tests/<file>.test.ts
```

Each vitest run of a `midgard-node` file pays global setup: Postgres shard
provisioning and a cargo build of the native owner binary
(`demo/midgard-node/tests/global-setup.ts`). Multiply that by N before you
pick N.

Escalate only until it reproduces, and record which level did it:

1. The one test, alone, N times.
2. The whole file, N times. A test that only fails after its neighbours
   shares state with them.
3. CI's settings: `CI=true`, which GitHub sets on every run. As of
   2026-09-25 no workflow sets a `MIDGARD_*_FORKS` variable, so CI uses each
   package's default fork count.
4. The whole package, so forks run other files at the same time.
5. Under load, with the load recorded (`uptime`, `nproc`).

rerun prints output only for failing runs. Before the loop, run the command
once by hand and read its collected count (`Tests N passed`): a narrowed
vitest filter that matches nothing exits 0 every time (see
[the writing-tests traps](../../writing-tests/SKILL.md#5-keep-it-able-to-fail)).

A failure that is red on every local run and green in CI is not a flake. It is
an environment difference; find the difference.
