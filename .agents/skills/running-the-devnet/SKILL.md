---
name: running-the-devnet
description: Launch, relaunch, redeploy, and wait on a local Midgard devnet (the isolated phase4 Cardano chain, the watcher-journey wrapper, or the demo-node Docker stack), decide whether a change leaves a running deployment stale, and read a service crash. Use when starting or restarting the devnet or a node/DA committee process, when asking "is it ready?", when `/healthz`, `/readyz`, `/metrics`, Kupo `/health`, a readiness timeout or restart loop comes up, when a consensus-profile field, the blueprint, or a deployment profile changed under a running deployment, or when a devnet service exited.
---

# Running the devnet

A local devnet is **three stacks**, and the first job is naming which one you
mean. Live Preprod acceptance is a different job: use
[midgard-e2e-acceptance](../midgard-e2e-acceptance/SKILL.md) for it, and do
not repeat its runbook here.

| Stack                  | Chain                                   | Entry point                                                             |
| ---------------------- | --------------------------------------- | ----------------------------------------------------------------------- |
| Phase4 isolated devnet | Private `Custom` chain, one pool        | `demo/midgard-node-tools/devnet/phase4-process/scripts/*.sh`            |
| Watcher-journey devnet | Same generator, host paths for watcher  | `demo/midgard-node-tools/devnet/watcher-journeys/scripts/generate.sh`   |
| Demo-node Docker stack | Follows `NETWORK` (Preprod) via Mithril | `demo/midgard-node/docker-compose.yaml` + `docker-compose.kupmios.yaml` |

Commands, relaunch and reset for each are in
[references/stacks.md](references/stacks.md). Read it before you start, stop
or reset anything.

As of 2026-09-25 other sessions and worktrees on this machine run their own
devnets. A devnet you did not start is not yours to stop, restart, reset or
probe.

## Before touching a running devnet

1. **Find out whose it is.** Phase4 compose projects are
   `midgard_phase4_process_<run id>` and their ports come from the run's
   `run.env` [script: `phase4-process/scripts/common.sh` `require_run_dir`].
   Work only on a run directory you generated. `[review]`
2. **Pick your own ports and run id.** On this branch the generator defaults
   every run to Ogmios 2337, Kupo 2442 and Postgres 5544, and derives the
   project name from the run directory's basename alone
   [script: `phase4-process/scripts/generate.sh`]. Two worktrees with default
   settings collide. Set `MIDGARD_PHASE4_RUN_ID` and all three
   `MIDGARD_PHASE4_*_PORT` variables explicitly. `[review]`
3. **The demo-node stack binds host port 5433 for Postgres**
   (`demo/midgard-node/docker-compose.yaml`), the same port as the test
   Postgres from `scripts/start-test-postgres.sh`. Only one of them can run. `[review]`
4. **Wiping local state means a full on-chain redeploy.** Never delete a
   Postgres volume, `db/`, or part of a run directory while keeping the chain
   state it belongs to. Follow [state-reset](../../../docs/agents/state-reset.md).
   `[review]`: nothing blocks `docker compose down -v`.

## Stale or not: relaunch versus redeploy

A **relaunch** rebuilds and restarts processes against the same on-chain
deployment. A **redeploy** creates a new deployment (new genesis or `init`, new
manifest, clean local state). Decide which one a change needs before you
restart anything:

| What changed                                                                                                                             | Needs                                                         | What catches it                                                                                                                                                                                                                                                                                                                                                                                    |
| ---------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Any field of `MIDGARD_CONSENSUS_PROFILE` in `demo/midgard-core/src/consensus-profile.ts` (for example `forcedTransactionSourceEncoding`) | Redeploy                                                      | [runtime: `parseDeploymentManifest`] every manifest records `consensusProfileDigest`; a changed profile makes every existing manifest fail with "consensusProfileDigest must exactly match canonical V1" and the node refuses it as a contract source                                                                                                                                              |
| A deployment profile in `config/deployments/*.yaml` (timings, economics, confirmation depth)                                             | Rebuild with `deployment:build`, then redeploy                | [runtime: `verifyDeploymentProfileBinding`] manifests carry `deploymentProfileDigest`; [runtime: `requireSelectedDeploymentProfile`] `MIDGARD_DEPLOYMENT_PROFILE` must name the compiled profile; [ci: Aiken CI/Check deployment profiles] catches stale generated files                                                                                                                           |
| Any validator source, or any `aiken build`                                                                                               | Rebuild with `deployment:build`, then redeploy to exercise it | [runtime: `verifyBlueprintDeploymentProfile`] the node refuses a `plutus.json` whose hash differs from `plutus.json.deployment.json`. **Blind spot:** once the build record matches, nothing at `listen` compares the manifest's `artifacts.blueprintHash` with the blueprint on disk; the node keeps running the scripts the manifest recorded, so a changed validator is silently not under test |
| Node, DA committee or watcher TypeScript only                                                                                            | Relaunch                                                      | `[review]`                                                                                                                                                                                                                                                                                                                                                                                         |
| A new database migration                                                                                                                 | Relaunch after `db:migrate`                                   | [runtime: `MigrationRunner.assertCompatible`] `listen` refuses any schema that is not exactly the expected version                                                                                                                                                                                                                                                                                 |
| Anything in a phase4 run's source, `dist`, images or config                                                                              | A new phase4 run                                              | [script: `phase4-process/scripts/reset.sh`] reset refuses node and tools source or `dist`, image, configuration and snapshot drift before touching the run. **Blind spot:** it never rehashes the blueprint, so a validator-only change passes reset `[review]`                                                                                                                                    |

The detail behind each row, with file references, is in
[references/staleness.md](references/staleness.md). Read it when a change is
not obviously one row, or when a node refuses to attach.

## Readiness has four bars

"It's up" can mean four different things. Say which bar you checked.

1. **Process up**: the container is running or the PID is alive. Proves
   nothing about the service.
2. **Live**: `/healthz` answers. The node returns 200 `{"status":"ok"}` as
   soon as its HTTP server runs, the DA committee 200 `{"ok":true}`. Neither
   looks at the chain or the database.
3. **Ready**: `/readyz` answers 200 with `"ready": true`; otherwise 503 with
   `reasons`. The node's check covers workers, admission backlog, database, L1
   provider, history owner and MPF owner. Kupo's `/health` is 200 only once it
   has caught up; it answers 202 while replaying.
4. **Working**: ready, and the thing you care about is advancing. Blocks
   committing, DA members attesting and merges landing do not appear in
   `/readyz`. Watch the node's `/pipeline-status` and the DA committee's
   `/readyz` `counts` (`signatures`, `submittedOrConfirmedL1Attestations`).

Ports, paths, response fields and the metrics endpoint are in
[references/endpoints-and-logs.md](references/endpoints-and-logs.md). Read it
when you need a URL.

### Waiting: `devnet-wait`

```bash
node .agents/skills/running-the-devnet/scripts/devnet-wait.mjs \
  --url http://127.0.0.1:3000/readyz --url http://127.0.0.1:8787/readyz \
  --timeout 300 --log /path/to/node.log --pid "$NODE_PID"
```

It polls every `--url` until all are ready, and exits with:

| Exit | Meaning                                                                                                                                                   |
| ---- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 0    | Ready: every URL answered HTTP 200 and no body said `ready: false` or a disconnected Kupo                                                                 |
| 1    | Crashed: a `--pid` is gone, a `--log` gained a known fatal line, or a DA committee reported a quarantined L1 source. The tail of every `--log` is printed |
| 2    | Timed out: every URL answered, not all became ready. The last reasons are printed                                                                         |
| 3    | Unreachable: a URL never answered, no `--url` was given, or the arguments were unusable. Never a pass                                                     |

Enforced by [script: `scripts/devnet-wait.mjs`] and its tests
[ci: Agent Skills CI/Test scripts shipped inside skills]. Blind spots, stated
so you do not over-read a result:

- It sends no credentials. The watcher's `/v1/status` needs a bearer and
  answers 401 without one, so it reads as never ready. The watcher's compose
  healthchecks accept 401 too, so they prove bar 2 only; watcher readiness
  needs the operations bearer.
- Log scanning starts at each file's size when the wait starts, and matches
  only the fatal lines listed in the script (`DEFAULT_FATAL_PATTERNS`), plus
  any `--fatal-pattern`. A `listen` that dies on another error leaves no
  distinctive line; pass `--pid` to catch it.
- A URL pointed at `/healthz` reaches bar 2 only.

When you start the process yourself, `e2e-start-service` in
`midgard-node-tools` already writes a PID file and raw log and waits on a
ready URL. Use `devnet-wait` for services that are already running or that
something else started.

## Reading a crash

1. Find the log. Where each service writes is listed in
   [references/endpoints-and-logs.md](references/endpoints-and-logs.md#logs).
2. **Copy it out first.** The demo-node container keeps one 1 MB json-file
   log (`max-size: "1m"`, `max-file: "1"`), and `restart: always` restarts it
   into the same file, so a crash loop overwrites the first failure within
   minutes. `[review]`
3. Read the first fatal line, not the last. The node logs startup refusals as
   `Startup protocol initialization failed: …` with the mismatch list;
   `Database schema is not compatible: …` means run `db:migrate`; a manifest
   refusal says `cannot be used as contract source`. A DA committee that lost
   its L1 view writes `{"event":"l1_view_unavailable_exit",…}` and exits 70,
   which its supervisor restarts by design.
4. Decide relaunch or redeploy from the table above before restarting.

## Pending change

Commit `86d66c6f8` ("Give each worktree's phase4 devnet its own project and
ports") is not on this branch as of 2026-09-25. When it lands, the phase4
generator gives each linked worktree its own project name and port offset and
prints them; rule 2 above then becomes a default rather than a manual step.
