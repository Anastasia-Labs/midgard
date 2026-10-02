import { spawn } from "node:child_process";
import { getEventListeners } from "node:events";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { needsFreshAuthPolicy } from "../src/devnet-stack/deploy.js";
import {
  type Identities,
  LIBP2P_IDENTITIES,
  WALLET_ROLES,
} from "../src/devnet-stack/identities.js";
import {
  addValues,
  type DepositRecord,
  drained,
  expectedHoldings,
  sameValue,
  type TransferRecord,
  type WithdrawalRecord,
} from "../src/devnet-stack/journey.js";
import { makeLayout, type RunEnv } from "../src/devnet-stack/layout.js";
import {
  acquireLock,
  lockOwner,
  processStartTime,
} from "../src/devnet-stack/lock.js";
import { nodeEnvironment } from "../src/devnet-stack/node-env.js";
import { specsDigest } from "../src/devnet-stack/services.js";
import {
  insideStartGrace,
  runningSupervisor,
  supervisorRuns,
} from "../src/devnet-stack/stack.js";
import {
  SERVICE_MARKER_ENV,
  type ServiceSpec,
  sleep,
  superviseServices,
  type SupervisorPaths,
  type SupervisorPolicy,
  sweepOrphans,
} from "../src/devnet-stack/supervisor.js";

const FAST: SupervisorPolicy = {
  startGraceMs: 2_000,
  probeIntervalMs: 100,
  probeTimeoutMs: 200,
  hangMs: 500,
  stopGraceMs: 1_000,
  initialBackoffMs: 50,
  maxBackoffMs: 200,
  stableMs: 60_000,
  prestartRetryMs: 50,
};

const dirs: string[] = [];
const scratch = () => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-stack-"));
  dirs.push(dir);
  return dir;
};
afterEach(() => {
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const pathsIn = (dir: string): SupervisorPaths => ({
  runDir: dir,
  pidDir: join(dir, "services"),
  events: join(dir, "events.ndjson"),
  serviceLog: (name) => join(dir, `${name}.log`),
});

const events = (paths: SupervisorPaths) =>
  existsSync(paths.events)
    ? readFileSync(paths.events, "utf8")
        .trim()
        .split("\n")
        .map((line) => JSON.parse(line) as Record<string, unknown>)
    : [];

const waitFor = async (
  what: string,
  check: () => boolean,
  timeoutMs = 10_000,
) => {
  const deadline = Date.now() + timeoutMs;
  while (!check()) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
};

const alive = (pid: number) => {
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
};

const stackLayout = () => {
  const layout = makeLayout(scratch());
  mkdirSync(layout.state, { recursive: true });
  return layout;
};

const nodeService = (
  name: string,
  script: string,
  extra: Partial<ServiceSpec> = {},
): ServiceSpec => ({
  name,
  command: process.execPath,
  args: ["-e", script],
  cwd: process.cwd(),
  env: {},
  ...extra,
});

describe("needsFreshAuthPolicy", () => {
  const at = (expiresAtUnixTime: number) => ({
    identity: {
      referenceScriptAuthPolicy: { nativeScript: { expiresAtUnixTime } },
    },
  });

  it("keeps a policy with more than the publication minimum left", () => {
    expect(needsFreshAuthPolicy(at(10_000_000), 0)).toBe(false);
  });

  it("replaces a policy inside the publication minimum or already expired", () => {
    expect(needsFreshAuthPolicy(at(5_000_000), 0)).toBe(true);
    expect(needsFreshAuthPolicy(at(1_000), 2_000)).toBe(true);
  });

  it("never asks for a replacement before a policy exists", () => {
    expect(needsFreshAuthPolicy(undefined, 0)).toBe(false);
    expect(needsFreshAuthPolicy({}, 0)).toBe(false);
  });
});

describe("superviseServices", () => {
  it("restarts a service that exits and stops it on abort", async () => {
    const dir = scratch();
    const paths = pathsIn(dir);
    const abort = new AbortController();
    const running = superviseServices(
      [nodeService("flaky", "setTimeout(() => process.exit(3), 100)")],
      paths,
      abort.signal,
      FAST,
    );
    await waitFor(
      "three starts",
      () => events(paths).filter((e) => e.event === "start").length >= 3,
    );
    abort.abort();
    await running;
    const log = events(paths);
    expect(
      log.filter((e) => e.event === "exit").some((e) => e.code === 3),
    ).toBe(true);
    expect(log.some((e) => e.event === "restart-scheduled")).toBe(true);
    expect(log.at(-1)?.event).toBe("supervisor-stop");
    expect(existsSync(join(paths.pidDir, "flaky.json"))).toBe(false);
  });

  it("restarts a service that stops answering its liveness URL", async () => {
    const dir = scratch();
    const paths = pathsIn(dir);
    const port = 20_000 + Math.floor(Math.random() * 20_000);
    // Answers twice, then hangs every request while staying alive.
    const script = `let n = 0; require("node:http").createServer((q, s) => { if (++n <= 2) s.end("ok"); }).listen(${port}, "127.0.0.1");`;
    const abort = new AbortController();
    const running = superviseServices(
      [
        nodeService("hangs", script, {
          healthUrl: `http://127.0.0.1:${port}/healthz`,
        }),
      ],
      paths,
      abort.signal,
      FAST,
    );
    await waitFor("a hang to be detected", () =>
      events(paths).some((e) => e.event === "hung"),
    );
    await waitFor(
      "a restart",
      () => events(paths).filter((e) => e.event === "start").length >= 2,
    );
    abort.abort();
    await running;
  });

  it("starts a service only once its prestart check passes", async () => {
    const dir = scratch();
    const paths = pathsIn(dir);
    let checks = 0;
    const abort = new AbortController();
    const running = superviseServices(
      [
        nodeService("gated", "setInterval(() => {}, 1000)", {
          prestart: async () => (checks += 1) >= 3,
        }),
      ],
      paths,
      abort.signal,
      FAST,
    );
    await waitFor("a start", () =>
      events(paths).some((e) => e.event === "start"),
    );
    expect(checks).toBe(3);
    abort.abort();
    await running;
  });
});

describe("sweepOrphans", () => {
  it("stops a previous supervisor's child and leaves unmarked processes alone", async () => {
    const dir = scratch();
    const paths = pathsIn(dir);
    const spawnIdle = (env: Record<string, string>) => {
      const child = spawn(
        process.execPath,
        ["-e", "setInterval(() => {}, 1000)"],
        {
          env: { ...process.env, ...env },
          detached: true,
          stdio: "ignore",
        },
      );
      child.unref();
      return child.pid!;
    };
    const orphan = spawnIdle({ [SERVICE_MARKER_ENV]: `${dir}#orphan` });
    // A recycled PID: the file names it, but the process is not ours.
    const stranger = spawnIdle({});
    try {
      await import("node:fs").then(({ mkdirSync }) =>
        mkdirSync(paths.pidDir, { recursive: true }),
      );
      writeFileSync(
        join(paths.pidDir, "orphan.json"),
        JSON.stringify({ pid: orphan }),
      );
      writeFileSync(
        join(paths.pidDir, "stranger.json"),
        JSON.stringify({ pid: stranger }),
      );
      await waitFor("children to start", () => {
        try {
          return readFileSync(`/proc/${orphan}/environ`, "utf8").includes(
            SERVICE_MARKER_ENV,
          );
        } catch {
          return false;
        }
      });
      const recorded: Record<string, unknown>[] = [];
      await sweepOrphans(paths, FAST, (event) => recorded.push(event));
      await waitFor("the orphan to stop", () => !alive(orphan));
      expect(alive(stranger)).toBe(true);
      expect(recorded).toEqual([
        { event: "orphan-stop", service: "orphan", pid: orphan },
      ]);
      expect(existsSync(join(paths.pidDir, "orphan.json"))).toBe(false);
      expect(existsSync(join(paths.pidDir, "stranger.json"))).toBe(false);
    } finally {
      for (const pid of [orphan, stranger])
        try {
          process.kill(pid, "SIGKILL");
        } catch {
          // Already gone.
        }
    }
  });
});

describe("journey accounting", () => {
  const unit = `${"ab".repeat(28)}74414c504841`;
  const ada = (n: bigint) => (n * 1_000_000n).toString();

  it("derives exact token holdings and a bounded lovelace window", () => {
    const deposits: DepositRecord[] = [
      {
        user: "userA",
        value: { lovelace: ada(100n), [unit]: "50" },
        txHash: "d1",
        eventId: "e1",
      },
      {
        user: "userB",
        value: { lovelace: ada(10n) },
        txHash: "d2",
        eventId: "e2",
      },
    ];
    const transfers: TransferRecord[] = [
      {
        from: "userA",
        to: "userB",
        value: { lovelace: ada(5n), [unit]: "20" },
        txId: "t1",
        selectedInputs: [],
      },
    ];
    const withdrawals: WithdrawalRecord[] = [
      {
        user: "userB",
        l2OutRef: "t1#0",
        l1Address: "addr",
        txHash: "w1",
        withdrawalEventId: "we1",
        l2Value: { lovelace: ada(5n), [unit]: "20" },
      },
    ];
    const { holdings, feeBound } = expectedHoldings(
      deposits,
      transfers,
      withdrawals,
    );
    expect(
      sameValue(holdings.userA, { lovelace: 95_000_000n, [unit]: 30n }),
    ).toBe(true);
    expect(sameValue(holdings.userB, { lovelace: 10_000_000n })).toBe(true);
    expect(holdings.userC).toEqual({});
    expect(feeBound).toEqual({ userA: 2_000_000n, userB: 0n, userC: 0n });
  });

  it("drops zero quantities when adding values", () => {
    expect(addValues({ lovelace: 5n, [unit]: 1n }, { [unit]: -1n })).toEqual({
      lovelace: 5n,
    });
  });

  it("counts the node drained only when every stage is empty", () => {
    const idle = {
      durableAdmission: { backlog: "0" },
      localResidue: { mempoolTxCount: "0", processedMempoolTxCount: "0" },
      stateQueue: {
        queueLength: 0,
        unconfirmedSubmittedBlockTxHash: null,
        localFinalizationPending: false,
      },
      pendingBlockFinalizations: { oldestActive: null },
      localMutationJobs: { unfinished: "0" },
      settlement: { unfinishedJobs: "0", failingJobs: [] },
    };
    expect(drained(idle)).toBe(true);
    expect(
      drained({ ...idle, stateQueue: { ...idle.stateQueue, queueLength: 1 } }),
    ).toBe(false);
    expect(
      drained({
        ...idle,
        localResidue: { ...idle.localResidue, mempoolTxCount: "2" },
      }),
    ).toBe(false);
    expect(
      drained({
        ...idle,
        settlement: { unfinishedJobs: "1", failingJobs: [] },
      }),
    ).toBe(false);
    expect(drained({})).toBe(false);
  });
});

describe("nodeEnvironment", () => {
  it("points every node command at this run's deployment run state", () => {
    const layout = makeLayout(scratch());
    const run: RunEnv = {
      runId: "t",
      composeProject: "p",
      networkMagic: 42,
      ogmiosPort: 1,
      kupoPort: 2,
      postgresPort: 3,
      postgresUser: "u",
      postgresPassword: "pw",
      postgresDatabase: "d",
      cardanoImage: "c",
      postgresImage: "pg",
      portOffset: 0,
    };
    const identities = {
      schemaVersion: "midgard-devnet-identities-v1",
      seeds: Object.fromEntries(
        WALLET_ROLES.map((role) => [role, `seed-${role}`]),
      ),
      libp2p: Object.fromEntries(
        LIBP2P_IDENTITIES.map((id) => [id, "00".repeat(32)]),
      ),
      adminApiKey: "k",
      publicReaderPassword: "r",
    } as Identities;
    const artifacts = {
      nativeOwnerBinary: "o",
      nativeOwnerSha256: "h",
      chainSyncBinary: "c",
    };
    for (const role of ["command", "listen"] as const)
      expect(
        nodeEnvironment({
          layout,
          run,
          identities,
          artifacts,
          historyGenesisPin: "ab".repeat(32),
          role,
        }).MIDGARD_RUN_STATE_PATH,
      ).toBe(layout.deploymentRunState);
  });
});

describe("run locks", () => {
  it("never counts a recycled PID as the lock's owner", () => {
    const layout = stackLayout();
    writeFileSync(
      layout.supervisorPid,
      `${process.pid} ${processStartTime(process.pid)}`,
    );
    expect(runningSupervisor(layout)).toBe(process.pid);
    // Same PID, another process: the kernel's start time differs.
    writeFileSync(layout.supervisorPid, `${process.pid} 1`);
    expect(runningSupervisor(layout)).toBeUndefined();
    const release = acquireLock(layout.supervisorPid);
    expect(lockOwner(layout.supervisorPid)).toBe(process.pid);
    release();
    expect(existsSync(layout.supervisorPid)).toBe(false);
  });
});

describe("supervisorRuns", () => {
  it("tells a supervisor started with another service set from one running this set", () => {
    const layout = stackLayout();
    const current = [
      nodeService("a", "0"),
      nodeService("watcher", "0", { startGraceMs: 1 }),
    ];
    writeFileSync(
      layout.supervisorSpecs,
      specsDigest(current.slice(0, 1), "code"),
    );
    expect(supervisorRuns(layout, current, "code")).toBe(false);
    writeFileSync(layout.supervisorSpecs, specsDigest(current, "code"));
    expect(supervisorRuns(layout, current, "code")).toBe(true);
    expect(
      supervisorRuns(
        layout,
        [current[0]!, { ...current[1]!, env: { X: "1" } }],
        "code",
      ),
    ).toBe(false);
    // The same set on rebuilt code is another supervisor's.
    expect(supervisorRuns(layout, current, "rebuilt")).toBe(false);
  });
});

describe("waiting on services", () => {
  it("keeps waiting on a running service only inside its start grace", () => {
    const spec = nodeService("watcher", "0", { startGraceMs: 60_000 });
    const startedAt = new Date(0).toISOString();
    const report = { name: "watcher", alive: true, startedAt };
    expect(insideStartGrace(spec, report, 59_000)).toBe(true);
    expect(insideStartGrace(spec, report, 60_000)).toBe(false);
    expect(insideStartGrace(spec, { ...report, alive: false }, 1)).toBe(false);
    expect(insideStartGrace(nodeService("node", "0"), report, 1)).toBe(false);
  });

  it("leaves no abort listener behind once a supervisor sleep ends", async () => {
    const abort = new AbortController();
    for (let n = 0; n < 20; n += 1) await sleep(1, abort.signal);
    expect(getEventListeners(abort.signal, "abort")).toHaveLength(0);
  });
});
