import { spawn } from "node:child_process";
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { createServer, type Server } from "node:http";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { generateSeedPhrase } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { DeployContext } from "../src/devnet-stack/deploy.js";
import {
  type Identities,
  LIBP2P_IDENTITIES,
  WALLET_ROLES,
} from "../src/devnet-stack/identities.js";
import { Journal } from "../src/devnet-stack/journal.js";
import {
  makeLayout,
  type RunEnv,
  servicePorts,
} from "../src/devnet-stack/layout.js";
import { serviceSpecs } from "../src/devnet-stack/services.js";
import { waitForServices } from "../src/devnet-stack/stack.js";
import type { ServiceSpec } from "../src/devnet-stack/supervisor.js";

// These cases exercise ordinary URL waits and start grace, not authenticated
// history readiness. The real history cohort has its own native/FD3 suites.
vi.mock("../src/devnet-stack/watcher.js", () => ({
  watcherServiceSpecs: ({ layout, run }: DeployContext): ServiceSpec[] => {
    const operations = `http://127.0.0.1:${servicePorts(run).watcherOperations}`;
    return [
      {
        name: "watcher",
        command: process.execPath,
        args: [],
        cwd: layout.toolsRoot,
        env: {},
        healthUrl: `${operations}/v1/status`,
        readyUrl: `${operations}/readyz`,
        startGraceMs: 60 * 60_000,
      },
    ];
  },
}));

const dirs: string[] = [];
const pids: number[] = [];
afterEach(() => {
  for (const pid of pids.splice(0))
    try {
      process.kill(pid, "SIGKILL");
    } catch {
      // Already gone.
    }
  for (const dir of dirs.splice(0))
    rmSync(dir, { recursive: true, force: true });
});

const context = (): DeployContext => {
  const dir = mkdtempSync(join(tmpdir(), "devnet-wait-"));
  dirs.push(dir);
  const run: RunEnv = {
    runId: "t",
    composeProject: "p",
    networkMagic: 42,
    ogmiosPort: 22_337,
    kupoPort: 21_442,
    postgresPort: 25_432,
    postgresUser: "u",
    postgresPassword: "pw",
    postgresDatabase: "d",
    cardanoImage: "c",
    postgresImage: "pg",
    // Nothing answers on these ports: every service reads as not running.
    portOffset: 20_000,
  };
  const identities = {
    schemaVersion: "midgard-devnet-identities-v1",
    seeds: Object.fromEntries(
      WALLET_ROLES.map((role) => [role, generateSeedPhrase()]),
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
  const layout = makeLayout(dir);
  mkdirSync(layout.state, { recursive: true });
  new Journal(layout.journal).set("historyGenesisPin", {
    algorithm: "ogmios-shelley-result-lossless-v1",
    sha256: "ab".repeat(32),
    recordedAt: "2026-09-30T00:00:00.000Z",
  });
  return { layout, run, identities, artifacts };
};

const oneShot = { txHash: "00".repeat(32), outputIndex: 0 };

describe("waitForServices", () => {
  it("watches the supervisor it was handed before that supervisor has written its PID file", async () => {
    const child = spawn(
      process.execPath,
      ["-e", "setInterval(() => {}, 1000)"],
      { stdio: "ignore" },
    );
    pids.push(child.pid!);
    await expect(
      waitForServices(context(), oneShot, 1, { supervisorPid: child.pid! }),
    ).rejects.toThrow(/services not ready/);
  });

  it("fails fast once that supervisor is gone", async () => {
    const child = spawn(process.execPath, ["-e", ""], { stdio: "ignore" });
    await new Promise((resolve) => child.once("exit", resolve));
    await expect(
      waitForServices(context(), oneShot, 60_000, {
        supervisorPid: child.pid!,
      }),
    ).rejects.toThrow(/supervisor is not running/);
  });

  it("fails fast when no supervisor runs at all", async () => {
    await expect(waitForServices(context(), oneShot, 60_000)).rejects.toThrow(
      /supervisor is not running/,
    );
  });
});

describe("the node's start grace", () => {
  it("covers the node's own default startup budget", () => {
    const node = serviceSpecs(context(), oneShot).find(
      (spec) => spec.name === "node",
    )!;
    // Four provider steps retried 120 x 5 s, plus the 15 min first-start ledger scan.
    const startupBudgetMs = 4 * 120 * 5_000 + 15 * 60_000;
    expect(node.startGraceMs).toBeGreaterThanOrEqual(startupBudgetMs);
  });
});

describe("waitForServices inside a start grace", () => {
  it("waits past its bound while every pending service is graced, and says so", async () => {
    const base = context();
    const portOffset = 20_000 + (process.pid % 3_000);
    const ctx = {
      ...base,
      run: {
        ...base.run,
        portOffset,
        ogmiosPort: 2337 + portOffset,
        kupoPort: 1442 + portOffset,
        postgresPort: 5432 + portOffset,
      },
    };
    const specs = serviceSpecs(ctx, oneShot);
    const sleeper = spawn(
      process.execPath,
      ["-e", "setInterval(() => {}, 1000)"],
      { stdio: "ignore" },
    );
    pids.push(sleeper.pid!);
    const pidDir = join(ctx.layout.state, "services");
    mkdirSync(pidDir, { recursive: true });
    const record = JSON.stringify({
      pid: sleeper.pid,
      startedAt: new Date().toISOString(),
    });
    for (const spec of specs)
      writeFileSync(join(pidDir, `${spec.name}.json`), record);
    const servers: Server[] = [];
    const answer = (urls: (string | undefined)[]) => {
      for (const port of new Set(
        urls.flatMap((url) => (url === undefined ? [] : [new URL(url).port])),
      ))
        servers.push(
          createServer((_request, response) => response.end("{}")).listen(
            Number(port),
            "127.0.0.1",
          ),
        );
    };
    // Only the services a start grace covers (node, watcher) are not ready yet.
    const graced = specs.filter((spec) => spec.startGraceMs !== undefined);
    expect(graced.map((spec) => spec.name).sort()).toEqual(["node", "watcher"]);
    answer(
      specs
        .filter((spec) => spec.startGraceMs === undefined)
        .flatMap((s) => [s.healthUrl, s.readyUrl]),
    );
    let graces = 0;
    try {
      await waitForServices(ctx, oneShot, 1, {
        honourStartGrace: true,
        supervisorPid: sleeper.pid!,
        onGrace: () => {
          graces += 1;
          if (graces === 1)
            answer(graced.flatMap((s) => [s.healthUrl, s.readyUrl]));
        },
      });
    } finally {
      for (const server of servers) server.close();
    }
    expect(graces).toBe(1);
  }, 20_000);
});
