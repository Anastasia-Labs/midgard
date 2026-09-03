import { readdirSync, readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { parseWatcherArguments } from "../../src/cli.js";
import { unsafeRunWatcherCommandForTestV1 } from "../../src/runtime/scaffold.js";

const packageRoot = dirname(dirname(dirname(fileURLToPath(import.meta.url))));

const readJson = (path: string): Record<string, unknown> =>
  JSON.parse(readFileSync(path, "utf8")) as Record<string, unknown>;

describe("W00 watcher package identity", () => {
  it("declares an independent command package with all required scripts", () => {
    const watcherPackage = readJson(join(packageRoot, "package.json"));
    const committeePackage = readJson(
      join(packageRoot, "..", "da-committee-node", "package.json"),
    );

    expect(watcherPackage.name).toBe("midgard-watcher");
    expect(watcherPackage.bin).toEqual({
      "midgard-watcher": "./dist/cli.js",
    });
    expect(watcherPackage.dependencies).toEqual({
      "@al-ft/midgard-core": "workspace:*",
      "@al-ft/midgard-fault-proofs": "workspace:*",
      "@al-ft/midgard-sdk": "workspace:*",
      "@al-ft/midgard-validation": "workspace:*",
      "@chainsafe/libp2p-noise": "17.0.0",
      "@chainsafe/libp2p-yamux": "8.0.1",
      "@libp2p/peer-id": "6.0.11",
      "@libp2p/tcp": "11.0.22",
      "@lucid-evolution/lucid": "0.6.2",
      "@multiformats/multiaddr": "13.0.3",
      libp2p: "3.3.4",
    });
    expect(watcherPackage.scripts).toMatchObject({
      authority: "node dist/cli.js authority",
      build: expect.any(String),
      lint: expect.any(String),
      replay: "node dist/cli.js replay",
      start: "node dist/cli.js start",
      test: expect.any(String),
      typecheck: "tsc --noEmit",
    });

    expect(committeePackage.name).toBe("da-committee-node");
    expect(committeePackage.bin).toEqual({
      "da-committee-node": "./dist/index.js",
      "midgard-public-retained-da": "./dist/public-retained-da.js",
    });
    expect(committeePackage.name).not.toBe(watcherPackage.name);
  });

  it("does not alias the committee or import operator-private services", () => {
    const sourceDirectory = join(packageRoot, "src");
    const source = readdirSync(sourceDirectory, { recursive: true })
      .map(String)
      .filter((path) => path.endsWith(".ts"))
      .map((path) => readFileSync(join(sourceDirectory, path), "utf8"))
      .join("\n");

    expect(source).not.toMatch(/da-committee-node/);
    expect(source).not.toMatch(/midgard-node/);
    expect(source).not.toMatch(
      /(?:^|[/.-])(?:admin|database|postgres)(?:[/.-]|$)/,
    );
    expect(source).not.toMatch(/export\s+\*\s+from/);
  });
});

describe("production watcher commands", () => {
  it("requires an explicit config for authority, start and replay", () => {
    expect(parseWatcherArguments([])).toMatchObject({ kind: "invalid" });
    expect(parseWatcherArguments(["start"])).toMatchObject({ kind: "invalid" });
    expect(
      parseWatcherArguments(["replay", "--config", "/etc/watcher.json"]),
    ).toEqual({
      kind: "command",
      command: "replay",
      configPath: "/etc/watcher.json",
    });
  });

  it("closes replay after durable catch-up and closes start on shutdown", async () => {
    const events: string[] = [];
    const io = {
      writeOutput: (text: string) => events.push(text),
      writeError: (text: string) => events.push(text),
    };
    const dependencies = {
      runAuthority: async () => ({ close: async () => undefined }),
      runWatcher: async () => ({
        done: new Promise<void>(() => undefined),
        caughtUp: Promise.resolve(),
        faultProofReadiness: [
          { ready: true as const, category: "doubleSpend" },
        ],
        recoveredFaultProofWorkflowCount: 0,
        faultProofSupervisor: {
          status: () => ({
            phase: "accepting" as const,
            recovered: true,
            deadlineHealth: "safe" as const,
          }),
        },
        close: async () => {
          events.push("closed");
        },
      }),
      waitForShutdown: async () => "SIGTERM" as const,
    };
    await expect(
      unsafeRunWatcherCommandForTestV1(
        "replay",
        "/etc/watcher.json",
        io,
        dependencies,
      ),
    ).resolves.toBe(0);
    await expect(
      unsafeRunWatcherCommandForTestV1(
        "start",
        "/etc/watcher.json",
        io,
        dependencies,
      ),
    ).resolves.toBe(0);
    expect(events.filter((event) => event === "closed")).toHaveLength(2);
    expect(events.join("\n")).toContain('"state":"caught_up"');
    expect(events.join("\n")).toContain('"proofSupervisorState":"accepting"');
  });

  it("refuses to advertise readiness when journal recovery supervision is blocked", async () => {
    let closed = false;
    await expect(
      unsafeRunWatcherCommandForTestV1(
        "start",
        "/etc/watcher.json",
        { writeOutput: () => undefined, writeError: () => undefined },
        {
          runAuthority: async () => ({ close: async () => undefined }),
          runWatcher: async () => ({
            done: new Promise<void>(() => undefined),
            caughtUp: Promise.resolve(),
            faultProofReadiness: [
              { ready: true as const, category: "doubleSpend" },
            ],
            recoveredFaultProofWorkflowCount: 0,
            faultProofSupervisor: {
              status: () => ({
                phase: "blocked" as const,
                recovered: false,
                deadlineHealth: "unsafe" as const,
              }),
            },
            close: async () => {
              closed = true;
            },
          }),
          waitForShutdown: async () => "SIGTERM",
        },
      ),
    ).rejects.toThrow("proof supervision is not ready");
    expect(closed).toBe(true);
  });

  it("keeps liveness separate from readiness when a proof deadline is at risk", async () => {
    let closed = false;
    await expect(
      unsafeRunWatcherCommandForTestV1(
        "start",
        "/etc/watcher.json",
        { writeOutput: () => undefined, writeError: () => undefined },
        {
          runAuthority: async () => ({ close: async () => undefined }),
          runWatcher: async () => ({
            done: new Promise<void>(() => undefined),
            caughtUp: Promise.resolve(),
            faultProofReadiness: [
              { ready: true as const, category: "doubleSpend" },
            ],
            recoveredFaultProofWorkflowCount: 0,
            faultProofSupervisor: {
              status: () => ({
                phase: "accepting" as const,
                recovered: true,
                deadlineHealth: "at_risk" as const,
              }),
            },
            close: async () => {
              closed = true;
            },
          }),
          waitForShutdown: async () => "SIGTERM",
        },
      ),
    ).rejects.toThrow("proof supervision is not ready");
    expect(closed).toBe(true);
  });

  it("treats an unexpected clean runtime exit as a liveness failure and closes", async () => {
    let closed = false;
    await expect(
      unsafeRunWatcherCommandForTestV1(
        "start",
        "/etc/watcher.json",
        { writeOutput: () => undefined, writeError: () => undefined },
        {
          runAuthority: async () => ({ close: async () => undefined }),
          runWatcher: async () => ({
            done: Promise.resolve(),
            caughtUp: Promise.resolve(),
            faultProofReadiness: [
              { ready: true as const, category: "doubleSpend" },
            ],
            recoveredFaultProofWorkflowCount: 0,
            faultProofSupervisor: {
              status: () => ({
                phase: "accepting" as const,
                recovered: true,
                deadlineHealth: "safe" as const,
              }),
            },
            close: async () => {
              closed = true;
            },
          }),
          waitForShutdown: async () =>
            await new Promise<"SIGTERM">(() => undefined),
        },
      ),
    ).rejects.toThrow("liveness ended before shutdown");
    expect(closed).toBe(true);
  });

  it("fails replay instead of hanging when runtime liveness ends before catch-up", async () => {
    let closed = false;
    await expect(
      unsafeRunWatcherCommandForTestV1(
        "replay",
        "/etc/watcher.json",
        { writeOutput: () => undefined, writeError: () => undefined },
        {
          runAuthority: async () => ({ close: async () => undefined }),
          runWatcher: async () => ({
            done: Promise.resolve(),
            caughtUp: new Promise<void>(() => undefined),
            faultProofReadiness: [
              { ready: true as const, category: "doubleSpend" },
            ],
            recoveredFaultProofWorkflowCount: 0,
            faultProofSupervisor: {
              status: () => ({
                phase: "accepting" as const,
                recovered: true,
                deadlineHealth: "safe" as const,
              }),
            },
            close: async () => {
              closed = true;
            },
          }),
          waitForShutdown: async () => "SIGTERM",
        },
      ),
    ).rejects.toThrow("liveness ended before durable catch-up");
    expect(closed).toBe(true);
  });

  it("keeps the trusted-head authority process separate and closes it on signal", async () => {
    const events: string[] = [];
    await expect(
      unsafeRunWatcherCommandForTestV1(
        "authority",
        "/etc/authority.json",
        {
          writeOutput: (text) => events.push(text),
          writeError: (text) => events.push(text),
        },
        {
          runAuthority: async () => ({
            close: async () => {
              events.push("authority-closed");
            },
          }),
          runWatcher: async () => {
            throw new Error("watcher process must not be constructed");
          },
          waitForShutdown: async () => "SIGINT",
        },
      ),
    ).resolves.toBe(0);
    expect(events).toContain("authority-closed");
  });
});
