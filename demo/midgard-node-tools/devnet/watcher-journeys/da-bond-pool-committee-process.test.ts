import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import {
  awaitDaBondPoolCommitteeSync,
  buildDaBondPoolCommitteeEnv,
  createDaBondPoolCommitteeObserver,
  DaBondPoolCommitteeEnvError,
  type DaBondPoolCommitteeExit,
  daBondPoolCommitteeExpectedView,
  daBondPoolCommitteeLifecycle,
  type DaBondPoolCommitteeProcess,
  DaBondPoolCommitteeProcessError,
  type DaBondPoolCommitteeRecord,
  daBondPoolCommitteeStopSettleMs,
  daBondPoolCommitteeSyncBoundMs,
  daBondPoolCommitteeViewAgrees,
  daBondPoolSubmitterUtxoChange,
  redactDaBondPoolEnv,
  spawnDaBondPoolCommitteeNode,
  worktreeDerivedPort,
} from "./da-bond-pool-committee-process.js";
import { DA_BOND_POOL_JOURNEY_CHRONOLOGY } from "./da-bond-pool-journey.js";

const T0 = Date.parse("2026-09-28T12:00:00.000Z");
const iso = (ms: number) => new Date(ms).toISOString();
const shortReason = (backing: bigint, at: number) =>
  `da_bond_pool_backing_short: backing=${backing}, required=100, checkedAt=${iso(at)}`;
const withdrawingReason = (unlockAt: bigint, at: number) =>
  `da_bond_pool_withdrawing: unlockAt=${unlockAt}, checkedAt=${iso(at)}`;
const body = (reasons: readonly string[], lastStartedAt?: number) =>
  JSON.stringify({
    ready: reasons.length === 0,
    reasons,
    ...(lastStartedAt === undefined
      ? {}
      : { scanner: { lastStartedAt: iso(lastStartedAt) } }),
  });
const answer = (reasons: readonly string[], lastStartedAt?: number) => ({
  httpStatus: reasons.length === 0 ? 200 : 503,
  body: body(reasons, lastStartedAt),
});

/** A clock that `sleep` advances. */
const fakeClock = (start = T0) => {
  let t = start;
  return {
    now: () => t,
    sleep: async (ms: number) => {
      t += ms;
    },
    advance: (ms: number) => {
      t += ms;
    },
  };
};

describe("the journey committee node's environment (P27(1))", () => {
  const base = {
    settings: {
      MIDGARD_NETWORK: "Custom",
      CARDANO_NETWORK_MAGIC: "42",
    },
    l1Submitter: { source: "file:/run/secrets/l1.key", keyHash: "aa" },
    availabilitySubmitter: {
      source: "file:/run/secrets/availability.key",
      keyHash: "bb",
    },
    operationalKeyHashes: { operator: "cc", challenger: "dd" },
    libp2pKeySource: "file:/run/secrets/libp2p.key",
    journalPath: "/run/committee/journal.jsonl",
    databaseUrl: "postgres://user:secret@127.0.0.1:5433/committee",
    apiHost: "127.0.0.1",
    apiPort: 23_456,
    pollIntervalMs: 1_000,
    inherited: {
      PATH: "/usr/bin",
      HOME: "/home/journey",
      DA_SIGNER_INDEX: "0",
      SECRET_TOKEN: "leak",
    },
  } as const;

  it("submits to L1 with preflight on, holds no signer, and drops every other inherited variable", () => {
    const { env, recorded } = buildDaBondPoolCommitteeEnv(base);
    expect(env).toMatchObject({
      DA_L1_SUBMISSION_ENABLED: "true",
      DA_L1_PREFLIGHT_ENABLED: "true",
      L1_SUBMITTER_KEY_SOURCE: "file:/run/secrets/l1.key",
      DA_AVAILABILITY_SUBMITTER_KEY_SOURCE:
        "file:/run/secrets/availability.key",
      DA_AVAILABILITY_JOURNAL_PATH: "/run/committee/journal.jsonl",
      DA_COMMITTEE_API_PORT: "23456",
      DA_COMMITTEE_POLL_INTERVAL_MS: "1000",
      MIDGARD_CONFIG_MODE: "disabled",
      MIDGARD_DOTENV_MODE: "disabled",
      PATH: "/usr/bin",
      HOME: "/home/journey",
      MIDGARD_NETWORK: "Custom",
    });
    expect(
      Object.keys(env).filter((name) =>
        /SIGNER_INDEX|SIGNER_KEY|AUTO_FUND|SECRET_TOKEN/u.test(name),
      ),
    ).toEqual([]);
    expect(recorded.DA_COMMITTEE_DATABASE_URL).toBe("<redacted>");
    expect(recorded.PATH).toBeUndefined();
  });

  it.each([
    [
      "a signer index",
      { settings: { DA_SIGNER_INDEX: "0" } },
      /DA_SIGNER_INDEX is set/u,
    ],
    [
      "a signer key source",
      { settings: { DA_SIGNER_KEY_SOURCE_0: "file:/k" } },
      /DA_SIGNER_KEY_SOURCE_0 is set/u,
    ],
    [
      "an auto-fund key",
      { settings: { DA_L1_AUTO_FUND_KEY_SOURCE: "file:/k" } },
      /DA_L1_AUTO_FUND_KEY_SOURCE is set/u,
    ],
    [
      "a port-owned variable in settings",
      { settings: { DA_L1_PREFLIGHT_ENABLED: "false" } },
      /DA_L1_PREFLIGHT_ENABLED is set by the port/u,
    ],
    [
      "equal submitter key hashes",
      {
        availabilitySubmitter: {
          source: "file:/run/secrets/availability.key",
          keyHash: "aa",
        },
      },
      /are the same key/u,
    ],
    [
      "one key file for both submitters",
      {
        availabilitySubmitter: {
          source: "file:/run/secrets/l1.key",
          keyHash: "bb",
        },
      },
      /are the same key/u,
    ],
    [
      "a submitter key that is an operational key",
      { l1Submitter: { source: "file:/run/secrets/l1.key", keyHash: "cc" } },
      /the L1 submitter key is the operator key/u,
    ],
    [
      "a relative journal path",
      { journalPath: "journal.jsonl" },
      /is not absolute/u,
    ],
  ] as const)("refuses %s", (_label, override, message) => {
    expect(() => buildDaBondPoolCommitteeEnv({ ...base, ...override })).toThrow(
      DaBondPoolCommitteeEnvError,
    );
    expect(() => buildDaBondPoolCommitteeEnv({ ...base, ...override })).toThrow(
      message,
    );
  });

  it("redacts inline key sources and named secrets, and keeps file sources", () => {
    expect(
      redactDaBondPoolEnv({
        A: "seed:abandon abandon",
        B: "file:/k",
        DA_COMMITTEE_DATABASE_URL: "postgres://x",
      }),
    ).toEqual({
      A: "<redacted>",
      B: "file:/k",
      DA_COMMITTEE_DATABASE_URL: "<redacted>",
    });
  });

  it("derives a stable port per worktree and purpose", () => {
    const a = worktreeDerivedPort("/w/one", "committee-api");
    expect(a).toBe(worktreeDerivedPort("/w/one", "committee-api"));
    expect(a).toBeGreaterThanOrEqual(20_000);
    expect(a).toBeLessThan(40_000);
    expect(
      new Set([
        a,
        worktreeDerivedPort("/w/two", "committee-api"),
        worktreeDerivedPort("/w/one", "other"),
      ]).size,
    ).toBe(3);
  });
});

describe("the committee node's view of the port's pool snapshot", () => {
  const view = (
    snapshot: Parameters<typeof daBondPoolCommitteeExpectedView>[0],
  ) => daBondPoolCommitteeExpectedView(snapshot, 100n);

  it("agrees exactly, in both directions", () => {
    const short = view({ state: "bonded", backing: 40n });
    expect(daBondPoolCommitteeViewAgrees([shortReason(40n, T0)], short)).toBe(
      true,
    );
    expect(daBondPoolCommitteeViewAgrees([shortReason(41n, T0)], short)).toBe(
      false,
    );
    expect(daBondPoolCommitteeViewAgrees([], short)).toBe(false);

    const backed = view({ state: "bonded", backing: 100n });
    expect(daBondPoolCommitteeViewAgrees([], backed)).toBe(true);
    expect(daBondPoolCommitteeViewAgrees([shortReason(100n, T0)], backed)).toBe(
      false,
    );

    const withdrawing = view({
      state: "withdrawing",
      backing: 100n,
      unlockAt: 777,
    });
    expect(
      daBondPoolCommitteeViewAgrees([withdrawingReason(777n, T0)], withdrawing),
    ).toBe(true);
    expect(
      daBondPoolCommitteeViewAgrees([withdrawingReason(778n, T0)], withdrawing),
    ).toBe(false);
    expect(
      daBondPoolCommitteeViewAgrees(
        [withdrawingReason(777n, T0), "da_bond_pool_something_else"],
        withdrawing,
      ),
    ).toBe(false);
    expect(view({ state: "missing", backing: 0n })).toBeUndefined();
    expect(daBondPoolCommitteeViewAgrees([], undefined)).toBe(false);
  });

  it("waits for a pool read that started after the action, not an older answer", async () => {
    const clock = fakeClock();
    const since = clock.now();
    const expected = view({ state: "bonded", backing: 100n });
    // The node's last tick started before the action, then a later tick.
    const answers = [
      answer([], since - 5_000),
      answer([], since + 1_000),
      answer([], since + 1_000),
      answer([], since + 2_000),
    ];
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => answers.shift()!,
      expected,
      since,
      timeoutMs: 60_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: true, synced: true, reads: 4 });
  });

  it("takes a pool reason checked after the action as fresh", async () => {
    const clock = fakeClock();
    const since = clock.now();
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => answer([shortReason(40n, since + 10)], since - 1),
      expected: view({ state: "bonded", backing: 40n }),
      since,
      timeoutMs: 60_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: true, synced: true, reads: 1 });
  });

  it("returns the stale answer unsynced once the bound passes", async () => {
    const clock = fakeClock();
    const since = clock.now();
    // A reason the top-up should have cleared, checked before the top-up.
    const stale = answer([shortReason(40n, since - 1)], since - 1);
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => stale,
      expected: view({ state: "bonded", backing: 140n }),
      since,
      timeoutMs: 2_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: false, synced: false, reads: 5 });
    expect(sync.readyz.poolReasons).toEqual([shortReason(40n, since - 1)]);
  });

  it("returns a fresh answer that disagrees unsynced", async () => {
    const clock = fakeClock();
    const since = clock.now();
    const sync = await awaitDaBondPoolCommitteeSync({
      read: async () => answer([shortReason(39n, since + 1)]),
      expected: view({ state: "bonded", backing: 40n }),
      since,
      timeoutMs: 1_000,
      pollMs: 500,
      ...clock,
    });
    expect(sync).toMatchObject({ fresh: true, synced: false });
  });

  it("bounds the sync wait by polls plus twice the ideal confirmation time", () => {
    // The devnet: 2 s polls, depth 3, 1 s slots, f = 0.05.
    expect(
      daBondPoolCommitteeSyncBoundMs({
        pollIntervalMs: 2_000,
        confirmationDepth: 3,
        slotLengthMs: 1_000,
        activeSlotsCoeff: 0.05,
      }),
    ).toBe(10 * 2_000 + 2 * 3 * 20_000);
    expect(
      daBondPoolCommitteeSyncBoundMs({
        pollIntervalMs: 1_000,
        confirmationDepth: 1,
        slotLengthMs: 1_000,
        activeSlotsCoeff: 1,
        polls: 3,
      }),
    ).toBe(3_000 + 2_000);
    for (const bad of [
      { pollIntervalMs: 0 },
      { confirmationDepth: 0 },
      { confirmationDepth: 1.5 },
      { slotLengthMs: 0 },
      { activeSlotsCoeff: 0 },
      { activeSlotsCoeff: 1.5 },
      { activeSlotsCoeff: Number.NaN },
      { polls: 0 },
    ]) {
      expect(() =>
        daBondPoolCommitteeSyncBoundMs({
          pollIntervalMs: 2_000,
          confirmationDepth: 3,
          slotLengthMs: 1_000,
          activeSlotsCoeff: 0.05,
          ...bad,
        }),
      ).toThrow("Invalid committee sync bound inputs");
    }
  });

  it("watches the submitters after a stop for a node poll plus the finality lag", () => {
    // The devnet: 2 s polls, depth 3, 1 s slots, f = 0.05.
    const cadence = {
      pollIntervalMs: 2_000,
      confirmationDepth: 3,
      slotLengthMs: 1_000,
      activeSlotsCoeff: 0.05,
    };
    expect(daBondPoolCommitteeStopSettleMs(cadence)).toBe(
      2_000 + 2 * 3 * 20_000,
    );
    expect(
      daBondPoolCommitteeStopSettleMs({ ...cadence, activeSlotsCoeff: 1 }),
    ).toBe(2_000 + 2 * 3 * 1_000);
    expect(() =>
      daBondPoolCommitteeStopSettleMs({ ...cadence, confirmationDepth: 0 }),
    ).toThrow("Invalid committee sync bound inputs");
  });

  it("names the UTxOs a submitter spent or created", () => {
    expect(daBondPoolSubmitterUtxoChange(["a#0", "b#1"], ["b#1", "a#0"])).toBe(
      undefined,
    );
    expect(daBondPoolSubmitterUtxoChange(["a#0", "b#1"], ["b#1", "c#0"])).toBe(
      "spent [a#0], created [c#0]",
    );
  });
});

describe("the committee node's lifecycle (P27(3))", () => {
  it("runs through steps 1, 3, 4, 5 and 6, is stopped for step 2, and ends stopped", () => {
    let running = false;
    const runningDuring: number[] = [];
    const actions: string[] = [];
    for (const step of DA_BOND_POOL_JOURNEY_CHRONOLOGY) {
      for (const phase of ["before", "after"] as const) {
        const action = daBondPoolCommitteeLifecycle(phase, step);
        if (action === undefined) {
          if (phase === "before" && running) runningDuring.push(step);
          continue;
        }
        actions.push(`${action} ${phase} ${step.toString()}`);
        // A start of a running node or a stop of a stopped one is refused.
        expect(running).toBe(action === "stop");
        running = action === "start";
        if (phase === "before" && running) runningDuring.push(step);
      }
    }
    expect(actions).toEqual([
      "start before 1",
      "stop before 2",
      "start before 6",
      "stop after 6",
    ]);
    expect(runningDuring).toEqual([1, 3, 4, 5, 6]);
    expect(running).toBe(false);
  });

  type FakeNode = DaBondPoolCommitteeProcess & {
    write: (line: string) => void;
    writeStdout: (line: string) => void;
    die: () => void;
  };

  // A 2 s node poll plus 2 * 1 * 1 s / 0.5: a 6 s settle window.
  const HARNESS_CADENCE = {
    pollIntervalMs: 2_000,
    confirmationDepth: 1,
    slotLengthMs: 1_000,
    activeSlotsCoeff: 0.5,
  };

  const harness = (
    options: {
      exit?: Partial<DaBondPoolCommitteeExit>;
      readyzFails?: boolean;
    } = {},
  ) => {
    const clock = fakeClock();
    const daemonChecks: number[][] = [];
    const records: DaBondPoolCommitteeRecord[] = [];
    let utxos = ["fund#0", "fund#1"];
    let utxoReads = 0;
    let onUtxoRead: ((read: number) => void) | undefined;
    let nextPid = 500;
    let reasons: string[] = [];
    const nodes: FakeNode[] = [];
    const spawn = async (): Promise<DaBondPoolCommitteeProcess> => {
      const pid = nextPid++;
      let stderr = Buffer.alloc(0);
      let stdout = Buffer.alloc(0);
      let alive = true;
      const node: FakeNode = {
        pid,
        alive: () => alive,
        stderr: () => new Uint8Array(stderr),
        stdout: () => new Uint8Array(stdout),
        readyz: async () => {
          if (options.readyzFails) throw new Error("connection refused");
          return answer(reasons, clock.now());
        },
        stop: async () => {
          alive = false;
          return {
            exitCode: 0,
            signal: null,
            killed: false,
            stderrTail: "",
            ...options.exit,
          };
        },
        write: (line) => {
          stderr = Buffer.concat([stderr, Buffer.from(`${line}\n`)]);
        },
        writeStdout: (line) => {
          stdout = Buffer.concat([stdout, Buffer.from(`${line}\n`)]);
        },
        die: () => {
          alive = false;
        },
      };
      nodes.push(node);
      return node;
    };
    const observer = createDaBondPoolCommitteeObserver({
      spawn,
      checkDaemons: (admitted) => daemonChecks.push([...admitted]),
      submitterUtxos: async () => {
        utxoReads += 1;
        onUtxoRead?.(utxoReads);
        return utxos;
      },
      expectedView: async () =>
        daBondPoolCommitteeExpectedView(
          {
            state: "bonded",
            backing: reasons.length === 0 ? 100n : 40n,
          },
          100n,
        ),
      env: { DA_L1_SUBMISSION_ENABLED: "true" },
      record: async (entry) => {
        records.push(entry);
      },
      syncTimeoutMs: 10_000,
      startTimeoutMs: 3_000,
      pollMs: 500,
      stopBoundMs: 1_000,
      nodeCadence: HARNESS_CADENCE,
      now: clock.now,
      sleep: clock.sleep,
    });
    return {
      observer,
      clock,
      daemonChecks,
      records,
      nodes,
      setUtxos: (next: string[]) => {
        utxos = next;
      },
      utxoReads: () => utxoReads,
      setOnUtxoRead: (next: (read: number) => void) => {
        onUtxoRead = next;
      },
      setReasons: (next: string[]) => {
        reasons = next;
      },
    };
  };

  it("admits only its own pid, ties each event to it, and never carries one across a restart", async () => {
    const h = harness();
    const pid = await h.observer.start();
    expect(pid).toBe(500);
    // No daemon before the spawn, exactly the node's pid after it.
    expect(h.daemonChecks).toEqual([[], [500]]);
    expect([...h.observer.admitted()]).toEqual([500]);

    const first = await h.observer.observe();
    expect(first).toMatchObject({
      readinessReasons: [],
      events: [],
      process: { pid: 500, readyzHttpStatus: 200, eventPids: [] },
      synced: true,
    });

    h.setReasons([shortReason(40n, h.clock.now() + 1)]);
    h.nodes[0]!.write("plain log line");
    h.nodes[0]!.write(
      JSON.stringify({ event: "da_bond_pool_backing_short", backing: "40" }),
    );
    const second = await h.observer.observe();
    expect(second.events).toEqual(["da_bond_pool_backing_short"]);
    expect(second.process.eventPids).toEqual([500]);
    expect(second.readinessReasons).toHaveLength(1);
    expect(JSON.parse(second.process.readyzBody)).toMatchObject({
      ready: false,
    });

    // The responder's reports: distinct lines, this pid's only.
    const unavailable = JSON.stringify({
      event: "availability_responder",
      challenges: 1,
      headerHash: "b1",
      status: "unavailable",
    });
    h.nodes[0]!.write(unavailable);
    h.nodes[0]!.write(unavailable);
    h.nodes[0]!.write(
      JSON.stringify({ event: "availability_responder", status: "failed" }),
    );
    // An action's report goes to stdout, and is read from there.
    const acted = JSON.stringify({
      event: "availability_responder",
      challenges: 1,
      headerHash: "b1",
      action: "publish",
      status: "pending",
    });
    h.nodes[0]!.writeStdout("plain stdout line");
    h.nodes[0]!.writeStdout(acted);
    const third = await h.observer.observe();
    expect(third.events).toEqual([]);
    expect(third.process.availabilityResponder).toEqual([
      unavailable,
      JSON.stringify({ event: "availability_responder", status: "failed" }),
      acted,
    ]);
    expect(second.process.availabilityResponder).toEqual([]);

    // An event written just before the stop is never reported afterwards.
    h.nodes[0]!.write(JSON.stringify({ event: "da_bond_pool_withdrawing" }));
    h.nodes[0]!.write(unavailable);
    await h.observer.stop();
    expect(h.daemonChecks.at(-1)).toEqual([]);
    h.setReasons([]);
    expect(await h.observer.start()).toBe(501);
    const restarted = await h.observer.observe();
    expect(restarted.events).toEqual([]);
    expect(restarted.process.availabilityResponder).toEqual([]);
    expect(restarted.process.pid).toBe(501);
    expect(h.records.map(({ kind }) => kind)).toEqual([
      "start",
      "observe",
      "observe",
      "observe",
      "stop",
      "start",
      "observe",
    ]);
  });

  it("fails the stop when the node did not exit 0 on SIGTERM", async () => {
    for (const exit of [
      { exitCode: 1 },
      { exitCode: null, signal: "SIGKILL", killed: true },
    ]) {
      const h = harness({ exit });
      await h.observer.start();
      await expect(h.observer.stop()).rejects.toThrow(
        /did not exit 0 on SIGTERM/u,
      );
      expect(h.observer.running()).toBe(false);
    }
  });

  it("fails when a submitter address changed while the node ran or while it was stopped", async () => {
    const ran = harness();
    await ran.observer.start();
    ran.setUtxos(["fund#0", "spent-by-node#0"]);
    await expect(ran.observer.stop()).rejects.toThrow(
      /submitter addresses changed while it ran: spent \[fund#1\], created \[spent-by-node#0\]/u,
    );

    const stopped = harness();
    await stopped.observer.start();
    await stopped.observer.stop();
    stopped.setUtxos(["fund#0"]);
    await expect(stopped.observer.start()).rejects.toThrow(
      /submitter addresses changed before its start/u,
    );
    expect(stopped.nodes).toHaveLength(1);
  });

  it("watches the submitter addresses for the settle window after an exit", async () => {
    const quiet = harness();
    await quiet.observer.start();
    const before = quiet.utxoReads();
    await quiet.observer.stop();
    // The window is derived from the node's cadence, not passed in: every
    // 500 ms poll across the 6 s window, then the last read at its end.
    expect(daBondPoolCommitteeStopSettleMs(HARNESS_CADENCE)).toBe(6_000);
    expect(quiet.utxoReads() - before).toBe(13);

    // A last-tick submission lands after the exit, on the third read.
    const late = harness();
    await late.observer.start();
    const afterStart = late.utxoReads();
    late.setOnUtxoRead((read) => {
      if (read === afterStart + 3) late.setUtxos(["fund#0", "late-tx#0"]);
    });
    await expect(late.observer.stop()).rejects.toThrow(
      /submitter addresses changed while it ran: spent \[fund#1\], created \[late-tx#0\]/u,
    );
  });

  it("fails an observation of a node that exited, and a start whose /readyz never answers", async () => {
    const h = harness();
    await h.observer.start();
    h.nodes[0]!.die();
    await expect(h.observer.observe()).rejects.toThrow(
      DaBondPoolCommitteeProcessError,
    );

    const silent = harness({ readyzFails: true });
    await expect(silent.observer.start()).rejects.toThrow(
      /never answered \/readyz/u,
    );
  });

  it("refuses a second start and an observation with no node, and tears down without throwing", async () => {
    const h = harness({ exit: { exitCode: 1 } });
    await expect(h.observer.observe()).rejects.toThrow(/not running/u);
    await h.observer.start();
    await expect(h.observer.start()).rejects.toThrow(/already runs/u);
    await expect(h.observer.teardown()).resolves.toMatchObject({
      exitCode: 1,
    });
    await expect(h.observer.teardown()).resolves.toBeUndefined();
  });
});

describe("the real committee node process's output capture (P27(2))", () => {
  it("reads the responder's stderr reports and its stdout action reports from a spawned child", async () => {
    const logDirectory = await mkdtemp(join(tmpdir(), "da-bond-pool-spawn-"));
    const unavailable = (headerHash: string) =>
      JSON.stringify({
        event: "availability_responder",
        headerHash,
        status: "unavailable",
      });
    const acted = (headerHash: string) =>
      JSON.stringify({
        event: "availability_responder",
        headerHash,
        action: "publish",
        status: "pending",
      });
    // A JSON event on stdout that is not a responder report, as the node's
    // chain sync writes; it must never reach the responder lines.
    const chainSync = JSON.stringify({ event: "l1_chain_sync_caught_up" });
    const flag = join(logDirectory, "second-round");
    // As the node does: failed/unavailable to stderr, actions to stdout.
    // stdin is ignored, so the second round waits on a flag file.
    const script = [
      'const { existsSync } = require("node:fs");',
      'process.on("SIGTERM", () => process.exit(0));',
      `process.stdout.write(${JSON.stringify(`${chainSync}\n`)});`,
      `process.stderr.write(${JSON.stringify(`${unavailable("b1")}\n`)});`,
      `process.stdout.write(${JSON.stringify(`${acted("b1")}\n`)});`,
      "let second = false;",
      "setInterval(() => {",
      `  if (second || !existsSync(${JSON.stringify(flag)})) return;`,
      "  second = true;",
      `  process.stderr.write(${JSON.stringify(`${unavailable("b2")}\n`)});`,
      `  process.stdout.write(${JSON.stringify(`${acted("b2")}\n`)});`,
      "}, 10);",
    ].join("\n");
    let spawned: DaBondPoolCommitteeProcess | undefined;
    const observer = createDaBondPoolCommitteeObserver({
      spawn: async () => {
        const node = await spawnDaBondPoolCommitteeNode({
          argv: [process.execPath, "-e", script],
          env: { PATH: process.env.PATH ?? "" },
          cwd: logDirectory,
          logDirectory,
          apiUrl: "http://127.0.0.1:1",
        });
        spawned = node;
        // No HTTP API in the child; the capture is what is under test.
        return { ...node, readyz: async () => answer([], Date.now()) };
      },
      checkDaemons: () => {},
      submitterUtxos: async () => ["fund#0"],
      expectedView: async () =>
        daBondPoolCommitteeExpectedView(
          { state: "bonded", backing: 100n },
          100n,
        ),
      env: {},
      syncTimeoutMs: 1_000,
      startTimeoutMs: 5_000,
      pollMs: 10,
      stopBoundMs: 5_000,
      // A 20 ms settle window.
      nodeCadence: {
        pollIntervalMs: 10,
        confirmationDepth: 1,
        slotLengthMs: 5,
        activeSlotsCoeff: 1,
      },
    });
    try {
      await observer.start();
      // Wait on the log files, which the capture does not feed, so a lost
      // in-memory line shows in the observation, not as a timeout here.
      const logged = (stream: "stdout" | "stderr") =>
        readFile(
          join(
            logDirectory,
            `committee-${spawned!.pid.toString()}.${stream}.log`,
          ),
          "utf8",
        ).catch(() => "");
      const awaitLogged = async (stdout: string, stderr: string) => {
        const deadline = Date.now() + 10_000;
        while (
          (await logged("stdout")) !== stdout ||
          (await logged("stderr")) !== stderr
        ) {
          if (Date.now() >= deadline)
            throw new Error("The child never logged its lines");
          await new Promise((resolveWait) => setTimeout(resolveWait, 10));
        }
      };
      await awaitLogged(
        `${chainSync}\n${acted("b1")}\n`,
        `${unavailable("b1")}\n`,
      );
      const observation = await observer.observe();
      expect(observation.process.pid).toBe(spawned!.pid);
      expect(observation.process.availabilityResponder).toEqual([
        unavailable("b1"),
        acted("b1"),
      ]);
      // Output after an observation reaches the next one, and only it.
      await writeFile(flag, "");
      await awaitLogged(
        `${chainSync}\n${acted("b1")}\n${acted("b2")}\n`,
        `${unavailable("b1")}\n${unavailable("b2")}\n`,
      );
      const second = await observer.observe();
      expect(second.process.availabilityResponder).toEqual([
        unavailable("b2"),
        acted("b2"),
      ]);
      await expect(observer.stop()).resolves.toMatchObject({
        exitCode: 0,
        killed: false,
      });
    } finally {
      await observer.teardown();
      await rm(logDirectory, { recursive: true, force: true });
    }
  });
});
