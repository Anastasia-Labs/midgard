import "./da-bond-pool-committee-process.the-committee-node.js";

import { describe, expect, it } from "vitest";

import {
  createDaBondPoolCommitteeObserver,
  type DaBondPoolCommitteeExit,
  daBondPoolCommitteeExpectedView,
  daBondPoolCommitteeLifecycle,
  type DaBondPoolCommitteeProcess,
  DaBondPoolCommitteeProcessError,
  type DaBondPoolCommitteeRecord,
  daBondPoolCommitteeStopSettleMs,
} from "./da-bond-pool-committee-process.js";
import {
  answer,
  fakeClock,
  type L1Source,
  quarantined,
  REPLAY_STUCK,
  shortReason,
} from "./da-bond-pool-committee-process.the-journey-committee-node.js";
import { DA_BOND_POOL_JOURNEY_CHRONOLOGY } from "./da-bond-pool-journey.js";

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
    let l1Source: L1Source | undefined;
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
          return answer(reasons, clock.now(), l1Source);
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
      setL1Source: (next: L1Source | undefined) => {
        l1Source = next;
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

  it("records, then fails, an observation whose L1 source is quarantined", async () => {
    const h = harness();
    h.setL1Source({ status: "healthy" });
    await h.observer.start();
    await expect(h.observer.observe()).resolves.toMatchObject({
      synced: true,
    });
    h.setReasons([`L1 source is quarantined: ${REPLAY_STUCK}`]);
    h.setL1Source(quarantined);
    await expect(h.observer.observe()).rejects.toThrow(
      new RegExp(
        `\\(pid 500\\) quarantined its L1 source: ${REPLAY_STUCK}`,
        "u",
      ),
    );
    expect(h.records.map(({ kind }) => kind)).toEqual([
      "start",
      "observe",
      "observe",
    ]);
    expect(h.records.at(-1)).toMatchObject({
      kind: "observe",
      readyzHttpStatus: 503,
    });
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
