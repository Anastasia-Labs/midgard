import { setTimeout as pause } from "node:timers/promises";

import { type DaBondPoolCommitteeExpectedView } from "./da-bond-pool-committee-process.build-da-bond-pool-committee-env.js";
import {
  awaitDaBondPoolCommitteeSync,
  DA_BOND_POOL_RESPONDER_EVENT,
  type DaBondPoolCommitteeExit,
  type DaBondPoolCommitteeProcess,
  daBondPoolCommitteeStopSettleMs,
  daBondPoolSubmitterUtxoChange,
  tail,
} from "./da-bond-pool-committee-process.spawn-da-bond-pool-committee-node.js";
import { createDaBondPoolStderrCursor } from "./da-bond-pool-process-evidence.js";

// ---------------------------------------------------------------------------
// The observer
// ---------------------------------------------------------------------------

/** What one observation of the node returns to the journey driver. */
export type DaBondPoolCommitteeObservation = Readonly<{
  readinessReasons: readonly string[];
  events: readonly string[];
  process: Readonly<{
    pid: number;
    readyzHttpStatus: number;
    readyzBody: string;
    eventPids: readonly number[];
    /**
     * The distinct `availability_responder` JSON lines this pid wrote since
     * the previous observation, stderr's (failed, unavailable) before
     * stdout's (pending, included, confirmed, each with its `action`), each
     * in first-seen order (P27(2)).
     */
    availabilityResponder: readonly string[];
  }>;
  synced: boolean;
  fresh: boolean;
}>;

/** A start, stop or observation the port records as an artifact. */
export type DaBondPoolCommitteeRecord = Readonly<
  | { kind: "start"; pid: number; env: Readonly<Record<string, string>> }
  | ({ kind: "stop"; pid: number } & DaBondPoolCommitteeExit)
  | {
      kind: "observe";
      pid: number;
      readyzHttpStatus: number;
      readyzBody: string;
      events: readonly string[];
      availabilityResponder: readonly string[];
      synced: boolean;
      fresh: boolean;
      reads: number;
    }
>;

export class DaBondPoolCommitteeProcessError extends Error {
  constructor(message: string, options?: ErrorOptions) {
    super(message, options);
    this.name = "DaBondPoolCommitteeProcessError";
  }
}

/**
 * The node's lifecycle (P27(3)): start, observe, stop, teardown. Every
 * failure throws, so the step it happens in fails; there is no fallback.
 *
 * - `start`: the submitter UTxOs are the baseline (the first start records
 *   it), no journey daemon runs, the node spawns, then the running daemons
 *   must be exactly its pid and it must answer `/readyz`.
 * - `observe`: the node is alive, `/readyz` is polled until it agrees with
 *   the port's pool snapshot (or the bound passes), and the stderr pool
 *   events and the availability responder reports on stderr and stdout
 *   since the last observation are taken, tied to the pid. An answer whose
 *   L1 follower holds it on an intervention (`l1Source.status` is
 *   `intervention`, such as `rollback_beyond_k`) is recorded, then fails the
 *   observation: no wait clears it, and the node stays up and unready.
 * - `stop`: SIGTERM, exit 0 required; the submitter UTxOs stay unchanged
 *   for `daBondPoolCommitteeStopSettleMs(nodeCadence)` after the exit.
 */
export const createDaBondPoolCommitteeObserver = (deps: {
  readonly spawn: () => Promise<DaBondPoolCommitteeProcess>;
  /** Throws unless the journey daemons are exactly `admitted`. */
  readonly checkDaemons: (admitted: ReadonlySet<number>) => void;
  /** Both submitter addresses' UTxO outrefs. */
  readonly submitterUtxos: () => Promise<readonly string[]>;
  readonly expectedView: () => Promise<
    DaBondPoolCommitteeExpectedView | undefined
  >;
  readonly env: Readonly<Record<string, string>>;
  readonly record?: (entry: DaBondPoolCommitteeRecord) => Promise<void>;
  readonly syncTimeoutMs: number;
  readonly startTimeoutMs: number;
  readonly pollMs: number;
  readonly stopBoundMs: number;
  /**
   * The node's poll interval and the chain's finality cadence. The observer
   * derives from them how long after an exit the submitter addresses are
   * watched (`daBondPoolCommitteeStopSettleMs`). A transaction the node
   * submitted in its last tick can still be in the mempool or unindexed
   * when it exits, and the stop after step 6 has no later read to catch it.
   */
  readonly nodeCadence: Parameters<typeof daBondPoolCommitteeStopSettleMs>[0];
  readonly now?: () => number;
  readonly sleep?: (ms: number) => Promise<unknown>;
}) => {
  const now = deps.now ?? Date.now;
  const sleep = deps.sleep ?? pause;
  const record = deps.record ?? (async () => {});
  const stopSettleMs = daBondPoolCommitteeStopSettleMs(deps.nodeCadence);
  let running:
    | Readonly<{
        node: DaBondPoolCommitteeProcess;
        cursor: ReturnType<typeof createDaBondPoolStderrCursor>;
        responderCursor: ReturnType<typeof createDaBondPoolStderrCursor>;
        stdoutResponderCursor: ReturnType<typeof createDaBondPoolStderrCursor>;
      }>
    | undefined;
  let baseline: readonly string[] | undefined;

  const requireUnchangedUtxos = async (when: string) => {
    const current = await deps.submitterUtxos();
    if (baseline === undefined) {
      baseline = current;
      return;
    }
    const change = daBondPoolSubmitterUtxoChange(baseline, current);
    if (change !== undefined)
      throw new DaBondPoolCommitteeProcessError(
        `The committee node's submitter addresses changed ${when}: ${change}`,
      );
  };

  const admitted = (): ReadonlySet<number> =>
    new Set(running === undefined ? [] : [running.node.pid]);

  const requireAlive = (node: DaBondPoolCommitteeProcess) => {
    if (!node.alive())
      throw new DaBondPoolCommitteeProcessError(
        `The committee node (pid ${node.pid.toString()}) exited; stderr: ${tail(node.stderr())}`,
      );
  };

  return {
    admitted,
    running: () => running !== undefined,

    start: async (): Promise<number> => {
      if (running !== undefined)
        throw new DaBondPoolCommitteeProcessError(
          `The committee node already runs (pid ${running.node.pid.toString()})`,
        );
      await requireUnchangedUtxos("before its start");
      deps.checkDaemons(new Set());
      const node = await deps.spawn();
      running = {
        node,
        cursor: createDaBondPoolStderrCursor(node.pid),
        responderCursor: createDaBondPoolStderrCursor(
          node.pid,
          (event) => event === DA_BOND_POOL_RESPONDER_EVENT,
        ),
        stdoutResponderCursor: createDaBondPoolStderrCursor(
          node.pid,
          (event) => event === DA_BOND_POOL_RESPONDER_EVENT,
        ),
      };
      await record({ kind: "start", pid: node.pid, env: deps.env });
      deps.checkDaemons(admitted());
      const deadline = now() + deps.startTimeoutMs;
      for (;;) {
        requireAlive(node);
        try {
          await node.readyz();
          return node.pid;
        } catch (cause) {
          if (now() >= deadline)
            throw new DaBondPoolCommitteeProcessError(
              `The committee node (pid ${node.pid.toString()}) never answered /readyz`,
              { cause },
            );
        }
        await sleep(deps.pollMs);
      }
    },

    observe: async (): Promise<DaBondPoolCommitteeObservation> => {
      if (running === undefined)
        throw new DaBondPoolCommitteeProcessError(
          "The committee node is not running",
        );
      const { node, cursor, responderCursor, stdoutResponderCursor } = running;
      requireAlive(node);
      const since = now();
      const expected = await deps.expectedView();
      const sync = await awaitDaBondPoolCommitteeSync({
        read: node.readyz,
        alive: node.alive,
        expected,
        since,
        timeoutMs: deps.syncTimeoutMs,
        pollMs: deps.pollMs,
        now,
        sleep,
      });
      requireAlive(node);
      const captured = node.stderr();
      const events = cursor.take(captured);
      const availabilityResponder = [
        ...new Set(
          [
            ...responderCursor.take(captured),
            ...stdoutResponderCursor.take(node.stdout()),
          ].map(({ line }) => line),
        ),
      ];
      const observation: DaBondPoolCommitteeObservation = {
        readinessReasons: sync.readyz.poolReasons,
        events: events.map((event) => event.event),
        process: {
          pid: node.pid,
          readyzHttpStatus: sync.read.httpStatus,
          readyzBody: sync.read.body,
          eventPids: events.map((event) => event.pid),
          availabilityResponder,
        },
        synced: sync.synced,
        fresh: sync.fresh,
      };
      await record({
        kind: "observe",
        pid: node.pid,
        readyzHttpStatus: sync.read.httpStatus,
        readyzBody: sync.read.body,
        events: observation.events,
        availabilityResponder,
        synced: sync.synced,
        fresh: sync.fresh,
        reads: sync.reads,
      });
      const l1Source = sync.readyz.l1Source;
      if (l1Source?.status === "intervention")
        throw new DaBondPoolCommitteeProcessError(
          `The committee node (pid ${node.pid.toString()}) is held on an L1 follower intervention: ${l1Source.intervention ?? "no reason given"}; /readyz: ${sync.read.body}`,
        );
      return observation;
    },

    stop: async (): Promise<DaBondPoolCommitteeExit> => {
      if (running === undefined)
        throw new DaBondPoolCommitteeProcessError(
          "The committee node is not running",
        );
      const { node } = running;
      running = undefined;
      const exit = await node.stop(deps.stopBoundMs);
      await record({ kind: "stop", pid: node.pid, ...exit });
      if (exit.exitCode !== 0 || exit.killed)
        throw new DaBondPoolCommitteeProcessError(
          `The committee node (pid ${node.pid.toString()}) did not exit 0 on SIGTERM: code ${String(exit.exitCode)}, signal ${String(exit.signal)}${exit.killed ? ", killed" : ""}; stderr: ${exit.stderrTail}`,
        );
      const settled = now() + stopSettleMs;
      for (;;) {
        await requireUnchangedUtxos("while it ran");
        if (now() >= settled) break;
        await sleep(deps.pollMs);
      }
      deps.checkDaemons(new Set());
      return exit;
    },

    /** SIGTERM, then SIGKILL after the bound; never throws. */
    teardown: async (): Promise<DaBondPoolCommitteeExit | undefined> => {
      if (running === undefined) return undefined;
      const { node } = running;
      running = undefined;
      try {
        const exit = await node.stop(deps.stopBoundMs);
        await record({ kind: "stop", pid: node.pid, ...exit }).catch(
          () => undefined,
        );
        return exit;
      } catch {
        return undefined;
      }
    },
  };
};
