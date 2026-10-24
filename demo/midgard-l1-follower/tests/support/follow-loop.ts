import {
  type ChainSyncEvent,
  type ChainSyncStream,
  IntersectNotFoundError,
  type TransportReadiness,
  TransportUnavailableError,
} from "@al-ft/l1-node-transport";

import {
  type FactStore,
  followChain,
  type FollowChainOptions,
  type FollowStatus,
  type OriginConfig,
  type OutRef,
} from "../../src/index.js";
import { SIM_ORIGIN } from "../../src/testing/index.js";

/** An outref no scenario spends: the protocol-init spend never comes (R3). */
export const UNSPENT: OutRef = { txHash: Buffer.alloc(32, 0xee), index: 0 };

export const simOrigin = (
  hubOracleOneShot: OutRef = UNSPENT,
): OriginConfig => ({
  origin: SIM_ORIGIN.point,
  hubOracleOneShot,
});

export type Script = {
  events: readonly ChainSyncEvent[];
  /** Events acknowledged so far: every stream resumes after them. */
  acked: number;
  opens: number;
  /** Only events below this index are served; later `next` calls wait. */
  limit?: number;
  /** `opened` rejects with this on the matching open (0-based). */
  intersectNotFound?: Readonly<{ open: number; resuming: boolean }>;
  /** `next` throws when about to serve this index (`failTimes` times, default once). */
  failAt?: number;
  failTimes?: number;
  /** What `next` throws there: by default the transport going away. */
  failWith?: () => Error;
};

export const script = (
  events: readonly ChainSyncEvent[],
  fields: Partial<Omit<Script, "events">> = {},
): Script => ({ events, acked: 0, opens: 0, ...fields });

const pause = (ms: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, ms));

/**
 * A chain-sync transport serving the script's events in order. Every stream
 * starts after the last acknowledged event, like a node intersecting at the
 * follower's cursor, so an event the follower failed to apply is served
 * again on the next stream.
 */
export const scriptedTransport = (
  s: Script,
  /** The node is reachable unless a test says otherwise. */
  readiness: TransportReadiness = { ready: true, nodeToClientVersion: 32784 },
) => ({
  readiness,
  onReadiness: (): (() => void) => () => undefined,
  openChainSync: (): ChainSyncStream => {
    const open = s.opens;
    s.opens += 1;
    let position = s.acked;
    let closed = false;
    const opened =
      s.intersectNotFound?.open === open
        ? Promise.reject(
            new IntersectNotFoundError(
              s.events[s.events.length - 1]!.tip,
              s.intersectNotFound.resuming,
            ),
          )
        : Promise.resolve();
    opened.catch(() => undefined);
    return {
      opened,
      next: async () => {
        for (;;) {
          if (closed) return undefined;
          if (s.failAt === position) {
            s.failTimes = (s.failTimes ?? 1) - 1;
            if (s.failTimes <= 0) s.failAt = undefined;
            throw (
              s.failWith?.() ??
              new TransportUnavailableError(
                "sidecar_restarting",
                "the transport sidecar went away",
              )
            );
          }
          if (position < Math.min(s.limit ?? Infinity, s.events.length)) {
            position += 1;
            return s.events[position - 1];
          }
          await pause(2);
        }
      },
      ack: (seq: bigint) => {
        const index = s.events.findIndex((event) => event.seq === seq);
        if (index >= 0) s.acked = Math.max(s.acked, index + 1);
      },
      close: () => {
        closed = true;
        return Promise.resolve();
      },
    } as unknown as ChainSyncStream;
  },
});

export type FollowRun = Readonly<{
  final: FollowStatus;
  statuses: readonly FollowStatus[];
  log: readonly string[];
}>;

/**
 * Runs `followChain` until `until` holds of a status (or it returns by
 * itself), then aborts it and returns every status it reported.
 */
export const follow = async (
  options: Readonly<{
    store: FactStore;
    script: Script;
    origin?: OriginConfig;
    until: (status: FollowStatus) => boolean;
    timeoutMs?: number;
    /** The transport's readiness for the whole run (default: ready). */
    readiness?: TransportReadiness;
  }> &
    Partial<
      Pick<
        FollowChainOptions,
        | "stuckAfter"
        | "prune"
        | "onStatus"
        | "nodeBehind"
        | "transientBudgetMs"
        | "now"
      >
    >,
): Promise<FollowRun> => {
  const abort = new AbortController();
  const statuses: FollowStatus[] = [];
  const log: string[] = [];
  let returned = false;
  let reached = false;
  const running = followChain({
    store: options.store,
    transport: scriptedTransport(options.script, options.readiness),
    origin: options.origin ?? simOrigin(),
    signal: abort.signal,
    backoffMs: { initial: 1, max: 4 },
    log: (line) => log.push(line),
    ...(options.stuckAfter === undefined
      ? {}
      : { stuckAfter: options.stuckAfter }),
    ...(options.prune === undefined ? {} : { prune: options.prune }),
    ...(options.transientBudgetMs === undefined
      ? {}
      : { transientBudgetMs: options.transientBudgetMs }),
    ...(options.now === undefined ? {} : { now: options.now }),
    ...(options.nodeBehind === undefined
      ? {}
      : { nodeBehind: options.nodeBehind }),
    onStatus: async (status) => {
      statuses.push(status);
      if (options.until(status)) reached = true;
      await options.onStatus?.(status);
    },
  }).finally(() => {
    returned = true;
  });
  const deadline = Date.now() + (options.timeoutMs ?? 8_000);
  while (!returned && !reached) {
    if (Date.now() > deadline)
      throw new Error(
        `timed out; last status ${JSON.stringify(statuses[statuses.length - 1])} log ${JSON.stringify(log.slice(-5))}`,
      );
    await pause(2);
  }
  abort.abort();
  const final = await running;
  return { final, statuses, log };
};

/** Whether the loop applied every scripted event. */
export const appliedAll =
  (s: Script) =>
  (status: FollowStatus): boolean =>
    status.events >= s.events.length && status.state === "following";
