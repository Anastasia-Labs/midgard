import { rename, writeFile } from "node:fs/promises";

import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  type FactStore,
  followChain,
  type FollowStatus,
  type InterventionReason,
  type OriginConfig,
} from "@al-ft/midgard-l1-follower";

/**
 * The committee follower's shadow status (phase A), written to `statusPath`.
 * Interventions are recorded here and nowhere else: in phase A they never
 * touch `/readyz`, and the current L1 code stays authoritative.
 */
export type ShadowFollowerStatus = Readonly<{
  schema: "committee-l1-follower-shadow-v1";
  /**
   * `following`: applying chain-sync events. `waiting`: a transient
   * condition (the writer lease, a stream failure) it backs off from.
   * `intervention`: stopped on a condition only an operator clears; the
   * process stays up. `stopped`: asked to stop.
   */
  state: "starting" | "following" | "waiting" | "intervention" | "stopped";
  /** The interventions in force (R1 to R5, `origin_mismatch`). */
  interventions: readonly Readonly<{
    reason: InterventionReason;
    detail: string;
  }>[];
  /** Whether the protocol-init tx (the hubOracleOneShot spend) is in the facts. */
  protocolInit: "seen" | "pending" | "unknown";
  cursor: Readonly<{ slot: number; height: number; generation: number }> | null;
  /** Events applied by this process. */
  events: number;
  /** The latest transient failure, cleared by the next applied event. */
  lastError: string | null;
  updatedAt: string;
}>;

export type ShadowFollowerOptions = Readonly<{
  store: FactStore;
  transport: Pick<L1NodeTransport, "openChainSync">;
  origin: OriginConfig;
  /** Written atomically (temp file and rename) on every status change. */
  statusPath: string;
  signal: AbortSignal;
  /** The chain-sync stream's credit (default 64). */
  credit?: number;
  backoffMs?: Readonly<{ initial: number; max: number }>;
  /** Stamps the status file only; no L1 decision reads it. */
  now?: () => Date;
  log?: (line: string) => void;
}>;

const message = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);

/**
 * Follows the chain into the store with the shared loop (`followChain`),
 * keeping the committee projections current, and records its status in the
 * status file. A change of anything but the cursor and the event count is
 * written at once; progress alone is written at the tip, or every 100
 * events while catching up. Never throws.
 */
export const runShadowFollower = async (
  options: ShadowFollowerOptions,
): Promise<void> => {
  const now = options.now ?? (() => new Date());
  const log = options.log ?? (() => undefined);
  let writtenShape = "";
  let writtenEvents = -1;
  const write = async (followed: FollowStatus): Promise<void> => {
    const status: Omit<ShadowFollowerStatus, "updatedAt"> = {
      schema: "committee-l1-follower-shadow-v1",
      state: followed.state,
      interventions: followed.interventions,
      protocolInit: followed.protocolInit,
      cursor: followed.cursor,
      events: followed.events,
      lastError: followed.lastError,
    };
    const shape = JSON.stringify({ ...status, cursor: null, events: null });
    if (shape === writtenShape && status.events === writtenEvents) return;
    if (
      shape === writtenShape &&
      !followed.atTip &&
      status.events - writtenEvents < 100
    )
      return;
    writtenShape = shape;
    writtenEvents = status.events;
    try {
      await writeFile(
        `${options.statusPath}.tmp`,
        JSON.stringify({ ...status, updatedAt: now().toISOString() }),
      );
      await rename(`${options.statusPath}.tmp`, options.statusPath);
    } catch (error) {
      log(`shadow status not written: ${message(error)}`);
    }
  };
  await followChain({
    store: options.store,
    transport: options.transport,
    origin: options.origin,
    signal: options.signal,
    credit: options.credit,
    backoffMs: options.backoffMs,
    log,
    onStatus: write,
  });
};
