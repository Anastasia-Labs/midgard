import {
  chainPoint,
  type ChainSyncStream,
  IntersectNotFoundError,
  type L1NodeTransport,
  type RollForward,
} from "@al-ft/l1-node-transport";

import type { FactStore, StoreError } from "./store/fact-store.js";
import type {
  Cursor,
  Intervention,
  OutRef,
  Point,
  StoreLocked,
} from "./types.js";

/**
 * The origin point O and its two assertions (l1-architecture-plan §5.3,
 * §7.5): R4 `origin_not_on_chain` on a fresh start whose `FindIntersect([O])`
 * fails, and R3 `origin_after_protocol_init` once the cursor reaches the tip
 * without the tx that spent the manifest's `hubOracleOneShot` outref.
 *
 * Every function here returns an `Intervention` instead of throwing, so the
 * role keeps its process up and fails `/readyz` with the reason.
 */

/** The configured origin and the outref whose spend proves O precedes the protocol. */
export type OriginConfig = Readonly<{
  /** O: the point immediately before the `prepareHubOracleNonce` block. */
  origin: Point;
  /** The manifest's `hubOracleOneShot` outref; the protocol-init tx spends it. */
  hubOracleOneShot: OutRef;
}>;

const intervention = (
  reason: Intervention["reason"],
  detail: string,
): Intervention => ({ kind: "intervention", reason, detail });

const asError = (error: unknown): Error =>
  error instanceof Error ? error : new Error(String(error));

const describePoint = (point: Point): string =>
  `${point.slot}.${point.hash.toString("hex")}`;

/**
 * Classifies a `FindIntersect` that matched none of the offered points. On a
 * fresh start (no cursor) the only point offered is O, so O is not on the
 * node's chain: R4. A resumed store offered its own history: R2.
 */
export const intersectionFailure = (
  error: IntersectNotFoundError,
  cursor: Cursor | null,
  origin: Point,
): Intervention => {
  const tip =
    error.tip.point.kind === "origin"
      ? "the genesis"
      : `${error.tip.point.slot}.${error.tip.point.hash}`;
  return cursor === null && !error.resuming
    ? intervention(
        "origin_not_on_chain",
        `l1Origin ${describePoint(origin)} is not on the node's chain (node tip ${tip}); correct l1Origin or the network`,
      )
    : intervention(
        "intersection_outside_history",
        `the node has none of the follower's intersection points, origin ${describePoint(origin)} included (node tip ${tip})`,
      );
};

/**
 * The origin row's height, from the first block after O: its parent must be
 * O, and O's height is one below it. A first block that does not extend O
 * means O is not on the chain the node serves: R4.
 */
export const originAnchor = (
  origin: Point,
  first: Pick<RollForward, "point" | "blockNo" | "prevHash">,
): Readonly<{ point: Point; height: number }> | Intervention => {
  if (first.prevHash !== origin.hash.toString("hex") || first.blockNo < 1n)
    return intervention(
      "origin_not_on_chain",
      `the block after l1Origin ${describePoint(origin)} is ${first.point.slot}.${first.point.hash} with parent ${first.prevHash ?? "none"}, not the origin`,
    );
  return { point: origin, height: Number(first.blockNo - 1n) };
};

export type OriginStart =
  | Readonly<{
      kind: "initialized" | "already_initialized";
      cursor: Cursor;
      /** The stream, opened at O: the caller applies `first`, acks it and follows on. */
      stream: ChainSyncStream;
      first: RollForward;
    }>
  /** The store already holds a cursor at this origin: resume from its own points. */
  | Readonly<{ kind: "resume"; cursor: Cursor }>
  | Intervention
  | StoreError
  | StoreLocked;

export type OriginStartOptions = Readonly<{
  store: FactStore;
  transport: Pick<L1NodeTransport, "openChainSync">;
  origin: Point;
  /** The stream's credit (see `ChainSyncOptions.credit`). */
  credit: ChainSyncStream["options"]["credit"];
}>;

/**
 * The fresh start of §5.3 step 2 on a store with no cursor: `FindIntersect`
 * at O alone, then the first block after O fixes O's height and initializes
 * the store. A store that already has a cursor is not touched; the caller
 * resumes from its own points (and checks the origin with `originMatches`).
 */
export const startFromOrigin = async (
  options: OriginStartOptions,
): Promise<OriginStart> => {
  const { store, origin } = options;
  let cursor: Cursor | null;
  try {
    cursor = await store.cursor();
  } catch (error) {
    return { kind: "error", error: asError(error) };
  }
  if (cursor !== null)
    return originMatches(cursor, origin)
      ? { kind: "resume", cursor }
      : originMismatch(cursor, origin);
  const stream = options.transport.openChainSync({
    points: [chainPoint(BigInt(origin.slot), origin.hash.toString("hex"))],
    credit: options.credit,
  });
  const close = async <T>(result: T): Promise<T> => {
    await stream.close();
    return result;
  };
  try {
    await stream.opened;
    let first: RollForward | undefined;
    while (first === undefined) {
      const event = await stream.next();
      if (event === undefined)
        return await close({
          kind: "error",
          error: new Error(
            "the chain-sync stream ended before a block after the origin",
          ),
        } as const);
      // A roll-backward to O itself only restates the intersection.
      if (event.kind === "roll_backward") {
        stream.ack(event.seq);
        continue;
      }
      first = event;
    }
    const anchor = originAnchor(origin, first);
    if ("kind" in anchor) return await close(anchor);
    const initialized = await store.initialize(anchor);
    if (initialized.kind === "error" || initialized.kind === "store_locked")
      return await close(initialized);
    if (initialized.kind === "origin_mismatch")
      return await close(originMismatch(initialized.cursor, origin));
    return { ...initialized, stream, first };
  } catch (error) {
    if (error instanceof IntersectNotFoundError)
      return await close(intersectionFailure(error, null, origin));
    return await close({ kind: "error", error: asError(error) } as const);
  }
};

/**
 * The store was initialized at another origin than the configured one, for
 * example after an operator corrected l1Origin to clear R3. The store is
 * never reset silently: the role stays unready until the operator runs the
 * follower reset or restores the old origin.
 */
const originMismatch = (cursor: Cursor, origin: Point): Intervention =>
  intervention(
    "origin_mismatch",
    `the store was initialized at l1Origin ${describePoint(cursor.origin)}, but the configured l1Origin is ${describePoint(origin)}; reset the follower store to the configured origin, or restore the old l1Origin`,
  );

/** Whether the store was initialized at the configured origin. */
export const originMatches = (cursor: Cursor, origin: Point): boolean =>
  cursor.origin.slot === origin.slot && cursor.origin.hash.equals(origin.hash);

export type ProtocolInitStatus =
  | Readonly<{ kind: "seen"; txHash: Buffer; slot: number }>
  | Readonly<{ kind: "pending"; detail: string }>
  | Intervention
  | StoreError;

/**
 * The completeness assertion of §5.3 step 3. `seen` once the store holds the
 * protocol-init fact: a valid tx spent the `hubOracleOneShot` outref in an
 * applied block (`FactStore.watchProtocolInit`). Until the cursor reaches
 * the node tip the answer is `pending`; at the tip without the fact it is
 * R3. It is not sticky: a later call that finds the fact answers `seen`.
 *
 * The fact is class A and outlives the init tx: the tx row is pruned once
 * the outputs it created are spent and k deep, but prune never removes the
 * fact. A rewind below the init block removes it, and the block landing
 * again records it again.
 */
export const protocolInitStatus = async (
  store: Pick<FactStore, "cursor" | "protocolInit">,
  config: OriginConfig,
  tip: Point,
): Promise<ProtocolInitStatus> => {
  try {
    const spend = await store.protocolInit(config.hubOracleOneShot);
    if (spend !== null) return { kind: "seen", ...spend };
    const cursor = await store.cursor();
    if (cursor === null)
      return { kind: "pending", detail: "the store is not initialized" };
    if (cursor.point.slot !== tip.slot || !cursor.point.hash.equals(tip.hash))
      return {
        kind: "pending",
        detail: `catching up: cursor ${describePoint(cursor.point)}, node tip ${describePoint(tip)}`,
      };
    const outRef = `${config.hubOracleOneShot.txHash.toString("hex")}#${config.hubOracleOneShot.index}`;
    return intervention(
      "origin_after_protocol_init",
      `caught up at ${describePoint(cursor.point)} from l1Origin ${describePoint(cursor.origin)} without seeing the tx that spends hubOracleOneShot ${outRef}; correct l1Origin to a point before the prepareHubOracleNonce block`,
    );
  } catch (error) {
    return { kind: "error", error: asError(error) };
  }
};
