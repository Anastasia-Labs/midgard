/**
 * What a node §8.4 family predicate reads (`l1-follower.intent-predicates.ts`):
 * the store transaction at the follower's current view, the intent's
 * derived state, and the landed state queue and operator set read lazily in
 * that transaction; with the small helpers the families share.
 */
import {
  type Dialect,
  type FactStore,
  type IntentState,
  liveUtxosIn,
  type OutRef,
  type SqlTx,
  type View,
} from "@al-ft/midgard-l1-follower";

import type {
  OperatorSet,
  OperatorSetConfig,
} from "../l1-operator-set/index.js";
import type {
  LandedStateQueue,
  StateQueueProjectionConfig,
} from "../l1-state-queue/index.js";

export type NodeFamilyPredicateDeps = Readonly<{
  store: Pick<FactStore, "dialect" | "transaction">;
  stateQueue: StateQueueProjectionConfig;
  /** The operator set and this operator's key hash; null when the key is unreadable. */
  operatorSet: Readonly<{ config: OperatorSetConfig; ownKey: string }> | null;
  /** POSIX milliseconds at the start of a slot (the node's slot clock). */
  slotToPosixMs: (slot: number) => number;
  /** The commit-event depth d: a commit waits until the tip is d blocks above its anchor. */
  commitEventDepth: number;
}>;

export type Read = Readonly<{
  tx: SqlTx;
  dialect: Dialect;
  view: View;
  state: IntentState;
  deps: NodeFamilyPredicateDeps;
  queue: () => Promise<LandedStateQueue>;
  operators: () => Promise<OperatorSet>;
}>;

export const outRefText = (outRef: OutRef): string =>
  `${outRef.txHash.toString("hex")}#${outRef.index.toString()}`;

export const spends = (state: IntentState, outRef: string): boolean =>
  state.intent.inputs.some((input) => outRefText(input) === outRef);

/** The text after `prefix` in the workflow key; a key of another shape throws. */
export const keyRest = (state: IntentState, prefix: string): string => {
  const { workflowKey } = state.intent;
  if (!workflowKey.startsWith(prefix))
    throw new Error(
      `${state.intent.family} intent key ${workflowKey} lacks ${prefix}`,
    );
  return workflowKey.slice(prefix.length);
};

export const contentRefHex = (state: IntentState): string => {
  if (state.intent.contentRef === null)
    throw new Error(
      `${state.intent.family} intent ${state.intent.workflowKey} has no content reference`,
    );
  return state.intent.contentRef.toString("hex");
};

export const count = async (
  tx: SqlTx,
  sql: string,
  params: readonly (Buffer | number | string)[],
): Promise<number> => {
  const rows = await tx.query(sql, [...params]);
  return Number(rows[0]?.n ?? 0);
};

/** The time the intent's validity starts at, or the tip's when it has no lower bound. */
export const validFromMs = (read: Read): number =>
  read.deps.slotToPosixMs(
    read.state.intent.validFromSlot ?? read.view.point.slot,
  );

/** Every input of the intent is still a live fact. */
export const allInputsLive = async (read: Read): Promise<boolean> => {
  const live = await liveUtxosIn(read.tx, read.dialect, {
    by: "outref",
    outRefs: read.state.intent.inputs,
  });
  if (live.kind !== "ok") throw new Error(`intent inputs: ${live.kind}`);
  return live.utxos.length === read.state.intent.inputs.length;
};
