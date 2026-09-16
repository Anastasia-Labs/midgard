/**
 * One consistent read of the operator directory: the three operator linked
 * lists (registered, active, retired), the scheduler, the hub oracle, and the
 * state-queue tail. Every operator-lifecycle builder and status query derives
 * its witnesses from this snapshot so that they all agree on the same chain
 * view.
 */
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  ActiveOperatorDatum,
  type ActiveOperatorDatum as ActiveOperatorDatumType,
} from "../active-operators.js";
import { type MidgardValidators, utxosAtByNFTPolicyId } from "../common.js";
import {
  DataCoercionError,
  type LucidError,
  type MissingDatumError,
} from "../errors.js";
import {
  fetchHubOracleUTxOProgram,
  type HubOracleError,
  type HubOracleUTxO,
} from "../hub-oracle.js";
import {
  ConfirmedState,
  getHeaderFromStateQueueDatum,
} from "../ledger-state.js";
import {
  getLinkedListNodeViewFromUTxO,
  type LinkedListNodeView,
  type NodeKey,
} from "../linked-list.js";
import {
  RegisteredOperatorDatum,
  type RegisteredOperatorDatum as RegisteredOperatorDatumType,
} from "../registered-operators.js";
import {
  RetiredOperatorDatum,
  type RetiredOperatorDatum as RetiredOperatorDatumType,
} from "../retired-operators.js";
import {
  fetchSchedulerUTxOProgram,
  type SchedulerError,
  type SchedulerUTxO,
} from "../scheduler.js";
import {
  fetchLatestCommittedBlockProgram,
  type StateQueueError,
  type StateQueueUTxO,
} from "../state-queue.js";
import { castDatum } from "./datum.js";
import type { NodeWithDatum } from "./layout.js";

/**
 * A registered-operators list node. `registered` is `null` for the root.
 */
export type RegisteredOperatorNode = NodeWithDatum & {
  readonly registered: RegisteredOperatorDatumType | null;
};

/**
 * An active-operators list node. `active` is `null` for the root.
 */
export type ActiveOperatorNode = NodeWithDatum & {
  readonly active: ActiveOperatorDatumType | null;
};

/**
 * A retired-operators list node. `retired` is `null` for the root.
 */
export type RetiredOperatorNode = NodeWithDatum & {
  readonly retired: RetiredOperatorDatumType | null;
};

export type StateQueueTail = {
  readonly utxo: UTxO;
  readonly datum: LinkedListNodeView;
  /** `end_time` of the latest committed block (or of the confirmed state when the queue is empty). */
  readonly endTime: bigint;
  /** Whether the tail is the root, i.e. no block is currently committed. */
  readonly isRoot: boolean;
};

export type OperatorDirectorySnapshot = {
  readonly registered: readonly RegisteredOperatorNode[];
  readonly active: readonly ActiveOperatorNode[];
  readonly retired: readonly RetiredOperatorNode[];
  readonly scheduler: SchedulerUTxO;
  readonly hubOracle: HubOracleUTxO;
  readonly stateQueueTail: StateQueueTail;
};

export type OperatorDirectorySnapshotError =
  | LucidError
  | DataCoercionError
  | MissingDatumError
  | SchedulerError
  | HubOracleError
  | StateQueueError;

export type OperatorDirectoryValidators = Pick<
  MidgardValidators,
  | "registeredOperators"
  | "activeOperators"
  | "retiredOperators"
  | "scheduler"
  | "hubOracle"
  | "stateQueue"
>;

/**
 * Where an operator key currently lives in the directory.
 */
const decodeNodeData = <TDatum>(
  view: LinkedListNodeView,
  schema: unknown,
  label: string,
): Effect.Effect<TDatum | null, DataCoercionError> =>
  view.key === "Empty"
    ? Effect.succeed(null)
    : Effect.try({
        try: () => castDatum<TDatum>(view.data, schema),
        catch: (cause) =>
          new DataCoercionError({
            message: `Failed decoding a ${label} node datum`,
            cause,
          }),
      });

const fetchListNodes = <TDatum>(
  lucid: LucidEvolution,
  address: string,
  policyId: string,
  schema: unknown,
  label: string,
): Effect.Effect<
  readonly (NodeWithDatum & { readonly decoded: TDatum | null })[],
  LucidError | DataCoercionError | MissingDatumError
> =>
  Effect.gen(function* () {
    const beacons = yield* utxosAtByNFTPolicyId(lucid, address, policyId);
    return yield* Effect.all(
      beacons.map(({ utxo, assetName }) =>
        Effect.gen(function* () {
          const datum = yield* getLinkedListNodeViewFromUTxO(utxo);
          const decoded = yield* decodeNodeData<TDatum>(datum, schema, label);
          return { utxo, datum, assetName, decoded };
        }),
      ),
    );
  });

const decodeStateQueueTail = (
  tail: StateQueueUTxO,
): Effect.Effect<StateQueueTail, DataCoercionError> =>
  Effect.gen(function* () {
    if (tail.datum.key === "Empty") {
      const confirmed = yield* Effect.try({
        try: () => Data.castFrom(tail.datum.data as never, ConfirmedState),
        catch: (cause) =>
          new DataCoercionError({
            message: "Failed decoding the state-queue root confirmed state",
            cause,
          }),
      });
      return {
        utxo: tail.utxo,
        datum: tail.datum,
        endTime: confirmed.endTime,
        isRoot: true,
      };
    }
    const header = yield* getHeaderFromStateQueueDatum(tail.datum);
    return {
      utxo: tail.utxo,
      datum: tail.datum,
      endTime: header.endTime,
      isRoot: false,
    };
  });

/**
 * Fetches the whole operator directory in one pass.
 */
export const fetchOperatorDirectorySnapshotProgram = (
  lucid: LucidEvolution,
  validators: OperatorDirectoryValidators,
): Effect.Effect<OperatorDirectorySnapshot, OperatorDirectorySnapshotError> =>
  Effect.gen(function* () {
    const [registeredRaw, activeRaw, retiredRaw, scheduler, hubOracle, tail] =
      yield* Effect.all(
        [
          fetchListNodes<RegisteredOperatorDatumType>(
            lucid,
            validators.registeredOperators.spendingScriptAddress,
            validators.registeredOperators.policyId,
            RegisteredOperatorDatum,
            "registered operator",
          ),
          fetchListNodes<ActiveOperatorDatumType>(
            lucid,
            validators.activeOperators.spendingScriptAddress,
            validators.activeOperators.policyId,
            ActiveOperatorDatum,
            "active operator",
          ),
          fetchListNodes<RetiredOperatorDatumType>(
            lucid,
            validators.retiredOperators.spendingScriptAddress,
            validators.retiredOperators.policyId,
            RetiredOperatorDatum,
            "retired operator",
          ),
          fetchSchedulerUTxOProgram(lucid, {
            schedulerAddress: validators.scheduler.spendingScriptAddress,
            schedulerPolicyId: validators.scheduler.policyId,
          }),
          fetchHubOracleUTxOProgram(lucid, {
            hubOracleAddress: validators.hubOracle.spendingScriptAddress,
            hubOraclePolicyId: validators.hubOracle.policyId,
          }),
          fetchLatestCommittedBlockProgram(lucid, {
            stateQueueAddress: validators.stateQueue.spendingScriptAddress,
            stateQueuePolicyId: validators.stateQueue.policyId,
          }),
        ],
        { concurrency: "unbounded" },
      );
    const stateQueueTail = yield* decodeStateQueueTail(tail);
    return {
      registered: registeredRaw.map(({ decoded, ...node }) => ({
        ...node,
        registered: decoded,
      })),
      active: activeRaw.map(({ decoded, ...node }) => ({
        ...node,
        active: decoded,
      })),
      retired: retiredRaw.map(({ decoded, ...node }) => ({
        ...node,
        retired: decoded,
      })),
      scheduler,
      hubOracle,
      stateQueueTail,
    };
  });

// ---------------------------------------------------------------------------
// Node-key helpers
// ---------------------------------------------------------------------------

/**
 * Encodes a POSIX time (ms) as the big-endian byte key used by the
 * registered-operators list.
 */
export const posixTimeToRegisteredNodeKey = (posixTime: bigint): string => {
  if (posixTime < 0n) {
    throw new Error("Registered-operator activation time cannot be negative");
  }
  const hex = posixTime.toString(16);
  return hex.length % 2 === 0 ? hex : `0${hex}`;
};

/**
 * Decodes a registered-operators node key back to its activation POSIX time.
 * Returns `undefined` for the root.
 */
export const registeredNodeKeyToPosixTime = (
  key: NodeKey,
): bigint | undefined => {
  if (key === "Empty") {
    return undefined;
  }
  return key.Key.key.length === 0 ? 0n : BigInt(`0x${key.Key.key}`);
};

export const nodeKeyHex = (key: NodeKey): string | null =>
  key === "Empty" ? null : key.Key.key;

// ---------------------------------------------------------------------------
// List traversal helpers (generic over the three node kinds)
// ---------------------------------------------------------------------------

/**
 * Returns the root node (key `Empty`) of a list, if present.
 */
export const findRootNode = <TNode extends NodeWithDatum>(
  nodes: readonly TNode[],
): TNode | undefined => nodes.find((node) => node.datum.key === "Empty");

/**
 * Returns the tail node (link `Empty`) of a list, if present. For a list
 * holding only the root, the root is the tail.
 */
export const findTailNode = <TNode extends NodeWithDatum>(
  nodes: readonly TNode[],
): TNode | undefined => nodes.find((node) => node.datum.next === "Empty");

/**
 * Returns the node whose own key equals `keyHex`.
 */
export const findNodeByKey = <TNode extends NodeWithDatum>(
  nodes: readonly TNode[],
  keyHex: string,
): TNode | undefined =>
  nodes.find(
    (node) => node.datum.key !== "Empty" && node.datum.key.Key.key === keyHex,
  );

/**
 * Returns the node whose `next` link points at `keyHex` (the removal anchor
 * for that key).
 */
export const findAnchorNodeForKey = <TNode extends NodeWithDatum>(
  nodes: readonly TNode[],
  keyHex: string,
): TNode | undefined =>
  nodes.find(
    (node) => node.datum.next !== "Empty" && node.datum.next.Key.key === keyHex,
  );

/**
 * The operator currently holding the shift according to the scheduler, or
 * `null` when the scheduler says there are no active operators.
 */
export const schedulerCurrentOperator = (
  scheduler: SchedulerUTxO,
): { readonly operator: string; readonly startTime: bigint } | null =>
  scheduler.datum === "NoActiveOperators"
    ? null
    : {
        operator: scheduler.datum.ActiveOperator.operator,
        startTime: scheduler.datum.ActiveOperator.start_time,
      };
