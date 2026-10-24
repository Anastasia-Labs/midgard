/**
 * Pure operator-directory status queries.
 *
 * Everything here is a synchronous function over an already-fetched directory
 * view: no chain access, no transaction building. Two consumers rely on it:
 *
 *  - the node, to refuse a registration locally (with a message naming the
 *    membership that already exists) before it spends a fee discovering the
 *    same thing on-chain, and
 *  - operator tooling, to report where an operator key stands.
 */
import {
  ActiveOperatorDatum,
  type ActiveOperatorDatum as ActiveOperatorDatumType,
} from "../active-operators.js";
import type { LinkedListNodeView as SDKLinkedListNodeView } from "../linked-list.js";
import {
  RegisteredOperatorDatum,
  type RegisteredOperatorDatum as RegisteredOperatorDatumType,
} from "../registered-operators.js";
import {
  RetiredOperatorDatum,
  type RetiredOperatorDatum as RetiredOperatorDatumType,
} from "../retired-operators.js";
import { castDatum } from "./datum.js";
import {
  nodeKeyHex,
  type OperatorDirectorySnapshot,
  registeredNodeKeyToPosixTime,
  schedulerCurrentOperator,
} from "./directory.js";
import { type NodeWithDatum } from "./layout.js";

const decodeNodeDatumValue = <TDatum>(
  value: unknown,
  schema: unknown,
): TDatum | null => {
  try {
    return castDatum<TDatum>(value, schema);
  } catch {
    return null;
  }
};

// ---------------------------------------------------------------------------
// Directory view
// ---------------------------------------------------------------------------

/**
 * The minimum a caller has to supply to classify an operator key: the three
 * operator linked lists as raw nodes. `OperatorDirectorySnapshot` satisfies
 * this structurally, and so does a plain per-list fetch that never decoded the
 * node data.
 */
export type OperatorDirectoryView = {
  readonly registered: readonly NodeWithDatum[];
  readonly active: readonly NodeWithDatum[];
  readonly retired: readonly NodeWithDatum[];
};

/**
 * The operator a registered-operators node belongs to, or `null` for the root
 * (and for any node whose data does not decode as a registered datum).
 *
 * Registered nodes are keyed by activation time, so this is the only way to
 * find an operator in the registered list.
 */
export const registeredNodeOperator = (
  node: SDKLinkedListNodeView,
): string | null => {
  if (node.key === "Empty") {
    return null;
  }
  const value = node.data;
  if (
    typeof value === "object" &&
    value !== null &&
    "operator" in value &&
    typeof (value as { readonly operator: unknown }).operator === "string"
  ) {
    return (value as { readonly operator: string }).operator;
  }
  return (
    decodeNodeDatumValue<RegisteredOperatorDatumType>(
      value,
      RegisteredOperatorDatum,
    )?.operator ?? null
  );
};

const activeNodeDatum = (
  node: NodeWithDatum,
): ActiveOperatorDatumType | null =>
  node.datum.key === "Empty"
    ? null
    : decodeNodeDatumValue<ActiveOperatorDatumType>(
        node.datum.data,
        ActiveOperatorDatum,
      );

const retiredNodeDatum = (
  node: NodeWithDatum,
): RetiredOperatorDatumType | null =>
  node.datum.key === "Empty"
    ? null
    : decodeNodeDatumValue<RetiredOperatorDatumType>(
        node.datum.data,
        RetiredOperatorDatum,
      );

const nodeHoldsKey = (node: NodeWithDatum, keyHash: string): boolean =>
  nodeKeyHex(node.datum.key) === keyHash;

const nodeLovelace = (node: NodeWithDatum): bigint =>
  node.utxo.assets["lovelace"] ?? 0n;

// ---------------------------------------------------------------------------
// Occupancies and the local duplicate refusal
// ---------------------------------------------------------------------------

export type OperatorDirectoryOccupancyKind =
  | "registered"
  | "active"
  | "retired";

/**
 * One place an operator key occupies in the directory. An honest directory has
 * at most one; more than one means a duplicate registration slipped in and is
 * slashable.
 */
export type OperatorDirectoryOccupancy = {
  readonly kind: OperatorDirectoryOccupancyKind;
  readonly node: NodeWithDatum;
};

/**
 * Every place the operator key occupies, registered first, in list order.
 */
export const findOperatorDirectoryOccupancies = (
  view: OperatorDirectoryView,
  operatorKeyHash: string,
): readonly OperatorDirectoryOccupancy[] => {
  const occupancies: OperatorDirectoryOccupancy[] = [];
  for (const node of view.registered) {
    if (registeredNodeOperator(node.datum) === operatorKeyHash) {
      occupancies.push({ kind: "registered", node });
    }
  }
  for (const node of view.active) {
    if (nodeHoldsKey(node, operatorKeyHash)) {
      occupancies.push({ kind: "active", node });
    }
  }
  for (const node of view.retired) {
    if (nodeHoldsKey(node, operatorKeyHash)) {
      occupancies.push({ kind: "retired", node });
    }
  }
  return occupancies;
};

const describeOccupancy = ({
  kind,
  node,
}: OperatorDirectoryOccupancy): string =>
  `${kind} node ${node.assetName} at ${node.utxo.txHash}#${node.utxo.outputIndex.toString()}`;

/**
 * Thrown by {@link assertOperatorNotInDirectory}. Carries the memberships that
 * blocked the registration so a caller can log or surface them.
 */
export class OperatorAlreadyInDirectoryError extends Error {
  override readonly name = "OperatorAlreadyInDirectoryError";
  readonly operatorKeyHash: string;
  readonly occupancies: readonly OperatorDirectoryOccupancy[];

  constructor(
    operatorKeyHash: string,
    occupancies: readonly OperatorDirectoryOccupancy[],
  ) {
    super(
      `Operator ${operatorKeyHash} is already in the operator directory and cannot register again: ${occupancies
        .map(describeOccupancy)
        .join("; ")}`,
    );
    this.operatorKeyHash = operatorKeyHash;
    this.occupancies = occupancies;
  }
}

/**
 * Refuses a registration for a key that already holds a registered, active, or
 * retired node.
 *
 * On-chain, `RegisterOperator` only proves non-membership of the active and
 * retired lists — it never looks at the registered list — so registering the
 * same key twice is a transaction the ledger accepts and
 * `SlashDuplicateOperator` then punishes. Refusing locally keeps an honest
 * operator from burning its own bond that way.
 */
export const assertOperatorNotInDirectory = (
  view: OperatorDirectoryView,
  operatorKeyHash: string,
): void => {
  const occupancies = findOperatorDirectoryOccupancies(view, operatorKeyHash);
  if (occupancies.length > 0) {
    throw new OperatorAlreadyInDirectoryError(operatorKeyHash, occupancies);
  }
};

// ---------------------------------------------------------------------------
// Operator status
// ---------------------------------------------------------------------------

export type OperatorState = "none" | "registered" | "active" | "retired";

export type OperatorStatusParams = {
  /**
   * `MAX_INACTIVITY_STRIKES` for the deployment. Only ever compared against
   * the strike count read off the node; no threshold is derived here.
   */
  readonly maxInactivityStrikes: bigint;
};

/**
 * Where an operator key stands, as of `nowMs`.
 *
 * Bigint-valued fields are milliseconds since the epoch or lovelace.
 */
export type OperatorStatus = {
  readonly operator: string;
  readonly nowMs: bigint;
  /**
   * The single membership, with `active` > `retired` > `registered` when the
   * directory holds a duplicate.
   */
  readonly state: OperatorState;
  /** True when the key holds more than one directory membership. */
  readonly duplicate: boolean;
  readonly occupancies: readonly OperatorDirectoryOccupancyKind[];
  /** Lovelace locked on the membership node, or `null` when unregistered. */
  readonly bondLovelace: bigint | null;
  /** Strikes on the active node; `null` unless the operator is active. */
  readonly inactivityStrikes: bigint | null;
  /** True when the strike count has reached the deployment's maximum. */
  readonly forcedRetirementEligible: boolean;
  /**
   * The bond hold from the active or retired node datum. `null` means no hold:
   * the bond is recoverable as soon as the operator is retired.
   */
  readonly bondUnlockTime: bigint | null;
  /** Activation time encoded in the registered node key, when registered. */
  readonly registeredActivationTime: bigint | null;
  /** Whether that activation time has passed. */
  readonly activationTimeReached: boolean;
  readonly holdsShift: boolean;
  /** `nowMs - scheduler start_time` when the operator holds the shift. */
  readonly shiftAgeMs: bigint | null;
  /** The operator the scheduler currently names, if any. */
  readonly scheduledOperator: string | null;
  readonly bondRecoveryAllowedNow: boolean;
  /**
   * The earliest `validFrom` a bond-recovery transaction may use, or `null`
   * when recovery is either already allowed or not applicable.
   */
  readonly bondRecoveryAllowedFrom: bigint | null;
};

/**
 * Pure status query over a directory snapshot.
 */
export const deriveOperatorStatus = (
  snapshot: OperatorDirectoryView &
    Pick<OperatorDirectorySnapshot, "scheduler">,
  operatorKeyHash: string,
  nowMs: bigint,
  params: OperatorStatusParams,
): OperatorStatus => {
  const occupancies = findOperatorDirectoryOccupancies(
    snapshot,
    operatorKeyHash,
  );
  const activeOccupancy = occupancies.find(({ kind }) => kind === "active");
  const retiredOccupancy = occupancies.find(({ kind }) => kind === "retired");
  const registeredOccupancy = occupancies.find(
    ({ kind }) => kind === "registered",
  );

  const state: OperatorState =
    activeOccupancy !== undefined
      ? "active"
      : retiredOccupancy !== undefined
        ? "retired"
        : registeredOccupancy !== undefined
          ? "registered"
          : "none";

  const activeDatum =
    activeOccupancy === undefined
      ? null
      : activeNodeDatum(activeOccupancy.node);
  const retiredDatum =
    retiredOccupancy === undefined
      ? null
      : retiredNodeDatum(retiredOccupancy.node);

  const bondUnlockTime =
    activeDatum?.bond_unlock_time ?? retiredDatum?.bond_unlock_time ?? null;
  const inactivityStrikes = activeDatum?.inactivity_strikes ?? null;

  const bondNode = (activeOccupancy ?? retiredOccupancy ?? registeredOccupancy)
    ?.node;

  const registeredActivationTime =
    registeredOccupancy === undefined
      ? null
      : (registeredNodeKeyToPosixTime(registeredOccupancy.node.datum.key) ??
        null);

  const scheduled = schedulerCurrentOperator(snapshot.scheduler);
  const holdsShift = scheduled?.operator === operatorKeyHash;

  // The on-chain recovery check is `is_entirely_after(bond_unlock_time)` with
  // an inclusive lower bound, which is strict: the range must start after the
  // hold, not at it.
  const bondRecoveryApplicable = retiredOccupancy !== undefined;
  const bondRecoveryAllowedNow =
    bondRecoveryApplicable &&
    (retiredDatum?.bond_unlock_time == null ||
      nowMs > retiredDatum.bond_unlock_time);

  return {
    operator: operatorKeyHash,
    nowMs,
    state,
    duplicate: occupancies.length > 1,
    occupancies: occupancies.map(({ kind }) => kind),
    bondLovelace: bondNode === undefined ? null : nodeLovelace(bondNode),
    inactivityStrikes,
    forcedRetirementEligible:
      inactivityStrikes !== null &&
      inactivityStrikes >= params.maxInactivityStrikes,
    bondUnlockTime,
    registeredActivationTime,
    activationTimeReached:
      registeredActivationTime !== null && nowMs >= registeredActivationTime,
    holdsShift,
    shiftAgeMs:
      holdsShift && scheduled !== null ? nowMs - scheduled.startTime : null,
    scheduledOperator: scheduled?.operator ?? null,
    bondRecoveryAllowedNow,
    bondRecoveryAllowedFrom:
      !bondRecoveryApplicable ||
      bondRecoveryAllowedNow ||
      retiredDatum?.bond_unlock_time == null
        ? null
        : retiredDatum.bond_unlock_time + 1n,
  };
};
