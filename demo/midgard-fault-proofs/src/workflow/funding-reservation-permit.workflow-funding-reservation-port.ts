import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { type TxSigned, type UTxO } from "@lucid-evolution/lucid";

import { type WorkflowActuationPermit } from "./actuation-permit.js";
import {
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
} from "./journal.js";
import { type WorkflowRuntimeFundingPolicy } from "./runtime-funding-policy.js";

export const WORKFLOW_FUNDING_RESERVATION_PERMIT =
  "midgard-production-workflow-funding-reservation-permit-v1" as const;

export const DIGEST = /^[0-9a-f]{64}$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const ACTION_KIND = /^[a-z][a-zA-Z0-9_.:-]{0,127}$/u;

export type WorkflowFundingReservedInput = Readonly<{
  outRef: string;
  role: "funding" | "collateral";
  lovelace: string;
  assets: readonly Readonly<{ unit: string; quantity: string }>[];
}>;

export type WorkflowFundingReservationSnapshot = Readonly<{
  reservationId: string;
  deploymentFingerprint: string;
  decisionDigest: string;
  policyDigest: string;
  reservationBasisDigest: string;
  rollbackGeneration: string;
  revision: string;
  walletAddress: string;
  fundingPaymentKeyHash: string;
  state: "active" | "released" | "conflict";
  activeInputs: readonly WorkflowFundingReservedInput[];
}>;

export type WorkflowFundingPreparedTransition = Readonly<{
  actionKind: string;
  signedTransactionCborHex: string;
  transactionHash: string;
  transactionBodySha256: string;
  consumedOutRefs: readonly string[];
  producedInputs: readonly WorkflowFundingReservedInput[];
}>;

export type WorkflowFundingJournalHandoff = Readonly<{
  workflowId: string;
  identity: FraudProofWorkflowIdentity;
  preparedArtifactDigest: string;
  expectedJournalSequence: number;
}>;

export type WorkflowFundingSubmissionHandoff = WorkflowFundingJournalHandoff &
  Readonly<{
    preflight: Extract<
      FraudProofWorkflowJournalEvent,
      { kind: "preflight_passed" }
    >;
    submissionIntent: Extract<
      FraudProofWorkflowJournalEvent,
      { kind: "submission_intent" }
    >;
  }>;

export type WorkflowFundingCompletionHandoff = WorkflowFundingJournalHandoff &
  Readonly<{
    completion: Extract<
      FraudProofWorkflowJournalEvent,
      { kind: "completed" | "terminal_included" }
    >;
  }>;

export type WorkflowFundingAbandonmentHandoff = WorkflowFundingJournalHandoff &
  Readonly<{
    submissionIntent: WorkflowFundingSubmissionHandoff["submissionIntent"];
    reconciliation: Extract<
      FraudProofWorkflowJournalEvent,
      { kind: "reconciled" }
    > &
      Readonly<{ outcome: "not_found"; txHash: string }>;
  }>;

/**
 * Durable watcher-owned authority. The production application supplies this
 * port from its authenticated SQLite reservation store and local-node UTxO
 * source; workflow constructors never accept reservation data from config.
 */
export interface WorkflowFundingReservationPort {
  load(): Promise<unknown>;
  readPendingTransition(): Promise<unknown>;
  readPendingHandoff(): Promise<unknown>;
  readCompletionHandoff(): Promise<unknown>;
  readAbandonmentHandoff(): Promise<unknown>;
  /** Re-select a recorded attempt for reconciliation after canonical re-observation. */
  /** Null means another unresolved attempt currently owns the required inputs. */
  reobserve?(input: {
    readonly expectedRevision: string;
    readonly transactionHash: string;
  }): Promise<unknown>;
  resolveInputs(outRefs: readonly string[]): Promise<readonly UTxO[]>;
  /** Exact confirmed output from this reservation, or null if it has no lineage. */
  resolveConfirmedInput(input: { readonly outRef: string }): Promise<unknown>;
  resolveProtocolInputAuthority(input: {
    readonly deploymentFingerprint: string;
    readonly outRef: string;
    readonly semanticRole: "protocol_state";
  }): Promise<unknown>;
  prepare(input: {
    readonly expectedRevision: string;
    readonly transition: WorkflowFundingPreparedTransition;
    readonly handoff: WorkflowFundingSubmissionHandoff;
  }): Promise<unknown>;
  confirm(input: {
    readonly expectedRevision: string;
    readonly transactionHash: string;
  }): Promise<unknown>;
  abandon(input: {
    readonly expectedRevision: string;
    readonly transactionHash: string;
    readonly handoff: WorkflowFundingAbandonmentHandoff;
  }): Promise<unknown>;
  releaseIdle?(input: { readonly expectedRevision: string }): Promise<unknown>;
  /** Refill an idle reservation under fresh submission authority; null means unavailable. */
  refreshIdle?(input: {
    readonly expectedRevision: string;
    readonly releaseStaleInputs: boolean;
  }): Promise<unknown>;
  acknowledgeAbandonment(input: {
    readonly expectedRevision: string;
    readonly handoff: WorkflowFundingAbandonmentHandoff;
  }): Promise<unknown>;
  markConflict(input: {
    readonly expectedRevision: string;
    readonly code: "unexpected_spend" | "reservation_collision";
  }): Promise<unknown>;
  release(input: {
    readonly expectedRevision: string;
    readonly handoff: WorkflowFundingCompletionHandoff;
  }): Promise<unknown>;
}

export interface WorkflowFundingReservationPermit {
  readonly permitVersion: typeof WORKFLOW_FUNDING_RESERVATION_PERMIT;
}

export class WorkflowFundingReservationUnavailableError extends Error {
  constructor() {
    super("prover funding is temporarily unavailable for a fresh transaction");
    this.name = "WorkflowFundingReservationUnavailableError";
  }
}

export type PermitState = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly policy: WorkflowRuntimeFundingPolicy | undefined;
  readonly actuationPermit: WorkflowActuationPermit;
  readonly port: WorkflowFundingReservationPort;
  readonly maximumCollateralInputs: number;
  readonly reservationMaximumCollateralInputs: number;
  requiresParameterRefresh: boolean;
  snapshot: WorkflowFundingReservationSnapshot;
  resolvedInputs: ReadonlyMap<string, UTxO>;
  boundJournal: object | undefined;
  currentActionKind: string | undefined;
  currentActionDigest: string | undefined;
  currentFundingOutRefs: readonly string[];
  currentCollateralOutRefs: readonly string[];
  pendingTransactionHash: string | undefined;
  idleReleaseAuthorized: boolean;
  preparedTransaction:
    | Readonly<{ signed: TxSigned; cborHex: string }>
    | undefined;
};

export const admittedPermits = new WeakMap<object, PermitState>();

export const journalPermits = new WeakMap<object, PermitState>();

export const isPlainObject = (
  value: unknown,
): value is Record<string, unknown> =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  Object.getPrototypeOf(value) === Object.prototype &&
  Reflect.ownKeys(value).length === Object.keys(value).length;

export const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (!isPlainObject(value)) throw new Error(`${label} is not a plain object`);
  const actual = Object.keys(value).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
  return value;
};

export const canonicalOutRefs = (
  values: readonly string[],
  label: string,
): readonly string[] => {
  if (
    values.some((value) => !OUT_REF.test(value)) ||
    values.some(
      (value, index) =>
        index > 0 && values[index - 1]!.localeCompare(value) >= 0,
    )
  ) {
    throw new Error(`${label} must be canonical, ordered, and unique`);
  }
  return Object.freeze([...values]);
};

export const reservedInput = (
  value: unknown,
  label: string,
): WorkflowFundingReservedInput => {
  const record = exact(value, ["outRef", "role", "lovelace", "assets"], label);
  if (
    typeof record.outRef !== "string" ||
    !OUT_REF.test(record.outRef) ||
    (record.role !== "funding" && record.role !== "collateral") ||
    typeof record.lovelace !== "string" ||
    !NATURAL.test(record.lovelace) ||
    BigInt(record.lovelace) <= 0n ||
    !Array.isArray(record.assets)
  ) {
    throw new Error(`${label} is invalid`);
  }
  const assets = record.assets.map((entry, index) => {
    const asset = exact(
      entry,
      ["unit", "quantity"],
      `${label}.assets[${index.toString()}]`,
    );
    if (
      typeof asset.unit !== "string" ||
      !/^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u.test(asset.unit) ||
      typeof asset.quantity !== "string" ||
      !NATURAL.test(asset.quantity) ||
      BigInt(asset.quantity) <= 0n
    ) {
      throw new Error(`${label}.assets[${index.toString()}] is invalid`);
    }
    return Object.freeze({ unit: asset.unit, quantity: asset.quantity });
  });
  if (
    assets.some(
      (asset, index) =>
        index > 0 && assets[index - 1]!.unit.localeCompare(asset.unit) >= 0,
    ) ||
    (record.role === "collateral" && assets.length !== 0)
  ) {
    throw new Error(`${label} asset set is invalid`);
  }
  return Object.freeze({
    outRef: record.outRef,
    role: record.role,
    lovelace: record.lovelace,
    assets: Object.freeze(assets),
  });
};
