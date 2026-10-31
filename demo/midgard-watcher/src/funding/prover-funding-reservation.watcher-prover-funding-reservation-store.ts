import type {
  FraudProofWorkflowJournalEntry,
  SignedWorkflowTransactionRetirement,
  WorkflowFundingAttemptIntent,
} from "@al-ft/midgard-fault-proofs";

export const WATCHER_PROVER_FUNDING_RESERVATION_PLAN =
  "midgard-watcher-production-prover-funding-reservation-plan-v1" as const;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const ASSET_UNIT = /^[0-9a-f]{56}(?:[0-9a-f]{2}){0,32}$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const ACTION_KIND = /^[a-z][a-zA-Z0-9_.:-]{0,127}$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

export class WatcherProverFundingUnavailableError extends Error {}

export type WatcherProverFundingReservationInput = Readonly<{
  outRef: string;
  role: "funding" | "collateral";
  lovelace: string;
  assets: readonly Readonly<{ unit: string; quantity: string }>[];
}>;

export type WatcherProverFundingReservationPlan = Readonly<{
  schemaVersion: typeof WATCHER_PROVER_FUNDING_RESERVATION_PLAN;
  deploymentFingerprint: string;
  decisionDigest: string;
  policyDigest: string;
  reservationBasisDigest: string;
  fundingPaymentKeyHash: string;
  walletAddress: string;
  inputs: readonly WatcherProverFundingReservationInput[];
  fundingLovelace: string;
  collateralLovelace: string;
  assets: readonly Readonly<{ unit: string; quantity: string }>[];
  reservationId: string;
}>;

export type WatcherProverFundingReservationTransition = Readonly<{
  actionKind: string;
  transactionHash: string;
  transactionBodySha256: string;
  signedTransactionCborHex: string;
  /** Reserved wallet funding inputs; empty for an admitted protocol-funded tx. */
  consumedOutRefs: readonly string[];
  producedInputs: readonly WatcherProverFundingReservationInput[];
  transitionDigest: string;
}>;

export type WatcherProverFundingReservationRecord = Readonly<{
  reservationId: string;
  deploymentFingerprint: string;
  decisionDigest: string;
  policyDigest: string;
  reservationBasisDigest: string;
  revision: string;
  state: "active" | "released" | "conflict";
  activeInputs: readonly WatcherProverFundingReservationInput[];
  pendingTransition: WatcherProverFundingReservationTransition | null;
  lastConfirmedTransitionDigest: string | null;
  conflictCode: "unexpected_spend" | "reservation_collision" | null;
  recordDigest: string;
}>;

/** The workflow journal entries a change implies, committed in its
 * transaction. Empty when the caller's journal is not this store's. */
type Journaled = Readonly<{
  journal: readonly FraudProofWorkflowJournalEntry[];
}>;

export type WatcherProverFundingReservationStore = Readonly<{
  readAll(): Promise<readonly unknown[]>;
  /** Whether a change commits entries of this workflow journal store in its
   * own transaction. */
  commitsJournal?(journal: object): boolean;
  readReservedOutRefs(input: {
    readonly excludingReservationId?: string;
  }): Promise<readonly string[]>;
  /** Includes recorded and superseded attempts and lineage, not just the
   * current pending attempt. */
  hasSignedHistory?(input: {
    readonly reservationId: string;
  }): Promise<boolean>;
  readReobservationInputs?(input: {
    readonly reservationId: string;
    readonly transactionHash: string;
  }): Promise<
    readonly Pick<WatcherProverFundingReservationInput, "outRef" | "role">[]
  >;
  reobserveTransition?(
    input: Journaled & {
      readonly plan: WatcherProverFundingReservationPlan;
      readonly expectedRevision: string;
      readonly transactionHash: string;
      readonly inputs: readonly WatcherProverFundingReservationInput[];
      /** Adopts a superseded attempt that landed: its intent is recorded
       * again at this journal sequence. */
      readonly adoption?: Readonly<{ intentSequence: number }>;
    },
  ): Promise<WatcherProverFundingReservationRecord>;
  /** Per superseded, unretired attempt that no recorded attempt shares an
   * input with, the funding inputs it spent; empty when there is none. */
  readSupersededAttemptFundingOutRefs?(input: {
    readonly reservationId: string;
  }): Promise<readonly (readonly string[])[]>;
  /** The superseded, unretired attempt of `actionId` that this signed
   * transaction or a reserved input descends from, or null: such an attempt
   * landed, and is adopted instead of a fresh attempt. */
  readLandedSupersededAttempt(input: {
    readonly reservationId: string;
    readonly actionId: string;
    readonly signedTransactionCborHex: string;
  }): Promise<string | null>;
  /** The exact signed bytes of a recorded attempt, or null. */
  readRecordedSignedTransaction(input: {
    readonly reservationId: string;
    readonly transactionHash: string;
  }): Promise<string | null>;
  /** Every superseded attempt, retired or not. */
  readSupersededAttempts?(input: {
    readonly reservationId: string;
  }): Promise<readonly unknown[]>;
  /** Every signed attempt, as recorded. */
  readRecordedSubmissions?(input: {
    readonly reservationId: string;
  }): Promise<readonly unknown[]>;
  retireSupersededAttempt?(
    input: Journaled & {
      readonly plan: WatcherProverFundingReservationPlan;
      readonly expectedRevision: string;
      readonly transactionHash: string;
      readonly retirement: SignedWorkflowTransactionRetirement;
    },
  ): Promise<WatcherProverFundingReservationRecord>;
  readPendingTransition(input: {
    readonly reservationId: string;
  }): Promise<unknown | null>;
  readConfirmedInput(input: {
    readonly reservationId: string;
    readonly outRef: string;
  }): Promise<unknown | null>;
  /** Clear a never-submitted reservation only if its complete snapshot still matches. */
  releaseUnused?(
    record: WatcherProverFundingReservationRecord,
  ): Promise<boolean>;
  /** Releases `orphan` once every signed attempt it holds is retired past
   * release finality, only if its snapshot still matches. */
  settleOrphanedReservation?(input: {
    readonly orphan: WatcherProverFundingReservationRecord;
    readonly retirements: readonly Readonly<{
      transactionHash: string;
      retirement: unknown;
    }>[];
    /** The execution of a forgotten objective that signed the orphan's
     * attempts: its workflow rows are deleted in the same transaction. */
    readonly workflowId?: string;
  }): Promise<boolean>;
  reserve(
    plan: WatcherProverFundingReservationPlan,
    expectedIdleRevision?: string,
  ): Promise<"reserved" | "unchanged">;
  prepareTransition(
    input: Journaled & {
      readonly attempt: WorkflowFundingAttemptIntent;
      readonly plan: WatcherProverFundingReservationPlan;
      readonly expectedRevision: string;
      readonly actionKind: string;
      readonly signedTransactionCborHex: string;
      readonly transactionHash: string;
      readonly transactionBodySha256: string;
      readonly consumedOutRefs: readonly string[];
      readonly producedInputs: readonly WatcherProverFundingReservationInput[];
    },
  ): Promise<WatcherProverFundingReservationRecord>;
  confirmTransition(
    input: Journaled & {
      readonly plan: WatcherProverFundingReservationPlan;
      readonly expectedRevision: string;
      readonly transactionHash: string;
      readonly transitionDigest: string;
    },
  ): Promise<WatcherProverFundingReservationRecord>;
  /** Supersedes the pending attempt; with `retirement`, it is retired. */
  abandonPendingTransition(
    input: Journaled & {
      readonly plan: WatcherProverFundingReservationPlan;
      readonly expectedRevision: string;
      readonly transitionDigest: string;
      readonly retirement?: SignedWorkflowTransactionRetirement;
    },
  ): Promise<WatcherProverFundingReservationRecord>;
  releaseIdle?(input: {
    readonly plan: WatcherProverFundingReservationPlan;
    readonly expectedRevision: string;
  }): Promise<WatcherProverFundingReservationRecord>;
  markConflict(
    input: Journaled & {
      readonly plan: WatcherProverFundingReservationPlan;
      readonly expectedRevision: string;
      readonly code: "unexpected_spend" | "reservation_collision";
    },
  ): Promise<WatcherProverFundingReservationRecord>;
  /** Releases the reservation at its workflow's final completion. */
  release(
    input: Journaled & {
      readonly plan: WatcherProverFundingReservationPlan;
      readonly expectedRevision: string;
    },
  ): Promise<WatcherProverFundingReservationRecord>;
}>;

export const admittedPlans = new WeakSet<object>();

export const assertWatcherProverFundingReservationPlan = (
  plan: WatcherProverFundingReservationPlan,
): void => {
  if (!admittedPlans.has(plan)) {
    throw new Error("prover funding reservation plan is not admitted");
  }
};

export const exactRecord = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} is not an exact plain object`);
  }
  const record = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
  return record;
};

const parseReservationInput = (
  value: unknown,
  label: string,
): WatcherProverFundingReservationInput => {
  const record = exactRecord(
    value,
    ["outRef", "role", "lovelace", "assets"],
    label,
  );
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
  const assets = record.assets.map((asset, index) => {
    const parsed = exactRecord(
      asset,
      ["unit", "quantity"],
      `${label}.assets[${index.toString()}]`,
    );
    if (
      typeof parsed.unit !== "string" ||
      !ASSET_UNIT.test(parsed.unit) ||
      typeof parsed.quantity !== "string" ||
      !NATURAL.test(parsed.quantity) ||
      BigInt(parsed.quantity) <= 0n
    ) {
      throw new Error(`${label}.assets[${index.toString()}] is invalid`);
    }
    return Object.freeze({ unit: parsed.unit, quantity: parsed.quantity });
  });
  if (
    assets.some(
      (asset, index) =>
        index > 0 && assets[index - 1]!.unit.localeCompare(asset.unit) >= 0,
    ) ||
    (record.role === "collateral" && assets.length !== 0)
  ) {
    throw new Error(`${label} asset ordering is invalid`);
  }
  return Object.freeze({
    outRef: record.outRef,
    role: record.role,
    lovelace: record.lovelace,
    assets: Object.freeze(assets),
  });
};

export const parseReservationInputs = (
  value: unknown,
  label: string,
): readonly WatcherProverFundingReservationInput[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an array`);
  const inputs = value.map((entry, index) =>
    parseReservationInput(entry, `${label}[${index.toString()}]`),
  );
  if (
    inputs.some(
      (input, index) =>
        index > 0 && inputs[index - 1]!.outRef.localeCompare(input.outRef) >= 0,
    )
  ) {
    throw new Error(`${label} output references are not ordered and unique`);
  }
  return Object.freeze(inputs);
};
