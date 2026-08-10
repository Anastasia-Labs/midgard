import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  fromHex,
  isEmulatorProvider,
  LucidEvolution,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Data, Duration, Effect, Schedule } from "effect";

import * as BlocksDB from "@/database/blocks.js";
import { ImmutableDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  SUBMIT_SLOT_LENGTH_MS,
  SUBMIT_SLOT_VALIDITY_BUFFER,
  type SubmitSlotSnapshot,
} from "@/local-ledger-slot.js";
import { Database } from "@/services/index.js";
import {
  type InlineWaitPolicy,
  planSubmitTiming,
  planSubmitTimingAfterInlineWait,
  type SubmitTimingPlan,
} from "@/transactions/submit-timing.js";

/**
 * Shared transaction signing, submission, confirmation, and recovery helpers.
 *
 * These utilities centralize the messy provider-facing parts of transaction
 * handling so higher-level transaction builders can stay focused on protocol
 * logic.
 */
const RETRY_ATTEMPTS = 1;

const INIT_RETRY_AFTER_MILLIS = 2_000;

const PAUSE_DURATION = "5 seconds";
const TX_CONFIRMATION_TIMEOUT_MS = 90_000;
const TX_CONFIRMATION_RETRIES = 1;
const TX_CONFIRMATION_POLL_INTERVAL_MS = 5_000;
const SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS = 90_000;
const SUBMIT_RECOVERY_POLL_INTERVAL_MS = 5_000;
export const EARLY_VALIDITY_RETRY_SLOT_BUFFER = SUBMIT_SLOT_VALIDITY_BUFFER;
const SLOT_LENGTH_MS = SUBMIT_SLOT_LENGTH_MS;
const DEFAULT_SIGNED_TX_INLINE_WAIT_MS = 60_000;
const DEFAULT_STALE_PROVIDER_VALIDITY_RETRY_MAX_ATTEMPTS = Math.ceil(
  DEFAULT_SIGNED_TX_INLINE_WAIT_MS / SLOT_LENGTH_MS,
);

export const isUnknownOutputReferenceSubmitError = (
  error: unknown,
): boolean => {
  const seen = new Set<unknown>();
  const hasStructuredUnknownInput = (value: unknown): boolean => {
    if (typeof value !== "object" || value === null || seen.has(value)) {
      return false;
    }
    seen.add(value);
    const record = value as Record<string, unknown>;
    if ("unknownOutputReferences" in record || "badInputs" in record) {
      return true;
    }
    return Object.values(record).some(hasStructuredUnknownInput);
  };
  if (hasStructuredUnknownInput(error)) {
    return true;
  }
  return errorTextSearchStrings(error).some(
    (message) =>
      message.includes("BadInputsUTxO") || message.includes("UnknownInput"),
  );
};

const OUTSIDE_VALIDITY_INTERVAL_REGEX =
  /OutsideValidityIntervalUTxO \(ValidityInterval \{invalidBefore = SJust \(SlotNo (\d+)\), invalidHereafter = SJust \(SlotNo (\d+)\)\}\) \(SlotNo (\d+)\)/;
const EMULATOR_LOWER_BOUND_OUTSIDE_VALIDITY_REGEX =
  /Lower bound \((\d+)\) not in slot range \((\d+)\)/;
export type OutsideValidityIntervalDetails = {
  readonly invalidBeforeSlot: number;
  readonly invalidHereafterSlot?: number;
  readonly currentSlot: number;
};

export type SignedTxValidityInterval = {
  readonly invalidBeforeSlot?: number;
  readonly invalidHereafterSlot?: number;
};

const outsideValidityDetails = (
  invalidBeforeSlot: number,
  invalidHereafterSlot: number | undefined,
  currentSlot: number,
): OutsideValidityIntervalDetails | null =>
  [invalidBeforeSlot, currentSlot].every(Number.isFinite) &&
  (invalidHereafterSlot === undefined || Number.isFinite(invalidHereafterSlot))
    ? {
        invalidBeforeSlot,
        ...(invalidHereafterSlot === undefined ? {} : { invalidHereafterSlot }),
        currentSlot,
      }
    : null;

/**
 * Extracts slot-boundary details from an `OutsideValidityIntervalUTxO` error.
 */
export const parseOutsideValidityIntervalDetails = (
  error: unknown,
): OutsideValidityIntervalDetails | null => {
  const structured = parseStructuredOutsideValidityIntervalDetails(error);
  if (structured !== null) {
    return structured;
  }
  for (const message of errorTextSearchStrings(error)) {
    const normalizedMessage = message.replace(/\\"/g, '"');
    const match = OUTSIDE_VALIDITY_INTERVAL_REGEX.exec(normalizedMessage);
    if (match !== null) {
      return outsideValidityDetails(
        Number(match[1]),
        Number(match[2]),
        Number(match[3]),
      );
    }

    const emulatorLowerBoundMatch =
      EMULATOR_LOWER_BOUND_OUTSIDE_VALIDITY_REGEX.exec(normalizedMessage);
    if (emulatorLowerBoundMatch !== null) {
      return outsideValidityDetails(
        Number(emulatorLowerBoundMatch[1]),
        Number.MAX_SAFE_INTEGER,
        Number(emulatorLowerBoundMatch[2]),
      );
    }
  }
  return null;
};

type ProviderValidityRetryDecision =
  | {
      readonly status: "wait";
      readonly waitMs: number;
      readonly targetSlot: number;
    }
  | { readonly status: "already_valid" }
  | { readonly status: "expired" }
  | { readonly status: "window_too_narrow" }
  | { readonly status: "attempts_exhausted" };

export const resolveEarlyValidityRetry = (
  details: OutsideValidityIntervalDetails,
  attempt: number,
  maxAttempts = DEFAULT_STALE_PROVIDER_VALIDITY_RETRY_MAX_ATTEMPTS,
): ProviderValidityRetryDecision => {
  if (attempt >= maxAttempts) {
    return { status: "attempts_exhausted" };
  }
  if (
    details.invalidHereafterSlot !== undefined &&
    details.currentSlot >= details.invalidHereafterSlot
  ) {
    return { status: "expired" };
  }
  const targetSlot =
    details.invalidBeforeSlot + EARLY_VALIDITY_RETRY_SLOT_BUFFER;
  if (
    details.invalidHereafterSlot !== undefined &&
    targetSlot >= details.invalidHereafterSlot
  ) {
    return { status: "window_too_narrow" };
  }
  if (details.currentSlot >= targetSlot) {
    return { status: "already_valid" };
  }
  return {
    status: "wait",
    targetSlot,
    waitMs: (targetSlot - details.currentSlot) * SLOT_LENGTH_MS,
  };
};

export const resolveEarlyValidityRetryDelayMs = (
  details: OutsideValidityIntervalDetails,
  attempt: number,
): number | null => {
  const decision = resolveEarlyValidityRetry(details, attempt);
  return decision.status === "wait" ? decision.waitMs : null;
};

export const inspectSignedTxValidityInterval = (
  signedTxCbor: string,
): SignedTxValidityInterval => {
  const body = CML.Transaction.from_cbor_hex(signedTxCbor).body() as unknown;
  return compactValidityInterval({
    invalidBeforeSlot: cmlBodySlotNumber(body, [
      "validity_interval_start",
      "validityStartInterval",
    ]),
    invalidHereafterSlot: cmlBodySlotNumber(body, ["ttl"]),
  });
};

const parseStructuredOutsideValidityIntervalDetails = (
  value: unknown,
  seen = new Set<unknown>(),
): OutsideValidityIntervalDetails | null => {
  if (value === null || value === undefined) {
    return null;
  }
  if (typeof value !== "object") {
    return null;
  }
  if (seen.has(value)) {
    return null;
  }
  seen.add(value);
  if (value instanceof Error) {
    const fromCause = parseStructuredOutsideValidityIntervalDetails(
      value.cause,
      seen,
    );
    if (fromCause !== null) {
      return fromCause;
    }
  }
  const record = value as Record<string, unknown>;
  const direct = detailsFromStructuredRecord(record);
  if (direct !== null) {
    return direct;
  }
  for (const key of [
    "error",
    "data",
    "response",
    "body",
    "detail",
    "cause",
  ] as const) {
    const nested = parseStructuredOutsideValidityIntervalDetails(
      record[key],
      seen,
    );
    if (nested !== null) {
      return nested;
    }
  }
  return null;
};

const detailsFromStructuredRecord = (
  record: Record<string, unknown>,
): OutsideValidityIntervalDetails | null => {
  const validityInterval = record.validityInterval;
  if (typeof validityInterval !== "object" || validityInterval === null) {
    return null;
  }
  const interval = validityInterval as Record<string, unknown>;
  const invalidBeforeSlot = numberFromUnknown(interval.invalidBefore);
  const invalidHereafterSlot =
    numberFromUnknown(interval.invalidHereafter) ??
    numberFromUnknown(interval.invalidAfter);
  const currentSlot = numberFromUnknown(record.currentSlot);
  if (invalidBeforeSlot === null || currentSlot === null) {
    return null;
  }
  return outsideValidityDetails(
    invalidBeforeSlot,
    invalidHereafterSlot ?? undefined,
    currentSlot,
  );
};

const errorTextSearchStrings = (
  value: unknown,
  seen = new Set<unknown>(),
): readonly string[] => {
  if (typeof value === "string") {
    return [value];
  }
  if (value instanceof Error) {
    return [
      value.message,
      String(value),
      ...errorTextSearchStrings(value.cause, seen),
    ];
  }
  if (typeof value !== "object" || value === null || seen.has(value)) {
    return [String(value)];
  }
  seen.add(value);
  const record = value as Record<string, unknown>;
  const direct = ["message", "detail", "body"].flatMap((key) =>
    typeof record[key] === "string" ? [record[key] as string] : [],
  );
  const nested = ["cause", "error", "response", "data"].flatMap((key) =>
    errorTextSearchStrings(record[key], seen),
  );
  return [...direct, ...nested];
};

const compactValidityInterval = (
  interval: SignedTxValidityInterval,
): SignedTxValidityInterval => ({
  ...(interval.invalidBeforeSlot === undefined
    ? {}
    : { invalidBeforeSlot: interval.invalidBeforeSlot }),
  ...(interval.invalidHereafterSlot === undefined
    ? {}
    : { invalidHereafterSlot: interval.invalidHereafterSlot }),
});

const slotNumber = (value: unknown): number | undefined => {
  const parsed = numberFromUnknown(value);
  return parsed === null ? undefined : parsed;
};

const cmlBodySlotNumber = (
  body: unknown,
  methodNames: readonly string[],
): number | undefined => {
  if (typeof body !== "object" || body === null) {
    return undefined;
  }
  const record = body as Record<string, unknown>;
  for (const methodName of methodNames) {
    const method = record[methodName];
    if (typeof method === "function") {
      return slotNumber((method as () => unknown).call(body));
    }
  }
  return undefined;
};

const numberFromUnknown = (value: unknown): number | null => {
  if (typeof value === "number") {
    return Number.isSafeInteger(value) && value >= 0 ? value : null;
  }
  if (typeof value === "bigint") {
    return value >= 0n && value <= BigInt(Number.MAX_SAFE_INTEGER)
      ? Number(value)
      : null;
  }
  if (typeof value === "string" && /^\d+$/.test(value)) {
    const parsed = Number(value);
    return Number.isSafeInteger(parsed) ? parsed : null;
  }
  if (
    typeof value === "object" &&
    value !== null &&
    typeof (value as { to_str?: unknown }).to_str === "function"
  ) {
    return numberFromUnknown((value as { to_str: () => string }).to_str());
  }
  return null;
};

export type SignSubmitContext = {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly walletAddress: string;
};

type AwaitTxConfirmationOptions = {
  readonly timeout?: number;
  readonly checkInterval?: number;
};

/**
 * Waits for exact transaction confirmation through Lucid's typed status API.
 *
 * Lucid's Emulator is the one provider whose public `awaitTx` call also
 * produces the next block. Advance it first, then retain the same exact-hash
 * status confirmation used by live providers.
 */
export const awaitExactTransactionConfirmation = async (
  lucid: LucidEvolution,
  txHash: string,
  options?: AwaitTxConfirmationOptions,
) => {
  if (isEmulatorProvider(lucid.config().provider)) {
    const included = await lucid.awaitTx(txHash, options?.checkInterval);
    if (!included) {
      throw new Error(`Emulator did not include transaction ${txHash}`);
    }
  }
  return lucid.awaitTxConfirmation(txHash, options);
};

/**
 * Formats an outref into a stable map key.
 */
const outRefToKey = (txHash: string, outputIndex: number): string =>
  `${txHash}#${outputIndex.toString()}`;

/**
 * Reconciles Lucid's local wallet view after a confirmed transaction.
 *
 * This is mainly relevant for local wallets/providers that expose
 * `overrideUTxOs`, allowing later transactions to reuse the updated wallet view
 * without waiting for an external provider refresh.
 */
const reconcileWalletUtxosFromSignedTx = (
  lucid: LucidEvolution,
  submission: SignSubmitContext,
): Effect.Effect<void, never> =>
  Effect.gen(function* () {
    const wallet = lucid.wallet() as {
      getUtxos: () => Promise<UTxO[]>;
      overrideUTxOs?: (utxos: UTxO[]) => void;
    };
    if (typeof wallet.overrideUTxOs !== "function") {
      return;
    }
    const tx = CML.Transaction.from_cbor_hex(submission.signedTxCbor);
    const txInputs = tx.body().inputs();
    const spentOutRefs = new Set<string>();
    for (let index = 0; index < txInputs.len(); index += 1) {
      const input = txInputs.get(index);
      spentOutRefs.add(
        outRefToKey(input.transaction_id().to_hex(), Number(input.index())),
      );
    }

    const walletUtxos = yield* Effect.tryPromise({
      try: () => wallet.getUtxos(),
      catch: () => [] as UTxO[],
    });
    const filteredWalletUtxos = walletUtxos.filter(
      (utxo) => !spentOutRefs.has(outRefToKey(utxo.txHash, utxo.outputIndex)),
    );
    const txOutputs = tx.body().outputs();
    const localWalletOutputs: UTxO[] = [];
    for (let outputIndex = 0; outputIndex < txOutputs.len(); outputIndex += 1) {
      const output = coreToTxOutput(txOutputs.get(outputIndex));
      if (output.address !== submission.walletAddress) {
        continue;
      }
      localWalletOutputs.push({
        txHash: submission.txHash,
        outputIndex,
        address: output.address,
        assets: output.assets,
        datumHash: output.datumHash ?? undefined,
        datum: output.datum ?? undefined,
        scriptRef: output.scriptRef ?? undefined,
      });
    }
    const reconciled = new Map<string, UTxO>();
    for (const utxo of filteredWalletUtxos) {
      reconciled.set(outRefToKey(utxo.txHash, utxo.outputIndex), utxo);
    }
    for (const utxo of localWalletOutputs) {
      reconciled.set(outRefToKey(utxo.txHash, utxo.outputIndex), utxo);
    }
    wallet.overrideUTxOs(Array.from(reconciled.values()));
  }).pipe(Effect.catchAll(() => Effect.void));

export type BlockTxPayload = {
  readonly txId: Buffer;
  readonly txCbor: Buffer;
};

export type SubmitRecoveryInlineOptions = {
  readonly label?: string;
  readonly sleep?: (milliseconds: number) => Effect.Effect<void, never>;
  readonly slotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>;
  readonly requireSlotForBoundedTx?: boolean;
  readonly maxPreSubmitWaitMs?: number;
  /**
   * Number of retries permitted after an unclassified provider submission
   * error. Callers with a durable exact-CBOR journal can set this to zero and
   * reconcile authoritative chain evidence before any later resubmission.
   */
  readonly maxProviderRetryAttempts?: number;
  /**
   * Number of retries permitted after an OutsideValidityInterval provider
   * response. A durable one-call journal sets this to zero.
   */
  readonly maxOutsideValidityRecoveryAttempts?: number;
  readonly confirmationTimeoutMs?: number;
  readonly confirmationRetries?: number;
  readonly confirmationPollIntervalMs?: number;
  readonly inlineWaitPolicy?: Extract<InlineWaitPolicy, "allow_inline_wait">;
  readonly noInlineSubmitDefer?: never;
};

export type NoInlineSubmitDeferKind =
  | "pre_submit_validity"
  | "early_validity_recovery"
  | "provider_slot_wait";

export type NoInlineSubmitDeferEvidence = {
  readonly key: string;
  readonly dependencyKey: string;
  readonly invalidationKey: string;
};

export type NoInlineSubmitRecoveryOptions = Omit<
  SubmitRecoveryInlineOptions,
  "inlineWaitPolicy" | "noInlineSubmitDefer"
> & {
  readonly inlineWaitPolicy: "defer_positive_wait";
  readonly noInlineSubmitDefer: NoInlineSubmitDeferEvidence;
};

export type SubmitRecoveryOptions =
  | SubmitRecoveryInlineOptions
  | NoInlineSubmitRecoveryOptions;

export class NoInlineSubmitDefer extends Data.TaggedError(
  "NoInlineSubmitDefer",
)<{
  readonly callerLabel: string;
  readonly kind: NoInlineSubmitDeferKind;
  readonly key: string;
  readonly txHash?: string;
  readonly currentSlot: number;
  readonly targetSlot: number;
  readonly dueSlot: number;
  readonly waitMs: number;
  readonly slotSource: string;
  readonly dependencyKey: string;
  readonly invalidationKey: string;
  readonly invalidBeforeSlot?: number;
  readonly invalidHereafterSlot?: number;
}> {}

export type SignSubmitNoConfirmationResult =
  | {
      readonly status: "submitted";
      readonly txHash: string;
    }
  | {
      readonly status: "deferred";
      readonly defer: NoInlineSubmitDefer;
    };

export const isNoInlineSubmitDefer = (
  value: unknown,
): value is NoInlineSubmitDefer =>
  typeof value === "object" &&
  value !== null &&
  (value as { readonly _tag?: unknown })._tag === "NoInlineSubmitDefer";

type EmulatorTimeProvider = {
  readonly now: () => number;
  readonly awaitSlot: (length?: number) => void | Promise<void>;
};

const isEmulatorTimeProvider = (
  provider: unknown,
): provider is EmulatorTimeProvider =>
  typeof provider === "object" &&
  provider !== null &&
  "slot" in provider &&
  typeof (provider as { readonly now?: unknown }).now === "function" &&
  typeof (provider as { readonly awaitSlot?: unknown }).awaitSlot ===
    "function";

const emulatorTimeProvider = (
  lucid: LucidEvolution,
): EmulatorTimeProvider | null => {
  if (typeof lucid.config !== "function") {
    return null;
  }
  const config = lucid.config();
  const provider = config.provider;
  return isEmulatorTimeProvider(provider) ? provider : null;
};

const emulatorSubmitSlotSnapshot = (
  lucid: LucidEvolution,
): SubmitSlotSnapshot | undefined => {
  const emulatorProvider = emulatorTimeProvider(lucid);
  if (emulatorProvider !== null) {
    const emulatorSlot = slotNumber(
      lucid.unixTimeToSlot(emulatorProvider.now()),
    );
    if (emulatorSlot !== undefined) {
      return {
        source: "emulator",
        currentSlot: emulatorSlot,
        observedAtMs: emulatorProvider.now(),
        slotLengthMs: SLOT_LENGTH_MS,
      };
    }
  }
  return undefined;
};

const submitRecoverySleep = (
  lucid: LucidEvolution,
): ((milliseconds: number) => Effect.Effect<void, never>) => {
  const emulatorProvider = emulatorTimeProvider(lucid);
  if (emulatorProvider === null) {
    return (milliseconds) => Effect.sleep(milliseconds);
  }
  return (milliseconds) =>
    Effect.promise(async () => {
      const slotsToAdvance = Math.max(
        1,
        Math.ceil(milliseconds / SLOT_LENGTH_MS),
      );
      await emulatorProvider.awaitSlot(slotsToAdvance);
    });
};

const resolvePreSubmitSlotSnapshot = (
  lucid: LucidEvolution,
  slotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
): Effect.Effect<SubmitSlotSnapshot, unknown> => {
  if (slotSnapshot !== undefined) {
    return slotSnapshot();
  }
  const emulatorSnapshot = emulatorSubmitSlotSnapshot(lucid);
  if (emulatorSnapshot !== undefined) {
    return Effect.succeed(emulatorSnapshot);
  }
  return Effect.fail(new Error("No local submit slot snapshot is configured"));
};

type SubmitTimingPositiveWaitPlan = Extract<
  SubmitTimingPlan,
  {
    readonly status: "wait" | "not_due" | "slot_source_stalled";
  }
>;

const noInlineSubmitDeferEvidence = (
  options: NoInlineSubmitRecoveryOptions,
): NoInlineSubmitDeferEvidence => options.noInlineSubmitDefer;

const noInlineSubmitDeferFromTimingPlan = (
  kind: NoInlineSubmitDeferKind,
  txHash: string,
  plan: SubmitTimingPlan,
  options: SubmitRecoveryOptions,
): NoInlineSubmitDefer | undefined => {
  if (
    options.inlineWaitPolicy !== "defer_positive_wait" ||
    !("waitMs" in plan) ||
    plan.waitMs <= 0 ||
    !("currentSlot" in plan) ||
    plan.currentSlot === undefined ||
    !("targetSlot" in plan) ||
    plan.targetSlot === undefined ||
    !("dueSlot" in plan) ||
    plan.dueSlot === undefined ||
    !("slotSource" in plan) ||
    plan.slotSource === undefined
  ) {
    return undefined;
  }
  const positiveWaitPlan = plan as SubmitTimingPositiveWaitPlan;
  const deferEvidence = noInlineSubmitDeferEvidence(options);
  return new NoInlineSubmitDefer({
    callerLabel: positiveWaitPlan.callerLabel,
    kind,
    key: deferEvidence.key,
    txHash,
    currentSlot: positiveWaitPlan.currentSlot,
    targetSlot: positiveWaitPlan.targetSlot,
    dueSlot: positiveWaitPlan.dueSlot,
    waitMs: positiveWaitPlan.waitMs,
    slotSource: positiveWaitPlan.slotSource,
    dependencyKey: deferEvidence.dependencyKey,
    invalidationKey: deferEvidence.invalidationKey,
    invalidBeforeSlot: positiveWaitPlan.invalidBeforeSlot,
    ...(positiveWaitPlan.invalidHereafterSlot === undefined
      ? {}
      : { invalidHereafterSlot: positiveWaitPlan.invalidHereafterSlot }),
  });
};

const noInlineSubmitProviderSlotDefer = ({
  txHash,
  options,
  callerLabel,
  kind,
  currentSlot,
  targetSlot,
  waitMs,
  invalidBeforeSlot,
  invalidHereafterSlot,
}: {
  readonly txHash: string;
  readonly options: SubmitRecoveryOptions;
  readonly callerLabel: string;
  readonly kind: NoInlineSubmitDeferKind;
  readonly currentSlot: number;
  readonly targetSlot: number;
  readonly waitMs: number;
  readonly invalidBeforeSlot: number;
  readonly invalidHereafterSlot?: number;
}): NoInlineSubmitDefer | undefined => {
  if (options.inlineWaitPolicy !== "defer_positive_wait" || waitMs <= 0) {
    return undefined;
  }
  const deferEvidence = noInlineSubmitDeferEvidence(options);
  return new NoInlineSubmitDefer({
    callerLabel,
    kind,
    key: deferEvidence.key,
    txHash,
    currentSlot,
    targetSlot,
    dueSlot: targetSlot,
    waitMs,
    slotSource: "provider",
    dependencyKey: deferEvidence.dependencyKey,
    invalidationKey: deferEvidence.invalidationKey,
    invalidBeforeSlot,
    ...(invalidHereafterSlot === undefined ? {} : { invalidHereafterSlot }),
  });
};

const preSubmitValidityCheck = (
  lucid: LucidEvolution,
  signed: Awaited<ReturnType<TxSignBuilder["complete"]>>,
  options: SubmitRecoveryOptions,
): Effect.Effect<SubmitTimingPlan, never> =>
  Effect.gen(function* () {
    if (typeof signed.toCBOR !== "function") {
      return {
        status: "ready",
        callerLabel: options.label ?? "submit",
        waitMs: 0,
      };
    }
    const interval = inspectSignedTxValidityInterval(signed.toCBOR());
    if (
      interval.invalidBeforeSlot === undefined &&
      interval.invalidHereafterSlot === undefined
    ) {
      return {
        status: "ready",
        callerLabel: options.label ?? "submit",
        waitMs: 0,
      };
    }
    const slotSnapshotResult = yield* Effect.either(
      resolvePreSubmitSlotSnapshot(lucid, options.slotSnapshot),
    );
    if (
      slotSnapshotResult._tag === "Left" &&
      options.requireSlotForBoundedTx !== true
    ) {
      return {
        status: "ready",
        callerLabel: options.label ?? "submit",
        waitMs: 0,
      };
    }
    return planSubmitTiming({
      ...interval,
      callerLabel: options.label ?? "submit",
      slotSnapshot:
        slotSnapshotResult._tag === "Right"
          ? slotSnapshotResult.right
          : undefined,
      slotSnapshotError:
        slotSnapshotResult._tag === "Left"
          ? slotSnapshotResult.left
          : undefined,
      submitSlotBuffer: EARLY_VALIDITY_RETRY_SLOT_BUFFER,
      maxInlineWaitMs:
        options.maxPreSubmitWaitMs ?? DEFAULT_SIGNED_TX_INLINE_WAIT_MS,
      inlineWaitPolicy: options.inlineWaitPolicy,
      dependencyKey: options.noInlineSubmitDefer?.dependencyKey,
      invalidationKey: options.noInlineSubmitDefer?.invalidationKey,
    });
  });

const inspectSignedTxValidityIntervalIfAvailable = (
  signed: Awaited<ReturnType<TxSignBuilder["complete"]>>,
): SignedTxValidityInterval | undefined =>
  typeof signed.toCBOR === "function"
    ? inspectSignedTxValidityInterval(signed.toCBOR())
    : undefined;

const submitTimingFailureError = (
  txHash: string,
  plan: Exclude<SubmitTimingPlan, { readonly status: "ready" | "wait" }>,
): Error => {
  switch (plan.status) {
    case "not_due":
      return new Error(
        `Tx ${txHash} is not due for submit; currentSlot=${plan.currentSlot.toString()}, targetSlot=${plan.targetSlot.toString()}, waitMs=${plan.waitMs.toString()}, max inline wait exceeded. Rebuild or register due work instead of retrying the same signed body.`,
      );
    case "slot_source_stalled":
      return new Error(
        `Tx ${txHash} local submit slot source stalled after inline wait; currentSlot=${plan.currentSlot.toString()}, targetSlot=${plan.targetSlot.toString()}, waitMs=${plan.waitMs.toString()}, slotSource=${plan.slotSource}. Rebuild required.`,
      );
    case "expired":
      return new Error(
        `Tx ${txHash} validity window is expired: currentSlot=${plan.currentSlot.toString()}, invalidBefore=${plan.invalidBeforeSlot?.toString() ?? "none"}, invalidHereafter=${plan.invalidHereafterSlot.toString()}; rebuild required`,
      );
    case "window_too_narrow":
      return new Error(
        `Tx ${txHash} validity window is too narrow for the submit margin: currentSlot=${plan.currentSlot.toString()}, invalidBefore=${plan.invalidBeforeSlot.toString()}, invalidHereafter=${plan.invalidHereafterSlot.toString()}, targetSlot=${plan.targetSlot.toString()}; rebuild required`,
      );
    case "slot_source_unavailable":
      return new Error(
        `Tx ${txHash} requires a local submit slot snapshot before bounded production submission, but no strict slot source was available: invalidBefore=${plan.invalidBeforeSlot?.toString() ?? "none"}, invalidHereafter=${plan.invalidHereafterSlot?.toString() ?? "none"}, cause=${formatUnknownError(plan.cause, { includeCause: true })}`,
      );
  }
};

/**
 * Submits a signed transaction with recovery logic for provider races and
 * early-validity-window failures.
 */
export const submitSignedTxWithRecovery = (
  lucid: LucidEvolution,
  signed: Awaited<ReturnType<TxSignBuilder["complete"]>>,
  txHash: string,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<void, unknown> =>
  Effect.gen(function* () {
    const sleep = options.sleep ?? submitRecoverySleep(lucid);
    const maxProviderRetryAttempts =
      options.maxProviderRetryAttempts ?? RETRY_ATTEMPTS;
    if (
      !Number.isSafeInteger(maxProviderRetryAttempts) ||
      maxProviderRetryAttempts < 0
    ) {
      return yield* Effect.fail(
        new Error(
          `Invalid maxProviderRetryAttempts=${maxProviderRetryAttempts.toString()}`,
        ),
      );
    }
    let providerRetryAttempts = 0;
    let outsideValidityRecoveryAttempts = 0;
    let outsideValidityRecoveryWaitedMs = 0;
    const maxOutsideValidityRecoveryWaitMs =
      options.maxPreSubmitWaitMs ?? DEFAULT_SIGNED_TX_INLINE_WAIT_MS;
    const maxOutsideValidityRecoveryAttempts =
      options.maxOutsideValidityRecoveryAttempts ??
      Math.max(1, Math.ceil(maxOutsideValidityRecoveryWaitMs / SLOT_LENGTH_MS));
    if (
      !Number.isSafeInteger(maxOutsideValidityRecoveryAttempts) ||
      maxOutsideValidityRecoveryAttempts < 0
    ) {
      return yield* Effect.fail(
        new Error(
          `Invalid maxOutsideValidityRecoveryAttempts=${maxOutsideValidityRecoveryAttempts.toString()}`,
        ),
      );
    }

    for (;;) {
      const preSubmitValidity = yield* preSubmitValidityCheck(
        lucid,
        signed,
        options,
      );
      if (
        preSubmitValidity.status === "expired" ||
        preSubmitValidity.status === "window_too_narrow"
      ) {
        return yield* Effect.fail(
          submitTimingFailureError(txHash, preSubmitValidity),
        );
      }
      if (preSubmitValidity.status === "slot_source_unavailable") {
        return yield* Effect.fail(
          submitTimingFailureError(txHash, preSubmitValidity),
        );
      }
      const preSubmitDefer = noInlineSubmitDeferFromTimingPlan(
        "pre_submit_validity",
        txHash,
        preSubmitValidity,
        options,
      );
      if (preSubmitDefer !== undefined) {
        return yield* Effect.fail(preSubmitDefer);
      }
      if (preSubmitValidity.status === "not_due") {
        return yield* Effect.fail(
          submitTimingFailureError(txHash, preSubmitValidity),
        );
      }
      if (preSubmitValidity.status === "slot_source_stalled") {
        return yield* Effect.fail(
          submitTimingFailureError(txHash, preSubmitValidity),
        );
      }
      if (preSubmitValidity.status === "wait") {
        yield* Effect.logWarning(
          [
            `Tx ${txHash} has not reached its validity submit margin;`,
            `currentSlot=${preSubmitValidity.currentSlot.toString()},`,
            `slotSource=${preSubmitValidity.slotSource},`,
            `invalidBefore=${preSubmitValidity.invalidBeforeSlot.toString()},`,
            `invalidHereafter=${preSubmitValidity.invalidHereafterSlot?.toString() ?? "none"}.`,
            `Waiting ${preSubmitValidity.waitMs.toString()}ms before submit`,
            options.label === undefined ? "" : `label=${options.label}.`,
          ].join(" "),
        );
        yield* sleep(preSubmitValidity.waitMs);
        const refreshedSnapshot = yield* Effect.either(
          resolvePreSubmitSlotSnapshot(lucid, options.slotSnapshot),
        );
        if (refreshedSnapshot._tag === "Left") {
          return yield* Effect.fail(
            new Error(
              `Tx ${txHash} failed to refresh local submit slot evidence after inline wait: ${formatUnknownError(
                refreshedSnapshot.left,
                { includeCause: true },
              )}`,
            ),
          );
        }
        const afterWait = planSubmitTimingAfterInlineWait(
          preSubmitValidity,
          refreshedSnapshot.right,
        );
        if (afterWait.status !== "ready") {
          if (afterWait.status === "wait") {
            return yield* Effect.fail(
              new Error(
                `Tx ${txHash} local submit slot evidence still requires another wait after inline wait: currentSlot=${afterWait.currentSlot.toString()}, targetSlot=${afterWait.targetSlot.toString()}; rebuild required`,
              ),
            );
          }
          return yield* Effect.fail(
            submitTimingFailureError(txHash, afterWait),
          );
        }
      }
      const submitResult = yield* Effect.either(signed.submitProgram());
      if (submitResult._tag === "Right") {
        return;
      }

      const e = submitResult.left;
      const submitError = formatUnknownError(e, { includeCause: true });
      const outsideValidityDetails =
        parseOutsideValidityIntervalDetails(e) ??
        parseOutsideValidityIntervalDetails(submitError);
      if (outsideValidityDetails !== null) {
        if (
          outsideValidityRecoveryAttempts >= maxOutsideValidityRecoveryAttempts
        ) {
          return yield* Effect.fail(
            new Error(
              `Tx ${txHash} provider still reports OutsideValidityInterval after ${outsideValidityRecoveryAttempts.toString()} bounded early-validity recoveries (${outsideValidityRecoveryWaitedMs.toString()}ms waited): ${submitError}`,
            ),
          );
        }
        const signedValidity =
          inspectSignedTxValidityIntervalIfAvailable(signed);
        const invalidHereafterSlot =
          outsideValidityDetails.invalidHereafterSlot ??
          signedValidity?.invalidHereafterSlot;
        const slotSnapshotResult = yield* Effect.either(
          resolvePreSubmitSlotSnapshot(lucid, options.slotSnapshot),
        );
        const providerSnapshot: SubmitSlotSnapshot = {
          source: "test",
          currentSlot: outsideValidityDetails.currentSlot,
          observedAtMs: Date.now(),
          slotLengthMs: SLOT_LENGTH_MS,
        };
        const recoveryPlan = planSubmitTiming({
          invalidBeforeSlot: outsideValidityDetails.invalidBeforeSlot,
          invalidHereafterSlot,
          callerLabel: options.label ?? "submit",
          slotSnapshot:
            slotSnapshotResult._tag === "Right"
              ? slotSnapshotResult.right
              : providerSnapshot,
          slotSnapshotError:
            slotSnapshotResult._tag === "Left"
              ? slotSnapshotResult.left
              : undefined,
          submitSlotBuffer: EARLY_VALIDITY_RETRY_SLOT_BUFFER,
          maxInlineWaitMs:
            options.maxPreSubmitWaitMs ?? DEFAULT_SIGNED_TX_INLINE_WAIT_MS,
          inlineWaitPolicy: options.inlineWaitPolicy,
          dependencyKey: options.noInlineSubmitDefer?.dependencyKey,
          invalidationKey: options.noInlineSubmitDefer?.invalidationKey,
        });
        if (recoveryPlan.status === "ready") {
          const providerDetails = {
            ...outsideValidityDetails,
            ...(invalidHereafterSlot === undefined
              ? {}
              : { invalidHereafterSlot }),
          };
          const providerRetry = resolveEarlyValidityRetry(
            providerDetails,
            outsideValidityRecoveryAttempts,
            maxOutsideValidityRecoveryAttempts,
          );
          if (providerRetry.status === "wait") {
            const providerDefer = noInlineSubmitProviderSlotDefer({
              txHash,
              options,
              callerLabel: recoveryPlan.callerLabel,
              kind: "provider_slot_wait",
              currentSlot: outsideValidityDetails.currentSlot,
              targetSlot: providerRetry.targetSlot,
              waitMs: providerRetry.waitMs,
              invalidBeforeSlot: outsideValidityDetails.invalidBeforeSlot,
              invalidHereafterSlot,
            });
            if (providerDefer !== undefined) {
              return yield* Effect.fail(providerDefer);
            }
            const remainingWaitMs =
              maxOutsideValidityRecoveryWaitMs -
              outsideValidityRecoveryWaitedMs;
            if (
              providerRetry.waitMs > remainingWaitMs ||
              remainingWaitMs <= 0
            ) {
              return yield* Effect.fail(
                new Error(
                  `Tx ${txHash} stale Ogmios early-validity recovery would wait ${providerRetry.waitMs.toString()}ms, exceeding remaining bounded recovery budget=${Math.max(0, remainingWaitMs).toString()}ms after ${outsideValidityRecoveryAttempts.toString()} attempt(s) and ${outsideValidityRecoveryWaitedMs.toString()}ms waited; rebuild required`,
                ),
              );
            }
            outsideValidityRecoveryAttempts += 1;
            outsideValidityRecoveryWaitedMs += providerRetry.waitMs;
            yield* Effect.logWarning(
              [
                `Tx ${txHash} provider returned an early-validity error,`,
                `and its submit ledger slot is still behind the submit margin`,
                `(providerSlot=${outsideValidityDetails.currentSlot},`,
                `localSlot=${recoveryPlan.currentSlot ?? "unknown"},`,
                `targetSlot=${providerRetry.targetSlot},`,
                `invalidBefore=${outsideValidityDetails.invalidBeforeSlot},`,
                `invalidHereafter=${invalidHereafterSlot ?? "none"}).`,
                `Waiting ${providerRetry.waitMs.toString()}ms before bounded retry`,
                `${outsideValidityRecoveryAttempts.toString()}/${maxOutsideValidityRecoveryAttempts.toString()}`,
                `(waited=${outsideValidityRecoveryWaitedMs.toString()}ms/${maxOutsideValidityRecoveryWaitMs.toString()}ms).`,
              ].join(" "),
            );
            yield* sleep(providerRetry.waitMs);
            continue;
          }
          if (providerRetry.status === "attempts_exhausted") {
            return yield* Effect.fail(
              new Error(
                `Tx ${txHash} stale Ogmios early-validity recovery exhausted ${maxOutsideValidityRecoveryAttempts.toString()} attempts after ${outsideValidityRecoveryWaitedMs.toString()}ms waited; rebuild required`,
              ),
            );
          }
          if (
            providerRetry.status === "expired" ||
            providerRetry.status === "window_too_narrow"
          ) {
            return yield* Effect.fail(
              new Error(
                `Tx ${txHash} stale Ogmios early-validity recovery is not safe: status=${providerRetry.status}, providerSlot=${outsideValidityDetails.currentSlot.toString()}, invalidBefore=${outsideValidityDetails.invalidBeforeSlot.toString()}, invalidHereafter=${invalidHereafterSlot?.toString() ?? "none"}; rebuild required`,
              ),
            );
          }
          const remainingWaitMs =
            maxOutsideValidityRecoveryWaitMs - outsideValidityRecoveryWaitedMs;
          const staleProviderDefer = noInlineSubmitProviderSlotDefer({
            txHash,
            options,
            callerLabel: recoveryPlan.callerLabel,
            kind: "provider_slot_wait",
            currentSlot: outsideValidityDetails.currentSlot,
            targetSlot: outsideValidityDetails.currentSlot + 1,
            waitMs: SLOT_LENGTH_MS,
            invalidBeforeSlot: outsideValidityDetails.invalidBeforeSlot,
            invalidHereafterSlot,
          });
          if (staleProviderDefer !== undefined) {
            return yield* Effect.fail(staleProviderDefer);
          }
          if (SLOT_LENGTH_MS > remainingWaitMs || remainingWaitMs <= 0) {
            return yield* Effect.fail(
              new Error(
                `Tx ${txHash} stale Ogmios early-validity recovery would wait ${SLOT_LENGTH_MS.toString()}ms, exceeding remaining bounded recovery budget=${Math.max(0, remainingWaitMs).toString()}ms after ${outsideValidityRecoveryAttempts.toString()} attempt(s) and ${outsideValidityRecoveryWaitedMs.toString()}ms waited; rebuild required`,
              ),
            );
          }
          outsideValidityRecoveryAttempts += 1;
          outsideValidityRecoveryWaitedMs += SLOT_LENGTH_MS;
          yield* Effect.logWarning(
            [
              `Tx ${txHash} provider returned an early-validity error,`,
              `but local slot evidence now satisfies the submit margin`,
              `(slot=${recoveryPlan.currentSlot ?? outsideValidityDetails.currentSlot},`,
              `invalidBefore=${outsideValidityDetails.invalidBeforeSlot},`,
              `invalidHereafter=${invalidHereafterSlot ?? "none"}).`,
              `Waiting ${SLOT_LENGTH_MS.toString()}ms before bounded retry`,
              `${outsideValidityRecoveryAttempts.toString()}/${maxOutsideValidityRecoveryAttempts.toString()}`,
              `(waited=${outsideValidityRecoveryWaitedMs.toString()}ms/${maxOutsideValidityRecoveryWaitMs.toString()}ms).`,
            ].join(" "),
          );
          yield* sleep(SLOT_LENGTH_MS);
          continue;
        }
        const recoveryDefer = noInlineSubmitDeferFromTimingPlan(
          "early_validity_recovery",
          txHash,
          recoveryPlan,
          options,
        );
        if (recoveryDefer !== undefined) {
          return yield* Effect.fail(recoveryDefer);
        }
        if (recoveryPlan.status !== "wait") {
          return yield* Effect.fail(
            submitTimingFailureError(txHash, recoveryPlan),
          );
        }
        const remainingWaitMs =
          maxOutsideValidityRecoveryWaitMs - outsideValidityRecoveryWaitedMs;
        if (
          recoveryPlan.waitMs > remainingWaitMs ||
          remainingWaitMs <= 0 ||
          outsideValidityRecoveryAttempts >= maxOutsideValidityRecoveryAttempts
        ) {
          return yield* Effect.fail(
            new Error(
              `Tx ${txHash} early-validity recovery would wait ${recoveryPlan.waitMs.toString()}ms, exceeding bounded recovery budget=${Math.max(0, remainingWaitMs).toString()}ms after ${outsideValidityRecoveryAttempts.toString()} attempt(s) and ${outsideValidityRecoveryWaitedMs.toString()}ms waited; rebuild required`,
            ),
          );
        }
        outsideValidityRecoveryAttempts += 1;
        outsideValidityRecoveryWaitedMs += recoveryPlan.waitMs;
        yield* Effect.logWarning(
          [
            `Tx ${txHash} submitted before validity interval opened `,
            `(slot=${recoveryPlan.currentSlot},`,
            `invalidBefore=${recoveryPlan.invalidBeforeSlot},`,
            `invalidHereafter=${recoveryPlan.invalidHereafterSlot ?? "none"}).`,
            ` Waiting ${recoveryPlan.waitMs.toString()}ms before bounded retry`,
            `${outsideValidityRecoveryAttempts.toString()}/${maxOutsideValidityRecoveryAttempts.toString()}`,
            `(waited=${outsideValidityRecoveryWaitedMs.toString()}ms/${maxOutsideValidityRecoveryWaitMs.toString()}ms).`,
          ].join(" "),
        );
        yield* sleep(recoveryPlan.waitMs);
        continue;
      }

      if (isUnknownOutputReferenceSubmitError(e)) {
        yield* Effect.logWarning(
          `Tx submit reported unknown inputs for ${txHash}; verifying the exact transaction through provider-neutral status before failing: ${submitError}`,
        );
        const confirmation = yield* Effect.either(
          Effect.tryPromise(() =>
            awaitExactTransactionConfirmation(lucid, txHash, {
              timeout: SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS,
              checkInterval: SUBMIT_RECOVERY_POLL_INTERVAL_MS,
            }),
          ),
        );
        if (confirmation._tag === "Right") {
          yield* Effect.logInfo(
            `Tx ${txHash} confirmed after submit race; treating submission as successful.`,
          );
          return;
        }
        return yield* Effect.fail(e);
      }

      if (options.inlineWaitPolicy === "defer_positive_wait") {
        return yield* Effect.fail(
          new Error(
            `Tx ${txHash} submit failed with provider error in no-inline mode; refusing provider retry sleep under ownership: ${submitError}`,
          ),
        );
      }

      if (providerRetryAttempts < maxProviderRetryAttempts) {
        const waitMs = INIT_RETRY_AFTER_MILLIS * 2 ** providerRetryAttempts;
        providerRetryAttempts += 1;
        yield* Effect.logWarning(
          `Tx ${txHash} submit failed with provider error; waiting ${waitMs}ms before retry ${providerRetryAttempts}/${maxProviderRetryAttempts.toString()}: ${submitError}`,
        );
        yield* sleep(waitMs);
        continue;
      }

      return yield* Effect.fail(e);
    }
  });

/**
 * Handle the signing and submission of a transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: NoInlineSubmitRecoveryOptions,
): Effect.Effect<
  string,
  TxSignError | TxSubmitError | TxConfirmError | NoInlineSubmitDefer
>;
export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options?: SubmitRecoveryOptions,
): Effect.Effect<string, TxSignError | TxSubmitError | TxConfirmError>;
export function handleSignSubmit(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  string,
  TxSignError | TxSubmitError | TxConfirmError | NoInlineSubmitDefer
> {
  return Effect.gen(function* () {
    const submission = yield* signSubmitTransaction(
      lucid,
      signBuilder,
      options,
    );
    return yield* awaitSubmittedTransactionConfirmation(
      lucid,
      submission,
      options,
    );
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
  );
}

export const awaitSubmittedTransactionConfirmation = (
  lucid: LucidEvolution,
  submission: SignSubmitContext,
  options: Pick<
    SubmitRecoveryInlineOptions,
    | "confirmationTimeoutMs"
    | "confirmationRetries"
    | "confirmationPollIntervalMs"
  > = {},
): Effect.Effect<string, TxConfirmError> =>
  Effect.gen(function* () {
    const txHash = submission.txHash;
    const confirmationTimeoutMs =
      options.confirmationTimeoutMs ?? TX_CONFIRMATION_TIMEOUT_MS;
    const confirmationRetries =
      options.confirmationRetries ?? TX_CONFIRMATION_RETRIES;
    const confirmationPollIntervalMs =
      options.confirmationPollIntervalMs ?? TX_CONFIRMATION_POLL_INTERVAL_MS;
    if (
      !Number.isSafeInteger(confirmationTimeoutMs) ||
      confirmationTimeoutMs <= 0 ||
      !Number.isSafeInteger(confirmationRetries) ||
      confirmationRetries < 0 ||
      !Number.isSafeInteger(confirmationPollIntervalMs) ||
      confirmationPollIntervalMs <= 0
    ) {
      return yield* Effect.fail(
        new TxConfirmError({
          message: "Invalid transaction confirmation options",
          txHash,
          cause: `timeout_ms=${confirmationTimeoutMs.toString()},retries=${confirmationRetries.toString()},poll_interval_ms=${confirmationPollIntervalMs.toString()}`,
        }),
      );
    }
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    const awaitWithTimeout = Effect.tryPromise({
      try: () =>
        awaitExactTransactionConfirmation(lucid, txHash, {
          timeout: confirmationTimeoutMs,
          checkInterval: confirmationPollIntervalMs,
        }),
      catch: (e) =>
        new TxConfirmError({
          message: `Failed to confirm transaction`,
          txHash,
          cause: e,
        }),
    }).pipe(
      Effect.retry(
        Schedule.intersect(
          Schedule.fixed(Duration.millis(confirmationPollIntervalMs)),
          Schedule.recurs(confirmationRetries),
        ),
      ),
    );

    yield* awaitWithTimeout;
    yield* reconcileWalletUtxosFromSignedTx(lucid, submission);
    yield* Effect.logInfo(`🎉 Transaction confirmed: ${txHash}`);
    yield* Effect.logInfo(`⌛ Pausing for ${PAUSE_DURATION}...`);
    yield* Effect.sleep(PAUSE_DURATION);
    yield* Effect.logInfo("✅ Pause ended.");
    return txHash;
  });

/**
 * Handle the signing and submission of a transaction without waiting for the
 * transaction to be confirmed.
 *
 * @param lucid - The LucidEvolution instance. Here it's only used for logging the signer's address.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: NoInlineSubmitRecoveryOptions,
): Effect.Effect<SignSubmitNoConfirmationResult, TxSignError | TxSubmitError>;
export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options?: SubmitRecoveryOptions,
): Effect.Effect<string, TxSignError | TxSubmitError>;
export function handleSignSubmitNoConfirmation(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  string | SignSubmitNoConfirmationResult,
  TxSignError | TxSubmitError | NoInlineSubmitDefer
> {
  const returnNoInlineDefer =
    options.inlineWaitPolicy === "defer_positive_wait";
  return Effect.gen(function* () {
    const submissionResult = yield* Effect.either(
      signSubmitTransactionWithDefer(lucid, signBuilder, options),
    );
    if (submissionResult._tag === "Left") {
      const error = submissionResult.left;
      if (isNoInlineSubmitDefer(error) && returnNoInlineDefer) {
        return {
          status: "deferred",
          defer: error,
        } satisfies SignSubmitNoConfirmationResult;
      }
      return yield* Effect.fail(error);
    }
    const submission = submissionResult.right;
    if (returnNoInlineDefer) {
      return {
        status: "submitted",
        txHash: submission.txHash,
      } satisfies SignSubmitNoConfirmationResult;
    }
    return submission.txHash;
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
  );
}

/**
 * Shared implementation used by the confirmation and no-confirmation sign/
 * submit entrypoints.
 */
export function signSubmitTransaction(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions & {
    readonly inlineWaitPolicy: "defer_positive_wait";
  },
): Effect.Effect<
  SignSubmitContext,
  TxSubmitError | TxSignError | NoInlineSubmitDefer
>;
export function signSubmitTransaction(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options?: SubmitRecoveryOptions,
): Effect.Effect<SignSubmitContext, TxSubmitError | TxSignError>;
export function signSubmitTransaction(
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  SignSubmitContext,
  TxSubmitError | TxSignError | NoInlineSubmitDefer
> {
  return signSubmitTransactionWithDefer(lucid, signBuilder, options);
}

const signSubmitTransactionWithDefer = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<
  SignSubmitContext,
  TxSubmitError | TxSignError | NoInlineSubmitDefer
> =>
  Effect.gen(function* () {
    const walletAddr = yield* Effect.tryPromise(() =>
      lucid.wallet().address(),
    ).pipe(Effect.catchAll((_e) => Effect.succeed("<unknown>")));
    yield* Effect.logInfo(`✍  Signing tx with ${walletAddr}`);
    const txHash = signBuilder.toHash();
    const signedProgram = signBuilder.sign
      .withWallet()
      .completeProgram()
      .pipe(
        Effect.tapError((e) => Effect.logError(e)),
        Effect.mapError(
          (e) =>
            new TxSignError({
              message: `Failed to sign transaction`,
              cause: e,
              txHash,
            }),
        ),
      );
    const signed = yield* signedProgram;
    const signedTxCbor = signed.toCBOR();
    yield* Effect.logInfo(
      `✍  Signed tx prepared: txHash=${txHash}, cborBytes=${signedTxCbor.length / 2}`,
    );
    yield* Effect.logInfo("✉️  Submitting transaction...");
    yield* submitSignedTxWithRecovery(lucid, signed, txHash, options).pipe(
      Effect.tapError((e) =>
        isNoInlineSubmitDefer(e)
          ? Effect.void
          : Effect.logError(
              `Tx submission provider error for ${txHash}: ${formatUnknownError(
                e,
                {
                  includeCause: true,
                },
              )}`,
            ),
      ),
      Effect.mapError((e) =>
        isNoInlineSubmitDefer(e)
          ? e
          : new TxSubmitError({
              message: `Failed to submit transaction: ${formatUnknownError(e, {
                includeCause: true,
              })}`,
              cause: e,
              txHash,
            }),
      ),
    );
    yield* Effect.logInfo(`🚀 Transaction submitted: ${txHash}`);
    return {
      txHash,
      signedTxCbor,
      walletAddress: walletAddr,
    };
  });

/**
 * Fetch transactions of the first block by querying BlocksDB and ImmutableDB.
 *
 * @param firstBlockUTxO - UTxO of the first block in queue.
 * @returns An Effect that resolves to an array of transactions, and block's
 *          header hash.
 */
export const fetchFirstBlockTxs = (
  firstBlockUTxO: SDK.StateQueueUTxO,
): Effect.Effect<
  {
    txs: readonly BlockTxPayload[];
    txHashes: readonly Buffer[];
    headerHash: Buffer;
  },
  SDK.HashingError | SDK.DataCoercionError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const blockHeader = yield* SDK.getHeaderFromStateQueueDatum(
      firstBlockUTxO.datum,
    );
    const headerHash: Buffer = yield* SDK.hashBlockHeader(blockHeader).pipe(
      Effect.map((hh) => Buffer.from(fromHex(hh))),
    );
    const txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    const txEntries = yield* ImmutableDB.retrieveTxEntriesByHashes(txHashes);
    const txById = new Map<string, Buffer>();
    for (const entry of txEntries) {
      txById.set(entry.tx_id.toString("hex"), entry.tx);
    }
    const txs: BlockTxPayload[] = [];
    for (const txHash of txHashes) {
      const txCbor = txById.get(txHash.toString("hex"));
      if (txCbor !== undefined) {
        txs.push({
          txId: txHash,
          txCbor,
        });
      }
    }
    return { txs, txHashes, headerHash };
  });

export class TxSignError extends Data.TaggedError("TxSignError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class TxSubmitError extends Data.TaggedError("TxSubmitError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class TxConfirmError extends Data.TaggedError("TxConfirmError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}
