import {
  CML,
  coreToTxOutput,
  LucidEvolution,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type SubmitSlotSnapshot } from "../local-ledger-slot.js";
import {
  type InlineWaitPolicy,
  type SubmitTimingPlan,
} from "./submit-timing.js";
import {
  NoInlineSubmitDefer,
  type NoInlineSubmitDeferKind,
  outRefToKey,
  type SignSubmitContext,
} from "./utils.await-required-output-visibility.js";
import {
  SLOT_LENGTH_MS,
  slotNumber,
} from "./utils.parse-structured-outside-validity-interval-details.js";

/**
 * Reconciles Lucid's local wallet view after a confirmed transaction.
 *
 * This is mainly relevant for local wallets/providers that expose
 * `overrideUTxOs`, allowing later transactions to reuse the updated wallet view
 * without waiting for an external provider refresh.
 */
export const reconcileWalletUtxosFromSignedTx = (
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
  /** Signed output indexes needed by the next operation; unrelated outputs are not gated. */
  readonly requiredOutputIndexes?: readonly number[];

  readonly label?: string;
  readonly sleep?: (milliseconds: number) => Effect.Effect<void, never>;
  readonly slotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>;
  readonly requireSlotForBoundedTx?: boolean;
  readonly maxPreSubmitWaitMs?: number;
  readonly confirmationTimeoutMs?: number;
  readonly confirmationRetries?: number;
  readonly confirmationPollIntervalMs?: number;
  /**
   * Unix time (ms) at which the whole confirmation wait, retries included,
   * fails with TxConfirmError whatever retries remain.
   */
  readonly confirmationDeadlineMs?: number;
  readonly inlineWaitPolicy?: Extract<InlineWaitPolicy, "allow_inline_wait">;
  readonly noInlineSubmitDefer?: never;
};

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
  /**
   * Fail at once when the provider reports unknown inputs, without waiting
   * inline for the exact transaction: the caller retains a durable signed
   * intent that its own reconciliation resolves (a commit block).
   */
  readonly unknownInputsFailFast?: boolean;
};

export type SubmitRecoveryOptions =
  | SubmitRecoveryInlineOptions
  | NoInlineSubmitRecoveryOptions;

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

export const submitRecoverySleep = (
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

export const resolvePreSubmitSlotSnapshot = (
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

export const noInlineSubmitDeferFromTimingPlan = (
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

export const noInlineSubmitProviderSlotDefer = ({
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
