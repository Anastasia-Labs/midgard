import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  isEmulatorProvider,
  LucidEvolution,
  UTxO,
} from "@lucid-evolution/lucid";
import { Context, Data, Duration, Effect, Schedule } from "effect";

import type { JournalInsert } from "../services/intent-journal.js";
import {
  compactValidityInterval,
  type SignedTxValidityInterval,
  slotNumber,
} from "./utils.parse-structured-outside-validity-interval-details.js";

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
export const outRefToKey = (txHash: string, outputIndex: number): string =>
  `${txHash}#${outputIndex.toString()}`;

const outputDatumMatches = (
  expected: Pick<UTxO, "datum" | "datumHash">,
  actual: Pick<UTxO, "datum" | "datumHash">,
): boolean => {
  const hash = (datum: string) =>
    CML.hash_plutus_data(CML.PlutusData.from_cbor_hex(datum)).to_hex();
  if (expected.datum != null)
    return (
      actual.datum === expected.datum &&
      (actual.datumHash == null || actual.datumHash === hash(expected.datum))
    );
  if (expected.datumHash != null)
    return (
      actual.datumHash === expected.datumHash &&
      (actual.datum == null || hash(actual.datum) === expected.datumHash)
    );
  return actual.datum == null && actual.datumHash == null;
};

/**
 * Confirmation and provider output visibility are separate dependencies. Only
 * gate outputs the caller needs next: unrelated payments can already be spent.
 * Query the provider, never the locally overridden wallet snapshot.
 */
export const awaitRequiredOutputVisibility = (
  lucid: LucidEvolution,
  submission: SignSubmitContext,
  indexes: readonly number[],
  timeoutMs: number,
  pollIntervalMs: number,
): Effect.Effect<void, TxConfirmError> =>
  Effect.gen(function* () {
    if (indexes.length === 0) return;
    const expected = yield* Effect.try({
      try: () => {
        const transaction = CML.Transaction.from_cbor_hex(
          submission.signedTxCbor,
        );
        if (
          CML.hash_transaction(transaction.body()).to_hex() !==
          submission.txHash
        )
          throw new Error(
            "Required outputs do not belong to the confirmed signed transaction",
          );
        const outputs = transaction.body().outputs();
        return [...new Set(indexes)].map((outputIndex) => {
          if (
            !Number.isSafeInteger(outputIndex) ||
            outputIndex < 0 ||
            outputIndex >= outputs.len()
          )
            throw new Error(
              `Required output index ${outputIndex} is outside the signed transaction`,
            );
          return {
            ...coreToTxOutput(outputs.get(outputIndex)),
            txHash: submission.txHash,
            outputIndex,
          };
        });
      },
      catch: (cause) =>
        new TxConfirmError({
          message: "Invalid required outputs for confirmed transaction",
          txHash: submission.txHash,
          cause,
        }),
    });
    const refs = expected.map(({ txHash, outputIndex }) => ({
      txHash,
      outputIndex,
    }));
    const ready = Effect.tryPromise({
      try: async () => {
        const visible = await lucid.utxosByOutRef(refs);
        return expected.every((output) =>
          visible.some(
            (candidate) =>
              candidate.txHash === output.txHash &&
              candidate.outputIndex === output.outputIndex &&
              candidate.address === output.address &&
              Object.keys(candidate.assets).length ===
                Object.keys(output.assets).length &&
              Object.entries(output.assets).every(
                ([unit, amount]) => candidate.assets[unit] === amount,
              ) &&
              outputDatumMatches(output, candidate) &&
              candidate.scriptRef?.type === output.scriptRef?.type &&
              candidate.scriptRef?.script === output.scriptRef?.script,
          ),
        );
      },
      catch: (cause) =>
        new TxConfirmError({
          message: "Failed to query required transaction outputs",
          txHash: submission.txHash,
          cause,
        }),
    }).pipe(
      Effect.flatMap((visible) =>
        visible
          ? Effect.void
          : Effect.fail(
              new TxConfirmError({
                message: "Required transaction outputs are not yet visible",
                txHash: submission.txHash,
                cause: refs
                  .map(({ txHash, outputIndex }) =>
                    outRefToKey(txHash, outputIndex),
                  )
                  .join(","),
              }),
            ),
      ),
    );
    yield* ready.pipe(
      Effect.retry(Schedule.spaced(Duration.millis(pollIntervalMs))),
      Effect.timeoutFail({
        duration: Duration.millis(timeoutMs),
        onTimeout: () =>
          new TxConfirmError({
            message:
              "Transaction confirmed but required outputs did not become visible before the provider deadline; reconcile the confirmed transaction before rebuilding",
            txHash: submission.txHash,
            cause: `timeout_ms=${timeoutMs},required_outputs=${refs.map(({ txHash, outputIndex }) => outRefToKey(txHash, outputIndex)).join(",")}`,
          }),
      }),
    );
  });

export type NoInlineSubmitDeferKind =
  | "pre_submit_validity"
  | "early_validity_recovery"
  | "provider_slot_wait";

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

/**
 * Submits a signed transaction with recovery logic for provider races and
 * early-validity-window failures.
 */
export const BeforeSignedTransactionSubmission = Context.GenericTag<{
  /**
   * The workflow's pre-broadcast gate (`PreBroadcastGate`): its checks and
   * durable write, in one SQL transaction it opens itself as the outermost
   * one, with `journal` (the intent journal's insert of these bytes) run
   * inside that same transaction. A refusal, or a stop before the commit,
   * leaves neither the write nor the journal row.
   */
  readonly persist: (
    intent: Readonly<{
      txHash: string;
      signedTxCbor: string;
      journal: JournalInsert;
    }>,
  ) => Effect.Effect<void, unknown>;
}>("midgard/BeforeSignedTransactionSubmission");

export class TxSignError extends Data.TaggedError("TxSignError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}

export class TxSubmitError extends Data.TaggedError("TxSubmitError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
    /** The slot a no-inline defer is due at, when the submit was deferred. */
    readonly dueSlot?: number;
  }
> {}

export class TxConfirmError extends Data.TaggedError("TxConfirmError")<
  SDK.GenericErrorFields & {
    readonly txHash: string;
  }
> {}
