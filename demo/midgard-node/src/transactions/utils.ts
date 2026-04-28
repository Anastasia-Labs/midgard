import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  LucidEvolution,
  OutRef,
  TxSignBuilder,
  UTxO,
  coreToTxOutput,
  fromHex,
} from "@lucid-evolution/lucid";
import { Data, Effect, Schedule } from "effect";
import * as BlocksDB from "@/database/blocks.js";
import { Database } from "@/services/index.js";
import { ImmutableDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";

/**
 * Shared transaction signing, submission, confirmation, and recovery helpers.
 *
 * These utilities centralize the messy provider-facing parts of transaction
 * handling so higher-level transaction builders can stay focused on protocol
 * logic.
 */
const RETRY_ATTEMPTS = 1;

const INIT_RETRY_AFTER_MILLIS = 2_000;

const PAUSE_DURATION = "1 seconds";
const TX_CONFIRMATION_TIMEOUT_MS = 90_000;
const TX_CONFIRMATION_RETRIES = 1;
const TX_CONFIRMATION_POLL_INTERVAL_MS = 5_000;
const SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS = 90_000;
const SUBMIT_RECOVERY_POLL_INTERVAL_MS = 5_000;
const EARLY_VALIDITY_RETRY_MAX_ATTEMPTS = 6;
const EARLY_VALIDITY_RETRY_SLOT_BUFFER = 2;
const SLOT_LENGTH_MS = 1_000;

const ALREADY_INCLUDED_ERROR_PATTERNS = [
  "All inputs are spent. Transaction has probably already been included",
  "ValueNotConservedUTxO",
  "BadInputsUTxO",
] as const;

/**
 * Returns whether a provider error looks like a transaction that may already
 * have landed on-chain despite the submit call failing.
 */
const isPotentiallyAlreadyIncludedError = (message: string): boolean =>
  ALREADY_INCLUDED_ERROR_PATTERNS.some((pattern) => message.includes(pattern));

const OUTSIDE_VALIDITY_INTERVAL_REGEX =
  /OutsideValidityIntervalUTxO \(ValidityInterval \{invalidBefore = SJust \(SlotNo (\d+)\), invalidHereafter = SJust \(SlotNo (\d+)\)\}\) \(SlotNo (\d+)\)/;
const EMULATOR_LOWER_BOUND_OUTSIDE_VALIDITY_REGEX =
  /Lower bound \((\d+)\) not in slot range \((\d+)\)/;
const OGMIOS_OUTSIDE_VALIDITY_INTERVAL_REGEX =
  /"validityInterval"\s*:\s*\{\s*"invalidBefore"\s*:\s*(\d+)\s*,\s*"invalidAfter"\s*:\s*(\d+)\s*\}\s*,\s*"currentSlot"\s*:\s*(\d+)/;

type OutsideValidityIntervalDetails = {
  readonly invalidBeforeSlot: number;
  readonly invalidHereafterSlot: number;
  readonly currentSlot: number;
};

/**
 * Extracts slot-boundary details from an `OutsideValidityIntervalUTxO` error.
 */
const parseOutsideValidityIntervalDetails = (
  message: string,
): OutsideValidityIntervalDetails | null => {
  const match = OUTSIDE_VALIDITY_INTERVAL_REGEX.exec(message);
  if (match !== null) {
    const invalidBeforeSlot = Number(match[1]);
    const invalidHereafterSlot = Number(match[2]);
    const currentSlot = Number(match[3]);
    if (
      !Number.isFinite(invalidBeforeSlot) ||
      !Number.isFinite(invalidHereafterSlot) ||
      !Number.isFinite(currentSlot)
    ) {
      return null;
    }
    return {
      invalidBeforeSlot,
      invalidHereafterSlot,
      currentSlot,
    };
  }

  const emulatorLowerBoundMatch =
    EMULATOR_LOWER_BOUND_OUTSIDE_VALIDITY_REGEX.exec(message);
  if (emulatorLowerBoundMatch === null) {
    const ogmiosMatch = OGMIOS_OUTSIDE_VALIDITY_INTERVAL_REGEX.exec(message);
    if (ogmiosMatch === null) {
      return null;
    }
    const invalidBeforeSlot = Number(ogmiosMatch[1]);
    const invalidHereafterSlot = Number(ogmiosMatch[2]);
    const currentSlot = Number(ogmiosMatch[3]);
    if (
      !Number.isFinite(invalidBeforeSlot) ||
      !Number.isFinite(invalidHereafterSlot) ||
      !Number.isFinite(currentSlot)
    ) {
      return null;
    }
    return {
      invalidBeforeSlot,
      invalidHereafterSlot,
      currentSlot,
    };
  }
  const invalidBeforeSlot = Number(emulatorLowerBoundMatch[1]);
  const currentSlot = Number(emulatorLowerBoundMatch[2]);
  if (
    !Number.isFinite(invalidBeforeSlot) ||
    !Number.isFinite(currentSlot)
  ) {
    return null;
  }
  return {
    invalidBeforeSlot,
    invalidHereafterSlot: Number.MAX_SAFE_INTEGER,
    currentSlot,
  };
};

type SignSubmitContext = {
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly walletAddress: string;
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

/**
 * Converts an unknown submit failure into a stable log/error string.
 */
const formatSubmitError = (error: unknown): string => {
  if (error instanceof Error) {
    const cause = (error as Error & { cause?: unknown }).cause;
    if (cause === undefined) {
      return `${error.name}: ${error.message}`;
    }
    return `${error.name}: ${error.message}; cause=${formatSubmitError(cause)}`;
  }
  if (typeof error === "string") {
    return error;
  }
  try {
    return JSON.stringify(error);
  } catch {
    return String(error);
  }
};

/**
 * Submits a signed transaction with recovery logic for provider races and
 * early-validity-window failures.
 */
const submitSignedTxWithRecovery = (
  lucid: LucidEvolution,
  signed: Awaited<ReturnType<TxSignBuilder["complete"]>>,
  txHash: string,
  attempt: number = 0,
): Effect.Effect<void, unknown> =>
  signed.submitProgram().pipe(
    Effect.catchAll((e) =>
      Effect.gen(function* () {
        const submitError = formatSubmitError(e);
        if (isPotentiallyAlreadyIncludedError(submitError)) {
          yield* Effect.logWarning(
            `Tx submit returned an already-included style error for ${txHash}; verifying on-chain confirmation before failing: ${submitError}`,
          );
          const confirmed = yield* Effect.tryPromise({
            try: () =>
              new Promise<boolean>((resolve, reject) => {
                const timeoutId = setTimeout(() => {
                  reject(
                    new Error(
                      `submit-recovery confirmation timeout after ${SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS}ms`,
                    ),
                  );
                }, SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS);
                lucid
                  .awaitTx(txHash, SUBMIT_RECOVERY_POLL_INTERVAL_MS)
                  .then((result) => {
                    clearTimeout(timeoutId);
                    resolve(result);
                  })
                  .catch((error) => {
                    clearTimeout(timeoutId);
                    reject(error);
                  });
              }),
            catch: (cause) =>
              new Error(
                `submit-recovery confirmation check failed for ${txHash}: ${formatSubmitError(cause)}`,
              ),
          });
          if (!confirmed) {
            return yield* Effect.fail(e);
          }
          yield* Effect.logInfo(
            `Tx ${txHash} confirmed after submit race; treating submission as successful.`,
          );
          return;
        }

        const outsideValidityDetails =
          parseOutsideValidityIntervalDetails(submitError);
        if (
          outsideValidityDetails !== null &&
          outsideValidityDetails.currentSlot <
            outsideValidityDetails.invalidBeforeSlot &&
          attempt < EARLY_VALIDITY_RETRY_MAX_ATTEMPTS
        ) {
          const slotsUntilValid =
            outsideValidityDetails.invalidBeforeSlot -
            outsideValidityDetails.currentSlot;
          const validityWindowSlots =
            outsideValidityDetails.invalidHereafterSlot -
            outsideValidityDetails.invalidBeforeSlot;
          const reportedSlotLagExceedsValidityWindow =
            validityWindowSlots > 0 && slotsUntilValid > validityWindowSlots;
          const retryWaitSlots = reportedSlotLagExceedsValidityWindow
            ? EARLY_VALIDITY_RETRY_SLOT_BUFFER
            : slotsUntilValid + EARLY_VALIDITY_RETRY_SLOT_BUFFER;
          const waitMs =
            retryWaitSlots * SLOT_LENGTH_MS;
          yield* Effect.logWarning(
            [
              `Tx ${txHash} submitted before validity interval opened `,
              `(slot=${outsideValidityDetails.currentSlot},`,
              `invalidBefore=${outsideValidityDetails.invalidBeforeSlot},`,
              `invalidHereafter=${outsideValidityDetails.invalidHereafterSlot}).`,
              reportedSlotLagExceedsValidityWindow
                ? " Reported slot lag exceeds validity window; using bounded retry delay."
                : "",
              ` Waiting ${waitMs}ms and retrying submit `,
              `(${attempt + 1}/${EARLY_VALIDITY_RETRY_MAX_ATTEMPTS}).`,
            ].join(" "),
          );
          yield* Effect.sleep(waitMs);
          return yield* submitSignedTxWithRecovery(
            lucid,
            signed,
            txHash,
            attempt + 1,
          );
        }

        return yield* Effect.fail(e);
      }),
    ),
  );

/**
 * Handle the signing and submission of a transaction.
 *
 * @param lucid - The LucidEvolution instance.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmit = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<string, TxSignError | TxSubmitError | TxConfirmError> =>
  Effect.gen(function* () {
    const submission = yield* signSubmitHelper(lucid, signBuilder);
    const txHash = submission.txHash;
    yield* Effect.logInfo(`⏳ Confirming Transaction...`);
    const awaitWithTimeout = Effect.tryPromise({
      try: () =>
        new Promise<boolean>((resolve, reject) => {
          const timeoutId = setTimeout(() => {
            reject(
              new Error(
                `timed out waiting for tx confirmation after ${TX_CONFIRMATION_TIMEOUT_MS}ms`,
              ),
            );
          }, TX_CONFIRMATION_TIMEOUT_MS);

          lucid
            .awaitTx(txHash, TX_CONFIRMATION_POLL_INTERVAL_MS)
            .then((result) => {
              clearTimeout(timeoutId);
              resolve(result);
            })
            .catch((error) => {
              clearTimeout(timeoutId);
              reject(error);
            });
        }),
      catch: (e) =>
        new TxConfirmError({
          message: `Failed to confirm transaction`,
          txHash,
          cause: e,
        }),
    }).pipe(Effect.retry(Schedule.recurs(TX_CONFIRMATION_RETRIES)));

    yield* awaitWithTimeout;
    yield* reconcileWalletUtxosFromSignedTx(lucid, submission);
    yield* Effect.logInfo(`🎉 Transaction confirmed: ${txHash}`);
    yield* Effect.logInfo(`⌛ Pausing for ${PAUSE_DURATION}...`);
    yield* Effect.sleep(PAUSE_DURATION);
    yield* Effect.logInfo("✅ Pause ended.");
    return txHash;
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
  );

/**
 * Handle the signing and submission of a transaction without waiting for the
 * transaction to be confirmed.
 *
 * @param lucid - The LucidEvolution instance. Here it's only used for logging the signer's address.
 * @param signBuilder - The transaction sign builder.
 * @returns An Effect that resolves when the transaction is signed, submitted, and confirmed.
 */
export const handleSignSubmitNoConfirmation = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<string, TxSignError | TxSubmitError> =>
  Effect.gen(function* () {
    const submission = yield* signSubmitHelper(lucid, signBuilder);
    return submission.txHash;
  }).pipe(
    Effect.tapErrorTag("TxSignError", (e) =>
      Effect.logError(`TxSignError: ${e}`),
    ),
  );

/**
 * Shared implementation used by the confirmation and no-confirmation sign/
 * submit entrypoints.
 */
const signSubmitHelper = (
  lucid: LucidEvolution,
  signBuilder: TxSignBuilder,
): Effect.Effect<SignSubmitContext, TxSubmitError | TxSignError> =>
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
    yield* submitSignedTxWithRecovery(lucid, signed, txHash).pipe(
      Effect.retry(
        Schedule.compose(
          Schedule.exponential(INIT_RETRY_AFTER_MILLIS),
          Schedule.recurs(RETRY_ATTEMPTS),
        ),
      ),
      Effect.tapError((e) =>
        Effect.logError(
          `Tx submission provider error for ${txHash}: ${formatSubmitError(e)}`,
        ),
      ),
      Effect.mapError(
        (e) =>
          new TxSubmitError({
            message: `Failed to submit transaction: ${formatSubmitError(e)}`,
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

/**
 * Projects a full UTxO into its outref-only form.
 */
export const utxoToOutRef = (utxo: UTxO): OutRef => ({
  txHash: utxo.txHash,
  outputIndex: utxo.outputIndex,
});

/**
 * Returns whether two outrefs point to the same transaction output.
 */
export const outRefsAreEqual = (outRef0: OutRef, outRef1: OutRef): boolean => {
  return (
    outRef0.txHash === outRef1.txHash &&
    outRef0.outputIndex === outRef1.outputIndex
  );
};

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

export class GenesisDepositError extends Data.TaggedError(
  "GenesisDepositError",
)<SDK.GenericErrorFields> {}
