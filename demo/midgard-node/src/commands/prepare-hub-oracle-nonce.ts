import { randomUUID } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { L1Origin } from "@al-ft/midgard-core/l1-origin";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  coreToTxOutput,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { IntentJournal } from "../services/intent-journal.js";
import { unjournaledSubmission } from "../services/intent-journal.js";
import { Lucid } from "../services/lucid.js";
import { awaitRequiredOutputVisibility } from "../transactions/utils.await-required-output-visibility.js";
import {
  awaitExactTransactionConfirmation,
  awaitSubmittedTransactionConfirmation,
  BeforeSignedTransactionSubmission,
  signSubmitTransaction,
  TxConfirmError,
} from "../transactions/utils.js";
import {
  TX_OUTPUT_VISIBILITY_POLL_INTERVAL_MS,
  TX_OUTPUT_VISIBILITY_TIMEOUT_MS,
} from "../transactions/utils.parse-structured-outside-validity-interval-details.js";
import {
  type HubOracleNonceOriginSource,
  noLocalNodeOriginError,
  settledL1Origin,
} from "./prepare-hub-oracle-nonce.l1-origin.js";
import { resumeSignedHubOracleNonceTransaction } from "./prepare-hub-oracle-nonce.resume-signed.js";

export const DEFAULT_NONCE_LOVELACE = 5_000_000n;
const DEFAULT_CONFIRMATION_RECONCILE_TIMEOUT_MS = 300_000;
const DEFAULT_NONCE_RECOVERY_POLL_INTERVAL_MS = 5_000;

export type HubOracleNonceConfirmationStatus =
  | "confirmed"
  | "reconciled_after_timeout";

export type PreparedHubOracleNonce = {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly outRef: string;
  readonly address: string;
  readonly lovelace: string;
  readonly inlineDatum: string;
  readonly confirmationStatus: HubOracleNonceConfirmationStatus;
  /** The deployment's L1 origin, recorded before the nonce was first signed. */
  readonly l1Origin?: L1Origin;
};

export type SubmittedHubOracleNonceAttempt = {
  readonly txHash: string;
  readonly address: string;
  readonly lovelace: string;
  readonly inlineDatum: string;
  /** Present when the attempt was recorded before its first submission. */
  readonly signedTxCbor?: string;
  /** The L1 origin recorded with the signed bytes, when one was. */
  readonly l1Origin?: L1Origin;
};

export type SignedHubOracleNonceAttempt = SubmittedHubOracleNonceAttempt & {
  readonly signedTxCbor: string;
  readonly l1Origin: L1Origin;
};

export type PrepareHubOracleNonceOptions = {
  readonly confirmationReconcileTimeoutMs?: number;
  readonly pollIntervalMs?: number;
  /**
   * Where a fresh nonce reads the deployment's L1 origin; without it the
   * command refuses before the wallet is used.
   */
  readonly l1Origin?: HubOracleNonceOriginSource;
  /** Must durably record the signed transaction; a failure blocks submission. */
  readonly beforeSubmission?: (
    attempt: SignedHubOracleNonceAttempt,
  ) => Effect.Effect<void, unknown>;
  readonly onSubmitted?: (
    attempt: SubmittedHubOracleNonceAttempt,
  ) => Effect.Effect<void, unknown>;
  readonly onTxHashConfirmed?: (
    attempt: SubmittedHubOracleNonceAttempt,
    confirmationStatus: HubOracleNonceConfirmationStatus,
  ) => Effect.Effect<void, unknown>;
};

export type OperatorWalletUtxoSummary = {
  readonly outRef: string;
  readonly lovelace: string;
  readonly assetCount: number;
  readonly hasDatum: boolean;
  readonly hasScriptRef: boolean;
};

export type OperatorWalletNonceReadiness = {
  readonly address: string;
  readonly requestedNonceLovelace: string;
  readonly spendableUtxos: readonly OperatorWalletUtxoSummary[];
  readonly totalSpendableLovelace: string;
};

export const parseNonceLovelaceOption = (value: string): bigint => {
  const trimmed = value.trim();
  if (!/^\d+$/.test(trimmed)) {
    throw new Error("--amount-lovelace must be a positive integer");
  }
  const parsed = BigInt(trimmed);
  if (parsed <= 0n) {
    throw new Error("--amount-lovelace must be greater than zero");
  }
  return parsed;
};

const makeHubOracleOneShotNonceMarkerHex = (): string =>
  Buffer.from(
    `${SDK.HUB_ORACLE_ONE_SHOT_NONCE_DATUM_DOMAIN}:${Date.now().toString(
      10,
    )}:${randomUUID()}`,
    "utf8",
  ).toString("hex");

const boundedMs = (value: number | undefined, fallback: number): number =>
  value === undefined || !Number.isFinite(value) || value < 0
    ? fallback
    : Math.trunc(value);

const isConfirmationTimeout = (error: TxConfirmError): boolean =>
  /timed out waiting for (?:(?:nonce )?tx confirmation|transaction)/i.test(
    formatUnknownError(error.cause, { includeCause: true }),
  );

const awaitTxHashConfirmation = ({
  lucid,
  txHash,
  timeoutMs,
  pollIntervalMs,
}: {
  readonly lucid: LucidEvolution;
  readonly txHash: string;
  readonly timeoutMs: number;
  readonly pollIntervalMs: number;
}): Effect.Effect<void, TxConfirmError> =>
  Effect.tryPromise({
    try: () =>
      awaitExactTransactionConfirmation(lucid, txHash, {
        timeout: timeoutMs,
        checkInterval: pollIntervalMs,
      }).then(() => undefined),
    catch: (cause) =>
      new TxConfirmError({
        message:
          "Failed to reconcile hub-oracle nonce transaction confirmation",
        txHash,
        cause,
      }),
  });

/**
 * The nonce's index in its signed transaction (the output carrying the
 * attempt's marked datum), once that output reads back by exact outref.
 */
const confirmedNonceOutputIndex = ({
  lucid,
  txHash,
  signedTxCbor,
  inlineDatum,
}: {
  readonly lucid: LucidEvolution;
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly inlineDatum: string;
}): Effect.Effect<number, Error> =>
  Effect.gen(function* () {
    const outputIndex = yield* Effect.try({
      try: () => {
        const outputs = CML.Transaction.from_cbor_hex(signedTxCbor)
          .body()
          .outputs();
        for (let index = 0; index < outputs.len(); index += 1)
          if (coreToTxOutput(outputs.get(index)).datum === inlineDatum)
            return index;
        throw new Error("no output carries the nonce datum");
      },
      catch: (cause) =>
        new Error(
          `Signed hub-oracle nonce transaction ${txHash} has no nonce output: ${formatUnknownError(cause)}`,
        ),
    });
    yield* awaitRequiredOutputVisibility(
      lucid,
      { txHash, signedTxCbor },
      [outputIndex],
      TX_OUTPUT_VISIBILITY_TIMEOUT_MS,
      TX_OUTPUT_VISIBILITY_POLL_INTERVAL_MS,
    );
    return outputIndex;
  });

export const reconcileHubOracleOneShotNonceAttemptProgram = (
  attempt: SubmittedHubOracleNonceAttempt,
  options: PrepareHubOracleNonceOptions = {},
): Effect.Effect<PreparedHubOracleNonce, unknown, Lucid> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    yield* lucidService.switchToOperatorsMainWallet;
    const lucid = lucidService.api;
    const pollIntervalMs = boundedMs(
      options.pollIntervalMs,
      DEFAULT_NONCE_RECOVERY_POLL_INTERVAL_MS,
    );
    const { signedTxCbor } = attempt;
    if (signedTxCbor === undefined)
      return yield* Effect.fail(
        new Error(
          `Hub-oracle nonce attempt ${attempt.txHash} was recorded without its signed transaction, so its nonce cannot be read back by outref. Pass --fresh-redeploy --fresh-redeploy-reason <reason> only when replacing the deployment identity is intentional.`,
        ),
      );
    yield* resumeSignedHubOracleNonceTransaction({
      lucid,
      txHash: attempt.txHash,
      signedTxCbor,
    });
    yield* awaitTxHashConfirmation({
      lucid,
      txHash: attempt.txHash,
      timeoutMs: boundedMs(
        options.confirmationReconcileTimeoutMs,
        DEFAULT_CONFIRMATION_RECONCILE_TIMEOUT_MS,
      ),
      pollIntervalMs,
    });
    if (options.onTxHashConfirmed !== undefined) {
      yield* options
        .onTxHashConfirmed(attempt, "reconciled_after_timeout")
        .pipe(
          Effect.catchAll((cause) =>
            Effect.logWarning(
              `Failed to record confirmed hub-oracle nonce tx ${attempt.txHash}: ${formatUnknownError(
                cause,
                { includeCause: true },
              )}`,
            ),
          ),
        );
    }
    const outputIndex = yield* confirmedNonceOutputIndex({
      lucid,
      txHash: attempt.txHash,
      signedTxCbor,
      inlineDatum: attempt.inlineDatum,
    });
    return {
      txHash: attempt.txHash,
      outputIndex,
      outRef: `${attempt.txHash}#${outputIndex.toString()}`,
      address: attempt.address,
      lovelace: attempt.lovelace,
      inlineDatum: attempt.inlineDatum,
      confirmationStatus: "reconciled_after_timeout",
      ...(attempt.l1Origin === undefined ? {} : { l1Origin: attempt.l1Origin }),
    };
  });

export const inspectOperatorWalletForNonceProgram = (
  requestedNonceLovelace: bigint,
): Effect.Effect<OperatorWalletNonceReadiness, unknown, Lucid> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    yield* lucidService.switchToOperatorsMainWallet;
    const lucid = lucidService.api;
    const address = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new Error(
          `Failed to derive operator wallet address: ${formatUnknownError(cause)}`,
        ),
    });
    const utxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new Error(
          `Failed to fetch operator wallet UTxOs: ${formatUnknownError(cause)}`,
        ),
    });
    const spendableUtxos = utxos
      .filter((utxo) => utxo.scriptRef === undefined)
      .map((utxo) => ({
        outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        lovelace: (utxo.assets.lovelace ?? 0n).toString(10),
        assetCount: Object.keys(utxo.assets).length,
        hasDatum: utxo.datum !== undefined || utxo.datumHash !== undefined,
        hasScriptRef: utxo.scriptRef !== undefined,
      }));
    const totalSpendableLovelace = spendableUtxos.reduce(
      (sum, utxo) => sum + BigInt(utxo.lovelace),
      0n,
    );
    return {
      address,
      requestedNonceLovelace: requestedNonceLovelace.toString(10),
      spendableUtxos,
      totalSpendableLovelace: totalSpendableLovelace.toString(10),
    };
  });

export const prepareHubOracleOneShotNonceProgram = (
  amountLovelace: bigint,
  options: PrepareHubOracleNonceOptions = {},
): Effect.Effect<PreparedHubOracleNonce, unknown, Lucid | IntentJournal> =>
  Effect.gen(function* () {
    const originSource = options.l1Origin;
    if (originSource === undefined)
      return yield* Effect.fail(noLocalNodeOriginError());
    const lucidService = yield* Lucid;
    yield* lucidService.switchToOperatorsMainWallet;
    const lucid = lucidService.api;
    const address = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new Error(
          `Failed to derive operator wallet address: ${formatUnknownError(cause)}`,
        ),
    });
    // Read and settled before anything is built or signed: the nonce is
    // broadcast only after `l1Origin` lies under the confirmation depth.
    const l1Origin = yield* settledL1Origin(originSource);
    const markerHex = makeHubOracleOneShotNonceMarkerHex();
    const { txBuilder, inlineDatum } =
      yield* SDK.incompleteHubOracleOneShotNonceTxProgram(lucid, {
        address,
        amountLovelace,
        markerHex,
      }).pipe(
        Effect.mapError(
          (cause) =>
            new Error(
              `Failed to build hub-oracle nonce preparation transaction: ${formatUnknownError(
                cause,
              )}`,
            ),
        ),
      );
    const unsigned = yield* Effect.tryPromise({
      try: () => txBuilder.complete({ localUPLCEval: true }),
      catch: (cause) =>
        new Error(
          `Failed to build hub-oracle nonce preparation transaction: ${formatUnknownError(
            cause,
          )}`,
        ),
    });
    const submit = signSubmitTransaction(
      lucid,
      unsigned,
      unjournaledSubmission("no_follower", "hub_oracle_nonce:prepare"),
    );
    const { beforeSubmission } = options;
    const submission = yield* beforeSubmission === undefined
      ? submit
      : submit.pipe(
          Effect.provideService(BeforeSignedTransactionSubmission, {
            persist: ({ txHash, signedTxCbor }) =>
              beforeSubmission({
                txHash,
                signedTxCbor,
                address,
                lovelace: amountLovelace.toString(10),
                inlineDatum,
                l1Origin,
              }),
          }),
        );
    const attempt: SubmittedHubOracleNonceAttempt = {
      txHash: submission.txHash,
      address,
      lovelace: amountLovelace.toString(10),
      inlineDatum,
    };
    if (options.onSubmitted !== undefined) {
      yield* options
        .onSubmitted(attempt)
        .pipe(
          Effect.catchAll((cause) =>
            Effect.logWarning(
              `Failed to record submitted hub-oracle nonce attempt ${attempt.txHash}: ${formatUnknownError(
                cause,
                { includeCause: true },
              )}`,
            ),
          ),
        );
    }
    const pollIntervalMs = boundedMs(
      options.pollIntervalMs,
      DEFAULT_NONCE_RECOVERY_POLL_INTERVAL_MS,
    );
    const confirmationStatus = yield* awaitSubmittedTransactionConfirmation(
      lucid,
      submission,
    ).pipe(
      Effect.map((): HubOracleNonceConfirmationStatus => "confirmed"),
      Effect.catchTag("TxConfirmError", (error) => {
        if (!isConfirmationTimeout(error)) {
          return Effect.fail(error);
        }
        return awaitTxHashConfirmation({
          lucid,
          txHash: attempt.txHash,
          timeoutMs: boundedMs(
            options.confirmationReconcileTimeoutMs,
            DEFAULT_CONFIRMATION_RECONCILE_TIMEOUT_MS,
          ),
          pollIntervalMs,
        }).pipe(
          Effect.map(
            (): HubOracleNonceConfirmationStatus => "reconciled_after_timeout",
          ),
        );
      }),
    );
    if (options.onTxHashConfirmed !== undefined) {
      yield* options
        .onTxHashConfirmed(attempt, confirmationStatus)
        .pipe(
          Effect.catchAll((cause) =>
            Effect.logWarning(
              `Failed to record confirmed hub-oracle nonce tx ${attempt.txHash}: ${formatUnknownError(
                cause,
                { includeCause: true },
              )}`,
            ),
          ),
        );
    }
    const outputIndex = yield* confirmedNonceOutputIndex({
      lucid,
      txHash: attempt.txHash,
      signedTxCbor: submission.signedTxCbor,
      inlineDatum,
    });
    return {
      txHash: attempt.txHash,
      outputIndex,
      outRef: `${attempt.txHash}#${outputIndex.toString()}`,
      address,
      lovelace: amountLovelace.toString(10),
      inlineDatum,
      confirmationStatus,
      l1Origin,
    };
  });
