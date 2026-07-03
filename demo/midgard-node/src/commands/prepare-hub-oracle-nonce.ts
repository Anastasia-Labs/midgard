import { randomUUID } from "node:crypto";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { Lucid } from "@/services/lucid.js";
import {
  awaitSubmittedTransactionConfirmation,
  signSubmitTransaction,
  TxConfirmError,
} from "@/transactions/utils.js";

export const DEFAULT_NONCE_LOVELACE = 5_000_000n;
const DEFAULT_CONFIRMATION_RECONCILE_TIMEOUT_MS = 300_000;
const DEFAULT_OUTPUT_LOOKUP_TIMEOUT_MS = 60_000;
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
};

export type SubmittedHubOracleNonceAttempt = {
  readonly txHash: string;
  readonly address: string;
  readonly lovelace: string;
  readonly inlineDatum: string;
};

export type PrepareHubOracleNonceOptions = {
  readonly confirmationReconcileTimeoutMs?: number;
  readonly outputLookupTimeoutMs?: number;
  readonly pollIntervalMs?: number;
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

const sleepMs = (ms: number): Effect.Effect<void> =>
  Effect.promise(() => new Promise((resolve) => setTimeout(resolve, ms)));

const boundedMs = (value: number | undefined, fallback: number): number =>
  value === undefined || !Number.isFinite(value) || value < 0
    ? fallback
    : Math.trunc(value);

const isConfirmationTimeout = (error: TxConfirmError): boolean =>
  formatUnknownError(error.cause, { includeCause: true }).includes(
    "timed out waiting for tx confirmation",
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
      new Promise<void>((resolve, reject) => {
        const timeoutId = setTimeout(() => {
          reject(
            new Error(
              `timed out waiting for nonce tx confirmation after ${timeoutMs.toString()}ms`,
            ),
          );
        }, timeoutMs);
        lucid
          .awaitTx(txHash, pollIntervalMs)
          .then(() => {
            clearTimeout(timeoutId);
            resolve();
          })
          .catch((error) => {
            clearTimeout(timeoutId);
            reject(error);
          });
      }),
    catch: (cause) =>
      new TxConfirmError({
        message:
          "Failed to reconcile hub-oracle nonce transaction confirmation",
        txHash,
        cause,
      }),
  });

const findMarkedNonceOutputs = async ({
  lucid,
  attempt,
  amountLovelace,
}: {
  readonly lucid: LucidEvolution;
  readonly attempt: SubmittedHubOracleNonceAttempt;
  readonly amountLovelace: bigint;
}): Promise<readonly UTxO[]> => {
  const visibleUtxos = await lucid.utxosAt(attempt.address);
  return visibleUtxos.filter(
    (utxo) =>
      utxo.txHash === attempt.txHash &&
      utxo.datum === attempt.inlineDatum &&
      (utxo.assets.lovelace ?? 0n) === amountLovelace &&
      utxo.scriptRef === undefined,
  );
};

const waitForMarkedNonceOutput = ({
  lucid,
  attempt,
  amountLovelace,
  timeoutMs,
  pollIntervalMs,
}: {
  readonly lucid: LucidEvolution;
  readonly attempt: SubmittedHubOracleNonceAttempt;
  readonly amountLovelace: bigint;
  readonly timeoutMs: number;
  readonly pollIntervalMs: number;
}): Effect.Effect<UTxO, Error> =>
  Effect.gen(function* () {
    const deadline = Date.now() + timeoutMs;
    for (;;) {
      const matches = yield* Effect.tryPromise({
        try: () =>
          findMarkedNonceOutputs({
            lucid,
            attempt,
            amountLovelace,
          }),
        catch: (cause) =>
          new Error(
            `Failed to refetch operator wallet UTxOs after nonce transaction ${attempt.txHash}: ${formatUnknownError(
              cause,
            )}`,
          ),
      });
      if (matches.length === 1) {
        return matches[0]!;
      }
      if (matches.length > 1 || Date.now() >= deadline) {
        return yield* Effect.fail(
          new Error(
            `Expected exactly one marked nonce output for ${attempt.txHash}, found ${matches.length.toString()}`,
          ),
        );
      }
      yield* sleepMs(pollIntervalMs);
    }
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
    const amountLovelace = BigInt(attempt.lovelace);
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
    const nonceUtxo = yield* waitForMarkedNonceOutput({
      lucid,
      attempt,
      amountLovelace,
      timeoutMs: boundedMs(
        options.outputLookupTimeoutMs,
        DEFAULT_OUTPUT_LOOKUP_TIMEOUT_MS,
      ),
      pollIntervalMs,
    });
    return {
      txHash: attempt.txHash,
      outputIndex: nonceUtxo.outputIndex,
      outRef: `${attempt.txHash}#${nonceUtxo.outputIndex.toString()}`,
      address: attempt.address,
      lovelace: attempt.lovelace,
      inlineDatum: attempt.inlineDatum,
      confirmationStatus: "reconciled_after_timeout",
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
): Effect.Effect<PreparedHubOracleNonce, unknown, Lucid> =>
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
    const submission = yield* signSubmitTransaction(lucid, unsigned);
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
    const nonceUtxo = yield* waitForMarkedNonceOutput({
      lucid,
      attempt,
      amountLovelace,
      timeoutMs: boundedMs(
        options.outputLookupTimeoutMs,
        DEFAULT_OUTPUT_LOOKUP_TIMEOUT_MS,
      ),
      pollIntervalMs,
    });
    return {
      txHash: attempt.txHash,
      outputIndex: nonceUtxo.outputIndex,
      outRef: `${attempt.txHash}#${nonceUtxo.outputIndex.toString()}`,
      address,
      lovelace: amountLovelace.toString(10),
      inlineDatum,
      confirmationStatus,
    };
  });
