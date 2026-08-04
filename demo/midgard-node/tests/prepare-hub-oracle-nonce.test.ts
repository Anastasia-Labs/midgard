import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  Constr,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  parseNonceLovelaceOption,
  type PreparedHubOracleNonce,
  type PrepareHubOracleNonceOptions,
  prepareHubOracleOneShotNonceProgram,
  reconcileHubOracleOneShotNonceAttemptProgram,
} from "@/commands/prepare-hub-oracle-nonce.js";
import { Lucid as LucidService } from "@/services/lucid.js";
import { TxConfirmError } from "@/transactions/utils.js";

const signSubmitTransactionMock = vi.hoisted(() => vi.fn());
const awaitSubmittedTransactionConfirmationMock = vi.hoisted(() => vi.fn());

vi.mock("@/transactions/utils.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("@/transactions/utils.js")>();
  return {
    ...actual,
    signSubmitTransaction: signSubmitTransactionMock,
    awaitSubmittedTransactionConfirmation:
      awaitSubmittedTransactionConfirmationMock,
  };
});

const TX_HASH = "ab".repeat(32);
const OPERATOR_ADDRESS = "addr_test1operatornonce";

type RecordedPayment = {
  readonly address: string;
  readonly datum: { readonly kind: "inline"; readonly value: string };
  readonly assets: Assets;
};

const makeVisibleUtxo = (
  outputIndex: number,
  inlineDatum: string,
  lovelace: bigint,
): UTxO =>
  ({
    txHash: TX_HASH,
    outputIndex,
    address: OPERATOR_ADDRESS,
    assets: { lovelace },
    datum: inlineDatum,
  }) as UTxO;

const makeLucidService = (params: {
  readonly amountLovelace: bigint;
  readonly matchingOutputCount: number;
}) => {
  const payments: RecordedPayment[] = [];
  const unsignedTx = { tx: "unsigned" };
  const complete = vi.fn(async () => unsignedTx);
  const tx = {
    pay: {
      ToAddressWithData: (
        address: string,
        datum: RecordedPayment["datum"],
        assets: Assets,
      ) => {
        payments.push({ address, datum, assets });
        return tx;
      },
    },
    complete,
  };
  const newTx = vi.fn(() => tx);
  const walletAddress = vi.fn(async () => OPERATOR_ADDRESS);
  const wallet = vi.fn(() => ({ address: walletAddress }));
  const awaitTxConfirmation = vi.fn(async (txHash: string) => ({ txHash }));
  const utxosAt = vi.fn(async () => {
    const inlineDatum = payments[0]?.datum.value ?? "";
    return Array.from({ length: params.matchingOutputCount }, (_, index) =>
      makeVisibleUtxo(index + 7, inlineDatum, params.amountLovelace),
    );
  });
  const switchToOperatorsMainWallet = vi.fn(() => Effect.void);
  const lucid = {
    config: () => ({ provider: undefined }),
    newTx,
    wallet,
    awaitTxConfirmation,
    utxosAt,
  } as unknown as LucidEvolution;
  const lucidService = {
    api: lucid,
    switchToOperatorsMainWallet: Effect.sync(() => {
      switchToOperatorsMainWallet();
    }),
  };

  return {
    lucidService,
    record: {
      payments,
      unsignedTx,
      complete,
      newTx,
      wallet,
      walletAddress,
      awaitTxConfirmation,
      utxosAt,
      switchToOperatorsMainWallet,
    },
  };
};

const runPrepare = (
  amountLovelace: bigint,
  lucidService: unknown,
  options: PrepareHubOracleNonceOptions = {},
): Promise<PreparedHubOracleNonce> =>
  Effect.runPromise(
    prepareHubOracleOneShotNonceProgram(amountLovelace, options).pipe(
      Effect.provideService(LucidService, lucidService as never),
    ),
  );

const runReconcile = (
  lucidService: unknown,
  attempt: {
    readonly txHash: string;
    readonly address: string;
    readonly lovelace: string;
    readonly inlineDatum: string;
  },
  options: PrepareHubOracleNonceOptions = {},
): Promise<PreparedHubOracleNonce> =>
  Effect.runPromise(
    reconcileHubOracleOneShotNonceAttemptProgram(attempt, options).pipe(
      Effect.provideService(LucidService, lucidService as never),
    ),
  );

const markerFromInlineDatum = (inlineDatum: string): string => {
  const decoded = Data.from(inlineDatum);
  expect(decoded).toBeInstanceOf(Constr);
  const constr = decoded as Constr<string>;
  expect(constr.index).toBe(0);
  expect(constr.fields).toHaveLength(1);
  return constr.fields[0]!;
};

describe("prepare hub-oracle one-shot nonce command boundary", () => {
  beforeEach(() => {
    signSubmitTransactionMock.mockReset();
    awaitSubmittedTransactionConfirmationMock.mockReset();
    signSubmitTransactionMock.mockImplementation(() =>
      Effect.succeed({
        txHash: TX_HASH,
        signedTxCbor: "00",
        walletAddress: OPERATOR_ADDRESS,
      }),
    );
    awaitSubmittedTransactionConfirmationMock.mockImplementation(
      (_lucid, submission: { readonly txHash: string }) =>
        Effect.succeed(submission.txHash),
    );
  });

  it("parses positive integer lovelace options and rejects invalid values", () => {
    expect(parseNonceLovelaceOption("5000000")).toBe(5_000_000n);
    expect(parseNonceLovelaceOption(" 42 ")).toBe(42n);
    expect(() => parseNonceLovelaceOption("0")).toThrow(
      "--amount-lovelace must be greater than zero",
    );
    expect(() => parseNonceLovelaceOption("-1")).toThrow(
      "--amount-lovelace must be a positive integer",
    );
    expect(() => parseNonceLovelaceOption("   ")).toThrow(
      "--amount-lovelace must be a positive integer",
    );
    expect(() => parseNonceLovelaceOption("1.5")).toThrow(
      "--amount-lovelace must be a positive integer",
    );
  });

  it("keeps wallet switching, completion, submission, refetching, and result shaping in the node", async () => {
    const amountLovelace = 5_000_000n;
    const { lucidService, record } = makeLucidService({
      amountLovelace,
      matchingOutputCount: 1,
    });

    const result = await runPrepare(amountLovelace, lucidService);

    expect(record.switchToOperatorsMainWallet).toHaveBeenCalledTimes(1);
    expect(record.walletAddress).toHaveBeenCalledTimes(1);
    expect(record.newTx).toHaveBeenCalledTimes(1);
    expect(record.payments).toHaveLength(1);
    expect(record.payments[0]).toMatchObject({
      address: OPERATOR_ADDRESS,
      assets: { lovelace: amountLovelace },
    });
    expect(record.complete).toHaveBeenCalledWith({ localUPLCEval: true });
    expect(signSubmitTransactionMock).toHaveBeenCalledWith(
      (lucidService as { readonly api: LucidEvolution }).api,
      record.unsignedTx,
    );
    expect(awaitSubmittedTransactionConfirmationMock).toHaveBeenCalledWith(
      (lucidService as { readonly api: LucidEvolution }).api,
      {
        txHash: TX_HASH,
        signedTxCbor: "00",
        walletAddress: OPERATOR_ADDRESS,
      },
    );
    expect(record.utxosAt).toHaveBeenCalledWith(OPERATOR_ADDRESS);

    expect(result).toEqual({
      txHash: TX_HASH,
      outputIndex: 7,
      outRef: `${TX_HASH}#7`,
      address: OPERATOR_ADDRESS,
      lovelace: amountLovelace.toString(10),
      inlineDatum: record.payments[0]!.datum.value,
      confirmationStatus: "confirmed",
    });

    const markerHex = markerFromInlineDatum(result.inlineDatum);
    expect(markerHex.length).toBeGreaterThan(0);
    const markerText = Buffer.from(markerHex, "hex").toString("utf8");
    expect(
      markerText.startsWith(SDK.HUB_ORACLE_ONE_SHOT_NONCE_DATUM_DOMAIN),
    ).toBe(true);
    await expect(
      Effect.runPromise(SDK.makeHubOracleOneShotNonceDatum({ markerHex })),
    ).resolves.toBe(result.inlineDatum);
  });

  it("fails when no submitted nonce output becomes visible", async () => {
    const amountLovelace = 5_000_000n;
    const { lucidService } = makeLucidService({
      amountLovelace,
      matchingOutputCount: 0,
    });

    await expect(
      runPrepare(amountLovelace, lucidService, { outputLookupTimeoutMs: 0 }),
    ).rejects.toThrow(
      `Expected exactly one marked nonce output for ${TX_HASH}, found 0`,
    );
  });

  it("fails when multiple submitted nonce outputs become visible", async () => {
    const amountLovelace = 5_000_000n;
    const { lucidService } = makeLucidService({
      amountLovelace,
      matchingOutputCount: 2,
    });

    await expect(
      runPrepare(amountLovelace, lucidService, { outputLookupTimeoutMs: 0 }),
    ).rejects.toThrow(
      `Expected exactly one marked nonce output for ${TX_HASH}, found 2`,
    );
  });

  it("records submitted nonce details before waiting for confirmation", async () => {
    const amountLovelace = 5_000_000n;
    const { lucidService, record } = makeLucidService({
      amountLovelace,
      matchingOutputCount: 1,
    });
    const onSubmitted = vi.fn(() => Effect.void);
    const onTxHashConfirmed = vi.fn(() => Effect.void);

    await runPrepare(amountLovelace, lucidService, {
      onSubmitted,
      onTxHashConfirmed,
    });

    const attempt = {
      txHash: TX_HASH,
      address: OPERATOR_ADDRESS,
      lovelace: amountLovelace.toString(10),
      inlineDatum: record.payments[0]!.datum.value,
    };
    expect(onSubmitted).toHaveBeenCalledWith(attempt);
    expect(onTxHashConfirmed).toHaveBeenCalledWith(attempt, "confirmed");
  });

  it("reconciles by tx hash when the first confirmation wait times out", async () => {
    const amountLovelace = 5_000_000n;
    const { lucidService, record } = makeLucidService({
      amountLovelace,
      matchingOutputCount: 1,
    });
    awaitSubmittedTransactionConfirmationMock.mockImplementationOnce(
      (_lucid, submission: { readonly txHash: string }) =>
        Effect.fail(
          new TxConfirmError({
            message: "Failed to confirm transaction",
            txHash: submission.txHash,
            cause: new Error(
              "timed out waiting for tx confirmation after 90000ms",
            ),
          }),
        ),
    );

    const result = await runPrepare(amountLovelace, lucidService);

    expect(record.awaitTxConfirmation).toHaveBeenCalledWith(TX_HASH, {
      timeout: 300_000,
      checkInterval: 5_000,
    });
    expect(result).toMatchObject({
      txHash: TX_HASH,
      outRef: `${TX_HASH}#7`,
      confirmationStatus: "reconciled_after_timeout",
    });
  });

  it("reconciles a pending submitted nonce attempt without building a new tx", async () => {
    const amountLovelace = 5_000_000n;
    const inlineDatum =
      "d8799f581d6d6964676172645f6875625f6f7261636c655f6f6e655f73686f745fff";
    const { lucidService, record } = makeLucidService({
      amountLovelace,
      matchingOutputCount: 1,
    });
    record.utxosAt.mockImplementation(async () => [
      makeVisibleUtxo(9, inlineDatum, amountLovelace),
    ]);

    const result = await runReconcile(lucidService, {
      txHash: TX_HASH,
      address: OPERATOR_ADDRESS,
      lovelace: amountLovelace.toString(10),
      inlineDatum,
    });

    expect(record.newTx).not.toHaveBeenCalled();
    expect(record.awaitTxConfirmation).toHaveBeenCalledWith(TX_HASH, {
      timeout: 300_000,
      checkInterval: 5_000,
    });
    expect(result).toEqual({
      txHash: TX_HASH,
      outputIndex: 9,
      outRef: `${TX_HASH}#9`,
      address: OPERATOR_ADDRESS,
      lovelace: amountLovelace.toString(10),
      inlineDatum,
      confirmationStatus: "reconciled_after_timeout",
    });
  });

  it("wraps completion failures with the existing operator-facing message", async () => {
    const amountLovelace = 5_000_000n;
    const { lucidService, record } = makeLucidService({
      amountLovelace,
      matchingOutputCount: 1,
    });
    const completionError = new Error("completion failed");
    record.complete.mockRejectedValueOnce(completionError);

    await expect(runPrepare(amountLovelace, lucidService)).rejects.toThrow(
      `Failed to build hub-oracle nonce preparation transaction: ${formatUnknownError(
        completionError,
      )}`,
    );
    expect(signSubmitTransactionMock).not.toHaveBeenCalled();
  });
});
