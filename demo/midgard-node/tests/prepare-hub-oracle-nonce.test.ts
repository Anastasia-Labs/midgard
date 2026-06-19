import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  Data,
  type Assets,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  type PreparedHubOracleNonce,
  parseNonceLovelaceOption,
  prepareHubOracleOneShotNonceProgram,
} from "@/commands/prepare-hub-oracle-nonce.js";
import { Lucid as LucidService } from "@/services/lucid.js";

const handleSignSubmitMock = vi.hoisted(() => vi.fn());

vi.mock("@/transactions/utils.js", () => ({
  handleSignSubmit: handleSignSubmitMock,
}));

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
  const utxosAt = vi.fn(async () => {
    const inlineDatum = payments[0]?.datum.value ?? "";
    return Array.from({ length: params.matchingOutputCount }, (_, index) =>
      makeVisibleUtxo(index + 7, inlineDatum, params.amountLovelace),
    );
  });
  const switchToOperatorsMainWallet = vi.fn(() => Effect.void);
  const lucid = {
    newTx,
    wallet,
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
      utxosAt,
      switchToOperatorsMainWallet,
    },
  };
};

const runPrepare = (
  amountLovelace: bigint,
  lucidService: unknown,
): Promise<PreparedHubOracleNonce> =>
  Effect.runPromise(
    prepareHubOracleOneShotNonceProgram(amountLovelace).pipe(
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
    handleSignSubmitMock.mockReset();
    handleSignSubmitMock.mockImplementation(() => Effect.succeed(TX_HASH));
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
    expect(handleSignSubmitMock).toHaveBeenCalledWith(
      (lucidService as { readonly api: LucidEvolution }).api,
      record.unsignedTx,
    );
    expect(record.utxosAt).toHaveBeenCalledWith(OPERATOR_ADDRESS);

    expect(result).toEqual({
      txHash: TX_HASH,
      outputIndex: 7,
      outRef: `${TX_HASH}#7`,
      address: OPERATOR_ADDRESS,
      lovelace: amountLovelace.toString(10),
      inlineDatum: record.payments[0]!.datum.value,
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

    await expect(runPrepare(amountLovelace, lucidService)).rejects.toThrow(
      `Expected exactly one marked nonce output for ${TX_HASH}, found 0`,
    );
  });

  it("fails when multiple submitted nonce outputs become visible", async () => {
    const amountLovelace = 5_000_000n;
    const { lucidService } = makeLucidService({
      amountLovelace,
      matchingOutputCount: 2,
    });

    await expect(runPrepare(amountLovelace, lucidService)).rejects.toThrow(
      `Expected exactly one marked nonce output for ${TX_HASH}, found 2`,
    );
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
    expect(handleSignSubmitMock).not.toHaveBeenCalled();
  });
});
