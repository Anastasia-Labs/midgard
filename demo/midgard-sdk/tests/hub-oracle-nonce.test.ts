import {
  Constr,
  Data,
  type Assets,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  HubOracleError,
  incompleteHubOracleOneShotNonceTxProgram,
  makeHubOracleOneShotNonceDatum,
} from "../src/index.js";

type RecordedPayment = {
  readonly address: string;
  readonly datum: { readonly kind: "inline"; readonly value: string };
  readonly assets: Assets;
};

const makeRecordingLucid = (): {
  readonly lucid: LucidEvolution;
  readonly record: {
    readonly payments: RecordedPayment[];
    readonly newTx: ReturnType<typeof vi.fn>;
    readonly wallet: ReturnType<typeof vi.fn>;
    readonly utxosAt: ReturnType<typeof vi.fn>;
    readonly utxosAtWithUnit: ReturnType<typeof vi.fn>;
    readonly complete: ReturnType<typeof vi.fn>;
    readonly sign: ReturnType<typeof vi.fn>;
    readonly submit: ReturnType<typeof vi.fn>;
    readonly awaitTx: ReturnType<typeof vi.fn>;
  };
} => {
  const payments: RecordedPayment[] = [];
  const complete = vi.fn(() => {
    throw new Error("complete should not be called by SDK nonce builder");
  });
  const sign = vi.fn(() => {
    throw new Error("sign should not be called by SDK nonce builder");
  });
  const submit = vi.fn(() => {
    throw new Error("submit should not be called by SDK nonce builder");
  });
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
    sign,
    submit,
  };
  const newTx = vi.fn(() => tx);
  const wallet = vi.fn(() => {
    throw new Error("wallet should not be called by SDK nonce builder");
  });
  const utxosAt = vi.fn(() => {
    throw new Error("utxosAt should not be called by SDK nonce builder");
  });
  const utxosAtWithUnit = vi.fn(() => {
    throw new Error(
      "utxosAtWithUnit should not be called by SDK nonce builder",
    );
  });
  const awaitTx = vi.fn(() => {
    throw new Error("awaitTx should not be called by SDK nonce builder");
  });

  return {
    lucid: {
      newTx,
      wallet,
      utxosAt,
      utxosAtWithUnit,
      awaitTx,
    } as unknown as LucidEvolution,
    record: {
      payments,
      newTx,
      wallet,
      utxosAt,
      utxosAtWithUnit,
      complete,
      sign,
      submit,
      awaitTx,
    },
  };
};

const expectHubOracleFailure = async <A>(
  program: Effect.Effect<A, HubOracleError>,
): Promise<void> => {
  const result = await Effect.runPromise(Effect.either(program));
  expect(result._tag).toBe("Left");
  if (result._tag === "Left") {
    expect(result.left).toBeInstanceOf(HubOracleError);
  }
};

describe("hub-oracle one-shot nonce SDK boundary", () => {
  it("encodes nonce marker datums byte-for-byte like the legacy node expression", async () => {
    const markerHex = "4d696467617264";

    await expect(
      Effect.runPromise(makeHubOracleOneShotNonceDatum({ markerHex })),
    ).resolves.toBe(Data.to(new Constr(0, [markerHex])));
  });

  it("rejects empty, odd-length, and non-hex markers", async () => {
    await expectHubOracleFailure(
      makeHubOracleOneShotNonceDatum({ markerHex: "" }),
    );
    await expectHubOracleFailure(
      makeHubOracleOneShotNonceDatum({ markerHex: "abc" }),
    );
    await expectHubOracleFailure(
      makeHubOracleOneShotNonceDatum({ markerHex: "not-hex" }),
    );
  });

  it("rejects non-positive lovelace before transaction construction", async () => {
    const { lucid, record } = makeRecordingLucid();

    await expectHubOracleFailure(
      incompleteHubOracleOneShotNonceTxProgram(lucid, {
        address: "addr_test1nonce",
        amountLovelace: 0n,
        markerHex: "aa",
      }),
    );

    expect(record.newTx).not.toHaveBeenCalled();
  });

  it("builds exactly one marked payment output without wallet, query, or finalization calls", async () => {
    const { lucid, record } = makeRecordingLucid();
    const markerHex = "aa55";
    const amountLovelace = 5_000_000n;
    const address = "addr_test1nonce";

    const result = await Effect.runPromise(
      incompleteHubOracleOneShotNonceTxProgram(lucid, {
        address,
        amountLovelace,
        markerHex,
      }),
    );

    const expectedDatum = Data.to(new Constr(0, [markerHex]));
    expect(result.inlineDatum).toBe(expectedDatum);
    expect(record.newTx).toHaveBeenCalledTimes(1);
    expect(record.payments).toEqual([
      {
        address,
        datum: { kind: "inline", value: expectedDatum },
        assets: { lovelace: amountLovelace },
      },
    ]);
    expect(result.txBuilder).toBeDefined();
    expect(record.wallet).not.toHaveBeenCalled();
    expect(record.utxosAt).not.toHaveBeenCalled();
    expect(record.utxosAtWithUnit).not.toHaveBeenCalled();
    expect(record.complete).not.toHaveBeenCalled();
    expect(record.sign).not.toHaveBeenCalled();
    expect(record.submit).not.toHaveBeenCalled();
    expect(record.awaitTx).not.toHaveBeenCalled();
  });
});
