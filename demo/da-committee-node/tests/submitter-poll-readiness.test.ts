import type { UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  AutoFundPaymentUnsettledError,
  pollReadiness,
  submitAutoFundPayment,
} from "../src/l1/submitter.prune-in-flight-spends.js";

const requirements = {
  minPlainAdaLovelace: 50_000_000n,
  minCollateralLovelace: 5_000_000n,
  minSpendableUtxoCount: 1,
  retryCount: 3,
  retryDelayMs: 1,
};

const plainInput: UTxO = {
  txHash: "ab".repeat(32),
  outputIndex: 0,
  address: "addr_test1submitter",
  assets: { lovelace: 80_000_000n },
};

/** A wallet whose UTxO read throws `failures` times, then lists `utxos`. */
const flakyWallet = (failures: number, utxos: readonly UTxO[]) => {
  let reads = 0;
  return {
    reads: () => reads,
    lucid: {
      wallet: () => ({
        address: async () => "addr_test1submitter",
        getUtxos: async () => [],
      }),
      utxosAt: async () => {
        reads += 1;
        if (reads <= failures) throw new Error("fetch failed: kupo:1442");
        return [...utxos];
      },
      overrideUTxOs: () => undefined,
    } as never,
  };
};

describe("pollReadiness", () => {
  it("retries a wallet read that throws and returns the first read that answers, exactly once", async () => {
    const wallet = flakyWallet(2, [plainInput]);

    const summary = await pollReadiness(wallet.lucid, requirements);

    expect(summary.ready).toBe(true);
    expect(summary.plainAdaLovelace).toBe(80_000_000n);
    expect(wallet.reads()).toBe(3);
  });

  it("throws the last error once every read in the budget has thrown", async () => {
    const wallet = flakyWallet(10, [plainInput]);

    await expect(pollReadiness(wallet.lucid, requirements)).rejects.toThrow(
      "fetch failed: kupo:1442",
    );
    expect(wallet.reads()).toBe(requirements.retryCount + 1);
  });

  it("still reports a wallet that answers short as not ready", async () => {
    const wallet = flakyWallet(1, []);

    const summary = await pollReadiness(wallet.lucid, requirements);

    expect(summary.ready).toBe(false);
    expect(summary.missingPlainLovelace).toBe(50_000_000n);
    expect(wallet.reads()).toBe(requirements.retryCount + 1);
  });

  it("does not retry a wallet that cannot be selected at all", async () => {
    await expect(
      pollReadiness({ wallet: () => undefined } as never, requirements),
    ).rejects.toThrow("requires a selectable wallet");
  });
});

describe("submitAutoFundPayment", () => {
  /** A Lucid whose payment builds, or fails to, and whose submit fails. */
  const fundingLucid = (buildFailure?: Error) =>
    ({
      newTx: () => ({
        pay: {
          ToAddress: () => ({
            complete: async () => {
              if (buildFailure !== undefined) throw buildFailure;
              return {
                sign: {
                  withWallet: () => ({
                    complete: async () => ({
                      toCBOR: () => "84a0a0f5f6",
                      submit: async () => {
                        throw new Error("submit: socket hang up");
                      },
                    }),
                  }),
                },
              };
            },
          }),
        },
      }),
      awaitTxConfirmation: async () => true,
    }) as never;

  const fund = (lucid: never) =>
    submitAutoFundPayment({
      lucid,
      submitterAddress: "addr_test1submitter",
      lovelace: 10_000_000n,
      confirmationPollIntervalMs: 1,
    });

  it("reports a payment that failed once built as one that may have been sent", async () => {
    const failure = await fund(fundingLucid()).catch((error: unknown) => error);
    expect(failure).toBeInstanceOf(AutoFundPaymentUnsettledError);
    expect((failure as Error).cause).toEqual(
      new Error("submit: socket hang up"),
    );
  });

  it("passes a payment that could not be built through as is: nothing was sent", async () => {
    const buildFailure = new Error("insufficient funds in the funder wallet");
    await expect(fund(fundingLucid(buildFailure))).rejects.toBe(buildFailure);
  });
});
