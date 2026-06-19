import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  CML,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { LucidDaAttestationSubmitter } from "../src/coordinator/lucid-submitter.js";
import type { DaAttestationValidatorSet } from "../src/l1/deployment.js";
import {
  classifyL1SubmitterUtxos,
  preflightL1SubmitterWallet,
  readL1SubmitterKeySource,
  refreshL1SubmitterPlainAdaUtxos,
  selectL1SubmitterWallet,
  signSubmitAndConfirm,
} from "../src/l1/submitter.js";
import { tempDir } from "./helpers.js";

describe("L1 submitter helpers", () => {
  it("parses inline and file-backed submitter key sources", async () => {
    const seed =
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
    await expect(readL1SubmitterKeySource(`seed:${seed}`)).resolves.toEqual({
      kind: "seed",
      value: seed,
    });
    await expect(
      readL1SubmitterKeySource("private-key:ed25519_sk_test"),
    ).resolves.toEqual({
      kind: "private_key",
      value: "ed25519_sk_test",
    });

    const dir = await tempDir();
    const path = join(dir, "l1-submitter.key");
    await writeFile(path, `mnemonic:${seed}\n`);
    await expect(readL1SubmitterKeySource(`file:${path}`)).resolves.toEqual({
      kind: "seed",
      value: seed,
    });
  });

  it("selects the requested Lucid wallet", async () => {
    const selected: string[] = [];
    const lucid = {
      selectWallet: {
        fromSeed: (seed: string) => selected.push(`seed:${seed}`),
        fromPrivateKey: (privateKey: string) =>
          selected.push(`private:${privateKey}`),
      },
    } as unknown as Pick<LucidEvolution, "selectWallet">;

    await selectL1SubmitterWallet(lucid, "private-key:ed25519_sk_test");
    expect(selected).toEqual(["private:ed25519_sk_test"]);
  });

  it("classifies live UTxOs into spendable plain ADA and ignored balances", () => {
    const plain = utxo("01", 0, { lovelace: 30_000_000n });
    const withDatum = utxo("02", 0, { lovelace: 10_000_000n }, { datum: "d87980" });
    const withScriptRef = utxo(
      "03",
      0,
      { lovelace: 20_000_000n },
      { scriptRef: { type: "PlutusV3", script: "00" } as never },
    );
    const withToken = utxo("04", 0, {
      lovelace: 40_000_000n,
      ["aa".repeat(28) + "746f6b656e"]: 1n,
    });

    const summary = classifyL1SubmitterUtxos({
      address: "addr_test1submitter",
      utxos: [withToken, plain, withDatum, withScriptRef],
      requirements: {
        minPlainAdaLovelace: 25_000_000n,
        minCollateralLovelace: 5_000_000n,
        minSpendableUtxoCount: 1,
      },
    });

    expect(summary).toMatchObject({
      address: "addr_test1submitter",
      totalLiveLovelace: 100_000_000n,
      plainAdaLovelace: 30_000_000n,
      plainAdaUtxoCount: 1,
      collateralCandidateLovelace: 30_000_000n,
      spendableOutRefs: [`${plain.txHash}#0`],
      ready: true,
    });
    expect(summary.ignoredOutRefs).toEqual([
      {
        outRef: `${withDatum.txHash}#0`,
        lovelace: 10_000_000n,
        reasons: ["has_datum"],
      },
      {
        outRef: `${withScriptRef.txHash}#0`,
        lovelace: 20_000_000n,
        reasons: ["has_script_ref"],
      },
      {
        outRef: `${withToken.txHash}#0`,
        lovelace: 40_000_000n,
        reasons: ["has_non_lovelace_assets"],
      },
    ]);
  });

  it("reports plain ADA, collateral, and count gaps for the submitter address", () => {
    const smallPlain = utxo("05", 0, { lovelace: 3_000_000n });
    const summary = classifyL1SubmitterUtxos({
      address: "addr_test1submitter",
      utxos: [smallPlain],
      requirements: {
        minPlainAdaLovelace: 10_000_000n,
        minCollateralLovelace: 5_000_000n,
        minSpendableUtxoCount: 2,
      },
    });

    expect(summary).toMatchObject({
      address: "addr_test1submitter",
      plainAdaLovelace: 3_000_000n,
      collateralCandidateLovelace: 0n,
      missingPlainLovelace: 7_000_000n,
      missingCollateralLovelace: 5_000_000n,
      missingSpendableUtxoCount: 1,
      ready: false,
    });
    expect(summary.ignoredOutRefs).toEqual([
      {
        outRef: `${smallPlain.txHash}#0`,
        lovelace: 3_000_000n,
        reasons: ["below_collateral_floor"],
      },
    ]);
  });

  it("signs, submits, and waits for confirmation", async () => {
    const calls: string[] = [];
    const tx = {
      sign: {
        withWallet: () => {
          calls.push("sign");
          return {
            complete: async () => ({
              toCBOR: () => submittedTxCbor([]),
              submit: async () => {
                calls.push("submit");
                return "txhash";
              },
            }),
          };
        },
      },
    } as unknown as TxSignBuilder;
    const lucid = {
      awaitTx: async (txHash: string, pollInterval?: number) => {
        calls.push(`await:${txHash}:${pollInterval?.toString() ?? ""}`);
        return true;
      },
    } as Pick<LucidEvolution, "awaitTx">;

    await expect(
      signSubmitAndConfirm(lucid, tx, { confirmationPollIntervalMs: 250 }),
    ).resolves.toBe("txhash");
    expect(calls).toEqual(["sign", "submit", "await:txhash:250"]);
  });

  it("refreshes live plain-ADA funding before signing and after confirmation", async () => {
    const calls: string[] = [];
    const overrides: UTxO[][] = [];
    const tx = {
      sign: {
        withWallet: () => {
          calls.push("sign");
          return {
            complete: async () => ({
              toCBOR: () => submittedTxCbor([]),
              submit: async () => {
                calls.push("submit");
                return "txhash";
              },
            }),
          };
        },
      },
    } as unknown as TxSignBuilder;
    const staleInput = utxo("11", 0, { lovelace: 10_000_000n });
    const liveTokenInput = utxo("22", 0, {
      lovelace: 5_000_000n,
      ["aa".repeat(28) + "746f6b656e"]: 1n,
    });
    const livePlainInput = utxo("33", 1, { lovelace: 8_000_000n });
    const lucid = {
      awaitTx: async (txHash: string) => {
        calls.push(`await:${txHash}`);
        return true;
      },
      wallet: () => ({
        address: async () => {
          calls.push("address");
          return "addr_test1submitter";
        },
        getUtxos: async () => [staleInput],
      }),
      utxosAt: async (address: string) => {
        calls.push(`utxosAt:${address}`);
        return [liveTokenInput, livePlainInput];
      },
      overrideUTxOs: (utxos: UTxO[]) => {
        overrides.push(utxos);
      },
    } as unknown as Pick<LucidEvolution, "awaitTx"> &
      Pick<LucidEvolution, "wallet"> & {
        readonly utxosAt: (address: string) => Promise<UTxO[]>;
        readonly overrideUTxOs: (utxos: UTxO[]) => void;
      };

    await expect(signSubmitAndConfirm(lucid, tx)).resolves.toBe("txhash");

    expect(calls).toEqual([
      "address",
      "utxosAt:addr_test1submitter",
      "sign",
      "submit",
      "await:txhash",
      "address",
      "utxosAt:addr_test1submitter",
    ]);
    expect(overrides).toEqual([[livePlainInput], [livePlainInput]]);
  });

  it("does not reuse plain-ADA inputs already spent by a submitted tx", async () => {
    const calls: string[] = [];
    const overrides: UTxO[][] = [];
    const spentInput = utxo("44", 0, { lovelace: 8_000_000n });
    const freshInput = utxo("55", 0, { lovelace: 9_000_000n });
    const tx = {
      sign: {
        withWallet: () => ({
          complete: async () => ({
            toCBOR: () => submittedTxCbor([spentInput]),
            submit: async () => "txhash",
          }),
        }),
      },
    } as unknown as TxSignBuilder;
    const lucid = {
      awaitTx: async () => true,
      wallet: () => ({
        address: async () => "addr_test1submitter",
        getUtxos: async () => [],
      }),
      utxosAt: async (address: string) => {
        calls.push(`utxosAt:${address}`);
        return [spentInput, freshInput];
      },
      overrideUTxOs: (utxos: UTxO[]) => {
        overrides.push(utxos);
      },
    } as unknown as Pick<LucidEvolution, "awaitTx"> &
      Pick<LucidEvolution, "wallet"> & {
        readonly utxosAt: (address: string) => Promise<UTxO[]>;
        readonly overrideUTxOs: (utxos: UTxO[]) => void;
      };

    await expect(signSubmitAndConfirm(lucid, tx)).resolves.toBe("txhash");

    expect(calls).toEqual([
      "utxosAt:addr_test1submitter",
      "utxosAt:addr_test1submitter",
    ]);
    expect(overrides).toEqual([[spentInput, freshInput], [freshInput]]);
  });

  it("excludes stale outrefs when live outref validation cannot find them", async () => {
    const overrides: UTxO[][] = [];
    const staleInput = utxo("66", 0, { lovelace: 8_000_000n });
    const liveInput = utxo("77", 0, { lovelace: 9_000_000n });
    const lucid = {
      wallet: () => ({
        address: async () => "addr_test1submitter",
        getUtxos: async () => [],
      }),
      utxosAt: async () => [staleInput, liveInput],
      utxosByOutRef: async () => [liveInput],
      overrideUTxOs: (utxos: UTxO[]) => {
        overrides.push(utxos);
      },
    } as unknown as Parameters<typeof refreshL1SubmitterPlainAdaUtxos>[0];

    const summary = await refreshL1SubmitterPlainAdaUtxos(lucid, {
      minPlainAdaLovelace: 8_000_000n,
      minCollateralLovelace: 5_000_000n,
      minSpendableUtxoCount: 1,
    });

    expect(summary?.spendableOutRefs).toEqual([`${liveInput.txHash}#0`]);
    expect(summary?.ignoredOutRefs).toEqual([
      {
        outRef: `${staleInput.txHash}#0`,
        lovelace: 8_000_000n,
        reasons: ["stale_out_ref"],
      },
    ]);
    expect(overrides).toEqual([[liveInput]]);
  });

  it("auto-funds once, refetches live UTxOs, and returns funded readiness", async () => {
    const calls: string[] = [];
    const funderInput = utxo("88", 0, { lovelace: 100_000_000n });
    const fundedSubmitterInput = utxo("99", 0, { lovelace: 70_000_000n });
    let selected = "submitter";
    let fundingSubmitted = false;
    const lucid = {
      selectWallet: {
        fromSeed: (seed: string) => {
          selected = seed;
          calls.push(`select-seed:${seed}`);
        },
        fromPrivateKey: (privateKey: string) => {
          selected = privateKey;
          calls.push(`select-private:${privateKey}`);
        },
      },
      wallet: () => ({
        address: async () =>
          selected === "funder" ? "addr_test1funder" : "addr_test1submitter",
        getUtxos: async () => [],
      }),
      utxosAt: async (address: string) => {
        calls.push(`utxosAt:${address}`);
        if (address === "addr_test1funder") {
          return [funderInput];
        }
        return fundingSubmitted ? [fundedSubmitterInput] : [];
      },
      overrideUTxOs: () => undefined,
      newTx: () => ({
        pay: {
          ToAddress: (address: string, assets: UTxO["assets"]) => {
            calls.push(`pay:${address}:${assets.lovelace?.toString() ?? "0"}`);
            return {
              complete: async () =>
                ({
                  sign: {
                    withWallet: () => ({
                      complete: async () => ({
                        toCBOR: () => submittedTxCbor([funderInput]),
                        submit: async () => {
                          fundingSubmitted = true;
                          calls.push("submit-funding");
                          return "fundingtx";
                        },
                      }),
                    }),
                  },
                }) as TxSignBuilder,
            };
          },
        },
      }),
      awaitTx: async (txHash: string) => {
        calls.push(`await:${txHash}`);
        return true;
      },
    } as unknown as LucidEvolution;

    await selectL1SubmitterWallet(lucid, "private-key:submitter");
    const result = await preflightL1SubmitterWallet(lucid, {
      submitterKeySource: "private-key:submitter",
      autoFundKeySource: "private-key:funder",
      minPlainAdaLovelace: 50_000_000n,
      minCollateralLovelace: 5_000_000n,
      minSpendableUtxoCount: 1,
      autoFundBufferLovelace: 10_000_000n,
      retryCount: 0,
      retryDelayMs: 1,
    });

    expect(result).toMatchObject({
      status: "funded",
      address: "addr_test1submitter",
      fundingTxHash: "fundingtx",
      autoFundLovelace: 60_000_000n,
      plainAdaLovelace: 70_000_000n,
      errors: [],
    });
    expect(calls).toContain("pay:addr_test1submitter:60000000");
  });

  it("rejects auto-funding when funder and submitter resolve to the same address", async () => {
    const calls: string[] = [];
    const lucid = {
      selectWallet: {
        fromSeed: () => undefined,
        fromPrivateKey: (privateKey: string) => {
          calls.push(`select:${privateKey}`);
        },
      },
      wallet: () => ({
        address: async () => "addr_test1same",
        getUtxos: async () => [],
      }),
      utxosAt: async () => [],
      overrideUTxOs: () => undefined,
      newTx: () => {
        throw new Error("must not build self-funding transaction");
      },
      awaitTx: async () => true,
    } as unknown as LucidEvolution;

    await selectL1SubmitterWallet(lucid, "private-key:submitter");
    const result = await preflightL1SubmitterWallet(lucid, {
      submitterKeySource: "private-key:submitter",
      autoFundKeySource: "private-key:funder",
      minPlainAdaLovelace: 50_000_000n,
      minCollateralLovelace: 5_000_000n,
      minSpendableUtxoCount: 1,
      autoFundBufferLovelace: 10_000_000n,
      retryCount: 0,
      retryDelayMs: 1,
    });

    expect(result.status).toBe("failed");
    expect(result.errors).toEqual(["auto_fund_source_matches_submitter_address"]);
    expect(calls).toEqual(["select:submitter", "select:funder", "select:submitter"]);
  });

  it("verifies that apply is visible on the state queue before succeeding", async () => {
    const submitter = new LucidDaAttestationSubmitter({
      lucid: {} as LucidEvolution,
      contracts,
      referenceScripts: {} as never,
      postSubmitVerificationRetryCount: 1,
      postSubmitVerificationDelayMs: 0,
    });
    const probe = submitter as unknown as SubmitterProbe;
    const states = ["", contracts.daAttestation.policyId];
    probe.findStateQueueHeader = async () => ({
      stateQueueNode: { da_attestation: states.shift() ?? "" },
    });

    await expect(probe.waitForApplied("01".repeat(28))).resolves.toBeUndefined();
  });

  it("rejects apply verification for an unexpected attestation policy", async () => {
    const submitter = new LucidDaAttestationSubmitter({
      lucid: {} as LucidEvolution,
      contracts,
      referenceScripts: {} as never,
      postSubmitVerificationRetryCount: 0,
      postSubmitVerificationDelayMs: 0,
    });
    const probe = submitter as unknown as SubmitterProbe;
    probe.findStateQueueHeader = async () => ({
      stateQueueNode: { da_attestation: "ff".repeat(28) },
    });

    await expect(probe.waitForApplied("01".repeat(28))).rejects.toThrow(
      /unexpected policy/,
    );
  });
});

type SubmitterProbe = {
  findStateQueueHeader(headerHash: string): Promise<{
    readonly stateQueueNode: { readonly da_attestation: string };
  }>;
  waitForApplied(headerHash: string): Promise<void>;
};

const contracts: DaAttestationValidatorSet = {
  daAttestation: validator("aa".repeat(28), "addr_test1daattestation"),
  daParamsGovernor: validator("bb".repeat(28), "addr_test1daparams"),
  stateQueue: validator("cc".repeat(28), "addr_test1statequeue"),
};

function validator(
  policyId: string,
  spendingScriptAddress: string,
): DaAttestationValidatorSet["daAttestation"] {
  return {
    mintingScriptCBOR: "",
    mintingScript: { type: "PlutusV3", script: "00" } as never,
    policyId,
    spendingScriptCBOR: "",
    spendingScript: { type: "PlutusV3", script: "00" } as never,
    spendingScriptHash: policyId,
    spendingScriptAddress,
  };
}

const utxo = (
  hashPrefix: string,
  outputIndex: number,
  assets: UTxO["assets"],
  extra: Partial<UTxO> = {},
): UTxO =>
  ({
    txHash: hashPrefix.repeat(32).slice(0, 64),
    outputIndex,
    address: "addr_test1submitter",
    assets,
    ...extra,
  }) as UTxO;

const submittedTxCbor = (utxos: readonly UTxO[]): string => {
  const inputs = CML.TransactionInputList.new();
  for (const utxo of utxos) {
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(utxo.txHash),
        BigInt(utxo.outputIndex),
      ),
    );
  }
  const outputs = CML.TransactionOutputList.new();
  const body = CML.TransactionBody.new(inputs, outputs, 0n);
  return CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  ).to_cbor_hex();
};
