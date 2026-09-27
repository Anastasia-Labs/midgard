import { writeFile } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as sleep } from "node:timers/promises";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccountFromPrivateKey,
  Lucid,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import {
  type DaBondFundingCheck,
  LucidDaAttestationSubmitter,
} from "../src/coordinator/lucid-submitter.js";
import {
  buildAddSignaturesTx,
  buildInitDaAttestationTx,
} from "../src/coordinator/tx-builders.js";
import { classifyDaAttestationMarker } from "../src/l1/attestation-marker.js";
import type { DaAttestationValidatorSet } from "../src/l1/deployment.js";
import {
  classifyL1SubmitterUtxos,
  type L1SubmitterReadinessSummary,
  preflightL1SubmitterWallet,
  readL1SubmitterKeySource,
  refreshL1SubmitterPlainAdaUtxos,
  selectL1SubmitterWallet,
  signSubmitAndConfirm,
} from "../src/l1/submitter.js";
import { deriveExpectedDaAvailabilityCommitment } from "../src/peer/signatures.js";
import { tempDir } from "./helpers.js";

// Wrapped, not replaced: each call runs the real builder unless a test queues
// a stub for it.
vi.mock("../src/coordinator/tx-builders.js", async (importOriginal) => {
  const actual =
    await importOriginal<typeof import("../src/coordinator/tx-builders.js")>();
  return {
    ...actual,
    buildInitDaAttestationTx: vi.fn(actual.buildInitDaAttestationTx),
    buildAddSignaturesTx: vi.fn(actual.buildAddSignaturesTx),
  };
});

describe("L1 submitter helpers", () => {
  it("classifies unattested and attested DA availability statuses", () => {
    expect(classifyDaAttestationMarker(SDK.NO_DA_ATTESTATION)).toEqual({
      kind: "unattested",
    });
    expect(classifyDaAttestationMarker(attestedStatus())).toEqual({
      kind: "already_attested_expected",
      availabilityKind: "Attested",
    });
  });

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
    const withDatum = utxo(
      "02",
      0,
      { lovelace: 10_000_000n },
      { datum: "d87980" },
    );
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
      awaitTxConfirmation: async (
        txHash: string,
        options?: { readonly checkInterval?: number },
      ) => {
        calls.push(
          `await:${txHash}:${options?.checkInterval?.toString() ?? ""}`,
        );
        return { txHash };
      },
    } as Pick<LucidEvolution, "awaitTxConfirmation">;

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
      awaitTxConfirmation: async (txHash: string) => {
        calls.push(`await:${txHash}`);
        return { txHash };
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
    } as unknown as Pick<LucidEvolution, "awaitTxConfirmation"> &
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
      awaitTxConfirmation: async (txHash: string) => ({ txHash }),
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
    } as unknown as Pick<LucidEvolution, "awaitTxConfirmation"> &
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

  it("keeps a dropped transaction's inputs out of selection until its TTL passes, then spends them", async () => {
    const { emulator, lucid, fundingOutRef } = await emulatorSubmitter();
    const dropped = await lucid
      .newTx()
      .pay.ToAddress(await lucid.wallet().address(), { lovelace: 5_000_000n })
      .validTo(emulator.now() + 60_000)
      .complete();
    const droppedTxHash = await signSubmitAndConfirm(lucid, dropped, {
      awaitConfirmation: false,
    });
    dropTransaction(emulator, droppedTxHash, fundingOutRef);

    // Listed again, but the dropped transaction may still land before its TTL.
    const beforeTtl = await refreshL1SubmitterPlainAdaUtxos(lucid);
    expect(beforeTtl?.spendableOutRefs).toEqual([]);
    expect(beforeTtl?.ignoredOutRefs).toEqual([
      {
        outRef: fundingOutRef,
        lovelace: 100_000_000n,
        reasons: ["spent_in_process"],
      },
    ]);

    emulator.awaitSlot(ttlSlotOf(dropped) - emulator.slot);
    const atTtl = await refreshL1SubmitterPlainAdaUtxos(lucid);
    expect(atTtl?.spendableOutRefs).toEqual([fundingOutRef]);
    const replacement = await lucid
      .newTx()
      .pay.ToAddress(await lucid.wallet().address(), { lovelace: 5_000_000n })
      .complete();
    const replacementTxHash = await submitAndConfirmOnEmulator(
      emulator,
      lucid,
      replacement,
    );
    expect(emulator.transactionHistory[replacementTxHash]).toMatchObject({
      status: "confirmed",
    });
  });

  it("forgets a landed transaction's inputs, so a rollback that restores them leaves them spendable", async () => {
    const { emulator, lucid, fundingOutRef, fundingUtxo } =
      await emulatorSubmitter();
    const tx = await lucid
      .newTx()
      .pay.ToAddress(await lucid.wallet().address(), { lovelace: 5_000_000n })
      .complete();
    const txHash = await submitAndConfirmOnEmulator(emulator, lucid, tx);
    expect(emulator.transactionHistory[txHash]).toMatchObject({
      status: "confirmed",
    });

    // The rollback: the block is gone, and the node dropped the transaction.
    const funding = emulator.ledger[flatOutRef(fundingOutRef)];
    expect(funding).toBeUndefined();
    for (const outRef of Object.keys(emulator.ledger)) {
      if (outRef.startsWith(txHash)) delete emulator.ledger[outRef];
    }
    emulator.ledger[flatOutRef(fundingOutRef)] = {
      utxo: fundingUtxo,
      spent: false,
    };
    delete emulator.transactionHistory[txHash];

    const summary = await refreshL1SubmitterPlainAdaUtxos(lucid);
    expect(summary?.spendableOutRefs).toEqual([fundingOutRef]);
    expect(summary?.ignoredOutRefs).toEqual([]);
  });

  it.each([
    ["the chain has not seen it", "not_found", [`${"44".repeat(32)}#0`], []],
    [
      "it is still pending",
      "pending",
      [],
      [
        {
          outRef: `${"44".repeat(32)}#0`,
          lovelace: 8_000_000n,
          reasons: ["spent_in_process"],
        },
      ],
    ],
    [
      "the status lookup fails",
      new Error("status lookup failed"),
      [],
      [
        {
          outRef: `${"44".repeat(32)}#0`,
          lovelace: 8_000_000n,
          reasons: ["spent_in_process"],
        },
      ],
    ],
  ])(
    "after a failed confirmation wait, releases the inputs only when %s",
    async (_, status, spendableOutRefs, ignoredOutRefs) => {
      const spentInput = utxo("44", 0, { lovelace: 8_000_000n });
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
      const statusCalls: string[] = [];
      const lucid = {
        awaitTxConfirmation: async (txHash: string) => {
          throw new Error(`Timed out waiting for transaction ${txHash}.`);
        },
        transactionStatus: async (txHash: string) => {
          statusCalls.push(txHash);
          if (status instanceof Error) throw status;
          return { status };
        },
        wallet: () => ({
          address: async () => "addr_test1submitter",
          getUtxos: async () => [],
        }),
        utxosAt: async () => [spentInput],
        overrideUTxOs: () => undefined,
      } as unknown as Parameters<typeof signSubmitAndConfirm>[0];

      await expect(signSubmitAndConfirm(lucid, tx)).rejects.toThrow(
        /Timed out waiting for transaction txhash/,
      );
      const summary = await refreshL1SubmitterPlainAdaUtxos(lucid);
      expect(statusCalls).toEqual(["txhash"]);
      expect(summary?.spendableOutRefs).toEqual(spendableOutRefs);
      expect(summary?.ignoredOutRefs).toEqual(ignoredOutRefs);
    },
  );

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
      awaitTxConfirmation: async (txHash: string) => {
        calls.push(`await:${txHash}`);
        return { txHash };
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
      awaitTxConfirmation: async (txHash: string) => ({ txHash }),
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
    expect(result.errors).toEqual([
      "auto_fund_source_matches_submitter_address",
    ]);
    expect(calls).toEqual([
      "select:submitter",
      "select:funder",
      "select:submitter",
    ]);
  });

  it("verifies that apply is visible on the state queue before succeeding", async () => {
    const submitter = new LucidDaAttestationSubmitter({
      lucid: {} as LucidEvolution,
      contracts,
      referenceScripts: {} as never,
      availabilityParameters,
      postSubmitVerificationRetryCount: 1,
      postSubmitVerificationDelayMs: 0,
    });
    const probe = submitter as unknown as SubmitterProbe;
    const states = [SDK.NO_DA_ATTESTATION, attestedStatus()];
    probe.findStateQueueHeader = async () => ({
      stateQueueNode: {
        da_attestation: states.shift() ?? SDK.NO_DA_ATTESTATION,
      },
    });

    await expect(
      probe.waitForApplied("01".repeat(28)),
    ).resolves.toBeUndefined();
  });

  it.each([
    ["covers", 0n, true],
    ["is one lovelace short of", -1n, false],
  ])(
    "checks before init whether the submitter's plain ADA %s the bond and fee headroom",
    async (_, offset, sufficient) => {
      // One bond plus the 50 ADA fee headroom.
      const requiredLovelace =
        availabilityParameters.da_bond_lovelace + 50_000_000n;
      const account = generateEmulatorAccountFromPrivateKey({
        lovelace: requiredLovelace + offset,
      });
      const lucid = await Lucid(new Emulator([account]), "Custom");
      await selectL1SubmitterWallet(lucid, `private-key:${account.privateKey}`);
      const checks: DaBondFundingCheck[] = [];
      const logged: string[] = [];
      const submitter = new LucidDaAttestationSubmitter({
        lucid,
        contracts,
        referenceScripts: {} as never,
        availabilityParameters,
        recordBondFunding: (check) => checks.push(check),
        log: (line) => logged.push(line),
      });
      const probe = submitter as unknown as SubmitterProbe;
      probe.findStateQueueHeader = async () => ({
        stateQueueNode: { da_attestation: SDK.NO_DA_ATTESTATION },
      });

      // This emulator holds no DA params UTxO, so the init stops right after
      // the check.
      await expect(
        submitter.initAttestation({
          headerHash: "01".repeat(28),
          availabilityCommitmentCbor: "",
          availabilityCommitmentDigest: "",
        }),
      ).rejects.toThrow(/expected exactly one DA params UTxO, found 0/);
      expect(checks).toEqual([
        {
          checkedAt: expect.any(String),
          plainAdaLovelace: requiredLovelace + offset,
          requiredLovelace,
          sufficient,
        },
      ]);
      expect(logged).toEqual(
        sufficient
          ? []
          : [
              expect.stringContaining(
                `"event":"l1_submitter_bond_funding_short","address":"${account.address}","plainAdaLovelace":"${(requiredLovelace + offset).toString()}","requiredLovelace":"${requiredLovelace.toString()}"`,
              ),
            ],
      );
    },
  );

  it("rechecks the bond funding once an init has locked its bond, so the next unfundable init shows before it starts", async () => {
    const requiredLovelace =
      availabilityParameters.da_bond_lovelace + 50_000_000n;
    const readings = [requiredLovelace, requiredLovelace - 1n];
    const { submitter, checks, logged } = bondFundingSubmitter(readings);
    vi.mocked(buildInitDaAttestationTx).mockResolvedValueOnce(
      {} as TxSignBuilder,
    );

    await expect(submitter.initAttestation(initRecord())).resolves.toEqual({
      status: "submitted",
      txHash: "inittx",
    });
    expect(
      checks.map(({ plainAdaLovelace, sufficient }) => ({
        plainAdaLovelace,
        sufficient,
      })),
    ).toEqual([
      { plainAdaLovelace: requiredLovelace, sufficient: true },
      { plainAdaLovelace: requiredLovelace - 1n, sufficient: false },
    ]);
    expect(logged).toEqual([
      expect.stringContaining('"event":"l1_submitter_bond_funding_short"'),
    ]);
  });

  it("reports a landed init as submitted when the check after it fails", async () => {
    const requiredLovelace =
      availabilityParameters.da_bond_lovelace + 50_000_000n;
    const { submitter, checks, logged } = bondFundingSubmitter([
      requiredLovelace,
      new Error("wallet listing unavailable"),
    ]);
    vi.mocked(buildInitDaAttestationTx).mockResolvedValueOnce(
      {} as TxSignBuilder,
    );

    await expect(submitter.initAttestation(initRecord())).resolves.toEqual({
      status: "submitted",
      txHash: "inittx",
    });
    expect(checks).toHaveLength(1);
    expect(logged).toEqual([
      '{"event":"l1_submitter_bond_funding_check_failed","error":"wallet listing unavailable"}\n',
    ]);
  });

  it("records the bond funding at add-signatures' refresh, so a top-up clears without waiting for an init", async () => {
    const requiredLovelace =
      availabilityParameters.da_bond_lovelace + 50_000_000n;
    const { submitter, checks, probe } = bondFundingSubmitter([
      requiredLovelace,
    ]);
    probe.fetchCandidateUtxo = async () => ({ utxo: {}, datum: {} });
    vi.mocked(buildAddSignaturesTx).mockRejectedValueOnce(
      new Error("stop after the refresh"),
    );

    await expect(
      submitter.addSignatures({
        record: { headerHash: "01".repeat(28) } as never,
        candidate: {} as never,
        packedWitnessesHex: "",
        signerIndexes: [],
      }),
    ).rejects.toThrow("stop after the refresh");
    expect(checks.map(({ sufficient }) => sufficient)).toEqual([true]);
  });

  it("treats add-signatures as a no-op once the expected DA attestation is already applied", async () => {
    let signCalls = 0;
    const submitter = new LucidDaAttestationSubmitter({
      lucid: {} as LucidEvolution,
      contracts,
      referenceScripts: {} as never,
      availabilityParameters,
      signSubmit: async () => {
        signCalls += 1;
        return "txhash";
      },
    });
    const probe = submitter as unknown as SubmitterProbe;
    probe.findStateQueueHeader = async () => ({
      stateQueueNode: { da_attestation: attestedStatus() },
    });

    await expect(
      submitter.addSignatures({
        record: { headerHash: "01".repeat(28) } as never,
        candidate: {} as never,
        packedWitnessesHex: "",
        signerIndexes: [],
      }),
    ).resolves.toEqual({ status: "already_attested" });
    expect(signCalls).toBe(0);
  });

  it.each([
    ["exactly at the deadline", 0n],
    ["after the deadline", 1n],
  ])(
    "refuses apply %s with the typed build error before any further L1 read or submission",
    async (_, elapsedMs) => {
      const headerEndTime = 1_800_000_000_000n;
      let signCalls = 0;
      let refreshCalls = 0;
      const submitter = new LucidDaAttestationSubmitter({
        // Any L1 read past the state-queue lookup would throw on this Lucid.
        lucid: {} as LucidEvolution,
        contracts,
        referenceScripts: {} as never,
        availabilityParameters,
        currentTime: () =>
          headerEndTime + SDK.DA_ATTESTATION_TIMEOUT_MS + elapsedMs,
        refreshFundingUtxos: async () => {
          refreshCalls += 1;
          return undefined;
        },
        signSubmit: async () => {
          signCalls += 1;
          return "txhash";
        },
      });
      const probe = submitter as unknown as SubmitterProbe;
      probe.findStateQueueHeader = async () => ({
        stateQueueNode: {
          da_attestation: SDK.NO_DA_ATTESTATION,
          header: { endTime: headerEndTime },
        },
      });

      const error = await submitter
        .applyAttestation({
          record: { headerHash: "01".repeat(28) } as never,
          candidate: { outRef: `${"02".repeat(32)}#0` } as never,
        })
        .then(
          () => undefined,
          (cause: unknown) => cause,
        );
      expect(error).toBeInstanceOf(SDK.DaAttestationBuildError);
      expect(error).toMatchObject({
        reason: "validity_range_past_deadline",
      });
      expect(signCalls).toBe(0);
      expect(refreshCalls).toBe(0);
    },
  );

  it("rejects apply verification while the state-queue node stays unattested", async () => {
    const submitter = new LucidDaAttestationSubmitter({
      lucid: {} as LucidEvolution,
      contracts,
      referenceScripts: {} as never,
      availabilityParameters,
      postSubmitVerificationRetryCount: 0,
      postSubmitVerificationDelayMs: 0,
    });
    const probe = submitter as unknown as SubmitterProbe;
    probe.findStateQueueHeader = async () => ({
      stateQueueNode: { da_attestation: SDK.NO_DA_ATTESTATION },
    });

    await expect(probe.waitForApplied("01".repeat(28))).rejects.toThrow(
      /did not show DA attestation policy/,
    );
  });
});

type SubmitterProbe = {
  fetchDaParamsUtxo(): Promise<unknown>;
  fetchCandidateUtxo(candidate: unknown): Promise<unknown>;
  findStateQueueHeader(headerHash: string): Promise<{
    readonly stateQueueNode: {
      readonly da_attestation: SDK.DaAvailabilityStateQueueStatus;
      readonly header?: { readonly endTime: bigint };
    };
  }>;
  waitForApplied(headerHash: string): Promise<void>;
};

const attestedStatus = (): SDK.DaAvailabilityStateQueueStatus => ({
  Attested: { da_bond_asset_name: "aa".repeat(32) },
});

const availabilityParameters = SDK.daAvailabilityParameters({
  responseGeometry: SDK.availabilityResponseGeometry({
    chunkByteLength: 14_020,
    trancheByteLength: 4 * 1_024 * 1_024,
    maxTrancheCount: 16,
  }),
  daBondLovelace: 10_000_000_000n,
  challengerBondLovelace: 10_000_000_000n,
  maxOpenFeeLovelace: 500_000n,
  maxPublicationFeeLovelace: 500_000n,
  maxSettlementFeeLovelace: 500_000n,
  maxCloseFeeLovelace: 1_000_000n,
  maxTimeoutFeeLovelace: 1_200_000n,
});

const availabilityChallengeValidator =
  (): DaAttestationValidatorSet["availabilityChallenge"] => ({
    ...validator("ee".repeat(28), "addr_test1availability"),
    yields: Object.fromEntries(
      ["bond", "open", "settle", "close", "timeout"].map((arm) => [
        arm,
        {
          withdrawalScriptCBOR: "49480100002221200101",
          withdrawalScript: {
            type: "PlutusV3",
            script: "49480100002221200101",
          },
          withdrawalScriptHash: "f1".repeat(28),
        },
      ]),
    ) as SDK.AvailabilityChallengeYieldValidators,
  });

const contracts: DaAttestationValidatorSet = {
  hubOracle: validator("99".repeat(28), "addr_test1huboracle"),
  availabilityChallenge: availabilityChallengeValidator(),
  daAttestation: validator("aa".repeat(28), "addr_test1daattestation"),
  daParamsGovernor: validator("bb".repeat(28), "addr_test1daparams"),
  stateQueue: stateQueueValidator("cc".repeat(28), "addr_test1statequeue"),
};

function stateQueueValidator(
  policyId: string,
  spendingScriptAddress: string,
): DaAttestationValidatorSet["stateQueue"] {
  const yieldValidator = (
    role: string,
  ): DaAttestationValidatorSet["stateQueue"]["yields"]["commit"] => ({
    withdrawalScriptCBOR: "",
    withdrawalScript: { type: "PlutusV3", script: "00" } as never,
    withdrawalScriptHash: role,
  });
  return {
    ...validator(policyId, spendingScriptAddress),
    yields: {
      commit: yieldValidator("c1".repeat(28)),
      unattestedTimeout: yieldValidator("c2".repeat(28)),
      unavailableTimeout: yieldValidator("c3".repeat(28)),
      fraudRemoval: yieldValidator("c4".repeat(28)),
      merge: yieldValidator("c5".repeat(28)),
    },
  };
}

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

/** An emulator whose submitter wallet holds one 100 ADA UTxO. */
const emulatorSubmitter = async (): Promise<{
  readonly emulator: Emulator;
  readonly lucid: LucidEvolution;
  readonly fundingOutRef: string;
  readonly fundingUtxo: UTxO;
}> => {
  const account = generateEmulatorAccountFromPrivateKey({
    lovelace: 100_000_000n,
  });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  await selectL1SubmitterWallet(lucid, `private-key:${account.privateKey}`);
  const [fundingUtxo] = await emulator.getUtxos(account.address);
  return {
    emulator,
    lucid,
    fundingOutRef: `${fundingUtxo!.txHash}#${fundingUtxo!.outputIndex.toString()}`,
    fundingUtxo: fundingUtxo!,
  };
};

/**
 * `signSubmitAndConfirm` with its confirmation wait, producing the block the
 * wait polls for once the transaction reaches the mempool.
 */
const submitAndConfirmOnEmulator = async (
  emulator: Emulator,
  lucid: LucidEvolution,
  tx: TxSignBuilder,
): Promise<string> => {
  const submitted = signSubmitAndConfirm(lucid, tx, {
    confirmationPollIntervalMs: 10,
  });
  while (Object.keys(emulator.mempool).length === 0) {
    await sleep(5);
  }
  emulator.awaitBlock();
  return submitted;
};

/** The emulator ledger's key for `txHash#index`. */
const flatOutRef = (outRef: string): string => outRef.replace("#", "");

/**
 * What a node that dropped a mempool transaction shows: its input unspent
 * again, its outputs and history gone.
 */
const dropTransaction = (
  emulator: Emulator,
  txHash: string,
  inputOutRef: string,
): void => {
  emulator.ledger[flatOutRef(inputOutRef)]!.spent = false;
  emulator.mempool = {};
  delete emulator.transactionHistory[txHash];
};

const ttlSlotOf = (tx: TxSignBuilder): number => {
  const ttl = CML.Transaction.from_cbor_hex(tx.toCBOR()).body().ttl();
  if (ttl === undefined) throw new Error("transaction has no TTL");
  return Number(ttl);
};

/**
 * A submitter whose successive funding refreshes read `readings` (a plain ADA
 * balance, or a refresh failure), with the header unattested and the DA
 * params UTxO present, recording every bond funding check and log line.
 */
const bondFundingSubmitter = (readings: (bigint | Error)[]) => {
  const address = generateEmulatorAccountFromPrivateKey({
    lovelace: 1n,
  }).address;
  const checks: DaBondFundingCheck[] = [];
  const logged: string[] = [];
  const submitter = new LucidDaAttestationSubmitter({
    lucid: {
      wallet: () => ({ address: async () => address }),
    } as unknown as LucidEvolution,
    contracts,
    referenceScripts: {} as never,
    availabilityParameters,
    refreshFundingUtxos: async () => {
      const reading = readings.shift();
      if (reading === undefined) throw new Error("no reading left");
      if (reading instanceof Error) throw reading;
      return {
        address,
        plainAdaLovelace: reading,
      } as L1SubmitterReadinessSummary;
    },
    signSubmit: async () => "inittx",
    recordBondFunding: (check) => checks.push(check),
    log: (line) => logged.push(line),
  });
  const probe = submitter as unknown as SubmitterProbe;
  probe.findStateQueueHeader = async () => ({
    stateQueueNode: { da_attestation: SDK.NO_DA_ATTESTATION },
  });
  probe.fetchDaParamsUtxo = async () => ({ utxo: {}, datum: {} });
  return { submitter, checks, logged, probe };
};

/** An init record whose availability commitment parses. */
const initRecord = () => {
  const headerHash = "01".repeat(28);
  const { commitmentCbor, commitmentDigest } =
    deriveExpectedDaAvailabilityCommitment({
      authority: {
        deploymentIdentity: "99".repeat(28),
        bondOwnerCredential: "44".repeat(28),
        responseGeometry: {
          chunkByteLength: 14_020,
          trancheByteLength: 4 * 1_024 * 1_024,
          maxTrancheCount: 16,
        },
      },
      headerHash,
      payloadCborHex: "aabb",
    });
  return {
    headerHash,
    availabilityCommitmentCbor: commitmentCbor,
    availabilityCommitmentDigest: commitmentDigest,
  };
};
