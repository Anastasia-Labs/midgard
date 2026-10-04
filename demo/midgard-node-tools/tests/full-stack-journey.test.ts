import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { assetsToValue, CML, walletFromSeed } from "@lucid-evolution/lucid";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "midgard-node/tests/midgard-output-helpers";
import { afterEach, describe, expect, it, vi } from "vitest";

import { readJsonIfPresent } from "../src/full-stack/journal.js";
import { type JourneyLedger, journeySteps } from "../src/full-stack/journey.js";
import {
  assertDepositCredit,
  assertDepositFunding,
  assertTransferDeltas,
  assertWithdrawalDebit,
} from "../src/full-stack/journey-balances.js";
import {
  includedPayout,
  payoutConclusion,
  type SettlementAttempt,
} from "../src/full-stack/payout-body.js";
import { runStackWorkflow } from "../src/full-stack/workflow.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

afterEach(async () => {
  vi.unstubAllGlobals();
  await removeStackFixtures();
});

describe("exact journey balances", () => {
  it("accepts only the exact deposit credit", () => {
    expect(() => assertDepositCredit(5n, 15n, 10n)).not.toThrow();
    for (const after of [14n, 16n])
      expect(() => assertDepositCredit(5n, after, 10n)).toThrow(
        "exact L2 balance",
      );
  });
  it("accepts only the exact transfer deltas and received output", () => {
    const exact = {
      amount: 10n,
      fee: 2n,
      senderBefore: 100n,
      senderAfter: 88n,
      recipientBefore: 7n,
      recipientAfter: 17n,
      received: [10n],
    };
    expect(() => assertTransferDeltas(exact)).not.toThrow();
    for (const [change, message] of [
      [{ senderAfter: 89n }, "balance or fee"],
      [{ senderAfter: 87n }, "balance or fee"],
      [{ recipientAfter: 18n }, "Recipient L2 balance"],
      [{ recipientAfter: 16n }, "Recipient L2 balance"],
      [{ received: [9n] }, "exact transfer"],
      [{ received: [10n, 10n] }, "exact transfer"],
      [{ received: [] }, "exact transfer"],
    ] as const)
      expect(() => assertTransferDeltas({ ...exact, ...change })).toThrow(
        message,
      );
  });
  it("accepts only the exact withdrawal debit", () => {
    expect(() => assertWithdrawalDebit(30n, 20n, 10n)).not.toThrow();
    for (const after of [19n, 21n])
      expect(() => assertWithdrawalDebit(30n, after, 10n)).toThrow(
        "exact recipient L2 balance",
      );
  });
  it("requires fee headroom above each deposit", () => {
    expect(() => assertDepositFunding(15_000_000n, 10_000_000n)).not.toThrow();
    expect(() => assertDepositFunding(14_999_999n, 10_000_000n)).toThrow(
      "including fee headroom",
    );
  });
});

describe("withdrawal payout decision", () => {
  const conclusion: SettlementAttempt = {
    phase: "conclude",
    status: "confirmed",
    txHash: "a".repeat(64),
    signedCbor: "",
  };
  const complete = [{ phase: "complete" }];
  it("refuses two confirmed conclusions for one withdrawal", () =>
    expect(() =>
      payoutConclusion({
        jobs: complete,
        attempts: [conclusion, { ...conclusion, txHash: "b".repeat(64) }],
      }),
    ).toThrow("More than one confirmed payout"));
  it("waits for a complete job with one confirmed conclusion", () => {
    expect(
      payoutConclusion({
        jobs: [{ phase: "concluding" }],
        attempts: [conclusion],
      }),
    ).toBeUndefined();
    expect(
      payoutConclusion({
        jobs: complete,
        attempts: [{ ...conclusion, status: "submitted" }],
      }),
    ).toBeUndefined();
    expect(payoutConclusion({ jobs: complete, attempts: [conclusion] })).toBe(
      conclusion,
    );
  });
  it("verifies the payout only once Cardano includes it", () => {
    const address = walletFromSeed(
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
      { network: "Preprod" },
    ).address;
    const outputs = CML.TransactionOutputList.new();
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        CML.Value.from_coin(10_000_000n),
      ),
    );
    const signedCbor = CML.Transaction.new(
      CML.TransactionBody.new(CML.TransactionInputList.new(), outputs, 1n),
      CML.TransactionWitnessSet.new(),
      true,
    ).to_cbor_hex();
    const attempt = { ...conclusion, signedCbor };
    const assets = { lovelace: "10000000" };
    expect(includedPayout(attempt, "pending", address, assets)).toBeUndefined();
    expect(includedPayout(attempt, "included", address, assets)).toEqual({
      outputIndex: 0,
    });
  });
});

async function journey() {
  const { config } = await stackFixture();
  const env: Record<string, string> = {
    ...stackEnvironment(config),
    MIN_FEE_A: "44",
    MIN_FEE_B: "155381",
  };
  const processes = new RecordingProcesses(config, env);
  const user = walletFromSeed(env[config.wallets.user!.seedEnv]!, {
    network: "Preprod",
  }).address;
  const reads: string[] = [];
  const sent: { cbor: string; txId: string }[] = [];
  let submit = async (): Promise<unknown> => {
    throw new Error("response lost");
  };
  const ledger: JourneyLedger = {
    utxos: async (address) => {
      reads.push(address);
      if (address !== user) return [];
      const txHash = "44".repeat(32);
      const assets = { lovelace: 50_000_000n };
      return [
        {
          txHash,
          outputIndex: 0,
          outrefCbor: makeOutRefCbor(txHash, 0),
          outputCbor: Buffer.from(
            makeMidgardTxOutput(
              CML.Address.from_bech32(address),
              assetsToValue(assets),
            ).to_cbor_bytes(),
          ),
          address,
          assets,
        },
      ];
    },
    submitTransfer: async (cbor, txId) => {
      sent.push({ cbor, txId });
      return submit();
    },
  };
  const steps = journeySteps(processes, ledger);
  const byId = (id: string) => steps.find((step) => step.id === id)!;
  const context = {
    directory: config.runDirectory,
    intentDigest: "a".repeat(64),
  };
  const transferFile = join(config.runDirectory, "cycle-0-transfer.json");
  const txStatus = (body: unknown) =>
    vi.stubGlobal("fetch", async () => new Response(JSON.stringify(body)));
  return {
    byId,
    config,
    context,
    processes,
    reads,
    sent,
    setSubmit: (next: typeof submit) => (submit = next),
    transferFile,
    txStatus,
    user,
  };
}

describe("wallet journey resume", () => {
  it("resends the saved signed transfer after a lost response without rebuilding", async () => {
    const value = await journey();
    value.txStatus({ status: "not_found" });
    const transfer = [value.byId("cycle-0-transfer")];
    await expect(runStackWorkflow(value.context, transfer)).rejects.toThrow(
      "response lost",
    );
    const saved = await readJsonIfPresent(value.transferFile);
    expect(saved).toMatchObject({ txId: value.sent[0]!.txId });
    value.setSubmit(async () => ({ accepted: true }));
    // The node never saw it, so confirmation stays open; the resend is what matters.
    await expect(runStackWorkflow(value.context, transfer)).rejects.toThrow(
      "completion has not been confirmed",
    );
    expect(value.sent).toHaveLength(2);
    expect(value.sent[1]).toEqual(value.sent[0]);
    expect(
      value.reads.filter((address) => address === value.user),
    ).toHaveLength(1);
    expect(await readJsonIfPresent(value.transferFile)).toEqual(saved);
  });
  it("refuses a saved transfer whose fee was changed", async () => {
    const value = await journey();
    value.txStatus({ status: "not_found" });
    const transfer = [value.byId("cycle-0-transfer")];
    await expect(runStackWorkflow(value.context, transfer)).rejects.toThrow(
      "response lost",
    );
    const saved = JSON.parse(await readFile(value.transferFile, "utf8"));
    await writeFile(
      value.transferFile,
      JSON.stringify({ ...saved, fee: String(BigInt(saved.fee) + 1n) }),
    );
    await expect(runStackWorkflow(value.context, transfer)).rejects.toThrow(
      "differs from its exact transaction identity or fee",
    );
    expect(value.sent).toHaveLength(1);
  });
  it("retries an unseen transfer and stops on a rejected one", async () => {
    const value = await journey();
    const transfer = value.byId("cycle-0-transfer");
    expect(await transfer.reconcile(undefined)).toEqual({ status: "retry" });
    value.txStatus({ status: "not_found" });
    await expect(runStackWorkflow(value.context, [transfer])).rejects.toThrow(
      "response lost",
    );
    expect(await transfer.reconcile(undefined)).toEqual({ status: "retry" });
    value.txStatus({ status: "rejected", reasonCode: "E_TEST" });
    await expect(transfer.reconcile(undefined)).rejects.toThrow(
      "Transfer rejected: E_TEST",
    );
  });
  it("checks the user's L1 funds before the first deposit is sent", async () => {
    const value = await journey();
    await runStackWorkflow(value.context, []);
    const deposit = value.byId("cycle-0-deposit");
    value.processes.responses["cycle-0-deposit-funds"] = {
      totals: { lovelace: value.config.journey.depositLovelace },
    };
    await expect(deposit.execute(undefined)).rejects.toThrow(
      "including fee headroom",
    );
    expect(value.processes.calls.map((call) => call.id)).toEqual([
      "cycle-0-deposit-funds",
    ]);
    const intent = join(
      value.config.runDirectory,
      "cycle-0-deposit-intent.json",
    );
    expect(await readJsonIfPresent(intent)).toBeUndefined();
    value.processes.responses["cycle-0-deposit-funds"] = {
      totals: { lovelace: "1000000000" },
    };
    value.processes.responses["storage-identity"] = { id: "7" };
    value.processes.responses["host-database-identity"] = "8";
    await expect(deposit.execute(undefined)).rejects.toThrow(
      "does not reach this stack's Postgres",
    );
    value.processes.responses["host-database-identity"] = "7";
    // A resend reuses the saved intent instead of re-checking spent funds.
    await deposit.execute(undefined);
    await deposit.execute(undefined);
    const submit = [
      "storage-identity",
      "host-database-identity",
      "cycle-0-deposit-submit",
    ];
    expect(value.processes.calls.map((call) => call.id)).toEqual([
      "cycle-0-deposit-funds",
      "cycle-0-deposit-funds",
      "storage-identity",
      "host-database-identity",
      ...submit,
      ...submit,
    ]);
  });
});
