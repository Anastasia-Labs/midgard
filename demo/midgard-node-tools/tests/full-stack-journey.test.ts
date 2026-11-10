import { access, readFile, stat, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import { readJsonIfPresent } from "../src/full-stack/journal.js";
import {
  assertDepositCredit,
  assertDepositFunding,
  assertTransferDeltas,
  assertWithdrawalDebit,
} from "../src/full-stack/journey-balances.js";
import { runStackWorkflow } from "../src/full-stack/workflow.js";
import {
  type RecordedCall,
  removeStackFixtures,
} from "./full-stack-fixtures.js";
import {
  committedTransfer,
  DEPOSIT_EVENT,
  HEADER_HASH,
  journey,
  savedDeposit,
  savedTransfer,
  TRANSFER_TX,
} from "./full-stack-journey.fixtures.js";

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

describe("wallet journey order on a long maturity", () => {
  it("finishes a deposit once its credit is spendable on L2, before its settlement completes", async () => {
    const value = await journey();
    // One read of each condition: a wait that does not hold at once fails.
    value.config.timeoutMs = 1;
    await value.writeManifest();
    await savedDeposit(value);
    value.processes.responses["settlement-deposit-observation"] = {
      jobs: [{ phase: "absorbed" }],
      attempts: null,
    };
    const deposit = value.byId("cycle-0-deposit");
    // Not yet spendable: the step waits and records no credit.
    await expect(deposit.reconcile(undefined)).rejects.toThrow(
      "did not complete within 1ms",
    );
    expect(await readJsonIfPresent(value.file("deposit-balance"))).toBe(
      undefined,
    );
    const credited = BigInt(value.config.journey.depositLovelace);
    value.hold(value.user, [
      { txHash: "44".repeat(32), lovelace: 50_000_000n },
      { txHash: "45".repeat(32), lovelace: credited },
    ]);
    await expect(deposit.reconcile(undefined)).resolves.toMatchObject({
      status: "complete",
      data: { eventId: DEPOSIT_EVENT },
    });
    expect(await readJsonIfPresent(value.file("deposit-balance"))).toEqual({
      before: "50000000",
      after: String(50_000_000n + credited),
      credited: value.config.journey.depositLovelace,
    });
  });
  it("finishes a transfer at its commit and public DA, before it merges", async () => {
    const value = await journey();
    value.config.timeoutMs = 1;
    await savedTransfer(value);
    value.txStatus(committedTransfer(false));
    await expect(
      value.byId("cycle-0-transfer").reconcile(undefined),
    ).resolves.toMatchObject({
      status: "complete",
      data: { committed: { txId: TRANSFER_TX, headerHash: HEADER_HASH } },
    });
    expect(value.verifiedDa).toEqual([HEADER_HASH]);
    expect(await readJsonIfPresent(value.file("transfer-balance"))).toEqual({
      senderBalance: String(
        50_000_000n - BigInt(value.config.journey.transferLovelace) - 200_000n,
      ),
      recipientBalance: value.config.journey.transferLovelace,
      fee: "200000",
      received: value.config.journey.transferLovelace,
    });
  });
  it("checks each cycle's deposit settlement and transfer merge after its payout", async () => {
    const value = await journey();
    value.config.timeoutMs = 1;
    expect(
      value.steps
        .map((step) => step.id)
        .filter((id) => id.startsWith("cycle-0-")),
    ).toEqual([
      "cycle-0-deposit",
      "cycle-0-transfer",
      "cycle-0-withdrawal",
      "cycle-0-settlement",
    ]);
    await value.writeManifest();
    await savedDeposit(value);
    await savedTransfer(value);
    const settlement = value.byId("cycle-0-settlement");
    const observed = (phase: string) => ({ jobs: [{ phase }], attempts: null });
    // The deposit's settlement job has not completed.
    value.processes.responses["settlement-deposit-observation"] =
      observed("absorbed");
    value.txStatus(committedTransfer(true));
    await expect(settlement.reconcile(undefined)).rejects.toThrow(
      "deposit settlement did not complete within 1ms",
    );
    // The transfer's block has not merged into the confirmed ledger.
    value.processes.responses["settlement-deposit-observation"] =
      observed("complete");
    value.txStatus(committedTransfer(false));
    await expect(settlement.reconcile(undefined)).rejects.toThrow(
      "transfer merge did not complete within 1ms",
    );
    value.txStatus(committedTransfer(true));
    await expect(settlement.reconcile(undefined)).resolves.toEqual({
      status: "complete",
      data: {
        deposit: { eventId: DEPOSIT_EVENT, observed: observed("complete") },
        transfer: { merged: committedTransfer(true) },
      },
    });
  });
});

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
  it("gives a public deposit a private seed file that is gone when the command ends", async () => {
    const value = await journey();
    // A preprod-public stack signs with seeds from files, never the environment.
    const root = value.config.runDirectory;
    value.config.public = {
      nodeConfigFile: join(root, "node-secrets.yaml"),
      tlsCertFile: join(root, "node.pem"),
      tlsKeyFile: join(root, "node.key"),
      tlsCaFile: join(root, "ca.pem"),
      trustRootsFile: join(root, "trust-roots.json"),
      blueprintClassificationFile: join(root, "classification.json"),
      releaseDirectory: join(root, "release"),
      ingressHostPort: null,
      retainedDaHost: "retained-da.node.example.test",
    };
    value.processes.env.MIDGARD_PUBLIC_API_ORIGIN = "https://node.example.test";
    await runStackWorkflow(value.context, []);
    const deposit = value.byId("cycle-0-deposit");
    value.processes.responses["cycle-0-deposit-funds"] = {
      totals: { lovelace: "1000000000" },
    };
    const seen: { file: string; mode: number; seed: string }[] = [];
    let fail = true;
    value.processes.responses["cycle-0-deposit-submit"] = async (
      call: RecordedCall,
    ) => {
      expect(call.args).not.toContain("--wallet-seed-phrase-env");
      expect(Object.values(call.overrides)).not.toContain(
        value.processes.env[value.config.wallets.user!.seedEnv],
      );
      const file =
        call.args[call.args.indexOf("--wallet-seed-phrase-file") + 1]!;
      seen.push({
        file,
        mode: (await stat(file)).mode & 0o777,
        seed: await readFile(file, "utf8"),
      });
      if (fail) throw new Error("submit failed");
      return { txHash: "d".repeat(64) };
    };
    await expect(deposit.execute(undefined)).rejects.toThrow("submit failed");
    fail = false;
    await deposit.execute(undefined);
    const seed = value.processes.env[value.config.wallets.user!.seedEnv]!;
    expect(seen).toHaveLength(2);
    for (const { file, mode, seed: written } of seen) {
      expect(mode).toBe(0o600);
      expect(written).toBe(seed);
      await expect(access(file)).rejects.toThrow("ENOENT");
    }
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
    // A resend reuses the saved intent instead of re-checking spent funds.
    await deposit.execute(undefined);
    await deposit.execute(undefined);
    expect(value.processes.calls.map((call) => call.id)).toEqual([
      "cycle-0-deposit-funds",
      "cycle-0-deposit-funds",
      "cycle-0-deposit-submit",
      "cycle-0-deposit-submit",
    ]);
  });
  it("deposits and withdraws with the user's own tool, never the node's database", async () => {
    const value = await journey();
    await runStackWorkflow(value.context, []);
    value.processes.responses["cycle-0-deposit-funds"] = {
      totals: { lovelace: "1000000000" },
    };
    await value.byId("cycle-0-deposit").execute(undefined);
    const funds = value.processes.calls.find(
      (call) => call.id === "cycle-0-deposit-funds",
    )!;
    expect(funds.overrides).toMatchObject({
      L1_ACCESS: "kupmios",
      L1_KUPO_URL: "http://127.0.0.1:1442",
      L1_OGMIOS_URL: "http://127.0.0.1:1337",
    });
    const submit = value.processes.calls.at(-1)!;
    expect(submit.id).toBe("cycle-0-deposit-submit");
    expect(submit.args.slice(0, 2)).toEqual(["dist/cli.js", "deposit"]);
    expect(submit.args).toContain("--json");
    // The host toolchain only: no node configuration, no Postgres.
    expect(submit.scope).toBe("host");
    const seedEnv = value.config.wallets.user!.seedEnv;
    expect(submit.args).toContain(seedEnv);
    expect(submit.overrides).toMatchObject({
      L1_ACCESS: "kupmios",
      L1_KUPO_URL: "http://127.0.0.1:1442",
      L1_OGMIOS_URL: "http://127.0.0.1:1337",
      [seedEnv]: value.processes.env[seedEnv],
      MIDGARD_DEPLOYMENT_MANIFEST: join(
        value.config.nodeRoot,
        "deploymentInfo/contract-deployment-info.json",
      ),
      MIDGARD_USER_RECORD_DIR: join(
        value.config.runDirectory,
        "user-submissions",
      ),
    });
    expect(submit.overrides).not.toHaveProperty("POSTGRES_PORT");
    expect(
      value.processes.calls
        .map((call) => call.id)
        .filter((id) =>
          ["storage-identity", "host-database-identity"].includes(id),
        ),
    ).toEqual([]);
  });
});
