import { access, readFile, stat, writeFile } from "node:fs/promises";
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
import { runStackWorkflow } from "../src/full-stack/workflow.js";
import {
  type RecordedCall,
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
