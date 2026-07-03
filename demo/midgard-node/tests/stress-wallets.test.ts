import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { formatJson, type NodeUtxo } from "@/commands/command-utils.js";
import {
  createL2Wallets,
  parseStressWalletRecord,
  prepareStressWallets,
  stressWalletFileName,
} from "@/commands/stress-wallets.js";

const TEST_SEEDS = [
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
  "second salad helmet humble left noise inform person swamp surround twice animal fitness sing laundry saddle stove guess cabin rural kidney reject oil fee",
];

const makeTempDir = async (): Promise<string> =>
  mkdtemp(join(tmpdir(), "midgard-stress-wallets-"));

const seedGenerator = () => {
  let index = 0;
  return () => TEST_SEEDS[index++]!;
};

const nodeUtxo = ({
  txHashByte,
  outputIndex = 0,
  address,
  lovelace,
}: {
  readonly txHashByte: string;
  readonly outputIndex?: number;
  readonly address: string;
  readonly lovelace: bigint;
}): NodeUtxo => ({
  txHash: txHashByte.repeat(32),
  outputIndex,
  outrefCbor: Buffer.from(`${txHashByte}${outputIndex.toString(16)}`, "hex"),
  outputCbor: Buffer.from("00", "hex"),
  address,
  assets: { lovelace },
});

describe("stress wallet commands", () => {
  it("creates persisted L2 wallet records and redacted command output", async () => {
    const dir = await makeTempDir();
    try {
      const result = await createL2Wallets({
        count: 2,
        outDir: dir,
        network: "Preprod",
        now: () => new Date("2026-01-01T00:00:00.000Z"),
        generateSeedPhrase: seedGenerator(),
      });

      expect(result.createdCount).toBe(2);
      expect(result.reusedCount).toBe(0);
      expect(result.wallets).toHaveLength(2);
      expect(result.wallets[0]?.envName).toBe("STRESS_WALLET_SEED_PHRASE_0001");
      expect(formatJson(result)).not.toContain(TEST_SEEDS[0]);

      const firstPath = join(dir, stressWalletFileName(1));
      const firstRecord = parseStressWalletRecord(
        JSON.parse(await readFile(firstPath, "utf8")) as unknown,
      );
      expect(firstRecord.seedPhrase).toBe(TEST_SEEDS[0]);
      expect(firstRecord.l2Address).toBe(result.wallets[0]?.l2Address);

      const envFile = await readFile(result.envFilePath, "utf8");
      const argsFile = await readFile(result.argsFilePath, "utf8");
      expect(envFile).toContain(TEST_SEEDS[0]);
      expect(argsFile).toContain(
        "--stress-wallet-seed-phrase-env STRESS_WALLET_SEED_PHRASE_0001",
      );
      expect(argsFile).toContain(
        "--stress-wallet-seed-phrase-env STRESS_WALLET_SEED_PHRASE_0002",
      );
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("prepares wallets by skipping funded records, depositing missing funds, projecting, and verifying /utxos", async () => {
    const dir = await makeTempDir();
    try {
      const created = await createL2Wallets({
        count: 2,
        outDir: dir,
        network: "Preprod",
        now: () => new Date("2026-01-01T00:00:00.000Z"),
        generateSeedPhrase: seedGenerator(),
      });
      const first = created.wallets[0]!;
      const second = created.wallets[1]!;
      const utxosByAddress = new Map<string, readonly NodeUtxo[]>([
        [
          first.l2Address,
          [
            nodeUtxo({
              txHashByte: "11",
              address: first.l2Address,
              lovelace: 10_000_000n,
            }),
          ],
        ],
        [second.l2Address, []],
      ]);
      const submitted: string[] = [];
      let projectCount = 0;

      const result = await prepareStressWallets(
        {
          count: 2,
          outDir: dir,
          network: "Preprod",
          lovelacePerWallet: 5_000_000n,
          nodeEndpoint: "http://127.0.0.1:3000",
          projectionWaitMs: 0,
          verifyTimeoutMs: 1_000,
          pollIntervalMs: 1,
          now: () => new Date("2026-01-01T00:01:00.000Z"),
        },
        {
          submitDeposit: async ({ wallet, lovelace }) => {
            submitted.push(wallet.envName);
            utxosByAddress.set(wallet.l2Address, [
              nodeUtxo({
                txHashByte: "22",
                address: wallet.l2Address,
                lovelace,
              }),
            ]);
            return {
              txHash: "aa".repeat(32),
              depositEventId: "bb".repeat(34),
            };
          },
          projectDeposits: async () => {
            projectCount += 1;
          },
          fetchUtxos: async (_endpoint, address) =>
            utxosByAddress.get(address) ?? [],
          sleep: async () => {},
        },
      );

      expect(submitted).toEqual(["STRESS_WALLET_SEED_PHRASE_0002"]);
      expect(projectCount).toBe(1);
      expect(result.submittedDepositCount).toBe(1);
      expect(result.alreadyFundedCount).toBe(1);
      expect(result.verifiedWalletCount).toBe(2);
      expect(result.wallets.map((entry) => entry.status)).toEqual([
        "already_funded",
        "submitted",
      ]);

      const secondRecord = parseStressWalletRecord(
        JSON.parse(
          await readFile(join(dir, stressWalletFileName(2)), "utf8"),
        ) as unknown,
      );
      expect(secondRecord.latestFunding?.depositTxHash).toBe("aa".repeat(32));
      expect(secondRecord.latestFunding?.verifiedFundingUtxoCount).toBe(1);
      expect(formatJson(result)).not.toContain(TEST_SEEDS[1]);
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("can create missing wallet records during prepare when explicitly requested", async () => {
    const dir = await makeTempDir();
    try {
      const utxosByAddress = new Map<string, readonly NodeUtxo[]>();
      const result = await prepareStressWallets(
        {
          count: 1,
          outDir: dir,
          network: "Preprod",
          createMissing: true,
          lovelacePerWallet: 4_000_000n,
          nodeEndpoint: "http://127.0.0.1:3000",
          projectionWaitMs: 0,
          verifyTimeoutMs: 1_000,
          pollIntervalMs: 1,
          now: () => new Date("2026-01-01T00:02:00.000Z"),
          generateSeedPhrase: seedGenerator(),
        },
        {
          submitDeposit: async ({ wallet, lovelace }) => {
            utxosByAddress.set(wallet.l2Address, [
              nodeUtxo({
                txHashByte: "33",
                address: wallet.l2Address,
                lovelace,
              }),
            ]);
            return { txHash: "cc".repeat(32) };
          },
          projectDeposits: async () => {},
          fetchUtxos: async (_endpoint, address) =>
            utxosByAddress.get(address) ?? [],
          sleep: async () => {},
        },
      );

      expect(result.generatedWalletCount).toBe(1);
      expect(result.submittedDepositCount).toBe(1);
      expect(result.wallets[0]?.wallet.envName).toBe(
        "STRESS_WALLET_SEED_PHRASE_0001",
      );
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });

  it("rejects existing wallet files that do not match the requested env prefix", async () => {
    const dir = await makeTempDir();
    try {
      await createL2Wallets({
        count: 1,
        outDir: dir,
        envPrefix: "OTHER_STRESS_WALLET",
        network: "Preprod",
        generateSeedPhrase: seedGenerator(),
      });

      await expect(
        prepareStressWallets(
          {
            count: 1,
            outDir: dir,
            network: "Preprod",
            lovelacePerWallet: 4_000_000n,
            nodeEndpoint: "http://127.0.0.1:3000",
            projectionWaitMs: 0,
            verifyTimeoutMs: 1,
            pollIntervalMs: 1,
          },
          {
            submitDeposit: async () => ({ txHash: "dd".repeat(32) }),
            projectDeposits: async () => {},
            fetchUtxos: async () => [],
            sleep: async () => {},
          },
        ),
      ).rejects.toThrow("records envName OTHER_STRESS_WALLET_0001");
    } finally {
      await rm(dir, { recursive: true, force: true });
    }
  });
});
