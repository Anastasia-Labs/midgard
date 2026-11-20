import "./utils.js";

import { mkdir, mkdtemp, readFile, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  assetsToValue,
  CML,
  getAddressDetails,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { formatJson } from "@/commands/command-utils.js";
import { planStressCorpus } from "@/commands/stress-corpus/plan.js";
import { verifyStressCorpus } from "@/commands/stress-corpus/verify.js";
import { computeStressCorpusWalletSetIdentity } from "@/commands/stress-corpus/wallet-set-identity.js";
import {
  generateStressCorpus,
  parseStressCorpusGenerateConfig,
  parseStressCorpusVerifyConfig,
} from "@/commands/stress-corpus-generate.js";
import { STRESS_WALLET_SCHEMA_VERSION } from "@/commands/stress-wallets.js";

import { makeMidgardTxOutput } from "./midgard-output-helpers.js";

const TEST_SEEDS = [
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
] as const;

const walletLabel = (index: number): string =>
  index.toString().padStart(4, "0");

const fundingOutputCbor = (address: string, lovelace: bigint): string =>
  Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      assetsToValue({ lovelace }),
    ).to_cbor_bytes(),
  ).toString("hex");

const writePreparedWallet = async ({
  walletsDir,
  seedPhrase,
  index,
  lovelace,
}: {
  readonly walletsDir: string;
  readonly seedPhrase: string;
  readonly index: number;
  readonly lovelace: bigint;
}): Promise<void> => {
  const wallet = walletFromSeed(seedPhrase, { network: "Preprod" });
  const paymentCredential = getAddressDetails(wallet.address).paymentCredential;
  if (paymentCredential?.type !== "Key") {
    throw new Error("test wallet must have a payment key credential");
  }
  const label = walletLabel(index);
  const txHash = index.toString(16).padStart(64, "0");
  const record = {
    schemaVersion: STRESS_WALLET_SCHEMA_VERSION,
    walletId: `stress-wallet-${label}`,
    index,
    envName: `STRESS_WALLET_SEED_PHRASE_${label}`,
    network: "Preprod",
    seedPhrase,
    l2Address: wallet.address,
    paymentKeyHash: paymentCredential.hash,
    createdAt: "2026-07-08T00:00:00.000Z",
    latestFunding: {
      preparedAt: "2026-07-08T00:00:00.000Z",
      status: "already_funded",
      lovelacePerWallet: lovelace.toString(10),
      nodeEndpoint: "http://127.0.0.1:3000",
      beforeUtxoCount: 1,
      afterUtxoCount: 1,
      verifiedFundingUtxoCount: 1,
      fundingUtxos: [
        {
          outref: `${txHash}#0`,
          outputCbor: fundingOutputCbor(wallet.address, lovelace),
          lovelace: lovelace.toString(10),
        },
      ],
    },
  };
  await writeFile(
    join(walletsDir, `wallet-${label}.json`),
    `${formatJson(record)}\n`,
    "utf8",
  );
};

const makeWalletDir = async (): Promise<string> => {
  const dir = await mkdtemp(join(tmpdir(), "midgard-stress-corpus-wallets-"));
  await mkdir(dir, { recursive: true });
  await Promise.all(
    TEST_SEEDS.map((seedPhrase, index) =>
      writePreparedWallet({
        walletsDir: dir,
        seedPhrase,
        index: index + 1,
        lovelace: 5_000_000n,
      }),
    ),
  );
  return dir;
};

describe("stress corpus planner", () => {
  it("sizes grouped chains and rejects unsafe wallet counts", () => {
    const plan = planStressCorpus({
      targetRateTps: 2_500,
      durationMs: 600_000,
      walletCount: 4_096,
      safetyFactor: 1.1,
      amountLovelace: 1_000_000n,
      minFeeA: 0n,
      minFeeB: 3_110n,
      assumedAcceptanceLatencyMs: 1_000,
    });

    expect(plan.walletCount).toBe(4_096);
    expect(plan.chainDepth).toBeGreaterThanOrEqual(403);
    expect(plan.interleavingPlan).toBe("grouped-by-chain");
    expect(plan.perWalletFundingLovelace).toBe(
      1_000_000n * BigInt(plan.chainDepth + 1) +
        3_110n * BigInt(plan.chainDepth),
    );
    const defaultPlan = planStressCorpus({
      targetRateTps: 2_500,
      durationMs: 600_000,
      amountLovelace: 1_000_000n,
      minFeeA: 0n,
      minFeeB: 3_110n,
      assumedAcceptanceLatencyMs: 1_000,
    });
    expect(defaultPlan.walletCount).toBe(4_096);
    expect(defaultPlan.chainDepth).toBe(403);
    expect(defaultPlan.rowCount).toBe(1_650_688);
    expect(() =>
      planStressCorpus({
        targetRateTps: 2_500,
        durationMs: 1_000,
        walletCount: 128,
        amountLovelace: 1_000_000n,
        minFeeA: 0n,
        minFeeB: 0n,
      }),
    ).toThrow("below the minimum");
  });

  it("sizes the exact Phase 1 continuous ten-minute corpus", () => {
    const plan = planStressCorpus({
      targetRateTps: 5_000,
      durationMs: 600_000,
      walletCount: 4_096,
      safetyFactor: 1.02,
      amountLovelace: 1n,
      minFeeA: 10n,
      minFeeB: 10n,
      assumedAcceptanceLatencyMs: 819,
    });

    expect(plan).toMatchObject({
      walletCount: 4_096,
      chainDepth: 748,
      rowCount: 3_063_808,
      perWalletFundingLovelace: 11_228_229n,
      totalFundingLovelace: 45_990_825_984n,
    });

    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "5000",
        durationMs: "600000",
        walletCount: "4096",
        safetyFactor: "1.02",
        amountLovelace: "1",
        minFeeA: "10",
        minFeeB: "10",
        maxSubmitTxCborBytes: "32768",
        assumedAcceptanceLatencyMs: "819",
        slices: "1",
        corpusSliceIdPrefix: "phase1",
        yes: true,
      },
      {},
    );
    expect(config.slices).toBe(1);
    expect(config.sliceWalletCounts).toBeUndefined();
  });
});

describe("stress corpus generation", () => {
  it("generates, assembles, manifests, and verifies a grouped chain corpus", async () => {
    const walletsDir = await makeWalletDir();
    const outDir = await mkdtemp(join(tmpdir(), "midgard-stress-corpus-out-"));
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: "32768",
        walletsDir,
        outDir,
        workers: "1",
        sliceWalletCounts: "1,1",
        corpusSliceIdPrefix: "phase1",
        yes: true,
      },
      {},
    );

    const result = await generateStressCorpus(config);

    expect(result.plan.chainDepth).toBe(2);
    expect(result.assembled.rowCount).toBe(4);
    expect(result.assembled.chainCount).toBe(2);
    expect(result.verified.rowCount).toBe(4);
    const manifest = JSON.parse(
      await readFile(result.manifestPath, "utf8"),
    ) as {
      readonly corpusSliceIds: readonly string[];
      readonly walletSetIdentity: typeof result.walletSetIdentity;
      readonly sliceSummary: readonly {
        readonly corpusSliceId: string;
        readonly walletCount: number;
        readonly rowCount: number;
      }[];
    };
    expect(manifest.walletSetIdentity).toEqual(result.walletSetIdentity);
    expect(manifest.corpusSliceIds).toEqual(["phase1-1", "phase1-2"]);
    expect(manifest.sliceSummary).toEqual([
      { corpusSliceId: "phase1-1", walletCount: 1, rowCount: 2 },
      { corpusSliceId: "phase1-2", walletCount: 1, rowCount: 2 },
    ]);
    expect(result.verified.rebuildSample).toMatchObject({
      sampleRate: 0.001,
      checkedChainCount: 1,
      checkedRowCount: 2,
    });
    expect(result.walletSetIdentity).toEqual(result.verified.walletSetIdentity);
    expect(result.verified.verificationArtifact.sha256).toMatch(
      /^[0-9a-f]{64}$/u,
    );
    const verificationArtifact = await readFile(
      result.verified.verificationArtifact.path,
      "utf8",
    );
    expect(verificationArtifact).not.toContain(TEST_SEEDS[0]);
    expect(verificationArtifact).not.toContain(TEST_SEEDS[1]);
    expect(result.verified.rebuildSample.livePreflightEntries).toHaveLength(1);
    expect(result.walletSetIdentity).toMatchObject({
      walletCount: 2,
      fundingRowCount: 2,
      uniqueFirstFundingOutrefCount: 2,
      walletSetSha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
      fundingSetSha256: expect.stringMatching(/^[0-9a-f]{64}$/u),
    });
    expect(JSON.stringify(result.walletSetIdentity)).not.toContain(
      TEST_SEEDS[0],
    );
    const standaloneVerifyConfig = parseStressCorpusVerifyConfig(
      {
        corpusPath: result.corpusPath,
        rebuildWalletsDir: walletsDir,
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: "32768",
      },
      {},
    );
    await expect(
      verifyStressCorpus({
        corpusPath: result.corpusPath,
        indexPath: result.indexPath,
        manifestPath: result.manifestPath,
        rebuildSample: standaloneVerifyConfig.rebuildSample!,
      }),
    ).resolves.toMatchObject({
      rowCount: 4,
      chainCount: 2,
      rebuildSample: {
        checkedChainCount: 1,
        checkedRowCount: 2,
      },
      walletSetIdentity: result.walletSetIdentity,
    });
  });

  it("rejects a wallet directory with records outside the exact current-run set", async () => {
    const walletsDir = await makeWalletDir();
    await writeFile(join(walletsDir, "wallet-0003.json"), "{}\n", "utf8");
    const outDir = await mkdtemp(
      join(tmpdir(), "midgard-corpus-extra-wallet-"),
    );
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: "32768",
        walletsDir,
        outDir,
        workers: "1",
        yes: true,
      },
      {},
    );

    await expect(generateStressCorpus(config)).rejects.toThrow(
      "expected exactly 2 for the current run",
    );
  });

  it("standalone verification requires complete funding snapshots for every chain, not only the rebuild sample", async () => {
    const walletsDir = await makeWalletDir();
    const outDir = await mkdtemp(
      join(tmpdir(), "midgard-corpus-full-funding-"),
    );
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: "32768",
        walletsDir,
        outDir,
        workers: "1",
        rebuildSampleRate: "0.5",
        yes: true,
      },
      {},
    );
    const result = await generateStressCorpus(config);
    const sampled = new Set(result.verified.rebuildSample.sampledChainIds);
    const unsampledIndex = sampled.has("stress-wallet-0001") ? 2 : 1;
    const unsampledPath = join(
      walletsDir,
      `wallet-${walletLabel(unsampledIndex)}.json`,
    );
    const unsampledRecord = JSON.parse(
      await readFile(unsampledPath, "utf8"),
    ) as Record<string, unknown>;
    delete unsampledRecord.latestFunding;
    await writeFile(unsampledPath, `${formatJson(unsampledRecord)}\n`, "utf8");

    await expect(
      verifyStressCorpus({
        corpusPath: result.corpusPath,
        indexPath: result.indexPath,
        manifestPath: result.manifestPath,
        rebuildSample: {
          walletsDir,
          amountLovelace: 1_000_000n,
          feeParams: { minFeeA: 0n, minFeeB: 0n },
          network: "Preprod",
          networkId: 0n,
          maxSubmitTxCborBytes: 32_768,
          sampleRate: 0.5,
        },
      }),
    ).rejects.toThrow("must contain at least one latestFunding");
  });

  it("standalone verification rejects wallet records outside the exact indexed chain set", async () => {
    const walletsDir = await makeWalletDir();
    const outDir = await mkdtemp(join(tmpdir(), "midgard-corpus-exact-set-"));
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: "32768",
        walletsDir,
        outDir,
        workers: "1",
        yes: true,
      },
      {},
    );
    const result = await generateStressCorpus(config);
    await writeFile(
      join(walletsDir, "wallet-0003.json"),
      await readFile(join(walletsDir, "wallet-0001.json"), "utf8"),
      "utf8",
    );

    await expect(
      verifyStressCorpus({
        corpusPath: result.corpusPath,
        indexPath: result.indexPath,
        manifestPath: result.manifestPath,
        rebuildSample: {
          walletsDir,
          amountLovelace: 1_000_000n,
          feeParams: { minFeeA: 0n, minFeeB: 0n },
          network: "Preprod",
          networkId: 0n,
          maxSubmitTxCborBytes: 32_768,
        },
      }),
    ).rejects.toThrow(
      "wallet record count 3 must equal expected current-run count 2",
    );
  });

  it("computes stable full-set hashes without seed phrase material", async () => {
    const walletsDir = await makeWalletDir();
    const records = await Promise.all(
      [1, 2].map(async (index) =>
        JSON.parse(
          await readFile(
            join(walletsDir, `wallet-${walletLabel(index)}.json`),
            "utf8",
          ),
        ),
      ),
    );
    const forward = computeStressCorpusWalletSetIdentity({
      records,
      expectedWalletCount: 2,
    });
    const reversed = computeStressCorpusWalletSetIdentity({
      records: [...records].reverse(),
      expectedWalletCount: 2,
    });

    expect(reversed).toEqual(forward);
    expect(forward.walletSetSha256).toMatch(/^[0-9a-f]{64}$/u);
    expect(forward.fundingSetSha256).toMatch(/^[0-9a-f]{64}$/u);
    expect(JSON.stringify(forward)).not.toContain(TEST_SEEDS[0]);
    expect(JSON.stringify(forward)).not.toContain(TEST_SEEDS[1]);
  });

  it("rejects duplicate first funding outrefs across the full wallet set", async () => {
    const walletsDir = await makeWalletDir();
    const records = await Promise.all(
      [1, 2].map(async (index) =>
        JSON.parse(
          await readFile(
            join(walletsDir, `wallet-${walletLabel(index)}.json`),
            "utf8",
          ),
        ),
      ),
    );
    records[1].latestFunding.fundingUtxos[0] =
      records[0].latestFunding.fundingUtxos[0];

    expect(() =>
      computeStressCorpusWalletSetIdentity({
        records,
        expectedWalletCount: 2,
      }),
    ).toThrow("duplicate first funding outref");
  });

  it("rejects duplicate selected inputs during verification", async () => {
    const walletsDir = await makeWalletDir();
    const outDir = await mkdtemp(join(tmpdir(), "midgard-stress-corpus-bad-"));
    const config = parseStressCorpusGenerateConfig(
      {
        targetRateTps: "1",
        durationMs: "4000",
        walletCount: "2",
        safetyFactor: "1",
        amountLovelace: "1000000",
        minFeeA: "0",
        minFeeB: "0",
        maxSubmitTxCborBytes: "32768",
        walletsDir,
        outDir,
        workers: "1",
        yes: true,
      },
      {},
    );
    const result = await generateStressCorpus(config);
    const lines = (await readFile(result.corpusPath, "utf8"))
      .trim()
      .split("\n");
    const first = JSON.parse(lines[0]!) as {
      readonly selectedInputOutref: string;
    };
    const second = JSON.parse(lines[1]!) as Record<string, unknown>;
    second.selectedInputOutref = first.selectedInputOutref;
    lines[1] = JSON.stringify(second);
    await writeFile(result.corpusPath, `${lines.join("\n")}\n`, "utf8");

    await expect(
      verifyStressCorpus({
        corpusPath: result.corpusPath,
        indexPath: result.indexPath,
      }),
    ).rejects.toThrow("duplicate selected input");
  });
});
