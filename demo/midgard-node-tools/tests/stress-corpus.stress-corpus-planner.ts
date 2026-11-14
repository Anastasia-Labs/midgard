import { mkdir, mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  assetsToValue,
  CML,
  getAddressDetails,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import { formatJson } from "midgard-node/commands/command-utils";
import { makeMidgardTxOutput } from "midgard-node/tests/midgard-output-helpers";
import { describe, expect, it } from "vitest";

import { planStressCorpus } from "../src/commands/stress-corpus/plan.js";
import { parseStressCorpusGenerateConfig } from "../src/commands/stress-corpus-generate.js";
import { STRESS_WALLET_RECORD_SCHEMA_VERSION } from "../src/commands/stress-wallets/index.js";

export const TEST_SEEDS = [
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart",
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail",
] as const;

export const MAX_SUBMIT_TX_CBOR_BYTES =
  MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes;

export const MAX_SUBMIT_TX_CBOR_BYTES_TEXT =
  MAX_SUBMIT_TX_CBOR_BYTES.toString();

export const walletLabel = (index: number): string =>
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
    schemaVersion: STRESS_WALLET_RECORD_SCHEMA_VERSION,
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

export const makeWalletDir = async (): Promise<string> => {
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
        maxSubmitTxCborBytes: MAX_SUBMIT_TX_CBOR_BYTES_TEXT,
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
