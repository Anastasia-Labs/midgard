import { mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { Worker } from "node:worker_threads";

import {
  formatJson,
  networkIdFromName,
} from "midgard-node/commands/command-utils";
import { resolveWorkerEntry } from "midgard-node/fibers/resolve-worker-entry";

import packageJson from "../../package.json" with { type: "json" };
import {
  type CorpusWorkerInput,
  type CorpusWorkerOutput,
  type CorpusWorkerWallet,
  runCorpusChainWorker,
} from "../workers/corpus-chain-builder.js";
import { assembleCorpusShards } from "./stress-corpus/assemble.js";
import {
  planStressCorpus,
  type StressCorpusPlan,
} from "./stress-corpus/plan.js";
import {
  parseStressCorpusManifest,
  STRESS_CORPUS_MANIFEST_SCHEMA_VERSION,
  STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
  verifyStressCorpus,
} from "./stress-corpus/verify.js";
import { computeStressCorpusWalletSetIdentity } from "./stress-corpus/wallet-set-identity.js";
import {
  corpusSliceId,
  fundingUtxoForRecord,
  readWalletRecords,
} from "./stress-corpus-generate.parse-stress-corpus-generate-config.js";
import { parseStressCorpusGenerationArtifact } from "./stress-corpus-generate.parse-stress-corpus-generation-artifact.js";
import {
  execFileAsync,
  type StressCorpusGenerateConfig,
  type StressCorpusGenerateResult,
} from "./stress-corpus-generate.stress-corpus-generate-config.js";

const gitSha = async (): Promise<string> => {
  try {
    const result = await execFileAsync("git", ["rev-parse", "HEAD"], {
      cwd: process.cwd(),
    });
    return result.stdout.trim();
  } catch {
    return "unknown";
  }
};

const workerInputForBatch = ({
  shardPath,
  walletBatch,
  plan,
  config,
}: {
  readonly shardPath: string;
  readonly walletBatch: readonly CorpusWorkerWallet[];
  readonly plan: StressCorpusPlan;
  readonly config: StressCorpusGenerateConfig;
}): CorpusWorkerInput => ({
  shardPath,
  walletBatch,
  depth: plan.chainDepth,
  amountLovelace: config.amountLovelace.toString(10),
  feeParams: {
    minFeeA: config.minFeeA.toString(10),
    minFeeB: config.minFeeB.toString(10),
  },
  network: config.network,
  networkId: networkIdFromName(config.network).toString(10),
  maxSubmitTxCborBytes: config.maxSubmitTxCborBytes,
  planShape: "chain",
  terminalChangeFloorLovelace: config.amountLovelace.toString(10),
});

const runWorkerProcess = async (
  input: CorpusWorkerInput,
): Promise<Extract<CorpusWorkerOutput, { readonly type: "done" }>> =>
  new Promise((resolve, reject) => {
    const worker = new Worker(
      resolveWorkerEntry(import.meta.url, "corpus-chain-builder.js"),
      {
        workerData: { data: input },
      },
    );
    let settled = false;
    worker.on("message", (message: CorpusWorkerOutput) => {
      if (message.type === "progress") {
        return;
      }
      settled = true;
      worker.terminate().catch(() => undefined);
      if (message.type === "failure") {
        reject(new Error(message.error));
      } else {
        resolve(message);
      }
    });
    worker.on("error", (error) => {
      settled = true;
      reject(error);
    });
    worker.on("exit", (code) => {
      if (!settled && code !== 0) {
        reject(
          new Error(
            `corpus-chain-builder worker exited with ${code.toString()}`,
          ),
        );
      }
    });
  });

const runShardBuilders = async ({
  wallets,
  config,
  plan,
}: {
  readonly wallets: readonly CorpusWorkerWallet[];
  readonly config: StressCorpusGenerateConfig;
  readonly plan: StressCorpusPlan;
}): Promise<readonly string[]> => {
  const workerCount = Math.min(config.workers, wallets.length);
  const batchSize = Math.ceil(wallets.length / workerCount);
  const shardDir = join(config.outDir, "shards");
  await mkdir(shardDir, { recursive: true });
  const inputs = Array.from({ length: workerCount }, (_entry, workerIndex) => {
    const start = workerIndex * batchSize;
    const walletBatch = wallets.slice(start, start + batchSize);
    return workerInputForBatch({
      shardPath: join(
        shardDir,
        `corpus.shard-${workerIndex.toString().padStart(2, "0")}.ndjson`,
      ),
      walletBatch,
      plan,
      config,
    });
  }).filter((input) => input.walletBatch.length > 0);
  const results =
    config.workers === 1
      ? [await runCorpusChainWorker(inputs[0]!)]
      : await Promise.all(inputs.map(runWorkerProcess));
  return results
    .sort((left, right) => left.shardPath.localeCompare(right.shardPath))
    .map((result) => result.shardPath);
};

export const generateStressCorpus = async (
  config: StressCorpusGenerateConfig,
): Promise<StressCorpusGenerateResult> => {
  if (!config.yes) {
    throw new Error("Refusing to generate stress corpus without --yes.");
  }
  if (config.fundingSource === "fanout") {
    throw new Error(
      "stress-corpus-generate --funding-source fanout is reserved for stress-wallets:fanout; generate from existing verified wallet funding snapshots.",
    );
  }
  const plan = planStressCorpus({
    targetRateTps: config.targetRateTps,
    durationMs: config.durationMs,
    warmupCount: config.warmupCount,
    cooldownCount: config.cooldownCount,
    walletCount: config.walletCount,
    safetyFactor: config.safetyFactor,
    amountLovelace: config.amountLovelace,
    minFeeA: config.minFeeA,
    minFeeB: config.minFeeB,
    assumedAcceptanceLatencyMs: config.assumedAcceptanceLatencyMs,
  });
  if (config.sliceWalletCounts !== undefined) {
    const configuredWalletTotal = config.sliceWalletCounts.reduce(
      (sum, count) => sum + count,
      0,
    );
    if (configuredWalletTotal !== plan.walletCount) {
      throw new Error(
        `--slice-wallet-counts sum ${configuredWalletTotal.toString()} must equal planned walletCount ${plan.walletCount.toString()}.`,
      );
    }
  }
  const records = await readWalletRecords(config.walletsDir, plan.walletCount);
  const walletSetIdentity = computeStressCorpusWalletSetIdentity({
    records,
    expectedWalletCount: plan.walletCount,
  });
  const wallets = records.map(
    (record, index): CorpusWorkerWallet => ({
      seedPhrase: record.seedPhrase,
      walletId: record.walletId,
      fundingUtxo: fundingUtxoForRecord(record),
      corpusSliceId: corpusSliceId({
        walletIndex: index,
        slices: config.slices,
        prefix: config.corpusSliceIdPrefix,
        ...(config.sliceWalletCounts === undefined
          ? {}
          : { sliceWalletCounts: config.sliceWalletCounts }),
      }),
    }),
  );
  await mkdir(config.outDir, { recursive: true });
  const shardPaths = await runShardBuilders({ wallets, config, plan });
  const corpusPath = join(config.outDir, "corpus.ndjson");
  const indexPath = `${corpusPath}.index.ndjson`;
  const manifestPath = `${corpusPath}.manifest.json`;
  const assembled = await assembleCorpusShards({
    shardPaths,
    corpusPath,
    indexPath,
  });
  const manifest = {
    schemaVersion: STRESS_CORPUS_MANIFEST_SCHEMA_VERSION,
    targetRateTps: config.targetRateTps,
    durationMs: config.durationMs,
    warmupCount: config.warmupCount,
    cooldownCount: config.cooldownCount,
    safetyFactor: config.safetyFactor,
    assumedAcceptanceLatencyMs: config.assumedAcceptanceLatencyMs,
    chainCount: assembled.chainCount,
    chainDepth: plan.chainDepth,
    corpusShape: plan.corpusShape,
    corpusSliceIds: [...new Set(wallets.map((wallet) => wallet.corpusSliceId))],
    generatedAtIso: new Date().toISOString(),
    generatorGitSha: await gitSha(),
    lucidMidgardVersion:
      packageJson.dependencies["@al-ft/lucid-midgard"] ?? "unknown",
    feeParams: {
      minFeeA: config.minFeeA.toString(10),
      minFeeB: config.minFeeB.toString(10),
    },
    network: config.network,
    networkId: networkIdFromName(config.network).toString(10),
    maxSubmitTxCborBytes: config.maxSubmitTxCborBytes,
    amountTemplate: {
      lovelace: config.amountLovelace.toString(10),
      shape: "self-transfer-change-chain",
    },
    verification: {
      rebuildSampleRate: config.rebuildSampleRate,
      rebuildSampleAlgorithm: STRESS_CORPUS_REBUILD_SAMPLE_ALGORITHM,
    },
    fundingSummary: {
      walletCount: plan.walletCount,
      perWalletFundingLovelace: plan.perWalletFundingLovelace.toString(10),
      totalFundingLovelace: plan.totalFundingLovelace.toString(10),
    },
    walletSetIdentity,
    sliceSummary: [
      ...new Set(wallets.map((wallet) => wallet.corpusSliceId)),
    ].map((sliceId) => {
      const walletCount = wallets.filter(
        (wallet) => wallet.corpusSliceId === sliceId,
      ).length;
      return {
        corpusSliceId: sliceId,
        walletCount,
        rowCount: walletCount * plan.chainDepth,
      };
    }),
    files: {
      corpus: {
        path: corpusPath,
        sha256: assembled.corpusSha256,
        rowCount: assembled.rowCount,
      },
      index: {
        path: indexPath,
        sha256: assembled.indexSha256,
        rowCount: assembled.chainCount,
      },
      shards: shardPaths,
    },
  };
  const canonicalManifest = parseStressCorpusManifest(
    JSON.parse(formatJson(manifest)) as unknown,
  );
  await writeFile(manifestPath, `${formatJson(canonicalManifest)}\n`, "utf8");
  const verified = await verifyStressCorpus({
    corpusPath,
    indexPath,
    manifestPath,
    rebuildSample: {
      walletsDir: config.walletsDir,
      amountLovelace: config.amountLovelace,
      feeParams: {
        minFeeA: config.minFeeA,
        minFeeB: config.minFeeB,
      },
      network: config.network,
      networkId: networkIdFromName(config.network),
      maxSubmitTxCborBytes: config.maxSubmitTxCborBytes,
      sampleRate: config.rebuildSampleRate,
      terminalChangeFloorLovelace: config.amountLovelace,
    },
    resultOutPath: `${corpusPath}.verify.json`,
  });
  const result: StressCorpusGenerateResult = {
    schemaVersion: "midgard-stress-corpus-generation-v1",
    outDir: config.outDir,
    corpusPath,
    indexPath,
    manifestPath,
    plan,
    walletSetIdentity,
    assembled,
    verified: {
      rowCount: verified.rowCount,
      chainCount: verified.chainCount,
      corpusSha256: verified.corpusSha256,
      indexSha256: verified.indexSha256,
      rebuildSample: verified.rebuildSample!,
      walletSetIdentity: verified.walletSetIdentity!,
      verificationArtifact: verified.verificationArtifact!,
    },
  };
  parseStressCorpusGenerationArtifact(
    JSON.parse(formatJson(result)) as unknown,
  );
  return result;
};
