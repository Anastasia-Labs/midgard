import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  defaultMidgardNodeEndpoint,
  fetchNodeUtxosByAddress,
  formatJson,
} from "midgard-node/commands/command-utils";
import { sleep } from "midgard-node/sleep";

import {
  parseStressWalletFanoutReport,
  parseStressWalletFanoutResult,
} from "./artifacts.js";
import {
  DEFAULT_FANOUT_BRANCH_FACTOR,
  DEFAULT_FANOUT_FEE_HEADROOM_LOVELACE,
  DEFAULT_FANOUT_MAX_IN_FLIGHT,
  DEFAULT_STRESS_WALLET_DIR,
  DEFAULT_VERIFY_POLL_INTERVAL_MS,
  DEFAULT_VERIFY_TIMEOUT_MS,
  STRESS_WALLET_FANOUT_REPORT_SCHEMA_VERSION,
  STRESS_WALLET_FANOUT_RESULT_SCHEMA_VERSION,
} from "./constants.js";
import { withExclusiveStressWalletFundsLock } from "./files.js";
import {
  defaultGenerateSeedPhrase,
  normalizeEnvPrefix,
  requireSafeNonNegativeInteger,
  requireSafePositiveInteger,
} from "./options.js";
import {
  resolveStressWalletRecords,
  summaryForRecord,
  writeStressWalletExports,
  writeStressWalletRecord,
} from "./records.js";
import {
  acceptedTxStatuses,
  nextFanoutPollDelayMs,
  rejectedTxStatuses,
  runBounded,
} from "./runtime.js";
import {
  type FanoutStressWalletsOptions,
  type StressWalletFanoutEdgeSummary,
  type StressWalletFanoutEntry,
  type StressWalletFanoutResult,
  type StressWalletFanoutRuntime,
  type StressWalletFanoutSource,
  type StressWalletRecord,
} from "./types.js";
import {
  fetchFanoutUtxosWithRetry,
  firstFundingUtxo,
  verifiedFundingSnapshots,
} from "./utxos.js";

type FanoutNode = {
  readonly index: number;
  readonly record: StressWalletRecord;
  readonly parentIndex: number | null;
  readonly children: number[];
  readonly level: number;
  subtreeSize: number;
};

const buildFanoutNodes = ({
  records,
  branchFactor,
}: {
  readonly records: readonly StressWalletRecord[];
  readonly branchFactor: number;
}): readonly FanoutNode[] => {
  const nodes: FanoutNode[] = records.map((record, index) => {
    const parentIndex =
      index < branchFactor
        ? null
        : Math.floor((index - branchFactor) / branchFactor);
    return {
      index,
      record,
      parentIndex,
      children: [],
      level: parentIndex === null ? 1 : 0,
      subtreeSize: 1,
    };
  });
  for (const node of nodes) {
    if (node.parentIndex !== null) {
      nodes[node.parentIndex]!.children.push(node.index);
    }
  }
  for (const node of nodes) {
    if (node.parentIndex !== null) {
      continue;
    }
    const stack = [node.index];
    while (stack.length > 0) {
      const current = nodes[stack.pop()!]!;
      for (const child of current.children) {
        const childNode = nodes[child]!;
        Object.assign(childNode, { level: current.level + 1 });
        stack.push(child);
      }
    }
  }
  for (let index = nodes.length - 1; index >= 0; index -= 1) {
    const node = nodes[index]!;
    if (node.parentIndex !== null) {
      nodes[node.parentIndex]!.subtreeSize += node.subtreeSize;
    }
  }
  return nodes;
};

const subtreeBudget = ({
  subtreeSize,
  lovelacePerWallet,
  feeHeadroomLovelace,
}: {
  readonly subtreeSize: number;
  readonly lovelacePerWallet: bigint;
  readonly feeHeadroomLovelace: bigint;
}): bigint =>
  BigInt(subtreeSize) * lovelacePerWallet +
  BigInt(Math.max(0, subtreeSize - 1)) * feeHeadroomLovelace;

const waitForFanoutAcceptance = async ({
  nodeEndpoint,
  txHash,
  runtime,
  acceptanceTimeoutMs,
  pollInitialIntervalMs,
  pollMaxIntervalMs,
}: {
  readonly nodeEndpoint: string;
  readonly txHash: string;
  readonly runtime: StressWalletFanoutRuntime;
  readonly acceptanceTimeoutMs: number;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
}): Promise<string> => {
  const sleepImpl = runtime.sleep ?? sleep;
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const startedAt = monotonicNow();
  let attempt = 0;
  while (true) {
    const status = (await runtime.fetchTxStatus(nodeEndpoint, txHash)).trim();
    if (acceptedTxStatuses.has(status)) {
      return status;
    }
    if (rejectedTxStatuses.has(status)) {
      throw new Error(
        `Fanout transfer ${txHash} reached rejected status ${status}.`,
      );
    }
    if (monotonicNow() - startedAt >= acceptanceTimeoutMs) {
      throw new Error(
        `Timed out waiting ${acceptanceTimeoutMs.toString()}ms for fanout transfer ${txHash} to reach accepted status; last status ${status}.`,
      );
    }
    await sleepImpl(
      nextFanoutPollDelayMs({
        attempt,
        initialMs: pollInitialIntervalMs,
        maxMs: pollMaxIntervalMs,
      }),
    );
    attempt += 1;
  }
};

const fanoutStressWalletsUnlocked = async (
  options: FanoutStressWalletsOptions,
  runtime: StressWalletFanoutRuntime,
): Promise<StressWalletFanoutResult> => {
  requireSafePositiveInteger(options.count, "count");
  if (options.lovelacePerWallet <= 0n) {
    throw new Error("lovelacePerWallet must be greater than zero.");
  }
  const branchFactor = requireSafePositiveInteger(
    options.branchFactor ?? DEFAULT_FANOUT_BRANCH_FACTOR,
    "branchFactor",
  );
  const maxInFlight = requireSafePositiveInteger(
    options.maxInFlight ?? DEFAULT_FANOUT_MAX_IN_FLIGHT,
    "maxInFlight",
  );
  const feeHeadroomLovelace =
    options.feeHeadroomLovelace ?? DEFAULT_FANOUT_FEE_HEADROOM_LOVELACE;
  if (feeHeadroomLovelace < 0n) {
    throw new Error("feeHeadroomLovelace must be non-negative.");
  }
  const acceptanceTimeoutMs = requireSafeNonNegativeInteger(
    options.acceptanceTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "acceptanceTimeoutMs",
  );
  const pollInitialIntervalMs = requireSafePositiveInteger(
    options.pollInitialIntervalMs ?? 250,
    "pollInitialIntervalMs",
  );
  const pollMaxIntervalMs = requireSafePositiveInteger(
    options.pollMaxIntervalMs ?? DEFAULT_VERIFY_POLL_INTERVAL_MS,
    "pollMaxIntervalMs",
  );
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const startIndex = requireSafePositiveInteger(
    options.startIndex ?? 1,
    "startIndex",
  );
  const envPrefix = normalizeEnvPrefix(options.envPrefix);
  const network = options.network ?? "Preprod";
  const now = runtime.now ?? options.now ?? (() => new Date());
  const nodeEndpoint = defaultMidgardNodeEndpoint({
    ...process.env,
    MIDGARD_NODE_URL: options.nodeEndpoint ?? process.env.MIDGARD_NODE_URL,
  });
  const fetchUtxos = runtime.fetchUtxos ?? fetchNodeUtxosByAddress;
  const sleepImpl = runtime.sleep ?? sleep;
  const resolved = await resolveStressWalletRecords({
    count: options.count,
    outDir,
    startIndex,
    envPrefix,
    network,
    overwrite: false,
    reuseExisting: true,
    createMissing: options.createMissing === true,
    now,
    generateSeedPhrase: options.generateSeedPhrase ?? defaultGenerateSeedPhrase,
  });
  const records = resolved.map(({ record }) => record);
  const exports = await writeStressWalletExports(outDir, records);
  const nodes = buildFanoutNodes({ records, branchFactor });
  const edges: StressWalletFanoutEdgeSummary[] = [];
  let submittedTransferCount = 0;
  let alreadyFundedTransferCount = 0;
  const levels = [...new Set(nodes.map((node) => node.level))].sort(
    (left, right) => left - right,
  );

  for (const level of levels) {
    const parents =
      level === 1
        ? [null]
        : nodes
            .filter(
              (node) => node.level === level - 1 && node.children.length > 0,
            )
            .map((node) => node.index);
    await runBounded(parents, maxInFlight, async (parentIndex) => {
      const childIndexes =
        parentIndex === null
          ? nodes
              .filter(
                (node) => node.parentIndex === null && node.level === level,
              )
              .map((node) => node.index)
          : nodes[parentIndex]!.children;
      for (const childIndex of childIndexes) {
        const child = nodes[childIndex]!;
        const source: StressWalletFanoutSource =
          parentIndex === null
            ? {
                kind: "treasury",
                seedPhrase: options.treasurySeedPhrase,
                walletId: "treasury",
              }
            : { kind: "wallet", wallet: nodes[parentIndex]!.record };
        const lovelace = subtreeBudget({
          subtreeSize: child.subtreeSize,
          lovelacePerWallet: options.lovelacePerWallet,
          feeHeadroomLovelace,
        });
        const existingFunding = firstFundingUtxo(
          await fetchFanoutUtxosWithRetry({
            nodeEndpoint,
            address: child.record.l2Address,
            fetchUtxos,
            sleep: sleepImpl,
          }),
          options.lovelacePerWallet,
        );
        if (existingFunding !== undefined) {
          alreadyFundedTransferCount += 1;
          edges.push({
            level,
            parentWalletId:
              parentIndex === null
                ? "treasury"
                : nodes[parentIndex]!.record.walletId,
            childWalletId: child.record.walletId,
            lovelace: lovelace.toString(10),
            txHash: existingFunding.txHash,
            acceptedStatus: "already_funded",
            submitted: false,
          });
          continue;
        }
        const submitted = await runtime.submitTransfer({
          source,
          destination: child.record,
          lovelace,
          level,
        });
        submittedTransferCount += 1;
        const acceptedStatus = await waitForFanoutAcceptance({
          nodeEndpoint,
          txHash: submitted.txHash,
          runtime,
          acceptanceTimeoutMs,
          pollInitialIntervalMs,
          pollMaxIntervalMs,
        });
        edges.push({
          level,
          parentWalletId:
            parentIndex === null
              ? "treasury"
              : nodes[parentIndex]!.record.walletId,
          childWalletId: child.record.walletId,
          lovelace: lovelace.toString(10),
          txHash: submitted.txHash,
          acceptedStatus,
          submitted: true,
        });
      }
    });
  }

  const preparedAt = now().toISOString();
  const pathsByEnv = new Map(
    resolved.map(({ path, record }) => [record.envName, path] as const),
  );
  const walletEntries: StressWalletFanoutEntry[] = new Array(records.length);
  await runBounded(
    records.map((record, index) => ({ record, index })),
    maxInFlight,
    async ({ record, index }) => {
      const snapshots = verifiedFundingSnapshots(
        await fetchFanoutUtxosWithRetry({
          nodeEndpoint,
          address: record.l2Address,
          fetchUtxos,
          sleep: sleepImpl,
        }),
        options.lovelacePerWallet,
      );
      if (snapshots.length === 0) {
        throw new Error(
          `Fanout did not verify a funding UTxO with at least ${options.lovelacePerWallet.toString(10)} lovelace for ${record.envName}:${record.l2Address}.`,
        );
      }
      const updatedRecord: StressWalletRecord = {
        ...record,
        latestFunding: {
          preparedAt,
          status: "submitted",
          lovelacePerWallet: options.lovelacePerWallet.toString(10),
          nodeEndpoint,
          beforeUtxoCount: 0,
          afterUtxoCount: snapshots.length,
          verifiedFundingUtxoCount: snapshots.length,
          fundingUtxos: snapshots,
        },
      };
      const path = pathsByEnv.get(record.envName);
      if (path === undefined) {
        throw new Error(`Missing path for stress wallet ${record.envName}.`);
      }
      await writeStressWalletRecord(path, updatedRecord);
      walletEntries[index] = {
        wallet: summaryForRecord(updatedRecord, path),
        verifiedFundingUtxoCount: snapshots.length,
      };
    },
  );
  const levelSummaries = levels.map((level) => ({
    level,
    transferCount: edges.filter((edge) => edge.level === level).length,
  }));
  const rootRequiredLovelace =
    BigInt(records.length) * options.lovelacePerWallet +
    BigInt(records.length) * feeHeadroomLovelace;
  const reportPath = join(outDir, "fanout-report.json");
  const result: StressWalletFanoutResult = {
    schemaVersion: STRESS_WALLET_FANOUT_RESULT_SCHEMA_VERSION,
    walletDirectory: outDir,
    requestedCount: options.count,
    generatedWalletCount: resolved.filter((wallet) => wallet.created).length,
    branchFactor,
    maxInFlight,
    lovelacePerWallet: options.lovelacePerWallet.toString(10),
    feeHeadroomLovelace: feeHeadroomLovelace.toString(10),
    rootRequiredLovelace: rootRequiredLovelace.toString(10),
    submittedTransferCount,
    alreadyFundedTransferCount,
    verifiedWalletCount: walletEntries.length,
    nodeEndpoint,
    envFilePath: exports.envFilePath,
    argsFilePath: exports.argsFilePath,
    reportPath,
    levels: levelSummaries,
    wallets: walletEntries,
  };
  parseStressWalletFanoutResult(result);
  const report = {
    ...result,
    schemaVersion: STRESS_WALLET_FANOUT_REPORT_SCHEMA_VERSION,
    edges,
  };
  parseStressWalletFanoutReport(report);
  await writeFile(reportPath, `${formatJson(report)}\n`, "utf8");
  return result;
};

export const fanoutStressWallets = async (
  options: FanoutStressWalletsOptions,
  runtime: StressWalletFanoutRuntime,
): Promise<StressWalletFanoutResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  return withExclusiveStressWalletFundsLock(outDir, () =>
    fanoutStressWalletsUnlocked(options, runtime),
  );
};
