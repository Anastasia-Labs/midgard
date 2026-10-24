import {
  defaultMidgardNodeEndpoint,
  fetchNodeUtxosByAddress,
  type NodeUtxo,
} from "midgard-node/commands/command-utils";
import { sleep } from "midgard-node/sleep";

import { parseStressWalletPrepareResult } from "./artifacts.js";
import {
  DEFAULT_PROJECTION_WAIT_MS,
  DEFAULT_STRESS_WALLET_DIR,
  DEFAULT_VERIFY_POLL_INTERVAL_MS,
  DEFAULT_VERIFY_TIMEOUT_MS,
  STRESS_WALLET_PREPARE_RESULT_SCHEMA_VERSION,
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
  type PrepareStressWalletsOptions,
  type PrepareStressWalletsResult,
  type PrepareStressWalletsRuntime,
  type StressWalletDepositResult,
  type StressWalletFundingSnapshot,
  type StressWalletPrepareEntry,
  type StressWalletRecord,
} from "./types.js";
import {
  fundingUtxos,
  newFundingUtxos,
  outRefKey,
  queryWalletUtxos,
} from "./utxos.js";

const prepareStressWalletsUnlocked = async (
  options: PrepareStressWalletsOptions,
  runtime: PrepareStressWalletsRuntime,
): Promise<PrepareStressWalletsResult> => {
  requireSafePositiveInteger(options.count, "count");
  if (options.lovelacePerWallet <= 0n) {
    throw new Error("lovelacePerWallet must be greater than zero.");
  }
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
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const projectionWaitMs = requireSafeNonNegativeInteger(
    options.projectionWaitMs ?? DEFAULT_PROJECTION_WAIT_MS,
    "projectionWaitMs",
  );
  const verifyTimeoutMs = requireSafeNonNegativeInteger(
    options.verifyTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "verifyTimeoutMs",
  );
  const pollIntervalMs = requireSafeNonNegativeInteger(
    options.pollIntervalMs ?? DEFAULT_VERIFY_POLL_INTERVAL_MS,
    "pollIntervalMs",
  );
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
  const pathsByEnv = new Map(
    resolved.map(({ path, record }) => [record.envName, path] as const),
  );
  const exports = await writeStressWalletExports(outDir, records);
  const beforeUtxos = await queryWalletUtxos({
    records,
    nodeEndpoint,
    fetchUtxos,
  });

  const pendingEntries: Array<{
    readonly record: StressWalletRecord;
    readonly status: "submitted" | "already_funded";
    readonly deposit?: StressWalletDepositResult;
  }> = [];

  for (const record of records) {
    const before = beforeUtxos.get(record.envName) ?? [];
    if (
      options.forceFundExisting !== true &&
      fundingUtxos(before, options.lovelacePerWallet).length > 0
    ) {
      pendingEntries.push({ record, status: "already_funded" });
      continue;
    }
    const deposit = await runtime.submitDeposit({
      wallet: record,
      lovelace: options.lovelacePerWallet,
    });
    pendingEntries.push({ record, status: "submitted", deposit });
  }

  if (pendingEntries.some((entry) => entry.status === "submitted")) {
    if (projectionWaitMs > 0) {
      await sleepImpl(projectionWaitMs);
    }
  }

  const startedVerification = monotonicNow();
  let afterUtxos = new Map<string, readonly NodeUtxo[]>();
  while (true) {
    afterUtxos = await queryWalletUtxos({
      records,
      nodeEndpoint,
      fetchUtxos,
    });
    const allVerified = pendingEntries.every((entry) => {
      const before = beforeUtxos.get(entry.record.envName) ?? [];
      const after = afterUtxos.get(entry.record.envName) ?? [];
      return entry.status === "already_funded"
        ? fundingUtxos(after, options.lovelacePerWallet).length > 0
        : newFundingUtxos({
            before,
            after,
            lovelacePerWallet: options.lovelacePerWallet,
          }).length > 0;
    });
    if (allVerified) {
      break;
    }
    if (monotonicNow() - startedVerification >= verifyTimeoutMs) {
      const missing = pendingEntries
        .filter((entry) => {
          const before = beforeUtxos.get(entry.record.envName) ?? [];
          const after = afterUtxos.get(entry.record.envName) ?? [];
          return entry.status === "already_funded"
            ? fundingUtxos(after, options.lovelacePerWallet).length === 0
            : newFundingUtxos({
                before,
                after,
                lovelacePerWallet: options.lovelacePerWallet,
              }).length === 0;
        })
        .map((entry) => `${entry.record.envName}:${entry.record.l2Address}`);
      throw new Error(
        `Timed out verifying stress wallet funding for ${missing.join(", ")}.`,
      );
    }
    await sleepImpl(pollIntervalMs);
  }

  const preparedAt = now().toISOString();
  const entries = await Promise.all(
    pendingEntries.map(async (entry): Promise<StressWalletPrepareEntry> => {
      const before = beforeUtxos.get(entry.record.envName) ?? [];
      const after = afterUtxos.get(entry.record.envName) ?? [];
      const verifiedFunding =
        entry.status === "already_funded"
          ? fundingUtxos(after, options.lovelacePerWallet)
          : newFundingUtxos({
              before,
              after,
              lovelacePerWallet: options.lovelacePerWallet,
            });
      const latestFunding: StressWalletFundingSnapshot = {
        preparedAt,
        status: entry.status,
        lovelacePerWallet: options.lovelacePerWallet.toString(10),
        nodeEndpoint,
        beforeUtxoCount: before.length,
        afterUtxoCount: after.length,
        verifiedFundingUtxoCount: verifiedFunding.length,
        fundingUtxos: verifiedFunding.map((utxo) => ({
          outref: outRefKey(utxo),
          outputCbor: utxo.outputCbor.toString("hex"),
          lovelace: (utxo.assets.lovelace ?? 0n).toString(10),
        })),
        ...(entry.deposit === undefined
          ? {}
          : { depositTxHash: entry.deposit.txHash }),
        ...(entry.deposit?.depositEventId === undefined
          ? {}
          : { depositEventId: entry.deposit.depositEventId }),
      };
      const updatedRecord: StressWalletRecord = {
        ...entry.record,
        latestFunding,
      };
      const path = pathsByEnv.get(entry.record.envName);
      if (path === undefined) {
        throw new Error(
          `Missing path for stress wallet ${entry.record.envName}.`,
        );
      }
      await writeStressWalletRecord(path, updatedRecord);
      return {
        wallet: summaryForRecord(updatedRecord, path),
        status: entry.status,
        beforeUtxoCount: before.length,
        afterUtxoCount: after.length,
        verifiedFundingUtxoCount: verifiedFunding.length,
        ...(entry.deposit === undefined
          ? {}
          : { depositTxHash: entry.deposit.txHash }),
        ...(entry.deposit?.depositEventId === undefined
          ? {}
          : { depositEventId: entry.deposit.depositEventId }),
      };
    }),
  );

  const result: PrepareStressWalletsResult = {
    schemaVersion: STRESS_WALLET_PREPARE_RESULT_SCHEMA_VERSION,
    walletDirectory: outDir,
    requestedCount: options.count,
    generatedWalletCount: resolved.filter((wallet) => wallet.created).length,
    submittedDepositCount: entries.filter(
      (entry) => entry.status === "submitted",
    ).length,
    alreadyFundedCount: entries.filter(
      (entry) => entry.status === "already_funded",
    ).length,
    verifiedWalletCount: entries.length,
    lovelacePerWallet: options.lovelacePerWallet.toString(10),
    nodeEndpoint,
    envFilePath: exports.envFilePath,
    argsFilePath: exports.argsFilePath,
    wallets: entries,
  };
  parseStressWalletPrepareResult(result);
  return result;
};

export const prepareStressWallets = async (
  options: PrepareStressWalletsOptions,
  runtime: PrepareStressWalletsRuntime,
): Promise<PrepareStressWalletsResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  return withExclusiveStressWalletFundsLock(outDir, () =>
    prepareStressWalletsUnlocked(options, runtime),
  );
};
