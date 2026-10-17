import { writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  defaultMidgardNodeEndpoint,
  deriveWalletInfo,
  fetchNodeUtxosByAddress,
  formatJson,
  type NodeUtxo,
} from "midgard-node/commands/command-utils";
import { sleep } from "midgard-node/sleep";

import {
  parseStressWalletConsolidationReport,
  parseStressWalletConsolidationResult,
} from "./artifacts.js";
import { assertConsolidationTransferIntent } from "./consolidation.assert-consolidation-transfer-intent.js";
import {
  parseStressWalletConsolidationJournal,
  readConsolidationState,
  waitForConsolidationAcceptance,
} from "./consolidation.parse-stress-wallet-consolidation-journal.js";
import { waitForConsolidationReadiness } from "./consolidation.wait-for-consolidation-readiness.js";
import {
  DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT,
  DEFAULT_CONSOLIDATE_RESERVE_LOVELACE,
  DEFAULT_STRESS_WALLET_DIR,
  DEFAULT_VERIFY_POLL_INTERVAL_MS,
  DEFAULT_VERIFY_TIMEOUT_MS,
  STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION,
  STRESS_WALLET_CONSOLIDATION_REPORT_SCHEMA_VERSION,
  STRESS_WALLET_CONSOLIDATION_RESULT_SCHEMA_VERSION,
} from "./constants.js";
import {
  assertFileGeneration,
  readFileGeneration,
  sha256Bytes,
  writePrivateFileAtomic,
} from "./files.js";
import {
  defaultGenerateSeedPhrase,
  normalizeEnvPrefix,
  requireSafeNonNegativeInteger,
  requireSafePositiveInteger,
} from "./options.js";
import { resolveStressWalletRecords } from "./records.js";
import {
  consolidationPendingTxStatuses,
  rejectedTxStatuses,
  runBounded,
} from "./runtime.js";
import {
  buildStressWalletOperationScope,
  computeSignedNativeTxHash,
  sameStressWalletOperationScope,
} from "./scope.js";
import {
  type ConsolidateStressWalletsOptions,
  type StressWalletConsolidateResult,
  type StressWalletConsolidateRuntime,
} from "./types.js";
import { outRefKey, sumLovelace, utxoAccounting } from "./utxos.js";

export const consolidateStressWalletsUnlocked = async (
  options: ConsolidateStressWalletsOptions,
  runtime: StressWalletConsolidateRuntime,
): Promise<StressWalletConsolidateResult> => {
  requireSafePositiveInteger(options.count, "count");
  const reserveLovelace =
    options.reserveLovelace ?? DEFAULT_CONSOLIDATE_RESERVE_LOVELACE;
  if (reserveLovelace < 0n) {
    throw new Error("reserveLovelace must be non-negative.");
  }
  const maxInFlight = requireSafePositiveInteger(
    options.maxInFlight ?? DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT,
    "maxInFlight",
  );
  const acceptanceTimeoutMs = requireSafeNonNegativeInteger(
    options.acceptanceTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "acceptanceTimeoutMs",
  );
  const readinessTimeoutMs = requireSafeNonNegativeInteger(
    options.readinessTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "readinessTimeoutMs",
  );
  const verificationTimeoutMs = requireSafeNonNegativeInteger(
    options.verificationTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "verificationTimeoutMs",
  );
  const requestTimeoutMs = requireSafePositiveInteger(
    options.requestTimeoutMs ?? 30_000,
    "requestTimeoutMs",
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
  const fetchUtxos =
    runtime.fetchUtxos ??
    ((endpoint: string, address: string) =>
      fetchNodeUtxosByAddress(endpoint, address, {
        timeoutMs: requestTimeoutMs,
      }));
  const sleepImpl = runtime.sleep ?? sleep;
  const resolved = await resolveStressWalletRecords({
    count: options.count,
    outDir,
    startIndex,
    envPrefix,
    network,
    overwrite: false,
    reuseExisting: true,
    createMissing: false,
    now,
    generateSeedPhrase: defaultGenerateSeedPhrase,
  });
  const records = resolved.map(({ record }) => record);
  const scope = buildStressWalletOperationScope({
    records,
    count: options.count,
    startIndex,
    envPrefix,
    network,
  });
  const treasuryAddress = deriveWalletInfo(
    { seedPhrase: options.treasurySeedPhrase, resolvedFrom: "treasury" },
    network,
  ).address;
  if (records.some((record) => record.l2Address === treasuryAddress)) {
    throw new Error(
      "Treasury address must be distinct from every stress wallet address.",
    );
  }

  const statePath = join(outDir, "consolidation-state.json");
  const readinessPath = join(outDir, "consolidation-readiness.jsonl");
  let expectedStateSha256 = await readFileGeneration(statePath);
  const priorState = await readConsolidationState(statePath);
  await assertFileGeneration(statePath, expectedStateSha256);
  if (
    priorState !== undefined &&
    (priorState.treasuryAddress !== treasuryAddress ||
      priorState.nodeEndpoint !== nodeEndpoint ||
      priorState.reserveLovelace !== reserveLovelace.toString(10) ||
      !sameStressWalletOperationScope(priorState.scope, scope))
  ) {
    throw new Error(
      `Existing consolidation state at ${statePath} does not match treasury, endpoint, reserve, or exact wallet scope.`,
    );
  }
  const recordsById = new Map(
    records.map((record) => [record.walletId, record]),
  );
  for (const entry of priorState?.entries ?? []) {
    const record = recordsById.get(entry.walletId);
    if (record === undefined || record.l2Address !== entry.address) {
      throw new Error(
        `Existing consolidation state at ${statePath} contains an entry outside the exact wallet scope.`,
      );
    }
    if (entry.signedTxCbor !== undefined) {
      assertConsolidationTransferIntent({
        entry,
        source: record,
        treasuryAddress,
        reserveLovelace,
        network,
      });
    }
  }
  const fetchSourceUtxos = async (): Promise<
    readonly (readonly NodeUtxo[])[]
  > => {
    const results: (readonly NodeUtxo[])[] = new Array(records.length);
    await runBounded(
      records.map((record, index) => ({ record, index })),
      maxInFlight,
      async ({ record, index }) => {
        results[index] = await fetchUtxos(nodeEndpoint, record.l2Address);
      },
    );
    return results;
  };

  // Complete the full read-only accounting pass before the first mutation.
  const treasuryBeforeUtxos = await fetchUtxos(nodeEndpoint, treasuryAddress);
  const sourceBeforeUtxos = await fetchSourceUtxos();
  const treasuryBefore = sumLovelace(treasuryBeforeUtxos);
  const sourceBefore = sourceBeforeUtxos.reduce(
    (total, utxos) => total + sumLovelace(utxos),
    0n,
  );
  const requestedByWallet = sourceBeforeUtxos.map((utxos) => {
    const total = sumLovelace(utxos);
    return total > reserveLovelace ? total - reserveLovelace : 0n;
  });
  const projectedTreasury = requestedByWallet.reduce(
    (total, amount) => total + amount,
    treasuryBefore,
  );
  if (
    options.requiredTreasuryLovelace !== undefined &&
    projectedTreasury < options.requiredTreasuryLovelace
  ) {
    throw new Error(
      `Projected treasury ${projectedTreasury.toString(10)} lovelace is below required ${options.requiredTreasuryLovelace.toString(10)} lovelace; no transfers submitted.`,
    );
  }

  const stateEntries = new Map(
    (priorState?.entries ?? []).map(
      (entry) => [entry.walletId, entry] as const,
    ),
  );
  let persistQueue = Promise.resolve();
  const persistState = async (): Promise<void> => {
    persistQueue = persistQueue.then(async () => {
      const entries = records.flatMap((record) => {
        const entry = stateEntries.get(record.walletId);
        return entry === undefined ? [] : [entry];
      });
      if (entries.length !== stateEntries.size) {
        throw new Error(
          "Refusing to persist consolidation state because an existing entry would be dropped.",
        );
      }
      const document = parseStressWalletConsolidationJournal({
        schemaVersion: STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION,
        treasuryAddress,
        nodeEndpoint,
        reserveLovelace: reserveLovelace.toString(10),
        scope,
        entries,
      });
      const contents = `${formatJson(document)}\n`;
      await assertFileGeneration(statePath, expectedStateSha256);
      await writePrivateFileAtomic(statePath, contents);
      const writtenSha256 = sha256Bytes(Buffer.from(contents, "utf8"));
      await assertFileGeneration(statePath, writtenSha256);
      expectedStateSha256 = writtenSha256;
    });
    await persistQueue;
  };

  let submittedTransferCount = 0;
  let resumedTransferCount = 0;
  let alreadyConsolidatedCount = 0;
  const consolidationItems = records.map((record, index) => ({
    record,
    index,
  }));
  let batchIndex = 0;
  for (
    let batchStart = 0;
    batchStart < consolidationItems.length;
    batchStart += maxInFlight
  ) {
    const batch = consolidationItems.slice(
      batchStart,
      batchStart + maxInFlight,
    );
    const actionable = batch.filter(
      ({ index }) => requestedByWallet[index]! > 0n,
    );
    if (actionable.length === 0) {
      alreadyConsolidatedCount += batch.length;
      continue;
    }
    await waitForConsolidationReadiness({
      nodeEndpoint,
      runtime,
      readinessPath,
      batchIndex,
      firstWalletId: actionable[0]!.record.walletId,
      timeoutMs: readinessTimeoutMs,
      requestTimeoutMs,
      pollInitialIntervalMs,
      pollMaxIntervalMs,
      now,
    });
    await runBounded(batch, maxInFlight, async ({ record, index }) => {
      const requested = requestedByWallet[index]!;
      if (requested === 0n) {
        alreadyConsolidatedCount += 1;
        return;
      }
      const beforeUtxos = sourceBeforeUtxos[index]!;
      const beforeOutrefs = beforeUtxos.map(outRefKey).sort();
      let entry = stateEntries.get(record.walletId);
      if (entry !== undefined) {
        if (
          entry.address !== record.l2Address ||
          entry.beforeLovelace !== sumLovelace(beforeUtxos).toString(10) ||
          entry.requestedLovelace !== requested.toString(10) ||
          entry.beforeOutrefs.join("|") !== beforeOutrefs.join("|")
        ) {
          throw new Error(
            `Wallet ${record.walletId} changed after its consolidation intent was journaled; refusing an ambiguous resubmission.`,
          );
        }
        if (entry.signedTxCbor !== undefined) {
          const selectedInputs = entry.selectedInputs!;
          const beforeByOutref = new Map(
            beforeUtxos.map((utxo) => [outRefKey(utxo), utxo] as const),
          );
          const selectedUtxos = selectedInputs.map((input) =>
            beforeByOutref.get(input),
          );
          if (selectedUtxos.some((utxo) => utxo === undefined)) {
            throw new Error(
              `Wallet ${record.walletId} no longer contains every journaled selected input; refusing resubmission.`,
            );
          }
          const exactSelectedUtxos = selectedUtxos as readonly NodeUtxo[];
          if (
            exactSelectedUtxos.some((utxo) =>
              Object.entries(utxo.assets).some(
                ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
              ),
            ) ||
            sumLovelace(exactSelectedUtxos).toString(10) !==
              entry.selectedInputLovelace
          ) {
            throw new Error(
              `Wallet ${record.walletId} selected-input value does not match its exact journaled ADA-only snapshot; refusing resubmission.`,
            );
          }
          assertConsolidationTransferIntent({
            entry,
            source: record,
            treasuryAddress,
            reserveLovelace,
            network,
          });
        }
      } else {
        entry = {
          walletId: record.walletId,
          address: record.l2Address,
          beforeLovelace: sumLovelace(beforeUtxos).toString(10),
          beforeOutrefs,
          requestedLovelace: requested.toString(10),
        };
        stateEntries.set(record.walletId, entry);
        await persistState();
      }
      const hadPreparedTransaction = entry.txHash !== undefined;
      if (entry.txHash === undefined) {
        const prepared = await runtime.prepareTransfer({
          source: record,
          treasuryAddress,
          lovelace: requested,
        });
        if (!/^[0-9a-f]{64}$/.test(prepared.txHash)) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} returned an invalid tx hash.`,
          );
        }
        if (
          !/^[0-9a-f]+$/.test(prepared.signedTxCbor) ||
          prepared.signedTxCbor.length % 2 !== 0
        ) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} returned invalid signed CBOR.`,
          );
        }
        if (
          computeSignedNativeTxHash(prepared.signedTxCbor, record.walletId) !==
          prepared.txHash
        ) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} returned a tx hash that does not match its signed CBOR.`,
          );
        }
        if (
          prepared.selectedInputs.length === 0 ||
          new Set(prepared.selectedInputs).size !==
            prepared.selectedInputs.length ||
          prepared.selectedInputs.some(
            (input) => !beforeOutrefs.includes(input),
          )
        ) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} selected inputs outside its journaled source snapshot.`,
          );
        }
        const beforeByOutref = new Map(
          beforeUtxos.map((utxo) => [outRefKey(utxo), utxo] as const),
        );
        const selectedUtxos = prepared.selectedInputs.map(
          (input) => beforeByOutref.get(input)!,
        );
        if (
          selectedUtxos.some((utxo) =>
            Object.entries(utxo.assets).some(
              ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
            ),
          )
        ) {
          throw new Error(
            `Prepared consolidation transfer for ${record.walletId} selected a non-ADA source input.`,
          );
        }
        entry = {
          ...entry,
          txHash: prepared.txHash,
          signedTxCbor: prepared.signedTxCbor,
          selectedInputs: [...prepared.selectedInputs].sort(),
          selectedInputLovelace: sumLovelace(selectedUtxos).toString(10),
        };
        assertConsolidationTransferIntent({
          entry,
          source: record,
          treasuryAddress,
          reserveLovelace,
          network,
        });
        stateEntries.set(record.walletId, entry);
        // This durable checkpoint must complete before the first submit attempt.
        await persistState();
      } else {
        resumedTransferCount += 1;
      }
      const txHash = entry.txHash;
      if (txHash === undefined) {
        throw new Error(
          `Consolidation transfer for ${record.walletId} was not durably prepared.`,
        );
      }
      const observedStatus = (await runtime.fetchTxStatus(nodeEndpoint, txHash))
        .trim()
        .toLowerCase();
      if (observedStatus === "not_found") {
        if (entry.signedTxCbor === undefined) {
          throw new Error(
            `V1 consolidation checkpoint ${txHash} lacks exact signed CBOR; resubmission is forbidden.`,
          );
        }
        const submitted = await runtime.submitPreparedTransfer({
          nodeEndpoint,
          txHash,
          signedTxCbor: entry.signedTxCbor,
        });
        if (submitted.txHash !== txHash) {
          throw new Error(
            `Exact-CBOR submission returned tx hash ${submitted.txHash}, expected ${txHash}.`,
          );
        }
        submittedTransferCount += 1;
      } else if (rejectedTxStatuses.has(observedStatus)) {
        throw new Error(
          `Consolidation transfer ${txHash} reached rejected status ${observedStatus}.`,
        );
      } else if (
        observedStatus !== "committed" &&
        !consolidationPendingTxStatuses.has(observedStatus)
      ) {
        throw new Error(
          `Consolidation transfer ${txHash} returned unknown status ${observedStatus || "<empty>"}; refusing to resubmit or infer commitment.`,
        );
      }
      const acceptedStatus = await waitForConsolidationAcceptance({
        nodeEndpoint,
        txHash,
        runtime,
        acceptanceTimeoutMs,
        pollInitialIntervalMs,
        pollMaxIntervalMs,
      });
      stateEntries.set(record.walletId, {
        ...entry,
        txHash,
        acceptedStatus,
      });
      await persistState();
      if (!hadPreparedTransaction && entry.signedTxCbor === undefined) {
        throw new Error(
          `New consolidation entry for ${record.walletId} lost its signed transaction checkpoint.`,
        );
      }
    });
    batchIndex += 1;
  }

  // Reconcile every scoped journal entry to an explicit terminal state before
  // reporting completion, including wallets that were already at/below reserve.
  for (const { record, index } of consolidationItems) {
    const requested = requestedByWallet[index]!;
    const existing = stateEntries.get(record.walletId);
    if (requested === 0n) {
      if (existing === undefined) {
        const beforeUtxos = sourceBeforeUtxos[index]!;
        stateEntries.set(record.walletId, {
          walletId: record.walletId,
          address: record.l2Address,
          beforeLovelace: sumLovelace(beforeUtxos).toString(10),
          beforeOutrefs: beforeUtxos.map(outRefKey).sort(),
          requestedLovelace: "0",
          acceptedStatus: "already_empty",
        });
      } else if (existing.txHash !== undefined) {
        const status = (
          await runtime.fetchTxStatus(nodeEndpoint, existing.txHash)
        )
          .trim()
          .toLowerCase();
        if (status !== "committed") {
          throw new Error(
            `Terminal reconciliation found ${record.walletId} transaction ${existing.txHash} in status ${status || "<empty>"}.`,
          );
        }
        stateEntries.set(record.walletId, {
          ...existing,
          acceptedStatus: "committed",
        });
      } else if (
        existing.requestedLovelace !== "0" ||
        existing.acceptedStatus !== "already_empty"
      ) {
        throw new Error(
          `Terminal reconciliation found ambiguous entry for ${record.walletId}.`,
        );
      }
    } else if (existing?.acceptedStatus !== "committed") {
      throw new Error(
        `Terminal reconciliation found non-terminal entry for ${record.walletId}.`,
      );
    }
  }
  await persistState();

  const expectedTreasuryDelta = requestedByWallet.reduce(
    (total, amount) => total + amount,
    0n,
  );
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const verificationStartedAt = monotonicNow();
  let treasuryAfterUtxos: readonly NodeUtxo[] = [];
  let sourceAfterUtxos: readonly (readonly NodeUtxo[])[] = [];
  while (true) {
    treasuryAfterUtxos = await fetchUtxos(nodeEndpoint, treasuryAddress);
    sourceAfterUtxos = await fetchSourceUtxos();
    const treasuryDelta = sumLovelace(treasuryAfterUtxos) - treasuryBefore;
    const sourcesConsolidated = sourceAfterUtxos.every(
      (utxos) => sumLovelace(utxos) <= reserveLovelace,
    );
    if (treasuryDelta === expectedTreasuryDelta && sourcesConsolidated) break;
    if (monotonicNow() - verificationStartedAt >= verificationTimeoutMs) {
      throw new Error(
        `Consolidation accounting did not converge: treasury delta ${treasuryDelta.toString(10)} expected ${expectedTreasuryDelta.toString(10)}, sourcesConsolidated=${String(sourcesConsolidated)}.`,
      );
    }
    await sleepImpl(pollInitialIntervalMs);
  }

  const treasuryAfter = sumLovelace(treasuryAfterUtxos);
  const sourceAfter = sourceAfterUtxos.reduce(
    (total, utxos) => total + sumLovelace(utxos),
    0n,
  );
  const treasuryDelta = treasuryAfter - treasuryBefore;
  const inferredFees = sourceBefore - sourceAfter - treasuryDelta;
  if (inferredFees < 0n) {
    throw new Error(
      `Consolidation conservation failed with negative inferred fees ${inferredFees.toString(10)} lovelace.`,
    );
  }
  const reportTimestamp = now().toISOString();
  const reportPath = join(
    outDir,
    `consolidation-report-${reportTimestamp.replaceAll(":", "-")}.json`,
  );
  const result: StressWalletConsolidateResult = {
    schemaVersion: STRESS_WALLET_CONSOLIDATION_RESULT_SCHEMA_VERSION,
    walletDirectory: outDir,
    requestedCount: options.count,
    reserveLovelace: reserveLovelace.toString(10),
    maxInFlight,
    nodeEndpoint,
    treasuryAddress,
    treasuryBeforeLovelace: treasuryBefore.toString(10),
    treasuryAfterLovelace: treasuryAfter.toString(10),
    treasuryDeltaLovelace: treasuryDelta.toString(10),
    sourceBeforeLovelace: sourceBefore.toString(10),
    sourceAfterLovelace: sourceAfter.toString(10),
    inferredFeesLovelace: inferredFees.toString(10),
    projectedTreasuryLovelace: projectedTreasury.toString(10),
    submittedTransferCount,
    resumedTransferCount,
    alreadyConsolidatedCount,
    reportPath,
  };
  parseStressWalletConsolidationResult(result);
  const report = {
    ...result,
    schemaVersion: STRESS_WALLET_CONSOLIDATION_REPORT_SCHEMA_VERSION,
    statePath,
    treasury: {
      before: utxoAccounting(treasuryBeforeUtxos),
      after: utxoAccounting(treasuryAfterUtxos),
    },
    wallets: records.map((record, index) => ({
      walletId: record.walletId,
      address: record.l2Address,
      before: utxoAccounting(sourceBeforeUtxos[index]!),
      after: utxoAccounting(sourceAfterUtxos[index]!),
      ...(stateEntries.has(record.walletId)
        ? { transfer: stateEntries.get(record.walletId) }
        : {}),
    })),
  };
  parseStressWalletConsolidationReport(report);
  await writeFile(reportPath, `${formatJson(report)}\n`, {
    encoding: "utf8",
    flag: "wx",
    mode: 0o600,
  });
  return result;
};
