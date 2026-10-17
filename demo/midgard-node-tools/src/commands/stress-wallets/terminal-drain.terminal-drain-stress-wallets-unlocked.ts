import { chmod } from "node:fs/promises";
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
  parseStressWalletTerminalDrainReport,
  parseStressWalletTerminalDrainResult,
} from "./artifacts.js";
import {
  DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT,
  DEFAULT_STRESS_WALLET_DIR,
  DEFAULT_VERIFY_POLL_INTERVAL_MS,
  DEFAULT_VERIFY_TIMEOUT_MS,
  STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION,
  STRESS_WALLET_TERMINAL_DRAIN_REPORT_SCHEMA_VERSION,
  STRESS_WALLET_TERMINAL_DRAIN_RESULT_SCHEMA_VERSION,
} from "./constants.js";
import {
  assertFileGeneration,
  readFileGeneration,
  sha256Bytes,
  writePrivateFileAtomic,
  writePrivateFileAtomicNoReplace,
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
  nextFanoutPollDelayMs,
  rejectedTxStatuses,
  runBounded,
} from "./runtime.js";
import {
  buildStressWalletOperationScope,
  sameStressWalletOperationScope,
} from "./scope.js";
import { assertTerminalDrainIntent } from "./terminal-drain.assert-terminal-drain-intent.js";
import {
  parseStressWalletTerminalDrainJournal,
  readTerminalDrainState,
} from "./terminal-drain.parse-stress-wallet-terminal-drain-journal.js";
import {
  type TerminalDrainEntry,
  type TerminalDrainState,
  terminalScopeHash,
  terminalSnapshotHash,
} from "./terminal-drain.terminal-snapshot-hash.js";
import {
  type StressWalletTerminalDrainResult,
  type StressWalletTerminalDrainRuntime,
  type TerminalDrainStressWalletsOptions,
} from "./types.js";
import { outRefKey, sumLovelace, utxoAccounting } from "./utxos.js";

export const terminalDrainStressWalletsUnlocked = async (
  options: TerminalDrainStressWalletsOptions,
  runtime: StressWalletTerminalDrainRuntime,
): Promise<StressWalletTerminalDrainResult> => {
  requireSafePositiveInteger(options.count, "count");
  if (options.minFeeA < 0n || options.minFeeB < 0n)
    throw new Error("Terminal drain fee parameters must be non-negative.");
  const feeCap = options.feeCapLovelace ?? 100_000n;
  if (feeCap < 0n) throw new Error("feeCapLovelace must be non-negative.");
  const maxFeeIterations = requireSafePositiveInteger(
    options.maxFeeIterations ?? 32,
    "maxFeeIterations",
  );
  const maxInFlight = requireSafePositiveInteger(
    options.maxInFlight ?? DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT,
    "maxInFlight",
  );
  const acceptanceTimeoutMs = requireSafeNonNegativeInteger(
    options.acceptanceTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "acceptanceTimeoutMs",
  );
  const verificationTimeoutMs = requireSafeNonNegativeInteger(
    options.verificationTimeoutMs ?? DEFAULT_VERIFY_TIMEOUT_MS,
    "verificationTimeoutMs",
  );
  const requestTimeoutMs = requireSafePositiveInteger(
    options.requestTimeoutMs ?? 30_000,
    "requestTimeoutMs",
  );
  const pollInitial = requireSafePositiveInteger(
    options.pollInitialIntervalMs ?? 250,
    "pollInitialIntervalMs",
  );
  const pollMax = requireSafePositiveInteger(
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
  const endpoint = defaultMidgardNodeEndpoint({
    ...process.env,
    MIDGARD_NODE_URL: options.nodeEndpoint ?? process.env.MIDGARD_NODE_URL,
  });
  const now = runtime.now ?? options.now ?? (() => new Date());
  const sleepImpl = runtime.sleep ?? sleep;
  const fetchUtxos =
    runtime.fetchUtxos ??
    ((e: string, a: string) =>
      fetchNodeUtxosByAddress(e, a, { timeoutMs: requestTimeoutMs }));
  const records = (
    await resolveStressWalletRecords({
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
    })
  ).map((x) => x.record);
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
  if (records.some((x) => x.l2Address === treasuryAddress))
    throw new Error("Terminal drain treasury must differ from all sources.");
  const statePath = join(outDir, "terminal-drain-state.json");
  let generation = await readFileGeneration(statePath);
  let state = await readTerminalDrainState(statePath);
  await assertFileGeneration(statePath, generation);
  const persist = async (next: TerminalDrainState): Promise<void> => {
    const canonical = parseStressWalletTerminalDrainJournal(
      JSON.parse(formatJson(next)) as unknown,
    );
    const text = formatJson(canonical) + "\n";
    await assertFileGeneration(statePath, generation);
    await writePrivateFileAtomic(statePath, text);
    await chmod(statePath, 0o600);
    generation = sha256Bytes(Buffer.from(text, "utf8"));
    await assertFileGeneration(statePath, generation);
  };
  if (state === undefined) {
    const treasuryBefore = sumLovelace(
      await fetchUtxos(endpoint, treasuryAddress),
    );
    const snapshots: (readonly NodeUtxo[])[] = new Array(records.length);
    await runBounded(
      records.map((record, index) => ({ record, index })),
      maxInFlight,
      async ({ record, index }) => {
        snapshots[index] = await fetchUtxos(endpoint, record.l2Address);
      },
    );
    const entries: TerminalDrainEntry[] = new Array(records.length);
    await runBounded(
      records.map((record, index) => ({ record, index })),
      maxInFlight,
      async ({ record, index }) => {
        const snapshot = snapshots[index]!;
        const beforeOutrefs = snapshot.map(outRefKey).sort();
        const beforeLovelace = sumLovelace(snapshot);
        if (
          snapshot.some((u) =>
            Object.entries(u.assets).some(
              ([unit, q]) => unit !== "lovelace" && q !== 0n,
            ),
          )
        )
          throw new Error(
            "Terminal drain source contains non-ADA assets: " + record.walletId,
          );
        if (snapshot.length === 0) {
          entries[index] = {
            walletId: record.walletId,
            address: record.l2Address,
            beforeOutrefs,
            beforeLovelace: "0",
            beforeValueSha256: terminalSnapshotHash(snapshot),
            status: "already_empty",
          };
          return;
        }
        const p = await runtime.prepareTransfer({
          source: record,
          treasuryAddress,
        });
        entries[index] = {
          walletId: record.walletId,
          address: record.l2Address,
          beforeOutrefs,
          beforeLovelace: beforeLovelace.toString(10),
          beforeValueSha256: terminalSnapshotHash(snapshot),
          status: "prepared",
          txHash: p.txHash,
          signedTxCbor: p.signedTxCbor,
          selectedInputs: [...p.selectedInputs].sort(),
          requestedLovelace: p.requestedLovelace.toString(10),
          feeLovelace: p.feeLovelace.toString(10),
          signedTxBytes: p.signedTxBytes,
        };
      },
    );
    state = {
      schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION,
      scope,
      scopeSha256: terminalScopeHash(scope),
      nodeEndpoint: endpoint,
      network,
      treasuryAddress,
      treasuryBeforeLovelace: treasuryBefore.toString(10),
      minFeeA: options.minFeeA.toString(10),
      minFeeB: options.minFeeB.toString(10),
      feeCapLovelace: feeCap.toString(10),
      maxFeeIterations,
      entries,
    };
    for (let i = 0; i < entries.length; i += 1)
      assertTerminalDrainIntent(entries[i]!, records[i]!, state);
    await persist(state);
  } else {
    if (
      !sameStressWalletOperationScope(state.scope, scope) ||
      state.scopeSha256 !== terminalScopeHash(scope) ||
      state.nodeEndpoint !== endpoint ||
      state.network !== network ||
      state.treasuryAddress !== treasuryAddress ||
      state.minFeeA !== options.minFeeA.toString(10) ||
      state.minFeeB !== options.minFeeB.toString(10) ||
      state.feeCapLovelace !== feeCap.toString(10) ||
      state.maxFeeIterations !== maxFeeIterations ||
      state.entries.length !== records.length
    )
      throw new Error("Terminal drain journal scope/config mismatch.");
    const ids = new Set<string>();
    const hashes = new Set<string>();
    for (let i = 0; i < state.entries.length; i += 1) {
      const entry = state.entries[i]!;
      const record = records[i]!;
      if (
        entry.walletId !== record.walletId ||
        entry.address !== record.l2Address ||
        ids.has(entry.walletId) ||
        (entry.txHash !== undefined && hashes.has(entry.txHash))
      )
        throw new Error(
          "Terminal drain journal ordering/identity uniqueness mismatch.",
        );
      ids.add(entry.walletId);
      if (entry.txHash) hashes.add(entry.txHash);
      assertTerminalDrainIntent(entry, record, state);
    }
  }
  const prepared = state.entries.filter(
    (x) => x.status !== "already_empty",
  ).length;
  const alreadyEmpty = state.entries.length - prepared;
  const gross = state.entries.reduce(
    (n, x) => n + BigInt(x.beforeLovelace),
    0n,
  );
  const fees = state.entries.reduce(
    (n, x) => n + BigInt(x.feeLovelace ?? "0"),
    0n,
  );
  if (options.prepareOnly) {
    const result: StressWalletTerminalDrainResult = {
      schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_RESULT_SCHEMA_VERSION,
      phase: "prepared",
      walletDirectory: outDir,
      requestedCount: options.count,
      nodeEndpoint: endpoint,
      treasuryAddress,
      treasuryBeforeLovelace: state.treasuryBeforeLovelace,
      grossSourceLovelace: gross.toString(10),
      totalFeesLovelace: fees.toString(10),
      preparedTransferCount: prepared,
      alreadyEmptyCount: alreadyEmpty,
      submittedTransferCount: 0,
      resumedTransferCount: 0,
      statePath,
    };
    parseStressWalletTerminalDrainResult(result);
    return result;
  }
  let submitted = 0;
  let resumed = 0;
  const entries = [...state.entries];
  for (let i = 0; i < entries.length; i += 1) {
    let entry = entries[i]!;
    if (entry.status === "already_empty") continue;
    let status = (await runtime.fetchTxStatus(endpoint, entry.txHash!))
      .trim()
      .toLowerCase();
    if (entry.status === "committed") {
      if (status !== "committed")
        throw new Error("Committed terminal drain checkpoint lost finality.");
      resumed += 1;
      continue;
    }
    if (status === "not_found") {
      const current = await fetchUtxos(endpoint, entry.address);
      if (
        current.map(outRefKey).sort().join("|") !==
          entry.beforeOutrefs.join("|") ||
        terminalSnapshotHash(current) !== entry.beforeValueSha256
      )
        throw new Error(
          "Terminal drain not_found transaction has missing/changed inputs; exact-CBOR resubmission refused.",
        );
      const result = await runtime.submitPreparedTransfer({
        nodeEndpoint: endpoint,
        txHash: entry.txHash!,
        signedTxCbor: entry.signedTxCbor!,
      });
      if (result.txHash !== entry.txHash)
        throw new Error("Terminal drain submit returned mismatched hash.");
      submitted += 1;
    } else if (
      status !== "committed" &&
      !consolidationPendingTxStatuses.has(status)
    )
      throw new Error("Terminal drain status is not committable: " + status);
    else resumed += 1;
    const started = (runtime.monotonicNow ?? (() => Date.now()))();
    let attempt = 0;
    while (
      (status = (await runtime.fetchTxStatus(endpoint, entry.txHash!))
        .trim()
        .toLowerCase()) !== "committed"
    ) {
      if (
        rejectedTxStatuses.has(status) ||
        !consolidationPendingTxStatuses.has(status)
      )
        throw new Error(
          "Terminal drain reached non-committable status " + status,
        );
      if (
        (runtime.monotonicNow ?? (() => Date.now()))() - started >=
        acceptanceTimeoutMs
      )
        throw new Error("Timed out waiting for terminal drain commitment.");
      await sleepImpl(
        nextFanoutPollDelayMs({
          attempt,
          initialMs: pollInitial,
          maxMs: pollMax,
        }),
      );
      attempt += 1;
    }
    entry = { ...entry, status: "committed" };
    entries[i] = entry;
    state = { ...state, entries };
    await persist(state);
  }
  const verifyStart = (runtime.monotonicNow ?? (() => Date.now()))();
  let treasuryAfter = 0n;
  let sources: readonly (readonly NodeUtxo[])[] = [];
  while (true) {
    treasuryAfter = sumLovelace(await fetchUtxos(endpoint, treasuryAddress));
    const next: (readonly NodeUtxo[])[] = new Array(records.length);
    await runBounded(
      records.map((record, index) => ({ record, index })),
      maxInFlight,
      async ({ record, index }) => {
        next[index] = await fetchUtxos(endpoint, record.l2Address);
      },
    );
    sources = next;
    const residual = sources.reduce((n, x) => n + sumLovelace(x), 0n);
    const delta = treasuryAfter - BigInt(state.treasuryBeforeLovelace);
    if (
      residual === 0n &&
      sources.every((utxos) => utxos.length === 0) &&
      delta + fees === gross
    )
      break;
    if (
      (runtime.monotonicNow ?? (() => Date.now()))() - verifyStart >=
      verificationTimeoutMs
    )
      throw new Error(
        "Terminal drain exact-zero conservation did not converge.",
      );
    await sleepImpl(pollInitial);
  }
  for (const entry of state.entries)
    if (
      entry.status !== "already_empty" &&
      (await runtime.fetchTxStatus(endpoint, entry.txHash!))
        .trim()
        .toLowerCase() !== "committed"
    )
      throw new Error("Terminal drain final status reconciliation failed.");
  const delta = treasuryAfter - BigInt(state.treasuryBeforeLovelace);
  const reportPath = join(
    outDir,
    "terminal-drain-report-" +
      now().toISOString().replaceAll(":", "-") +
      ".json",
  );
  const report = {
    schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_REPORT_SCHEMA_VERSION,
    scope,
    statePath,
    endpoint,
    network,
    treasuryAddress,
    conservation: {
      grossSourceLovelace: gross.toString(10),
      treasuryDeltaLovelace: delta.toString(10),
      totalFeesLovelace: fees.toString(10),
      residualSourceLovelace: "0",
    },
    wallets: records.map((record, i) => ({
      walletId: record.walletId,
      address: record.l2Address,
      before: state!.entries[i],
      after: utxoAccounting(sources[i]!),
    })),
  };
  parseStressWalletTerminalDrainReport(report);
  await writePrivateFileAtomicNoReplace(reportPath, formatJson(report) + "\n");
  const result: StressWalletTerminalDrainResult = {
    schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_RESULT_SCHEMA_VERSION,
    phase: "committed",
    walletDirectory: outDir,
    requestedCount: options.count,
    nodeEndpoint: endpoint,
    treasuryAddress,
    treasuryBeforeLovelace: state.treasuryBeforeLovelace,
    treasuryAfterLovelace: treasuryAfter.toString(10),
    treasuryDeltaLovelace: delta.toString(10),
    grossSourceLovelace: gross.toString(10),
    totalFeesLovelace: fees.toString(10),
    residualSourceLovelace: "0",
    preparedTransferCount: prepared,
    alreadyEmptyCount: alreadyEmpty,
    submittedTransferCount: submitted,
    resumedTransferCount: resumed,
    statePath,
    reportPath,
  };
  parseStressWalletTerminalDrainResult(result);
  return result;
};
