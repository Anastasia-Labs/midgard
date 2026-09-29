import { readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import {
  EMPTY_NULL_ROOT,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core/codec/native";
import { decodeMidgardLedgerTxFromCanonicalCbor } from "@al-ft/midgard-validation/ledger-tx/codec";
import { CML, type Network } from "@lucid-evolution/lucid";
import {
  defaultMidgardNodeEndpoint,
  deriveWalletInfo,
  fetchNodeUtxosByAddress,
  formatJson,
  type NodeUtxo,
} from "midgard-node/commands/command-utils";
import { sleep } from "midgard-node/sleep";

import {
  asObject,
  assertExactKeys,
  requiredString,
} from "./artifact-fields.js";
import {
  parseStressWalletConsolidationReadinessEvidence,
  parseStressWalletConsolidationReport,
  parseStressWalletConsolidationResult,
} from "./artifacts.js";
import {
  DEFAULT_CONSOLIDATE_MAX_IN_FLIGHT,
  DEFAULT_CONSOLIDATE_RESERVE_LOVELACE,
  DEFAULT_STRESS_WALLET_DIR,
  DEFAULT_VERIFY_POLL_INTERVAL_MS,
  DEFAULT_VERIFY_TIMEOUT_MS,
  STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION,
  STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION,
  STRESS_WALLET_CONSOLIDATION_REPORT_SCHEMA_VERSION,
  STRESS_WALLET_CONSOLIDATION_RESULT_SCHEMA_VERSION,
} from "./constants.js";
import {
  assertFileGeneration,
  fileExists,
  readFileGeneration,
  sha256Bytes,
  withExclusiveConsolidationStateLock,
  withExclusiveStressWalletFundsLock,
  writePrivateFileAtomic,
} from "./files.js";
import {
  defaultGenerateSeedPhrase,
  normalizeEnvPrefix,
  requireSafeNonNegativeInteger,
  requireSafePositiveInteger,
} from "./options.js";
import {
  type ConsolidationReadinessSnapshot,
  defaultFetchConsolidationReadiness,
  isFullConsolidationReadiness,
  parseConsolidationReadiness,
} from "./readiness.js";
import { resolveStressWalletRecords } from "./records.js";
import {
  consolidationPendingTxStatuses,
  nextFanoutPollDelayMs,
  rejectedTxStatuses,
  runBounded,
} from "./runtime.js";
import {
  buildStressWalletOperationScope,
  computeSignedNativeTxHash,
  parseStressWalletOperationScope,
  sameStressWalletOperationScope,
  type StressWalletOperationScope,
} from "./scope.js";
import {
  type ConsolidateStressWalletsOptions,
  type StressWalletConsolidateResult,
  type StressWalletConsolidateRuntime,
  type StressWalletRecord,
} from "./types.js";
import { outRefKey, sumLovelace, utxoAccounting } from "./utxos.js";

export type ConsolidationStateEntry = {
  readonly walletId: string;
  readonly address: string;
  readonly beforeLovelace: string;
  readonly beforeOutrefs: readonly string[];
  readonly requestedLovelace: string;
  readonly txHash?: string;
  readonly signedTxCbor?: string;
  readonly selectedInputs?: readonly string[];
  readonly selectedInputLovelace?: string;
  readonly acceptedStatus?: string;
};

export type ConsolidationState = {
  readonly schemaVersion: typeof STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION;
  readonly treasuryAddress: string;
  readonly nodeEndpoint: string;
  readonly reserveLovelace: string;
  readonly scope: StressWalletOperationScope;
  readonly entries: readonly ConsolidationStateEntry[];
};

const assertConsolidationTransferIntent = ({
  entry,
  source,
  treasuryAddress,
  reserveLovelace,
  network,
}: {
  readonly entry: ConsolidationStateEntry;
  readonly source: StressWalletRecord;
  readonly treasuryAddress: string;
  readonly reserveLovelace: bigint;
  readonly network: Network;
}): void => {
  if (
    entry.txHash === undefined ||
    entry.signedTxCbor === undefined ||
    entry.selectedInputs === undefined ||
    entry.selectedInputLovelace === undefined
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is incomplete.`,
    );
  }
  let nativeTx: ReturnType<typeof decodeMidgardNativeTxFullFromCanonicalCbor>;
  let tx: ReturnType<typeof decodeMidgardLedgerTxFromCanonicalCbor>;
  try {
    const signedTxCbor = Buffer.from(entry.signedTxCbor, "hex");
    nativeTx = decodeMidgardNativeTxFullFromCanonicalCbor(signedTxCbor);
    tx = decodeMidgardLedgerTxFromCanonicalCbor(signedTxCbor);
  } catch (cause) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is not decodable: ${String(cause)}`,
    );
  }
  if (tx.txId.toString("hex") !== entry.txHash) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has a tx hash mismatch.`,
    );
  }
  const expectedNetworkId = network === "Mainnet" ? 1n : 0n;
  if (tx.networkId !== expectedNetworkId) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has network id ${String(tx.networkId)}, expected ${expectedNetworkId.toString()}.`,
    );
  }
  if (tx.validity !== "TxIsValid") {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is marked invalid.`,
    );
  }
  if (
    nativeTx.version !== MIDGARD_NATIVE_TX_VERSION ||
    nativeTx.body.validityIntervalStart !== MIDGARD_POSIX_TIME_NONE ||
    nativeTx.body.validityIntervalEnd !== MIDGARD_POSIX_TIME_NONE ||
    tx.validityIntervalStart !== undefined ||
    tx.validityIntervalEnd !== undefined
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} must use the supported native version with unbounded validity intervals.`,
    );
  }
  const decodedInputs = tx.spendInputs
    .map((input) => `${input.txId.toString("hex")}#${input.index.toString()}`)
    .sort();
  const journaledInputs = [...entry.selectedInputs].sort();
  if (
    decodedInputs.length !== journaledInputs.length ||
    decodedInputs.some((input, index) => input !== journaledInputs[index])
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} does not spend exactly its journaled selected inputs.`,
    );
  }
  if (
    new Set(journaledInputs).size !== journaledInputs.length ||
    journaledInputs.some((input) => !entry.beforeOutrefs.includes(input))
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} selects inputs outside its exact source snapshot.`,
    );
  }
  const requestedLovelace = BigInt(entry.requestedLovelace);
  const selectedInputLovelace = BigInt(entry.selectedInputLovelace);
  if (tx.fee < 0n || tx.fee > reserveLovelace) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has fee ${tx.fee.toString()} outside reserve ${reserveLovelace.toString()}.`,
    );
  }
  const expectedChange = selectedInputLovelace - requestedLovelace - tx.fee;
  if (expectedChange < 0n) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} does not conserve selected input lovelace.`,
    );
  }
  let treasuryOutputCount = 0;
  let sourceOutputCount = 0;
  let sourceChange = 0n;
  for (const output of tx.outputs) {
    const outputAddress = encodeMidgardAddressText(output.address);
    if (
      output.datum !== undefined ||
      output.scriptRef !== undefined ||
      output.value.assets.size !== 0
    ) {
      throw new Error(
        `Consolidation transaction intent for ${entry.walletId} contains datum, script, or non-ADA output content.`,
      );
    }
    if (outputAddress === treasuryAddress) {
      treasuryOutputCount += 1;
      if (output.value.lovelace !== requestedLovelace) {
        throw new Error(
          `Consolidation transaction intent for ${entry.walletId} has the wrong treasury value.`,
        );
      }
    } else if (outputAddress === source.l2Address) {
      sourceOutputCount += 1;
      sourceChange += output.value.lovelace;
    } else {
      throw new Error(
        `Consolidation transaction intent for ${entry.walletId} pays an unexpected address ${outputAddress}.`,
      );
    }
  }
  if (treasuryOutputCount !== 1 || sourceOutputCount > 1) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} must contain exactly one treasury output and at most one source change output.`,
    );
  }
  if (sourceChange !== expectedChange) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} has source change ${sourceChange.toString()}, expected ${expectedChange.toString()}.`,
    );
  }
  const expectedSigner = source.paymentKeyHash;
  const requiredSigners = tx.requiredSignerHashes.map((hash) =>
    hash.toString("hex"),
  );
  const witnessSigners = tx.witnessKeyHashes.map((hash) =>
    hash.toString("hex"),
  );
  if (
    requiredSigners.length !== 1 ||
    requiredSigners[0] !== expectedSigner ||
    witnessSigners.length !== 1 ||
    witnessSigners[0] !== expectedSigner ||
    tx.vkeyWitnesses.length !== 1
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} is not signed solely by its source payment key.`,
    );
  }
  const witness = tx.vkeyWitnesses[0]!;
  const publicKey = CML.PublicKey.from_bytes(witness.vkey);
  try {
    const signature = CML.Ed25519Signature.from_raw_bytes(witness.signature);
    try {
      if (!publicKey.verify(tx.txId, signature)) {
        throw new Error(
          `Consolidation transaction intent for ${entry.walletId} has an invalid source signature.`,
        );
      }
    } finally {
      signature.free();
    }
  } finally {
    publicKey.free();
  }
  if (
    tx.referenceInputs.length !== 0 ||
    tx.requiredObserverHashes.length !== 0 ||
    tx.scriptWitnesses.length !== 0 ||
    tx.nativeScriptHashes.length !== 0 ||
    tx.plutusScriptHashes.length !== 0 ||
    tx.redeemers.length !== 0 ||
    tx.mint.assets.length !== 0 ||
    tx.requiresPlutusEvaluation ||
    !tx.auxiliaryDataHash.equals(EMPTY_NULL_ROOT) ||
    !tx.scriptIntegrityHash.equals(EMPTY_NULL_ROOT)
  ) {
    throw new Error(
      `Consolidation transaction intent for ${entry.walletId} contains forbidden reference, mint, observer, auxiliary, or script content.`,
    );
  }
};

export const parseStressWalletConsolidationJournal = (
  value: unknown,
  path = "<memory>",
): ConsolidationState => {
  const raw = asObject(value, "stress wallet consolidation state");
  assertExactKeys(raw, "stress wallet consolidation state", [
    "schemaVersion",
    "treasuryAddress",
    "nodeEndpoint",
    "reserveLovelace",
    "scope",
    "entries",
  ]);
  if (
    raw.schemaVersion !== STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION
  ) {
    throw new Error(
      `Unsupported consolidation state schema at ${path}; canonical V1 is required.`,
    );
  }
  if (!Array.isArray(raw.entries)) {
    throw new Error(`Consolidation state entries at ${path} must be an array.`);
  }
  const walletIds = new Set<string>();
  const txHashes = new Set<string>();
  const entries = raw.entries.map((value, index): ConsolidationStateEntry => {
    const entry = asObject(value, `entries[${index.toString()}]`);
    assertExactKeys(
      entry,
      `entries[${index.toString()}]`,
      [
        "walletId",
        "address",
        "beforeLovelace",
        "beforeOutrefs",
        "requestedLovelace",
      ],
      [
        "txHash",
        "signedTxCbor",
        "selectedInputs",
        "selectedInputLovelace",
        "acceptedStatus",
      ],
    );
    if (!Array.isArray(entry.beforeOutrefs)) {
      throw new Error(
        `Consolidation state entries[${index.toString()}].beforeOutrefs must be an array.`,
      );
    }
    if (
      entry.selectedInputs !== undefined &&
      !Array.isArray(entry.selectedInputs)
    ) {
      throw new Error(
        `Consolidation state entries[${index.toString()}].selectedInputs must be an array.`,
      );
    }
    const parsed: ConsolidationStateEntry = {
      walletId: requiredString(entry.walletId, "walletId"),
      address: requiredString(entry.address, "address"),
      beforeLovelace: requiredString(entry.beforeLovelace, "beforeLovelace"),
      beforeOutrefs: entry.beforeOutrefs.map((outref) =>
        requiredString(outref, "beforeOutref"),
      ),
      requestedLovelace: requiredString(
        entry.requestedLovelace,
        "requestedLovelace",
      ),
      ...(entry.txHash === undefined
        ? {}
        : { txHash: requiredString(entry.txHash, "txHash") }),
      ...(entry.signedTxCbor === undefined
        ? {}
        : {
            signedTxCbor: requiredString(entry.signedTxCbor, "signedTxCbor"),
          }),
      ...(entry.selectedInputs === undefined
        ? {}
        : {
            selectedInputs: entry.selectedInputs.map((input) =>
              requiredString(input, "selectedInput"),
            ),
          }),
      ...(entry.selectedInputLovelace === undefined
        ? {}
        : {
            selectedInputLovelace: requiredString(
              entry.selectedInputLovelace,
              "selectedInputLovelace",
            ),
          }),
      ...(entry.acceptedStatus === undefined
        ? {}
        : {
            acceptedStatus: requiredString(
              entry.acceptedStatus,
              "acceptedStatus",
            ),
          }),
    };
    if (walletIds.has(parsed.walletId)) {
      throw new Error(
        `Duplicate walletId ${parsed.walletId} in consolidation state at ${path}.`,
      );
    }
    walletIds.add(parsed.walletId);
    if (parsed.txHash !== undefined) {
      if (!/^[0-9a-f]{64}$/.test(parsed.txHash)) {
        throw new Error(
          `Consolidation state txHash for ${parsed.walletId} must be a lowercase 32-byte digest.`,
        );
      }
      if (txHashes.has(parsed.txHash)) {
        throw new Error(
          `Duplicate txHash ${parsed.txHash} in consolidation state at ${path}.`,
        );
      }
      txHashes.add(parsed.txHash);
    }
    if (
      parsed.signedTxCbor !== undefined &&
      (!/^[0-9a-f]+$/.test(parsed.signedTxCbor) ||
        parsed.signedTxCbor.length % 2 !== 0)
    ) {
      throw new Error(
        `Consolidation state signedTxCbor for ${parsed.walletId} must be non-empty lowercase hex.`,
      );
    }
    if (
      parsed.signedTxCbor !== undefined &&
      parsed.txHash !== undefined &&
      computeSignedNativeTxHash(parsed.signedTxCbor, parsed.walletId) !==
        parsed.txHash
    ) {
      throw new Error(
        `Consolidation state txHash/signedTxCbor mismatch for ${parsed.walletId}.`,
      );
    }
    if (parsed.signedTxCbor !== undefined && parsed.txHash === undefined) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} has signedTxCbor without txHash.`,
      );
    }
    if (
      parsed.selectedInputLovelace !== undefined &&
      !/^(0|[1-9]\d*)$/.test(parsed.selectedInputLovelace)
    ) {
      throw new Error(
        `Consolidation state selectedInputLovelace for ${parsed.walletId} must be a canonical non-negative decimal.`,
      );
    }
    if (
      parsed.signedTxCbor !== undefined &&
      (parsed.selectedInputs === undefined ||
        parsed.selectedInputLovelace === undefined)
    ) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} has signedTxCbor without exact selected input accounting.`,
      );
    }
    if (
      parsed.signedTxCbor === undefined &&
      parsed.selectedInputLovelace !== undefined
    ) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} has selected input accounting without signedTxCbor.`,
      );
    }
    if (parsed.txHash !== undefined && parsed.signedTxCbor === undefined) {
      throw new Error(
        `Consolidation state entry ${parsed.walletId} lacks its exact signedTxCbor.`,
      );
    }
    return parsed;
  });
  return {
    schemaVersion: STRESS_WALLET_CONSOLIDATION_JOURNAL_SCHEMA_VERSION,
    treasuryAddress: requiredString(raw.treasuryAddress, "treasuryAddress"),
    nodeEndpoint: requiredString(raw.nodeEndpoint, "nodeEndpoint"),
    reserveLovelace: requiredString(raw.reserveLovelace, "reserveLovelace"),
    scope: parseStressWalletOperationScope(raw.scope),
    entries,
  };
};

const readConsolidationState = async (
  path: string,
): Promise<ConsolidationState | undefined> => {
  if (!(await fileExists(path))) return undefined;
  return parseStressWalletConsolidationJournal(
    JSON.parse(await readFile(path, "utf8")) as unknown,
    path,
  );
};

const waitForConsolidationAcceptance = async ({
  nodeEndpoint,
  txHash,
  runtime,
  acceptanceTimeoutMs,
  pollInitialIntervalMs,
  pollMaxIntervalMs,
}: {
  readonly nodeEndpoint: string;
  readonly txHash: string;
  readonly runtime: StressWalletConsolidateRuntime;
  readonly acceptanceTimeoutMs: number;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
}): Promise<string> => {
  const sleepImpl = runtime.sleep ?? sleep;
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const startedAt = monotonicNow();
  let attempt = 0;
  while (true) {
    const status = (await runtime.fetchTxStatus(nodeEndpoint, txHash))
      .trim()
      .toLowerCase();
    if (status === "committed") return status;
    if (rejectedTxStatuses.has(status)) {
      throw new Error(
        `Consolidation transfer ${txHash} reached rejected status ${status}.`,
      );
    }
    if (!consolidationPendingTxStatuses.has(status)) {
      throw new Error(
        `Consolidation transfer ${txHash} returned unknown status ${status || "<empty>"}; refusing to infer commitment/finality.`,
      );
    }
    if (monotonicNow() - startedAt >= acceptanceTimeoutMs) {
      throw new Error(
        `Timed out waiting ${acceptanceTimeoutMs.toString()}ms for consolidation transfer ${txHash}; last status ${status}.`,
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

const waitForConsolidationReadiness = async ({
  nodeEndpoint,
  runtime,
  readinessPath,
  batchIndex,
  firstWalletId,
  timeoutMs,
  requestTimeoutMs,
  pollInitialIntervalMs,
  pollMaxIntervalMs,
  now,
}: {
  readonly nodeEndpoint: string;
  readonly runtime: StressWalletConsolidateRuntime;
  readonly readinessPath: string;
  readonly batchIndex: number;
  readonly firstWalletId: string;
  readonly timeoutMs: number;
  readonly requestTimeoutMs: number;
  readonly pollInitialIntervalMs: number;
  readonly pollMaxIntervalMs: number;
  readonly now: () => Date;
}): Promise<void> => {
  const fetchReadiness =
    runtime.fetchReadiness ??
    ((endpoint: string) =>
      defaultFetchConsolidationReadiness(endpoint, requestTimeoutMs));
  const sleepImpl = runtime.sleep ?? sleep;
  const monotonicNow = runtime.monotonicNow ?? (() => Date.now());
  const startedAt = monotonicNow();
  let attempt = 0;
  while (true) {
    const response = await fetchReadiness(nodeEndpoint);
    let snapshot: ConsolidationReadinessSnapshot;
    try {
      snapshot = parseConsolidationReadiness(response);
    } catch (error) {
      const evidence = {
        schemaVersion: STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION,
        observedAt: now().toISOString(),
        batchIndex,
        firstWalletId,
        attempt,
        malformed: true,
        error: error instanceof Error ? error.message : String(error),
        response,
      };
      parseStressWalletConsolidationReadinessEvidence(evidence);
      await writeFile(readinessPath, JSON.stringify(evidence) + "\n", {
        encoding: "utf8",
        flag: "a",
        mode: 0o600,
      });
      throw error;
    }
    const fullReady = isFullConsolidationReadiness(snapshot);
    const evidence = {
      schemaVersion: STRESS_WALLET_CONSOLIDATION_READINESS_SCHEMA_VERSION,
      observedAt: now().toISOString(),
      batchIndex,
      firstWalletId,
      attempt,
      fullReady,
      snapshot,
    };
    parseStressWalletConsolidationReadinessEvidence(evidence);
    await writeFile(readinessPath, JSON.stringify(evidence) + "\n", {
      encoding: "utf8",
      flag: "a",
      mode: 0o600,
    });
    if (fullReady) return;
    if (monotonicNow() - startedAt >= timeoutMs) {
      throw new Error(
        "Timed out waiting " +
          timeoutMs.toString() +
          "ms for full consolidation readiness before batch " +
          batchIndex.toString() +
          " (first wallet " +
          firstWalletId +
          "); last snapshot " +
          formatJson(snapshot) +
          ".",
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

const consolidateStressWalletsUnlocked = async (
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

export const consolidateStressWallets = async (
  options: ConsolidateStressWalletsOptions,
  runtime: StressWalletConsolidateRuntime,
): Promise<StressWalletConsolidateResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  const statePath = join(outDir, "consolidation-state.json");
  return withExclusiveStressWalletFundsLock(outDir, () =>
    withExclusiveConsolidationStateLock(statePath, () =>
      consolidateStressWalletsUnlocked(options, runtime),
    ),
  );
};
