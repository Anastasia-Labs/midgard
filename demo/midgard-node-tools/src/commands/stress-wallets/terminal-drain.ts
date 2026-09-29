import { chmod, readFile } from "node:fs/promises";
import { join } from "node:path";

import {
  EMPTY_NULL_ROOT,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import {
  computeMidgardNativeTxId,
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
  requiredPositiveInteger,
  requiredString,
} from "./artifact-fields.js";
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
  fileExists,
  readFileGeneration,
  sha256Bytes,
  withExclusiveStressWalletFundsLock,
  writePrivateFileAtomic,
  writePrivateFileAtomicNoReplace,
} from "./files.js";
import {
  defaultGenerateSeedPhrase,
  normalizeEnvPrefix,
  parseStressWalletNetwork,
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
  parseStressWalletOperationScope,
  sameStressWalletOperationScope,
  type StressWalletOperationScope,
} from "./scope.js";
import {
  type StressWalletRecord,
  type StressWalletTerminalDrainResult,
  type StressWalletTerminalDrainRuntime,
  type TerminalDrainStressWalletsOptions,
} from "./types.js";
import { outRefKey, sumLovelace, utxoAccounting } from "./utxos.js";

export type TerminalDrainEntry = {
  readonly walletId: string;
  readonly address: string;
  readonly beforeOutrefs: readonly string[];
  readonly beforeLovelace: string;
  readonly beforeValueSha256: string;
  readonly status: "already_empty" | "prepared" | "committed";
  readonly txHash?: string;
  readonly signedTxCbor?: string;
  readonly selectedInputs?: readonly string[];
  readonly requestedLovelace?: string;
  readonly feeLovelace?: string;
  readonly signedTxBytes?: number;
};
export type TerminalDrainState = {
  readonly schemaVersion: typeof STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION;
  readonly scope: StressWalletOperationScope;
  readonly scopeSha256: string;
  readonly nodeEndpoint: string;
  readonly network: Network;
  readonly treasuryAddress: string;
  readonly treasuryBeforeLovelace: string;
  readonly minFeeA: string;
  readonly minFeeB: string;
  readonly feeCapLovelace: string;
  readonly maxFeeIterations: number;
  readonly entries: readonly TerminalDrainEntry[];
};
const terminalScopeHash = (scope: StressWalletOperationScope): string =>
  sha256Bytes(Buffer.from(JSON.stringify(scope), "utf8"));
const terminalSnapshotHash = (utxos: readonly NodeUtxo[]): string =>
  sha256Bytes(
    Buffer.from(
      JSON.stringify(
        [...utxos]
          .sort((a, b) => outRefKey(a).localeCompare(outRefKey(b)))
          .map((utxo) => ({
            outref: outRefKey(utxo),
            outputCbor: utxo.outputCbor.toString("hex"),
            assets: Object.fromEntries(
              Object.entries(utxo.assets)
                .filter(([, q]) => q !== 0n)
                .sort(([a], [b]) => a.localeCompare(b))
                .map(([unit, q]) => [unit, q.toString(10)]),
            ),
          })),
      ),
      "utf8",
    ),
  );
const terminalDecimal = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.length === 0 || value !== value.trim())
    throw new Error(label + " must be an exact non-empty string.");
  const parsed = value;
  if (!/^(0|[1-9]\d*)$/.test(parsed))
    throw new Error(label + " must be a canonical non-negative decimal.");
  return parsed;
};
const terminalExactString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || value.length === 0 || value !== value.trim())
    throw new Error(label + " must be an exact non-empty string.");
  return value;
};
export const parseStressWalletTerminalDrainJournal = (
  value: unknown,
): TerminalDrainState => {
  const raw = asObject(value, "terminal drain state");
  assertExactKeys(raw, "terminal drain state", [
    "schemaVersion",
    "scope",
    "scopeSha256",
    "nodeEndpoint",
    "network",
    "treasuryAddress",
    "treasuryBeforeLovelace",
    "minFeeA",
    "minFeeB",
    "feeCapLovelace",
    "maxFeeIterations",
    "entries",
  ]);
  if (
    raw.schemaVersion !== STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION ||
    !Array.isArray(raw.entries)
  )
    throw new Error("Unsupported terminal drain journal schema.");
  const scope = parseStressWalletOperationScope(raw.scope);
  const walletIds = new Set<string>();
  const addresses = new Set<string>();
  const txHashes = new Set<string>();
  const entries = raw.entries.map((value, index): TerminalDrainEntry => {
    const e = asObject(value, "entries[" + index.toString() + "]");
    assertExactKeys(
      e,
      "entries[" + index.toString() + "]",
      [
        "walletId",
        "address",
        "beforeOutrefs",
        "beforeLovelace",
        "beforeValueSha256",
        "status",
      ],
      [
        "txHash",
        "signedTxCbor",
        "selectedInputs",
        "requestedLovelace",
        "feeLovelace",
        "signedTxBytes",
      ],
    );
    if (
      !Array.isArray(e.beforeOutrefs) ||
      (e.selectedInputs !== undefined && !Array.isArray(e.selectedInputs))
    )
      throw new Error("Terminal drain input lists must be arrays.");
    const status = requiredString(e.status, "status");
    if (
      status !== "already_empty" &&
      status !== "prepared" &&
      status !== "committed"
    )
      throw new Error("Invalid terminal drain status.");
    const transactionFields = [
      e.txHash,
      e.signedTxCbor,
      e.selectedInputs,
      e.requestedLovelace,
      e.feeLovelace,
      e.signedTxBytes,
    ];
    if (
      status === "already_empty" &&
      transactionFields.some((field) => field !== undefined)
    )
      throw new Error(
        "Terminal drain already_empty entry contains forbidden transaction fields.",
      );
    if (
      status !== "already_empty" &&
      transactionFields.some((field) => field === undefined)
    )
      throw new Error("Terminal drain prepared/committed entry is incomplete.");
    const walletId = terminalExactString(e.walletId, "walletId");
    const address = terminalExactString(e.address, "address");
    if (walletIds.has(walletId) || addresses.has(address))
      throw new Error("Terminal drain wallet identities must be unique.");
    walletIds.add(walletId);
    addresses.add(address);
    const beforeOutrefs = e.beforeOutrefs.map((x) =>
      terminalExactString(x, "beforeOutref"),
    );
    if (
      beforeOutrefs.some(
        (outref) => !/^[0-9a-f]{64}#(0|[1-9]\d*)$/.test(outref),
      ) ||
      new Set(beforeOutrefs).size !== beforeOutrefs.length ||
      [...beforeOutrefs].sort().join("|") !== beforeOutrefs.join("|")
    )
      throw new Error(
        "Terminal drain beforeOutrefs must be unique sorted canonical outrefs.",
      );
    const beforeLovelace = terminalDecimal(e.beforeLovelace, "beforeLovelace");
    const beforeValueSha256 = terminalExactString(
      e.beforeValueSha256,
      "beforeValueSha256",
    );
    if (!/^[0-9a-f]{64}$/.test(beforeValueSha256))
      throw new Error(
        "Terminal drain beforeValueSha256 must be a lowercase SHA-256 digest.",
      );
    const parsed: TerminalDrainEntry = {
      walletId,
      address,
      beforeOutrefs,
      beforeLovelace,
      beforeValueSha256,
      status,
      ...(e.txHash === undefined
        ? {}
        : { txHash: terminalExactString(e.txHash, "txHash") }),
      ...(e.signedTxCbor === undefined
        ? {}
        : {
            signedTxCbor: terminalExactString(e.signedTxCbor, "signedTxCbor"),
          }),
      ...(e.selectedInputs === undefined
        ? {}
        : {
            selectedInputs: e.selectedInputs.map((x) =>
              terminalExactString(x, "selectedInput"),
            ),
          }),
      ...(e.requestedLovelace === undefined
        ? {}
        : {
            requestedLovelace: terminalDecimal(
              e.requestedLovelace,
              "requestedLovelace",
            ),
          }),
      ...(e.feeLovelace === undefined
        ? {}
        : { feeLovelace: terminalDecimal(e.feeLovelace, "feeLovelace") }),
      ...(e.signedTxBytes === undefined
        ? {}
        : {
            signedTxBytes: requiredPositiveInteger(
              e.signedTxBytes,
              "signedTxBytes",
            ),
          }),
    };
    if (status === "already_empty") {
      if (
        beforeOutrefs.length !== 0 ||
        beforeLovelace !== "0" ||
        beforeValueSha256 !== terminalSnapshotHash([])
      )
        throw new Error(
          "Terminal drain already_empty entry must bind the exact empty snapshot.",
        );
      return parsed;
    }
    const txHash = parsed.txHash!;
    const signedTxCbor = parsed.signedTxCbor!;
    const selectedInputs = parsed.selectedInputs!;
    if (
      !/^[0-9a-f]{64}$/.test(txHash) ||
      !/^[0-9a-f]+$/.test(signedTxCbor) ||
      signedTxCbor.length % 2 !== 0 ||
      txHashes.has(txHash)
    )
      throw new Error(
        "Terminal drain transaction identities must be unique lowercase canonical encodings.",
      );
    txHashes.add(txHash);
    if (
      selectedInputs.some(
        (outref) => !/^[0-9a-f]{64}#(0|[1-9]\d*)$/.test(outref),
      ) ||
      new Set(selectedInputs).size !== selectedInputs.length ||
      [...selectedInputs].sort().join("|") !== selectedInputs.join("|") ||
      selectedInputs.join("|") !== beforeOutrefs.join("|")
    )
      throw new Error(
        "Terminal drain selectedInputs must exactly equal the sorted snapshot outrefs.",
      );
    const signedTxBytes = Buffer.from(signedTxCbor, "hex");
    if (
      signedTxBytes.toString("hex") !== signedTxCbor ||
      signedTxBytes.length !== parsed.signedTxBytes
    )
      throw new Error(
        "Terminal drain signedTxBytes must bind exact signedTxCbor.",
      );
    let computedTxHash: string;
    try {
      computedTxHash = computeMidgardNativeTxId(
        decodeMidgardNativeTxFullFromCanonicalCbor(signedTxBytes),
      ).toString("hex");
    } catch (cause) {
      throw new Error(
        "Terminal drain signedTxCbor must be canonical Midgard native V1 transaction CBOR: " +
          String(cause),
      );
    }
    if (computedTxHash !== txHash)
      throw new Error("Terminal drain txHash must bind exact signedTxCbor.");
    if (
      BigInt(parsed.requestedLovelace!) + BigInt(parsed.feeLovelace!) !==
      BigInt(beforeLovelace)
    )
      throw new Error(
        "Terminal drain entry must conserve beforeLovelace as requestedLovelace+feeLovelace.",
      );
    return parsed;
  });
  const scopeSha256 = terminalExactString(raw.scopeSha256, "scopeSha256");
  if (
    !/^[0-9a-f]{64}$/.test(scopeSha256) ||
    scopeSha256 !== terminalScopeHash(scope)
  )
    throw new Error(
      "Terminal drain scopeSha256 must bind the exact parsed scope.",
    );
  return {
    schemaVersion: STRESS_WALLET_TERMINAL_DRAIN_JOURNAL_SCHEMA_VERSION,
    scope,
    scopeSha256,
    nodeEndpoint: terminalExactString(raw.nodeEndpoint, "nodeEndpoint"),
    network: parseStressWalletNetwork(
      requiredString(raw.network, "network"),
      {},
    ),
    treasuryAddress: terminalExactString(
      raw.treasuryAddress,
      "treasuryAddress",
    ),
    treasuryBeforeLovelace: terminalDecimal(
      raw.treasuryBeforeLovelace,
      "treasuryBeforeLovelace",
    ),
    minFeeA: terminalDecimal(raw.minFeeA, "minFeeA"),
    minFeeB: terminalDecimal(raw.minFeeB, "minFeeB"),
    feeCapLovelace: terminalDecimal(raw.feeCapLovelace, "feeCapLovelace"),
    maxFeeIterations: requiredPositiveInteger(
      raw.maxFeeIterations,
      "maxFeeIterations",
    ),
    entries,
  };
};
const readTerminalDrainState = async (
  path: string,
): Promise<TerminalDrainState | undefined> => {
  if (!(await fileExists(path))) return undefined;
  return parseStressWalletTerminalDrainJournal(
    JSON.parse(await readFile(path, "utf8")) as unknown,
  );
};
const assertTerminalDrainIntent = (
  entry: TerminalDrainEntry,
  source: StressWalletRecord,
  state: TerminalDrainState,
): void => {
  if (
    !/^[0-9a-f]{64}$/.test(entry.beforeValueSha256) ||
    new Set(entry.beforeOutrefs).size !== entry.beforeOutrefs.length
  )
    throw new Error("Terminal drain snapshot digest/outrefs are malformed.");
  if (entry.status === "already_empty") {
    if (
      entry.beforeOutrefs.length !== 0 ||
      entry.beforeLovelace !== "0" ||
      entry.beforeValueSha256 !== terminalSnapshotHash([]) ||
      entry.txHash !== undefined
    )
      throw new Error(
        "Terminal drain already_empty entry is not exactly empty.",
      );
    return;
  }
  if (
    !entry.txHash ||
    !entry.signedTxCbor ||
    !entry.selectedInputs ||
    entry.requestedLovelace === undefined ||
    entry.feeLovelace === undefined ||
    entry.signedTxBytes === undefined
  )
    throw new Error("Terminal drain prepared entry is incomplete.");
  if (
    !/^[0-9a-f]{64}$/.test(entry.txHash) ||
    !/^[0-9a-f]+$/.test(entry.signedTxCbor) ||
    entry.signedTxCbor.length % 2 !== 0
  )
    throw new Error("Terminal drain hash/CBOR encoding is malformed.");
  const bytes = Buffer.from(entry.signedTxCbor, "hex");
  const native = decodeMidgardNativeTxFullFromCanonicalCbor(bytes);
  const tx = decodeMidgardLedgerTxFromCanonicalCbor(bytes);
  if (
    tx.txId.toString("hex") !== entry.txHash ||
    computeMidgardNativeTxId(native).toString("hex") !== entry.txHash
  )
    throw new Error("Terminal drain hash does not bind its exact signed CBOR.");
  if (
    tx.networkId !== (state.network === "Mainnet" ? 1n : 0n) ||
    tx.validity !== "TxIsValid" ||
    native.version !== MIDGARD_NATIVE_TX_VERSION ||
    native.body.validityIntervalStart !== MIDGARD_POSIX_TIME_NONE ||
    native.body.validityIntervalEnd !== MIDGARD_POSIX_TIME_NONE ||
    tx.validityIntervalStart !== undefined ||
    tx.validityIntervalEnd !== undefined
  )
    throw new Error(
      "Terminal drain network/version/validity invariant failed.",
    );
  const decodedInputs = tx.spendInputs
    .map((x) => x.txId.toString("hex") + "#" + x.index.toString())
    .sort();
  const selected = [...entry.selectedInputs].sort();
  const before = [...entry.beforeOutrefs].sort();
  if (
    new Set(selected).size !== selected.length ||
    decodedInputs.join("|") !== selected.join("|") ||
    selected.join("|") !== before.join("|")
  )
    throw new Error(
      "Terminal drain must spend every and only snapshotted input.",
    );
  const total = BigInt(entry.beforeLovelace);
  const requested = BigInt(entry.requestedLovelace);
  const fee = BigInt(entry.feeLovelace);
  const requiredFee =
    BigInt(state.minFeeA) * BigInt(bytes.length) + BigInt(state.minFeeB);
  if (
    requested <= 0n ||
    tx.fee !== fee ||
    entry.signedTxBytes !== bytes.length ||
    requested + fee !== total ||
    fee < requiredFee ||
    fee > BigInt(state.feeCapLovelace)
  )
    throw new Error("Terminal drain fee/size/conservation invariant failed.");
  if (tx.outputs.length !== 1)
    throw new Error(
      "Terminal drain must have exactly one output and zero source change.",
    );
  const output = tx.outputs[0]!;
  if (
    encodeMidgardAddressText(output.address) !== state.treasuryAddress ||
    output.value.lovelace !== requested ||
    output.value.assets.size !== 0 ||
    output.datum !== undefined ||
    output.scriptRef !== undefined
  )
    throw new Error(
      "Terminal drain output is not the exact ADA-only treasury payment.",
    );
  const signer = source.paymentKeyHash;
  if (
    tx.requiredSignerHashes.length !== 1 ||
    tx.requiredSignerHashes[0]!.toString("hex") !== signer ||
    tx.witnessKeyHashes.length !== 1 ||
    tx.witnessKeyHashes[0]!.toString("hex") !== signer ||
    tx.vkeyWitnesses.length !== 1
  )
    throw new Error("Terminal drain is not signed solely by the source key.");
  const witness = tx.vkeyWitnesses[0]!;
  const publicKey = CML.PublicKey.from_bytes(witness.vkey);
  try {
    const signature = CML.Ed25519Signature.from_raw_bytes(witness.signature);
    try {
      if (!publicKey.verify(tx.txId, signature))
        throw new Error("Terminal drain signature is invalid.");
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
  )
    throw new Error(
      "Terminal drain contains forbidden script/mint/reference/auxiliary content.",
    );
};
const terminalDrainStressWalletsUnlocked = async (
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
export const terminalDrainStressWallets = async (
  options: TerminalDrainStressWalletsOptions,
  runtime: StressWalletTerminalDrainRuntime,
): Promise<StressWalletTerminalDrainResult> => {
  const outDir = options.outDir?.trim() || DEFAULT_STRESS_WALLET_DIR;
  return withExclusiveStressWalletFundsLock(outDir, () =>
    terminalDrainStressWalletsUnlocked(options, runtime),
  );
};
