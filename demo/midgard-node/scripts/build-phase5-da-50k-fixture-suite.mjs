#!/usr/bin/env node

import { createHash, randomBytes } from "node:crypto";
import { createReadStream } from "node:fs";
import {
  access,
  link,
  mkdir,
  open,
  readFile,
  rename,
  rm,
  stat,
  unlink,
  writeFile,
} from "node:fs/promises";
import { isAbsolute, relative, resolve, sep } from "node:path";
import { createInterface } from "node:readline";
import { Transform } from "node:stream";
import { pathToFileURL } from "node:url";

import { wrapDaPayloadV3 } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  PHASE5_DA_ANCHOR,
  PHASE5_DA_FIXTURE_SUITE_SCHEMA,
  PHASE5_DA_SAMPLE_COUNT,
  PHASE5_DA_TX_COUNT,
  verifyPhase5DaNativeTransactionIdentity,
  verifyPhase5DaSourceCorpusEvidence,
} from "./verify-phase5-da-50k-distribution-report.mjs";
import { createPhase5TransactionIdDisjointnessTracker } from "./phase5-transaction-id-disjointness.mjs";

export const PHASE5_DA_FIXTURE_SUITE_TEST_SCHEMA =
  "midgard-phase-5-da-50k-fixture-suite-test-v1";
export const PHASE5_DA_FIXTURE_SUITE_TEST_GUARD =
  "MIDGARD_PHASE5_FIXTURE_SUITE_TEST_ONLY_v1";

const MANIFEST_NAME = "manifest.json";
const ENVELOPES_DIRECTORY = "envelopes";
const LOCK_NAME = ".phase5-da-fixture-suite.lock";
const LOCK_SCHEMA = "midgard-phase5-fixture-suite-lock-v1";
const TRANSACTION_ID_BUCKETS_DIRECTORY =
  ".phase5-da-fixture-transaction-id-buckets";
const SHA256_PATTERN = /^[0-9a-f]{64}$/u;

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");

const requirePositiveSafeInteger = (value, label) => {
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${label} must be a positive safe integer`);
  }
  return value;
};

const requireSha256 = (value, label) => {
  if (typeof value !== "string" || !SHA256_PATTERN.test(value)) {
    throw new Error(`${label} must be 32-byte lowercase hex`);
  }
  return value;
};

const isAnchor = (value) =>
  value !== null &&
  typeof value === "object" &&
  SHA256_PATTERN.test(value.corpusPrefixSha256 ?? "") &&
  /^[0-9a-f]{56}$/u.test(value.headerHash ?? "") &&
  SHA256_PATTERN.test(value.innerSha256 ?? "") &&
  SHA256_PATTERN.test(value.envelopeSha256 ?? "") &&
  Number.isSafeInteger(value.innerBytes) &&
  value.innerBytes > 0 &&
  Number.isSafeInteger(value.envelopeBytes) &&
  value.envelopeBytes > 0;

const anchorsEqual = (left, right) =>
  Object.entries(left).every(([key, value]) => right?.[key] === value);

export const resolveFixtureBuildContract = ({
  sampleCount = PHASE5_DA_SAMPLE_COUNT,
  transactionsPerSample = PHASE5_DA_TX_COUNT,
  testOnlyGuard,
  testAnchor,
} = {}) => {
  requirePositiveSafeInteger(sampleCount, "sampleCount");
  requirePositiveSafeInteger(transactionsPerSample, "transactionsPerSample");
  const testOnly = testOnlyGuard !== undefined;
  const requiredRows = sampleCount * transactionsPerSample;
  if (!Number.isSafeInteger(requiredRows)) {
    throw new Error("fixture-suite required row count exceeds safe bounds");
  }
  if (testOnly) {
    if (testOnlyGuard !== PHASE5_DA_FIXTURE_SUITE_TEST_GUARD) {
      throw new Error("invalid Phase 5 fixture-suite test-only guard");
    }
    if (!isAnchor(testAnchor)) {
      throw new Error(
        "test-only fixture generation requires an exact test anchor",
      );
    }
    return {
      formal: false,
      schemaVersion: PHASE5_DA_FIXTURE_SUITE_TEST_SCHEMA,
      sampleCount,
      transactionsPerSample,
      requiredRows,
      anchor: { ...testAnchor },
    };
  }
  if (
    sampleCount !== PHASE5_DA_SAMPLE_COUNT ||
    transactionsPerSample !== PHASE5_DA_TX_COUNT ||
    testAnchor !== undefined
  ) {
    throw new Error(
      "formal Phase 5 fixture cardinality is pinned to 100 windows of 50,000 transactions; test overrides require the explicit test-only guard",
    );
  }
  return {
    formal: true,
    schemaVersion: PHASE5_DA_FIXTURE_SUITE_SCHEMA,
    sampleCount: PHASE5_DA_SAMPLE_COUNT,
    transactionsPerSample: PHASE5_DA_TX_COUNT,
    requiredRows: PHASE5_DA_SAMPLE_COUNT * PHASE5_DA_TX_COUNT,
    anchor: { ...PHASE5_DA_ANCHOR },
  };
};

const resolveContainedPath = (suiteDirectory, value, label) => {
  if (typeof value !== "string" || value.length === 0 || isAbsolute(value)) {
    throw new Error(`${label} must be a non-empty relative path`);
  }
  const resolved = resolve(suiteDirectory, value);
  const fromSuite = relative(suiteDirectory, resolved);
  if (
    fromSuite === "" ||
    fromSuite === ".." ||
    fromSuite.startsWith(`..${sep}`) ||
    isAbsolute(fromSuite)
  ) {
    throw new Error(`${label} escapes the fixture suite`);
  }
  return resolved;
};

const rejectReservedInputPath = (value, label) => {
  const firstSegment = value.split("/")[0];
  if (
    value === MANIFEST_NAME ||
    firstSegment === ENVELOPES_DIRECTORY ||
    firstSegment.startsWith(".phase5-da-fixture-")
  ) {
    throw new Error(`${label} uses a fixture-suite output path`);
  }
};

const pathExists = async (path) => {
  try {
    await access(path);
    return true;
  } catch {
    return false;
  }
};

const fixedLengthHex = (seed, bytes) => {
  const needed = bytes * 2;
  return seed.repeat(Math.ceil(needed / seed.length)).slice(0, needed);
};

const sortEntries = (entries) =>
  entries.sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0));

const hashTransactionEntries = (entries) => {
  const digest = createHash("sha256");
  for (const [key, value] of [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  )) {
    const keyBytes = Buffer.from(key, "hex");
    const valueBytes = Buffer.from(value, "hex");
    const lengths = Buffer.allocUnsafe(8);
    lengths.writeUInt32BE(keyBytes.length, 0);
    lengths.writeUInt32BE(valueBytes.length, 4);
    digest.update(lengths).update(keyBytes).update(valueBytes);
  }
  return digest.digest("hex");
};

const hashTransactionContents = (entries) => {
  const digest = createHash("sha256");
  for (const value of entries
    .map(([, transactionCbor]) => transactionCbor)
    .sort()) {
    const valueBytes = Buffer.from(value, "hex");
    const length = Buffer.allocUnsafe(4);
    length.writeUInt32BE(valueBytes.length);
    digest.update(length).update(valueBytes);
  }
  return digest.digest("hex");
};

const BASE_HEADER = Object.freeze({
  prevUtxosRoot: "00".repeat(32),
  utxosRoot: "01".repeat(32),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: "02".repeat(32),
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: "03".repeat(32),
  eventToStepRoot: "04".repeat(32),
  startTime: 1n,
  endTime: 2n,
  prevHeaderHash: "05".repeat(28),
  operatorVkey: "06".repeat(28),
  protocolVersion: 1n,
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 0n,
  depositCount: 0n,
  totalEventCount: 0n,
  transitionStepCount: 0n,
});

const headerForSample = (sampleIndex, transactionCount) => {
  const offset = BigInt(sampleIndex) * 2n;
  return {
    ...BASE_HEADER,
    ...(sampleIndex === 0
      ? {}
      : {
          startTime: 1n + offset,
          endTime: 2n + offset,
          prevHeaderHash: createHash("sha256")
            .update("midgard-phase5-da-50k-fixture-header-v1\0")
            .update(String(sampleIndex))
            .digest("hex")
            .slice(0, 56),
        }),
    l2TransactionCount: BigInt(transactionCount),
    totalEventCount: BigInt(transactionCount),
    transitionStepCount: BigInt(transactionCount),
  };
};

export const buildOperationalDaWindow = async (rows, sampleIndex) => {
  if (!Array.isArray(rows) || rows.length === 0) {
    throw new Error("operational DA window must contain transactions");
  }
  const transactions = sortEntries(
    rows.map((row) => [row.txHash, row.canonicalCborHex]),
  );
  const utxos = sortEntries(
    rows.flatMap((row) => [
      [`${row.txHash}00000000`, fixedLengthHex(row.canonicalCborHex, 69)],
      [
        `${row.txHash}00000001`,
        fixedLengthHex(row.canonicalCborHex.slice(2), 69),
      ],
    ]),
  );
  const transitionTrace = sortEntries(
    rows.map((row, index) => [
      LucidData.to(BigInt(index), LucidData.Integer()),
      LucidData.to(
        {
          schema_version: 1n,
          step_index: BigInt(index),
          event_key: { L2TransactionEventKey: { tx_id: row.txHash } },
          phase: "L2Transaction",
          pre_utxos_root: "01".repeat(32),
          post_utxos_root: "01".repeat(32),
        },
        SDK.TransitionStepSchema,
      ),
    ]),
  );
  const eventToStep = sortEntries(
    rows.map((row, index) => [
      LucidData.to(
        { L2TransactionEventKey: { tx_id: row.txHash } },
        SDK.EventKeySchema,
      ),
      LucidData.to(
        { step_index: BigInt(index), phase: "L2Transaction" },
        SDK.EventToStepValueSchema,
      ),
    ]),
  );
  const header = headerForSample(sampleIndex, rows.length);
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: BigInt(rows.length),
    depositCount: 0n,
    totalEventCount: BigInt(rows.length),
    transitionStepCount: BigInt(rows.length),
  };
  const inner = SDK.encodeDaPayloadV2({
    version: SDK.DA_PAYLOAD_V2_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos,
      withdrawals: [],
      forced_transactions: [],
      transactions,
      deposits: [],
      transition_trace: transitionTrace,
      event_to_step: eventToStep,
      counts,
    },
  });
  if (inner.length > DA_TRANSPORT_LIMITS_V1.maxPayloadBytes) {
    throw new Error(
      `fixture ${sampleIndex.toString()} inner payload is ${inner.length.toString()} bytes, above the pinned V1 limit`,
    );
  }
  const envelope = await wrapDaPayloadV3(inner, {
    mode: "zstd",
    zstdLevel: 3,
  });
  if (envelope.length > DA_TRANSPORT_LIMITS_V1.maxPayloadBytes) {
    throw new Error(
      `fixture ${sampleIndex.toString()} envelope is ${envelope.length.toString()} bytes, above the pinned V1 limit`,
    );
  }
  return {
    headerHash,
    inner,
    envelope,
    innerSha256: sha256(inner),
    envelopeSha256: sha256(envelope),
    transactionSetSha256: hashTransactionEntries(transactions),
    transactionContentSha256: hashTransactionContents(transactions),
  };
};

const parseCorpusRow = (line, rowIndex) => {
  let parsed;
  try {
    parsed = JSON.parse(line);
  } catch (error) {
    throw new Error(
      `source corpus row ${rowIndex.toString()} is not JSON: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  const txHash = parsed?.txHash;
  const canonicalCborHex = parsed?.canonicalCborHex;
  verifyPhase5DaNativeTransactionIdentity(
    txHash,
    canonicalCborHex,
    `source corpus row ${rowIndex.toString()}`,
  );
  const bytes = Buffer.from(canonicalCborHex, "hex");
  if (
    parsed.canonicalCborByteLength !== bytes.length ||
    parsed.canonicalCborSha256 !== sha256(bytes)
  ) {
    throw new Error(
      `source corpus row ${rowIndex.toString()} canonical byte metadata changed`,
    );
  }
  return { txHash, canonicalCborHex };
};

const requireUnique = (values, label) => {
  if (new Set(values).size !== values.length) {
    throw new Error(`fixture suite ${label} must be unique`);
  }
};

const scanCorpusAndBuildEntries = async ({
  corpusPath,
  stagingDirectory,
  stagingEnvelopesDirectory,
  prefixEvidence,
  contract,
}) => {
  const normalizedHash = createHash("sha256");
  const fileHash = createHash("sha256");
  const prefixHash = createHash("sha256");
  let prefixBytesRemaining = prefixEvidence?.bytes;
  const entries = [];
  let rows = 0;
  let windowRows = [];
  let windowTransactionIds = new Set();
  let windowHash = createHash("sha256");
  const transactionIdBucketsDirectory = resolve(
    stagingDirectory,
    TRANSACTION_ID_BUCKETS_DIRECTORY,
  );
  const transactionIdTracker =
    await createPhase5TransactionIdDisjointnessTracker(
      transactionIdBucketsDirectory,
    );
  const stream = createReadStream(corpusPath);
  const hashingStream = new Transform({
    transform(chunk, _encoding, callback) {
      const buffer = Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk);
      fileHash.update(buffer);
      if (prefixBytesRemaining !== undefined && prefixBytesRemaining > 0) {
        const prefixChunk = buffer.subarray(
          0,
          Math.min(prefixBytesRemaining, buffer.length),
        );
        prefixHash.update(prefixChunk);
        prefixBytesRemaining -= prefixChunk.length;
      }
      callback(null, buffer);
    },
  });
  const input = createInterface({
    input: stream.pipe(hashingStream),
    crlfDelay: Infinity,
  });
  try {
    for await (const line of input) {
      if (line.length === 0) continue;
      normalizedHash.update(line).update("\n");
      const row = parseCorpusRow(line, rows);
      if (rows < contract.requiredRows) {
        await transactionIdTracker.add({
          rowIndex: rows,
          txHash: row.txHash,
        });
        if (windowTransactionIds.has(row.txHash)) {
          throw new Error(
            `source corpus transaction ${row.txHash} is duplicated in window ${entries.length.toString()} at row ${rows.toString()}`,
          );
        }
        windowTransactionIds.add(row.txHash);
        windowRows.push(row);
        windowHash.update(line).update("\n");
        if (windowRows.length === contract.transactionsPerSample) {
          const sampleIndex = entries.length;
          const built = await buildOperationalDaWindow(windowRows, sampleIndex);
          const envelopePath = `${ENVELOPES_DIRECTORY}/${sampleIndex
            .toString()
            .padStart(3, "0")}.cbor`;
          await writeFile(
            resolve(
              stagingEnvelopesDirectory,
              `${sampleIndex.toString().padStart(3, "0")}.cbor`,
            ),
            built.envelope,
            { flag: "wx" },
          );
          entries.push({
            sampleIndex,
            envelopePath,
            headerHash: built.headerHash,
            envelopeSha256: built.envelopeSha256,
            innerSha256: built.innerSha256,
            transactionSetSha256: built.transactionSetSha256,
            transactionContentSha256: built.transactionContentSha256,
            envelopeBytes: built.envelope.length,
            innerBytes: built.inner.length,
            corpusWindow: {
              startRow: sampleIndex * contract.transactionsPerSample,
              rowCount: contract.transactionsPerSample,
              sha256: windowHash.digest("hex"),
            },
          });
          windowRows = [];
          windowTransactionIds = new Set();
          windowHash = createHash("sha256");
        }
      }
      rows += 1;
    }
  } catch (error) {
    await transactionIdTracker.cleanup();
    throw error;
  }
  if (
    rows < contract.requiredRows ||
    windowRows.length !== 0 ||
    entries.length !== contract.sampleCount
  ) {
    await transactionIdTracker.cleanup();
    throw new Error(
      `source corpus has ${rows.toString()} rows; ${contract.requiredRows.toString()} are required for ${contract.sampleCount.toString()} complete disjoint windows`,
    );
  }
  try {
    if (
      prefixEvidence !== undefined &&
      (prefixBytesRemaining !== 0 ||
        prefixHash.digest("hex") !== prefixEvidence.sha256)
    ) {
      throw new Error(
        "source corpus does not preserve the byte-identical historical base prefix",
      );
    }
    await transactionIdTracker.verify();
  } finally {
    await transactionIdTracker.cleanup();
  }
  requireUnique(
    entries.map((entry) => entry.headerHash),
    "header hashes",
  );
  requireUnique(
    entries.map((entry) => entry.envelopeSha256),
    "envelope hashes",
  );
  requireUnique(
    entries.map((entry) => entry.innerSha256),
    "inner hashes",
  );
  requireUnique(
    entries.map((entry) => entry.transactionSetSha256),
    "transaction-set hashes",
  );
  requireUnique(
    entries.map((entry) => entry.transactionContentSha256),
    "transaction-content hashes",
  );
  const first = entries[0];
  const observedAnchor = {
    corpusPrefixSha256: first.corpusWindow.sha256,
    headerHash: first.headerHash,
    innerSha256: first.innerSha256,
    envelopeSha256: first.envelopeSha256,
    innerBytes: first.innerBytes,
    envelopeBytes: first.envelopeBytes,
  };
  if (!anchorsEqual(contract.anchor, observedAnchor)) {
    throw new Error(
      "fixture suite entry zero does not match the required anchor",
    );
  }
  return {
    rows,
    normalizedSha256: normalizedHash.digest("hex"),
    fileSha256: fileHash.digest("hex"),
    entries,
  };
};

const evidenceOptions = (options) => ({
  sourceCorpusBindingPath: options.sourceCorpusBindingPath,
  sourceCorpusBindingSha256: requireSha256(
    options.sourceCorpusBindingSha256,
    "sourceCorpusBindingSha256",
  ),
  sourceCorpusManifestPath: options.sourceCorpusManifestPath,
  sourceCorpusManifestSha256: requireSha256(
    options.sourceCorpusManifestSha256,
    "sourceCorpusManifestSha256",
  ),
  sourceCorpusGenerationResultPath: options.sourceCorpusGenerationResultPath,
  sourceCorpusGenerationResultSha256: requireSha256(
    options.sourceCorpusGenerationResultSha256,
    "sourceCorpusGenerationResultSha256",
  ),
});

const acquireBuildLock = async (suiteDirectory) => {
  const lockPath = resolve(suiteDirectory, LOCK_NAME);
  const readStartTicks = async (pid) => {
    const processStat = await readFile(`/proc/${pid.toString()}/stat`, "utf8");
    const commandEnd = processStat.lastIndexOf(")");
    const fields = processStat
      .slice(commandEnd + 2)
      .trim()
      .split(/\s+/u);
    const startTicks = fields[19];
    if (!/^\d+$/u.test(startTicks ?? "")) {
      throw new Error(`unable to parse start ticks for pid ${pid.toString()}`);
    }
    return startTicks;
  };
  const bootId = (
    await readFile("/proc/sys/kernel/random/boot_id", "utf8")
  ).trim();
  const identity = {
    schemaVersion: LOCK_SCHEMA,
    pid: process.pid,
    bootId,
    startTicks: await readStartTicks(process.pid),
  };
  let handle;
  let created = false;
  for (let attempt = 0; attempt < 3; attempt += 1) {
    try {
      handle = await open(lockPath, "wx", 0o600);
      created = true;
      await handle.writeFile(`${JSON.stringify(identity)}\n`, "utf8");
      break;
    } catch (error) {
      await handle?.close().catch(() => undefined);
      handle = undefined;
      if (created) {
        await unlink(lockPath).catch(() => undefined);
        created = false;
      }
      if (error?.code !== "EEXIST") {
        throw new Error(
          `unable to acquire fixture-suite build lock: ${error instanceof Error ? error.message : String(error)}`,
        );
      }
      const before = await stat(lockPath);
      let observed;
      try {
        observed = JSON.parse(await readFile(lockPath, "utf8"));
      } catch {
        observed = undefined;
      }
      let active = false;
      if (
        observed?.schemaVersion === LOCK_SCHEMA &&
        Number.isSafeInteger(observed.pid) &&
        observed.pid > 0 &&
        observed.bootId === bootId &&
        typeof observed.startTicks === "string"
      ) {
        try {
          active = (await readStartTicks(observed.pid)) === observed.startTicks;
        } catch (probeError) {
          if (probeError?.code !== "ENOENT") throw probeError;
        }
      }
      if (active) {
        throw new Error(
          `fixture-suite build lock is held by live pid ${observed.pid.toString()}`,
        );
      }
      const after = await stat(lockPath);
      if (
        before.dev === after.dev &&
        before.ino === after.ino &&
        before.size === after.size &&
        before.mtimeMs === after.mtimeMs
      ) {
        await unlink(lockPath);
      }
    }
  }
  if (handle === undefined) {
    throw new Error(
      "unable to acquire fixture-suite build lock after stale-lock recovery",
    );
  }
  return {
    release: async () => {
      await handle.close();
      await unlink(lockPath).catch(() => undefined);
    },
  };
};

const promoteSuite = async ({
  suiteDirectory,
  stagingDirectory,
  stagingEnvelopesDirectory,
}) => {
  const targetEnvelopes = resolve(suiteDirectory, ENVELOPES_DIRECTORY);
  const targetManifest = resolve(suiteDirectory, MANIFEST_NAME);
  const stagingManifest = resolve(stagingDirectory, MANIFEST_NAME);
  let promoted = false;
  try {
    await rename(stagingEnvelopesDirectory, targetEnvelopes);
    promoted = true;
    await link(stagingManifest, targetManifest);
  } catch (error) {
    if (promoted) {
      await rename(targetEnvelopes, stagingEnvelopesDirectory).catch(async () =>
        rm(targetEnvelopes, { recursive: true, force: true }),
      );
    }
    throw error;
  }
};

export const buildPhase5DaFixtureSuite = async (options) => {
  const suiteDirectory = resolve(options.suiteDirectory);
  const contract = resolveFixtureBuildContract(options);
  const corpusPath = resolveContainedPath(
    suiteDirectory,
    options.sourceCorpusPath,
    "sourceCorpusPath",
  );
  rejectReservedInputPath(options.sourceCorpusPath, "sourceCorpusPath");
  for (const [value, label] of [
    [options.sourceCorpusBindingPath, "sourceCorpusBindingPath"],
    [options.sourceCorpusManifestPath, "sourceCorpusManifestPath"],
    [
      options.sourceCorpusGenerationResultPath,
      "sourceCorpusGenerationResultPath",
    ],
  ]) {
    if (contract.formal) {
      resolveContainedPath(suiteDirectory, value, label);
      rejectReservedInputPath(value, label);
    }
  }
  const targetManifest = resolve(suiteDirectory, MANIFEST_NAME);
  const targetEnvelopes = resolve(suiteDirectory, ENVELOPES_DIRECTORY);
  if (await pathExists(targetManifest)) {
    throw new Error(
      "refusing to overwrite an existing committed Phase 5 fixture manifest",
    );
  }
  const lock = await acquireBuildLock(suiteDirectory);
  const stagingDirectory = resolve(
    suiteDirectory,
    `.phase5-da-fixture-stage-${process.pid.toString()}-${randomBytes(8).toString("hex")}`,
  );
  const stagingEnvelopesDirectory = resolve(
    stagingDirectory,
    ENVELOPES_DIRECTORY,
  );
  try {
    if (await pathExists(targetManifest)) {
      throw new Error(
        "refusing to overwrite an existing committed Phase 5 fixture manifest",
      );
    }
    if (await pathExists(targetEnvelopes)) {
      await rm(targetEnvelopes, { recursive: true, force: true });
    }
    await mkdir(stagingEnvelopesDirectory, { recursive: true, mode: 0o700 });
    const evidence = contract.formal ? evidenceOptions(options) : undefined;
    const provenance =
      evidence === undefined
        ? undefined
        : await verifyPhase5DaSourceCorpusEvidence(suiteDirectory, evidence, {
            sampleCount: contract.sampleCount,
            transactionCount: contract.transactionsPerSample,
          });
    const corpus = await scanCorpusAndBuildEntries({
      corpusPath,
      stagingDirectory,
      stagingEnvelopesDirectory,
      ...(provenance?.prefixBytes === undefined
        ? {}
        : {
            prefixEvidence: {
              bytes: provenance.prefixBytes,
              sha256: provenance.prefixSha256,
            },
          }),
      contract,
    });
    if (
      provenance !== undefined &&
      (provenance.corpusRows !== corpus.rows ||
        provenance.corpusFileSha256 !== corpus.fileSha256)
    ) {
      throw new Error(
        "source corpus bytes disagree with the bound provenance evidence",
      );
    }
    const document = {
      schemaVersion: contract.schemaVersion,
      ...(!contract.formal ? { testOnly: true } : {}),
      sampleCount: contract.sampleCount,
      transactionsPerSample: contract.transactionsPerSample,
      sourceCorpusPath: options.sourceCorpusPath,
      sourceCorpusSha256: corpus.normalizedSha256,
      sourceCorpusFileSha256: corpus.fileSha256,
      sourceCorpusRows: corpus.rows,
      ...(provenance === undefined
        ? {}
        : { sourceCorpusEvidenceMode: provenance.evidenceMode }),
      ...(evidence ?? {}),
      anchor: contract.anchor,
      entries: corpus.entries,
    };
    const manifestBytes = Buffer.from(`${JSON.stringify(document, null, 2)}\n`);
    await writeFile(resolve(stagingDirectory, MANIFEST_NAME), manifestBytes, {
      flag: "wx",
      mode: 0o600,
    });
    await promoteSuite({
      suiteDirectory,
      stagingDirectory,
      stagingEnvelopesDirectory,
    });
    return {
      path: targetManifest,
      sha256: sha256(manifestBytes),
      schemaVersion: contract.schemaVersion,
      sampleCount: contract.sampleCount,
      transactionsPerSample: contract.transactionsPerSample,
      sourceCorpusRows: corpus.rows,
    };
  } finally {
    await rm(stagingDirectory, { recursive: true, force: true });
    await lock.release();
  }
};

const argumentValue = (name) => {
  const index = process.argv.indexOf(name);
  if (index < 0 || process.argv[index + 1] === undefined) {
    throw new Error(`missing required ${name}`);
  }
  return process.argv[index + 1];
};

const optionalIntegerArgument = (name) => {
  const index = process.argv.indexOf(name);
  if (index < 0) return undefined;
  const value = Number(process.argv[index + 1]);
  return requirePositiveSafeInteger(value, name);
};

const runCli = async () => {
  const testSampleCount = optionalIntegerArgument("--test-sample-count");
  const testTransactionsPerSample = optionalIntegerArgument(
    "--test-transactions-per-sample",
  );
  const testOnly =
    testSampleCount !== undefined || testTransactionsPerSample !== undefined;
  const testAnchor = testOnly
    ? JSON.parse(
        await readFile(
          resolveContainedPath(
            resolve(argumentValue("--suite-dir")),
            argumentValue("--test-anchor-path"),
            "--test-anchor-path",
          ),
          "utf8",
        ),
      )
    : undefined;
  const result = await buildPhase5DaFixtureSuite({
    suiteDirectory: argumentValue("--suite-dir"),
    sourceCorpusPath: argumentValue("--source-corpus-path"),
    sourceCorpusBindingPath: testOnly
      ? undefined
      : argumentValue("--source-binding-path"),
    sourceCorpusBindingSha256: testOnly
      ? undefined
      : argumentValue("--source-binding-sha256"),
    sourceCorpusManifestPath: testOnly
      ? undefined
      : argumentValue("--source-manifest-path"),
    sourceCorpusManifestSha256: testOnly
      ? undefined
      : argumentValue("--source-manifest-sha256"),
    sourceCorpusGenerationResultPath: testOnly
      ? undefined
      : argumentValue("--source-generation-result-path"),
    sourceCorpusGenerationResultSha256: testOnly
      ? undefined
      : argumentValue("--source-generation-result-sha256"),
    ...(testSampleCount === undefined ? {} : { sampleCount: testSampleCount }),
    ...(testTransactionsPerSample === undefined
      ? {}
      : { transactionsPerSample: testTransactionsPerSample }),
    ...(testOnly
      ? {
          testOnlyGuard: process.env.MIDGARD_PHASE5_FIXTURE_SUITE_TEST_ONLY,
          testAnchor,
        }
      : {}),
  });
  process.stdout.write(`${JSON.stringify(result)}\n`);
};

if (
  process.argv[1] !== undefined &&
  import.meta.url === pathToFileURL(resolve(process.argv[1])).href
) {
  runCli().catch((error) => {
    process.stderr.write(
      `${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exitCode = 1;
  });
}
