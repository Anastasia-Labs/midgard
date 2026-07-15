#!/usr/bin/env node

import { createHash } from "node:crypto";
import { createReadStream } from "node:fs";
import { mkdtemp, open, readFile, rm, stat } from "node:fs/promises";
import { tmpdir } from "node:os";
import {
  basename,
  dirname,
  isAbsolute,
  join,
  relative,
  resolve,
  sep,
} from "node:path";
import { createInterface } from "node:readline";
import { Transform } from "node:stream";
import { pathToFileURL } from "node:url";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec/native";
import { decodeMidgardAddressText } from "@al-ft/midgard-core/codec/address";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { decodeMidgardLedgerTxFromCanonicalCbor } from "@al-ft/midgard-validation/ledger-tx/codec";
import { CML } from "@lucid-evolution/lucid";

import { createPhase5TransactionIdDisjointnessTracker } from "./phase5-transaction-id-disjointness.mjs";
import {
  PHASE1_FORMAL_CHAIN_COUNT,
  PHASE1_FORMAL_CHAIN_DEPTH,
  PHASE1_FORMAL_LIVE_SAMPLE_SIZE,
  PHASE1_FORMAL_ROW_COUNT,
  PHASE1_FORMAL_SAMPLE_ALGORITHM,
} from "./phase1-formal-identity.mjs";

export const PHASE5_DA_DISTRIBUTION_SCHEMA =
  "midgard-phase-5-da-50k-distribution-v1";
export const PHASE5_DA_FIXTURE_SUITE_SCHEMA =
  "midgard-phase-5-da-50k-fixture-suite-v1";
export const PHASE5_DA_SAMPLE_COUNT = 100;
export const PHASE5_DA_TX_COUNT = 50_000;
export const PHASE5_DA_THRESHOLD_P99_LIMIT_MS = 2_000;
export const PHASE5_DA_EXPECTED_NODE_VERSION = "v22.22.2";
export const PHASE5_HISTORICAL_BINDING_SCHEMA =
  "midgard-phase5-historical-corpus-binding-v1";
export const PHASE5_HISTORICAL_MANIFEST_SCHEMA =
  "midgard-stress-corpus-historical-extension-manifest-v1";
export const PHASE5_HISTORICAL_VERIFICATION_SCHEMA =
  "midgard-stress-corpus-historical-extension-verification-v1";
export const PHASE5_HISTORICAL_GENERATION_SCHEMA =
  "midgard-stress-corpus-historical-extension-generation-v1";
export const PHASE5_HISTORICAL_CLAIM_SCOPE =
  "historical-offline-corpus-extension";
export const PHASE5_HISTORICAL_RETAINED_TERMINAL_SET_HASH_ALGORITHM =
  "sha256-chain-id-outref-output-cbor-sha256-lovelace-lines-v1";
export const PHASE5_HISTORICAL_FORMAL_BASE_ANCHOR = Object.freeze({
  corpusSha256:
    "61c53f60e2993bbd09df61510437d2f944a87c00aef135025404e5a4c7ef0e59",
  indexSha256:
    "244747e844fd6320ef8af362d471c35b474e9cafef262082345fb508df52f629",
  manifestSha256:
    "a3cef4073d241671436a6812a5fc69f7baaae0ba4ee259e55e879942509f5f84",
  verificationSha256:
    "eed377f741ad6bb49a2c8cad8fd1b0cb5efec04cdf40a48571a3f875c83c39a6",
  phase1BindingSha256:
    "b2a92b86ace71ad7685e9b253e239ac774468f6526334acb11dfad62c30b28da",
  fanoutReportSha256:
    "a5ae2acac635dc2869efa8c612ae1e24a410d98cb96365e60ccdcf7ffe139723",
});
const PHASE5_HISTORICAL_WALLET_SET_HASH_ALGORITHM =
  "sha256-wallet-id-l2-address-lines-v1";
const PHASE5_HISTORICAL_FUNDING_SET_HASH_ALGORITHM =
  "sha256-wallet-id-outref-output-cbor-sha256-lines-v1";
export const PHASE5_HISTORICAL_COMPATIBILITY = Object.freeze({
  consumerScope: "phase5-da-distribution-only",
  chainRunLayout: "retained-base-runs-then-continuation-runs",
  phase1FormalBindingCompatible: false,
  phase2ValidationCorpusCompatible: false,
});
export const PHASE5_DA_ANCHOR = Object.freeze({
  corpusPrefixSha256:
    "4c08d4c17df63a8e004f4ee3ba24ca92eacbabff8ce273ac98c4be23d396b26e",
  headerHash: "8ffd0001ced7f02bc858def1b3bd6f254a90e1ae908529985e7d7d99",
  innerSha256:
    "0cad493355048c36b85c9d9998863c47b5fe8c012b4de1ae88dd91f7587603d0",
  envelopeSha256:
    "d3601c2595f1ab6af5c99f297c1608d0447fd0147a07bcc277595b357e8b79d6",
  innerBytes: 41_949_577,
  envelopeBytes: 13_681_302,
});

const assert = (condition, message) => {
  if (!condition) throw new Error(`invalid Phase 5 DA report: ${message}`);
};

const isSha256 = (value) =>
  typeof value === "string" && /^[0-9a-f]{64}$/u.test(value);
const isHeaderHash = (value) =>
  typeof value === "string" && /^[0-9a-f]{56}$/u.test(value);
const isFiniteNonNegative = (value) =>
  typeof value === "number" && Number.isFinite(value) && value >= 0;
const closeEnough = (left, right) =>
  Math.abs(left - right) <= Math.max(1e-6, Math.abs(right) * 1e-9);

export const nearestRank = (values, percentile) => {
  assert(values.length > 0, "cannot compute a percentile from no samples");
  const sorted = [...values].sort((left, right) => left - right);
  return sorted[Math.max(0, Math.ceil(percentile * sorted.length) - 1)];
};

export const distributionStats = (values) => ({
  p50Ms: nearestRank(values, 0.5),
  p95Ms: nearestRank(values, 0.95),
  p99Ms: nearestRank(values, 0.99),
  maxMs: Math.max(...values),
});

const verifyStats = (label, declared, values) => {
  assert(
    declared !== null && typeof declared === "object",
    `${label} stats missing`,
  );
  const computed = distributionStats(values);
  for (const key of ["p50Ms", "p95Ms", "p99Ms", "maxMs"]) {
    assert(
      isFiniteNonNegative(declared[key]) &&
        closeEnough(declared[key], computed[key]),
      `${label}.${key} does not match the raw samples`,
    );
  }
  return computed;
};

const requireUnique = (values, label) => {
  assert(new Set(values).size === values.length, `${label} must be unique`);
};

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");

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

export const verifyPhase5DaNativeTransactionIdentity = (
  txHash,
  canonicalCborHex,
  label,
) => {
  assert(
    /^[0-9a-f]{64}$/u.test(txHash ?? "") &&
      /^(?:[0-9a-f]{2})+$/u.test(canonicalCborHex ?? ""),
    `${label} is not canonical transaction evidence`,
  );
  const transactionBytes = Buffer.from(canonicalCborHex, "hex");
  let transaction;
  try {
    transaction = decodeMidgardNativeTxFullFromCanonicalCbor(transactionBytes);
  } catch (error) {
    throw new Error(
      `invalid Phase 5 DA report: ${label} is invalid Midgard native transaction CBOR: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  assert(
    encodeMidgardNativeTxCanonical(transaction).equals(transactionBytes),
    `${label} transaction CBOR is not canonical`,
  );
  assert(
    computeMidgardNativeTxId(transaction).toString("hex") === txHash,
    `${label} transaction ID does not match its Midgard native body`,
  );
  return transaction;
};

const fixtureEntryFields = [
  "sampleIndex",
  "envelopePath",
  "headerHash",
  "envelopeSha256",
  "innerSha256",
  "transactionSetSha256",
  "transactionContentSha256",
  "envelopeBytes",
  "innerBytes",
];

const sameFixtureEntry = (left, right) =>
  fixtureEntryFields.every((key) => left?.[key] === right?.[key]) &&
  left?.corpusWindow?.startRow === right?.corpusWindow?.startRow &&
  left?.corpusWindow?.rowCount === right?.corpusWindow?.rowCount &&
  left?.corpusWindow?.sha256 === right?.corpusWindow?.sha256;

const verifyFixtureEntry = (entry, index) => {
  assert(entry?.sampleIndex === index, `fixture entry ${index} order mismatch`);
  assert(
    typeof entry.envelopePath === "string" &&
      entry.envelopePath.length > 0 &&
      !isAbsolute(entry.envelopePath),
    `fixture entry ${index} envelope path invalid`,
  );
  assert(
    isHeaderHash(entry.headerHash),
    `fixture entry ${index} header missing`,
  );
  for (const key of [
    "envelopeSha256",
    "innerSha256",
    "transactionSetSha256",
    "transactionContentSha256",
  ]) {
    assert(isSha256(entry[key]), `fixture entry ${index} ${key} missing`);
  }
  assert(
    Number.isSafeInteger(entry.envelopeBytes) && entry.envelopeBytes > 0,
    `fixture entry ${index} envelope size invalid`,
  );
  assert(
    Number.isSafeInteger(entry.innerBytes) && entry.innerBytes > 0,
    `fixture entry ${index} inner size invalid`,
  );
  assert(
    entry.corpusWindow?.startRow === index * PHASE5_DA_TX_COUNT &&
      entry.corpusWindow?.rowCount === PHASE5_DA_TX_COUNT &&
      isSha256(entry.corpusWindow?.sha256),
    `fixture entry ${index} corpus window invalid`,
  );
};

export const verifyPhase5DaDistributionReport = (report) => {
  assert(
    report?.schemaVersion === PHASE5_DA_DISTRIBUTION_SCHEMA,
    "wrong schema",
  );
  assert(report.formal === true, "formal must be true");
  assert(
    report.sampleCount === PHASE5_DA_SAMPLE_COUNT,
    "sampleCount must be 100",
  );
  assert(
    report.independentSemanticEnvelopeCount === PHASE5_DA_SAMPLE_COUNT,
    "independentSemanticEnvelopeCount must be 100",
  );

  const fixture = report.fixtureSuite;
  assert(
    fixture?.schemaVersion === PHASE5_DA_FIXTURE_SUITE_SCHEMA,
    "wrong fixture suite schema",
  );
  assert(isSha256(fixture.manifestSha256), "fixture manifest hash missing");
  assert(
    fixture.sourceCorpusEvidenceMode === "phase1-live-binding" ||
      fixture.sourceCorpusEvidenceMode === "historical-offline-extension",
    "source corpus evidence mode missing",
  );
  assert(isSha256(fixture.sourceCorpusSha256), "source corpus hash missing");
  assert(
    isSha256(fixture.sourceCorpusFileSha256),
    "source corpus file hash missing",
  );
  assert(
    isSha256(fixture.sourceCorpusBindingSha256),
    "source corpus binding hash missing",
  );
  assert(
    isSha256(fixture.sourceCorpusManifestSha256),
    "source corpus manifest hash missing",
  );
  assert(
    isSha256(fixture.sourceCorpusGenerationResultSha256),
    "source corpus generation-result hash missing",
  );
  assert(
    fixture.sourceCorpusRows >= 5_000_000,
    "source corpus has fewer than 5,000,000 rows",
  );
  assert(
    fixture.anchor?.corpusPrefixSha256 === PHASE5_DA_ANCHOR.corpusPrefixSha256,
    "anchor corpus prefix changed",
  );
  for (const key of [
    "headerHash",
    "innerSha256",
    "envelopeSha256",
    "innerBytes",
    "envelopeBytes",
  ]) {
    assert(
      fixture.anchor?.[key] === PHASE5_DA_ANCHOR[key],
      `anchor ${key} changed`,
    );
  }
  assert(
    Array.isArray(fixture.entries) &&
      fixture.entries.length === PHASE5_DA_SAMPLE_COUNT,
    "fixture suite must retain exactly 100 entry identities",
  );
  fixture.entries.forEach(verifyFixtureEntry);
  for (const [values, label] of [
    [fixture.entries.map((entry) => entry.headerHash), "fixture header hashes"],
    [
      fixture.entries.map((entry) => entry.envelopeSha256),
      "fixture envelope hashes",
    ],
    [fixture.entries.map((entry) => entry.innerSha256), "fixture inner hashes"],
    [
      fixture.entries.map((entry) => entry.transactionSetSha256),
      "fixture transaction-set hashes",
    ],
    [
      fixture.entries.map((entry) => entry.transactionContentSha256),
      "fixture transaction-content hashes",
    ],
  ]) {
    requireUnique(values, label);
  }
  assert(
    fixture.entries[0].headerHash === PHASE5_DA_ANCHOR.headerHash &&
      fixture.entries[0].envelopeSha256 === PHASE5_DA_ANCHOR.envelopeSha256 &&
      fixture.entries[0].innerSha256 === PHASE5_DA_ANCHOR.innerSha256 &&
      fixture.entries[0].envelopeBytes === PHASE5_DA_ANCHOR.envelopeBytes &&
      fixture.entries[0].innerBytes === PHASE5_DA_ANCHOR.innerBytes &&
      fixture.entries[0].corpusWindow.sha256 ===
        PHASE5_DA_ANCHOR.corpusPrefixSha256,
    "fixture entry zero is not the checked anchor",
  );

  const runtime = report.runtime;
  assert(
    runtime?.nodeVersion === PHASE5_DA_EXPECTED_NODE_VERSION,
    "wrong Node runtime version",
  );
  assert(runtime.platform === "linux", "formal gate must run on Linux");
  assert(
    typeof runtime.architecture === "string" && runtime.architecture.length > 0,
    "runtime architecture missing",
  );
  assert(
    Number.isSafeInteger(runtime.cpuCount) && runtime.cpuCount > 0,
    "runtime CPU count missing",
  );
  assert(
    typeof runtime.cpuModel === "string" && runtime.cpuModel.length > 0,
    "runtime CPU model missing",
  );
  assert(
    Number.isSafeInteger(runtime.totalMemoryBytes) &&
      runtime.totalMemoryBytes > 0,
    "runtime memory identity missing",
  );
  assert(
    typeof runtime.expectedImageReference === "string" &&
      runtime.expectedImageReference.length > 0,
    "expected image reference missing",
  );
  assert(
    isSha256(runtime.expectedImageId),
    "expected immutable image ID missing",
  );
  assert(
    runtime.actualImageReference === runtime.expectedImageReference,
    "running image reference differs from expected",
  );
  assert(
    runtime.actualImageId === runtime.expectedImageId,
    "running immutable image ID differs from expected",
  );
  assert(
    typeof runtime.containerId === "string" && runtime.containerId.length >= 12,
    "container ID missing",
  );
  assert(
    typeof runtime.observedHostname === "string" &&
      runtime.containerId.startsWith(runtime.observedHostname) &&
      runtime.configuredHostname === runtime.observedHostname,
    "container self-identity binding missing",
  );
  assert(runtime.cpusetCpus === "28-31", "benchmark CPU affinity changed");
  assert(
    runtime.memoryLimitBytes === 12 * 1024 * 1024 * 1024,
    "benchmark memory limit changed",
  );

  const config = report.config;
  assert(config?.committeePeers === 3, "committeePeers must be 3");
  assert(config.threshold === 2, "threshold must be 2");
  assert(
    config.transactionCountPerEnvelope === PHASE5_DA_TX_COUNT,
    "transaction count must be 50,000",
  );
  assert(config.payloadSchemaVersion === 3, "payload schema must be V3");
  assert(
    config.transportProtocolVersion === 1,
    "transport protocol must remain V1",
  );
  assert(
    config.deploymentFingerprint === "b6".repeat(32),
    "distribution deployment fingerprint changed",
  );
  assert(config.publishConcurrency === 8, "publish concurrency must be 8");
  assert(config.zstdLevel === 3, "zstd level must be 3");
  assert(
    config.timingBoundary ===
      "verified_inner_to_threshold_acceptance_including_zstd",
    "threshold timing boundary excludes compression or includes post-threshold work",
  );
  assert(
    config.producerProcessStarts === 1,
    "producer process must start once",
  );
  assert(
    config.committeeProcessStarts === 3,
    "committee processes must start once each",
  );
  assert(config.transportStarts === 1, "producer transport must start once");
  assert(
    config.perSampleProcessStarts === 0,
    "per-sample process startup is forbidden",
  );
  assert(config.maxPayloadBytes === 67_108_864, "V1 maxPayloadBytes changed");
  assert(
    config.maxInlineResponseBytes === 1_048_576,
    "V1 maxInlineResponseBytes changed",
  );
  assert(config.maxChunkBytes === 1_048_576, "V1 maxChunkBytes changed");
  assert(config.maxStreamsPerPeer === 16, "V1 maxStreamsPerPeer changed");
  assert(config.requestTimeoutMs === 15_000, "V1 requestTimeoutMs changed");

  assert(
    Array.isArray(report.samples) &&
      report.samples.length === PHASE5_DA_SAMPLE_COUNT,
    "exactly 100 raw samples are required",
  );
  const headerHashes = [];
  const envelopeHashes = [];
  const innerHashes = [];
  const transactionSetHashes = [];
  const transactionContentHashes = [];
  const thresholdDurations = [];
  const allPeerDurations = [];
  for (let index = 0; index < report.samples.length; index += 1) {
    const sample = report.samples[index];
    const fixtureEntry = fixture.entries[index];
    assert(
      sample.sampleIndex === index,
      `sample ${index.toString()} index/order mismatch`,
    );
    assert(
      isHeaderHash(sample.headerHash),
      `sample ${index.toString()} header hash missing`,
    );
    assert(
      isSha256(sample.envelopeSha256),
      `sample ${index.toString()} envelope hash missing`,
    );
    assert(
      isSha256(sample.innerSha256),
      `sample ${index.toString()} inner hash missing`,
    );
    assert(
      isSha256(sample.transactionSetSha256),
      `sample ${index.toString()} transaction-set hash missing`,
    );
    assert(
      isSha256(sample.transactionContentSha256),
      `sample ${index.toString()} transaction-content hash missing`,
    );
    assert(
      sample.transactionCount === PHASE5_DA_TX_COUNT,
      `sample ${index.toString()} transaction count changed`,
    );
    assert(
      sample.headerHash === fixtureEntry.headerHash &&
        sample.envelopeSha256 === fixtureEntry.envelopeSha256 &&
        sample.innerSha256 === fixtureEntry.innerSha256 &&
        sample.transactionSetSha256 === fixtureEntry.transactionSetSha256 &&
        sample.transactionContentSha256 ===
          fixtureEntry.transactionContentSha256,
      `sample ${index.toString()} identity does not match the fixture suite`,
    );
    assert(
      sample.acceptedPeers === 3,
      `sample ${index.toString()} did not reach 3/3 acceptance`,
    );
    assert(
      Array.isArray(sample.peerStatuses) &&
        sample.peerStatuses.length === 3 &&
        sample.peerStatuses.every((status) => status === "accepted"),
      `sample ${index.toString()} includes duplicate/rejected/transport traffic`,
    );
    assert(
      isFiniteNonNegative(sample.thresholdDurationMs),
      `sample ${index.toString()} threshold duration invalid`,
    );
    assert(
      isFiniteNonNegative(sample.allPeerDurationMs),
      `sample ${index.toString()} all-peer duration invalid`,
    );
    assert(
      sample.allPeerDurationMs >= sample.thresholdDurationMs,
      `sample ${index.toString()} all-peer duration precedes threshold`,
    );
    assert(
      Number.isSafeInteger(sample.producerRssBeforeBytes) &&
        sample.producerRssBeforeBytes > 0 &&
        Number.isSafeInteger(sample.producerRssAfterBytes) &&
        sample.producerRssAfterBytes > 0 &&
        Number.isSafeInteger(sample.producerPeakRssBytes) &&
        sample.producerPeakRssBytes > 0,
      `sample ${index.toString()} producer resource evidence missing`,
    );
    headerHashes.push(sample.headerHash);
    envelopeHashes.push(sample.envelopeSha256);
    innerHashes.push(sample.innerSha256);
    transactionSetHashes.push(sample.transactionSetSha256);
    transactionContentHashes.push(sample.transactionContentSha256);
    thresholdDurations.push(sample.thresholdDurationMs);
    allPeerDurations.push(sample.allPeerDurationMs);
  }
  requireUnique(headerHashes, "sample header hashes");
  requireUnique(envelopeHashes, "sample envelope hashes");
  requireUnique(innerHashes, "sample inner hashes");
  requireUnique(transactionSetHashes, "sample transaction-set hashes");
  requireUnique(transactionContentHashes, "sample transaction-content hashes");
  assert(
    report.samples[0].headerHash === PHASE5_DA_ANCHOR.headerHash,
    "sample zero is not the checked anchor envelope",
  );
  assert(
    report.samples[0].envelopeSha256 === PHASE5_DA_ANCHOR.envelopeSha256,
    "sample zero anchor envelope hash changed",
  );
  assert(
    report.samples[0].innerSha256 === PHASE5_DA_ANCHOR.innerSha256,
    "sample zero anchor inner hash changed",
  );

  const threshold = verifyStats(
    "threshold",
    report.statistics?.threshold,
    thresholdDurations,
  );
  verifyStats("allPeer", report.statistics?.allPeer, allPeerDurations);

  assert(
    Array.isArray(report.committeeProcesses) &&
      report.committeeProcesses.length === 3,
    "three committee-process summaries required",
  );
  const pids = [];
  for (let index = 0; index < report.committeeProcesses.length; index += 1) {
    const peer = report.committeeProcesses[index];
    assert(
      peer.peerIndex === index,
      `committee process ${index.toString()} index mismatch`,
    );
    assert(
      Number.isSafeInteger(peer.pid) && peer.pid > 0,
      `committee process ${index.toString()} PID invalid`,
    );
    assert(
      peer.requestCount === PHASE5_DA_SAMPLE_COUNT,
      `committee process ${index.toString()} did not handle 100 requests`,
    );
    assert(
      Number.isSafeInteger(peer.peakRssBytes) && peer.peakRssBytes > 0,
      `committee process ${index.toString()} peak RSS missing`,
    );
    assert(
      peer.maxAdmissionPeakActive === 1,
      `committee process ${index.toString()} admission exceeded one active submit`,
    );
    assert(
      Array.isArray(peer.samples) &&
        peer.samples.length === PHASE5_DA_SAMPLE_COUNT,
      `committee process ${index.toString()} raw resource samples missing`,
    );
    for (const sample of peer.samples) {
      assert(
        sample.peerIndex === index && sample.pid === peer.pid,
        `committee process ${index.toString()} raw sample identity mismatch`,
      );
      assert(
        sample.outcome === "completed",
        `committee process ${index.toString()} handler did not complete`,
      );
      assert(
        isFiniteNonNegative(sample.durationMs) &&
          Number.isSafeInteger(sample.rssBeforeBytes) &&
          sample.rssBeforeBytes > 0 &&
          Number.isSafeInteger(sample.rssAfterBytes) &&
          sample.rssAfterBytes > 0 &&
          Number.isSafeInteger(sample.peakRssBytes) &&
          sample.peakRssBytes > 0,
        `committee process ${index.toString()} raw resource sample invalid`,
      );
      assert(
        sample.admissionPeakActive === 1,
        `committee process ${index.toString()} raw admission evidence changed`,
      );
    }
    assert(
      peer.peakRssBytes ===
        Math.max(...peer.samples.map((sample) => sample.peakRssBytes)),
      `committee process ${index.toString()} peak RSS summary mismatch`,
    );
    pids.push(peer.pid);
  }
  requireUnique(pids, "committee process PIDs");
  assert(
    Number.isSafeInteger(report.resources?.producerPeakRssBytes) &&
      report.resources.producerPeakRssBytes > 0,
    "producer peak RSS missing",
  );

  assert(
    report.verdict?.thresholdP99LimitMs === PHASE5_DA_THRESHOLD_P99_LIMIT_MS,
    "p99 target changed",
  );
  assert(
    closeEnough(report.verdict.thresholdP99Ms, threshold.p99Ms),
    "verdict p99 differs from raw samples",
  );
  const passed = threshold.p99Ms <= PHASE5_DA_THRESHOLD_P99_LIMIT_MS;
  assert(
    report.verdict.passed === passed,
    "declared verdict differs from recomputed p99",
  );
  return { passed, thresholdP99Ms: threshold.p99Ms };
};

const resolveContainedPath = (base, value, label) => {
  assert(
    typeof value === "string" && value.length > 0 && !isAbsolute(value),
    `${label} must be a non-empty relative path`,
  );
  const resolved = resolve(base, value);
  const fromBase = relative(base, resolved);
  assert(
    fromBase !== ".." &&
      !fromBase.startsWith(`..${sep}`) &&
      !isAbsolute(fromBase),
    `${label} escapes the fixture suite`,
  );
  return resolved;
};

const fixtureEntryIdentity = (entry) => ({
  sampleIndex: entry.sampleIndex,
  envelopePath: entry.envelopePath,
  headerHash: entry.headerHash,
  envelopeSha256: entry.envelopeSha256,
  innerSha256: entry.innerSha256,
  transactionSetSha256: entry.transactionSetSha256,
  transactionContentSha256: entry.transactionContentSha256,
  envelopeBytes: entry.envelopeBytes,
  innerBytes: entry.innerBytes,
  corpusWindow: {
    startRow: entry.corpusWindow.startRow,
    rowCount: entry.corpusWindow.rowCount,
    sha256: entry.corpusWindow.sha256,
  },
});

const sameJson = (left, right) =>
  JSON.stringify(left) === JSON.stringify(right);

const sha256File = async (path) =>
  new Promise((resolveHash, reject) => {
    const digest = createHash("sha256");
    const input = createReadStream(path);
    input.on("data", (chunk) => digest.update(chunk));
    input.on("error", reject);
    input.on("end", () => resolveHash(digest.digest("hex")));
  });

const evidencePath = (base, value, label) => {
  assert(typeof value === "string" && value.length > 0, `${label} missing`);
  return isAbsolute(value)
    ? resolve(value)
    : resolveContainedPath(base, value, label);
};

const assertHistoricalMarker = (value, schemaVersion, label) => {
  assert(
    value?.schemaVersion === schemaVersion &&
      value.claimScope === PHASE5_HISTORICAL_CLAIM_SCOPE &&
      value.freshLiveClaim === false &&
      sameJson(value.compatibility, PHASE5_HISTORICAL_COMPATIBILITY),
    `${label} is not explicitly Phase-5-only historical evidence`,
  );
};

const assertHistoricalSchedule = (schedule, requiredRows) => {
  assert(
    schedule?.algorithm === "balanced-prefix-preserving-chain-depth-v1" &&
      Number.isSafeInteger(schedule.baseChainCount) &&
      schedule.baseChainCount > 0 &&
      Number.isSafeInteger(schedule.baseDepth) &&
      schedule.baseDepth > 0 &&
      Number.isSafeInteger(schedule.baseRowCount) &&
      Number.isSafeInteger(schedule.targetRowCount) &&
      Number.isSafeInteger(schedule.extensionRowCount) &&
      schedule.targetRowCount >= requiredRows &&
      schedule.baseRowCount === schedule.baseChainCount * schedule.baseDepth &&
      schedule.extensionRowCount ===
        schedule.targetRowCount - schedule.baseRowCount &&
      schedule.extensionRowCount >= schedule.baseChainCount &&
      Array.isArray(schedule.entries) &&
      schedule.entries.length === schedule.baseChainCount,
    "historical extension schedule shape is invalid",
  );
  const uniformExtensionDepth = Math.floor(
    schedule.extensionRowCount / schedule.baseChainCount,
  );
  const remainder = schedule.extensionRowCount % schedule.baseChainCount;
  const chainIds = new Set();
  const histogram = new Map();
  const digest = createHash("sha256");
  for (const [index, entry] of schedule.entries.entries()) {
    const extensionRows = uniformExtensionDepth + (index < remainder ? 1 : 0);
    assert(
      typeof entry?.chainId === "string" &&
        entry.chainId.length > 0 &&
        !chainIds.has(entry.chainId) &&
        entry.baseDepth === schedule.baseDepth &&
        entry.extensionRows === extensionRows &&
        entry.targetDepth === schedule.baseDepth + extensionRows,
      `historical extension schedule entry ${index} is invalid`,
    );
    chainIds.add(entry.chainId);
    histogram.set(
      entry.targetDepth,
      (histogram.get(entry.targetDepth) ?? 0) + 1,
    );
    if (index > 0) digest.update("\n");
    digest.update(
      `${entry.chainId}|${entry.baseDepth.toString()}|${entry.targetDepth.toString()}|${entry.extensionRows.toString()}`,
    );
  }
  const expectedHistogram = [...histogram.entries()]
    .sort(([left], [right]) => left - right)
    .map(([targetDepth, chainCount]) => ({ targetDepth, chainCount }));
  assert(
    schedule.minimumTargetDepth === expectedHistogram[0]?.targetDepth &&
      schedule.maximumTargetDepth ===
        expectedHistogram[expectedHistogram.length - 1]?.targetDepth &&
      sameJson(schedule.depthHistogram, expectedHistogram) &&
      schedule.entriesSha256 === digest.digest("hex"),
    "historical extension schedule digest or histogram changed",
  );
};

const readCorpusIndex = async (path, label) => {
  const rows = (await readFile(path, "utf8"))
    .split(/\r?\n/u)
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .map((line, index) => {
      const entry = JSON.parse(line);
      assert(
        typeof entry?.corpusSliceId === "string" &&
          ["fanout", "chain", "mixed"].includes(entry.planShape) &&
          typeof entry.chainId === "string" &&
          Number.isSafeInteger(entry.startByteOffset) &&
          Number.isSafeInteger(entry.endByteOffset) &&
          Number.isSafeInteger(entry.rowCount) &&
          entry.startByteOffset >= 0 &&
          entry.endByteOffset > entry.startByteOffset &&
          entry.rowCount > 0,
        `${label} row ${index + 1} is invalid`,
      );
      return entry;
    });
  for (const [index, entry] of rows.entries()) {
    assert(
      entry.startByteOffset ===
        (index === 0 ? 0 : rows[index - 1].endByteOffset),
      `${label} row ${index + 1} is not byte-contiguous`,
    );
  }
  return rows;
};

const verifyHistoricalFanoutEvidence = (
  fanout,
  walletSetIdentity,
  schedule,
) => {
  assert(
    fanout?.schemaVersion === "midgard-stress-wallet-fanout-v1" &&
      fanout.requestedCount === schedule.baseChainCount &&
      fanout.verifiedWalletCount === schedule.baseChainCount &&
      Array.isArray(fanout.wallets) &&
      fanout.wallets.length === schedule.baseChainCount,
    "historical fanout report does not cover the scheduled wallet set",
  );
  const expectedChainIds = new Set(
    schedule.entries.map((entry) => entry.chainId),
  );
  const records = fanout.wallets.map((entry, index) => {
    const wallet = entry?.wallet;
    const fundingUtxos = wallet?.latestFunding?.fundingUtxos;
    assert(
      wallet?.schemaVersion === "midgard-stress-wallet-v1" &&
        typeof wallet.walletId === "string" &&
        expectedChainIds.has(wallet.walletId) &&
        typeof wallet.l2Address === "string" &&
        /^[0-9a-f]{56}$/u.test(wallet.paymentKeyHash ?? "") &&
        wallet.network === "Preprod" &&
        Array.isArray(fundingUtxos) &&
        fundingUtxos.length > 0 &&
        wallet.latestFunding.verifiedFundingUtxoCount === fundingUtxos.length &&
        entry.verifiedFundingUtxoCount === fundingUtxos.length,
      `historical fanout wallet ${index} is invalid`,
    );
    try {
      const decodedAddress = decodeMidgardAddressText(wallet.l2Address);
      assert(
        decodedAddress.networkId === 0 &&
          decodedAddress.paymentCredential.kind === "PubKey" &&
          decodedAddress.paymentCredential.hash.toString("hex") ===
            wallet.paymentKeyHash,
        `historical fanout wallet ${index} payment key does not own its address`,
      );
    } catch (error) {
      throw new Error(
        `invalid Phase 5 DA report: historical fanout wallet ${index} address is invalid: ${error instanceof Error ? error.message : String(error)}`,
      );
    }
    const funding = fundingUtxos.map((candidate, fundingIndex) => {
      const outref = String(candidate?.outref ?? "")
        .trim()
        .toLowerCase();
      const outputCborHex = String(candidate?.outputCbor ?? "")
        .trim()
        .toLowerCase();
      const outputCbor = Buffer.from(outputCborHex, "hex");
      assert(
        /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(outref) &&
          /^(?:[0-9a-f]{2})+$/u.test(outputCborHex) &&
          outputCbor.toString("hex") === outputCborHex &&
          /^(?:0|[1-9][0-9]*)$/u.test(candidate?.lovelace ?? ""),
        `historical fanout wallet ${index} funding ${fundingIndex} is invalid`,
      );
      let output;
      try {
        output = CML.TransactionOutput.from_cbor_bytes(outputCbor);
      } catch (error) {
        throw new Error(
          `invalid Phase 5 DA report: historical fanout wallet ${index} funding ${fundingIndex} output is invalid: ${error instanceof Error ? error.message : String(error)}`,
        );
      }
      assert(
        output.address().to_bech32() === wallet.l2Address &&
          output.amount().coin().toString(10) === candidate.lovelace,
        `historical fanout wallet ${index} funding ${fundingIndex} address or value changed`,
      );
      return {
        outref,
        outputCbor: Buffer.from(outputCbor),
        outputCborSha256: sha256(outputCbor),
      };
    });
    return {
      walletId: wallet.walletId,
      l2Address: wallet.l2Address,
      paymentKeyHash: wallet.paymentKeyHash,
      funding,
    };
  });
  const ordered = [...records].sort((left, right) =>
    left.walletId.localeCompare(right.walletId),
  );
  assert(
    new Set(ordered.map((record) => record.walletId)).size ===
      schedule.baseChainCount &&
      new Set(ordered.map((record) => record.l2Address)).size ===
        schedule.baseChainCount &&
      new Set(ordered.map((record) => record.paymentKeyHash)).size ===
        schedule.baseChainCount,
    "historical fanout wallet IDs, addresses, and payment keys must be unique",
  );
  const firstFundingOutrefs = new Set(
    ordered.map((record) => record.funding[0].outref),
  );
  assert(
    firstFundingOutrefs.size === schedule.baseChainCount,
    "historical fanout first funding outrefs must be unique per chain",
  );
  const fundingRows = ordered.flatMap((record) =>
    record.funding.map(
      (funding) =>
        `${record.walletId}|${funding.outref}|${funding.outputCborSha256}`,
    ),
  );
  const walletSetSha256 = sha256(
    Buffer.from(
      ordered
        .map((record) => `${record.walletId}|${record.l2Address}`)
        .join("\n"),
    ),
  );
  const fundingSetSha256 = sha256(Buffer.from(fundingRows.join("\n")));
  assert(
    walletSetIdentity?.walletCount === schedule.baseChainCount &&
      walletSetIdentity.fundingRowCount === fundingRows.length &&
      walletSetIdentity.uniqueFirstFundingOutrefCount ===
        firstFundingOutrefs.size &&
      walletSetIdentity.walletSetHashAlgorithm ===
        PHASE5_HISTORICAL_WALLET_SET_HASH_ALGORITHM &&
      walletSetIdentity.walletSetSha256 === walletSetSha256 &&
      walletSetIdentity.fundingSetHashAlgorithm ===
        PHASE5_HISTORICAL_FUNDING_SET_HASH_ALGORITHM &&
      walletSetIdentity.fundingSetSha256 === fundingSetSha256,
    "historical fanout wallet/funding identity does not match the bound identity",
  );
  return new Map(
    ordered.map((record) => [
      record.walletId,
      {
        l2Address: record.l2Address,
        paymentKeyHash: record.paymentKeyHash,
        firstFundingOutref: record.funding[0].outref,
        firstFundingOutputCbor: record.funding[0].outputCbor,
      },
    ]),
  );
};

const readHistoricalPhase1Provenance = async ({
  identity,
  manifestDir,
  baseManifestPath,
}) => {
  assert(
    typeof identity?.path === "string" && isSha256(identity.sha256),
    "historical Phase 1 binding provenance identity is missing",
  );
  const candidates = [
    evidencePath(manifestDir, identity.path, "historical Phase 1 provenance"),
    join(dirname(baseManifestPath), basename(identity.path)),
  ];
  let lastError;
  for (const path of new Set(candidates)) {
    try {
      const bytes = await readFile(path);
      assert(
        sha256(bytes) === identity.sha256,
        "historical Phase 1 provenance bytes changed",
      );
      return JSON.parse(bytes.toString("utf8"));
    } catch (error) {
      lastError = error;
    }
  }
  throw new Error(
    `invalid Phase 5 DA report: historical Phase 1 provenance is unavailable: ${lastError instanceof Error ? lastError.message : String(lastError)}`,
  );
};

const verifyHistoricalPhase1Binding = async ({
  baseBinding,
  baseManifest,
  baseVerification,
  baseIndex,
  fanout,
  baseEvidence,
  schedule,
  formalDistribution,
  manifestDir,
  baseManifestPath,
}) => {
  const usesGenerationResult = baseBinding.generationResult !== undefined;
  const provenanceIdentity = usesGenerationResult
    ? baseBinding.generationResult
    : baseBinding.verifier;
  const allowedKeys = new Set([
    "schemaVersion",
    "deploymentManifestId",
    "nodeImageId",
    "nodeContainerId",
    "walletSetSha256",
    "fundingSetSha256",
    "corpus",
    usesGenerationResult ? "generationResult" : "verifier",
    "livePreflight",
    "harness",
    "stressCorpusEnv",
  ]);
  assert(
    Object.keys(baseBinding).every((key) => allowedKeys.has(key)) &&
      isSha256(baseBinding.deploymentManifestId) &&
      /^sha256:[0-9a-f]{64}$/u.test(baseBinding.nodeImageId ?? "") &&
      /^[0-9a-f]{64}$/u.test(baseBinding.nodeContainerId ?? "") &&
      baseBinding.walletSetSha256 ===
        baseManifest.walletSetIdentity.walletSetSha256 &&
      baseBinding.fundingSetSha256 ===
        baseManifest.walletSetIdentity.fundingSetSha256 &&
      typeof baseBinding.corpus?.path === "string" &&
      typeof baseBinding.corpus?.indexPath === "string" &&
      typeof baseBinding.corpus?.manifestPath === "string" &&
      typeof baseBinding.corpus?.sliceId === "string" &&
      baseBinding.corpus.sliceId.length > 0 &&
      baseBinding.corpus.corpusSha256 === baseEvidence.corpus.sha256 &&
      baseBinding.corpus.indexSha256 === baseEvidence.index.sha256 &&
      baseBinding.corpus.manifestSha256 === baseEvidence.manifest.sha256 &&
      isSha256(baseBinding.harness?.scenarioId) &&
      isSha256(baseBinding.harness?.engineId),
    "historical Phase 1 binding identity contract is incomplete",
  );
  const environment = baseBinding.stressCorpusEnv;
  const expectedEnvironmentKeys = [
    "STRESS_CORPUS_INDEX_PATH",
    "STRESS_CORPUS_MANIFEST_PATH",
    "STRESS_CORPUS_PATH",
    "STRESS_CORPUS_READAHEAD_ROWS",
    "STRESS_CORPUS_SHAPE",
    "STRESS_CORPUS_SLICE_ID",
  ];
  assert(
    typeof environment === "object" &&
      environment !== null &&
      sameJson(Object.keys(environment).sort(), expectedEnvironmentKeys) &&
      expectedEnvironmentKeys.every(
        (key) =>
          typeof environment[key] === "string" && environment[key].length > 0,
      ) &&
      environment.STRESS_CORPUS_PATH === baseBinding.corpus.path &&
      environment.STRESS_CORPUS_INDEX_PATH === baseBinding.corpus.indexPath &&
      environment.STRESS_CORPUS_MANIFEST_PATH ===
        baseBinding.corpus.manifestPath &&
      environment.STRESS_CORPUS_SHAPE === "chain" &&
      environment.STRESS_CORPUS_SLICE_ID === baseBinding.corpus.sliceId &&
      /^[1-9][0-9]*$/u.test(environment.STRESS_CORPUS_READAHEAD_ROWS),
    "historical Phase 1 stress-corpus environment contract is incomplete",
  );
  const preflight = baseBinding.livePreflight;
  const expectedSampleSize = Math.min(
    PHASE1_FORMAL_LIVE_SAMPLE_SIZE,
    schedule.baseChainCount,
  );
  const deterministicSampleIds = [...baseIndex]
    .sort((left, right) => {
      const key = (entry) =>
        createHash("sha256")
          .update(baseEvidence.corpus.sha256)
          .update("\0")
          .update(entry.chainId)
          .update("\0")
          .update(String(entry.startByteOffset))
          .digest("hex");
      return key(left).localeCompare(key(right));
    })
    .slice(0, expectedSampleSize)
    .map((entry) => entry.chainId);
  assert(
    preflight?.algorithm === PHASE1_FORMAL_SAMPLE_ALGORITHM &&
      preflight.sampleSize === expectedSampleSize &&
      Array.isArray(preflight.entries) &&
      preflight.entries.length === expectedSampleSize &&
      new Set(preflight.entries.map((entry) => entry.walletId)).size ===
        expectedSampleSize &&
      sameJson(
        preflight.entries.map((entry) => entry.walletId),
        deterministicSampleIds,
      ),
    "historical Phase 1 live preflight contract is incomplete",
  );
  const fanoutByWalletId = new Map(
    fanout.wallets.map((entry) => [entry.wallet.walletId, entry.wallet]),
  );
  for (const [index, entry] of preflight.entries.entries()) {
    const wallet = fanoutByWalletId.get(entry.walletId);
    const funding = wallet?.latestFunding?.fundingUtxos?.[0];
    assert(
      wallet !== undefined &&
        entry.l2Address === wallet.l2Address &&
        entry.firstInputOutref === funding?.outref.toLowerCase() &&
        entry.outputCborSha256 ===
          sha256(Buffer.from(funding.outputCbor, "hex")),
      `historical Phase 1 live preflight row ${index.toString()} is not bound to the fanout snapshot`,
    );
  }
  assert(
    baseVerification.rebuildSample?.algorithm === preflight.algorithm &&
      baseVerification.rebuildSample.sampleRate ===
        baseManifest.verification?.rebuildSampleRate &&
      baseVerification.rebuildSample.checkedChainCount === expectedSampleSize &&
      baseVerification.rebuildSample.checkedRowCount ===
        expectedSampleSize * schedule.baseDepth &&
      sameJson(
        baseVerification.rebuildSample.sampledChainIds,
        preflight.entries.map((entry) => entry.walletId),
      ) &&
      sameJson(
        baseVerification.rebuildSample.livePreflightEntries,
        preflight.entries,
      ),
    "historical Phase 1 live preflight is not reproduced by the standalone verifier",
  );
  if (formalDistribution) {
    assert(
      schedule.baseChainCount === PHASE1_FORMAL_CHAIN_COUNT &&
        schedule.baseDepth === PHASE1_FORMAL_CHAIN_DEPTH &&
        schedule.baseRowCount === PHASE1_FORMAL_ROW_COUNT &&
        baseIndex.length === PHASE1_FORMAL_CHAIN_COUNT &&
        baseIndex.every(
          (entry) => entry.rowCount === PHASE1_FORMAL_CHAIN_DEPTH,
        ) &&
        baseManifest.targetRateTps === 5_000 &&
        baseManifest.durationMs === 600_000 &&
        baseManifest.warmupCount === 0 &&
        baseManifest.cooldownCount === 0 &&
        baseManifest.safetyFactor === 1.02 &&
        baseManifest.assumedAcceptanceLatencyMs === 819 &&
        baseManifest.maxSubmitTxCborBytes === 32_768 &&
        baseManifest.feeParams?.minFeeA === "10" &&
        baseManifest.feeParams?.minFeeB === "10" &&
        baseManifest.amountTemplate?.lovelace === "1" &&
        baseManifest.fundingSummary?.walletCount ===
          PHASE1_FORMAL_CHAIN_COUNT &&
        baseManifest.fundingSummary?.perWalletFundingLovelace === "11228229" &&
        baseManifest.fundingSummary?.totalFundingLovelace === "45990825984" &&
        baseManifest.verification?.rebuildSampleRate === 0.001 &&
        baseManifest.verification?.rebuildSampleAlgorithm ===
          PHASE1_FORMAL_SAMPLE_ALGORITHM &&
        baseVerification.rebuildSample.sampleRate === 0.001 &&
        baseVerification.rebuildSample.checkedRowCount === 3_740 &&
        sameJson(baseManifest.corpusSliceIds, [baseBinding.corpus.sliceId]) &&
        sameJson(baseManifest.sliceSummary, [
          {
            corpusSliceId: baseBinding.corpus.sliceId,
            walletCount: PHASE1_FORMAL_CHAIN_COUNT,
            rowCount: PHASE1_FORMAL_ROW_COUNT,
          },
        ]) &&
        sameJson(
          {
            corpusSha256: baseEvidence.corpus.sha256,
            indexSha256: baseEvidence.index.sha256,
            manifestSha256: baseEvidence.manifest.sha256,
            verificationSha256: baseEvidence.verification.sha256,
            phase1BindingSha256: baseEvidence.phase1Binding.sha256,
            fanoutReportSha256: baseEvidence.fanoutReport.sha256,
          },
          PHASE5_HISTORICAL_FORMAL_BASE_ANCHOR,
        ),
      "historical formal distribution is not anchored to the retained Phase 1 corpus",
    );
  }
  const provenance = await readHistoricalPhase1Provenance({
    identity: provenanceIdentity,
    manifestDir,
    baseManifestPath,
  });
  const provenanceVerification = usesGenerationResult
    ? provenance.verified
    : provenance;
  const provenanceCorpusIdentity = usesGenerationResult
    ? {
        corpusSha256: provenanceVerification?.corpusSha256,
        indexSha256: provenanceVerification?.indexSha256,
        manifestSha256: baseBinding.corpus.manifestSha256,
      }
    : provenanceVerification?.corpus;
  assert(
    provenance.schemaVersion ===
      (usesGenerationResult
        ? "midgard-stress-corpus-generation-v1"
        : "midgard-stress-corpus-verification-v1") &&
      provenanceCorpusIdentity?.corpusSha256 === baseEvidence.corpus.sha256 &&
      provenanceCorpusIdentity.indexSha256 === baseEvidence.index.sha256 &&
      provenanceCorpusIdentity.manifestSha256 ===
        baseEvidence.manifest.sha256 &&
      provenanceVerification.rowCount === schedule.baseRowCount &&
      provenanceVerification.chainCount === schedule.baseChainCount &&
      sameJson(
        provenanceVerification.walletSetIdentity,
        baseManifest.walletSetIdentity,
      ) &&
      sameJson(
        provenanceVerification.rebuildSample,
        baseVerification.rebuildSample,
      ),
    "historical Phase 1 generation/verifier provenance does not reproduce the bound corpus",
  );
};

const readIndexedTerminalLine = async (file, entry) => {
  const chunkSize = 64 * 1024;
  let cursor = entry.endByteOffset;
  let suffix = Buffer.alloc(0);
  while (cursor > entry.startByteOffset) {
    const length = Math.min(chunkSize, cursor - entry.startByteOffset);
    const offset = cursor - length;
    const chunk = Buffer.allocUnsafe(length);
    const { bytesRead } = await file.read(chunk, 0, length, offset);
    assert(
      bytesRead === length,
      `could not read retained terminal for ${entry.chainId}`,
    );
    suffix = Buffer.concat([chunk, suffix]);
    cursor = offset;
    assert(
      suffix.at(-1) === 0x0a,
      `retained chain ${entry.chainId} is not newline terminated`,
    );
    const previousNewline = suffix.lastIndexOf(0x0a, suffix.length - 2);
    if (previousNewline >= 0 || cursor === entry.startByteOffset) {
      const terminalBytes = suffix.subarray(previousNewline + 1, -1);
      assert(
        terminalBytes.length > 0 && !terminalBytes.includes(0x0a),
        `retained chain ${entry.chainId} terminal boundary is invalid`,
      );
      return terminalBytes.toString("utf8").replace(/\r$/u, "");
    }
  }
  throw new Error(
    `invalid Phase 5 DA report: retained chain ${entry.chainId} has no terminal row`,
  );
};

const deriveRetainedTerminalSetEvidence = async (
  corpusPath,
  index,
  schedule,
) => {
  const file = await open(corpusPath, "r");
  const digest = createHash("sha256");
  let lovelaceTotal = 0n;
  try {
    for (const [position, entry] of index.entries()) {
      const row = JSON.parse(await readIndexedTerminalLine(file, entry));
      assert(
        row?.senderWalletId === schedule.entries[position].chainId &&
          row.senderWalletId === entry.chainId &&
          row.corpusSliceId === entry.corpusSliceId &&
          row.planShape === "chain" &&
          Array.isArray(row.outputOutrefs) &&
          row.outputOutrefs[1] === `${row.txHash}#1`,
        `retained terminal ${position} disagrees with its chain`,
      );
      verifyPhase5DaNativeTransactionIdentity(
        row.txHash,
        row.canonicalCborHex,
        `retained terminal ${position}`,
      );
      const native = decodeMidgardNativeTxFullFromCanonicalCbor(
        Buffer.from(row.canonicalCborHex, "hex"),
      );
      const outputs = decodeMidgardNativeByteListPreimage(
        native.body.outputsPreimageCbor,
        "native.outputs",
      );
      assert(
        outputs[1] !== undefined,
        `retained terminal ${position} has no output 1`,
      );
      const lovelace = CML.TransactionOutput.from_cbor_bytes(outputs[1])
        .amount()
        .coin();
      assert(lovelace > 0n, `retained terminal ${position} has no lovelace`);
      lovelaceTotal += lovelace;
      if (position > 0) digest.update("\n");
      digest.update(
        `${schedule.entries[position].chainId}|${row.txHash}#1|${sha256(outputs[1])}|${lovelace.toString(10)}`,
      );
    }
    return {
      sha256: digest.digest("hex"),
      lovelaceTotal: lovelaceTotal.toString(10),
    };
  } finally {
    await file.close();
  }
};

const verifyHistoricalStressTransferSemantics = ({
  nativeTransaction,
  transactionBytes,
  txHash,
  outputs,
  inputOutputCbor,
  wallet,
  isContinuation,
  fundingModel,
  maxSubmitTxCborBytes,
  label,
}) => {
  let ledgerTransaction;
  try {
    ledgerTransaction =
      decodeMidgardLedgerTxFromCanonicalCbor(transactionBytes);
  } catch (error) {
    throw new Error(
      `invalid Phase 5 DA report: ${label} ledger transaction is invalid: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  const amountLovelace = BigInt(fundingModel.amountLovelacePerRow);
  const minFeeA = BigInt(fundingModel.feeFormula.minFeeA);
  const minFeeB = BigInt(fundingModel.feeFormula.minFeeB);
  const expectedFee = minFeeA * BigInt(transactionBytes.length) + minFeeB;
  assert(
    ledgerTransaction.validity === "TxIsValid" &&
      ledgerTransaction.networkId === 0n &&
      ledgerTransaction.spendInputs.length === 1 &&
      ledgerTransaction.referenceInputs.length === 0 &&
      ledgerTransaction.outputs.length === 2 &&
      ledgerTransaction.requiredObserverHashes.length === 0 &&
      ledgerTransaction.scriptWitnesses.length === 0 &&
      ledgerTransaction.redeemers.length === 0 &&
      ledgerTransaction.mint.assets.length === 0 &&
      ledgerTransaction.auxiliaryDataHash.equals(EMPTY_NULL_ROOT) &&
      ledgerTransaction.scriptIntegrityHash.equals(EMPTY_NULL_ROOT) &&
      ledgerTransaction.validityIntervalStart === undefined &&
      ledgerTransaction.validityIntervalEnd === undefined &&
      transactionBytes.length <= maxSubmitTxCborBytes &&
      ledgerTransaction.fee === expectedFee,
    `${label} is not an exact fee-bound plain Preprod transfer`,
  );
  const requiredSignerHashes = ledgerTransaction.requiredSignerHashes.map(
    (hash) => hash.toString("hex"),
  );
  assert(
    isContinuation
      ? requiredSignerHashes.length === 1 &&
          requiredSignerHashes[0] === wallet.paymentKeyHash
      : requiredSignerHashes.length <= 1 &&
          requiredSignerHashes.every(
            (signer) => signer === wallet.paymentKeyHash,
          ),
    `${label} required signer does not match its bound wallet`,
  );
  assert(
    ledgerTransaction.vkeyWitnesses.length === 1 &&
      ledgerTransaction.witnessKeyHashes.length === 1 &&
      ledgerTransaction.witnessKeyHashes[0].toString("hex") ===
        wallet.paymentKeyHash,
    `${label} is not witnessed solely by its bound wallet`,
  );
  const witnessBytes = decodeMidgardNativeByteListPreimage(
    nativeTransaction.witnessSet.addrTxWitsPreimageCbor,
    "native.addr_tx_wits",
  );
  assert(witnessBytes.length === 1, `${label} must have one address witness`);
  let witness;
  try {
    witness = CML.Vkeywitness.from_cbor_bytes(witnessBytes[0]);
    assert(
      Buffer.from(witness.to_cbor_bytes()).equals(witnessBytes[0]),
      `${label} address witness is not canonical`,
    );
  } catch (error) {
    witness?.free();
    throw new Error(
      `invalid Phase 5 DA report: ${label} address witness is invalid: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  const vkey = witness.vkey();
  const keyHash = vkey.hash();
  const signature = witness.ed25519_signature();
  const publicKey = CML.PublicKey.from_bytes(vkey.to_raw_bytes());
  try {
    assert(
      keyHash.to_hex() === wallet.paymentKeyHash &&
        publicKey.verify(Buffer.from(txHash, "hex"), signature),
      `${label} bound wallet signature is invalid`,
    );
  } finally {
    publicKey.free();
    signature.free();
    keyHash.free();
    vkey.free();
    witness.free();
  }
  let inputOutput;
  let destinationOutput;
  let changeOutput;
  try {
    inputOutput = CML.TransactionOutput.from_cbor_bytes(inputOutputCbor);
    destinationOutput = CML.TransactionOutput.from_cbor_bytes(outputs[0]);
    changeOutput = CML.TransactionOutput.from_cbor_bytes(outputs[1]);
  } catch (error) {
    throw new Error(
      `invalid Phase 5 DA report: ${label} transfer output is invalid: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  const inputAmount = inputOutput.amount();
  const destinationAmount = destinationOutput.amount();
  const changeAmount = changeOutput.amount();
  const inputLovelace = inputAmount.coin();
  const destinationLovelace = destinationAmount.coin();
  const changeLovelace = changeAmount.coin();
  assert(
    !inputAmount.has_multiassets() &&
      !destinationAmount.has_multiassets() &&
      !changeAmount.has_multiassets() &&
      destinationOutput.address().to_bech32() === wallet.l2Address &&
      changeOutput.address().to_bech32() === wallet.l2Address &&
      destinationLovelace === amountLovelace &&
      inputLovelace ===
        destinationLovelace + changeLovelace + ledgerTransaction.fee &&
      ledgerTransaction.outputs.every(
        (output) =>
          output.value.assets.size === 0 &&
          output.datum === undefined &&
          output.scriptRef === undefined,
      ),
    `${label} transfer amount, address, or value conservation changed`,
  );
  return {
    changeOutputCbor: Buffer.from(outputs[1]),
    changeLovelace,
  };
};

const verifyHistoricalCorpusChainEvidence = async ({
  corpusPath,
  extendedIndex,
  schedule,
  walletsByChain,
  fundingModel,
  maxSubmitTxCborBytes,
}) => {
  const input = createInterface({
    input: createReadStream(corpusPath),
    crlfDelay: Infinity,
  });
  const lastByChain = new Map();
  let runIndex = 0;
  let rowInRun = 0;
  let rowCount = 0;
  let byteOffset = 0;
  let checkedBaseTerminalRows = 0;
  let checkedContinuationRows = 0;
  let checkedContinuationBoundaries = 0;
  try {
    for await (const line of input) {
      const lineStartByteOffset = byteOffset;
      byteOffset += Buffer.byteLength(line, "utf8") + 1;
      assert(
        line.trim().length > 0,
        `historical corpus contains an empty row at byte ${lineStartByteOffset.toString()}`,
      );
      const entry = extendedIndex[runIndex];
      assert(entry !== undefined, "historical corpus has rows after its index");
      if (rowInRun === 0) {
        assert(
          lineStartByteOffset === entry.startByteOffset,
          `historical corpus run ${runIndex.toString()} does not start at its indexed byte offset`,
        );
      }
      let row;
      try {
        row = JSON.parse(line);
      } catch (error) {
        throw new Error(
          `invalid Phase 5 DA report: historical corpus row ${rowCount + 1} is not JSON: ${error instanceof Error ? error.message : String(error)}`,
        );
      }
      rowCount += 1;
      const label = `historical corpus row ${rowCount}`;
      assert(
        row?.senderWalletId === entry.chainId &&
          row.corpusSliceId === entry.corpusSliceId &&
          row.planShape === entry.planShape &&
          typeof row.selectedInputOutref === "string" &&
          /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(row.selectedInputOutref) &&
          (row.parentTxHash === null ||
            /^[0-9a-f]{64}$/u.test(row.parentTxHash ?? "")) &&
          isSha256(row.canonicalCborSha256) &&
          Number.isSafeInteger(row.canonicalCborByteLength) &&
          row.canonicalCborByteLength > 0 &&
          Array.isArray(row.outputOutrefs),
        `${label} metadata is invalid or disagrees with its indexed run`,
      );
      const transaction = verifyPhase5DaNativeTransactionIdentity(
        row.txHash,
        row.canonicalCborHex,
        label,
      );
      const transactionBytes = Buffer.from(row.canonicalCborHex, "hex");
      assert(
        row.canonicalCborByteLength === transactionBytes.length &&
          row.canonicalCborSha256 === sha256(transactionBytes),
        `${label} canonical byte identity changed`,
      );
      const spendInputs = decodeMidgardNativeByteListPreimage(
        transaction.body.spendInputsPreimageCbor,
        "native.spend_inputs",
      );
      assert(spendInputs.length === 1, `${label} must spend exactly one input`);
      let nativeInput;
      try {
        nativeInput = CML.TransactionInput.from_cbor_bytes(spendInputs[0]);
      } catch (error) {
        throw new Error(
          `invalid Phase 5 DA report: ${label} native input is invalid: ${error instanceof Error ? error.message : String(error)}`,
        );
      }
      const nativeInputOutref = `${nativeInput.transaction_id().to_hex()}#${nativeInput.index().toString()}`;
      assert(
        nativeInputOutref === row.selectedInputOutref,
        `${label} native input does not match selectedInputOutref`,
      );
      const outputs = decodeMidgardNativeByteListPreimage(
        transaction.body.outputsPreimageCbor,
        "native.outputs",
      );
      const expectedOutputOutrefs = outputs.map(
        (_output, outputIndex) => `${row.txHash}#${outputIndex}`,
      );
      assert(
        outputs[1] !== undefined &&
          row.outputOutrefs.length === expectedOutputOutrefs.length &&
          row.outputOutrefs.every(
            (outref, outputIndex) =>
              outref === expectedOutputOutrefs[outputIndex],
          ),
        `${label} declared outputs do not match its native outputs`,
      );
      const wallet = walletsByChain.get(entry.chainId);
      assert(wallet !== undefined, `${label} has no bound fanout wallet`);
      const previous = lastByChain.get(entry.chainId);
      const inputOutputCbor =
        previous === undefined
          ? wallet.firstFundingOutputCbor
          : previous.changeOutputCbor;
      if (previous === undefined) {
        assert(
          runIndex < schedule.baseChainCount &&
            rowInRun === 0 &&
            row.parentTxHash === null &&
            row.selectedInputOutref === wallet.firstFundingOutref,
          `${label} does not start from its bound first funding UTxO`,
        );
      } else {
        assert(
          row.parentTxHash === previous.txHash &&
            row.selectedInputOutref === previous.changeOutref,
          `${label} does not continue its exact previous chain state`,
        );
        if (runIndex >= schedule.baseChainCount && rowInRun === 0) {
          checkedContinuationBoundaries += 1;
        }
      }
      const transfer = verifyHistoricalStressTransferSemantics({
        nativeTransaction: transaction,
        transactionBytes,
        txHash: row.txHash,
        outputs,
        inputOutputCbor,
        wallet,
        isContinuation: runIndex >= schedule.baseChainCount,
        fundingModel,
        maxSubmitTxCborBytes,
        label,
      });
      lastByChain.set(entry.chainId, {
        txHash: row.txHash,
        changeOutref: row.outputOutrefs[1],
        changeOutputCbor: transfer.changeOutputCbor,
        changeLovelace: transfer.changeLovelace,
      });
      rowInRun += 1;
      if (runIndex >= schedule.baseChainCount) {
        checkedContinuationRows += 1;
      }
      if (rowInRun === entry.rowCount) {
        assert(
          byteOffset === entry.endByteOffset,
          `historical corpus run ${runIndex.toString()} does not end at its indexed byte offset`,
        );
        if (runIndex < schedule.baseChainCount) {
          let terminalOutput;
          try {
            terminalOutput = CML.TransactionOutput.from_cbor_bytes(outputs[1]);
          } catch (error) {
            throw new Error(
              `invalid Phase 5 DA report: ${label} terminal output is invalid: ${error instanceof Error ? error.message : String(error)}`,
            );
          }
          assert(
            terminalOutput.address().to_bech32() === wallet.l2Address,
            `${label} terminal output does not belong to its bound fanout wallet`,
          );
          checkedBaseTerminalRows += 1;
        }
        runIndex += 1;
        rowInRun = 0;
      }
    }
  } finally {
    input.close();
  }
  const minimumTerminalChangeLovelace = BigInt(
    fundingModel.minimumTerminalChangeLovelacePerChain,
  );
  assert(
    rowCount === schedule.targetRowCount &&
      runIndex === extendedIndex.length &&
      rowInRun === 0 &&
      byteOffset === extendedIndex.at(-1)?.endByteOffset &&
      lastByChain.size === schedule.baseChainCount &&
      checkedBaseTerminalRows === schedule.baseChainCount &&
      checkedContinuationRows === schedule.extensionRowCount &&
      checkedContinuationBoundaries === schedule.baseChainCount &&
      [...lastByChain.values()].every(
        (state) => state.changeLovelace >= minimumTerminalChangeLovelace,
      ),
    "historical corpus chain verification did not cover its exact schedule",
  );
  return {
    rowCount,
    checkedBaseTerminalRows,
    checkedContinuationRows,
    checkedContinuationBoundaries,
  };
};

const verifyHistoricalIndexes = async ({
  manifestDir,
  baseEvidence,
  binding,
  schedule,
}) => {
  const baseIndexPath = evidencePath(
    manifestDir,
    baseEvidence.index.path,
    "historical base index path",
  );
  const extendedIndexPath = evidencePath(
    manifestDir,
    binding.corpus?.indexPath,
    "historical extended index path",
  );
  const extendedCorpusPath = evidencePath(
    manifestDir,
    binding.corpus?.path,
    "historical extended corpus path",
  );
  const [baseIndex, extendedIndex] = await Promise.all([
    readCorpusIndex(baseIndexPath, "historical base index"),
    readCorpusIndex(extendedIndexPath, "historical extended index"),
  ]);
  assert(
    baseIndex.length === schedule.baseChainCount &&
      extendedIndex.length === schedule.baseChainCount * 2,
    "historical indexes do not have one base and one continuation run per chain",
  );
  for (let index = 0; index < schedule.baseChainCount; index += 1) {
    const base = baseIndex[index];
    const retained = extendedIndex[index];
    const continuation = extendedIndex[index + schedule.baseChainCount];
    const scheduled = schedule.entries[index];
    assert(
      sameJson(base, retained) &&
        base.chainId === scheduled.chainId &&
        base.rowCount === schedule.baseDepth &&
        base.planShape === "chain" &&
        continuation.chainId === scheduled.chainId &&
        continuation.corpusSliceId === base.corpusSliceId &&
        continuation.planShape === base.planShape &&
        continuation.rowCount === scheduled.extensionRows,
      `historical continuation index entry ${index} disagrees with its schedule`,
    );
  }
  const [baseCorpusStat, extendedCorpusStat] = await Promise.all([
    stat(
      evidencePath(
        manifestDir,
        baseEvidence.corpus.path,
        "historical base corpus path",
      ),
    ),
    stat(extendedCorpusPath),
  ]);
  assert(
    baseIndex.at(-1)?.endByteOffset === baseCorpusStat.size &&
      extendedIndex.at(-1)?.endByteOffset === extendedCorpusStat.size &&
      extendedIndex[schedule.baseChainCount]?.startByteOffset ===
        baseCorpusStat.size,
    "historical indexes do not cover the exact base and extended corpus bytes",
  );
  const baseCorpusPath = evidencePath(
    manifestDir,
    baseEvidence.corpus.path,
    "historical base corpus path",
  );
  return {
    prefixBytes: baseCorpusStat.size,
    extendedCorpusPath,
    baseIndex,
    extendedIndex,
    retainedTerminalSet: await deriveRetainedTerminalSetEvidence(
      baseCorpusPath,
      baseIndex,
      schedule,
    ),
  };
};

const verifyPhase1CorpusEvidenceDocuments = ({
  binding,
  corpusManifest,
  generation,
  manifestSha256,
  generationResultSha256,
  sampleCount,
  transactionCount,
}) => {
  const corpusFileSha256 = corpusManifest.files?.corpus?.sha256;
  const corpusRows = corpusManifest.files?.corpus?.rowCount;
  assert(
    binding.schemaVersion === "midgard-phase1-live-corpus-binding-v2" &&
      generation.schemaVersion === "midgard-stress-corpus-generation-v1" &&
      isSha256(corpusFileSha256) &&
      Number.isSafeInteger(corpusRows) &&
      corpusRows >= sampleCount * transactionCount &&
      binding.corpus?.corpusSha256 === corpusFileSha256 &&
      binding.corpus?.manifestSha256 === manifestSha256 &&
      binding.generationResult?.sha256 === generationResultSha256 &&
      binding.walletSetSha256 ===
        corpusManifest.walletSetIdentity?.walletSetSha256 &&
      binding.fundingSetSha256 ===
        corpusManifest.walletSetIdentity?.fundingSetSha256 &&
      generation.verified?.corpusSha256 === corpusFileSha256 &&
      generation.verified?.indexSha256 ===
        corpusManifest.files?.index?.sha256 &&
      generation.verified?.rowCount === corpusRows &&
      sameJson(
        generation.verified?.walletSetIdentity,
        corpusManifest.walletSetIdentity,
      ),
    "Phase 1 binding, manifest, and generation evidence disagree",
  );
  return {
    corpusFileSha256,
    corpusRows,
    evidenceMode: "phase1-live-binding",
  };
};

const verifyHistoricalCorpusEvidenceDocuments = async ({
  manifestDir,
  binding,
  corpusManifest,
  generation,
  bindingSha256,
  manifestSha256,
  generationResultSha256,
  sampleCount,
  transactionCount,
}) => {
  assertHistoricalMarker(
    binding,
    PHASE5_HISTORICAL_BINDING_SCHEMA,
    "historical binding",
  );
  assertHistoricalMarker(
    corpusManifest,
    PHASE5_HISTORICAL_MANIFEST_SCHEMA,
    "historical manifest",
  );
  assertHistoricalMarker(
    generation,
    PHASE5_HISTORICAL_GENERATION_SCHEMA,
    "historical generation result",
  );
  assertHistoricalSchedule(binding.schedule, sampleCount * transactionCount);
  assert(
    sameJson(binding.schedule, corpusManifest.schedule) &&
      sameJson(binding.schedule, generation.schedule) &&
      sameJson(binding.baseEvidence, corpusManifest.baseEvidence) &&
      sameJson(binding.baseEvidence, generation.baseEvidence) &&
      sameJson(binding.walletSetIdentity, corpusManifest.walletSetIdentity) &&
      sameJson(binding.walletSetIdentity, generation.walletSetIdentity) &&
      sameJson(binding.fundingModel, corpusManifest.fundingModel) &&
      sameJson(binding.fundingModel, generation.fundingModel) &&
      binding.fundingModel?.source ===
        "cryptographically-verified-retained-terminal-output-1-per-wallet" &&
      binding.fundingModel?.freshFundingLovelace === "0" &&
      binding.fundingModel?.retainedBaseOriginalFundingSetSha256 ===
        binding.walletSetIdentity?.fundingSetSha256 &&
      binding.fundingModel?.retainedTerminalSetHashAlgorithm ===
        PHASE5_HISTORICAL_RETAINED_TERMINAL_SET_HASH_ALGORITHM &&
      isSha256(binding.fundingModel?.retainedTerminalSetSha256) &&
      binding.fundingModel?.continuationFundingValueSource ===
        "decoded-canonical-retained-terminal-output-1-cross-checked-against-wallet-and-chain" &&
      /^(?:0|[1-9][0-9]*)$/u.test(
        binding.fundingModel?.retainedTerminalLovelaceTotal ?? "",
      ) &&
      /^(?:0|[1-9][0-9]*)$/u.test(
        binding.fundingModel?.amountLovelacePerRow ?? "",
      ) &&
      binding.fundingModel?.retainedBaseRequestedTransferLovelace ===
        (
          BigInt(binding.fundingModel.amountLovelacePerRow) *
          BigInt(binding.schedule.baseRowCount)
        ).toString(10) &&
      binding.fundingModel?.extensionRequestedTransferLovelace ===
        (
          BigInt(binding.fundingModel.amountLovelacePerRow) *
          BigInt(binding.schedule.extensionRowCount)
        ).toString(10) &&
      binding.fundingModel?.feeFormula?.formula ===
        "minFeeA * canonicalCborByteLength + minFeeB" &&
      /^(?:0|[1-9][0-9]*)$/u.test(
        binding.fundingModel?.feeFormula?.minFeeA ?? "",
      ) &&
      /^(?:0|[1-9][0-9]*)$/u.test(
        binding.fundingModel?.feeFormula?.minFeeB ?? "",
      ) &&
      /^(?:0|[1-9][0-9]*)$/u.test(
        binding.fundingModel?.minimumTerminalChangeLovelacePerChain ?? "",
      ) &&
      binding.fundingModel?.proof ===
        "every continuation started from canonical retained terminal output 1 and built only its scheduled extension rows",
    "historical binding, manifest, and generation metadata disagree",
  );
  const corpusFileSha256 = corpusManifest.files?.corpus?.sha256;
  const corpusIndexSha256 = corpusManifest.files?.index?.sha256;
  const corpusRows = corpusManifest.files?.corpus?.rowCount;
  const verificationIdentity = generation.files?.verification;
  assert(
    isSha256(corpusFileSha256) &&
      isSha256(corpusIndexSha256) &&
      corpusRows === binding.schedule.targetRowCount &&
      corpusRows >= sampleCount * transactionCount &&
      corpusManifest.files?.index?.rowCount ===
        binding.schedule.baseChainCount * 2 &&
      binding.corpus?.corpusSha256 === corpusFileSha256 &&
      binding.corpus?.indexSha256 === corpusIndexSha256 &&
      binding.corpus?.manifestSha256 === manifestSha256 &&
      binding.corpus?.rowCount === corpusRows &&
      binding.corpus?.uniqueChainCount === binding.schedule.baseChainCount &&
      binding.corpus?.indexEntryCount === binding.schedule.baseChainCount * 2 &&
      generation.files?.corpus?.sha256 === corpusFileSha256 &&
      generation.files?.corpus?.rowCount === corpusRows &&
      generation.files?.index?.sha256 === corpusIndexSha256 &&
      generation.files?.index?.rowCount ===
        binding.schedule.baseChainCount * 2 &&
      generation.files?.manifest?.sha256 === manifestSha256 &&
      generation.files?.historicalBinding?.sha256 === bindingSha256 &&
      generation.assembled?.corpusSha256 === corpusFileSha256 &&
      generation.assembled?.indexSha256 === corpusIndexSha256 &&
      generation.assembled?.rowCount === corpusRows &&
      generation.assembled?.indexEntryCount ===
        binding.schedule.baseChainCount * 2 &&
      generation.verification?.corpusSha256 === corpusFileSha256 &&
      generation.verification?.indexSha256 === corpusIndexSha256 &&
      generation.verification?.rowCount === corpusRows &&
      isSha256(verificationIdentity?.sha256) &&
      binding.corpus?.verificationSha256 === verificationIdentity.sha256,
    "historical corpus file identities disagree",
  );
  const baseEvidence = binding.baseEvidence;
  const baseIdentities = [
    [baseEvidence?.corpus, "base corpus"],
    [baseEvidence?.index, "base index"],
    [baseEvidence?.manifest, "base manifest"],
    [baseEvidence?.verification, "base verification"],
    [baseEvidence?.phase1Binding, "base Phase 1 binding"],
    [baseEvidence?.fanoutReport, "base fanout report"],
  ];
  for (const [identity, label] of baseIdentities) {
    assert(
      typeof identity?.path === "string" && isSha256(identity.sha256),
      `historical ${label} identity missing`,
    );
  }
  assert(
    baseEvidence.phase1Binding.schemaVersion ===
      "midgard-phase1-live-corpus-binding-v2" &&
      baseEvidence.fanoutReport.schemaVersion ===
        "midgard-stress-wallet-fanout-v1",
    "historical base evidence schemas changed",
  );
  const verificationPath = evidencePath(
    manifestDir,
    verificationIdentity.path,
    "historical verification path",
  );
  const basePaths = baseIdentities.map(([identity, label]) => [
    evidencePath(manifestDir, identity.path, `historical ${label} path`),
    identity,
    label,
  ]);
  const [verificationBytes, ...baseHashes] = await Promise.all([
    readFile(verificationPath),
    ...basePaths.map(([path]) => sha256File(path)),
  ]);
  assert(
    sha256(verificationBytes) === verificationIdentity.sha256,
    "historical verification bytes changed",
  );
  for (const [index, actualHash] of baseHashes.entries()) {
    assert(
      actualHash === basePaths[index][1].sha256,
      `historical ${basePaths[index][2]} bytes changed`,
    );
  }
  const verification = JSON.parse(verificationBytes.toString("utf8"));
  assertHistoricalMarker(
    verification,
    PHASE5_HISTORICAL_VERIFICATION_SCHEMA,
    "historical verification",
  );
  assert(
    sameJson(verification.baseEvidence, baseEvidence) &&
      sameJson(verification.schedule, binding.schedule) &&
      sameJson(verification.walletSetIdentity, binding.walletSetIdentity) &&
      sameJson(verification.fundingModel, binding.fundingModel) &&
      verification.corpus?.corpusSha256 === corpusFileSha256 &&
      verification.corpus?.indexSha256 === corpusIndexSha256 &&
      verification.corpus?.manifestSha256 === manifestSha256 &&
      verification.checks?.baseGlobalPrefixByteIdentical === true &&
      verification.checks?.everyBaseChainPrefixByteIdentical === true &&
      verification.checks
        ?.everyContinuationMetadataLinkValidByStressCorpusVerifier === true &&
      verification.checks
        ?.everyRetainedTerminalCanonicalNativeIdentityAndDeclaredIoValid ===
        true &&
      verification.checks
        ?.everyContinuationCanonicalNativeIdentityAndDeclaredIoValid === true &&
      verification.checks?.exactTargetRowCount === true &&
      verification.checks?.rowCount === corpusRows &&
      verification.checks?.checkedPrefixRows ===
        binding.schedule.baseRowCount &&
      verification.checks?.checkedExtensionRows ===
        binding.schedule.extensionRowCount &&
      verification.checks?.checkedContinuationCount ===
        binding.schedule.baseChainCount &&
      verification.checks?.checkedCanonicalBaseTerminalRows ===
        binding.schedule.baseChainCount &&
      verification.checks?.checkedCanonicalContinuationRows ===
        binding.schedule.extensionRowCount,
    "historical verification does not prove the bound extension",
  );
  const [baseManifest, baseVerification, baseBinding, fanout] =
    await Promise.all(
      [2, 3, 4, 5].map(async (index) =>
        JSON.parse(await readFile(basePaths[index][0], "utf8")),
      ),
    );
  assert(
    baseManifest.schemaVersion === "midgard-stress-corpus-manifest-v1" &&
      baseVerification.schemaVersion ===
        "midgard-stress-corpus-verification-v1" &&
      baseBinding.schemaVersion === "midgard-phase1-live-corpus-binding-v2" &&
      fanout.schemaVersion === "midgard-stress-wallet-fanout-v1" &&
      baseManifest.files?.corpus?.sha256 === baseEvidence.corpus.sha256 &&
      baseManifest.files?.corpus?.rowCount === binding.schedule.baseRowCount &&
      baseManifest.files?.index?.sha256 === baseEvidence.index.sha256 &&
      baseManifest.files?.index?.rowCount === binding.schedule.baseChainCount &&
      baseManifest.chainCount === binding.schedule.baseChainCount &&
      baseManifest.chainDepth === binding.schedule.baseDepth &&
      baseManifest.corpusShape === "chain" &&
      baseManifest.network === "Preprod" &&
      baseManifest.networkId === "0" &&
      Number.isSafeInteger(baseManifest.maxSubmitTxCborBytes) &&
      baseManifest.maxSubmitTxCborBytes > 0 &&
      baseManifest.amountTemplate?.shape === "self-transfer-change-chain" &&
      /^[1-9][0-9]*$/u.test(baseManifest.amountTemplate?.lovelace ?? "") &&
      /^(?:0|[1-9][0-9]*)$/u.test(baseManifest.feeParams?.minFeeA ?? "") &&
      /^(?:0|[1-9][0-9]*)$/u.test(baseManifest.feeParams?.minFeeB ?? "") &&
      baseVerification.corpus?.corpusSha256 === baseEvidence.corpus.sha256 &&
      baseVerification.corpus?.indexSha256 === baseEvidence.index.sha256 &&
      baseVerification.corpus?.manifestSha256 ===
        baseEvidence.manifest.sha256 &&
      baseVerification.rowCount === binding.schedule.baseRowCount &&
      baseVerification.chainCount === binding.schedule.baseChainCount &&
      baseBinding.corpus?.corpusSha256 === baseEvidence.corpus.sha256 &&
      baseBinding.corpus?.indexSha256 === baseEvidence.index.sha256 &&
      baseBinding.corpus?.manifestSha256 === baseEvidence.manifest.sha256 &&
      baseBinding.walletSetSha256 ===
        binding.walletSetIdentity?.walletSetSha256 &&
      baseBinding.fundingSetSha256 ===
        binding.walletSetIdentity?.fundingSetSha256 &&
      sameJson(baseManifest.walletSetIdentity, binding.walletSetIdentity) &&
      sameJson(baseVerification.walletSetIdentity, binding.walletSetIdentity) &&
      fanout.requestedCount === binding.schedule.baseChainCount &&
      fanout.verifiedWalletCount === binding.schedule.baseChainCount &&
      Array.isArray(fanout.wallets) &&
      fanout.wallets.length === binding.schedule.baseChainCount,
    "historical retained Phase 1 evidence disagrees with the extension",
  );
  const actualExtendedIndexSha256 = await sha256File(
    evidencePath(
      manifestDir,
      binding.corpus?.indexPath,
      "historical extended index path",
    ),
  );
  assert(
    actualExtendedIndexSha256 === corpusIndexSha256,
    "historical extended index bytes changed",
  );
  const indexProof = await verifyHistoricalIndexes({
    manifestDir,
    baseEvidence,
    binding,
    schedule: binding.schedule,
  });
  await verifyHistoricalPhase1Binding({
    baseBinding,
    baseManifest,
    baseVerification,
    baseIndex: indexProof.baseIndex,
    fanout,
    baseEvidence,
    schedule: binding.schedule,
    formalDistribution:
      sampleCount === PHASE5_DA_SAMPLE_COUNT &&
      transactionCount === PHASE5_DA_TX_COUNT,
    manifestDir,
    baseManifestPath: basePaths[2][0],
  });
  const walletsByChain = verifyHistoricalFanoutEvidence(
    fanout,
    binding.walletSetIdentity,
    binding.schedule,
  );
  const authoritativeFundingModel = {
    source: "cryptographically-verified-retained-terminal-output-1-per-wallet",
    retainedBaseOriginalFundingSetSha256:
      binding.walletSetIdentity.fundingSetSha256,
    retainedTerminalSetHashAlgorithm:
      PHASE5_HISTORICAL_RETAINED_TERMINAL_SET_HASH_ALGORITHM,
    retainedTerminalSetSha256: indexProof.retainedTerminalSet.sha256,
    freshFundingLovelace: "0",
    retainedTerminalLovelaceTotal: indexProof.retainedTerminalSet.lovelaceTotal,
    continuationFundingValueSource:
      "decoded-canonical-retained-terminal-output-1-cross-checked-against-wallet-and-chain",
    amountLovelacePerRow: baseManifest.amountTemplate.lovelace,
    retainedBaseRequestedTransferLovelace: (
      BigInt(baseManifest.amountTemplate.lovelace) *
      BigInt(binding.schedule.baseRowCount)
    ).toString(10),
    extensionRequestedTransferLovelace: (
      BigInt(baseManifest.amountTemplate.lovelace) *
      BigInt(binding.schedule.extensionRowCount)
    ).toString(10),
    feeFormula: {
      minFeeA: baseManifest.feeParams.minFeeA,
      minFeeB: baseManifest.feeParams.minFeeB,
      formula: "minFeeA * canonicalCborByteLength + minFeeB",
    },
    minimumTerminalChangeLovelacePerChain: baseManifest.amountTemplate.lovelace,
    proof:
      "every continuation started from canonical retained terminal output 1 and built only its scheduled extension rows",
  };
  assert(
    sameJson(binding.fundingModel, authoritativeFundingModel),
    "historical funding model does not match the immutable base manifest and canonical terminal set",
  );
  await verifyHistoricalCorpusChainEvidence({
    corpusPath: indexProof.extendedCorpusPath,
    extendedIndex: indexProof.extendedIndex,
    schedule: binding.schedule,
    walletsByChain,
    fundingModel: authoritativeFundingModel,
    maxSubmitTxCborBytes: baseManifest.maxSubmitTxCborBytes,
  });
  return {
    corpusFileSha256,
    corpusRows,
    prefixBytes: indexProof.prefixBytes,
    prefixSha256: baseEvidence.corpus.sha256,
    evidenceMode: "historical-offline-extension",
  };
};

export const verifyPhase5DaSourceCorpusEvidence = async (
  manifestDir,
  suite,
  {
    sampleCount = PHASE5_DA_SAMPLE_COUNT,
    transactionCount = PHASE5_DA_TX_COUNT,
  } = {},
) => {
  const bindingPath = resolveContainedPath(
    manifestDir,
    suite.sourceCorpusBindingPath,
    "source corpus binding path",
  );
  const manifestPath = resolveContainedPath(
    manifestDir,
    suite.sourceCorpusManifestPath,
    "source corpus manifest path",
  );
  const generationPath = resolveContainedPath(
    manifestDir,
    suite.sourceCorpusGenerationResultPath,
    "source corpus generation-result path",
  );
  const [bindingBytes, manifestBytes, generationBytes] = await Promise.all([
    readFile(bindingPath),
    readFile(manifestPath),
    readFile(generationPath),
  ]);
  const bindingSha256 = sha256(bindingBytes);
  const manifestSha256 = sha256(manifestBytes);
  const generationResultSha256 = sha256(generationBytes);
  assert(
    bindingSha256 === suite.sourceCorpusBindingSha256 &&
      manifestSha256 === suite.sourceCorpusManifestSha256 &&
      generationResultSha256 === suite.sourceCorpusGenerationResultSha256,
    "source corpus evidence hashes do not match the fixture suite",
  );

  const binding = JSON.parse(bindingBytes.toString("utf8"));
  const corpusManifest = JSON.parse(manifestBytes.toString("utf8"));
  const generation = JSON.parse(generationBytes.toString("utf8"));
  const verified =
    binding.schemaVersion === PHASE5_HISTORICAL_BINDING_SCHEMA
      ? await verifyHistoricalCorpusEvidenceDocuments({
          manifestDir,
          binding,
          corpusManifest,
          generation,
          bindingSha256,
          manifestSha256,
          generationResultSha256,
          sampleCount,
          transactionCount,
        })
      : verifyPhase1CorpusEvidenceDocuments({
          binding,
          corpusManifest,
          generation,
          manifestSha256,
          generationResultSha256,
          sampleCount,
          transactionCount,
        });
  return {
    bindingSha256,
    manifestSha256,
    generationResultSha256,
    ...verified,
  };
};

// Backward-compatible export for callers introduced with the original live
// Phase 1 fixture contract. The implementation now dispatches explicitly by
// the source binding schema and never relabels historical evidence as live.
export const verifyPhase5DaPhase1Evidence = verifyPhase5DaSourceCorpusEvidence;

export const verifyPhase5DaCorpusEvidence = async (
  path,
  entries,
  {
    sampleCount = PHASE5_DA_SAMPLE_COUNT,
    transactionCount = PHASE5_DA_TX_COUNT,
    expectedRows,
    expectedNormalizedSha256,
    expectedFileSha256,
    expectedPrefixBytes,
    expectedPrefixSha256,
  } = {},
) => {
  const normalizedHash = createHash("sha256");
  const fileHash = createHash("sha256");
  const prefixHash = createHash("sha256");
  let prefixBytesRemaining = expectedPrefixBytes;
  const trackerRoot = await mkdtemp(
    join(tmpdir(), "midgard-phase5-transaction-ids-"),
  );
  const transactionIdTracker =
    await createPhase5TransactionIdDisjointnessTracker(
      join(trackerRoot, "buckets"),
    );
  const stream = createReadStream(path);
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
  let rows = 0;
  let activeEntries = [];
  let windowHash = createHash("sha256");
  try {
    for await (const line of input) {
      if (line.length === 0) continue;
      normalizedHash.update(line).update("\n");
      if (rows < sampleCount * transactionCount) {
        const parsed = JSON.parse(line);
        verifyPhase5DaNativeTransactionIdentity(
          parsed.txHash,
          parsed.canonicalCborHex,
          `source corpus row ${rows}`,
        );
        await transactionIdTracker.add({
          rowIndex: rows,
          txHash: parsed.txHash,
        });
        activeEntries.push([parsed.txHash, parsed.canonicalCborHex]);
        windowHash.update(line).update("\n");
        if (activeEntries.length === transactionCount) {
          const entry = entries[Math.floor(rows / transactionCount)];
          assert(
            windowHash.digest("hex") === entry.corpusWindow.sha256 &&
              hashTransactionEntries(activeEntries) ===
                entry.transactionSetSha256 &&
              hashTransactionContents(activeEntries) ===
                entry.transactionContentSha256,
            `source corpus window ${entry.sampleIndex} does not match its fixture entry`,
          );
          activeEntries = [];
          windowHash = createHash("sha256");
        }
      }
      rows += 1;
    }
    await transactionIdTracker.verify();
  } finally {
    await transactionIdTracker.cleanup();
    await rm(trackerRoot, { recursive: true, force: true });
  }
  assert(
    rows >= sampleCount * transactionCount && activeEntries.length === 0,
    "source corpus cannot supply all complete disjoint transaction windows",
  );
  const identity = {
    rows,
    normalizedSha256: normalizedHash.digest("hex"),
    fileSha256: fileHash.digest("hex"),
    ...(expectedPrefixBytes === undefined
      ? {}
      : {
          prefixBytes: expectedPrefixBytes,
          prefixSha256: prefixHash.digest("hex"),
        }),
  };
  assert(
    (expectedRows === undefined || identity.rows === expectedRows) &&
      (expectedNormalizedSha256 === undefined ||
        identity.normalizedSha256 === expectedNormalizedSha256) &&
      (expectedFileSha256 === undefined ||
        identity.fileSha256 === expectedFileSha256) &&
      (expectedPrefixBytes === undefined ||
        (Number.isSafeInteger(expectedPrefixBytes) &&
          expectedPrefixBytes > 0 &&
          prefixBytesRemaining === 0 &&
          isSha256(expectedPrefixSha256) &&
          identity.prefixSha256 === expectedPrefixSha256)),
    "source corpus bytes disagree with the declared identity",
  );
  return identity;
};

export const verifyPhase5DaEnvelopeEvidence = async (
  manifestDir,
  entry,
  { transactionCount = PHASE5_DA_TX_COUNT } = {},
) => {
  const envelopePath = resolveContainedPath(
    manifestDir,
    entry.envelopePath,
    `fixture entry ${entry.sampleIndex} envelope path`,
  );
  const envelope = await readFile(envelopePath);
  assert(
    envelope.length === entry.envelopeBytes &&
      envelope.length <= 67_108_864 &&
      sha256(envelope) === entry.envelopeSha256,
    `fixture entry ${entry.sampleIndex} envelope bytes changed`,
  );
  const unwrapped = await unwrapDaPayload(envelope, {
    maxPayloadBytes: 67_108_864,
    schemaVersion: 3,
  });
  assert(
    unwrapped.innerBytes.length === entry.innerBytes &&
      sha256(unwrapped.innerBytes) === entry.innerSha256,
    `fixture entry ${entry.sampleIndex} inner payload changed`,
  );
  const payload = SDK.decodeDaPayloadV2Canonical(unwrapped.innerBytes);
  const transactions = payload.block_body.transactions;
  for (const [index, [txHash, canonicalCborHex]] of transactions.entries()) {
    verifyPhase5DaNativeTransactionIdentity(
      txHash,
      canonicalCborHex,
      `fixture entry ${entry.sampleIndex} transaction ${index}`,
    );
  }
  assert(
    payload.block_body.header_hash === entry.headerHash &&
      transactions.length === transactionCount &&
      payload.block_body.counts.l2TransactionCount ===
        BigInt(transactionCount) &&
      payload.block_body.utxos.length === transactionCount * 2 &&
      payload.block_body.transition_trace.length === transactionCount &&
      payload.block_body.event_to_step.length === transactionCount &&
      hashTransactionEntries(transactions) === entry.transactionSetSha256 &&
      hashTransactionContents(transactions) === entry.transactionContentSha256,
    `fixture entry ${entry.sampleIndex} envelope does not contain its declared transaction window`,
  );
};

export const loadPhase5DaDistributionEvidence = async (fixtureSuitePath) => {
  assert(
    process.version === PHASE5_DA_EXPECTED_NODE_VERSION,
    `evidence verification requires ${PHASE5_DA_EXPECTED_NODE_VERSION}`,
  );
  const absoluteManifestPath = resolve(fixtureSuitePath);
  const manifestDir = dirname(absoluteManifestPath);
  const manifestBytes = await readFile(absoluteManifestPath);
  const suite = JSON.parse(manifestBytes.toString("utf8"));
  assert(
    suite.schemaVersion === PHASE5_DA_FIXTURE_SUITE_SCHEMA &&
      suite.sampleCount === PHASE5_DA_SAMPLE_COUNT &&
      suite.transactionsPerSample === PHASE5_DA_TX_COUNT &&
      suite.sourceCorpusRows >= PHASE5_DA_SAMPLE_COUNT * PHASE5_DA_TX_COUNT &&
      Array.isArray(suite.entries) &&
      suite.entries.length === PHASE5_DA_SAMPLE_COUNT,
    "fixture suite does not contain the exact formal cardinality",
  );
  const entries = suite.entries.map((entry, index) => {
    verifyFixtureEntry(entry, index);
    return fixtureEntryIdentity(entry);
  });
  for (const [values, label] of [
    [entries.map((entry) => entry.headerHash), "fixture header hashes"],
    [entries.map((entry) => entry.envelopeSha256), "fixture envelope hashes"],
    [entries.map((entry) => entry.innerSha256), "fixture inner hashes"],
    [
      entries.map((entry) => entry.transactionSetSha256),
      "fixture transaction-set hashes",
    ],
    [
      entries.map((entry) => entry.transactionContentSha256),
      "fixture transaction-content hashes",
    ],
  ]) {
    requireUnique(values, label);
  }
  assert(
    Object.entries(PHASE5_DA_ANCHOR).every(
      ([key, value]) => suite.anchor?.[key] === value,
    ) &&
      entries[0].headerHash === PHASE5_DA_ANCHOR.headerHash &&
      entries[0].envelopeSha256 === PHASE5_DA_ANCHOR.envelopeSha256 &&
      entries[0].innerSha256 === PHASE5_DA_ANCHOR.innerSha256 &&
      entries[0].envelopeBytes === PHASE5_DA_ANCHOR.envelopeBytes &&
      entries[0].innerBytes === PHASE5_DA_ANCHOR.innerBytes &&
      entries[0].corpusWindow.sha256 === PHASE5_DA_ANCHOR.corpusPrefixSha256,
    "fixture suite does not retain the checked anchor",
  );

  const provenance = await verifyPhase5DaSourceCorpusEvidence(
    manifestDir,
    suite,
  );
  assert(
    suite.sourceCorpusEvidenceMode === provenance.evidenceMode,
    "fixture suite source corpus evidence mode is false",
  );
  const corpusPath = resolveContainedPath(
    manifestDir,
    suite.sourceCorpusPath,
    "source corpus path",
  );
  const corpus = await verifyPhase5DaCorpusEvidence(corpusPath, entries, {
    expectedRows: suite.sourceCorpusRows,
    expectedNormalizedSha256: suite.sourceCorpusSha256,
    expectedFileSha256: suite.sourceCorpusFileSha256,
    ...(provenance.prefixBytes === undefined
      ? {}
      : {
          expectedPrefixBytes: provenance.prefixBytes,
          expectedPrefixSha256: provenance.prefixSha256,
        }),
  });
  assert(
    corpus.rows === suite.sourceCorpusRows &&
      corpus.normalizedSha256 === suite.sourceCorpusSha256 &&
      corpus.fileSha256 === suite.sourceCorpusFileSha256 &&
      provenance.corpusRows === corpus.rows &&
      provenance.corpusFileSha256 === corpus.fileSha256,
    "source corpus bytes disagree with the fixture suite or Phase 1 evidence",
  );
  for (const entry of entries) {
    await verifyPhase5DaEnvelopeEvidence(manifestDir, entry);
  }
  return {
    schemaVersion: PHASE5_DA_FIXTURE_SUITE_SCHEMA,
    manifestSha256: sha256(manifestBytes),
    sourceCorpusSha256: corpus.normalizedSha256,
    sourceCorpusFileSha256: corpus.fileSha256,
    sourceCorpusRows: corpus.rows,
    sourceCorpusEvidenceMode: provenance.evidenceMode,
    sourceCorpusBindingSha256: provenance.bindingSha256,
    sourceCorpusManifestSha256: provenance.manifestSha256,
    sourceCorpusGenerationResultSha256: provenance.generationResultSha256,
    anchor: { ...PHASE5_DA_ANCHOR },
    entries,
  };
};

export const verifyPhase5DaDistributionEvidenceBinding = (report, evidence) => {
  const declared = report.fixtureSuite;
  for (const key of [
    "schemaVersion",
    "manifestSha256",
    "sourceCorpusSha256",
    "sourceCorpusFileSha256",
    "sourceCorpusRows",
    "sourceCorpusEvidenceMode",
    "sourceCorpusBindingSha256",
    "sourceCorpusManifestSha256",
    "sourceCorpusGenerationResultSha256",
  ]) {
    assert(
      declared?.[key] === evidence?.[key],
      `report fixture evidence ${key} does not match re-hashed evidence`,
    );
  }
  assert(
    Object.entries(PHASE5_DA_ANCHOR).every(
      ([key, value]) => evidence.anchor?.[key] === value,
    ),
    "re-hashed evidence anchor changed",
  );
  assert(
    Array.isArray(evidence.entries) &&
      evidence.entries.length === PHASE5_DA_SAMPLE_COUNT &&
      declared.entries.every((entry, index) =>
        sameFixtureEntry(entry, evidence.entries[index]),
      ),
    "report fixture entries do not match re-hashed evidence",
  );
};

export const verifyPhase5DaDistributionReportWithEvidence = async (
  report,
  fixtureSuitePath,
) => {
  const verdict = verifyPhase5DaDistributionReport(report);
  const evidence = await loadPhase5DaDistributionEvidence(fixtureSuitePath);
  verifyPhase5DaDistributionEvidenceBinding(report, evidence);
  return verdict;
};

const main = async () => {
  const reportPath = process.argv[2];
  const fixtureSuitePath = process.argv[3];
  if (reportPath === undefined || fixtureSuitePath === undefined) {
    throw new Error(
      "usage: verify-phase5-da-50k-distribution-report.mjs <report.json> <fixture-suite.json>",
    );
  }
  const report = JSON.parse(await readFile(reportPath, "utf8"));
  const verdict = await verifyPhase5DaDistributionReportWithEvidence(
    report,
    fixtureSuitePath,
  );
  process.stdout.write(`${JSON.stringify(verdict)}\n`);
  if (!verdict.passed) process.exitCode = 1;
};

if (
  process.argv[1] !== undefined &&
  import.meta.url === pathToFileURL(process.argv[1]).href
) {
  await main();
}
