import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { wrapDaPayloadV3 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";

import {
  distributionStats,
  PHASE5_DA_ANCHOR,
  PHASE5_DA_DISTRIBUTION_SCHEMA,
  PHASE5_DA_FIXTURE_SUITE_SCHEMA,
  verifyPhase5DaCorpusEvidence,
  verifyPhase5DaDistributionEvidenceBinding,
  verifyPhase5DaEnvelopeEvidence,
  verifyPhase5DaNativeTransactionIdentity,
  verifyPhase5DaPhase1Evidence,
  verifyPhase5DaDistributionReport,
} from "./verify-phase5-da-50k-distribution-report.mjs";

const checkedTransaction = JSON.parse(
  await readFile(
    new URL(
      "./fixtures/phase5-checked-first-native-transaction.json",
      import.meta.url,
    ),
    "utf8",
  ),
);

const hash = (index, suffix = "") =>
  (index.toString(16).padStart(8, "0") + suffix.padEnd(56, "a")).slice(0, 64);

const sha256 = (bytes) => createHash("sha256").update(bytes).digest("hex");

const transactionSetSha256 = (entries) => {
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

const transactionContentSha256 = (entries) => {
  const digest = createHash("sha256");
  for (const value of entries.map(([, cbor]) => cbor).sort()) {
    const valueBytes = Buffer.from(value, "hex");
    const length = Buffer.allocUnsafe(4);
    length.writeUInt32BE(valueBytes.length);
    digest.update(length).update(valueBytes);
  }
  return digest.digest("hex");
};

const repeatedBytes = (value, length) =>
  value.toString(16).padStart(2, "0").repeat(length);

const miniPayload = (transaction, headerSeed = 20) => {
  const pair = (seed) => [repeatedBytes(seed, 28), repeatedBytes(seed + 1, 8)];
  const headerHash = repeatedBytes(headerSeed, 28);
  const counts = {
    withdrawalCount: 0n,
    forcedTransactionCount: 0n,
    l2TransactionCount: 1n,
    depositCount: 0n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
  };
  return {
    version: 2n,
    block_body: {
      header_hash: headerHash,
      header: {
        prevUtxosRoot: repeatedBytes(headerSeed + 1, 32),
        utxosRoot: repeatedBytes(headerSeed + 2, 32),
        withdrawalsRoot: repeatedBytes(0, 32),
        forcedTransactionsRoot: repeatedBytes(0, 32),
        transactionsRoot: repeatedBytes(headerSeed + 3, 32),
        depositsRoot: repeatedBytes(0, 32),
        transitionTraceRoot: repeatedBytes(headerSeed + 4, 32),
        eventToStepRoot: repeatedBytes(headerSeed + 5, 32),
        ...counts,
        startTime: 0n,
        endTime: 1n,
        prevHeaderHash: repeatedBytes(headerSeed + 6, 28),
        operatorVkey: repeatedBytes(headerSeed + 7, 28),
        protocolVersion: 1n,
      },
      utxos: [pair(1), pair(3)],
      withdrawals: [],
      forced_transactions: [],
      transactions: [[transaction.txHash, transaction.canonicalCborHex]],
      deposits: [],
      transition_trace: [pair(5)],
      event_to_step: [pair(7)],
      counts,
    },
  };
};

const validReport = () => {
  const samples = Array.from({ length: 100 }, (_, index) => ({
    sampleIndex: index,
    headerHash:
      index === 0 ? PHASE5_DA_ANCHOR.headerHash : hash(index, "b").slice(0, 56),
    envelopeSha256:
      index === 0 ? PHASE5_DA_ANCHOR.envelopeSha256 : hash(index, "c"),
    innerSha256: index === 0 ? PHASE5_DA_ANCHOR.innerSha256 : hash(index, "d"),
    transactionSetSha256: hash(index, "e"),
    transactionContentSha256: hash(index, "f"),
    transactionCount: 50_000,
    acceptedPeers: 3,
    peerStatuses: ["accepted", "accepted", "accepted"],
    thresholdDurationMs: 1_000 + index,
    allPeerDurationMs: 1_100 + index,
    producerRssBeforeBytes: 700_000_000,
    producerRssAfterBytes: 750_000_000,
    producerPeakRssBytes: 900_000_000,
  }));
  return {
    schemaVersion: PHASE5_DA_DISTRIBUTION_SCHEMA,
    formal: true,
    sampleCount: 100,
    independentSemanticEnvelopeCount: 100,
    fixtureSuite: {
      schemaVersion: PHASE5_DA_FIXTURE_SUITE_SCHEMA,
      manifestSha256: "1".repeat(64),
      sourceCorpusSha256: "2".repeat(64),
      sourceCorpusFileSha256: "7".repeat(64),
      sourceCorpusRows: 5_000_000,
      sourceCorpusEvidenceMode: "phase1-live-binding",
      sourceCorpusBindingSha256: "8".repeat(64),
      sourceCorpusManifestSha256: "9".repeat(64),
      sourceCorpusGenerationResultSha256: "a".repeat(64),
      anchor: { ...PHASE5_DA_ANCHOR },
      entries: samples.map((sample, index) => ({
        sampleIndex: index,
        envelopePath: `envelopes/${index.toString().padStart(3, "0")}.cbor`,
        headerHash: sample.headerHash,
        envelopeSha256: sample.envelopeSha256,
        innerSha256: sample.innerSha256,
        transactionSetSha256: sample.transactionSetSha256,
        transactionContentSha256: sample.transactionContentSha256,
        envelopeBytes:
          index === 0 ? PHASE5_DA_ANCHOR.envelopeBytes : 13_000_000 + index,
        innerBytes:
          index === 0 ? PHASE5_DA_ANCHOR.innerBytes : 41_000_000 + index,
        corpusWindow: {
          startRow: index * 50_000,
          rowCount: 50_000,
          sha256:
            index === 0
              ? PHASE5_DA_ANCHOR.corpusPrefixSha256
              : hash(index, "9"),
        },
      })),
    },
    runtime: {
      nodeVersion: "v22.22.2",
      platform: "linux",
      architecture: "x64",
      cpuCount: 8,
      cpuModel: "benchmark-cpu",
      totalMemoryBytes: 16_000_000_000,
      expectedImageReference: "midgard-node:phase5-gate",
      expectedImageId: "3".repeat(64),
      actualImageReference: "midgard-node:phase5-gate",
      actualImageId: "3".repeat(64),
      containerId: "4".repeat(64),
      observedHostname: "4".repeat(12),
      configuredHostname: "4".repeat(12),
      cpusetCpus: "28-31",
      memoryLimitBytes: 12 * 1024 * 1024 * 1024,
    },
    config: {
      committeePeers: 3,
      threshold: 2,
      transactionCountPerEnvelope: 50_000,
      payloadSchemaVersion: 3,
      transportProtocolVersion: 1,
      deploymentFingerprint: "b6".repeat(32),
      publishConcurrency: 8,
      zstdLevel: 3,
      timingBoundary: "verified_inner_to_threshold_acceptance_including_zstd",
      producerProcessStarts: 1,
      committeeProcessStarts: 3,
      transportStarts: 1,
      perSampleProcessStarts: 0,
      maxPayloadBytes: 67_108_864,
      maxInlineResponseBytes: 1_048_576,
      maxChunkBytes: 1_048_576,
      maxStreamsPerPeer: 16,
      requestTimeoutMs: 15_000,
    },
    samples,
    statistics: {
      threshold: distributionStats(
        samples.map((sample) => sample.thresholdDurationMs),
      ),
      allPeer: distributionStats(
        samples.map((sample) => sample.allPeerDurationMs),
      ),
    },
    committeeProcesses: [0, 1, 2].map((peerIndex) => ({
      peerIndex,
      pid: 100 + peerIndex,
      requestCount: 100,
      peakRssBytes: 800_000_000,
      maxAdmissionPeakActive: 1,
      samples: Array.from({ length: 100 }, () => ({
        peerIndex,
        pid: 100 + peerIndex,
        outcome: "completed",
        durationMs: 900,
        rssBeforeBytes: 700_000_000,
        rssAfterBytes: 750_000_000,
        peakRssBytes: 800_000_000,
        admissionPeakActive: 1,
      })),
    })),
    resources: { producerPeakRssBytes: 900_000_000 },
    verdict: {
      thresholdP99LimitMs: 2_000,
      thresholdP99Ms: 1_098,
      passed: true,
    },
  };
};

test("accepts a report whose verdict is recomputed from 100 raw samples", () => {
  assert.deepEqual(verifyPhase5DaDistributionReport(validReport()), {
    passed: true,
    thresholdP99Ms: 1_098,
  });
});

test("accepts the checked operational fixture's first Midgard native transaction", () => {
  assert.equal(
    checkedTransaction.sourceCorpusPrefixSha256,
    PHASE5_DA_ANCHOR.corpusPrefixSha256,
  );
  assert.doesNotThrow(() =>
    verifyPhase5DaNativeTransactionIdentity(
      checkedTransaction.txHash,
      checkedTransaction.canonicalCborHex,
      "checked operational fixture transaction zero",
    ),
  );
});

for (const [name, mutate, pattern] of [
  ["sample padding", (report) => report.samples.pop(), /exactly 100/u],
  [
    "duplicate envelope",
    (report) => {
      report.samples[1].envelopeSha256 = report.samples[0].envelopeSha256;
      report.fixtureSuite.entries[1].envelopeSha256 =
        report.samples[0].envelopeSha256;
    },
    /fixture envelope hashes must be unique/u,
  ],
  [
    "duplicate transaction set",
    (report) => {
      report.samples[1].transactionSetSha256 =
        report.samples[0].transactionSetSha256;
      report.fixtureSuite.entries[1].transactionSetSha256 =
        report.samples[0].transactionSetSha256;
    },
    /fixture transaction-set hashes must be unique/u,
  ],
  [
    "relabeled duplicate transaction contents",
    (report) => {
      report.samples[1].transactionContentSha256 =
        report.samples[0].transactionContentSha256;
      report.fixtureSuite.entries[1].transactionContentSha256 =
        report.samples[0].transactionContentSha256;
    },
    /fixture transaction-content hashes must be unique/u,
  ],
  [
    "duplicate response relabeling",
    (report) => {
      report.samples[3].peerStatuses[0] = "duplicate";
    },
    /duplicate\/rejected/u,
  ],
  [
    "declarative percentile",
    (report) => {
      report.statistics.threshold.p99Ms = 1;
    },
    /does not match the raw samples/u,
  ],
  [
    "forced pass",
    (report) => {
      for (const sample of report.samples) {
        sample.thresholdDurationMs += 2_000;
        sample.allPeerDurationMs += 2_000;
      }
      report.statistics.threshold = distributionStats(
        report.samples.map((sample) => sample.thresholdDurationMs),
      );
      report.statistics.allPeer = distributionStats(
        report.samples.map((sample) => sample.allPeerDurationMs),
      );
      report.verdict.thresholdP99Ms = report.statistics.threshold.p99Ms;
    },
    /declared verdict/u,
  ],
  [
    "startup contamination",
    (report) => {
      report.config.perSampleProcessStarts = 100;
    },
    /per-sample process startup/u,
  ],
  [
    "floating image",
    (report) => {
      report.runtime.actualImageId = "5".repeat(64);
    },
    /immutable image ID/u,
  ],
  [
    "container self-identity spoofing",
    (report) => {
      report.runtime.configuredHostname = "different-host";
    },
    /self-identity binding/u,
  ],
  [
    "wrong runtime",
    (report) => {
      report.runtime.nodeVersion = "v22.22.1";
    },
    /Node runtime/u,
  ],
  [
    "V1 cap drift",
    (report) => {
      report.config.maxPayloadBytes += 1;
    },
    /maxPayloadBytes/u,
  ],
  [
    "wrong threshold timing boundary",
    (report) => {
      report.config.timingBoundary = "publish_call_return";
    },
    /timing boundary/u,
  ],
  [
    "missing Phase 1 provenance",
    (report) => {
      delete report.fixtureSuite.sourceCorpusGenerationResultSha256;
    },
    /generation-result hash/u,
  ],
  [
    "committee resource sample padding",
    (report) => {
      report.committeeProcesses[0].samples.pop();
    },
    /raw resource samples/u,
  ],
  [
    "admission drift",
    (report) => {
      report.committeeProcesses[1].maxAdmissionPeakActive = 2;
    },
    /admission exceeded/u,
  ],
  [
    "anchor substitution",
    (report) => {
      report.fixtureSuite.anchor.envelopeSha256 = "6".repeat(64);
    },
    /anchor envelopeSha256/u,
  ],
]) {
  test(`rejects ${name}`, () => {
    const report = validReport();
    mutate(report);
    assert.throws(() => verifyPhase5DaDistributionReport(report), pattern);
  });
}

test("accepts a truthful target miss while returning a failed verdict", () => {
  const report = validReport();
  for (const sample of report.samples) {
    sample.thresholdDurationMs += 2_000;
    sample.allPeerDurationMs += 2_000;
  }
  report.statistics.threshold = distributionStats(
    report.samples.map((sample) => sample.thresholdDurationMs),
  );
  report.statistics.allPeer = distributionStats(
    report.samples.map((sample) => sample.allPeerDurationMs),
  );
  report.verdict.thresholdP99Ms = report.statistics.threshold.p99Ms;
  report.verdict.passed = false;
  assert.deepEqual(verifyPhase5DaDistributionReport(report), {
    passed: false,
    thresholdP99Ms: 3_098,
  });
});

test("rejects a report sample identity unrelated to its retained fixture entry", () => {
  const report = validReport();
  report.samples[1].transactionContentSha256 = hash(9_001, "f");
  assert.throws(
    () => verifyPhase5DaDistributionReport(report),
    /identity does not match the fixture suite/u,
  );
});

test("rejects report provenance unrelated to independently re-hashed evidence", () => {
  const report = validReport();
  const evidence = structuredClone(report.fixtureSuite);
  evidence.sourceCorpusGenerationResultSha256 = "b".repeat(64);
  assert.throws(
    () => verifyPhase5DaDistributionEvidenceBinding(report, evidence),
    /sourceCorpusGenerationResultSha256 does not match re-hashed evidence/u,
  );
});

test("rejects report fixture entries unrelated to independently decoded evidence", () => {
  const report = validReport();
  const evidence = structuredClone(report.fixtureSuite);
  evidence.entries[42].transactionContentSha256 = hash(9_042, "c");
  assert.throws(
    () => verifyPhase5DaDistributionEvidenceBinding(report, evidence),
    /fixture entries do not match re-hashed evidence/u,
  );
});

test("rejects tampering across retained corpus, envelope, Phase 1, and report files", async () => {
  const root = await mkdtemp(join(tmpdir(), "midgard-phase5-verifier-"));
  try {
    const transaction = checkedTransaction;
    const entries = [[transaction.txHash, transaction.canonicalCborHex]];
    const corpusLine = JSON.stringify(transaction);
    const corpusBytes = Buffer.from(`${corpusLine}\n`);
    const corpusPath = join(root, "corpus.ndjson");
    await writeFile(corpusPath, corpusBytes);

    const innerBytes = SDK.encodeDaPayloadV2(miniPayload(transaction));
    const envelope = await wrapDaPayloadV3(innerBytes, {
      mode: "zstd",
      zstdLevel: 3,
    });
    const envelopePath = join(root, "sample.cbor");
    await writeFile(envelopePath, envelope);
    const entry = {
      sampleIndex: 0,
      envelopePath: "sample.cbor",
      headerHash: repeatedBytes(20, 28),
      envelopeSha256: sha256(envelope),
      innerSha256: sha256(innerBytes),
      transactionSetSha256: transactionSetSha256(entries),
      transactionContentSha256: transactionContentSha256(entries),
      envelopeBytes: envelope.length,
      innerBytes: innerBytes.length,
      corpusWindow: {
        startRow: 0,
        rowCount: 1,
        sha256: sha256(corpusBytes),
      },
    };
    const corpusIdentity = {
      sampleCount: 1,
      transactionCount: 1,
      expectedRows: 1,
      expectedNormalizedSha256: sha256(corpusBytes),
      expectedFileSha256: sha256(corpusBytes),
    };

    await verifyPhase5DaCorpusEvidence(corpusPath, [entry], corpusIdentity);
    await verifyPhase5DaEnvelopeEvidence(root, entry, {
      transactionCount: 1,
    });

    const walletSetIdentity = {
      walletSetSha256: "1".repeat(64),
      fundingSetSha256: "2".repeat(64),
    };
    const corpusManifest = {
      files: {
        corpus: { sha256: sha256(corpusBytes), rowCount: 1 },
        index: { sha256: "3".repeat(64) },
      },
      walletSetIdentity,
    };
    const manifestBytes = Buffer.from(JSON.stringify(corpusManifest));
    const generation = {
      schemaVersion: "midgard-stress-corpus-generation-v1",
      verified: {
        corpusSha256: sha256(corpusBytes),
        indexSha256: "3".repeat(64),
        rowCount: 1,
        walletSetIdentity,
      },
    };
    const generationBytes = Buffer.from(JSON.stringify(generation));
    const binding = {
      schemaVersion: "midgard-phase1-live-corpus-binding-v2",
      corpus: {
        corpusSha256: sha256(corpusBytes),
        manifestSha256: sha256(manifestBytes),
      },
      generationResult: { sha256: sha256(generationBytes) },
      ...walletSetIdentity,
    };
    const bindingBytes = Buffer.from(JSON.stringify(binding));
    await Promise.all([
      writeFile(join(root, "binding.json"), bindingBytes),
      writeFile(join(root, "manifest.json"), manifestBytes),
      writeFile(join(root, "generation.json"), generationBytes),
    ]);
    const suite = {
      sourceCorpusBindingPath: "binding.json",
      sourceCorpusBindingSha256: sha256(bindingBytes),
      sourceCorpusManifestPath: "manifest.json",
      sourceCorpusManifestSha256: sha256(manifestBytes),
      sourceCorpusGenerationResultPath: "generation.json",
      sourceCorpusGenerationResultSha256: sha256(generationBytes),
    };
    await verifyPhase5DaPhase1Evidence(root, suite, {
      sampleCount: 1,
      transactionCount: 1,
    });

    const falseHash = "f".repeat(64);
    const falselyRelabeled = {
      txHash: falseHash,
      canonicalCborHex: transaction.canonicalCborHex,
    };
    const falseCorpusLine = JSON.stringify(falselyRelabeled);
    const falseCorpusBytes = Buffer.from(`${falseCorpusLine}\n`);
    const falseCorpusPath = join(root, "false-hash-corpus.ndjson");
    await writeFile(falseCorpusPath, falseCorpusBytes);
    const falseEntries = [[falseHash, transaction.canonicalCborHex]];
    await assert.rejects(
      verifyPhase5DaCorpusEvidence(
        falseCorpusPath,
        [
          {
            ...entry,
            transactionSetSha256: transactionSetSha256(falseEntries),
            transactionContentSha256: transactionContentSha256(falseEntries),
            corpusWindow: {
              ...entry.corpusWindow,
              sha256: sha256(falseCorpusBytes),
            },
          },
        ],
        {
          ...corpusIdentity,
          expectedNormalizedSha256: sha256(falseCorpusBytes),
          expectedFileSha256: sha256(falseCorpusBytes),
        },
      ),
      /transaction ID does not match its Midgard native body/u,
    );

    const falselyRelabeledInner = SDK.encodeDaPayloadV2(
      miniPayload(falselyRelabeled),
    );
    const falselyRelabeledEnvelope = await wrapDaPayloadV3(
      falselyRelabeledInner,
      { mode: "zstd", zstdLevel: 3 },
    );
    await writeFile(envelopePath, falselyRelabeledEnvelope);
    await assert.rejects(
      verifyPhase5DaEnvelopeEvidence(
        root,
        {
          ...entry,
          envelopeSha256: sha256(falselyRelabeledEnvelope),
          innerSha256: sha256(falselyRelabeledInner),
          transactionSetSha256: transactionSetSha256(falseEntries),
          transactionContentSha256: transactionContentSha256(falseEntries),
          envelopeBytes: falselyRelabeledEnvelope.length,
          innerBytes: falselyRelabeledInner.length,
        },
        { transactionCount: 1 },
      ),
      /transaction ID does not match its Midgard native body/u,
    );
    await writeFile(envelopePath, envelope);

    await writeFile(
      corpusPath,
      Buffer.concat([corpusBytes, Buffer.from("\n")]),
    );
    await assert.rejects(
      verifyPhase5DaCorpusEvidence(corpusPath, [entry], corpusIdentity),
      /source corpus bytes disagree with the declared identity/u,
    );
    await writeFile(corpusPath, corpusBytes);
    await assert.rejects(
      verifyPhase5DaCorpusEvidence(
        corpusPath,
        [
          {
            ...entry,
            corpusWindow: { ...entry.corpusWindow, sha256: "0".repeat(64) },
          },
        ],
        corpusIdentity,
      ),
      /source corpus window 0 does not match/u,
    );

    const damagedEnvelope = Buffer.from(envelope);
    damagedEnvelope[damagedEnvelope.length - 1] ^= 1;
    await writeFile(envelopePath, damagedEnvelope);
    await assert.rejects(
      verifyPhase5DaEnvelopeEvidence(root, entry, { transactionCount: 1 }),
      /envelope bytes changed/u,
    );

    const changedInner = SDK.encodeDaPayloadV2(miniPayload(transaction, 21));
    const rewrapped = await wrapDaPayloadV3(changedInner, {
      mode: "zstd",
      zstdLevel: 3,
    });
    await writeFile(envelopePath, rewrapped);
    await assert.rejects(
      verifyPhase5DaEnvelopeEvidence(
        root,
        {
          ...entry,
          envelopeSha256: sha256(rewrapped),
          envelopeBytes: rewrapped.length,
        },
        { transactionCount: 1 },
      ),
      /inner payload changed/u,
    );
    await writeFile(envelopePath, envelope);

    await writeFile(
      join(root, "generation.json"),
      Buffer.concat([generationBytes, Buffer.from(" ")]),
    );
    await assert.rejects(
      verifyPhase5DaPhase1Evidence(root, suite, {
        sampleCount: 1,
        transactionCount: 1,
      }),
      /source corpus evidence hashes do not match/u,
    );

    const reportPath = join(root, "report.json");
    const tamperedReport = validReport();
    tamperedReport.samples[8].transactionSetSha256 = hash(12_345, "d");
    await writeFile(reportPath, JSON.stringify(tamperedReport));
    const retainedReport = JSON.parse(await readFile(reportPath, "utf8"));
    assert.throws(
      () => verifyPhase5DaDistributionReport(retainedReport),
      /identity does not match the fixture suite/u,
    );
  } finally {
    await rm(root, { recursive: true, force: true });
  }
});
