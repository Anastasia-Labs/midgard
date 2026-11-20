import { type ChildProcess, execFile, spawn } from "node:child_process";
import { createHash } from "node:crypto";
import { createReadStream, existsSync } from "node:fs";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { createServer } from "node:net";
import { cpus, hostname, tmpdir, totalmem } from "node:os";
import { dirname, isAbsolute, join, relative, resolve } from "node:path";
import { createInterface } from "node:readline";
import { Transform } from "node:stream";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec/native";
import {
  unwrapDaPayload,
  wrapDaPayloadV3,
} from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { readDaHardeningConfig } from "@/da/hardening-config.js";
import {
  createDaLibp2pProducerTransport,
  type DaProducerCommitteePeer,
  type DaProducerPublicationManifest,
  publishDaPayloadInsert,
} from "@/da/libp2p-producer.js";
import { DaPayloadsDB } from "@/database/index.js";

import type {
  Libp2pDaPeerConfig,
  Libp2pDaTransportConfig,
} from "../../da-committee-node/src/config.js";
import { loadDaLibp2pIdentity } from "../../da-committee-node/src/da/libp2p/index.js";
// @ts-expect-error Shared executable ESM helper; the method contract is
// narrowed by its use below.
import { createPhase5TransactionIdDisjointnessTracker } from "../scripts/phase5-transaction-id-disjointness.mjs";
// @ts-expect-error The shared formal evidence verifier is an executable ESM
// script; its returned contract is narrowed below before use.
import { verifyPhase5DaSourceCorpusEvidence } from "../scripts/verify-phase5-da-50k-distribution-report.mjs";

const execFileAsync = promisify(execFile);
const packageRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const operationalFixtureDir = join(
  packageRoot,
  "tests/fixtures/da-operational-50k",
);
const envelopePath = join(operationalFixtureDir, "envelope-50000.cbor");
const measurementPath = join(operationalFixtureDir, "measurement.json");
const DEPLOYMENT = "b6".repeat(32);
const FORMAL = process.env.MIDGARD_DA_PHASE5_DISTRIBUTION === "1";
const SAMPLE_COUNT = 100;
const TRANSACTION_COUNT = 50_000;
const EXPECTED_NODE_VERSION = "v22.22.2";
const FIXTURE_SUITE_SCHEMA = "midgard-phase-5-da-50k-fixture-suite-v1";
const REPORT_SCHEMA = "midgard-phase-5-da-50k-distribution-v1";
const P99_LIMIT_MS = 2_000;
const CURRENT_OPERATIONAL_50K = {
  corpusPrefixSha256:
    "4c08d4c17df63a8e004f4ee3ba24ca92eacbabff8ce273ac98c4be23d396b26e",
  headerHash: "8ffd0001ced7f02bc858def1b3bd6f254a90e1ae908529985e7d7d99",
  innerSha256:
    "0cad493355048c36b85c9d9998863c47b5fe8c012b4de1ae88dd91f7587603d0",
  envelopeSha256:
    "d3601c2595f1ab6af5c99f297c1608d0447fd0147a07bcc277595b357e8b79d6",
  innerBytes: 41_949_577,
  envelopeBytes: 13_681_302,
} as const;

type SuiteEntry = {
  readonly sampleIndex: number;
  readonly envelopePath: string;
  readonly headerHash: string;
  readonly envelopeSha256: string;
  readonly innerSha256: string;
  readonly transactionSetSha256: string;
  readonly transactionContentSha256: string;
  readonly envelopeBytes: number;
  readonly innerBytes: number;
  readonly corpusWindow: {
    readonly startRow: number;
    readonly rowCount: number;
    readonly sha256: string;
  };
};

type FixtureSuite = {
  readonly schemaVersion: string;
  readonly sampleCount: number;
  readonly transactionsPerSample: number;
  readonly sourceCorpusPath: string;
  readonly sourceCorpusSha256: string;
  readonly sourceCorpusFileSha256: string;
  readonly sourceCorpusRows: number;
  readonly sourceCorpusEvidenceMode:
    | "phase1-live-binding"
    | "historical-offline-extension";
  readonly sourceCorpusBindingPath: string;
  readonly sourceCorpusBindingSha256: string;
  readonly sourceCorpusManifestPath: string;
  readonly sourceCorpusManifestSha256: string;
  readonly sourceCorpusGenerationResultPath: string;
  readonly sourceCorpusGenerationResultSha256: string;
  readonly anchor: typeof CURRENT_OPERATIONAL_50K;
  readonly entries: readonly SuiteEntry[];
};

type FixtureInput = SuiteEntry & {
  readonly absoluteEnvelopePath: string;
};

const fixtureEntryIdentity = (entry: SuiteEntry): SuiteEntry => ({
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

type PeerMetric = {
  readonly peerIndex: number;
  readonly pid: number;
  readonly outcome: string;
  readonly durationMs: number;
  readonly rssBeforeBytes: number;
  readonly rssAfterBytes: number;
  readonly peakRssBytes: number;
  readonly admissionPeakActive: number;
};

describe("real separate-process DA publication", () => {
  it(
    FORMAL
      ? "measures 100 independent exact-50k publications without process-start contamination"
      : "publishes the checked exact-50k envelope through three committee processes",
    async () => {
      const runtime = FORMAL ? await loadFormalRuntimeIdentity() : undefined;
      const fixtureContract = FORMAL
        ? await loadFormalFixtureSuite()
        : await loadSmokeFixture();
      const publishConcurrency = readDaHardeningConfig().publishConcurrency;
      const zstdLevel = readDaHardeningConfig().zstdLevel;
      if (FORMAL && publishConcurrency !== 8) {
        throw new Error(
          `formal Phase 5 DA gate requires publish concurrency 8; got ${publishConcurrency.toString()}`,
        );
      }
      if (FORMAL && zstdLevel !== 3) {
        throw new Error(
          `formal Phase 5 DA gate requires zstd level 3; got ${zstdLevel.toString()}`,
        );
      }
      const logsDir = join(packageRoot, "logs");
      await mkdir(logsDir, { recursive: true });
      const temp = await mkdtemp(join(logsDir, "phase5-multiprocess-"));
      const bundleDir = join(temp, "bundle");
      const helperSource = join(
        packageRoot,
        "tests/helpers/da-committee-peer-process.ts",
      );
      await execFileAsync(
        "pnpm",
        [
          "exec",
          "tsup",
          helperSource,
          "--format",
          "esm",
          "--platform",
          "node",
          "--target",
          "node22",
          "--out-dir",
          bundleDir,
          "--no-config",
          "--no-splitting",
        ],
        { cwd: packageRoot },
      );
      const helper = join(bundleDir, "da-committee-peer-process.js");
      const producerSeed = `seed:${"00".repeat(31)}19`;
      const committeeSeeds = [
        `seed:${"00".repeat(31)}11`,
        `seed:${"00".repeat(31)}12`,
        `seed:${"00".repeat(31)}13`,
      ] as const;
      const producerIdentity = await loadDaLibp2pIdentity(producerSeed);
      const identities = await Promise.all(
        committeeSeeds.map(loadDaLibp2pIdentity),
      );
      const ports = await Promise.all(committeeSeeds.map(reserveLoopbackPort));
      const peers: readonly (DaProducerCommitteePeer & Libp2pDaPeerConfig)[] =
        identities.map((identity, index) => ({
          signerIndex: index,
          daVkey: (index + 11).toString(16).padStart(2, "0").repeat(32),
          peerId: identity.peerId,
          multiaddrs: [
            `/ip4/127.0.0.1/tcp/${ports[index]!.toString()}/p2p/${identity.peerId}`,
          ],
          roles: ["committee", "retrieval"],
        }));
      const producerPeer: Libp2pDaPeerConfig = {
        signerIndex: 99,
        daVkey: "99".repeat(32),
        peerId: producerIdentity.peerId,
        multiaddrs: [`/ip4/127.0.0.1/tcp/0/p2p/${producerIdentity.peerId}`],
        roles: ["producer"],
      };
      const children: ChildProcess[] = [];
      let transport:
        | Awaited<ReturnType<typeof createDaLibp2pProducerTransport>>
        | undefined;
      try {
        for (let index = 0; index < committeeSeeds.length; index += 1) {
          const transportConfig: Libp2pDaTransportConfig = {
            kind: "libp2p",
            deploymentFingerprint: DEPLOYMENT,
            noHttpDaTransport: true,
            threshold: 2,
            listenMultiaddrs: [
              `/ip4/127.0.0.1/tcp/${ports[index]!.toString()}`,
            ],
            announceMultiaddrs: peers[index]!.multiaddrs,
            bootstrapMultiaddrs: [
              peers[(index + 1) % peers.length]!.multiaddrs[0]!,
            ],
            gossip: {
              strictSign: true,
              emitSelf: false,
              allowedTopicsOnly: true,
              maxGossipMessageBytes:
                DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
            },
            limits: {
              maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
              maxInlineResponseBytes:
                DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
              maxChunkBytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
              maxStreamsPerPeer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
              requestTimeoutMs: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
            },
            retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
            peers: [...peers, producerPeer],
          };
          const configPath = join(temp, `peer-${index.toString()}.json`);
          await writeFile(
            configPath,
            JSON.stringify({
              peerIndex: index,
              privateKeySource: committeeSeeds[index],
              storeDir: join(temp, `store-${index.toString()}`),
              metricsPath: join(temp, `metrics-${index.toString()}.ndjson`),
              transport: transportConfig,
            }),
          );
          const child = spawn(process.execPath, [helper, configPath], {
            cwd: packageRoot,
            stdio: ["ignore", "pipe", "pipe"],
          });
          children.push(child);
          await waitForReady(child);
        }

        const manifest: DaProducerPublicationManifest = {
          deploymentFingerprint: DEPLOYMENT,
          contractDeploymentManifestId: DEPLOYMENT,
          localPrivateKeySource: producerSeed,
          threshold: 2,
          requestTimeoutMs: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
          maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
          maxInlineResponseBytes: DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
          maxChunkBytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
          maxStreamsPerPeer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
          maxGossipMessageBytes: DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
          listenMultiaddrs: [],
          announceMultiaddrs: [
            `/ip4/127.0.0.1/tcp/0/p2p/${producerIdentity.peerId}`,
          ],
          bootstrapMultiaddrs: [],
          committeePeers: peers,
        };
        transport = await createDaLibp2pProducerTransport(manifest, {
          mode: "dial-only",
        });
        const samples = [];
        for (const fixture of fixtureContract.entries) {
          const loaded = await loadAndVerifyFixture(fixture);
          const producerRssBeforeBytes = process.memoryUsage().rss;
          const startedAt = performance.now();
          const envelope = await wrapDaPayloadV3(loaded.innerBytes, {
            mode: "zstd",
            zstdLevel,
          });
          if (
            envelope.length !== fixture.envelopeBytes ||
            sha256(envelope) !== fixture.envelopeSha256
          ) {
            throw new Error(
              `fixture ${fixture.sampleIndex.toString()} is not reproducible with the pinned zstd configuration`,
            );
          }
          let acceptedResults = 0;
          let settledResults = 0;
          let thresholdDurationMs: number | undefined;
          let allPeerDurationMs: number | undefined;
          const publication = await publishDaPayloadInsert({
            insert: insertFor(loaded.headerHash, envelope),
            manifest,
            transport,
            onPeerResult: async ({ result }) => {
              settledResults += 1;
              if (result.status === "accepted") {
                acceptedResults += 1;
                if (acceptedResults === manifest.threshold) {
                  thresholdDurationMs = performance.now() - startedAt;
                }
              }
              if (settledResults === manifest.committeePeers.length) {
                allPeerDurationMs = performance.now() - startedAt;
              }
            },
          });
          const peerResults = await publication.allPeerResults;
          if (
            thresholdDurationMs === undefined ||
            allPeerDurationMs === undefined
          ) {
            throw new Error(
              `fixture ${fixture.sampleIndex.toString()} did not produce exact threshold/all-peer decision timestamps`,
            );
          }
          expect(publication.acceptedPeers).toBeGreaterThanOrEqual(2);
          expect(peerResults).toHaveLength(3);
          expect(
            peerResults?.every((result) => result.status === "accepted"),
          ).toBe(true);
          samples.push({
            sampleIndex: fixture.sampleIndex,
            headerHash: fixture.headerHash,
            envelopeSha256: fixture.envelopeSha256,
            innerSha256: fixture.innerSha256,
            transactionSetSha256: fixture.transactionSetSha256,
            transactionContentSha256: fixture.transactionContentSha256,
            transactionCount: TRANSACTION_COUNT,
            acceptedPeers: peerResults?.filter(
              (result) => result.status === "accepted",
            ).length,
            peerStatuses: peerResults?.map((result) => result.status),
            thresholdDurationMs,
            allPeerDurationMs,
            producerRssBeforeBytes,
            producerRssAfterBytes: process.memoryUsage().rss,
            producerPeakRssBytes: process.resourceUsage().maxRSS * 1024,
          });
        }
        const metricsByPeer = await Promise.all(
          committeeSeeds.map((_, index) => readPeerMetrics(temp, index)),
        );
        const expectedRequests = fixtureContract.entries.length;
        expect(
          metricsByPeer.every(
            (metrics) =>
              metrics.length === expectedRequests &&
              metrics.every(
                (metric) =>
                  metric.admissionPeakActive === 1 &&
                  metric.outcome === "completed",
              ),
          ),
        ).toBe(true);
        const statistics = {
          threshold: distributionStats(
            samples.map((sample) => sample.thresholdDurationMs),
          ),
          allPeer: distributionStats(
            samples.map((sample) => sample.allPeerDurationMs),
          ),
        };

        if (FORMAL) {
          const reportOutput =
            process.env.MIDGARD_DA_PHASE5_DISTRIBUTION_REPORT;
          if (reportOutput === undefined || reportOutput.length === 0) {
            throw new Error(
              "MIDGARD_DA_PHASE5_DISTRIBUTION_REPORT is required in formal mode",
            );
          }
          const report = {
            schemaVersion: REPORT_SCHEMA,
            formal: true,
            generatedAt: new Date().toISOString(),
            sampleCount: SAMPLE_COUNT,
            independentSemanticEnvelopeCount: SAMPLE_COUNT,
            fixtureSuite: fixtureContract.identity,
            runtime,
            config: {
              committeePeers: 3,
              threshold: 2,
              transactionCountPerEnvelope: TRANSACTION_COUNT,
              payloadSchemaVersion: 3,
              transportProtocolVersion: 1,
              deploymentFingerprint: DEPLOYMENT,
              publishConcurrency,
              zstdLevel,
              timingBoundary:
                "verified_inner_to_threshold_acceptance_including_zstd",
              producerProcessStarts: 1,
              committeeProcessStarts: 3,
              transportStarts: 1,
              perSampleProcessStarts: 0,
              maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
              maxInlineResponseBytes:
                DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
              maxChunkBytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
              maxStreamsPerPeer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
              requestTimeoutMs: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
            },
            samples,
            statistics,
            committeeProcesses: metricsByPeer.map((metrics, peerIndex) => ({
              peerIndex,
              pid: metrics[0]!.pid,
              requestCount: metrics.length,
              peakRssBytes: Math.max(
                ...metrics.map((metric) => metric.peakRssBytes),
              ),
              maxAdmissionPeakActive: Math.max(
                ...metrics.map((metric) => metric.admissionPeakActive),
              ),
              samples: metrics,
            })),
            resources: {
              producerPeakRssBytes: process.resourceUsage().maxRSS * 1024,
              producerRssBytes: process.memoryUsage().rss,
            },
            verdict: {
              thresholdP99LimitMs: P99_LIMIT_MS,
              thresholdP99Ms: statistics.threshold.p99Ms,
              passed: statistics.threshold.p99Ms <= P99_LIMIT_MS,
            },
          };
          await writeFile(
            reportOutput,
            `${JSON.stringify(report, null, 2)}\n`,
            {
              flag: "wx",
            },
          );
          process.stdout.write(`${JSON.stringify(report.verdict)}\n`);
        } else {
          process.stdout.write(
            `${JSON.stringify({
              schemaVersion: "midgard-phase-5-live-da-50k-multiprocess-v1",
              peers: 3,
              threshold: 2,
              envelopeBytes: CURRENT_OPERATIONAL_50K.envelopeBytes,
              artifactSource: "operational-artifact",
              thresholdDurationMs: samples[0]!.thresholdDurationMs,
              allPeerDurationMs: samples[0]!.allPeerDurationMs,
              committeeProcesses: metricsByPeer.map((metrics) => metrics[0]),
            })}\n`,
          );
        }
      } finally {
        await transport?.close?.();
        await Promise.all(children.map(stopChild));
        await rm(temp, { recursive: true, force: true });
      }
    },
    FORMAL ? 1_800_000 : 90_000,
  );
});

const loadSmokeFixture = async () => {
  await verifyCheckedAnchorMeasurement();
  const transactionIdentities =
    await transactionIdentitiesForEnvelope(envelopePath);
  const entry: FixtureInput = {
    sampleIndex: 0,
    envelopePath,
    absoluteEnvelopePath: envelopePath,
    headerHash: CURRENT_OPERATIONAL_50K.headerHash,
    envelopeSha256: CURRENT_OPERATIONAL_50K.envelopeSha256,
    innerSha256: CURRENT_OPERATIONAL_50K.innerSha256,
    ...transactionIdentities,
    envelopeBytes: CURRENT_OPERATIONAL_50K.envelopeBytes,
    innerBytes: CURRENT_OPERATIONAL_50K.innerBytes,
    corpusWindow: {
      startRow: 0,
      rowCount: TRANSACTION_COUNT,
      sha256: CURRENT_OPERATIONAL_50K.corpusPrefixSha256,
    },
  };
  return { entries: [entry], identity: undefined };
};

const loadFormalFixtureSuite = async () => {
  if (process.version !== EXPECTED_NODE_VERSION) {
    throw new Error(
      `formal Phase 5 DA distribution requires ${EXPECTED_NODE_VERSION}; got ${process.version}`,
    );
  }
  const manifestPath = process.env.MIDGARD_DA_PHASE5_FIXTURE_SUITE;
  if (manifestPath === undefined || manifestPath.length === 0) {
    throw new Error(
      "MIDGARD_DA_PHASE5_FIXTURE_SUITE is required; replaying the one checked envelope is forbidden",
    );
  }
  const absoluteManifestPath = resolve(manifestPath);
  const manifestBytes = await readFile(absoluteManifestPath);
  const suite = JSON.parse(manifestBytes.toString("utf8")) as FixtureSuite;
  if (
    suite.schemaVersion !== FIXTURE_SUITE_SCHEMA ||
    suite.sampleCount !== SAMPLE_COUNT ||
    suite.transactionsPerSample !== TRANSACTION_COUNT ||
    suite.sourceCorpusRows < SAMPLE_COUNT * TRANSACTION_COUNT ||
    suite.entries?.length !== SAMPLE_COUNT ||
    !anchorMatches(suite.anchor)
  ) {
    throw new Error(
      "formal fixture suite must bind 100 independent 50k envelopes and the checked exact anchor",
    );
  }
  const manifestDir = dirname(absoluteManifestPath);
  const entries = suite.entries.map((entry, index): FixtureInput => {
    if (
      entry.sampleIndex !== index ||
      entry.corpusWindow?.startRow !== index * TRANSACTION_COUNT ||
      entry.corpusWindow.rowCount !== TRANSACTION_COUNT ||
      !isSha256(entry.corpusWindow.sha256) ||
      !isSha256(entry.transactionSetSha256) ||
      !isSha256(entry.transactionContentSha256) ||
      !isSha256(entry.envelopeSha256) ||
      !isSha256(entry.innerSha256) ||
      !/^[0-9a-f]{56}$/u.test(entry.headerHash)
    ) {
      throw new Error(`invalid fixture suite entry ${index.toString()}`);
    }
    if (isAbsolute(entry.envelopePath)) {
      throw new Error(
        `fixture suite entry ${index.toString()} path must be relative`,
      );
    }
    const absoluteEnvelopePath = resolve(manifestDir, entry.envelopePath);
    if (relative(manifestDir, absoluteEnvelopePath).startsWith("..")) {
      throw new Error(
        `fixture suite entry ${index.toString()} escapes its directory`,
      );
    }
    return { ...entry, absoluteEnvelopePath };
  });
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
  if (
    entries[0]!.headerHash !== CURRENT_OPERATIONAL_50K.headerHash ||
    entries[0]!.envelopeSha256 !== CURRENT_OPERATIONAL_50K.envelopeSha256 ||
    entries[0]!.innerSha256 !== CURRENT_OPERATIONAL_50K.innerSha256 ||
    entries[0]!.envelopeBytes !== CURRENT_OPERATIONAL_50K.envelopeBytes ||
    entries[0]!.innerBytes !== CURRENT_OPERATIONAL_50K.innerBytes ||
    entries[0]!.corpusWindow.sha256 !==
      CURRENT_OPERATIONAL_50K.corpusPrefixSha256
  ) {
    throw new Error(
      "fixture suite entry zero must be the checked exact anchor",
    );
  }
  if (
    typeof suite.sourceCorpusPath !== "string" ||
    isAbsolute(suite.sourceCorpusPath)
  ) {
    throw new Error("source corpus path must be relative to the fixture suite");
  }
  const sourceCorpusPath = resolve(manifestDir, suite.sourceCorpusPath);
  if (relative(manifestDir, sourceCorpusPath).startsWith("..")) {
    throw new Error("source corpus path escapes the fixture suite");
  }
  const provenance = await verifySourceCorpusProvenance(manifestDir, suite);
  if (suite.sourceCorpusEvidenceMode !== provenance.evidenceMode) {
    throw new Error("fixture suite source corpus evidence mode is false");
  }
  const corpus = await verifyCorpusProvenance(
    sourceCorpusPath,
    entries,
    provenance.prefixBytes === undefined
      ? undefined
      : {
          bytes: provenance.prefixBytes,
          sha256: provenance.prefixSha256!,
        },
  );
  if (
    corpus.rows !== suite.sourceCorpusRows ||
    corpus.sha256 !== suite.sourceCorpusSha256 ||
    corpus.fileSha256 !== suite.sourceCorpusFileSha256 ||
    provenance.corpusFileSha256 !== corpus.fileSha256 ||
    provenance.corpusRows !== corpus.rows
  ) {
    throw new Error(
      "fixture suite source corpus identity does not match bytes",
    );
  }
  return {
    entries,
    identity: {
      schemaVersion: FIXTURE_SUITE_SCHEMA,
      manifestSha256: sha256(manifestBytes),
      sourceCorpusSha256: corpus.sha256,
      sourceCorpusFileSha256: corpus.fileSha256,
      sourceCorpusRows: corpus.rows,
      sourceCorpusEvidenceMode: provenance.evidenceMode,
      sourceCorpusBindingSha256: provenance.bindingSha256,
      sourceCorpusManifestSha256: provenance.manifestSha256,
      sourceCorpusGenerationResultSha256: provenance.generationResultSha256,
      anchor: CURRENT_OPERATIONAL_50K,
      entries: entries.map(fixtureEntryIdentity),
    },
  };
};

const loadAndVerifyFixture = async (fixture: FixtureInput) => {
  const envelope = await readFile(fixture.absoluteEnvelopePath);
  if (
    envelope.length !== fixture.envelopeBytes ||
    envelope.length > DA_TRANSPORT_LIMITS_V1.maxPayloadBytes ||
    sha256(envelope) !== fixture.envelopeSha256
  ) {
    throw new Error(
      `fixture ${fixture.sampleIndex.toString()} envelope identity mismatch`,
    );
  }
  const unwrapped = await unwrapDaPayload(envelope, {
    maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
    schemaVersion: 3,
  });
  if (
    unwrapped.innerBytes.length !== fixture.innerBytes ||
    sha256(unwrapped.innerBytes) !== fixture.innerSha256
  ) {
    throw new Error(
      `fixture ${fixture.sampleIndex.toString()} inner identity mismatch`,
    );
  }
  const payload = SDK.decodeDaPayloadV2Canonical(unwrapped.innerBytes);
  for (const [
    index,
    [txHash, canonicalCborHex],
  ] of payload.block_body.transactions.entries()) {
    verifyCanonicalTransactionIdentity(
      txHash,
      canonicalCborHex,
      `fixture ${fixture.sampleIndex.toString()} transaction ${index.toString()}`,
    );
  }
  if (
    payload.block_body.header_hash !== fixture.headerHash ||
    payload.block_body.transactions.length !== TRANSACTION_COUNT ||
    payload.block_body.counts.l2TransactionCount !==
      BigInt(TRANSACTION_COUNT) ||
    payload.block_body.utxos.length !== TRANSACTION_COUNT * 2 ||
    payload.block_body.transition_trace.length !== TRANSACTION_COUNT ||
    payload.block_body.event_to_step.length !== TRANSACTION_COUNT ||
    hashTransactionEntries(payload.block_body.transactions) !==
      fixture.transactionSetSha256 ||
    hashTransactionContents(payload.block_body.transactions) !==
      fixture.transactionContentSha256
  ) {
    throw new Error(
      `fixture ${fixture.sampleIndex.toString()} is not its declared operational 50k transaction set`,
    );
  }
  return {
    innerBytes: unwrapped.innerBytes,
    headerHash: Buffer.from(fixture.headerHash, "hex"),
  };
};

const verifyCheckedAnchorMeasurement = async (): Promise<void> => {
  if (!existsSync(envelopePath) || !existsSync(measurementPath)) {
    throw new Error(
      "DA 50k operational gate is blocked: the checked envelope and measurement are required; structural fallback is forbidden",
    );
  }
  const [envelope, reportBytes] = await Promise.all([
    readFile(envelopePath),
    readFile(measurementPath),
  ]);
  const report = JSON.parse(reportBytes.toString("utf8")) as {
    readonly complete?: boolean;
    readonly encoder?: string;
    readonly scenario?: string;
    readonly traceStepsPerTx?: number;
    readonly corpusPath?: string;
    readonly corpusRowsRead?: number;
    readonly corpusPrefixSha256?: string;
    readonly pinnedMaxPayloadBytes?: number;
    readonly model?: string;
    readonly measurements?: readonly {
      readonly txCount: number;
      readonly headerHash: string;
      readonly modeledEntries?: {
        readonly transactions: number;
        readonly utxos: number;
        readonly transitionTrace: number;
        readonly eventToStep: number;
      };
      readonly uncompressedBytes: number;
      readonly envelopeBytes: number;
      readonly innerSha256?: string;
      readonly envelopeSha256: string;
      readonly uncompressedFitsPinnedLimit: boolean;
      readonly envelopeFitsPinnedLimit: boolean;
    }[];
  };
  const measurement = report.measurements?.find(
    (candidate) => candidate.txCount === TRANSACTION_COUNT,
  );
  if (
    report.complete !== true ||
    report.encoder !== "byte" ||
    report.scenario !== "operational" ||
    report.traceStepsPerTx !== 1 ||
    report.corpusRowsRead !== TRANSACTION_COUNT ||
    !report.corpusPath?.endsWith("corpus-live-4096/corpus.ndjson") ||
    !/real canonical transaction CBOR/u.test(report.model ?? "") ||
    report.corpusPrefixSha256 !== CURRENT_OPERATIONAL_50K.corpusPrefixSha256 ||
    report.pinnedMaxPayloadBytes !== DA_TRANSPORT_LIMITS_V1.maxPayloadBytes ||
    report.measurements?.length !== 1 ||
    measurement?.headerHash !== CURRENT_OPERATIONAL_50K.headerHash ||
    measurement?.modeledEntries?.transactions !== TRANSACTION_COUNT ||
    measurement.modeledEntries.utxos !== TRANSACTION_COUNT * 2 ||
    measurement.modeledEntries.transitionTrace !== TRANSACTION_COUNT ||
    measurement.modeledEntries.eventToStep !== TRANSACTION_COUNT ||
    measurement.uncompressedBytes !== CURRENT_OPERATIONAL_50K.innerBytes ||
    measurement.envelopeBytes !== CURRENT_OPERATIONAL_50K.envelopeBytes ||
    measurement.innerSha256 !== CURRENT_OPERATIONAL_50K.innerSha256 ||
    measurement.envelopeSha256 !== CURRENT_OPERATIONAL_50K.envelopeSha256 ||
    sha256(envelope) !== CURRENT_OPERATIONAL_50K.envelopeSha256 ||
    measurement.uncompressedFitsPinnedLimit !== true ||
    measurement.envelopeFitsPinnedLimit !== true
  ) {
    throw new Error("checked exact-50k envelope/measurement binding failed");
  }
};

const transactionIdentitiesForEnvelope = async (
  path: string,
): Promise<{
  readonly transactionSetSha256: string;
  readonly transactionContentSha256: string;
}> => {
  const unwrapped = await unwrapDaPayload(await readFile(path), {
    maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
    schemaVersion: 3,
  });
  const transactions = SDK.decodeDaPayloadV2Canonical(unwrapped.innerBytes)
    .block_body.transactions;
  return {
    transactionSetSha256: hashTransactionEntries(transactions),
    transactionContentSha256: hashTransactionContents(transactions),
  };
};

const hashTransactionEntries = (
  entries: readonly (readonly [string, string])[],
): string => {
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

const hashTransactionContents = (
  entries: readonly (readonly [string, string])[],
): string => {
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

const verifyCanonicalTransactionIdentity = (
  txHash: string,
  canonicalCborHex: string,
  label: string,
): void => {
  if (
    !/^[0-9a-f]{64}$/u.test(txHash) ||
    !/^(?:[0-9a-f]{2})+$/u.test(canonicalCborHex)
  ) {
    throw new Error(`${label} is not canonical transaction evidence`);
  }
  const transactionBytes = Buffer.from(canonicalCborHex, "hex");
  let transaction: ReturnType<
    typeof decodeMidgardNativeTxFullFromCanonicalCbor
  >;
  try {
    transaction = decodeMidgardNativeTxFullFromCanonicalCbor(transactionBytes);
  } catch (error) {
    throw new Error(
      `${label} is invalid Midgard native transaction CBOR: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  if (!encodeMidgardNativeTxCanonical(transaction).equals(transactionBytes)) {
    throw new Error(`${label} transaction CBOR is not canonical`);
  }
  if (computeMidgardNativeTxId(transaction).toString("hex") !== txHash) {
    throw new Error(
      `${label} transaction ID does not match its Midgard native body`,
    );
  }
};

const verifyCorpusProvenance = async (
  path: string,
  entries: readonly FixtureInput[],
  prefixEvidence?: { readonly bytes: number; readonly sha256: string },
): Promise<{
  readonly rows: number;
  readonly sha256: string;
  readonly fileSha256: string;
}> => {
  const fullHash = createHash("sha256");
  const fileHash = createHash("sha256");
  const prefixHash = createHash("sha256");
  let prefixBytesRemaining = prefixEvidence?.bytes;
  const trackerRoot = await mkdtemp(
    join(tmpdir(), "midgard-phase5-process-transaction-ids-"),
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
  let activeEntries: [string, string][] = [];
  let windowHash = createHash("sha256");
  try {
    for await (const line of input) {
      if (line.length === 0) continue;
      fullHash.update(line).update("\n");
      if (rows < SAMPLE_COUNT * TRANSACTION_COUNT) {
        const parsed = JSON.parse(line) as {
          readonly txHash?: string;
          readonly canonicalCborHex?: string;
        };
        if (
          !/^[0-9a-f]{64}$/u.test(parsed.txHash ?? "") ||
          !/^(?:[0-9a-f]{2})+$/u.test(parsed.canonicalCborHex ?? "")
        ) {
          throw new Error(`invalid source corpus row ${rows.toString()}`);
        }
        verifyCanonicalTransactionIdentity(
          parsed.txHash!,
          parsed.canonicalCborHex!,
          `source corpus row ${rows.toString()}`,
        );
        await transactionIdTracker.add({
          rowIndex: rows,
          txHash: parsed.txHash!,
        });
        activeEntries.push([parsed.txHash!, parsed.canonicalCborHex!]);
        windowHash.update(line).update("\n");
        if (activeEntries.length === TRANSACTION_COUNT) {
          const entry = entries[Math.floor(rows / TRANSACTION_COUNT)]!;
          const rawWindowHash = windowHash.digest("hex");
          if (
            rawWindowHash !== entry.corpusWindow.sha256 ||
            hashTransactionEntries(activeEntries) !==
              entry.transactionSetSha256 ||
            hashTransactionContents(activeEntries) !==
              entry.transactionContentSha256
          ) {
            throw new Error(
              `source corpus window ${entry.sampleIndex.toString()} does not bind its envelope transaction set`,
            );
          }
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
  if (rows < SAMPLE_COUNT * TRANSACTION_COUNT || activeEntries.length !== 0) {
    throw new Error(
      `source corpus has ${rows.toString()} rows; 5,000,000 are required for independent windows`,
    );
  }
  if (
    prefixEvidence !== undefined &&
    (prefixBytesRemaining !== 0 ||
      prefixHash.digest("hex") !== prefixEvidence.sha256)
  ) {
    throw new Error(
      "source corpus does not preserve the bound historical base prefix",
    );
  }
  return {
    rows,
    sha256: fullHash.digest("hex"),
    fileSha256: fileHash.digest("hex"),
  };
};

const verifySourceCorpusProvenance = async (
  manifestDir: string,
  suite: FixtureSuite,
): Promise<{
  readonly corpusRows: number;
  readonly corpusFileSha256: string;
  readonly bindingSha256: string;
  readonly manifestSha256: string;
  readonly generationResultSha256: string;
  readonly evidenceMode: "phase1-live-binding" | "historical-offline-extension";
  readonly prefixBytes?: number;
  readonly prefixSha256?: string;
}> => {
  const verified = (await verifyPhase5DaSourceCorpusEvidence(
    manifestDir,
    suite,
  )) as {
    readonly corpusRows?: unknown;
    readonly corpusFileSha256?: unknown;
    readonly bindingSha256?: unknown;
    readonly manifestSha256?: unknown;
    readonly generationResultSha256?: unknown;
    readonly evidenceMode?: unknown;
    readonly prefixBytes?: unknown;
    readonly prefixSha256?: unknown;
  };
  if (
    !Number.isSafeInteger(verified.corpusRows) ||
    (verified.corpusRows as number) < SAMPLE_COUNT * TRANSACTION_COUNT ||
    !isSha256(String(verified.corpusFileSha256 ?? "")) ||
    !isSha256(String(verified.bindingSha256 ?? "")) ||
    !isSha256(String(verified.manifestSha256 ?? "")) ||
    !isSha256(String(verified.generationResultSha256 ?? "")) ||
    (verified.evidenceMode !== "phase1-live-binding" &&
      verified.evidenceMode !== "historical-offline-extension") ||
    (verified.prefixBytes !== undefined &&
      (!Number.isSafeInteger(verified.prefixBytes) ||
        (verified.prefixBytes as number) <= 0 ||
        !isSha256(String(verified.prefixSha256 ?? ""))))
  ) {
    throw new Error(
      "shared source corpus evidence verifier returned invalid data",
    );
  }
  return verified as {
    readonly corpusRows: number;
    readonly corpusFileSha256: string;
    readonly bindingSha256: string;
    readonly manifestSha256: string;
    readonly generationResultSha256: string;
    readonly evidenceMode:
      | "phase1-live-binding"
      | "historical-offline-extension";
    readonly prefixBytes?: number;
    readonly prefixSha256?: string;
  };
};

const loadFormalRuntimeIdentity = async () => {
  const expectedImageReference =
    process.env.MIDGARD_DA_PHASE5_EXPECTED_IMAGE_REFERENCE;
  const expectedImageId = normalizeSha256(
    process.env.MIDGARD_DA_PHASE5_EXPECTED_IMAGE_ID,
  );
  if (expectedImageReference === undefined || expectedImageId === undefined) {
    throw new Error(
      "formal mode requires exact EXPECTED_IMAGE_REFERENCE and immutable EXPECTED_IMAGE_ID",
    );
  }
  const containerId = hostname();
  const { stdout } = await execFileAsync("docker", ["inspect", containerId]);
  const inspected = JSON.parse(stdout) as readonly {
    readonly Id?: string;
    readonly Image?: string;
    readonly Config?: { readonly Image?: string; readonly Hostname?: string };
    readonly HostConfig?: {
      readonly CpusetCpus?: string;
      readonly Memory?: number;
    };
    readonly State?: { readonly Running?: boolean };
  }[];
  const container = inspected[0];
  const actualImageId = normalizeSha256(container?.Image);
  const actualImageReference = container?.Config?.Image;
  const configuredHostname = container?.Config?.Hostname;
  const cpusetCpus = container?.HostConfig?.CpusetCpus;
  const memoryLimitBytes = container?.HostConfig?.Memory;
  if (
    container?.State?.Running !== true ||
    actualImageId !== expectedImageId ||
    actualImageReference !== expectedImageReference ||
    container.Id === undefined ||
    !container.Id.startsWith(containerId) ||
    configuredHostname !== containerId ||
    cpusetCpus !== "28-31" ||
    memoryLimitBytes !== 12 * 1024 * 1024 * 1024
  ) {
    throw new Error(
      "running container image identity does not match the formal gate pin",
    );
  }
  return {
    nodeVersion: process.version,
    platform: process.platform,
    architecture: process.arch,
    cpuCount: cpus().length,
    cpuModel: cpus()[0]?.model ?? "unknown",
    totalMemoryBytes: totalmem(),
    observedHostname: containerId,
    configuredHostname,
    cpusetCpus,
    memoryLimitBytes,
    expectedImageReference,
    expectedImageId,
    actualImageReference,
    actualImageId,
    containerId: container.Id,
  };
};

const readPeerMetrics = async (
  temp: string,
  peerIndex: number,
): Promise<readonly PeerMetric[]> =>
  (await readFile(join(temp, `metrics-${peerIndex.toString()}.ndjson`), "utf8"))
    .trim()
    .split("\n")
    .filter((line) => line.length > 0)
    .map((line) => JSON.parse(line) as PeerMetric);

const insertFor = (
  headerHash: Buffer,
  envelope: Buffer,
): DaPayloadsDB.InsertInput => ({
  header_hash: headerHash,
  version: 3,
  payload_cbor: envelope,
  payload_sha256: computeDaSha256Hash(envelope),
  utxos_root: "00".repeat(32),
  forced_transactions_root: "00".repeat(32),
  transactions_root: "00".repeat(32),
  deposits_root: "00".repeat(32),
  withdrawals_root: "00".repeat(32),
  transition_trace_root: "00".repeat(32),
  event_to_step_root: "00".repeat(32),
  withdrawal_count: 0n,
  forced_transaction_count: 0n,
  l2_transaction_count: BigInt(TRANSACTION_COUNT),
  deposit_count: 0n,
  total_event_count: BigInt(TRANSACTION_COUNT),
  transition_step_count: BigInt(TRANSACTION_COUNT),
  block_start_time: new Date(1),
  block_end_time: new Date(2),
});

const distributionStats = (values: readonly number[]) => {
  const sorted = [...values].sort((left, right) => left - right);
  const at = (percentile: number) =>
    sorted[Math.max(0, Math.ceil(percentile * sorted.length) - 1)]!;
  return {
    p50Ms: at(0.5),
    p95Ms: at(0.95),
    p99Ms: at(0.99),
    maxMs: sorted.at(-1)!,
  };
};

const sha256 = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");
const isSha256 = (value: string): boolean => /^[0-9a-f]{64}$/u.test(value);
const normalizeSha256 = (value: string | undefined): string | undefined => {
  const normalized = value?.replace(/^sha256:/u, "");
  return normalized !== undefined && isSha256(normalized)
    ? normalized
    : undefined;
};
const anchorMatches = (anchor: typeof CURRENT_OPERATIONAL_50K): boolean =>
  Object.entries(CURRENT_OPERATIONAL_50K).every(
    ([key, value]) => anchor?.[key as keyof typeof anchor] === value,
  );
const requireUnique = (values: readonly string[], label: string): void => {
  if (new Set(values).size !== values.length) {
    throw new Error(`formal fixture suite ${label} must be unique`);
  }
};

const waitForReady = (child: ChildProcess): Promise<void> =>
  new Promise((resolveReady, reject) => {
    let stdout = "";
    let stderr = "";
    const timeout = setTimeout(
      () => reject(new Error(`committee peer readiness timed out: ${stderr}`)),
      15_000,
    );
    child.stdout?.on("data", (chunk) => {
      stdout += String(chunk);
      if (stdout.includes('"ready":true')) {
        clearTimeout(timeout);
        resolveReady();
      }
    });
    child.stderr?.on("data", (chunk) => {
      stderr += String(chunk);
    });
    child.once("exit", (code) => {
      clearTimeout(timeout);
      reject(
        new Error(
          `committee peer exited before readiness code=${String(code)} stderr=${stderr}`,
        ),
      );
    });
  });

const stopChild = (child: ChildProcess): Promise<void> =>
  new Promise((resolveStop) => {
    if (child.exitCode !== null) {
      resolveStop();
      return;
    }
    child.once("exit", () => resolveStop());
    child.kill("SIGTERM");
    setTimeout(() => {
      if (child.exitCode === null) child.kill("SIGKILL");
    }, 5_000).unref();
  });

const reserveLoopbackPort = (): Promise<number> =>
  new Promise((resolvePort, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      if (address === null || typeof address === "string") {
        reject(new Error("failed to reserve loopback port"));
        return;
      }
      server.close((error) =>
        error === undefined ? resolvePort(address.port) : reject(error),
      );
    });
  });
