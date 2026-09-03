import { type ChildProcess, spawn } from "node:child_process";
import { mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { createServer } from "node:net";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
} from "@al-ft/midgard-core/da-transport";
import type {
  Libp2pDaPeerConfig,
  Libp2pDaTransportConfig,
} from "da-committee-node/config";
import { loadDaLibp2pIdentity } from "da-committee-node/da/libp2p";
import { JsonFileWatcherStore } from "da-committee-node/store";
import { build as bundleWithTsup } from "tsup";
import { describe, expect, it } from "vitest";

import { makePayloadFixture } from "../../da-committee-node/tests/helpers.js";
import {
  createDaLibp2pProducerTransport,
  DaPayloadPublicationError,
  type DaProducerCommitteePeer,
  type DaProducerPublicationManifest,
  probeDaEnvelopeCapabilities,
  publishDaPayloadInsert,
} from "../src/da/libp2p-producer.js";
import { DaPayloadsDB } from "../src/database/index.js";
import { sha256Hex } from "../src/sha256.js";

const packageRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));
const DEPLOYMENT = "b6".repeat(32);
const TRANSACTION_COUNT = 10_000;
const CHILD_COUNT = 3;
const THRESHOLD = 2;

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

describe("real separate-process canonical V1 DA publication", () => {
  it("publishes the maximum-valid 10k envelope and rejects one mutation", async () => {
    const fixture = await makePayloadFixture(TRANSACTION_COUNT);
    // Submit acknowledgement is retention-only.  The fixture itself remains
    // a known-good exact-10k body, but neither producer nor committee submit
    // handlers may semantically traverse it on the ACK path.
    expect(fixture.payload.block_body.transactions).toHaveLength(
      TRANSACTION_COUNT,
    );
    expect(
      new Set(fixture.payload.block_body.transactions.map(([txId]) => txId))
        .size,
    ).toBe(TRANSACTION_COUNT);
    expect(fixture.payload.block_body.counts).toMatchObject({
      l2TransactionCount: BigInt(TRANSACTION_COUNT),
      totalEventCount: BigInt(TRANSACTION_COUNT),
      transitionStepCount: BigInt(TRANSACTION_COUNT),
      validationTraceCount: BigInt(TRANSACTION_COUNT),
    });

    const envelope = await wrapDaPayload(fixture.innerPayloadCbor, {
      mode: "zstd",
      zstdLevel: 3,
    });
    expect(fixture.innerPayloadCbor.length).toBeGreaterThan(0);
    expect(envelope.length).toBeGreaterThan(0);
    expect(fixture.innerPayloadCbor.length).toBeLessThanOrEqual(
      DA_TRANSPORT_LIMITS.maxPayloadBytes,
    );
    expect(envelope.length).toBeLessThanOrEqual(
      DA_TRANSPORT_LIMITS.maxPayloadBytes,
    );

    // Keep the bundled executable below the package root so Node's ESM
    // package lookup can resolve the workspace dependencies. Every file is
    // removed in the finally block; no fixture or measurement artifact is
    // retained.
    const temp = await mkdtemp(join(packageRoot, ".rf078-10k-"));
    const children: ChildProcess[] = [];
    let transport:
      | Awaited<ReturnType<typeof createDaLibp2pProducerTransport>>
      | undefined;
    let stopPromise: Promise<void> | undefined;
    const stopAll = (): Promise<void> => {
      if (stopPromise !== undefined) return stopPromise;
      stopPromise = (async () => {
        await transport?.close?.().catch(() => undefined);
        await Promise.all(children.map(stopChildBounded));
      })();
      return stopPromise;
    };
    try {
      const helperSource = join(
        packageRoot,
        "tests/helpers/da-committee-peer-process.ts",
      );
      const bundleDir = join(temp, "bundle");
      // The child is a plain Node process, so it would resolve every
      // workspace package through the `import` condition, i.e. a gitignored
      // dist. Bundle all workspace packages into the helper from source
      // through the `midgard-source` exports condition (as tsc and vitest
      // do) so the child never runs against a stale or missing dist; only
      // third-party dependencies stay external.
      await bundleWithTsup({
        entry: [helperSource],
        format: ["esm"],
        platform: "node",
        target: "node22",
        outDir: bundleDir,
        config: false,
        splitting: false,
        silent: true,
        noExternal: [/^@al-ft\//, /^da-committee-node(\/|$)/],
        // Bundled workspace source reaches some CJS-only dependencies through
        // `require`; give the ESM bundle a real one instead of esbuild's throwing
        // "Dynamic require" shim.
        banner: {
          js: 'import { createRequire as __createRequire } from "node:module"; const require = __createRequire(import.meta.url);',
        },
        esbuildOptions(options) {
          options.conditions = [
            "midgard-source",
            ...(options.conditions ?? []),
          ];
        },
      });
      const helper = join(bundleDir, "da-committee-peer-process.js");
      const producerSeed = `seed:${"00".repeat(31)}19`;
      const committeeSeeds = [
        `seed:${"00".repeat(31)}11`,
        `seed:${"00".repeat(31)}12`,
        `seed:${"00".repeat(31)}13`,
      ] as const;
      const producerIdentity = await loadDaLibp2pIdentity(producerSeed);
      const committeeIdentities = await Promise.all(
        committeeSeeds.map(loadDaLibp2pIdentity),
      );
      const ports = await Promise.all(
        committeeSeeds.map(() => reserveLoopbackPort()),
      );
      const committeePeers: readonly (DaProducerCommitteePeer &
        Libp2pDaPeerConfig)[] = committeeIdentities.map((identity, index) => ({
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

      for (let index = 0; index < CHILD_COUNT; index += 1) {
        const transportConfig: Libp2pDaTransportConfig = {
          kind: "libp2p",
          deploymentFingerprint: DEPLOYMENT,
          noHttpDaTransport: true,
          threshold: THRESHOLD,
          listenMultiaddrs: [`/ip4/127.0.0.1/tcp/${ports[index]!.toString()}`],
          announceMultiaddrs: committeePeers[index]!.multiaddrs,
          bootstrapMultiaddrs: [
            committeePeers[(index + 1) % committeePeers.length]!.multiaddrs[0]!,
          ],
          gossip: {
            strictSign: true,
            emitSelf: false,
            allowedTopicsOnly: true,
            maxGossipMessageBytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
          },
          limits: {
            maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
            maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
            maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
            maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
            requestTimeoutMs: DA_TRANSPORT_LIMITS.requestTimeoutMs,
          },
          retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
          peers: [...committeePeers, producerPeer],
        };
        const configPath = join(temp, `peer-${index.toString()}.json`);
        await writeFile(
          configPath,
          `${JSON.stringify({
            peerIndex: index,
            privateKeySource: committeeSeeds[index],
            storeDir: join(temp, `store-${index.toString()}`),
            metricsPath: join(temp, `metrics-${index.toString()}.ndjson`),
            transport: transportConfig,
          })}\n`,
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
        threshold: THRESHOLD,
        requestTimeoutMs: DA_TRANSPORT_LIMITS.requestTimeoutMs,
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        maxInlineResponseBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        maxChunkBytes: DA_TRANSPORT_LIMITS.maxChunkBytes,
        maxStreamsPerPeer: DA_TRANSPORT_LIMITS.maxStreamsPerPeer,
        maxGossipMessageBytes: DA_TRANSPORT_LIMITS.maxGossipMessageBytes,
        listenMultiaddrs: [],
        announceMultiaddrs: [
          `/ip4/127.0.0.1/tcp/0/p2p/${producerIdentity.peerId}`,
        ],
        bootstrapMultiaddrs: [],
        committeePeers,
      };
      transport = await createDaLibp2pProducerTransport(manifest, {
        mode: "dial-only",
      });
      const capabilities = await probeDaEnvelopeCapabilities({
        manifest,
        mode: "zstd",
        transport,
      });
      expect(capabilities).toHaveLength(CHILD_COUNT);
      expect(capabilities.every((result) => result.capable)).toBe(true);

      const insert = insertFromFixture(fixture, envelope);
      const validStartedAt = performance.now();
      const validProducerRssBeforeBytes = process.memoryUsage().rss;
      let validThresholdDurationMs: number | undefined;
      let validAllPeerDurationMs: number | undefined;
      let validSettledPeers = 0;
      let validAcceptedPeers = 0;
      let validPublication: Awaited<ReturnType<typeof publishDaPayloadInsert>>;
      try {
        validPublication = await publishDaPayloadInsert({
          insert,
          manifest,
          transport,
          onPeerResult: async ({ result }) => {
            validSettledPeers += 1;
            if (result.status === "accepted") {
              validAcceptedPeers += 1;
              if (
                validAcceptedPeers === manifest.threshold &&
                validThresholdDurationMs === undefined
              ) {
                validThresholdDurationMs = performance.now() - validStartedAt;
              }
            }
            if (
              validSettledPeers === manifest.committeePeers.length &&
              validAllPeerDurationMs === undefined
            ) {
              validAllPeerDurationMs = performance.now() - validStartedAt;
            }
          },
        });
      } catch (error) {
        const metricsByPeer = await waitForPeerMetrics(temp, 1, 60_000);
        process.stdout.write(
          `${JSON.stringify({
            schemaVersion: "midgard-rf-078-canonical-v1-10k-runtime-v1",
            outcome: "valid-publication-failed",
            transactionCount: TRANSACTION_COUNT,
            innerBytes: fixture.innerPayloadCbor.length,
            envelopeBytes: envelope.length,
            elapsedMs: performance.now() - validStartedAt,
            error: error instanceof Error ? error.message : String(error),
            committeeProcesses: metricsByPeer,
          })}\n`,
        );
        throw error;
      }
      const validPeerResults = await validPublication.allPeerResults!;
      expect(validPublication.acceptedPeers).toBeGreaterThanOrEqual(THRESHOLD);
      expect(validPeerResults).toHaveLength(CHILD_COUNT);
      expect(
        validPeerResults.every((result) => result.status === "accepted"),
      ).toBe(true);
      expect(validThresholdDurationMs).toBeGreaterThan(0);
      expect(validThresholdDurationMs).toBeLessThanOrEqual(
        DA_TRANSPORT_LIMITS.requestTimeoutMs,
      );
      expect(validAllPeerDurationMs).toBeGreaterThan(0);
      expect(validAllPeerDurationMs).toBeLessThanOrEqual(
        DA_TRANSPORT_LIMITS.requestTimeoutMs,
      );

      const mutatedEnvelope = Buffer.from(envelope);
      mutatedEnvelope[0] ^= 1;
      const hostileStartedAt = performance.now();
      let hostileSettledPeers = 0;
      let hostileAllPeerDurationMs: number | undefined;
      let hostileError: unknown;
      try {
        await publishDaPayloadInsert({
          insert: {
            ...insert,
            [DaPayloadsDB.Columns.PAYLOAD_CBOR]: mutatedEnvelope,
            [DaPayloadsDB.Columns.PAYLOAD_SHA256]:
              computeDaSha256Hash(mutatedEnvelope),
          },
          manifest,
          transport,
          onPeerResult: async () => {
            hostileSettledPeers += 1;
            if (
              hostileSettledPeers === manifest.committeePeers.length &&
              hostileAllPeerDurationMs === undefined
            ) {
              hostileAllPeerDurationMs = performance.now() - hostileStartedAt;
            }
          },
        });
      } catch (error) {
        hostileError = error;
      }
      expect(hostileError).toBeInstanceOf(DaPayloadPublicationError);
      const hostileReport = (hostileError as DaPayloadPublicationError).report;
      const hostilePeerResults = await hostileReport.allPeerResults!;
      expect(hostileReport.acceptedPeers).toBe(0);
      expect(hostilePeerResults).toHaveLength(CHILD_COUNT);
      expect(
        hostilePeerResults.every((result) => result.status === "rejected"),
      ).toBe(true);
      expect(
        hostilePeerResults.every(
          (result) =>
            typeof result.error === "string" && result.error.length > 0,
        ),
      ).toBe(true);
      expect(hostileAllPeerDurationMs).toBeGreaterThan(0);
      expect(hostileAllPeerDurationMs).toBeLessThanOrEqual(
        DA_TRANSPORT_LIMITS.requestTimeoutMs,
      );
      expect(children.every((child) => child.exitCode === null)).toBe(true);

      const metricsByPeer = await Promise.all(
        committeeSeeds.map((_, index) => readPeerMetrics(temp, index)),
      );
      expect(metricsByPeer).toHaveLength(CHILD_COUNT);
      expect(
        new Set(metricsByPeer.map((metrics) => metrics[0]!.pid)).size,
      ).toBe(CHILD_COUNT);
      const allMetrics = metricsByPeer.flat();
      expect(allMetrics).toHaveLength(CHILD_COUNT * 2);
      for (const metric of allMetrics) {
        expect(metric.durationMs).toBeGreaterThan(0);
        expect(metric.rssBeforeBytes).toBeGreaterThan(0);
        expect(metric.rssAfterBytes).toBeGreaterThan(0);
        expect(metric.peakRssBytes).toBeGreaterThan(0);
        expect(metric.peakRssBytes).toBeGreaterThanOrEqual(
          metric.rssBeforeBytes,
        );
        expect(metric.peakRssBytes).toBeGreaterThanOrEqual(
          metric.rssAfterBytes,
        );
        expect(metric.admissionPeakActive).toBe(1);
      }
      expect(allMetrics.every((metric) => metric.outcome === "completed")).toBe(
        true,
      );

      // The handler returns a structured rejection for the hostile envelope,
      // so every request completes normally before the children are stopped.
      await stopAll();

      await Promise.all(
        committeeSeeds.map(async (_, index) => {
          const store = await JsonFileWatcherStore.open(
            join(temp, `store-${index.toString()}`),
          );
          try {
            await expect(
              store.getDaPayload(fixture.headerHash),
            ).resolves.toMatchObject({
              payloadSchemaVersion: 1,
              payloadCborHex: envelope.toString("hex"),
              payloadSha256: computeDaSha256Hash(envelope).toString("hex"),
              validationStatus: "fetched",
            });
          } finally {
            await store.close?.();
          }
        }),
      );

      const evidence = {
        schemaVersion: "midgard-rf-078-canonical-v1-10k-runtime-v1",
        transactionCount: TRANSACTION_COUNT,
        innerBytes: fixture.innerPayloadCbor.length,
        envelopeBytes: envelope.length,
        innerSha256: sha256Hex(fixture.innerPayloadCbor),
        envelopeSha256: sha256Hex(envelope),
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        valid: {
          acceptedPeers: validPublication.acceptedPeers,
          thresholdDurationMs: validThresholdDurationMs,
          allPeerDurationMs: validAllPeerDurationMs,
          producerRssBeforeBytes: validProducerRssBeforeBytes,
          producerRssAfterBytes: process.memoryUsage().rss,
          producerPeakRssBytes: process.resourceUsage().maxRSS * 1024,
        },
        hostile: {
          acceptedPeers: hostileReport.acceptedPeers,
          allPeerDurationMs: hostileAllPeerDurationMs,
        },
        committeeProcesses: metricsByPeer,
      };
      process.stdout.write(`${JSON.stringify(evidence)}\n`);
    } finally {
      await stopAll();
      await rm(temp, { recursive: true, force: true });
    }
  }, 420_000);
});

const insertFromFixture = (
  fixture: Awaited<ReturnType<typeof makePayloadFixture>>,
  envelope: Buffer,
): DaPayloadsDB.InsertInput => ({
  [DaPayloadsDB.Columns.HEADER_HASH]: Buffer.from(fixture.headerHash, "hex"),
  [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE_ID,
  [DaPayloadsDB.Columns.VERSION]: 1,
  [DaPayloadsDB.Columns.PAYLOAD_CBOR]: envelope,
  [DaPayloadsDB.Columns.PAYLOAD_SHA256]: computeDaSha256Hash(envelope),
  [DaPayloadsDB.Columns.UTXOS_ROOT]: fixture.header.utxosRoot,
  [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
    fixture.header.forcedTransactionsRoot,
  [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: fixture.header.transactionsRoot,
  [DaPayloadsDB.Columns.DEPOSITS_ROOT]: fixture.header.depositsRoot,
  [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: fixture.header.withdrawalsRoot,
  [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]:
    fixture.header.transitionTraceRoot,
  [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: fixture.header.eventToStepRoot,
  [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]:
    fixture.header.validationTracesRoot,
  [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: fixture.header.withdrawalCount,
  [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]:
    fixture.header.forcedTransactionCount,
  [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]:
    fixture.header.l2TransactionCount,
  [DaPayloadsDB.Columns.DEPOSIT_COUNT]: fixture.header.depositCount,
  [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: fixture.header.totalEventCount,
  [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]:
    fixture.header.transitionStepCount,
  [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]:
    fixture.header.validationTraceCount,
  [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(1),
  [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(2),
});

const readPeerMetrics = async (
  temp: string,
  peerIndex: number,
): Promise<readonly PeerMetric[]> =>
  (await readFile(join(temp, `metrics-${peerIndex.toString()}.ndjson`), "utf8"))
    .trim()
    .split("\n")
    .filter((line) => line.length > 0)
    .map((line) => JSON.parse(line) as PeerMetric);

const waitForPeerMetrics = async (
  temp: string,
  expectedMetricsPerPeer: number,
  timeoutMs: number,
): Promise<readonly (readonly PeerMetric[])[]> => {
  const deadline = performance.now() + timeoutMs;
  let metricsByPeer: readonly (readonly PeerMetric[])[] = [];
  while (performance.now() < deadline) {
    metricsByPeer = await Promise.all(
      Array.from({ length: CHILD_COUNT }, (_, index) =>
        readPeerMetrics(temp, index).catch(() => []),
      ),
    );
    if (
      metricsByPeer.every((metrics) => metrics.length >= expectedMetricsPerPeer)
    ) {
      return metricsByPeer;
    }
    await new Promise<void>((resolveDelay) => setTimeout(resolveDelay, 250));
  }
  return metricsByPeer;
};

const waitForReady = (child: ChildProcess): Promise<void> =>
  new Promise((resolveReady, reject) => {
    let stdout = "";
    let stderr = "";
    const timeout = setTimeout(() => {
      reject(new Error(`committee peer readiness timed out: ${stderr}`));
    }, 15_000);
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

const stopChildBounded = async (child: ChildProcess): Promise<void> => {
  if (child.exitCode !== null) return;
  await new Promise<void>((resolveStop) => {
    let settled = false;
    // Assigned after finish is defined so the closure can clear both timers.
    // eslint-disable-next-line prefer-const
    let killTimer: NodeJS.Timeout | undefined;
    // eslint-disable-next-line prefer-const
    let finishTimer: NodeJS.Timeout | undefined;
    const finish = () => {
      if (settled) return;
      settled = true;
      if (killTimer !== undefined) clearTimeout(killTimer);
      if (finishTimer !== undefined) clearTimeout(finishTimer);
      resolveStop();
    };
    child.once("exit", finish);
    if (child.exitCode !== null) {
      finish();
      return;
    }
    try {
      child.kill("SIGTERM");
    } catch {
      finish();
      return;
    }
    killTimer = setTimeout(() => {
      if (child.exitCode !== null) return;
      try {
        child.kill("SIGKILL");
      } catch {
        finish();
      }
    }, 5_000);
    killTimer.unref();
    finishTimer = setTimeout(finish, 10_000);
    finishTimer.unref();
  });
};

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
      const port = address.port;
      server.close((error) =>
        error === undefined ? resolvePort(port) : reject(error),
      );
    });
  });
