import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { createServer } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
} from "@al-ft/midgard-core/da-transport";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  closeDaLibp2pPublicationTransport,
  createDaLibp2pProducerTransport,
  type DaProducerCommitteePeer,
  type DaProducerPublicationManifest,
  getDaPublicationTransportForTest,
  probeDaEnvelopeCapabilities,
} from "@/da/libp2p-producer.js";
import { DaPayloadPublicationsDB, DaPayloadsDB } from "@/database/index.js";
import { reconcileDaPublicationsOnce } from "@/fibers/da-publication-reconciler.js";

import type {
  Libp2pDaPeerConfig,
  Libp2pDaTransportConfig,
} from "../../da-committee-node/src/config.js";
import {
  createDaLibp2pPayloadRequestHandlers,
  DaLibp2pNode,
  DaPayloadSubmitAdmission,
  loadDaLibp2pIdentity,
} from "../../da-committee-node/src/da/libp2p/index.js";
import { JsonFileWatcherStore } from "../../da-committee-node/src/store.js";
import { makePayloadFixture } from "../../da-committee-node/tests/helpers.js";
import { provideDatabaseLayers } from "./utils.js";

const enabled = process.env.MIDGARD_RUN_DA_PHASE5_JOINED_E2E === "1";
const DEPLOYMENT = "c7".repeat(32);
const RETENTION_DAYS = DA_TRANSPORT_LIMITS_V1.minimumRetentionDays;
const ENV_KEYS = [
  "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
  "DA_LIBP2P_PRIVATE_KEY_SOURCE",
  "MIDGARD_DA_PUBLISH_CONCURRENCY",
  "MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS",
  "MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS",
] as const;

describe.skipIf(!enabled)("joined DA publication reconciler E2E", () => {
  it("uses the real reconciler to converge durable delivery after a committee member restart", async () => {
    const temp = await mkdtemp(join(tmpdir(), "midgard-da-reconciler-e2e-"));
    const producerSeed = "seed:" + "00".repeat(31) + "29";
    const committeeSeeds = [
      "seed:" + "00".repeat(31) + "21",
      "seed:" + "00".repeat(31) + "22",
      "seed:" + "00".repeat(31) + "23",
    ] as const;
    const previousEnv = new Map(
      ENV_KEYS.map((key) => [key, process.env[key]] as const),
    );
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
      daVkey: (index + 21).toString(16).padStart(2, "0").repeat(32),
      peerId: identity.peerId,
      multiaddrs: [
        "/ip4/127.0.0.1/tcp/" +
          ports[index]!.toString() +
          "/p2p/" +
          identity.peerId,
      ],
      roles: ["committee", "retrieval"],
    }));
    const producerPeer: Libp2pDaPeerConfig = {
      signerIndex: 99,
      daVkey: "99".repeat(32),
      peerId: producerIdentity.peerId,
      multiaddrs: ["/ip4/127.0.0.1/tcp/0/p2p/" + producerIdentity.peerId],
      roles: ["producer"],
    };
    const stores = await Promise.all(
      committeeSeeds.map((_, index) =>
        JsonFileWatcherStore.open(join(temp, "store-" + index.toString())),
      ),
    );
    const committeeNodes = committeeSeeds.map((seed, index) => {
      const config: Libp2pDaTransportConfig = {
        kind: "libp2p",
        deploymentFingerprint: DEPLOYMENT,
        noHttpDaTransport: true,
        threshold: 2,
        listenMultiaddrs: ["/ip4/127.0.0.1/tcp/" + ports[index]!.toString()],
        announceMultiaddrs: committeePeers[index]!.multiaddrs,
        bootstrapMultiaddrs: [
          committeePeers[(index + 1) % committeePeers.length]!.multiaddrs[0]!,
        ],
        gossip: {
          strictSign: true,
          emitSelf: false,
          allowedTopicsOnly: true,
          maxGossipMessageBytes: DA_TRANSPORT_LIMITS_V1.maxGossipMessageBytes,
        },
        limits: {
          maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
          maxInlineResponseBytes: DA_TRANSPORT_LIMITS_V1.maxInlineResponseBytes,
          maxChunkBytes: DA_TRANSPORT_LIMITS_V1.maxChunkBytes,
          maxStreamsPerPeer: DA_TRANSPORT_LIMITS_V1.maxStreamsPerPeer,
          requestTimeoutMs: DA_TRANSPORT_LIMITS_V1.requestTimeoutMs,
        },
        retentionDays: RETENTION_DAYS,
        peers: [...committeePeers, producerPeer],
      };
      return new DaLibp2pNode({
        config,
        privateKeySource: seed,
        requestHandlers: createDaLibp2pPayloadRequestHandlers({
          deploymentFingerprint: DEPLOYMENT,
          store: stores[index]!,
          limits: config.limits,
          payloadSubmitAdmission: new DaPayloadSubmitAdmission(1),
        }),
      });
    });
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
        "/ip4/127.0.0.1/tcp/0/p2p/" + producerIdentity.peerId,
      ],
      bootstrapMultiaddrs: [],
      committeePeers,
    };
    const manifestPath = join(temp, "producer-manifest.json");
    let readinessTransport:
      | Awaited<ReturnType<typeof createDaLibp2pProducerTransport>>
      | undefined;
    try {
      await writeFile(
        manifestPath,
        JSON.stringify(runtimeManifest(manifest, committeePeers)),
      );
      process.env.MIDGARD_DEPLOYMENT_MANIFEST_PATH = manifestPath;
      process.env.DA_LIBP2P_PRIVATE_KEY_SOURCE = producerSeed;
      process.env.MIDGARD_DA_PUBLISH_CONCURRENCY = "3";
      process.env.MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MS = "1";
      process.env.MIDGARD_DA_PUBLISH_RETRY_BACKOFF_MAX_MS = "1";

      for (const node of committeeNodes) {
        await node.start();
      }
      readinessTransport = await createDaLibp2pProducerTransport(manifest, {
        mode: "dial-only",
      });
      expect(
        (
          await probeDaEnvelopeCapabilities({
            manifest,
            mode: "identity",
            transport: readinessTransport,
          })
        ).every((result) => result.capable),
      ).toBe(true);
      await readinessTransport.close?.();
      readinessTransport = undefined;

      const fixture = await makePayloadFixture();
      const insert = insertFromFixture(fixture);
      await Effect.runPromise(
        provideDatabaseLayers(
          Effect.gen(function* () {
            yield* DaPayloadsDB.clear;
            yield* DaPayloadsDB.upsertAvailable(insert);
          }),
        ),
      );

      await committeeNodes[2]!.stop();
      const interrupted = await runReconciler("phase5-joined-before-restart");
      const interruptedState = await publicationState(fixture.headerHash);
      expect(interrupted).toMatchObject({
        configured: true,
        claimed: 3,
        attempted: 3,
        conflicts: 0,
      });
      expect(interruptedState).toEqual({
        acceptedPeers: 2,
        peerBacklog: 1,
      });
      expect(interrupted.backlog).toBeGreaterThanOrEqual(1);
      // The failed member is durable incomplete work with no live lease. A
      // stale completion token must be fenced and cannot turn it accepted.
      const staleCompletion = await Effect.runPromise(
        provideDatabaseLayers(
          DaPayloadPublicationsDB.recordAttempt({
            headerHash: Buffer.from(fixture.headerHash, "hex"),
            peer: committeePeers[2]!,
            status: "accepted",
            retryBackoffMs: 1,
            retryBackoffMaxMs: 1,
            lease: { owner: "stale-process", token: "stale-token" },
          }),
        ),
      );
      expect(staleCompletion).toBe(false);

      await committeeNodes[2]!.start();
      const cachedTransport = await getDaPublicationTransportForTest(manifest);
      expect(
        (
          await probeDaEnvelopeCapabilities({
            manifest,
            mode: "identity",
            transport: cachedTransport,
          })
        ).every((result) => result.capable),
      ).toBe(true);

      let recovered = interrupted;
      let recoveryAttempts = 0;
      for (
        let attempt = 0;
        attempt < 5 && recovered.backlog > 0;
        attempt += 1
      ) {
        await delay(10);
        recovered = await runReconciler(
          "phase5-joined-after-restart-" + attempt.toString(),
        );
        recoveryAttempts += recovered.attempted;
      }
      const convergedState = await publicationState(fixture.headerHash);
      expect(recoveryAttempts).toBeGreaterThanOrEqual(1);
      expect(convergedState).toEqual({
        acceptedPeers: 3,
        peerBacklog: 0,
      });
      await expect(
        Effect.runPromise(
          provideDatabaseLayers(
            DaPayloadPublicationsDB.conflictCount(RETENTION_DAYS),
          ),
        ),
      ).resolves.toBe(0);
      expect(recovered).toMatchObject({
        configured: true,
        conflicts: 0,
        backlog: 0,
      });
      await expect(
        stores[2]!.getDaPayload(fixture.headerHash),
      ).resolves.toMatchObject({
        payloadSchemaVersion: 2,
        payloadCborHex: fixture.payloadCbor.toString("hex"),
      });
    } finally {
      await readinessTransport?.close?.().catch(() => undefined);
      await closeDaLibp2pPublicationTransport().catch(() => undefined);
      await Promise.all(
        committeeNodes.map((node) => node.stop().catch(() => undefined)),
      );
      await Effect.runPromise(provideDatabaseLayers(DaPayloadsDB.clear)).catch(
        () => undefined,
      );
      for (const key of ENV_KEYS) {
        const previous = previousEnv.get(key);
        if (previous === undefined) {
          delete process.env[key];
        } else {
          process.env[key] = previous;
        }
      }
      await rm(temp, { recursive: true, force: true });
    }
  }, 60_000);
});

const runReconciler = (leaseOwner: string) =>
  Effect.runPromise(
    provideDatabaseLayers(
      reconcileDaPublicationsOnce({ leaseOwner, limit: 3 }),
    ),
  );

const publicationState = (headerHash: string) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.all({
        acceptedPeers: DaPayloadPublicationsDB.acceptedCount(
          Buffer.from(headerHash, "hex"),
        ),
        peerBacklog: DaPayloadPublicationsDB.backlogCount(RETENTION_DAYS),
      }),
    ),
  );

const runtimeManifest = (
  manifest: DaProducerPublicationManifest,
  peers: readonly DaProducerCommitteePeer[],
): Record<string, unknown> => ({
  schemaVersion: "midgard-da-libp2p-runtime-manifest-v2",
  deployment: {
    fingerprint: manifest.deploymentFingerprint,
    contract_deployment_manifest_id: manifest.deploymentFingerprint,
    contract_deployment_info_sha256: "cd".repeat(32),
    identity_source: "contract_deployment_manifest_id",
  },
  da_transport: {
    kind: "libp2p",
    no_http_da_transport: true,
    listen_multiaddrs: manifest.listenMultiaddrs,
    announce_multiaddrs: manifest.announceMultiaddrs,
    bootstrap_multiaddrs: manifest.bootstrapMultiaddrs,
    gossip: {
      strict_sign: true,
      emit_self: false,
      allowed_topics_only: true,
      max_gossip_message_bytes: manifest.maxGossipMessageBytes,
    },
    limits: {
      max_payload_bytes: manifest.maxPayloadBytes,
      max_inline_response_bytes: manifest.maxInlineResponseBytes,
      max_chunk_bytes: manifest.maxChunkBytes,
      max_streams_per_peer: manifest.maxStreamsPerPeer,
      request_timeout_ms: manifest.requestTimeoutMs,
    },
  },
  da_committee: {
    threshold: manifest.threshold,
    members: peers.map((peer) => ({
      signer_index: peer.signerIndex,
      da_vkey: peer.daVkey,
      peer_id: peer.peerId,
      multiaddrs: peer.multiaddrs,
      roles: peer.roles,
    })),
  },
});

const insertFromFixture = (
  fixture: Awaited<ReturnType<typeof makePayloadFixture>>,
): DaPayloadsDB.InsertInput => ({
  [DaPayloadsDB.Columns.HEADER_HASH]: Buffer.from(fixture.headerHash, "hex"),
  [DaPayloadsDB.Columns.VERSION]: 2,
  [DaPayloadsDB.Columns.PAYLOAD_CBOR]: fixture.payloadCbor,
  [DaPayloadsDB.Columns.PAYLOAD_SHA256]: computeDaSha256Hash(
    fixture.payloadCbor,
  ),
  [DaPayloadsDB.Columns.UTXOS_ROOT]: fixture.header.utxosRoot,
  [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
    fixture.header.forcedTransactionsRoot,
  [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: fixture.header.transactionsRoot,
  [DaPayloadsDB.Columns.DEPOSITS_ROOT]: fixture.header.depositsRoot,
  [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: fixture.header.withdrawalsRoot,
  [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]:
    fixture.header.transitionTraceRoot,
  [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: fixture.header.eventToStepRoot,
  [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: fixture.header.withdrawalCount,
  [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]:
    fixture.header.forcedTransactionCount,
  [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]:
    fixture.header.l2TransactionCount,
  [DaPayloadsDB.Columns.DEPOSIT_COUNT]: fixture.header.depositCount,
  [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: fixture.header.totalEventCount,
  [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]:
    fixture.header.transitionStepCount,
  [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(1),
  [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(2),
});

const delay = (milliseconds: number): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, milliseconds));

const reserveLoopbackPort = (): Promise<number> =>
  new Promise((resolve, reject) => {
    const server = createServer();
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      if (address === null || typeof address === "string") {
        reject(new Error("failed to reserve loopback port"));
        return;
      }
      server.close((error) =>
        error === undefined ? resolve(address.port) : reject(error),
      );
    });
  });
