import { createServer } from "node:net";

import { MIDGARD_CONSENSUS_PROFILE_V1_ID } from "@al-ft/midgard-core/consensus-profile-v1";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
  decodeDaPayloadSubmitRequestV1Cbor,
  encodeDaPayloadSubmitResponseV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  createDaLibp2pProducerTransport,
  type DaProducerCommitteePeer,
  type DaProducerPublicationManifest,
  probeDaEnvelopeCapabilities,
  publishDaPayloadInsert,
} from "@/da/libp2p-producer.js";
import { DaPayloadsDB } from "@/database/index.js";

import type {
  Libp2pDaPeerConfig,
  Libp2pDaTransportConfig,
} from "../../da-committee-node/src/config.js";
import {
  readSingleDaStreamFrame,
  writeDaStreamFrame,
} from "../../da-committee-node/src/da/libp2p/DaStreamCodec.js";
import {
  createDaLibp2pPayloadRequestHandlers,
  DaLibp2pNode,
  DaPayloadSubmitAdmission,
  loadDaLibp2pIdentity,
} from "../../da-committee-node/src/da/libp2p/index.js";
import { hashBlockHeaderV1 } from "../../da-committee-node/src/l1/state-queue-scanner.js";
import { JsonFileWatcherStore } from "../../da-committee-node/src/store.js";
import {
  makePayloadFixture,
  tempDir,
} from "../../da-committee-node/tests/helpers.js";

const DEPLOYMENT = "a5".repeat(32);

describe("real multi-peer DA publication", () => {
  it("publishes a zstd V1 envelope to threshold, returns before a dead peer timeout, and recovers after peer restart", async () => {
    const producerSeed = `seed:${"00".repeat(31)}09`;
    const committeeSeeds = [
      `seed:${"00".repeat(31)}01`,
      `seed:${"00".repeat(31)}02`,
      `seed:${"00".repeat(31)}03`,
    ] as const;
    const producerIdentity = await loadDaLibp2pIdentity(producerSeed);
    const committeeIdentities = await Promise.all(
      committeeSeeds.map((seed) => loadDaLibp2pIdentity(seed)),
    );
    const ports = await Promise.all(
      committeeSeeds.map(() => reserveLoopbackPort()),
    );
    const committeePeers: readonly (DaProducerCommitteePeer &
      Libp2pDaPeerConfig)[] = committeeIdentities.map((identity, index) => ({
      signerIndex: index,
      daVkey: (index + 1).toString(16).padStart(2, "0").repeat(32),
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
    const stores = await Promise.all(
      committeeSeeds.map(async () =>
        JsonFileWatcherStore.open(await tempDir()),
      ),
    );
    let slowThirdPeer = false;
    let rejectEnvelopeOnThirdPeer = false;
    const committeeNodes = committeeSeeds.map((seed, index) => {
      const config: Libp2pDaTransportConfig = {
        kind: "libp2p",
        deploymentFingerprint: DEPLOYMENT,
        noHttpDaTransport: true,
        threshold: 2,
        listenMultiaddrs: [`/ip4/127.0.0.1/tcp/${ports[index]!.toString()}`],
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
        retentionDays: DA_TRANSPORT_LIMITS_V1.minimumRetentionDays,
        peers: [...committeePeers, producerPeer],
      };
      const requestHandlers = new Map(
        createDaLibp2pPayloadRequestHandlers({
          deploymentFingerprint: DEPLOYMENT,
          store: stores[index]!,
          limits: config.limits,
          payloadSubmitAdmission: new DaPayloadSubmitAdmission(1),
        }),
      );
      const submitProtocolId = [...requestHandlers.keys()].find((key) =>
        key.includes("payload-submit"),
      );
      if (submitProtocolId === undefined) {
        throw new Error("missing payload-submit protocol handler");
      }
      const submitHandler = requestHandlers.get(submitProtocolId)!;
      requestHandlers.set(submitProtocolId, async (context) => {
        if (index === 2 && slowThirdPeer) {
          await new Promise((resolve) => setTimeout(resolve, 14_000));
        }
        if (index === 2 && rejectEnvelopeOnThirdPeer) {
          const request = decodeDaPayloadSubmitRequestV1Cbor(
            await readSingleDaStreamFrame(context.stream, {
              maxFrameBytes: config.limits.maxPayloadBytes,
            }),
          );
          await writeDaStreamFrame(
            context.stream,
            encodeDaPayloadSubmitResponseV1Cbor({
              status: "rejected",
              headerHash: request.headerHash,
              payloadHash: request.payloadHash,
              reasonCode: "payload_decode_failed",
              retryAfterMs: null,
            }),
            { maxFrameBytes: config.limits.maxPayloadBytes, close: true },
          );
        } else {
          await submitHandler(context);
        }
      });
      return new DaLibp2pNode({
        config,
        privateKeySource: seed,
        requestHandlers,
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
        `/ip4/127.0.0.1/tcp/0/p2p/${producerIdentity.peerId}`,
      ],
      bootstrapMultiaddrs: [],
      committeePeers,
    };
    let transport:
      | Awaited<ReturnType<typeof createDaLibp2pProducerTransport>>
      | undefined;
    try {
      for (const node of committeeNodes) {
        await node.start();
      }
      transport = await createDaLibp2pProducerTransport(manifest, {
        mode: "dial-only",
      });
      // `DaLibp2pNode.start()` completes after handlers are registered, but
      // the TCP listeners may still be accepting their first inbound dials.
      // A real capabilities round-trip is the readiness barrier for the
      // initial publication; without it, the first three concurrent submits
      // can spend the entire pinned deadline in connection setup and produce
      // a false below-threshold failure.  This does not alter the protocol
      // deadline or quorum — it proves the peers are reachable before timing
      // the publication itself.
      const capabilities = await probeDaEnvelopeCapabilities({
        manifest,
        mode: "zstd",
        transport,
      });
      expect(capabilities.every((result) => result.capable)).toBe(true);
      const fixture = await makePayloadFixture();
      const envelope = await wrapDaPayloadV1(fixture.innerPayloadCbor, {
        mode: "zstd",
        zstdLevel: 3,
      });
      const insert = insertFromFixture(fixture, envelope);

      const initial = await publishDaPayloadInsert({
        insert,
        manifest,
        transport,
      });
      expect(await initial.allPeerResults).toHaveLength(3);
      await Promise.all(
        stores.map((store) =>
          expect(store.getDaPayload(fixture.headerHash)).resolves.toMatchObject(
            {
              payloadSchemaVersion: 1,
              payloadCborHex: envelope.toString("hex"),
            },
          ),
        ),
      );

      rejectEnvelopeOnThirdPeer = true;
      const mixed = await publishDaPayloadInsert({
        insert,
        manifest,
        transport,
      });
      expect(mixed.acceptedPeers).toBe(2);
      expect(await mixed.allPeerResults).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            peerId: committeePeers[2]!.peerId,
            status: "rejected",
            error: "payload_decode_failed",
          }),
        ]),
      );

      const secondHeader = {
        ...fixture.header,
        startTime: fixture.header.startTime + 10n,
        endTime: fixture.header.endTime + 10n,
      };
      const secondHeaderHash = hashBlockHeaderV1(secondHeader);
      const secondPayloadCbor = SDK.encodeDaPayloadV1({
        ...fixture.payload,
        block_body: {
          ...fixture.payload.block_body,
          header_hash: secondHeaderHash,
          header: secondHeader,
        },
      });
      const secondEnvelope = await wrapDaPayloadV1(secondPayloadCbor, {
        mode: "zstd",
        zstdLevel: 3,
      });
      rejectEnvelopeOnThirdPeer = false;
      const inverse = await publishDaPayloadInsert({
        insert: {
          ...insertFromFixture(fixture, secondEnvelope),
          [DaPayloadsDB.Columns.HEADER_HASH]: Buffer.from(
            secondHeaderHash,
            "hex",
          ),
          [DaPayloadsDB.Columns.VERSION]: 1,
        },
        manifest,
        transport,
      });
      expect(
        (await inverse.allPeerResults)?.every(
          (result) => result.status === "accepted",
        ),
      ).toBe(true);

      slowThirdPeer = true;
      const startedAt = performance.now();
      const degraded = await publishDaPayloadInsert({
        insert,
        manifest,
        transport,
      });
      const thresholdDurationMs = performance.now() - startedAt;
      expect(degraded.acceptedPeers).toBe(2);
      expect(thresholdDurationMs).toBeLessThan(2_000);
      const allStartedAt = performance.now();
      const degradedAll = await degraded.allPeerResults;
      const stragglerDurationMs = performance.now() - allStartedAt;
      expect(degradedAll).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            peerId: committeePeers[2]!.peerId,
            status: "transport_error",
          }),
        ]),
      );
      expect(stragglerDurationMs).toBeGreaterThan(10_000);

      await committeeNodes[2]!.stop();
      slowThirdPeer = false;
      await committeeNodes[2]!.start();
      const recoveredCapabilities = await probeDaEnvelopeCapabilities({
        manifest,
        mode: "zstd",
        transport,
      });
      expect(
        recoveredCapabilities.find(
          (result) => result.peerId === committeePeers[2]!.peerId,
        ),
      ).toMatchObject({ capable: true });
      const recovered = await publishDaPayloadInsert({
        insert,
        manifest,
        transport,
      });
      expect(await recovered.allPeerResults).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            peerId: committeePeers[2]!.peerId,
            status: "duplicate",
          }),
        ]),
      );
    } finally {
      await transport?.close?.();
      await Promise.all(
        committeeNodes.map((node) => node.stop().catch(() => undefined)),
      );
    }
  }, 70_000);
});

const insertFromFixture = (
  fixture: Awaited<ReturnType<typeof makePayloadFixture>>,
  envelope: Buffer,
): DaPayloadsDB.InsertInput => ({
  [DaPayloadsDB.Columns.HEADER_HASH]: Buffer.from(fixture.headerHash, "hex"),
  [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE_V1_ID,
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
  [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
  [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: fixture.header.withdrawalCount,
  [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]:
    fixture.header.forcedTransactionCount,
  [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]:
    fixture.header.l2TransactionCount,
  [DaPayloadsDB.Columns.DEPOSIT_COUNT]: fixture.header.depositCount,
  [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: fixture.header.totalEventCount,
  [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]:
    fixture.header.transitionStepCount,
  [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]: 0n,
  [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(1),
  [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(2),
});

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
      const port = address.port;
      server.close((error) =>
        error === undefined ? resolve(port) : reject(error),
      );
    });
  });
