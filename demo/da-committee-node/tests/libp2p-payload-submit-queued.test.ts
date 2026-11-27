import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import {
  DA_TRANSPORT_LIMITS,
  decodeDaPayloadSubmitResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { Libp2pDaTransportConfig } from "../src/config.js";
import { DaLibp2pNode } from "../src/da/libp2p/DaLibp2pNode.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import {
  createDaLibp2pPayloadRequestHandlers,
  DaPayloadSubmitAdmission,
} from "../src/da/libp2p/payload-source.js";
import { makePayloadFixture } from "./helpers.js";
import { openTestCommitteeStore } from "./helpers/committee-store.js";
import { reserveLoopbackPort } from "./libp2p-attestation-exchange.state-queue-record.js";
import {
  deploymentFingerprint,
  encodeSubmit,
  payloadSubmitProtocolId,
} from "./libp2p-payload-protocols.make-mock-stream.js";

const REQUEST_TIMEOUT_MS = 5_000;

const running: DaLibp2pNode[] = [];
afterEach(async () => {
  await Promise.all(running.splice(0).map((node) => node.stop()));
});

/** A committee member and a peer that submits to it, over real libp2p. */
const startPair = async (
  requestHandlers: ReturnType<typeof createDaLibp2pPayloadRequestHandlers>,
) => {
  const keySources = [0xc1, 0xc2].map(
    (seed) => `seed:${"00".repeat(31)}${seed.toString(16)}`,
  );
  const identities = await Promise.all(keySources.map(loadDaLibp2pIdentity));
  const ports = await Promise.all(keySources.map(() => reserveLoopbackPort()));
  const multiaddrs = identities.map(
    (identity, index) =>
      `/ip4/127.0.0.1/tcp/${ports[index]!.toString()}/p2p/${identity.peerId}`,
  );
  const configs = keySources.map(
    (_, index): Libp2pDaTransportConfig => ({
      kind: "libp2p",
      deploymentFingerprint,
      noHttpDaTransport: true,
      threshold: 1,
      listenMultiaddrs: [`/ip4/127.0.0.1/tcp/${ports[index]!.toString()}`],
      announceMultiaddrs: [multiaddrs[index]!],
      bootstrapMultiaddrs: [multiaddrs[1 - index]!],
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
        requestTimeoutMs: REQUEST_TIMEOUT_MS,
      },
      retentionDays: DA_TRANSPORT_LIMITS.minimumRetentionDays,
      peers: identities.map((identity, peer) => ({
        signerIndex: peer,
        daVkey: (peer + 1).toString(16).padStart(2, "0").repeat(32),
        peerId: identity.peerId,
        multiaddrs: [multiaddrs[peer]!],
        roles: ["committee", "retrieval"],
      })),
    }),
  );
  const member = new DaLibp2pNode({
    config: configs[0]!,
    privateKeySource: keySources[0]!,
    requestHandlers,
  });
  const submitter = new DaLibp2pNode({
    config: configs[1]!,
    privateKeySource: keySources[1]!,
    requestHandlers: new Map(),
  });
  running.push(member, submitter);
  await member.start();
  await submitter.start();
  return {
    submitter,
    memberEntry: DaPeerRegistry.fromConfig(configs[1]!).getBySignerIndex(0)!,
  };
};

describe("a payload submit queued behind admission over real libp2p", () => {
  it("is answered once admitted, though the submitter finished writing while it waited", async () => {
    const fixture = await makePayloadFixture();
    const store = await openTestCommitteeStore();
    const admission = new DaPayloadSubmitAdmission(1);
    const handlers = new Map(
      createDaLibp2pPayloadRequestHandlers({
        deploymentFingerprint,
        store,
        limits: {
          ...DA_TRANSPORT_LIMITS,
          requestTimeoutMs: REQUEST_TIMEOUT_MS,
        },
        payloadSubmitAdmission: admission,
      }),
    );
    const submit = handlers.get(payloadSubmitProtocolId)!;
    let inbound: { readonly remoteWriteStatus?: string } | undefined;
    handlers.set(payloadSubmitProtocolId, (context) => {
      inbound = context.stream as typeof inbound;
      return submit(context);
    });
    const { submitter, memberEntry } = await startPair(handlers);

    // Another submit holds the only slot.
    let releaseSlot!: () => void;
    const held = admission.run(
      () =>
        new Promise<void>((resolve) => {
          releaseSlot = resolve;
        }),
    );
    const startedAt = performance.now();
    const response = submitter.request({
      peer: memberEntry,
      protocolId: payloadSubmitProtocolId,
      payload: encodeSubmit(fixture.headerHash, fixture.payloadCbor),
    });
    // The submitter has sent its request and closed its writable end, and
    // the member has not begun to read it.
    await vi.waitFor(() => expect(inbound?.remoteWriteStatus).toBe("closed"), {
      timeout: REQUEST_TIMEOUT_MS / 2,
      interval: 10,
    });
    releaseSlot();
    await held;

    const answer = decodeDaPayloadSubmitResponseCbor(await response);
    expect(answer).toMatchObject({ status: "accepted", reasonCode: null });
    expect(performance.now() - startedAt).toBeLessThan(REQUEST_TIMEOUT_MS);
    await expect(store.getDaPayload(fixture.headerHash)).resolves.toMatchObject(
      { payloadCborHex: fixture.payloadCbor.toString("hex") },
    );
    expect(admission.active).toBe(0);
  }, 30_000);
});
