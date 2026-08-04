import {
  computeDaSha256Hash,
  DaRequestResponseProtocol,
  decodeDaEventToStepByEventResponseV1Cbor,
  decodeDaProofBundleByHeaderResponseV1Cbor,
  decodeDaTraceStepByIndexResponseV1Cbor,
  encodeDaEventToStepByEventRequestV1Cbor,
  encodeDaProofBundleByHeaderRequestV1Cbor,
  encodeDaTraceStepByIndexRequestV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import type { DaLibp2pStreamHandler } from "../src/da/libp2p/DaLibp2pNode.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "../src/da/libp2p/DaProtocols.js";
import {
  encodeDaStreamFrame,
  readSingleDaStreamFrame,
} from "../src/da/libp2p/DaStreamCodec.js";
import {
  createDaLibp2pProofRequestHandlers,
  DaLibp2pProofProtocolHandlers,
} from "../src/da/libp2p/proof-protocols.js";
import type {
  Header,
  PayloadRootSet,
  StateQueueHeaderRecord,
} from "../src/domain.js";
import { JsonFileWatcherStore } from "../src/store.js";
import {
  IDENTITY_TX_PROJECTOR,
  makePayloadFixture,
  tempDir,
} from "./helpers.js";

const deploymentFingerprint = "01".repeat(32);
const deploymentFingerprintBytes = Buffer.from(deploymentFingerprint, "hex");

describe("DA libp2p proof protocol handlers", () => {
  it("serves proof bundles, trace openings, and event-to-step membership proofs from verified payloads", async () => {
    const { handlers, store } = await makeHandlers();
    const { payload, payloadCbor, header, headerHash } =
      await makePayloadFixture();
    const payloadHash = computeDaSha256Hash(payloadCbor);
    await saveVerifiedPayload({
      store,
      payloadCbor,
      payloadHash,
      header,
      headerHash,
    });

    const proofBundle = decodeDaProofBundleByHeaderResponseV1Cbor(
      await handlers.handleProofBundleByHeader(
        encodeDaProofBundleByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          maxInlineBytes: 4096,
        }),
      ),
    );
    expect(proofBundle.status).toBe("found_inline");
    expect(proofBundle.reasonCode).toBeNull();
    expect(proofBundle.proofBundleHash).toHaveLength(32);
    expect(proofBundle.proofBundleBytes).not.toBeNull();
    expect(
      computeDaSha256Hash(proofBundle.proofBundleBytes!).equals(
        proofBundle.proofBundleHash!,
      ),
    ).toBe(true);
    expect(proofBundle.chunkManifest).toBeNull();

    const trace = decodeDaTraceStepByIndexResponseV1Cbor(
      await handlers.handleTraceStepByIndex(
        encodeDaTraceStepByIndexRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          stepIndex: 2,
        }),
      ),
    );
    expect(trace.status).toBe("found");
    const openedStep = LucidData.from(
      trace.transitionStepBytes!.toString("hex"),
      SDK.TransitionStepSchema as never,
    ) as SDK.TransitionStep;
    expect(openedStep.step_index).toBe(2n);
    const traceProof = LucidData.from(
      trace.membershipProofBytes!.toString("hex"),
      SDK.TransitionTraceMembershipProofSchema as never,
    ) as SDK.IndexedTraceProof;
    expect(traceProof).toMatchObject({
      domain: SDK.ROOT_DOMAINS.transitionTrace,
      root: header.transitionTraceRoot,
      key: 2n,
    });

    const [eventKeyHex, eventToStepValueHex] =
      payload.block_body.event_to_step[0]!;
    const eventToStep = decodeDaEventToStepByEventResponseV1Cbor(
      await handlers.handleEventToStepByEvent(
        encodeDaEventToStepByEventRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          eventKey: Buffer.from(eventKeyHex, "hex"),
        }),
      ),
    );
    expect(eventToStep.status).toBe("found");
    expect(eventToStep.eventToStepEntryBytes?.toString("hex")).toBe(
      eventToStepValueHex,
    );
    const eventProof = LucidData.from(
      eventToStep.membershipOrNonmembershipProofBytes!.toString("hex"),
      SDK.EventToStepProofSchema as never,
    ) as SDK.EventToStepProof;
    expect(eventProof).toMatchObject({
      EventToStepMembership: {
        membership: {
          domain: SDK.ROOT_DOMAINS.eventToStep,
          root: header.eventToStepRoot,
        },
      },
    });
  });

  it("serves event-to-step non-membership proofs for absent event keys", async () => {
    const { handlers, store } = await makeHandlers();
    const { payloadCbor, header, headerHash } = await makePayloadFixture();
    await saveVerifiedPayload({
      store,
      payloadCbor,
      payloadHash: computeDaSha256Hash(payloadCbor),
      header,
      headerHash,
    });
    const absentEventKey = Buffer.from(
      LucidData.to(
        { L2TransactionEventKey: { tx_id: "99".repeat(32) } } as never,
        SDK.EventKeySchema as never,
      ),
      "hex",
    );

    const response = decodeDaEventToStepByEventResponseV1Cbor(
      await handlers.handleEventToStepByEvent(
        encodeDaEventToStepByEventRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          eventKey: absentEventKey,
        }),
      ),
    );

    expect(response.status).toBe("found");
    expect(response.eventToStepEntryBytes).toBeNull();
    const eventProof = LucidData.from(
      response.membershipOrNonmembershipProofBytes!.toString("hex"),
      SDK.EventToStepProofSchema as never,
    ) as SDK.EventToStepProof;
    expect(eventProof).toMatchObject({
      EventToStepNonMembership: {
        non_membership: {
          domain: SDK.ROOT_DOMAINS.eventToStep,
          root: header.eventToStepRoot,
        },
      },
    });
  });

  it("fails closed for unverified payloads, root mismatches, and deployment mismatches", async () => {
    const { handlers, store } = await makeHandlers();
    const { payloadCbor, header, headerHash } = await makePayloadFixture();
    const payloadHash = computeDaSha256Hash(payloadCbor);
    await store.saveDaPayload({
      deploymentFingerprint,
      headerHash,
      payloadCborHex: payloadCbor.toString("hex"),
      payloadSha256: payloadHash.toString("hex"),
      sourcePeerId: "libp2p-fixture",
      fetchedAt: "2026-06-21T00:00:00.000Z",
      rootSummary: rootSummaryFromHeader(header),
      validationStatus: "fetched",
    });

    const unverified = decodeDaTraceStepByIndexResponseV1Cbor(
      await handlers.handleTraceStepByIndex(
        encodeDaTraceStepByIndexRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          stepIndex: 0,
        }),
      ),
    );
    expect(unverified.status).toBe("rejected");

    await saveVerifiedPayload({
      store,
      payloadCbor,
      payloadHash,
      header,
      headerHash,
      rootSummary: {
        ...rootSummaryFromHeader(header),
        transitionTraceRoot: "ff".repeat(32),
      },
    });
    const mismatched = decodeDaTraceStepByIndexResponseV1Cbor(
      await handlers.handleTraceStepByIndex(
        encodeDaTraceStepByIndexRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          stepIndex: 0,
        }),
      ),
    );
    expect(mismatched.status).toBe("rejected");

    await saveVerifiedPayload({
      store,
      payloadCbor,
      payloadHash,
      header,
      headerHash,
    });
    await store.upsertStateQueueHeader(
      stateQueueHeaderRecord({
        header: {
          ...header,
          eventToStepRoot: "00".repeat(32),
        },
        headerHash,
      }),
    );
    const committedMismatch = decodeDaTraceStepByIndexResponseV1Cbor(
      await handlers.handleTraceStepByIndex(
        encodeDaTraceStepByIndexRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          stepIndex: 0,
        }),
      ),
    );
    expect(committedMismatch.status).toBe("rejected");

    const wrongDeployment = decodeDaProofBundleByHeaderResponseV1Cbor(
      await handlers.handleProofBundleByHeader(
        encodeDaProofBundleByHeaderRequestV1Cbor({
          deploymentFingerprint: Buffer.alloc(32, 0x02),
          headerHash: Buffer.from(headerHash, "hex"),
          maxInlineBytes: 4096,
        }),
      ),
    );
    expect(wrongDeployment).toMatchObject({
      status: "rejected",
      reasonCode: "deployment_fingerprint_mismatch",
    });
  });

  it("gates stream request handlers by registered requester roles", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const registry = new DaPeerRegistry([
      {
        peerId: "peer-retrieval",
        roles: ["retrieval"],
        multiaddrs: [],
        bootstrap: false,
      },
      {
        peerId: "peer-coordinator",
        roles: ["coordinator"],
        multiaddrs: [],
        bootstrap: false,
      },
      {
        peerId: "peer-producer",
        roles: ["producer"],
        multiaddrs: [],
        bootstrap: false,
      },
    ]);
    const handlerMap = createDaLibp2pProofRequestHandlers({
      deploymentFingerprint,
      store,
      limits: streamLimits,
      registry,
    });
    const protocolId = createDaProtocolAllowlist(
      deploymentFingerprint,
    ).protocolIdByName.get(DaRequestResponseProtocol.proofBundleByHeader)!;
    const handler = handlerMap.get(protocolId);
    expect(handler).toBeDefined();
    const requestCbor = encodeDaProofBundleByHeaderRequestV1Cbor({
      deploymentFingerprint: deploymentFingerprintBytes,
      headerHash: Buffer.alloc(28, 0xaa),
      maxInlineBytes: 4096,
    });

    const allowed = decodeDaProofBundleByHeaderResponseV1Cbor(
      await invokeStreamHandler(
        handler!,
        protocolId,
        requestCbor,
        "peer-retrieval",
      ),
    );
    expect(allowed).toMatchObject({
      status: "not_found",
      reasonCode: "stored_payload_not_found",
    });

    const unauthorized = decodeDaProofBundleByHeaderResponseV1Cbor(
      await invokeStreamHandler(
        handler!,
        protocolId,
        requestCbor,
        "peer-coordinator",
      ),
    );
    expect(unauthorized).toMatchObject({
      status: "rejected",
      reasonCode: "unauthorized_peer_role",
    });

    const producer = decodeDaProofBundleByHeaderResponseV1Cbor(
      await invokeStreamHandler(
        handler!,
        protocolId,
        requestCbor,
        "peer-producer",
      ),
    );
    expect(producer).toMatchObject({
      status: "rejected",
      reasonCode: "unauthorized_peer_role",
    });

    const unknown = decodeDaProofBundleByHeaderResponseV1Cbor(
      await invokeStreamHandler(
        handler!,
        protocolId,
        requestCbor,
        "peer-unknown",
      ),
    );
    expect(unknown).toMatchObject({
      status: "rejected",
      reasonCode: "unknown_peer",
    });
  });
});

const streamLimits = {
  maxPayloadBytes: 1_048_576,
  maxInlineResponseBytes: 65_536,
  maxChunkBytes: 65_536,
  maxStreamsPerPeer: 8,
  requestTimeoutMs: 1_000,
};

const makeHandlers = async (): Promise<{
  readonly handlers: DaLibp2pProofProtocolHandlers;
  readonly store: JsonFileWatcherStore;
}> => {
  const store = await JsonFileWatcherStore.open(await tempDir());
  const handlers = new DaLibp2pProofProtocolHandlers({
    deploymentFingerprint,
    store,
    transactionProjector: IDENTITY_TX_PROJECTOR,
  });
  return { handlers, store };
};

const invokeStreamHandler = async (
  handler: DaLibp2pStreamHandler,
  protocolId: string,
  requestCbor: Uint8Array,
  remotePeerId: string,
): Promise<Buffer> => {
  const sent: Buffer[] = [];
  const stream = {
    async *[Symbol.asyncIterator](): AsyncGenerator<Buffer> {
      yield encodeDaStreamFrame(requestCbor, {
        maxFrameBytes: streamLimits.maxPayloadBytes,
      });
    },
    send(data: Uint8Array): boolean {
      sent.push(Buffer.from(data));
      return true;
    },
  };
  await handler({
    protocolId,
    protocolName: DaRequestResponseProtocol.proofBundleByHeader,
    stream,
    connection: {},
    remotePeerId,
  });
  expect(sent).toHaveLength(1);
  return readSingleDaStreamFrame(sent, {
    maxFrameBytes: streamLimits.maxPayloadBytes,
  });
};

const saveVerifiedPayload = async ({
  store,
  payloadCbor,
  payloadHash,
  header,
  headerHash,
  rootSummary = rootSummaryFromHeader(header),
}: {
  readonly store: JsonFileWatcherStore;
  readonly payloadCbor: Buffer;
  readonly payloadHash: Buffer;
  readonly header: Header;
  readonly headerHash: string;
  readonly rootSummary?: PayloadRootSet;
}): Promise<void> => {
  await store.saveDaPayload({
    deploymentFingerprint,
    headerHash,
    payloadCborHex: payloadCbor.toString("hex"),
    payloadSha256: payloadHash.toString("hex"),
    sourcePeerId: "libp2p-fixture",
    fetchedAt: "2026-06-21T00:00:00.000Z",
    verifiedAt: "2026-06-21T00:00:01.000Z",
    rootSummary,
    validationStatus: "verified",
  });
  await store.upsertStateQueueHeader(
    stateQueueHeaderRecord({ header, headerHash }),
  );
};

const stateQueueHeaderRecord = ({
  header,
  headerHash,
}: {
  readonly header: Header;
  readonly headerHash: string;
}): StateQueueHeaderRecord => ({
  deploymentFingerprint,
  headerHash,
  stateQueueOutRef: "aa".repeat(32) + "#0",
  blockAssetName: `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`,
  header,
  computedHeaderHash: headerHash,
  daAttestation: SDK.NO_DA_ATTESTATION,
  observedChainPoint: {
    slot: 1,
    blockHash: "bb".repeat(32),
    depth: 10,
    providerSource: "fixture",
  },
  finalized: true,
  status: "attested",
  validationErrors: [],
  updatedAt: "2026-06-21T00:00:02.000Z",
});

const rootSummaryFromHeader = (header: Header): PayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
});
