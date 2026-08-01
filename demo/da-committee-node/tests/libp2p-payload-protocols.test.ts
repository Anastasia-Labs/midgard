import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { DaRequestTimeoutError } from "@al-ft/midgard-core/da-request-deadline";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS_V1,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaAttestationsByHeaderResponseV1Cbor,
  decodeDaCapabilitiesResponseV1Cbor,
  decodeDaMetadataByHeaderResponseV1Cbor,
  decodeDaPayloadByHeaderResponseV1Cbor,
  decodeDaPayloadChunkResponseV1Cbor,
  decodeDaPayloadSubmitResponseV1Cbor,
  encodeDaAttestationsByHeaderRequestV1Cbor,
  encodeDaAttestationsByHeaderResponseV1Cbor,
  encodeDaCapabilitiesRequestV1Cbor,
  encodeDaPayloadByHeaderRequestV1Cbor,
  encodeDaPayloadChunkRequestV1Cbor,
  encodeDaPayloadSubmitRequestV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  DaLibp2pAttestationExchange,
  StoreBackedDaAttestationProtocol,
} from "../src/da/libp2p/attestations.js";
import type {
  DaLibp2pStream,
  DaLibp2pStreamHandler,
} from "../src/da/libp2p/DaLibp2pNode.js";
import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import {
  encodeDaStreamFrame,
  readSingleDaStreamFrame,
} from "../src/da/libp2p/DaStreamCodec.js";
import {
  DaLibp2pPayloadProtocolError,
  DaLibp2pPayloadProtocolHandlers,
} from "../src/da/libp2p/payload-protocols.js";
import {
  createDaLibp2pPayloadRequestHandlers,
  DaPayloadSubmitAdmission,
  processWideDaPayloadSubmitAdmission,
} from "../src/da/libp2p/payload-source.js";
import type { DaStoredPayloadRootSetV1, HeaderV1 } from "../src/domain.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { makePayloadFixture, tempDir } from "./helpers.js";

const deploymentFingerprint = "01".repeat(32);
const deploymentFingerprintBytes = Buffer.from(deploymentFingerprint, "hex");
const payloadSubmitProtocolId = daRequestResponseProtocolId(
  deploymentFingerprint,
  DaRequestResponseProtocol.payloadSubmit,
);

describe("canonical V1 DA libp2p payload protocols", () => {
  it("advertises only the exact V1 payload and envelope capabilities", async () => {
    const { handlers } = await makeHandlers({
      maxChunkBytes: 1_234,
      maxStreamsPerPeer: 7,
      requestTimeoutMs: 4_321,
    });
    const capabilities = decodeDaCapabilitiesResponseV1Cbor(
      await handlers.handleCapabilities(
        encodeDaCapabilitiesRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
        }),
      ),
    );

    expect(capabilities).toMatchObject({
      transportProtocolVersion: 1,
      payloadSchemaVersions: [1],
      envelopeContentEncodings: [0, 1],
      maxChunkBytes: 1_234,
      maxStreamsPerPeer: 7,
      requestTimeoutMs: 4_321,
    });
  });

  it("serializes expensive submit admission and releases the slot", async () => {
    const admission = new DaPayloadSubmitAdmission(1);
    let active = 0;
    let peak = 0;

    await Promise.all(
      Array.from({ length: 4 }, (_, index) =>
        admission.run(async () => {
          active += 1;
          peak = Math.max(peak, active);
          await new Promise((resolve) => setTimeout(resolve, 3 + index));
          active -= 1;
        }),
      ),
    );

    expect(peak).toBe(1);
    expect(admission.active).toBe(0);
  });

  it("shares process admission and times out a queued submit before frame read", async () => {
    expect(processWideDaPayloadSubmitAdmission.active).toBe(0);
    const fixture = await makePayloadFixture();
    const frame = encodeDaStreamFrame(
      encodeSubmit(fixture.headerHash, fixture.payloadCbor),
      { maxFrameBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes },
    );
    const firstStore = await JsonFileWatcherStore.open(await tempDir());
    const secondStore = await JsonFileWatcherStore.open(await tempDir());
    const firstLimits = {
      ...DA_TRANSPORT_LIMITS_V1,
      requestTimeoutMs: 1_000,
    };
    const queuedLimits = {
      ...DA_TRANSPORT_LIMITS_V1,
      requestTimeoutMs: 200,
    };
    const firstHandler = createDaLibp2pPayloadRequestHandlers({
      deploymentFingerprint,
      store: firstStore,
      limits: firstLimits,
    }).get(payloadSubmitProtocolId)!;
    const queuedHandler = createDaLibp2pPayloadRequestHandlers({
      deploymentFingerprint,
      store: secondStore,
      limits: queuedLimits,
    }).get(payloadSubmitProtocolId)!;
    const reads: string[] = [];
    let releaseFirst!: () => void;
    const firstGate = new Promise<void>((resolve) => {
      releaseFirst = resolve;
    });
    const firstMock = makeMockStream(
      (async function* () {
        reads.push("first");
        await firstGate;
        yield frame;
      })(),
    );
    const queuedMock = makeMockStream(
      (async function* () {
        reads.push("queued");
        yield frame;
      })(),
    );
    let firstRequest: Promise<void> | undefined;
    let queuedRequest: Promise<void> | undefined;
    try {
      firstRequest = invokePayloadSubmitHandler(firstHandler, firstMock.stream);
      await waitFor(
        () =>
          processWideDaPayloadSubmitAdmission.active === 1 &&
          reads.includes("first"),
        "first payload-submit admission",
      );
      queuedRequest = invokePayloadSubmitHandler(
        queuedHandler,
        queuedMock.stream,
      );

      await expect(queuedRequest).rejects.toBeInstanceOf(DaRequestTimeoutError);
      expect(reads).toEqual(["first"]);
      expect(queuedMock.state.aborted).toBeInstanceOf(DaRequestTimeoutError);
      expect(queuedMock.state.sent).toHaveLength(0);
      expect(queuedMock.state.closed).toBe(false);
      await expect(
        secondStore.getDaPayload(fixture.headerHash),
      ).resolves.toBeUndefined();
      expect(processWideDaPayloadSubmitAdmission.active).toBe(1);
    } finally {
      releaseFirst();
      await firstRequest?.catch(() => undefined);
      await queuedRequest?.catch(() => undefined);
    }

    expect(firstMock.state.aborted).toBeUndefined();
    expect(firstMock.state.closed).toBe(true);
    const response = decodeDaPayloadSubmitResponseV1Cbor(
      await readSingleDaStreamFrame(firstMock.state.sent, {
        maxFrameBytes: firstLimits.maxPayloadBytes,
      }),
    );
    expect(response).toMatchObject({ status: "accepted", reasonCode: null });
    await expect(
      firstStore.getDaPayload(fixture.headerHash),
    ).resolves.toMatchObject({
      payloadCborHex: fixture.payloadCbor.toString("hex"),
      validationStatus: "fetched",
    });
    expect(processWideDaPayloadSubmitAdmission.active).toBe(0);
  });

  it("aborts a stalled inbound submit, releases admission, and recovers", async () => {
    const store = await JsonFileWatcherStore.open(await tempDir());
    const admission = new DaPayloadSubmitAdmission(1);
    const limits = {
      ...DA_TRANSPORT_LIMITS_V1,
      requestTimeoutMs: 200,
    };
    const handler = createDaLibp2pPayloadRequestHandlers({
      deploymentFingerprint,
      store,
      limits,
      payloadSubmitAdmission: admission,
    }).get(payloadSubmitProtocolId)!;
    let rejectRead!: (error: Error) => void;
    const stalledRead = new Promise<never>((_resolve, reject) => {
      rejectRead = reject;
    });
    const stalledMock = makeMockStream(
      (async function* () {
        await stalledRead;
      })(),
      rejectRead,
    );

    await expect(
      invokePayloadSubmitHandler(handler, stalledMock.stream),
    ).rejects.toBeInstanceOf(DaRequestTimeoutError);
    expect(stalledMock.state.aborted).toBeInstanceOf(DaRequestTimeoutError);
    expect(stalledMock.state.sent).toHaveLength(0);
    expect(stalledMock.state.closed).toBe(false);
    await waitFor(
      () => admission.active === 0,
      "stalled payload-submit admission release",
    );
    expect(admission.active).toBe(0);

    const recoveryLimits = {
      ...DA_TRANSPORT_LIMITS_V1,
      requestTimeoutMs: 1_000,
    };
    const recoveryHandler = createDaLibp2pPayloadRequestHandlers({
      deploymentFingerprint,
      store,
      limits: recoveryLimits,
      payloadSubmitAdmission: admission,
    }).get(payloadSubmitProtocolId)!;
    const fixture = await makePayloadFixture();
    const healthyMock = makeMockStream(
      (async function* () {
        yield encodeDaStreamFrame(
          encodeSubmit(fixture.headerHash, fixture.payloadCbor),
          { maxFrameBytes: recoveryLimits.maxPayloadBytes },
        );
      })(),
    );
    await invokePayloadSubmitHandler(recoveryHandler, healthyMock.stream);
    expect(healthyMock.state.aborted).toBeUndefined();
    expect(healthyMock.state.closed).toBe(true);
    const response = decodeDaPayloadSubmitResponseV1Cbor(
      await readSingleDaStreamFrame(healthyMock.state.sent, {
        maxFrameBytes: recoveryLimits.maxPayloadBytes,
      }),
    );
    expect(response).toMatchObject({ status: "accepted", reasonCode: null });
    await expect(store.getDaPayload(fixture.headerHash)).resolves.toMatchObject(
      {
        payloadCborHex: fixture.payloadCbor.toString("hex"),
        validationStatus: "fetched",
      },
    );
    expect(admission.active).toBe(0);
  });

  it("rejects an incomplete frame without response or persistence and recovers", async () => {
    const fixture = await makePayloadFixture();
    const store = await JsonFileWatcherStore.open(await tempDir());
    const admission = new DaPayloadSubmitAdmission(1);
    const limits = {
      ...DA_TRANSPORT_LIMITS_V1,
      requestTimeoutMs: 1_000,
    };
    const handler = createDaLibp2pPayloadRequestHandlers({
      deploymentFingerprint,
      store,
      limits,
      payloadSubmitAdmission: admission,
    }).get(payloadSubmitProtocolId)!;
    const requestCbor = encodeSubmit(fixture.headerHash, fixture.payloadCbor);
    const incompleteFrame = Buffer.alloc(4 + requestCbor.length);
    incompleteFrame.writeUInt32BE(requestCbor.length + 1, 0);
    requestCbor.copy(incompleteFrame, 4);
    const malformedMock = makeMockStream(
      (async function* () {
        yield incompleteFrame;
      })(),
    );

    await expect(
      invokePayloadSubmitHandler(handler, malformedMock.stream),
    ).rejects.toThrow(/incomplete DA libp2p stream frame/);
    expect(malformedMock.state.aborted).toBeUndefined();
    expect(malformedMock.state.sent).toHaveLength(0);
    expect(malformedMock.state.closed).toBe(false);
    await expect(
      store.getDaPayload(fixture.headerHash),
    ).resolves.toBeUndefined();
    await waitFor(
      () => admission.active === 0,
      "malformed payload-submit admission release",
    );
    expect(admission.active).toBe(0);

    const healthyMock = makeMockStream(
      (async function* () {
        yield encodeDaStreamFrame(requestCbor, {
          maxFrameBytes: limits.maxPayloadBytes,
        });
      })(),
    );
    await invokePayloadSubmitHandler(handler, healthyMock.stream);
    expect(healthyMock.state.aborted).toBeUndefined();
    expect(healthyMock.state.closed).toBe(true);
    const response = decodeDaPayloadSubmitResponseV1Cbor(
      await readSingleDaStreamFrame(healthyMock.state.sent, {
        maxFrameBytes: limits.maxPayloadBytes,
      }),
    );
    expect(response).toMatchObject({ status: "accepted", reasonCode: null });
    await expect(store.getDaPayload(fixture.headerHash)).resolves.toMatchObject(
      {
        payloadCborHex: fixture.payloadCbor.toString("hex"),
        validationStatus: "fetched",
      },
    );
    expect(admission.active).toBe(0);
  });

  it("durably accepts an inline V1 envelope and serves bytes plus metadata", async () => {
    const { handlers, store } = await makeHandlers();
    const fixture = await makePayloadFixture();
    const payloadHash = computeDaSha256Hash(fixture.payloadCbor);

    const submit = decodeDaPayloadSubmitResponseV1Cbor(
      await handlers.handlePayloadSubmit(
        encodeSubmit(fixture.headerHash, fixture.payloadCbor),
      ),
    );
    expect(submit).toMatchObject({ status: "accepted", reasonCode: null });
    await expect(store.getDaPayload(fixture.headerHash)).resolves.toMatchObject(
      {
        payloadSchemaVersion: 1,
        payloadCborHex: fixture.payloadCbor.toString("hex"),
        payloadSha256: payloadHash.toString("hex"),
        validationStatus: "fetched",
      },
    );

    const byHeader = decodeDaPayloadByHeaderResponseV1Cbor(
      await handlers.handlePayloadByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(fixture.headerHash, "hex"),
          acceptedPayloadHashes: [payloadHash],
          maxInlineBytes: fixture.payloadCbor.length,
        }),
      ),
    );
    expect(byHeader.status).toBe("found_inline");
    expect(byHeader.payloadBytes?.equals(fixture.payloadCbor)).toBe(true);

    const metadata = decodeDaMetadataByHeaderResponseV1Cbor(
      await handlers.handleMetadataByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(fixture.headerHash, "hex"),
          acceptedPayloadHashes: [payloadHash],
          maxInlineBytes: 0,
        }),
      ),
    );
    expect(metadata).toMatchObject({
      status: "found",
      payloadSchemaVersion: 1,
      payloadBytes: fixture.payloadCbor.length,
      localStatus: "staged",
    });
  });

  it("serves bounded authenticated chunks when inline delivery is unavailable", async () => {
    const fixture = await makePayloadFixture();
    const { handlers } = await makeHandlers({
      maxInlineResponseBytes: 0,
      maxChunkBytes: 128,
    });
    await handlers.handlePayloadSubmit(
      encodeSubmit(fixture.headerHash, fixture.payloadCbor),
    );

    const byHeader = decodeDaPayloadByHeaderResponseV1Cbor(
      await handlers.handlePayloadByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(fixture.headerHash, "hex"),
          acceptedPayloadHashes: null,
          maxInlineBytes: 0,
        }),
      ),
    );
    expect(byHeader.status).toBe("found_chunked");
    expect(byHeader.chunkManifest?.chunkSize).toBe(128);

    const chunk = decodeDaPayloadChunkResponseV1Cbor(
      await handlers.handlePayloadChunk(
        encodeDaPayloadChunkRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(fixture.headerHash, "hex"),
          payloadHash: computeDaSha256Hash(fixture.payloadCbor),
          chunkIndex: 0,
        }),
      ),
    );
    expect(chunk.status).toBe("found");
    expect(chunk.chunkBytes).toHaveLength(128);
    expect(
      chunk.chunkHash?.equals(computeDaSha256Hash(chunk.chunkBytes!)),
    ).toBe(true);
  });

  it("treats identical submits as duplicates and valid divergent envelopes as conflicts", async () => {
    const fixture = await makePayloadFixture();
    const { handlers } = await makeHandlers();

    await handlers.handlePayloadSubmit(
      encodeSubmit(fixture.headerHash, fixture.payloadCbor),
    );
    const duplicate = decodeDaPayloadSubmitResponseV1Cbor(
      await handlers.handlePayloadSubmit(
        encodeSubmit(fixture.headerHash, fixture.payloadCbor),
      ),
    );
    expect(duplicate.status).toBe("duplicate");

    const zstdEnvelope = await wrapDaPayloadV1(fixture.innerPayloadCbor, {
      mode: "zstd",
    });
    const conflict = decodeDaPayloadSubmitResponseV1Cbor(
      await handlers.handlePayloadSubmit(
        encodeSubmit(fixture.headerHash, zstdEnvelope),
      ),
    );
    expect(conflict.status).toBe("conflict");
  });

  it("returns verified root metadata from a canonical stored record", async () => {
    const fixture = await makePayloadFixture();
    const { handlers, store } = await makeHandlers();
    const payloadHash = computeDaSha256Hash(fixture.payloadCbor);
    await store.saveDaPayload({
      deploymentFingerprint,
      headerHash: fixture.headerHash,
      payloadSchemaVersion: 1,
      payloadCborHex: fixture.payloadCbor.toString("hex"),
      payloadSha256: payloadHash.toString("hex"),
      sourcePeerId: "fixture",
      fetchedAt: "2026-07-24T00:00:00.000Z",
      verifiedAt: "2026-07-24T00:00:01.000Z",
      rootSummary: rootSummaryFromHeader(fixture.header),
      validationStatus: "verified",
    });

    const metadata = decodeDaMetadataByHeaderResponseV1Cbor(
      await handlers.handleMetadataByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(fixture.headerHash, "hex"),
          acceptedPayloadHashes: [payloadHash],
          maxInlineBytes: 0,
        }),
      ),
    );
    expect(metadata.localStatus).toBe("verified");
    expect(metadata.rootSummaryHash).toHaveLength(32);
    expect(metadata.transitionTraceRoot?.toString("hex")).toBe(
      fixture.header.transitionTraceRoot,
    );
  });

  it("rejects malformed, oversized, and hash-mismatched submissions", async () => {
    const fixture = await makePayloadFixture();
    const { handlers } = await makeHandlers({
      maxPayloadBytes: fixture.payloadCbor.length - 1,
      maxChunkBytes: 64,
    });

    await expect(
      handlers.handlePayloadSubmit(Buffer.from([0xff])),
    ).rejects.toBeInstanceOf(DaLibp2pPayloadProtocolError);
    const oversized = decodeDaPayloadSubmitResponseV1Cbor(
      await handlers.handlePayloadSubmit(
        encodeSubmit(fixture.headerHash, fixture.payloadCbor),
      ),
    );
    expect(oversized).toMatchObject({
      status: "rejected",
      reasonCode: "payload_too_large",
    });

    const permissive = (await makeHandlers()).handlers;
    const mismatched = decodeDaPayloadSubmitResponseV1Cbor(
      await permissive.handlePayloadSubmit(
        encodeDaPayloadSubmitRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(fixture.headerHash, "hex"),
          payloadHash: Buffer.alloc(32, 0xaa),
          payloadSchemaVersion: 1,
          mode: "inline",
          payloadBytes: fixture.payloadCbor,
          chunkManifest: null,
        }),
      ),
    );
    expect(mismatched).toMatchObject({
      status: "rejected",
      reasonCode: "payload_hash_mismatch",
    });
  });

  it("honors an exact zero attestation result limit", async () => {
    const protocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint,
      localPeerId: "committee-peer",
      committeeValidation: {
        committeeKeys: [],
        committeeSignersHash: "00".repeat(32),
        threshold: 0,
      },
      store: {
        getDaPayload: async () => undefined,
        getL1SourceState: async () => undefined,
        saveDaSignature: async () => undefined,
        listDaSignatures: async () => [{} as never],
      },
    });

    const response = decodeDaAttestationsByHeaderResponseV1Cbor(
      await protocol.handleAttestationsByHeaderRequest(
        encodeDaAttestationsByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.alloc(28, 0x02),
          acceptedSignerIndexes: null,
          maxAttestations: 0,
        }),
      ),
    );

    expect(response).toMatchObject({
      status: "not_found",
      attestations: [],
      reasonCode: null,
    });
  });

  it("rejects an attestation whose announced peer differs from the remote peer", async () => {
    let touchedPayloadContext = false;
    const daVkey = Buffer.alloc(32, 0x0c);
    const protocolStore = {
      getDaPayload: async () => undefined,
      getL1SourceState: async () => undefined,
      saveDaSignature: async () => undefined,
      listDaSignatures: async () => [],
    };
    const protocol = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint,
      localPeerId: "local-peer",
      committeeValidation: {
        committeeKeys: [daVkey.toString("hex")],
        committeeSignersHash: "00".repeat(32),
        threshold: 1,
      },
      store: protocolStore,
    });
    const exchange = new DaLibp2pAttestationExchange({
      deploymentFingerprint,
      localPeerId: "local-peer",
      node: {
        request: async () =>
          encodeDaAttestationsByHeaderResponseV1Cbor({
            status: "found",
            headerHash: Buffer.alloc(28, 0x02),
            attestations: [
              {
                deploymentFingerprint: deploymentFingerprintBytes,
                headerHash: Buffer.alloc(28, 0x02),
                payloadHash: Buffer.alloc(32, 0x03),
                signerIndex: 0,
                daVkey,
                onChainWitness: Buffer.alloc(65, 0x04),
                retentionUntilSlot: 42,
                announcedByPeerId: "forged-peer",
              },
            ],
            reasonCode: null,
          }),
        publishGossip: async () => undefined,
      },
      registry: new DaPeerRegistry([
        {
          peerId: "remote-peer",
          signerIndex: 0,
          daVkey: daVkey.toString("hex"),
          roles: ["committee"],
          multiaddrs: [],
          bootstrap: false,
        },
      ]),
      protocol,
      committeeValidation: {
        committeeKeys: [daVkey.toString("hex")],
        committeeSignersHash: "00".repeat(32),
        threshold: 1,
      },
      store: {
        getDaPayload: async () => {
          touchedPayloadContext = true;
          return undefined;
        },
        getStateQueueHeader: async () => {
          touchedPayloadContext = true;
          return undefined;
        },
      },
      requestTimeoutMs: 1000,
    });

    await expect(
      exchange.attestationsByHeader({
        peer: { peerId: "remote-peer", signerIndex: 0 },
        deploymentFingerprint,
        headerHash: "02".repeat(28),
      }),
    ).resolves.toEqual([]);
    expect(touchedPayloadContext).toBe(false);
  });
});

const encodeSubmit = (headerHash: string, payloadBytes: Buffer): Buffer =>
  encodeDaPayloadSubmitRequestV1Cbor({
    deploymentFingerprint: deploymentFingerprintBytes,
    headerHash: Buffer.from(headerHash, "hex"),
    payloadHash: computeDaSha256Hash(payloadBytes),
    payloadSchemaVersion: 1,
    mode: "inline",
    payloadBytes,
    chunkManifest: null,
  });

const makeHandlers = async (
  limits: Partial<{
    readonly maxPayloadBytes: number;
    readonly maxInlineResponseBytes: number;
    readonly maxChunkBytes: number;
    readonly maxStreamsPerPeer: number;
    readonly requestTimeoutMs: number;
  }> = {},
): Promise<{
  readonly handlers: DaLibp2pPayloadProtocolHandlers;
  readonly store: JsonFileWatcherStore;
}> => {
  const store = await JsonFileWatcherStore.open(await tempDir());
  return {
    store,
    handlers: new DaLibp2pPayloadProtocolHandlers({
      deploymentFingerprint,
      store,
      limits,
      now: () => new Date("2026-07-24T00:00:00.000Z"),
    }),
  };
};

type MockDaStreamState = {
  readonly sent: Buffer[];
  closed: boolean;
  aborted?: Error;
};

const makeMockStream = (
  chunks: AsyncIterable<Buffer>,
  onAbort?: (error: Error) => void,
): { readonly stream: DaLibp2pStream; readonly state: MockDaStreamState } => {
  const state: MockDaStreamState = { sent: [], closed: false };
  return {
    state,
    stream: {
      async *[Symbol.asyncIterator](): AsyncGenerator<Buffer> {
        yield* chunks;
      },
      send(data: Uint8Array): boolean {
        state.sent.push(Buffer.from(data));
        return true;
      },
      close(): void {
        state.closed = true;
      },
      abort(error: Error): void {
        state.aborted = error;
        onAbort?.(error);
      },
    },
  };
};

const invokePayloadSubmitHandler = (
  handler: DaLibp2pStreamHandler,
  stream: DaLibp2pStream,
): Promise<void> =>
  Promise.resolve(
    handler({
      protocolId: payloadSubmitProtocolId,
      protocolName: DaRequestResponseProtocol.payloadSubmit,
      stream,
      connection: {},
    }),
  );

const waitFor = async (
  predicate: () => boolean,
  label: string,
): Promise<void> => {
  const deadline = Date.now() + 1_000;
  while (!predicate()) {
    if (Date.now() >= deadline) {
      throw new Error(`timed out waiting for ${label}`);
    }
    await new Promise((resolve) => setTimeout(resolve, 1));
  }
};

const rootSummaryFromHeader = (header: HeaderV1): DaStoredPayloadRootSetV1 => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});
