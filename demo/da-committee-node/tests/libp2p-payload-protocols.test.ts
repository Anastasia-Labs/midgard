import {
  computeDaSha256Hash,
  type DaPayloadChunkManifestV1,
  decodeDaMetadataByHeaderResponseV1Cbor,
  decodeDaPayloadByHeaderResponseV1Cbor,
  decodeDaPayloadChunkResponseV1Cbor,
  decodeDaPayloadSubmitResponseV1Cbor,
  encodeDaPayloadByHeaderRequestV1Cbor,
  encodeDaPayloadChunkRequestV1Cbor,
  encodeDaPayloadSubmitRequestV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  DaLibp2pPayloadProtocolError,
  DaLibp2pPayloadProtocolHandlers,
} from "../src/da/libp2p/payload-protocols.js";
import type { Header, PayloadRootSet } from "../src/domain.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { makePayloadFixture, tempDir } from "./helpers.js";

const deploymentFingerprint = "01".repeat(32);
const deploymentFingerprintBytes = Buffer.from(deploymentFingerprint, "hex");
const payloadSchemaVersion = Number(SDK.DA_PAYLOAD_V2_VERSION);

describe("DA libp2p payload protocol handlers", () => {
  it("accepts inline payload submits and serves inline payload plus metadata", async () => {
    const { handlers, store } = await makeHandlers();
    const { payloadCbor, headerHash } = await makePayloadFixture();
    const payloadHash = computeDaSha256Hash(payloadCbor);

    const submitResponse = decodeDaPayloadSubmitResponseV1Cbor(
      await handlers.handlePayloadSubmit(
        encodeDaPayloadSubmitRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          payloadHash,
          payloadSchemaVersion,
          mode: "inline",
          payloadBytes: payloadCbor,
          chunkManifest: null,
        }),
      ),
    );
    expect(submitResponse).toMatchObject({
      status: "accepted",
      reasonCode: null,
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      deploymentFingerprint,
      headerHash,
      payloadCborHex: payloadCbor.toString("hex"),
      payloadSha256: payloadHash.toString("hex"),
      validationStatus: "fetched",
    });

    const byHeaderResponse = decodeDaPayloadByHeaderResponseV1Cbor(
      await handlers.handlePayloadByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          acceptedPayloadHashes: [payloadHash],
          maxInlineBytes: payloadCbor.length,
        }),
      ),
    );
    expect(byHeaderResponse.status).toBe("found_inline");
    expect(byHeaderResponse.payloadHash?.equals(payloadHash)).toBe(true);
    expect(byHeaderResponse.payloadBytes?.equals(payloadCbor)).toBe(true);
    expect(byHeaderResponse.chunkManifest).toBeNull();

    const metadataResponse = decodeDaMetadataByHeaderResponseV1Cbor(
      await handlers.handleMetadataByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          acceptedPayloadHashes: null,
          maxInlineBytes: 0,
        }),
      ),
    );
    expect(metadataResponse).toMatchObject({
      status: "found",
      payloadSchemaVersion,
      payloadBytes: payloadCbor.length,
      rootSummaryHash: null,
      localStatus: "staged",
    });
    expect(metadataResponse.payloadHash?.equals(payloadHash)).toBe(true);
  });

  it("treats duplicate submits as idempotent and detects conflicting bytes", async () => {
    const { handlers, store } = await makeHandlers();
    const { payload, payloadCbor, headerHash } = await makePayloadFixture();
    const payloadHash = computeDaSha256Hash(payloadCbor);
    const request = encodeDaPayloadSubmitRequestV1Cbor({
      deploymentFingerprint: deploymentFingerprintBytes,
      headerHash: Buffer.from(headerHash, "hex"),
      payloadHash,
      payloadSchemaVersion,
      mode: "inline",
      payloadBytes: payloadCbor,
      chunkManifest: null,
    });

    expect(
      decodeDaPayloadSubmitResponseV1Cbor(
        await handlers.handlePayloadSubmit(request),
      ).status,
    ).toBe("accepted");
    expect(
      decodeDaPayloadSubmitResponseV1Cbor(
        await handlers.handlePayloadSubmit(request),
      ).status,
    ).toBe("duplicate");

    const [depositKey] = payload.block_body.deposits[0]!;
    const conflictingPayload = SDK.encodeDaPayloadV2({
      ...payload,
      block_body: {
        ...payload.block_body,
        deposits: [[depositKey, "de"]],
      },
    });
    const conflictingHash = computeDaSha256Hash(conflictingPayload);
    const conflictResponse = decodeDaPayloadSubmitResponseV1Cbor(
      await handlers.handlePayloadSubmit(
        encodeDaPayloadSubmitRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          payloadHash: conflictingHash,
          payloadSchemaVersion,
          mode: "inline",
          payloadBytes: conflictingPayload,
          chunkManifest: null,
        }),
      ),
    );
    expect(conflictResponse).toMatchObject({
      status: "conflict",
      reasonCode: "conflicting_payload_bytes",
    });
    await expect(store.getDaPayload(headerHash)).resolves.toMatchObject({
      payloadSha256: conflictingHash.toString("hex"),
      validationStatus: "conflicted",
      conflictStatus: "conflicting_bytes",
    });

    const byHeaderResponse = decodeDaPayloadByHeaderResponseV1Cbor(
      await handlers.handlePayloadByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          acceptedPayloadHashes: null,
          maxInlineBytes: payloadCbor.length,
        }),
      ),
    );
    expect(byHeaderResponse).toMatchObject({
      status: "conflict",
      reasonCode: "stored_conflict",
    });
  });

  it("serves chunked retrieval with bounded chunks and checked hashes", async () => {
    const { handlers } = await makeHandlers({
      maxChunkBytes: 64,
      maxInlineResponseBytes: 32,
    });
    const { payloadCbor, headerHash } = await makePayloadFixture();
    const payloadHash = computeDaSha256Hash(payloadCbor);
    await handlers.handlePayloadSubmit(
      encodeDaPayloadSubmitRequestV1Cbor({
        deploymentFingerprint: deploymentFingerprintBytes,
        headerHash: Buffer.from(headerHash, "hex"),
        payloadHash,
        payloadSchemaVersion,
        mode: "inline",
        payloadBytes: payloadCbor,
        chunkManifest: null,
      }),
    );

    const byHeaderResponse = decodeDaPayloadByHeaderResponseV1Cbor(
      await handlers.handlePayloadByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          acceptedPayloadHashes: null,
          maxInlineBytes: 0,
        }),
      ),
    );
    expect(byHeaderResponse.status).toBe("found_chunked");
    expect(byHeaderResponse.payloadBytes).toBeNull();
    expect(byHeaderResponse.chunkManifest).toMatchObject({
      totalBytes: payloadCbor.length,
      chunkSize: 64,
    });
    expect(
      byHeaderResponse.chunkManifest?.payloadHash.equals(payloadHash),
    ).toBe(true);

    const chunkResponse = decodeDaPayloadChunkResponseV1Cbor(
      await handlers.handlePayloadChunk(
        encodeDaPayloadChunkRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          payloadHash,
          chunkIndex: 1,
        }),
      ),
    );
    const expectedChunk = payloadCbor.subarray(64, 128);
    expect(chunkResponse.status).toBe("found");
    expect(chunkResponse.chunkBytes?.equals(expectedChunk)).toBe(true);
    expect(
      chunkResponse.chunkHash?.equals(computeDaSha256Hash(expectedChunk)),
    ).toBe(true);
    expect(chunkResponse.chunkBytes?.length).toBeLessThanOrEqual(64);

    const outOfRange = decodeDaPayloadChunkResponseV1Cbor(
      await handlers.handlePayloadChunk(
        encodeDaPayloadChunkRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          payloadHash,
          chunkIndex: 999,
        }),
      ),
    );
    expect(outOfRange.status).toBe("not_found");

    const wrongHash = decodeDaPayloadChunkResponseV1Cbor(
      await handlers.handlePayloadChunk(
        encodeDaPayloadChunkRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          payloadHash: Buffer.alloc(32, 0xff),
          chunkIndex: 0,
        }),
      ),
    );
    expect(wrongHash.status).toBe("not_found");
  });

  it("validates chunked submit manifests and defers chunk acquisition", async () => {
    const { handlers } = await makeHandlers({ maxChunkBytes: 64 });
    const { payloadCbor, headerHash } = await makePayloadFixture();
    const manifest = chunkManifestFor(payloadCbor, 64);
    const deferred = decodeDaPayloadSubmitResponseV1Cbor(
      await handlers.handlePayloadSubmit(
        encodeDaPayloadSubmitRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          payloadHash: manifest.payloadHash,
          payloadSchemaVersion,
          mode: "chunked",
          payloadBytes: null,
          chunkManifest: manifest,
        }),
      ),
    );
    expect(deferred).toMatchObject({
      status: "deferred",
      reasonCode: "chunked_submit_deferred",
    });

    const rejected = decodeDaPayloadSubmitResponseV1Cbor(
      await handlers.handlePayloadSubmit(
        encodeDaPayloadSubmitRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          payloadHash: manifest.payloadHash,
          payloadSchemaVersion,
          mode: "chunked",
          payloadBytes: null,
          chunkManifest: {
            ...manifest,
            chunkSize: 65,
          },
        }),
      ),
    );
    expect(rejected).toMatchObject({
      status: "rejected",
      reasonCode: "chunk_too_large",
    });
  });

  it("returns verified metadata from stored root summaries", async () => {
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
      verifiedAt: "2026-06-21T00:00:01.000Z",
      rootSummary: rootSummaryFromHeader(header),
      validationStatus: "verified",
    });

    const metadataResponse = decodeDaMetadataByHeaderResponseV1Cbor(
      await handlers.handleMetadataByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          acceptedPayloadHashes: [payloadHash],
          maxInlineBytes: 0,
        }),
      ),
    );
    expect(metadataResponse.status).toBe("found");
    expect(metadataResponse.rootSummaryHash).toHaveLength(32);
    expect(metadataResponse.transitionTraceRoot?.toString("hex")).toBe(
      header.transitionTraceRoot,
    );
    expect(metadataResponse.eventToStepRoot?.toString("hex")).toBe(
      header.eventToStepRoot,
    );
    expect(metadataResponse.localStatus).toBe("verified");
  });

  it("does not report stale missing metadata after verified bytes are stored", async () => {
    const { handlers, store } = await makeHandlers();
    const { payloadCbor, header, headerHash } = await makePayloadFixture();
    const payloadHash = computeDaSha256Hash(payloadCbor);
    await store.saveDaPayload({
      deploymentFingerprint,
      headerHash,
      payloadCborHex: "",
      payloadSha256: "",
      sourcePeerId: "",
      fetchedAt: "2026-06-21T00:00:00.000Z",
      payloadFetchStatus: "missing_da",
      validationStatus: "missing_da",
    });
    await store.saveDaPayload({
      deploymentFingerprint,
      headerHash,
      payloadCborHex: payloadCbor.toString("hex"),
      payloadSha256: payloadHash.toString("hex"),
      sourcePeerId: "libp2p-fixture",
      fetchedAt: "2026-06-21T00:00:01.000Z",
      verifiedAt: "2026-06-21T00:00:02.000Z",
      rootSummary: rootSummaryFromHeader(header),
      payloadFetchStatus: "available",
      validationStatus: "verified",
    });

    const metadataResponse = decodeDaMetadataByHeaderResponseV1Cbor(
      await handlers.handleMetadataByHeader(
        encodeDaPayloadByHeaderRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          acceptedPayloadHashes: null,
          maxInlineBytes: 0,
        }),
      ),
    );

    expect(metadataResponse.status).toBe("found");
    expect(metadataResponse.localStatus).toBe("verified");
    expect(metadataResponse.payloadHash?.equals(payloadHash)).toBe(true);
  });

  it("rejects malformed, oversized, and hash-mismatched submit requests", async () => {
    const { payloadCbor, headerHash } = await makePayloadFixture();
    const { handlers } = await makeHandlers({
      maxPayloadBytes: payloadCbor.length - 1,
      maxChunkBytes: 64,
    });
    await expect(
      handlers.handlePayloadSubmit(Buffer.from([0xff])),
    ).rejects.toBeInstanceOf(DaLibp2pPayloadProtocolError);

    const oversized = decodeDaPayloadSubmitResponseV1Cbor(
      await handlers.handlePayloadSubmit(
        encodeDaPayloadSubmitRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          payloadHash: computeDaSha256Hash(payloadCbor),
          payloadSchemaVersion,
          mode: "inline",
          payloadBytes: payloadCbor,
          chunkManifest: null,
        }),
      ),
    );
    expect(oversized).toMatchObject({
      status: "rejected",
      reasonCode: "payload_too_large",
    });

    const permissive = (
      await makeHandlers({
        maxPayloadBytes: payloadCbor.length,
        maxChunkBytes: 64,
      })
    ).handlers;
    const mismatched = decodeDaPayloadSubmitResponseV1Cbor(
      await permissive.handlePayloadSubmit(
        encodeDaPayloadSubmitRequestV1Cbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(headerHash, "hex"),
          payloadHash: Buffer.alloc(32, 0xaa),
          payloadSchemaVersion,
          mode: "inline",
          payloadBytes: payloadCbor,
          chunkManifest: null,
        }),
      ),
    );
    expect(mismatched).toMatchObject({
      status: "rejected",
      reasonCode: "payload_hash_mismatch",
    });
  });
});

const makeHandlers = async (
  limits: Partial<{
    readonly maxPayloadBytes: number;
    readonly maxInlineResponseBytes: number;
    readonly maxChunkBytes: number;
  }> = {},
): Promise<{
  readonly handlers: DaLibp2pPayloadProtocolHandlers;
  readonly store: JsonFileWatcherStore;
}> => {
  const store = await JsonFileWatcherStore.open(await tempDir());
  const handlers = new DaLibp2pPayloadProtocolHandlers({
    deploymentFingerprint,
    store,
    limits,
    now: () => new Date("2026-06-21T00:00:00.000Z"),
  });
  return { handlers, store };
};

const chunkManifestFor = (
  payloadBytes: Buffer,
  chunkSize: number,
): DaPayloadChunkManifestV1 => {
  const chunkHashes: Buffer[] = [];
  for (let offset = 0; offset < payloadBytes.length; offset += chunkSize) {
    chunkHashes.push(
      computeDaSha256Hash(payloadBytes.subarray(offset, offset + chunkSize)),
    );
  }
  return {
    payloadHash: computeDaSha256Hash(payloadBytes),
    totalBytes: payloadBytes.length,
    chunkSize,
    chunkHashes,
  };
};

const rootSummaryFromHeader = (header: Header): PayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
});
