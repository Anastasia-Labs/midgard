import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  decodeDaCapabilitiesResponseV1Cbor,
  decodeDaMetadataByHeaderResponseV1Cbor,
  decodeDaPayloadByHeaderResponseV1Cbor,
  decodeDaPayloadChunkResponseV1Cbor,
  decodeDaPayloadSubmitResponseV1Cbor,
  encodeDaCapabilitiesRequestV1Cbor,
  encodeDaPayloadByHeaderRequestV1Cbor,
  encodeDaPayloadChunkRequestV1Cbor,
  encodeDaPayloadSubmitRequestV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  DaLibp2pPayloadProtocolError,
  DaLibp2pPayloadProtocolHandlers,
} from "../src/da/libp2p/payload-protocols.js";
import { DaPayloadSubmitAdmission } from "../src/da/libp2p/payload-source.js";
import type {
  DaStoredPayloadRootSetV1,
  HeaderV1,
} from "../src/domain.js";
import { JsonFileWatcherStore } from "../src/store.js";
import { makePayloadFixture, tempDir } from "./helpers.js";

const deploymentFingerprint = "01".repeat(32);
const deploymentFingerprintBytes = Buffer.from(deploymentFingerprint, "hex");

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
    await expect(store.getDaPayload(fixture.headerHash)).resolves.toMatchObject({
      payloadSchemaVersion: 1,
      payloadCborHex: fixture.payloadCbor.toString("hex"),
      payloadSha256: payloadHash.toString("hex"),
      validationStatus: "fetched",
    });

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
    expect(chunk.chunkHash?.equals(computeDaSha256Hash(chunk.chunkBytes!))).toBe(
      true,
    );
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

const rootSummaryFromHeader = (
  header: HeaderV1,
): DaStoredPayloadRootSetV1 => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});
