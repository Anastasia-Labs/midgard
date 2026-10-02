import "@al-ft/midgard-core/da-payload-envelope";
import "@al-ft/midgard-core/da-transport";
import "vitest";
import "../src/da/libp2p/DaPeerRegistry.js";
import "../src/da/libp2p/DaProtocols.js";
import "../src/da/libp2p/payload-protocols.js";
import "../src/da/libp2p/payload-source.js";
import "../src/store.js";
import "./helpers.js";
import "./libp2p-payload-protocols.make-mock-stream.js";

import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  DaRequestResponseProtocol,
  decodeDaCapabilitiesResponseCbor,
  decodeDaMetadataByHeaderResponseCbor,
  decodeDaPayloadByHeaderResponseCbor,
  decodeDaPayloadSubmitResponseCbor,
  encodeDaCapabilitiesRequestCbor,
  encodeDaPayloadByHeaderRequestCbor,
  encodeDaPayloadSubmitRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import { DaPeerRegistry } from "../src/da/libp2p/DaPeerRegistry.js";
import { createDaProtocolAllowlist } from "../src/da/libp2p/DaProtocols.js";
import {
  DaLibp2pPayloadProtocolError,
  DaLibp2pPayloadProtocolHandlers,
} from "../src/da/libp2p/payload-protocols.js";
import { DaLibp2pPayloadSource } from "../src/da/libp2p/payload-source.js";
import type { DaPayloadRecord } from "../src/domain.js";
import { JsonFileCommitteeStore } from "../src/store.js";
import { makePayloadFixture, tempDir } from "./helpers.js";
import {
  deploymentFingerprint,
  deploymentFingerprintBytes,
  encodeSubmit,
  rootSummaryFromHeader,
} from "./libp2p-payload-protocols.make-mock-stream.js";

const makeLoggedHandlers = async (): Promise<{
  readonly handlers: DaLibp2pPayloadProtocolHandlers;
  readonly store: JsonFileCommitteeStore;
  readonly logged: string[];
}> => {
  const store = await JsonFileCommitteeStore.open(await tempDir());
  const logged: string[] = [];
  return {
    store,
    logged,
    handlers: new DaLibp2pPayloadProtocolHandlers({
      deploymentFingerprint,
      store,
      now: () => new Date("2026-07-24T00:00:00.000Z"),
      log: (message) => logged.push(message),
    }),
  };
};

const settledRecord = (
  fixture: Awaited<ReturnType<typeof makePayloadFixture>>,
  validationStatus: "verified" | "malformed_da" | "root_mismatch",
): DaPayloadRecord => ({
  deploymentFingerprint,
  headerHash: fixture.headerHash,
  payloadSchemaVersion: 1,
  payloadCborHex: fixture.payloadCbor.toString("hex"),
  payloadSha256: computeDaSha256Hash(fixture.payloadCbor).toString("hex"),
  sourcePeerId: "fixture",
  fetchedAt: "2026-07-24T00:00:00.000Z",
  payloadFetchStatus: "available",
  validationStatus,
  ...(validationStatus === "verified"
    ? {
        verifiedAt: "2026-07-24T00:00:01.000Z",
        rootSummary: rootSummaryFromHeader(fixture.header),
      }
    : { validationError: `fixture ${validationStatus}` }),
});

const byHeaderRequest = (headerHash: string): Buffer =>
  encodeDaPayloadByHeaderRequestCbor({
    deploymentFingerprint: deploymentFingerprintBytes,
    headerHash: Buffer.from(headerHash, "hex"),
    acceptedPayloadHashes: null,
    maxInlineBytes: 1_000_000,
  });

/** A read-only store that hands the handlers exactly one stored record. */
const singleRecordHandlers = (
  record: DaPayloadRecord,
): DaLibp2pPayloadProtocolHandlers =>
  new DaLibp2pPayloadProtocolHandlers({
    deploymentFingerprint,
    store: {
      getDaPayload: async (headerHash) =>
        headerHash === record.headerHash ? record : undefined,
      saveDaPayload: async () => {
        throw new Error("metadata reads must not write");
      },
    },
  });

describe("DA libp2p payload-submit never overwrites a settled record", () => {
  it.each(["verified", "malformed_da", "root_mismatch"] as const)(
    "refuses divergent bytes over a %s record without writing",
    async (validationStatus) => {
      const fixture = await makePayloadFixture();
      const { handlers, store, logged } = await makeLoggedHandlers();
      await store.saveDaPayload(settledRecord(fixture, validationStatus));
      const before = await store.getDaPayload(fixture.headerHash);
      const zstdEnvelope = await wrapDaPayload(fixture.innerPayloadCbor, {
        mode: "zstd",
      });

      const response = decodeDaPayloadSubmitResponseCbor(
        await handlers.handlePayloadSubmit(
          encodeSubmit(fixture.headerHash, zstdEnvelope),
        ),
      );

      expect(response).toMatchObject({
        status: "conflict",
        reasonCode: "conflicting_payload_bytes",
      });
      expect(
        response.payloadHash.equals(computeDaSha256Hash(zstdEnvelope)),
      ).toBe(true);
      await expect(store.getDaPayload(fixture.headerHash)).resolves.toEqual(
        before,
      );
      expect(logged).toHaveLength(1);
      expect(logged[0]).toContain(fixture.headerHash);
      expect(logged[0]).toContain(validationStatus);
    },
  );

  it("keeps serving and accepting the first verified bytes after a refused submit", async () => {
    const fixture = await makePayloadFixture();
    const { handlers, store } = await makeLoggedHandlers();
    await store.saveDaPayload(settledRecord(fixture, "verified"));
    const original = computeDaSha256Hash(fixture.payloadCbor);

    await handlers.handlePayloadSubmit(
      encodeSubmit(
        fixture.headerHash,
        await wrapDaPayload(fixture.innerPayloadCbor, { mode: "zstd" }),
      ),
    );

    const byHeader = decodeDaPayloadByHeaderResponseCbor(
      await handlers.handlePayloadByHeader(byHeaderRequest(fixture.headerHash)),
    );
    expect(byHeader.status).toBe("found_inline");
    expect(byHeader.payloadHash?.equals(original)).toBe(true);
    expect(byHeader.payloadBytes?.equals(fixture.payloadCbor)).toBe(true);
    const metadata = decodeDaMetadataByHeaderResponseCbor(
      await handlers.handleMetadataByHeader(
        byHeaderRequest(fixture.headerHash),
      ),
    );
    expect(metadata).toMatchObject({
      status: "found",
      localStatus: "verified",
    });
    const resubmit = decodeDaPayloadSubmitResponseCbor(
      await handlers.handlePayloadSubmit(
        encodeSubmit(fixture.headerHash, fixture.payloadCbor),
      ),
    );
    expect(resubmit.status).toBe("duplicate");
    await expect(store.getDaPayload(fixture.headerHash)).resolves.toMatchObject(
      {
        validationStatus: "verified",
        payloadSha256: original.toString("hex"),
      },
    );
  });

  it("still latches a divergent submit over an unverified record as a conflict", async () => {
    const fixture = await makePayloadFixture();
    const { handlers, store, logged } = await makeLoggedHandlers();
    await handlers.handlePayloadSubmit(
      encodeSubmit(fixture.headerHash, fixture.payloadCbor),
    );
    const zstdEnvelope = await wrapDaPayload(fixture.innerPayloadCbor, {
      mode: "zstd",
    });

    const response = decodeDaPayloadSubmitResponseCbor(
      await handlers.handlePayloadSubmit(
        encodeSubmit(fixture.headerHash, zstdEnvelope),
      ),
    );

    expect(response).toMatchObject({
      status: "conflict",
      reasonCode: "conflicting_payload_bytes",
    });
    await expect(store.getDaPayload(fixture.headerHash)).resolves.toMatchObject(
      {
        validationStatus: "conflicted",
        conflictStatus: "conflicting_bytes",
      },
    );
    expect(logged).toEqual([]);
    const later = decodeDaPayloadSubmitResponseCbor(
      await handlers.handlePayloadSubmit(
        encodeSubmit(fixture.headerHash, fixture.payloadCbor),
      ),
    );
    expect(later).toMatchObject({
      status: "conflict",
      reasonCode: "stored_conflict",
    });
  });

  it("still rejects a hash-mismatched submit over a verified record before any write", async () => {
    const fixture = await makePayloadFixture();
    const { handlers, store, logged } = await makeLoggedHandlers();
    await store.saveDaPayload(settledRecord(fixture, "verified"));
    const before = await store.getDaPayload(fixture.headerHash);

    const response = decodeDaPayloadSubmitResponseCbor(
      await handlers.handlePayloadSubmit(
        encodeDaPayloadSubmitRequestCbor({
          deploymentFingerprint: deploymentFingerprintBytes,
          headerHash: Buffer.from(fixture.headerHash, "hex"),
          payloadHash: Buffer.alloc(32, 0xaa),
          payloadSchemaVersion: 1,
          mode: "inline",
          payloadBytes: Buffer.from("deadbeef", "hex"),
          chunkManifest: null,
        }),
      ),
    );

    expect(response).toMatchObject({
      status: "rejected",
      reasonCode: "payload_hash_mismatch",
    });
    await expect(store.getDaPayload(fixture.headerHash)).resolves.toEqual(
      before,
    );
    expect(logged).toEqual([]);
  });
});

describe("DA libp2p metadata-by-header answers instead of aborting", () => {
  it("answers rejected for retained bytes that are not a canonical envelope", async () => {
    const fixture = await makePayloadFixture();
    const notAnEnvelope = Buffer.from("deadbeef", "hex");
    const handlers = singleRecordHandlers({
      ...settledRecord(fixture, "malformed_da"),
      payloadCborHex: notAnEnvelope.toString("hex"),
      payloadSha256: computeDaSha256Hash(notAnEnvelope).toString("hex"),
    });

    const metadata = decodeDaMetadataByHeaderResponseCbor(
      await handlers.handleMetadataByHeader(
        byHeaderRequest(fixture.headerHash),
      ),
    );

    expect(metadata).toMatchObject({
      status: "rejected",
      payloadHash: null,
      payloadSchemaVersion: null,
    });
  });

  it("answers rejected for a retained record outside schema V1", async () => {
    const fixture = await makePayloadFixture();
    const handlers = singleRecordHandlers({
      ...settledRecord(fixture, "verified"),
      payloadSchemaVersion: 2 as never,
    });

    const metadata = decodeDaMetadataByHeaderResponseCbor(
      await handlers.handleMetadataByHeader(
        byHeaderRequest(fixture.headerHash),
      ),
    );

    expect(metadata).toMatchObject({
      status: "rejected",
      payloadSchemaVersion: null,
    });
  });

  it("still answers found for a canonical V1 envelope", async () => {
    const fixture = await makePayloadFixture();
    const handlers = singleRecordHandlers(settledRecord(fixture, "verified"));

    const metadata = decodeDaMetadataByHeaderResponseCbor(
      await handlers.handleMetadataByHeader(
        byHeaderRequest(fixture.headerHash),
      ),
    );

    expect(metadata).toMatchObject({
      status: "found",
      payloadSchemaVersion: 1,
      payloadBytes: fixture.payloadCbor.length,
      localStatus: "verified",
    });
  });
});

/** A payload source whose only peer is served by `handlers` in process. */
const sourceServedBy = (
  handlers: DaLibp2pPayloadProtocolHandlers,
): DaLibp2pPayloadSource => {
  const { protocolNameById } = createDaProtocolAllowlist(deploymentFingerprint);
  const peer = {
    peerId: "retained-peer",
    signerIndex: 0,
    daVkey: "0c".repeat(32),
    roles: ["retrieval"],
    multiaddrs: [],
    bootstrap: false,
  } as const;
  return new DaLibp2pPayloadSource({
    deploymentFingerprint,
    node: {
      request: async ({
        protocolId,
        payload,
      }: {
        readonly protocolId: string;
        readonly payload: Uint8Array;
      }) => {
        const protocol = protocolNameById.get(protocolId);
        if (protocol === DaRequestResponseProtocol.payloadByHeader) {
          return handlers.handlePayloadByHeader(payload);
        }
        if (protocol === DaRequestResponseProtocol.metadataByHeader) {
          return handlers.handleMetadataByHeader(payload);
        }
        throw new Error(`unexpected protocol ${protocolId}`);
      },
    } as never,
    registry: new DaPeerRegistry([peer]),
    peers: [peer],
    limits: DA_TRANSPORT_LIMITS,
  });
};

describe("DA libp2p payload source reads a rejected metadata answer", () => {
  it("records invalid content, not a transport error, for malformed retained bytes", async () => {
    const fixture = await makePayloadFixture();
    const notAnEnvelope = Buffer.from("deadbeef", "hex");
    const source = sourceServedBy(
      singleRecordHandlers({
        ...settledRecord(fixture, "malformed_da"),
        payloadCborHex: notAnEnvelope.toString("hex"),
        payloadSha256: computeDaSha256Hash(notAnEnvelope).toString("hex"),
      }),
    );

    await expect(
      source.fetchPayloadCandidates(fixture.headerHash),
    ).resolves.toMatchObject({
      ok: false,
      attempts: [{ sourcePeerId: "retained-peer", status: "invalid_content" }],
    });
  });

  it("still yields the candidate for a canonical retained envelope", async () => {
    const fixture = await makePayloadFixture();
    const source = sourceServedBy(
      singleRecordHandlers(settledRecord(fixture, "verified")),
    );

    const fetched = await source.fetchPayloadCandidates(fixture.headerHash);

    expect(fetched.ok).toBe(true);
    expect(
      fetched.ok &&
        fetched.candidates[0]?.payloadCbor.equals(fixture.payloadCbor),
    ).toBe(true);
  });
});

describe("DA libp2p capabilities answers a foreign deployment fingerprint", () => {
  it("answers with the local fingerprint so the prober sees the mismatch", async () => {
    const fixture = await makePayloadFixture();
    const handlers = singleRecordHandlers(settledRecord(fixture, "verified"));
    const foreignFingerprint = Buffer.alloc(32, 0x02);

    const capabilities = decodeDaCapabilitiesResponseCbor(
      await handlers.handleCapabilities(
        encodeDaCapabilitiesRequestCbor({
          deploymentFingerprint: foreignFingerprint,
        }),
      ),
    );

    expect(
      capabilities.deploymentFingerprint.equals(deploymentFingerprintBytes),
    ).toBe(true);
    expect(capabilities.deploymentFingerprint.equals(foreignFingerprint)).toBe(
      false,
    );
  });

  it("still refuses an undecodable capabilities request", async () => {
    const fixture = await makePayloadFixture();
    const handlers = singleRecordHandlers(settledRecord(fixture, "verified"));

    await expect(
      handlers.handleCapabilities(Buffer.from([0xff])),
    ).rejects.toBeInstanceOf(DaLibp2pPayloadProtocolError);
  });
});
