import {
  encodeDaStreamFrame,
  readSingleDaStreamFrame,
} from "@al-ft/midgard-core/da-stream-codec";
import {
  computeDaSha256Hash,
  DA_TRANSPORT_LIMITS,
  DaRequestResponseProtocol,
  daRequestResponseProtocolId,
  decodeDaPayloadByHeaderResponseCbor,
  encodeDaPayloadByHeaderRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import { createDaLibp2pPublicRetainedDaPayloadRequestHandlers } from "../src/da/libp2p/payload-source.js";
import { fakePool, openWith } from "./helpers/public-retained-da-pool.js";
import { makeMockStream } from "./libp2p-payload-protocols.make-mock-stream.js";

const FINGERPRINT = "cd".repeat(32);
const HEADER_HASH = "ab".repeat(28);
const PAYLOAD = Buffer.from("80", "hex");

const stored = (validationStatus: "verified" | "conflicted") => ({
  deploymentFingerprint: FINGERPRINT,
  headerHash: HEADER_HASH,
  payloadSchemaVersion: 1,
  payloadCborHex: PAYLOAD.toString("hex"),
  payloadSha256: computeDaSha256Hash(PAYLOAD).toString("hex"),
  sourcePeerId: "public-peer",
  fetchedAt: "2026-08-03T00:00:00.000Z",
  validationStatus,
  ...(validationStatus === "conflicted"
    ? { validationError: "conflicting payload bytes" }
    : {}),
});

/** One payload-by-header read through the public listener's handlers. */
const payloadByHeader = async (record: Record<string, unknown>) => {
  const store = await openWith(fakePool({ payload: record }).pool);
  try {
    const handlers = createDaLibp2pPublicRetainedDaPayloadRequestHandlers({
      deploymentFingerprint: FINGERPRINT,
      store,
      limits: DA_TRANSPORT_LIMITS,
    });
    const protocolId = daRequestResponseProtocolId(
      FINGERPRINT,
      DaRequestResponseProtocol.payloadByHeader,
    );
    const request = encodeDaPayloadByHeaderRequestCbor({
      deploymentFingerprint: Buffer.from(FINGERPRINT, "hex"),
      headerHash: Buffer.from(HEADER_HASH, "hex"),
      acceptedPayloadHashes: null,
      maxInlineBytes: 1_000_000,
    });
    const { stream, state } = makeMockStream(
      (async function* () {
        yield encodeDaStreamFrame(request, {
          maxFrameBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        });
      })(),
    );
    await handlers.get(protocolId)!({
      protocolId,
      protocolName: DaRequestResponseProtocol.payloadByHeader,
      stream,
      connection: {},
    });
    return decodeDaPayloadByHeaderResponseCbor(
      await readSingleDaStreamFrame(state.sent, {
        maxFrameBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      }),
    );
  } finally {
    await store.close();
  }
};

/**
 * The public retained-DA listener reads its payloads through the read-only
 * Postgres store: a payload the committee recorded as conflicted is refused
 * as a stored conflict, never served.
 */
describe("the public retained-DA listener over the read-only Postgres store", () => {
  it("refuses a payload stored as conflicted with stored_conflict, serving no bytes", async () => {
    await expect(payloadByHeader(stored("conflicted"))).resolves.toMatchObject({
      status: "conflict",
      reasonCode: "stored_conflict",
      payloadBytes: null,
    });
  });

  it("serves the same bytes inline when they are stored verified", async () => {
    const response = await payloadByHeader(stored("verified"));
    expect(response).toMatchObject({
      status: "found_inline",
      reasonCode: null,
    });
    expect(Buffer.from(response.payloadBytes!)).toEqual(PAYLOAD);
  });
});
