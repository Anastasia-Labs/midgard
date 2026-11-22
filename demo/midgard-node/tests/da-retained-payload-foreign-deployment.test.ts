import "@al-ft/midgard-core/da-transport";
import "vitest";
import "../src/da/libp2p-producer.js";
import "./da-payload-libp2p-producer.manifest-fixture.js";

import {
  DA_TRANSPORT_LIMITS,
  DaRequestResponseProtocol,
  decodeDaPayloadByHeaderResponseCbor,
  encodeDaPayloadByHeaderRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import { createDaLibp2pRetainedPayloadRequestHandlers } from "../src/da/libp2p-producer.js";
import {
  callRetainedPayloadHandler,
  HEADER_HASH,
  parseManifestFixture,
  rowFixture,
} from "./da-payload-libp2p-producer.manifest-fixture.js";

describe("retained DA payload requests from another deployment", () => {
  it("rejects a payload-by-header request carrying another deployment's fingerprint", async () => {
    const manifest = parseManifestFixture();
    if (manifest === null) {
      throw new Error("expected libp2p publication manifest");
    }
    const handlers = createDaLibp2pRetainedPayloadRequestHandlers({
      manifest,
      retrieveByHeaderHash: async (headerHash) =>
        headerHash.equals(HEADER_HASH) ? rowFixture() : undefined,
    });

    const response = decodeDaPayloadByHeaderResponseCbor(
      await callRetainedPayloadHandler({
        handlers,
        protocol: DaRequestResponseProtocol.payloadByHeader,
        request: encodeDaPayloadByHeaderRequestCbor({
          deploymentFingerprint: Buffer.from("cd".repeat(28), "hex"),
          headerHash: HEADER_HASH,
          acceptedPayloadHashes: null,
          maxInlineBytes: DA_TRANSPORT_LIMITS.maxInlineResponseBytes,
        }),
      }),
    );

    expect(response).toMatchObject({
      status: "rejected",
      headerHash: HEADER_HASH,
      payloadBytes: null,
      reasonCode: "deployment_fingerprint_mismatch",
    });
  });
});
