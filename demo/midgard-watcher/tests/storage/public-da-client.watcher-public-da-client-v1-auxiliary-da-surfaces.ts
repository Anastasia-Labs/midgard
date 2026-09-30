import "./public-da-client.watcher-public-da-client-v1-deadline-and-permit-control.js";

import {
  computeDaSha256Hash,
  encodeDaEventToStepByEventResponseCbor,
  encodeDaProofBundleByHeaderResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  HEADER_HASH,
  PEERS,
  ScriptedTransport,
} from "./public-da-client.raw-config.js";
import {
  capabilitiesBytes,
  chunksOf,
  clientWith,
  expectClientError,
  statuses,
} from "./public-da-client.watcher-public-da-client-v1-construction.js";

// ---------------------------------------------------------------------------
// 9. Proof bundle, trace step, and event-to-step surfaces
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 auxiliary DA surfaces", () => {
  const proofBundle = Buffer.alloc(96, 0x7c);

  it("accepts a proof bundle whose SHA-256 matches", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "proof-bundle-by-header": () =>
          encodeDaProofBundleByHeaderResponseCbor({
            status: "found_inline",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            proofBundleHash: computeDaSha256Hash(proofBundle),
            proofBundleBytes: proofBundle,
            chunkManifest: null,
            reasonCode: null,
          }),
      },
    });
    const result = await clientWith(transport).fetchProofBundleByHeader({
      headerHash: HEADER_HASH,
    });
    expect(result.proofBundleHash).toBe(
      computeDaSha256Hash(proofBundle).toString("hex"),
    );
    expect(result.proofBundleBytes.equals(proofBundle)).toBe(true);
    expect(result.durableInput.kind).toBe("proof_input");
  });

  it("REJECTS a proof bundle whose SHA-256 does not match", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "proof-bundle-by-header": () =>
          encodeDaProofBundleByHeaderResponseCbor({
            status: "found_inline",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            proofBundleHash: computeDaSha256Hash(Buffer.from("elsewhere")),
            proofBundleBytes: proofBundle,
            chunkManifest: null,
            reasonCode: null,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchProofBundleByHeader({
        headerHash: HEADER_HASH,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a chunked proof bundle (only inline is supported)", async () => {
    const { manifest } = chunksOf(proofBundle, 32);
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "proof-bundle-by-header": () =>
          encodeDaProofBundleByHeaderResponseCbor({
            status: "found_chunked",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            proofBundleHash: computeDaSha256Hash(proofBundle),
            proofBundleBytes: null,
            chunkManifest: manifest,
            reasonCode: null,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchProofBundleByHeader({
        headerHash: HEADER_HASH,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("accepts an event-to-step membership answer and hashes both parts", async () => {
    const entry = Buffer.alloc(24, 0x21);
    const proof = Buffer.alloc(48, 0x22);
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: entry,
            membershipOrNonmembershipProofBytes: proof,
          }),
      },
    });
    const result = await clientWith(transport).fetchEventToStepByEvent({
      headerHash: HEADER_HASH,
      eventKey,
    });
    expect(result.eventToStepEntrySha256).toBe(
      computeDaSha256Hash(entry).toString("hex"),
    );
    expect(result.membershipOrNonmembershipProofSha256).toBe(
      computeDaSha256Hash(proof).toString("hex"),
    );
  });

  it("accepts a non-membership answer with a null entry", async () => {
    const proof = Buffer.alloc(48, 0x33);
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: null,
            membershipOrNonmembershipProofBytes: proof,
          }),
      },
    });
    const result = await clientWith(transport).fetchEventToStepByEvent({
      headerHash: HEADER_HASH,
      eventKey,
    });
    expect(result.eventToStepEntryBytes).toBeNull();
    expect(result.eventToStepEntrySha256).toBeNull();
  });

  it("REJECTS an event-to-step answer echoing a different event key", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from("ffffffff", "hex"),
            eventToStepEntryBytes: null,
            membershipOrNonmembershipProofBytes: Buffer.alloc(8, 1),
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchEventToStepByEvent({
        headerHash: HEADER_HASH,
        eventKey: "0a1b2c3d",
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an event-to-step answer with no proof bytes", async () => {
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: Buffer.alloc(8, 1),
            membershipOrNonmembershipProofBytes: null,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchEventToStepByEvent({
        headerHash: HEADER_HASH,
        eventKey,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });
});

export const collectBuffers = async (
  iterable: AsyncIterable<Buffer>,
): Promise<Buffer[]> => {
  const values: Buffer[] = [];
  for await (const value of iterable) values.push(value);
  return values;
};

export const asAsyncIterable = async function* (
  values: readonly Uint8Array[],
): AsyncGenerator<Uint8Array> {
  yield* values;
};
