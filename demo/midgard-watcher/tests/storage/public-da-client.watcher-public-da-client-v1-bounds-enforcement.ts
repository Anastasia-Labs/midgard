import "./public-da-client.watcher-public-da-client-v1-inline-payload-verification.js";

import {
  computeDaSha256Hash,
  encodeDaEventToStepByEventResponseCbor,
  encodeDaProofBundleByHeaderResponseCbor,
  encodeDaTraceStepByIndexResponseCbor,
} from "@al-ft/midgard-core/da-transport";
import { describe, expect, it } from "vitest";

import {
  HEADER_HASH,
  PEERS,
  ScriptedTransport,
} from "./public-da-client.raw-config.js";
import {
  capabilitiesBytes,
  clientWith,
  expectClientError,
  fixture,
  honestInlineScript,
  statuses,
} from "./public-da-client.watcher-public-da-client-v1-construction.js";

// ---------------------------------------------------------------------------
// 5. Bounds checks
// ---------------------------------------------------------------------------

describe("WatcherPublicDaClientV1 bounds enforcement", () => {
  it("REJECTS an inline payload above the negotiated inline ceiling", async () => {
    const inlineCeiling = fixture.envelope.length - 1;
    const transport = new ScriptedTransport(
      honestInlineScript(
        {},
        { maxPayloadBytes: 1_000_000, maxInlineResponseBytes: inlineCeiling },
      ),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("accepts an inline payload exactly at the negotiated inline ceiling", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript(
        {},
        {
          maxPayloadBytes: 1_000_000,
          maxInlineResponseBytes: fixture.envelope.length,
        },
      ),
    );
    const result = await clientWith(transport).fetchPayloadByHeader({
      headerHash: HEADER_HASH,
    });
    expect(result.payloadEnvelopeCbor).toHaveLength(fixture.envelope.length);
  });

  it("REJECTS an empty inline payload body", async () => {
    const transport = new ScriptedTransport(
      honestInlineScript({
        payloadHash: computeDaSha256Hash(Buffer.alloc(0)),
        payloadBytes: Buffer.alloc(0),
      }),
    );
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an empty transport response frame", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: { capabilities: () => new Uint8Array(0) },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a transport response that is not a byte array", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => "definitely-not-bytes" as unknown as Uint8Array,
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchPayloadByHeader({ headerHash: HEADER_HASH }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS a trace step whose parts jointly exceed the payload ceiling", async () => {
    const half = Buffer.alloc(600, 0x5a);
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes({
            maxPayloadBytes: 1_000,
            maxInlineResponseBytes: 1_000,
            maxChunkBytes: 1_000,
          }),
        "trace-step-by-index": () =>
          encodeDaTraceStepByIndexResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            stepIndex: 0,
            transitionStepBytes: half,
            membershipProofBytes: half,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchTraceStepByIndex({
        headerHash: HEADER_HASH,
        stepIndex: 0,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("accepts a trace step that fits inside the payload ceiling", async () => {
    const step = Buffer.alloc(300, 0x5a);
    const proof = Buffer.alloc(400, 0x6b);
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () =>
          capabilitiesBytes({
            maxPayloadBytes: 1_000,
            maxInlineResponseBytes: 1_000,
            maxChunkBytes: 1_000,
          }),
        "trace-step-by-index": () =>
          encodeDaTraceStepByIndexResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            stepIndex: 3,
            transitionStepBytes: step,
            membershipProofBytes: proof,
          }),
      },
    });
    const result = await clientWith(transport).fetchTraceStepByIndex({
      headerHash: HEADER_HASH,
      stepIndex: 3,
    });
    expect(result.stepIndex).toBe(3);
    expect(result.transitionStepSha256).toBe(
      computeDaSha256Hash(step).toString("hex"),
    );
    expect(result.membershipProofSha256).toBe(
      computeDaSha256Hash(proof).toString("hex"),
    );
  });

  /*
   * Zero-length (but non-null) parts are the one shape the joint size ceiling
   * cannot catch: an empty part only shrinks the sum. These pin the per-field
   * emptiness bound itself rather than a downstream duplicate of it.
   */
  it.each([
    ["transition step", Buffer.alloc(0), Buffer.alloc(32, 0x11)],
    ["membership proof", Buffer.alloc(32, 0x11), Buffer.alloc(0)],
  ])("REJECTS a trace step with an empty %s", async (_label, step, proof) => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "trace-step-by-index": () =>
          encodeDaTraceStepByIndexResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            stepIndex: 0,
            transitionStepBytes: step,
            membershipProofBytes: proof,
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchTraceStepByIndex({
        headerHash: HEADER_HASH,
        stepIndex: 0,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });

  it("REJECTS an event-to-step answer with empty proof bytes", async () => {
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: Buffer.alloc(16, 0x44),
            membershipOrNonmembershipProofBytes: Buffer.alloc(0),
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

  it("REJECTS an event-to-step answer with a present-but-empty entry", async () => {
    const eventKey = "0a1b2c3d";
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "event-to-step-by-event": () =>
          encodeDaEventToStepByEventResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            eventKey: Buffer.from(eventKey, "hex"),
            eventToStepEntryBytes: Buffer.alloc(0),
            membershipOrNonmembershipProofBytes: Buffer.alloc(16, 0x55),
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

  it("REJECTS a proof bundle with empty bytes", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "proof-bundle-by-header": () =>
          encodeDaProofBundleByHeaderResponseCbor({
            status: "found_inline",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            proofBundleHash: computeDaSha256Hash(Buffer.alloc(0)),
            proofBundleBytes: Buffer.alloc(0),
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

  it("REJECTS a trace step answering a different index", async () => {
    const transport = new ScriptedTransport({
      [PEERS[0]!]: {
        capabilities: () => capabilitiesBytes(),
        "trace-step-by-index": () =>
          encodeDaTraceStepByIndexResponseCbor({
            status: "found",
            headerHash: Buffer.from(HEADER_HASH, "hex"),
            stepIndex: 9,
            transitionStepBytes: Buffer.alloc(8, 1),
            membershipProofBytes: Buffer.alloc(8, 2),
          }),
      },
    });
    const error = await expectClientError(
      clientWith(transport).fetchTraceStepByIndex({
        headerHash: HEADER_HASH,
        stepIndex: 3,
      }),
    );
    expect(statuses(error.attempts)).toEqual(["invalid_content"]);
  });
});
