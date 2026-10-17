import "./phase-a-verifier.block-verification.js";

import * as SDK from "@al-ft/midgard-sdk";
import { makeNativeTx } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { describe, expect, it } from "vitest";

import {
  evaluateWatcherPhaseAQueuedTxs,
  watcherPhaseAQueuedTxs,
  WatcherPhaseAVerifierError,
} from "../../src/verification/phase-a-verifier.js";
import { CONFIG, KEY, queuedTx } from "./phase-a-verifier.base-header.js";
import { buildBlock, evaluateBlock } from "./phase-a-verifier.build-block.js";

// ---------------------------------------------------------------------------
// Malformed inputs
// ---------------------------------------------------------------------------

describe("malformed inputs", () => {
  it("reports a canonical reconstruction failure for truncated bytes", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    const result = await evaluateBlock(fixture, {
      payloadEnvelopeCbor: fixture.envelope.subarray(
        0,
        fixture.envelope.length - 8,
      ),
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "canonical_reconstruction_failed",
    ]);
    expect(result.acceptedTxIds).toStrictEqual([]);
  });

  it("cannot even encode a non-V1 payload version, and fails closed if fed one", async () => {
    const fixture = await buildBlock({
      txCbors: [makeNativeTx({ privateKey: KEY }).txCbor],
    });
    expect(() =>
      SDK.encodeDaPayload({
        ...fixture.payload,
        version: (SDK.DA_PAYLOAD_VERSION + 1n) as never,
      }),
    ).toThrow(/version must equal/u);
    // A version byte flipped after encoding is not decodable either.
    const corrupted = Buffer.from(fixture.envelope);
    corrupted[corrupted.length - 1] ^= 0xff;
    const result = await evaluateBlock(fixture, {
      payloadEnvelopeCbor: corrupted,
    });
    expect(result.action).toBe("error");
    expect(result.reasonCodes).toStrictEqual([
      "canonical_reconstruction_failed",
    ]);
  });

  it("rejects undecodable transaction bytes through the canonical decoder", () => {
    const result = evaluateWatcherPhaseAQueuedTxs({
      queuedTxs: [queuedTx(Buffer.alloc(32), Buffer.alloc(0))],
      config: CONFIG,
    });
    expect(result.action).toBe("reject");
    expect(result.rejections[0]!.code).toBe(RejectCodes.CborDeserialization);
  });

  it("rejects a malformed transaction id in the derived queue", () => {
    expect(() =>
      watcherPhaseAQueuedTxs({
        transactions: [{ txId: "zz", txCbor: Buffer.alloc(1) }],
        programMaterial: [],
      }),
    ).toThrow(WatcherPhaseAVerifierError);
  });
});
