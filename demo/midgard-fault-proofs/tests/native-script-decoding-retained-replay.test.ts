import { describe, expect, it } from "vitest";

import {
  admitNativeScriptDecodingWorkflowArtifact,
  prepareNativeScriptDecodingWorkflowArtifact,
} from "../src/native-script-decoding/artifact.js";
import { reconstructDaPayload } from "../src/transition-trace/reconstruct.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.js";
import { NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../src/workflow/complete-replay.js";
import { reencodeFixturePayload } from "./helpers/canonical-block-evidence-fixture.js";
import { nativeDecodingFixture } from "./support/native-script-decoding-retained.js";

describe("nativeScriptDecoding authenticated retained replay", () => {
  it.each([0n, -1n, 1n])(
    "authenticates forced coordinate %s and JSON recovery",
    async (index) => {
      const { evidence, context } = await nativeDecodingFixture({ index });
      const decision =
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        );
      expect(decision.detections).toHaveLength(1);
      const classification = await classifyCanonicalBlockViolations({
        evidence,
        detections: decision.detections,
        minimumConfirmationDepth: 1,
      });
      if (classification.decision !== "fault_detected")
        throw new Error("missing detection");
      const artifact = await prepareNativeScriptDecodingWorkflowArtifact({
        evidence,
        classification,
        replayContext: context,
      });
      const restored = await admitNativeScriptDecodingWorkflowArtifact(
        JSON.parse(JSON.stringify(artifact)),
      );
      expect(restored.coordinate.outpointCursor).toBe(index.toString());
      await expect(
        admitNativeScriptDecodingWorkflowArtifact({
          ...artifact,
          headerHash: "00".repeat(28),
        }),
      ).rejects.toThrow();
    },
  );
  it("detects malformed accepted source without treating its descriptor as canonical", async () => {
    const { evidence, context } = await nativeDecodingFixture({
      direction: 0,
      item: Buffer.from("8200428109", "hex"),
    });
    expect(
      (
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        )
      ).detections,
    ).toHaveLength(1);
  });
  it("refuses honest acceptance and substantive rejection", async () => {
    const accepted = await nativeDecodingFixture({ direction: 0 });
    expect(
      (
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          accepted.evidence,
          accepted.context,
        )
      ).detections,
    ).toHaveLength(0);
    const rejected = await nativeDecodingFixture({
      item: Buffer.from("8200428109", "hex"),
    });
    expect(
      (
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          rejected.evidence,
          rejected.context,
        )
      ).detections,
    ).toHaveLength(0);
  });
  it("honors prior committed spending of the selected reference", async () => {
    const { evidence, context } = await nativeDecodingFixture({
      consumed: true,
    });
    expect(
      (
        await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
          evidence,
          context,
        )
      ).detections,
    ).toHaveLength(0);
  });
});

it.each([
  "ResolvedReferenceScriptMalformed",
  "ResolvedReferenceScriptNodeLimit",
  "ResolvedReferenceScriptDepthLimit",
] as const)("installs exact wrongful reason %s", async (reasonName) => {
  const { evidence, context } = await nativeDecodingFixture({ reasonName });
  expect(
    (
      await NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY.replay(
        evidence,
        context,
      )
    ).detections,
  ).toHaveLength(1);
});

it("reconstructs a block whose native script_ref does not decode, from the output bytes alone", async () => {
  const fixture = await nativeDecodingFixture({
    direction: 0,
    item: Buffer.from("8200428109", "hex"),
  });
  const body = fixture.predecessor.reconstruction.payload.block_body;
  expect(body.validation_trace_witnesses).toEqual([]);
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor: await reencodeFixturePayload(
      fixture.predecessor.reconstruction.payload,
    ),
    expectedHeaderHash: fixture.predecessor.headerHash,
  });
  expect(reconstruction.payload.block_body.header.utxosRoot).toBe(
    body.header.utxosRoot,
  );
});

it.each(["key", "raw-script", "root"] as const)(
  "refuses %s substitution under the authenticated header",
  async (mutation) => {
    const fixture = await nativeDecodingFixture({
      direction: 0,
      item: Buffer.from("8200428109", "hex"),
    });
    const body = fixture.predecessor.reconstruction.payload.block_body;
    let utxos = body.utxos;
    if (mutation === "key")
      utxos = [
        ["00".repeat(fixture.key.length), fixture.output.toString("hex")],
      ];
    if (mutation === "raw-script")
      utxos = [
        [
          fixture.key.toString("hex"),
          fixture.output.subarray(0, -1).toString("hex") + "08",
        ],
      ];
    const payload = {
      ...fixture.predecessor.reconstruction.payload,
      block_body: {
        ...body,
        utxos,
        ...(mutation === "root"
          ? { header: { ...body.header, utxosRoot: "00".repeat(32) } }
          : {}),
      },
    };
    await expect(
      reconstructDaPayload({
        payloadEnvelopeCbor: await reencodeFixturePayload(payload),
        expectedHeaderHash: fixture.predecessor.headerHash,
      }),
    ).rejects.toThrow();
  },
);
