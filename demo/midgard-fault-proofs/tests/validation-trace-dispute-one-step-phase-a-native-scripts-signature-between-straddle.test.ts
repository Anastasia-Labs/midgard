/** The validation-trace dispute runner at the phase-A native scripts
 * signature between step whose token witness is the largest one: a token
 * read across two whole chunks of a native script three chunks long, over
 * the signature of a key between two signers of a transaction signed by
 * the most signers its witness field holds, proven between them by the
 * deepest siblings that signer set has. Driven through the phase resolver
 * and the semantic resolver to the award, each move within the transaction
 * size limit less its reserve even when our wallet pays it
 * (support/validation-trace-dispute-one-step-phase-step.ts). */
import { describe, expect, it } from "vitest";

import { betweenSignersNativeScript } from "./helpers/native-script-shapes.js";
import { extraSignerKeyHash } from "./helpers/plutus-execution-block-fixture.js";
import { ownerKeyHash } from "./helpers/plutus-execution-seeds.js";
import { describePhaseStep } from "./support/validation-trace-dispute-one-step-phase-step.js";

/** The most address witnesses, 103 bytes each, the witness field holds. */
const SIGNERS = 318;
/** The signer below the key: the last leaf of the signer set's deepest
 * peak but one, so both its neighbours' proofs are that peak's depth. */
const LOWER = 127;
const DEPTH = 8;
/** Enough signatures of the key that the script runs to a third chunk. */
const BETWEEN = 270;
const CHUNK_BYTES = 4_095;
/** The step reading the first signature of the key, across the first two
 * chunks. */
const FROM = 657;

const MAX_TX_SIZE = 16_384;
const SIZE_RESERVE = 512;

const signers = [
  ownerKeyHash(),
  ...Array.from({ length: SIGNERS - 1 }, (_, index) =>
    extraSignerKeyHash(index),
  ),
];

const drive = describePhaseStep({
  role: "phase-A native scripts signature between, across two whole chunks,",
  refusedBy: "phase_a_native_scripts_signature_between_payload_semantic_v1",
  from: FROM,
  phase: "phaseANativeScripts",
  semantic: 12,
  evidence: { auxiliary: 3 },
  at: "phaseANativeScriptsSignatureBetweenSemantic",
  // A raised per-byte fee prices the moves over the per-move cap, so our
  // wallet pays them: their largest shape.
  minFeeA: 80,
  failingNative: {
    script: betweenSignersNativeScript(signers, LOWER, BETWEEN),
  },
  failingNativeMint: true,
  failingSigners: { extraSigners: SIGNERS - 1 },
});

describe("validation-trace dispute runner: the largest native-script token witness", () => {
  it("reads the token across two whole chunks, proving the key between two signers by the deepest siblings", async () => {
    const trace = await drive.dispute().material.steps(FROM);
    const witness = trace.witnesses[FROM]!.auxiliary as {
      kind: string;
      chunkProof: {
        chunkIndex: number;
        chunk: Uint8Array;
        totalLength: number;
      };
      nextChunkProof: { chunkIndex: number; chunk: Uint8Array } | null;
      signerProof: {
        kind: string;
        lowerIndex: number;
        lowerSiblings: readonly unknown[];
        upperSiblings: readonly unknown[];
      };
    };
    expect(witness.kind).toBe("nativeScriptToken");
    expect(witness.chunkProof.totalLength).toBeGreaterThan(2 * CHUNK_BYTES);
    expect(witness.chunkProof.chunkIndex).toBe(0);
    expect(witness.chunkProof.chunk).toHaveLength(CHUNK_BYTES);
    expect(witness.nextChunkProof?.chunkIndex).toBe(1);
    expect(witness.nextChunkProof?.chunk).toHaveLength(CHUNK_BYTES);
    expect(witness.signerProof.kind).toBe("between");
    expect(Number(witness.signerProof.lowerIndex)).toBe(LOWER);
    expect(witness.signerProof.lowerSiblings).toHaveLength(DEPTH);
    expect(witness.signerProof.upperSiblings).toHaveLength(DEPTH);
  }, 300_000);

  it("lands each move of the resolution within the transaction size limit less its reserve", () => {
    for (const label of [
      "PrepareSelected phaseANativeScripts",
      "Verify phaseANativeScripts 12",
    ])
      expect(drive.bytesOf(label)).toBeLessThanOrEqual(
        MAX_TX_SIZE - SIZE_RESERVE,
      );
  });
});
