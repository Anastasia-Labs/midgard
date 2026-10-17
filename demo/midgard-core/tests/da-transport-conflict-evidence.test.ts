import { describe, expect, it } from "vitest";

import { decodeSingleCbor, encodeCbor } from "../src/codec/cbor.js";
import {
  decodeDaConflictingSignatureHeaderEvidenceCbor,
  encodeDaConflictingSignatureHeaderEvidenceCbor,
} from "../src/da-transport.js";

const b = (byte: number, count: number): Buffer => Buffer.alloc(count, byte);

// Owner ruling 2026-10-07: equivocation is one signer, one header hash and two
// availability commitments. A pair over sibling headers is not evidence.
const sameHeaderEvidence = () => ({
  signerIndex: 4,
  daVkey: b(0x0c, 32),
  lowerHeaderHash: b(0x02, 28),
  lowerCommitmentCbor: Buffer.from([0x80]),
  lowerHeaderWitness: Buffer.concat([Buffer.from([4]), b(0xaa, 64)]),
  upperHeaderHash: b(0x02, 28),
  upperCommitmentCbor: Buffer.from([0x81, 0x00]),
  upperHeaderWitness: Buffer.concat([Buffer.from([4]), b(0xbb, 64)]),
});

describe("DA conflicting signature/header evidence header rule", () => {
  it("round-trips one header with two commitments", () => {
    const evidence = sameHeaderEvidence();
    expect(
      decodeDaConflictingSignatureHeaderEvidenceCbor(
        encodeDaConflictingSignatureHeaderEvidenceCbor(evidence),
      ),
    ).toEqual(evidence);
  });

  it("refuses to encode or decode a pair over sibling headers", () => {
    expect(() =>
      encodeDaConflictingSignatureHeaderEvidenceCbor({
        ...sameHeaderEvidence(),
        upperHeaderHash: b(0x03, 28),
      }),
    ).toThrow(/evidence must name one header hash/u);
    // A peer's bytes bypass the encoder: rewrite tuple item 5 (upper header).
    const tuple = decodeSingleCbor(
      encodeDaConflictingSignatureHeaderEvidenceCbor(sameHeaderEvidence()),
    ) as unknown[];
    tuple[5] = b(0x03, 28);
    const siblingBytes = encodeCbor(tuple);
    expect(() =>
      decodeDaConflictingSignatureHeaderEvidenceCbor(siblingBytes),
    ).toThrow(/evidence must name one header hash/u);
  });
});
