import { describe, expect, it } from "vitest";

import {
  decodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramEnvelope,
  hashMidgardCekProgramEnvelope,
} from "../src/cek-proof.js";
import { hash } from "./cek-proof.make-nested-list-constant-program-material.js";

const envelopeAt = (uplcVersion: readonly [bigint, bigint, bigint]): Buffer =>
  encodeMidgardCekProgramEnvelope({
    uplcVersion,
    termRoot: hash(1),
    nodeCount: 3n,
    materialByteLength: 144n,
  });

describe("CEK program envelope version", () => {
  it("admits 1.0.0 and 1.1.0 and commits the version in the envelope hash", () => {
    const v100 = decodeMidgardCekProgramEnvelope(envelopeAt([1n, 0n, 0n]));
    const v110 = decodeMidgardCekProgramEnvelope(envelopeAt([1n, 1n, 0n]));
    expect(v100.uplcVersion).toEqual([1n, 0n, 0n]);
    expect(v110.uplcVersion).toEqual([1n, 1n, 0n]);
    expect(encodeMidgardCekProgramEnvelope(v100)).toEqual(
      envelopeAt([1n, 0n, 0n]),
    );
    expect(hashMidgardCekProgramEnvelope(v100)).not.toEqual(
      hashMidgardCekProgramEnvelope(v110),
    );
  });

  it("refuses every other program version", () => {
    for (const uplcVersion of [
      [2n, 0n, 0n],
      [1n, 2n, 0n],
      [1n, 1n, 1n],
      [1n, 0n, 1n],
      [0n, 1n, 0n],
    ] as const) {
      expect(() =>
        decodeMidgardCekProgramEnvelope(envelopeAt(uplcVersion)),
      ).toThrow(/only UPLC 1\.0\.0 and 1\.1\.0/u);
    }
  });
});
