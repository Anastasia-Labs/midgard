import {
  Application,
  Builtin,
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  buildMidgardCanonicalCekProgramV1,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
} from "../src/cek-program.js";

const compile = (
  version: readonly [number, number, number],
): Buffer => {
  const program = new UPLCProgram(
    [version[0], version[1], version[2]],
    new Application(
      new Lambda(
        new Application(
          new Application(Builtin.addInteger, new UPLCVar(0)),
          UPLCConst.int(1),
        ),
      ),
      UPLCConst.int(41),
    ),
  );
  return Buffer.from(UPLCEncoder.compile(program).toBuffer().buffer);
};

const compileLargeString = (byteLength: number): Buffer => {
  const program = new UPLCProgram(
    [1, 1, 0],
    UPLCConst.str("x".repeat(byteLength)),
  );
  return Buffer.from(UPLCEncoder.compile(program).toBuffer().buffer);
};

describe("canonical V1 CEK programs", () => {
  it("turns raw UPLC into one deterministic content-addressed graph", () => {
    const script = compile([1, 1, 0]);
    const first = buildMidgardCanonicalCekProgramV1(script);
    const second = buildMidgardCanonicalCekProgramV1(script);

    expect(first.envelope).toEqual(second.envelope);
    expect(first.envelopeCbor).toEqual(second.envelopeCbor);
    expect(first.envelopeHash).toEqual(second.envelopeHash);
    expect(first.material.size).toBeGreaterThan(0);
    expect(first.envelope.nodeCount).toBe(BigInt(first.material.size));
    expect(first.envelope.materialByteLength).toBe(
      [...first.material.values()].reduce(
        (total, node) => total + BigInt(node.preimage.length),
        0n,
      ),
    );
    expect(first.envelope.nodeCount).toBeLessThanOrEqual(
      BigInt(MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT),
    );
    expect(first.envelope.materialByteLength).toBeLessThanOrEqual(
      BigInt(MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES),
    );
    expect(Buffer.from(first.envelope.termRoot).toString("hex")).toBe(
      "f4906930d1dd9b7010019a55cf5c3eafacd451473a633aeb28d836cab4410ba3",
    );
    expect(Buffer.from(first.envelopeHash).toString("hex")).toBe(
      "0dfd4adfb26d0a893123a345ed89b4dfb0dbb8fb5fb3a4db5585d9ff31934cca",
    );
  });

  it("fails closed on an unpinned UPLC version without a stale raw-byte cap", () => {
    expect(() =>
      buildMidgardCanonicalCekProgramV1(compile([1, 0, 0])),
    ).toThrow(/only UPLC 1\.1\.0/u);
    const raw = compileLargeString(7_000);
    expect(raw.length).toBeGreaterThan(6_911);
    const canonical = buildMidgardCanonicalCekProgramV1(raw);
    expect(canonical.envelopeCbor.length).toBeLessThanOrEqual(48);
    expect(canonical.envelope.materialByteLength).toBeGreaterThan(6_911n);
  });

  it("restores canonical builtin forces and rejects lossy force normalization", () => {
    const canonical = buildMidgardCanonicalCekProgramV1(
      Buffer.from(
        UPLCEncoder.compile(
          new UPLCProgram([1, 1, 0], Builtin.headList),
        ).toBuffer().buffer,
      ),
    );
    expect(canonical.material.size).toBeGreaterThanOrEqual(2);

    // Bare and over-forced Flat both decode to Harmonic's same normalized
    // Builtin AST. Neither is the canonical UPLC 1.1.0 encoding.
    expect(() =>
      buildMidgardCanonicalCekProgramV1(
        Buffer.from("0101007430", "hex"),
      ),
    ).toThrow(/exactly the builtin forces/u);
    expect(() =>
      buildMidgardCanonicalCekProgramV1(
        Buffer.from("010100557421", "hex"),
      ),
    ).toThrow(/exactly the builtin forces/u);
  });

});
