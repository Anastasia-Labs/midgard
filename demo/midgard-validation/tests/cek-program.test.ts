import {
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramMaterialSidecar,
  hashMidgardVersionedScript,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core";
import {
  Application,
  Builtin,
  ErrorUPLC,
  Lambda,
  UPLCConst,
  UPLCEncoder,
  UPLCProgram,
  UPLCVar,
} from "@harmoniclabs/uplc";
import { describe, expect, it } from "vitest";

import {
  buildMidgardCanonicalCekProgram,
  buildMidgardCanonicalScriptArtifact,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT,
} from "../src/cek-program.js";

const compile = (version: readonly [number, number, number]): Buffer => {
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

const compileError = (): Buffer =>
  Buffer.from(
    UPLCEncoder.compile(new UPLCProgram([1, 1, 0], new ErrorUPLC())).toBuffer()
      .buffer,
  );

const materialShape = (
  entries: Iterable<{
    readonly kind: string;
    readonly root: Uint8Array;
    readonly preimage: Uint8Array;
  }>,
): readonly (readonly [string, string, string])[] =>
  [...entries]
    .map(
      (entry) =>
        [
          entry.kind,
          Buffer.from(entry.root).toString("hex"),
          Buffer.from(entry.preimage).toString("hex"),
        ] as const,
    )
    .sort((left, right) => left[1].localeCompare(right[1]));

describe("canonical V1 CEK programs", () => {
  it("turns raw UPLC into one deterministic content-addressed graph", () => {
    const script = compile([1, 1, 0]);
    const first = buildMidgardCanonicalCekProgram(script);
    const second = buildMidgardCanonicalCekProgram(script);

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
    expect(() => buildMidgardCanonicalCekProgram(compile([1, 0, 0]))).toThrow(
      /only UPLC 1\.1\.0/u,
    );
    const raw = compileLargeString(7_000);
    expect(raw.length).toBeGreaterThan(6_911);
    const canonical = buildMidgardCanonicalCekProgram(raw);
    expect(canonical.envelopeCbor.length).toBeLessThanOrEqual(48);
    expect(canonical.envelope.materialByteLength).toBeGreaterThan(6_911n);
  });

  it("restores canonical builtin forces and rejects lossy force normalization", () => {
    const canonical = buildMidgardCanonicalCekProgram(
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
      buildMidgardCanonicalCekProgram(Buffer.from("0101007430", "hex")),
    ).toThrow(/exactly the builtin forces/u);
    expect(() =>
      buildMidgardCanonicalCekProgram(Buffer.from("010100557421", "hex")),
    ).toThrow(/exactly the builtin forces/u);
  });
});

describe("canonical V1 script artifacts", () => {
  it("binds the existing program to one deterministic credential and exact sidecar", () => {
    const source = compile([1, 1, 0]);
    const program = buildMidgardCanonicalCekProgram(source);
    const first = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });
    const second = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });

    expect(first.canonicalProgram.envelope).toEqual(program.envelope);
    expect(first.canonicalProgram.envelopeCbor).toEqual(program.envelopeCbor);
    expect(first.canonicalProgram.envelopeHash).toEqual(program.envelopeHash);
    expect(materialShape(first.canonicalMaterialEntries)).toEqual(
      materialShape(program.material.values()),
    );
    const orderedRoots = first.canonicalMaterialEntries.map((entry) =>
      Buffer.from(entry.root).toString("hex"),
    );
    expect(orderedRoots).toEqual([...orderedRoots].sort());
    expect(first.canonicalMidgardCredentialScript).toEqual({
      language: "PlutusV3",
      scriptBytes: program.envelopeCbor,
    });
    expect(first.canonicalMidgardCredentialScriptHash).toBe(
      hashMidgardVersionedScript(first.canonicalMidgardCredentialScript),
    );
    expect(first.sourceRawScriptAuditHash).toBe(
      hashMidgardVersionedScript({
        language: "PlutusV3",
        scriptBytes: source,
      }),
    );
    expect(first.sourceRawScriptAuditHash).not.toBe(
      first.canonicalMidgardCredentialScriptHash,
    );
    expect(first.canonicalMaterialSidecarCbor).toEqual(
      encodeMidgardCekProgramMaterialSidecar([...program.material.values()]),
    );
    expect(first.canonicalMaterialSidecarCbor).toEqual(
      second.canonicalMaterialSidecarCbor,
    );
    expect(first.canonicalMidgardCredentialScriptHash).toBe(
      second.canonicalMidgardCredentialScriptHash,
    );

    const decoded = decodeMidgardCekProgramMaterialSidecar(
      first.canonicalMaterialSidecarCbor,
    );
    expect(materialShape(decoded)).toEqual(
      materialShape(first.canonicalMaterialEntries),
    );
    expect(
      verifyMidgardCekProgramMaterialBundle(
        [first.canonicalProgram.envelope],
        decoded,
      ),
    ).toHaveLength(1);
  });

  it("uses the requested canonical MidgardV1 language tag", () => {
    const source = compile([1, 1, 0]);
    const plutus = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });
    const midgard = buildMidgardCanonicalScriptArtifact({
      language: "MidgardV1",
      sourceRawFlatProgramBytes: source,
    });

    expect(midgard.canonicalMidgardCredentialScript.language).toBe("MidgardV1");
    expect(midgard.canonicalMidgardCredentialScript.scriptBytes).toEqual(
      plutus.canonicalMidgardCredentialScript.scriptBytes,
    );
    expect(midgard.canonicalMidgardCredentialScriptHash).not.toBe(
      plutus.canonicalMidgardCredentialScriptHash,
    );
    expect(midgard.canonicalMidgardCredentialScriptHash).toBe(
      hashMidgardVersionedScript(midgard.canonicalMidgardCredentialScript),
    );
  });

  it("rejects mutated preimages and valid unreachable material", () => {
    const source = compile([1, 1, 0]);
    const artifact = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });
    const mutated = artifact.canonicalMaterialEntries.map((entry, index) => {
      const preimage = Buffer.from(entry.preimage);
      if (index === 0) preimage[preimage.length - 1]! ^= 0x01;
      return {
        kind: entry.kind,
        root: Buffer.from(entry.root),
        preimage,
      };
    });
    expect(() =>
      verifyMidgardCekProgramMaterialBundle(
        [artifact.canonicalProgram.envelope],
        mutated,
      ),
    ).toThrow();

    const unreachable = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: compileError(),
    });
    const merged = new Map(
      artifact.canonicalMaterialEntries.map((entry) => [
        Buffer.from(entry.root).toString("hex"),
        entry,
      ]),
    );
    for (const entry of unreachable.canonicalMaterialEntries) {
      merged.set(Buffer.from(entry.root).toString("hex"), entry);
    }
    expect(merged.size).toBeGreaterThan(
      artifact.canonicalMaterialEntries.length,
    );
    expect(() =>
      verifyMidgardCekProgramMaterialBundle(
        [artifact.canonicalProgram.envelope],
        merged.values(),
      ),
    ).toThrow(/unreachable/u);
  });

  it("isolates all returned bytes from caller and cross-view mutation", () => {
    const source = compile([1, 1, 0]);
    const artifact = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });
    const fresh = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: Buffer.from(source),
    });
    const script = artifact.canonicalMidgardCredentialScript;
    const program = artifact.canonicalProgram;
    const material = artifact.canonicalMaterialEntries[0]!;

    source.fill(0);
    script.scriptBytes.fill(0);
    program.envelopeCbor.fill(0);
    program.envelopeHash.fill(0);
    const termRoot = program.envelope.termRoot;
    termRoot.fill(0);
    material.root.fill(0);
    material.preimage.fill(0);
    artifact.canonicalMaterialSidecarCbor.fill(0);
    const programMaterial = program.material.values().next().value;
    if (programMaterial !== undefined) {
      programMaterial.root.fill(0);
      programMaterial.preimage.fill(0);
    }

    expect(artifact.canonicalMidgardCredentialScript).toEqual(
      fresh.canonicalMidgardCredentialScript,
    );
    expect(artifact.canonicalProgram.envelope).toEqual(
      fresh.canonicalProgram.envelope,
    );
    expect(artifact.canonicalProgram.envelopeCbor).toEqual(
      fresh.canonicalProgram.envelopeCbor,
    );
    expect(artifact.canonicalProgram.envelopeHash).toEqual(
      fresh.canonicalProgram.envelopeHash,
    );
    expect(materialShape(artifact.canonicalMaterialEntries)).toEqual(
      materialShape(fresh.canonicalMaterialEntries),
    );
    expect(artifact.canonicalMaterialSidecarCbor).toEqual(
      fresh.canonicalMaterialSidecarCbor,
    );
  });
});
