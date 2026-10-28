import { readFileSync } from "node:fs";

import {
  encodeCbor,
  encodeMidgardCekSequenceNode,
  encodeMidgardCekTermNode,
  type Hash32,
  hashMidgardCekSequenceNode,
  hashMidgardCekTermNode,
  hashMidgardVersionedScript,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  type MidgardCekTermNode,
  verifyMidgardCekProgramMaterial,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import {
  buildMidgardCanonicalCekProgram,
  buildMidgardCanonicalScriptArtifact,
} from "../src/cek-program.js";
import { evaluateMidgardCekScript } from "../src/phase-b.evaluate-cek-script.js";
import { scriptDecodeRecordOfScript } from "../src/script-decode.step-walk.js";
import { UPLCConst } from "../src/uplc-constant.js";
import {
  Application,
  Case,
  Constr,
  Delay,
  ErrorUPLC,
  Force,
  Lambda,
  UPLCProgram,
  UPLCVar,
} from "../src/uplc-term.js";
import { deepFlatProgram } from "./cek-program.deep-flat.fixture.js";
import { builtin } from "./helpers/uplc-builtins.js";
import { buildCekProgramFromRawFlat } from "./validation-fixtures.flat-program.js";

const compile = (version: readonly [number, number, number]): Buffer =>
  Buffer.from(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram(
        [version[0], version[1], version[2]],
        new Application(
          new Lambda(
            new Application(
              new Application(builtin("addInteger"), new UPLCVar(0)),
              UPLCConst.int(1),
            ),
          ),
          UPLCConst.int(41),
        ),
      ),
    ),
  );

describe("local Flat dispute material", () => {
  it("preserves child, sequence, and deduplicated material order in a mixed tree", () => {
    const body = new Case(
      new Constr(7n, [new UPLCVar(0), new Delay(new ErrorUPLC())]),
      [new Lambda(new UPLCVar(1)), new Force(new Delay(new ErrorUPLC()))],
    );
    const program = buildCekProgramFromRawFlat(
      Buffer.from(
        encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], body)),
      ),
    );
    const expected: [string, Uint8Array, Uint8Array][] = [];
    const term = (node: MidgardCekTermNode) => {
      const root = hashMidgardCekTermNode(node);
      expected.push(["term", root, encodeMidgardCekTermNode(node)]);
      return root;
    };
    const sequence = (head: Hash32, tail: Hash32, length: bigint) => {
      const node = { head, tail, length };
      const root = hashMidgardCekSequenceNode(node);
      expected.push(["sequence", root, encodeMidgardCekSequenceNode(node)]);
      return root;
    };
    // Scrutinee first; each sequence visits its last child before its first.
    const error = term({ kind: "error" });
    const delayed = term({ kind: "delay", body: error });
    const lastField = sequence(delayed, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT, 1n);
    const variable0 = term({ kind: "variable", index: 1n });
    const fields = sequence(variable0, lastField, 2n);
    const scrutinee = term({
      kind: "constr",
      tag: 7n,
      termsCount: 2n,
      termsRoot: fields,
    });
    const forced = term({ kind: "force", term: delayed });
    const lastBranch = sequence(forced, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT, 1n);
    const variable1 = term({ kind: "variable", index: 2n });
    const lambda = term({ kind: "lambda", body: variable1 });
    const branches = sequence(lambda, lastBranch, 2n);
    const root = term({
      kind: "case",
      scrutinee,
      branchesCount: 2n,
      branchesRoot: branches,
    });
    expect(program.termRoot).toEqual(root);
    expect(
      [...program.material.values()].map(({ kind, root, preimage }) => [
        kind,
        root,
        preimage,
      ]),
    ).toEqual(expected);
  });

  describe.each([
    ["case", 8_000],
    ["constr", 4_000],
    ["application", 8_000],
  ] as const)("deep %s terms at depth %i", (shape, depth) => {
    it("preserves the canonical graph and material order", () => {
      const program = buildCekProgramFromRawFlat(deepFlatProgram(shape, depth));
      const error = hashMidgardCekTermNode({ kind: "error" });
      let root = error;
      const roots = [Buffer.from(root).toString("hex")];
      for (let index = 0; index < depth; index += 1) {
        if (shape === "case") {
          root = hashMidgardCekTermNode({
            kind: "case",
            scrutinee: root,
            branchesCount: 0n,
            branchesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          });
        } else if (shape === "constr") {
          const sequence = hashMidgardCekSequenceNode({
            head: root,
            tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
            length: 1n,
          });
          roots.push(Buffer.from(sequence).toString("hex"));
          root = hashMidgardCekTermNode({
            kind: "constr",
            tag: 0n,
            termsCount: 1n,
            termsRoot: sequence,
          });
        } else {
          root = hashMidgardCekTermNode({
            kind: "application",
            function: root,
            argument: error,
          });
        }
        roots.push(Buffer.from(root).toString("hex"));
      }
      expect(program.termRoot).toEqual(root);
      expect(BigInt(program.material.size)).toBe(BigInt(roots.length));
      expect([...program.material.keys()]).toEqual(roots);
      expect(program.constantWitnesses.size).toBe(0);
    });

    it("builds a constant past the direct payload bound at the deepest leaf as its decode record names it", () => {
      const program = buildCekProgramFromRawFlat(
        deepFlatProgram(shape, depth, true),
      );
      const record = scriptDecodeRecordOfScript(3, program.scriptBytes);
      if (record.k !== "decoded") throw new Error("expected decoded record");
      expect(program.termRoot).toEqual(record.termRoot);
    });
  });

  it("derives the deployed history spending program", () => {
    const blueprint = JSON.parse(
      readFileSync(
        new URL("../../../onchain/aiken/plutus.json", import.meta.url),
        "utf8",
      ),
    ) as { validators: readonly { title: string; compiledCode: string }[] };
    const history = blueprint.validators.find(
      (validator) => validator.title === "user_events/history.history.spend",
    );
    expect(history).toBeDefined();
    expect(
      buildMidgardCanonicalCekProgram(Buffer.from(history!.compiledCode, "hex"))
        .material.size,
    ).toBeGreaterThan(0);
  });

  it.each([
    ["map", "0101004c0103a101010001"],
    ["nested map", "0101004c0105a101a102030001"],
    ["list containing map", "0101004bd70903a101010001"],
    ["pair containing map", "0101004bded88103a10101000e01"],
  ])("derives Cardano Flat Data constants: %s", (_name, hex) => {
    expect(
      buildCekProgramFromRawFlat(Buffer.from(hex, "hex")).constantWitnesses
        .size,
    ).toBe(1);
  });

  it("derives one deterministic graph with Cardano variable indices", () => {
    const first = buildCekProgramFromRawFlat(compile([1, 1, 0]));
    const second = buildCekProgramFromRawFlat(compile([1, 1, 0]));
    expect(first.termRoot).toEqual(second.termRoot);
    expect([...first.material.values()]).toEqual([...second.material.values()]);
    const variableRoot = (index: bigint): string =>
      Buffer.from(hashMidgardCekTermNode({ kind: "variable", index })).toString(
        "hex",
      );
    expect(first.material.has(variableRoot(1n))).toBe(true);
    expect(first.material.has(variableRoot(0n))).toBe(false);
    expect(Buffer.from(first.termRoot).toString("hex")).toBe(
      "450a63254c66d0c6c79ebcdd458b77410fd0ad65f43280d7e6461c136f96644c",
    );
    expect(
      verifyMidgardCekProgramMaterial(first.termRoot, first.material.values())
        .nodeCount,
    ).toBe(BigInt(first.material.size));
  });

  it("retains UPLC 1.0.0 and 1.1.0 and admits constr only from 1.1.0", () => {
    const v100 = buildCekProgramFromRawFlat(compile([1, 0, 0]));
    const v110 = buildCekProgramFromRawFlat(compile([1, 1, 0]));
    expect(v100.uplcVersion).toEqual([1n, 0n, 0n]);
    expect(v110.uplcVersion).toEqual([1n, 1n, 0n]);
    expect(v100.termRoot).toEqual(v110.termRoot);
    const constrUnder100 = Buffer.from(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram([1, 0, 0], new Constr(0, [])),
      ),
    );
    expect(() => buildCekProgramFromRawFlat(constrUnder100)).toThrow(
      /refused script|constrOrCaseBeforeV110/u,
    );
    expect(
      buildCekProgramFromRawFlat(
        Buffer.from(
          encodeMidgardCekCardanoFlatProgram(
            new UPLCProgram([1, 1, 0], new Constr(0, [])),
          ),
        ),
      ).uplcVersion,
    ).toEqual([1n, 1n, 0n]);
  });

  it("fails closed on an unpinned UPLC version without a stale raw-byte cap", () => {
    for (const version of [
      [2, 0, 0],
      [1, 2, 0],
      [1, 1, 1],
    ] as const) {
      expect(() => buildCekProgramFromRawFlat(compile(version))).toThrow(
        /refused script|only UPLC|outsideProfile/u,
      );
    }
    const raw = Buffer.from(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram([1, 1, 0], UPLCConst.str("x".repeat(7_000))),
      ),
    );
    expect(raw.length).toBeGreaterThan(6_911);
    const program = buildCekProgramFromRawFlat(raw);
    expect(
      [...program.material.values()].reduce(
        (total, node) => total + node.preimage.length,
        0,
      ),
    ).toBeGreaterThan(6_911);
  });

  it("derives and executes a builtin instantiated through a lambda-bound variable", () => {
    const body = new Lambda(
      new Application(
        new Lambda(
          new Application(
            new Application(
              new Application(new Force(new UPLCVar(0)), UPLCConst.bool(true)),
              UPLCConst.unit,
            ),
            UPLCConst.unit,
          ),
        ),
        builtin("ifThenElse"),
      ),
    );
    const scriptBytes = encodeCbor(
      Buffer.from(
        encodeMidgardCekCardanoFlatProgram(new UPLCProgram([1, 1, 0], body)),
      ),
    );
    const record = scriptDecodeRecordOfScript(3, scriptBytes);
    expect(record).toMatchObject({ k: "decoded", phase2Class: 0 });
    if (record.k !== "decoded") throw new Error("expected decoded record");
    const program = buildMidgardCanonicalCekProgram(scriptBytes);
    expect(program.termRoot).toEqual(record.termRoot);
    const outcome = evaluateMidgardCekScript(
      {
        scriptBytes,
        scriptLanguage: "PlutusV3",
        contextCbor: Buffer.from("d87980", "hex"),
        executionIndex: 0n,
        maxSteps: 100,
      },
      "proof",
    );
    expect(outcome.result.kind).toBe("accepted");
    expect(outcome.execution?.terminalState.mode).toBe("haltSuccess");
  });

  it("detects local material mutation and unrelated nodes", () => {
    const program = buildCekProgramFromRawFlat(compile([1, 1, 0]));
    const material = [...program.material.values()];
    const mutated = material.map((entry, index) => {
      const preimage = Buffer.from(entry.preimage);
      if (index === 0) preimage[preimage.length - 1]! ^= 1;
      return { ...entry, preimage };
    });
    expect(() =>
      verifyMidgardCekProgramMaterial(program.termRoot, mutated),
    ).toThrow();
    const unrelated = buildCekProgramFromRawFlat(
      encodeMidgardCekCardanoFlatProgram(
        new UPLCProgram([1, 1, 0], new ErrorUPLC()),
      ),
    );
    const all = new Map(
      [...material, ...unrelated.material.values()].map((entry) => [
        Buffer.from(entry.root).toString("hex"),
        entry,
      ]),
    );
    expect(() =>
      verifyMidgardCekProgramMaterial(program.termRoot, all.values()),
    ).toThrow(/unreachable/u);
  });
});

describe("stored Flat script identity", () => {
  it("wraps raw Flat exactly once and hashes the exact carried representation", () => {
    const source = compile([1, 1, 0]);
    const artifact = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });
    expect(artifact.canonicalMidgardCredentialScript).toEqual({
      language: "PlutusV3",
      scriptBytes: encodeCbor(source),
    });
    expect(artifact.canonicalMidgardCredentialScriptHash).toBe(
      hashMidgardVersionedScript(artifact.canonicalMidgardCredentialScript),
    );
    expect(artifact.canonicalMidgardCredentialScriptHash).not.toBe(
      hashMidgardVersionedScript({ language: "PlutusV3", scriptBytes: source }),
    );
  });

  it("preserves the requested language tag", () => {
    const source = compile([1, 1, 0]);
    const plutus = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });
    const midgard = buildMidgardCanonicalScriptArtifact({
      language: "MidgardV1",
      sourceRawFlatProgramBytes: source,
    });
    expect(midgard.canonicalMidgardCredentialScript.scriptBytes).toEqual(
      plutus.canonicalMidgardCredentialScript.scriptBytes,
    );
    expect(midgard.canonicalMidgardCredentialScriptHash).not.toBe(
      plutus.canonicalMidgardCredentialScriptHash,
    );
  });

  it("does not normalize alternate Flat spellings while wrapping them", () => {
    const source = Buffer.from("0101004c000101000001", "hex");
    const artifact = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });
    expect(artifact.canonicalMidgardCredentialScript.scriptBytes).toEqual(
      encodeCbor(source),
    );
  });

  it("isolates stored bytes from caller and cross-view mutation", () => {
    const source = compile([1, 1, 0]);
    const stored = encodeCbor(source);
    const artifact = buildMidgardCanonicalScriptArtifact({
      language: "PlutusV3",
      sourceRawFlatProgramBytes: source,
    });
    source.fill(0);
    artifact.canonicalMidgardCredentialScript.scriptBytes.fill(0);
    expect(artifact.canonicalMidgardCredentialScript.scriptBytes).toEqual(
      stored,
    );
  });
});
