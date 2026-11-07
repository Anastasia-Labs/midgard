import { describe, expect, it } from "vitest";

import { midgardCekSemanticMkCons } from "../src/cek-executor.semantic-constructors.js";
import {
  MIDGARD_CEK_SEMANTIC_EMPTY_MATERIAL,
  type MidgardCekSemanticArgument,
  type MidgardCekSemanticStep,
} from "../src/cek-executor.semantic-step.js";
import { executeMidgardCekStructuralProgram } from "../src/index.js";
import { constT, UPLCConst } from "../src/uplc-constant.js";
import { type Data, DataB, DataConstr } from "../src/uplc-data.js";
import { Builtin, type UPLCTerm } from "../src/uplc-term.js";
import {
  app,
  discard,
  force,
  lam,
  MIDGARD_CEK_TX_BUDGET,
  prepareMidgardCekProgram,
} from "./helpers/cek-uplc-terms.js";
import { builtin } from "./helpers/uplc-builtins.js";

/**
 * A builtin applied to arguments outside its argument kinds fails, as on L1,
 * whatever the arguments' size, and a semantic mkCons is built only of an
 * item of the list's element type. The verifier proves the type failure
 * when the arguments' revealed payloads together fit one direct payload;
 * over larger arguments the proof-mode run stops at its own check of that
 * step instead of producing a step the verifier refuses.
 */

const CONTEXT: Data = new DataConstr(0n, []);

// Two payloads that each fit one direct payload and together do not.
const LARGE = 6_000;
const SMALL = 10;

const bytes = (length: number) =>
  UPLCConst.byteString(new DataB(new Uint8Array(length).fill(7)).bytes);
const text = (length: number) => UPLCConst.str("x".repeat(length));
const bytesList = (length: number) =>
  UPLCConst.listOf(constT.byteStr)([
    new DataB(new Uint8Array(length).fill(9)).bytes,
  ] as never);
const textList = (length: number) =>
  UPLCConst.listOf(constT.str)(["x".repeat(length)] as never);

const mkCons = (item: UPLCTerm, list: UPLCTerm): UPLCTerm =>
  lam(discard(app(force(builtin("mkCons")), item, list)));
const append = (left: UPLCTerm, right: UPLCTerm): UPLCTerm =>
  lam(discard(app(builtin("appendByteString"), left, right)));

const runOf = (script: UPLCTerm, mode: "proof" | "verdict") => {
  const prepared = prepareMidgardCekProgram(script, CONTEXT);
  return executeMidgardCekStructuralProgram({
    root: prepared.root,
    material: prepared.material.values(),
    constantWitnesses: prepared.constantWitnesses,
    maxSteps: 0xffff_ffff,
    executionBudget: MIDGARD_CEK_TX_BUDGET,
    mode,
  });
};

const large: readonly (readonly [string, UPLCTerm])[] = [
  [
    "mkCons of bytes onto a list of strings",
    mkCons(bytes(LARGE), textList(LARGE)),
  ],
  ["appendByteString of bytes and a string", append(bytes(LARGE), text(LARGE))],
];

// Each case: its label, the script, its halt, and the one builtin step the
// proof-mode run records (kind and builtin).
const proved: readonly (readonly [
  string,
  UPLCTerm,
  string,
  readonly [string, Builtin],
])[] = [
  [
    "mkCons of small bytes onto a list of strings",
    mkCons(bytes(SMALL), textList(SMALL)),
    "haltError",
    ["executeBuiltinTypeFailure", builtin("mkCons")],
  ],
  [
    "appendByteString of small bytes and a string",
    append(bytes(SMALL), text(SMALL)),
    "haltError",
    ["executeBuiltinTypeFailure", builtin("appendByteString")],
  ],
  [
    "mkCons of large bytes onto a list of large bytes",
    mkCons(bytes(LARGE), bytesList(LARGE)),
    "haltSuccess",
    ["executeBuiltinSemantic", builtin("mkCons")],
  ],
];

describe("CEK builtin type failures on large arguments", () => {
  it.each(large)("fails %s of large payloads", (_, script) => {
    expect(runOf(script, "verdict").terminalState.mode).toBe("haltError");
  });

  it.each(large)(
    "stops the proof-mode run of %s of large payloads at the type failure",
    (_, script) => {
      expect(() => runOf(script, "proof")).toThrow(
        "generated CEK executeBuiltinTypeFailure transition does not pass the consensus verifier",
      );
    },
  );

  it.each(proved)(
    "runs %s to its halt in both modes",
    (_, script, halt, [kind, builtin]) => {
      expect(runOf(script, "verdict").terminalState.mode).toBe(halt);
      const proof = runOf(script, "proof");
      expect(proof.terminalState.mode).toBe(halt);
      const steps = "steps" in proof ? proof.steps : [];
      expect(
        steps.flatMap((step) =>
          step.witness.kind.startsWith("executeBuiltin") &&
          "tag" in step.witness
            ? [[step.witness.kind, step.witness.tag]]
            : [],
        ),
      ).toEqual([[kind, BigInt(builtin.builtinTag)]]);
    },
  );

  it("builds a semantic mkCons only of an item of the list's element type", () => {
    const root = new Uint8Array(32);
    const item: MidgardCekSemanticArgument = {
      type: { kind: "bytes" },
      root,
      node: { kind: "bytes" } as never,
    };
    const step = { resolve: () => item } as unknown as MidgardCekSemanticStep;
    const textList = {
      type: { kind: "list", element: { kind: "string" } },
      root,
      node: { kind: "list", itemsRoot: root, itemsCount: 1n },
    } as unknown as MidgardCekSemanticArgument;
    expect(() =>
      midgardCekSemanticMkCons(
        step,
        textList,
        MIDGARD_CEK_SEMANTIC_EMPTY_MATERIAL,
      ),
    ).toThrow("CEK semantic mkCons requires an item of the list type");
  });
});
