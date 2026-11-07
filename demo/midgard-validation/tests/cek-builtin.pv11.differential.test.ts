import { describe, expect, it } from "vitest";

import type { MidgardCekDirectValueWitness } from "../src/cek-builtin.argument-kinds.js";
import {
  builtinValueToDirectWitness,
  evaluateMidgardCekDirectBuiltin,
} from "../src/cek-builtin.evaluate-direct-builtin.js";
import { midgardCekKnownBuiltinFailure } from "../src/cek-builtin.known-failure.js";
import { evaluatePinnedBuiltin } from "../src/cek-builtin.pinned-evaluator.js";
import {
  decodeMidgardCekConstantWitness,
  encodeMidgardCekConstantTypeCbor,
  encodeMidgardCekPlutusData,
  type MidgardCekConstantType,
  midgardCekConstantWitnessToUplc,
} from "../src/cek-constant.js";
import { midgardCekBuiltinArgumentCount } from "../src/cek-machine.midgard-cek-builtin-argument-count.js";
import { ConstTyTag, UPLCConst } from "../src/uplc-constant.js";
import { type Data, DataB, DataList, isData } from "../src/uplc-data.js";
import { Builtin, ErrorUPLC, type UPLCTerm } from "../src/uplc-term.js";
import {
  holdsPoint,
  type Pv11Case,
  pv11Corpus,
} from "./helpers/cek-pv11-corpus.js";
import { evaluatorCharge } from "./helpers/cek-staged-failure-charge.js";
import { app, force, lam } from "./helpers/cek-uplc-terms.js";

/**
 * The PV 11 builtins (tags 87 to 100) in proof mode against the pinned
 * reference evaluator, on a generated corpus with the boundary of every
 * Plutus-defined failure: the outcome, the result, whether a failure is
 * charged, and the charge. The charge is checked twice: against the
 * evaluator's own charge for the application (its default Plutus V3 cost
 * model) and, for every application a program can spell, against what a
 * program evaluated under Midgard's pinned cost model is charged for it.
 * Every failure is one the direct failure arm proves, and no success is.
 */

const POINT_TYPES = new Map<number, MidgardCekConstantType>([
  [ConstTyTag.bls12_381_G1_element, { kind: "blsG1" }],
  [ConstTyTag.bls12_381_G2_element, { kind: "blsG2" }],
]);

/** A constant's direct witness; points and sequences of points by their compressed bytes. */
const witnessOf = (constant: UPLCConst): MidgardCekDirectValueWitness => {
  const [head, element] = constant.type;
  const point = POINT_TYPES.get(element ?? -1);
  if (
    (head === ConstTyTag.list || head === ConstTyTag.array) &&
    point !== undefined
  ) {
    const witness = {
      typeCbor: encodeMidgardCekConstantTypeCbor({
        kind: head === ConstTyTag.list ? "list" : "array",
        element: point,
      }),
      payloadCbor: encodeMidgardCekPlutusData(
        new DataList(
          (constant.value as readonly Uint8Array[]).map(
            (bytes) => new DataB(bytes),
          ),
        ),
      ),
    };
    decodeMidgardCekConstantWitness(witness);
    return { kind: "constant", witness };
  }
  return builtinValueToDirectWitness(constant);
};

/** A constant result witness as the evaluator's value: a point as its bytes. */
const resultValue = (result: MidgardCekDirectValueWitness): UPLCConst => {
  if (result.kind !== "constant") throw new Error("not a constant result");
  const decoded = decodeMidgardCekConstantWitness(result.witness);
  if (decoded.type.kind === "blsG1" || decoded.type.kind === "blsG2") {
    return new UPLCConst(
      [
        decoded.type.kind === "blsG1"
          ? ConstTyTag.bls12_381_G1_element
          : ConstTyTag.bls12_381_G2_element,
      ],
      (decoded.payload as DataB).bytes,
    );
  }
  return midgardCekConstantWitnessToUplc(result.witness);
};

const canonical = (value: unknown): unknown => {
  if (typeof value === "bigint") return `${value.toString(10)}n`;
  if (value instanceof Uint8Array) return Buffer.from(value).toString("hex");
  if (isData(value))
    return `data:${Buffer.from(encodeMidgardCekPlutusData(value as Data)).toString("hex")}`;
  if (Array.isArray(value)) return value.map(canonical);
  if (typeof value === "object" && value !== null && "fst" in value) {
    const { fst, snd } = value as { fst: unknown; snd: unknown };
    return { fst: canonical(fst), snd: canonical(snd) };
  }
  return value;
};

const show = (constant: UPLCConst): string =>
  JSON.stringify({ type: constant.type, value: canonical(constant.value) });

/** The application as a program body: forced as the builtin requires. */
const application = (pv11Case: Pv11Case): UPLCTerm => {
  const forces = pv11Case.tag >= 88 && pv11Case.tag <= 91 ? 1 : 0;
  let head: UPLCTerm = new Builtin(pv11Case.tag);
  for (let index = 0; index < forces; index += 1) head = force(head);
  return app(head, ...pv11Case.arguments);
};

const ZERO = { cpu: 0n, memory: 0n };
const corpus = pv11Corpus();

/** What a program evaluated under Midgard's pinned cost model is charged for the application. */
const programCharge = (pv11Case: Pv11Case, succeeds: boolean) => {
  const body = application(pv11Case);
  return evaluatorCharge(succeeds ? app(lam(new ErrorUPLC()), body) : body);
};

const MAX_SIGNED_64 = 9_223_372_036_854_775_807n;

/**
 * A charge as a program's whole spend sees it: after its start-up charge,
 * saturated to signed 64 bits as the evaluator saturates its spend.
 */
const spent = ({ cpu, memory }: { cpu: bigint; memory: bigint }) => ({
  cpu: cpu + 100n > MAX_SIGNED_64 ? MAX_SIGNED_64 : cpu + 100n,
  memory: memory + 100n > MAX_SIGNED_64 ? MAX_SIGNED_64 : memory + 100n,
});

/** Tags whose cases with points are checked against the evaluator's own charge. */
const POINT_TAGS = new Set([88, 89, 90, 91, 92, 93]);

describe("PV 11 builtins against the pinned reference evaluator", () => {
  it("cover every tag from 87 to 100 with successes and each failure", () => {
    const tags = new Set(corpus.map(({ tag }) => tag));
    expect([...tags].sort((a, b) => a - b)).toEqual(
      Array.from({ length: 14 }, (_, index) => 87 + index),
    );
    for (const pv11Case of corpus)
      expect(BigInt(pv11Case.arguments.length)).toBe(
        midgardCekBuiltinArgumentCount(BigInt(pv11Case.tag)),
      );
    expect(
      corpus.filter(holdsPoint).every(({ tag }) => POINT_TAGS.has(tag)),
    ).toBe(true);
    expect(corpus.length).toBeGreaterThan(150);
  });

  it("charge as the pinned cost model does where the evaluator's own charge is the reference", () => {
    // Points cannot be program constants, so a case holding one is checked
    // against the evaluator's charge under its default cost model. For these
    // tags that charge is the pinned model's on every case a program spells.
    let compared = 0;
    for (const pv11Case of corpus) {
      if (!POINT_TAGS.has(pv11Case.tag) || holdsPoint(pv11Case)) continue;
      const reference = evaluatePinnedBuiltin(pv11Case.tag, pv11Case.arguments);
      const charged = reference.kind === "value" || reference.charged;
      expect(
        spent(programCharge(pv11Case, reference.kind === "value")),
      ).toEqual(spent(charged ? reference.budget : ZERO));
      compared += 1;
    }
    expect(compared).toBeGreaterThan(50);
  });

  it.each(corpus.map((pv11Case) => [pv11Case.name, pv11Case] as const))(
    "%s: outcome, result and charge",
    (_name, pv11Case) => {
      const reference = evaluatePinnedBuiltin(pv11Case.tag, pv11Case.arguments);
      const witnesses = pv11Case.arguments.map(witnessOf);
      const proof = evaluateMidgardCekDirectBuiltin(
        BigInt(pv11Case.tag),
        witnesses,
      );
      expect(proof.kind).toBe(
        reference.kind === "value" ? "success" : "failure",
      );
      expect(
        midgardCekKnownBuiltinFailure(BigInt(pv11Case.tag), witnesses),
      ).toBe(reference.kind !== "value");
      if (reference.kind === "value" && proof.kind === "success")
        expect(show(resultValue(proof.result))).toBe(show(reference.value));
      const charged = reference.kind === "value" || reference.charged;
      if (!charged) expect(proof.budget).toEqual(ZERO);
      if (holdsPoint(pv11Case))
        expect(proof.budget).toEqual(charged ? reference.budget : ZERO);
      else
        expect(spent(proof.budget)).toEqual(
          spent(programCharge(pv11Case, reference.kind === "value")),
        );
    },
  );
});
