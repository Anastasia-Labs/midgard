import { describe, expect, it } from "vitest";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import { verifyMidgardCekCoreStep } from "../src/cek-machine.verify-builtin.js";
import {
  midgardCekSerialiseLoopOf,
  planMidgardCekSerialiseHops,
} from "../src/cek-serialise.js";
import { plutusDataFromCborIterative } from "../src/plutus-data-iterative.decode.js";
import {
  blake2b256,
  evaluatorSerialiseCharge,
  evaluatorSerialisesTo,
  evaluatorSerialisesToLength,
  runOver,
  SERIALISE,
  serialiseSteps,
  stepCharge,
  valueNamed,
} from "./helpers/cek-serialise-runs.js";

/**
 * serialiseData at the honest maxima the PV11 evaluator still accepts within
 * one transaction's budget: 11,700 `Constr 0 []` in one list (35,102 bytes of
 * CBOR) and one 370,000-byte leaf (381,565 bytes). Each is proved by one
 * serialise step whose loop ends within the hop budget, with the evaluator's
 * verdict, length and charge. The evaluator reads no byte of a result past
 * 65,536 bytes but its length, so the bytes are checked against the reference
 * encoder too.
 */

const MAXIMA = ["flat_11700_empty_constrs", "leaf_of_370000_bytes"] as const;

describe("serialiseData at the honest maxima", () => {
  it.each(MAXIMA)(
    "proves %s with the PV11 evaluator's verdict, bytes and charge",
    (name) => {
      const { hex } = valueNamed(name);
      const verdict = runOver(SERIALISE, hex, "verdict");
      const proof = runOver(SERIALISE, hex, "proof");
      expect(verdict.result.kind).toBe("accepted");
      expect(proof.result).toEqual(verdict.result);
      const [step, ...others] = serialiseSteps(proof);
      expect(others).toHaveLength(0);
      expect(Buffer.from(step!.witness.serialised).toString("hex")).toBe(hex);
      expect(stepCharge(step!)).toEqual(evaluatorSerialiseCharge(hex));
      for (const { pre, post, witness } of proof.execution?.steps ?? []) {
        expect(verifyMidgardCekCoreStep(pre, post, witness)).toBe(true);
      }
      const bytes = Buffer.from(hex, "hex");
      expect(evaluatorSerialisesToLength(hex, bytes.length)).toBe(true);
      expect(evaluatorSerialisesToLength(hex, bytes.length + 1)).toBe(false);
      if (bytes.length <= 65_536) {
        expect(evaluatorSerialisesTo(hex, blake2b256(hex))).toBe(true);
      }
      expect(
        Buffer.from(
          encodeMidgardCekPlutusData(plutusDataFromCborIterative(bytes)),
        ).equals(bytes),
      ).toBe(true);
      const hops = planMidgardCekSerialiseHops(
        midgardCekSerialiseLoopOf(step!.witness),
      );
      expect(hops.at(-1)?.next).toBeNull();
    },
    600_000,
  );
});
