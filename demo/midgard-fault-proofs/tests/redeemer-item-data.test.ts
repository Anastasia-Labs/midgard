import {
  buildMidgardRedeemerItemProofTrace,
  encodeCbor,
  MidgardRedeemerItemProofModes,
} from "@al-ft/midgard-core";
import {
  redeemerItemControlData,
  redeemerItemProofWitnessData,
} from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  decodeRedeemerItemControlData,
  decodeRedeemerItemWitnessData,
  deriveRedeemerItemStepPlan,
} from "../src/redeemer-item-data.js";

const traceFor = (value: string) =>
  buildMidgardRedeemerItemProofTrace({
    itemIndex: 0,
    itemCount: 1,
    itemBytes: encodeCbor([0n, 0n, Buffer.from(value, "hex"), [10n, 20n]]),
    mode: MidgardRedeemerItemProofModes.Data,
  });

describe("shared raw redeemer item codec", () => {
  it.each([
    ["integer", "00"],
    ["large integer", Data.to(-(2n ** 90n))],
    ["source blob", Data.to("ab".repeat(4500))],
    ["list", "9f0001ff"],
    ["map", "a200010203"],
    ["small constructor", "d8799f0102ff"],
    ["large constructor", "d8668218809f01ff"],
  ] satisfies [string, string][])(
    "round-trips every %s action and its canonical successor",
    (_name, value) => {
      const trace = traceFor(value);
      expect(trace.steps.length).toBeGreaterThan(2);
      for (const step of trace.steps) {
        const current = redeemerItemControlData(step.control);
        const witness = redeemerItemProofWitnessData(step.witness);
        const claimedNext = redeemerItemControlData(step.next);
        expect(decodeRedeemerItemControlData(current)).toEqual(step.control);
        expect(decodeRedeemerItemWitnessData(witness)).toEqual(step.witness);
        expect(
          deriveRedeemerItemStepPlan({ current, witness, claimedNext }).next,
        ).toEqual(step.next);
      }
      expect(
        decodeRedeemerItemControlData(redeemerItemControlData(trace.terminal)),
      ).toEqual(trace.terminal);
    },
  );
  it("refuses wrong wire arity, mutated source, and a different well-formed successor", () => {
    const trace = traceFor("00"),
      step = trace.steps[0]!;
    const current = redeemerItemControlData(step.control),
      witness = redeemerItemProofWitnessData(step.witness);
    expect(() =>
      decodeRedeemerItemControlData(new Constr(0, current.fields.slice(1))),
    ).toThrow();
    expect(() =>
      decodeRedeemerItemWitnessData(new Constr(0, [...witness.fields, 0n])),
    ).toThrow();
    expect(() =>
      deriveRedeemerItemStepPlan({
        current,
        witness,
        claimedNext: redeemerItemControlData(trace.terminal),
      }),
    ).toThrow();
    const mutated = redeemerItemProofWitnessData({
      ...step.witness,
      chunkProof: {
        ...step.witness.chunkProof!,
        chunk: Buffer.alloc(step.witness.chunkProof!.chunk.length),
      },
    });
    expect(() =>
      deriveRedeemerItemStepPlan({
        current,
        witness: mutated,
        claimedNext: redeemerItemControlData(step.next),
      }),
    ).toThrow();
  });
});
