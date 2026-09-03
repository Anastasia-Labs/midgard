import {
  hashMidgardCekTermNode,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekCoreStepDataCbor,
  midgardCekCoreStepWitnessData,
  midgardCekMachineStateData,
} from "../src/cek-data.js";

const hash = (fill: number): Buffer => Buffer.alloc(32, fill);

describe("V1 CEK Plutus Data ABI", () => {
  it("matches the Aiken machine-state and application-step vector", () => {
    const pre: MidgardCekMachineState = {
      mode: "compute",
      executionIndex: 2n,
      focusRoot: hashMidgardCekTermNode({
        kind: "application",
        function: hash(1),
        argument: hash(2),
      }),
      environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      auxiliary: 0n,
      cpu: 10n,
      memory: 11n,
    };
    const post: MidgardCekMachineState = {
      ...pre,
      focusRoot: hash(1),
      continuationRoot: hash(3),
      cpu: 16_010n,
      memory: 111n,
    };
    const witness = {
      kind: "computeApplication",
      function: hash(1),
      argument: hash(2),
    } as const;

    expect(midgardCekMachineStateData(pre).index).toBe(0);
    expect(midgardCekCoreStepWitnessData(witness).index).toBe(4);
    expect(
      encodeMidgardCekCoreStepDataCbor({
        pre,
        post,
        witness,
      }).toString("hex"),
    ).toBe(
      "d8799fd8799f000258202a37aa5b923cf90c6f3c8849e8fe2b28adcda97ccd736af6bf35b8312035f43158200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7582053163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b000a0bffd8799f00025820010101010101010101010101010101010101010101010101010101010101010158200b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a75820030303030303030303030303030303030303030303030303030303030303030300193e8a186fffd87d9f5820010101010101010101010101010101010101010101010101010101010101010158200202020202020202020202020202020202020202020202020202020202020202ffff",
    );
  });

  it("pins the dedicated BLS-final and failure constructor indices", () => {
    const constant = {
      kind: "constant",
      witness: {
        typeCbor: Buffer.from("9f04ff", "hex"),
        payloadCbor: Buffer.from("d87a80", "hex"),
      },
    } as const;
    const leaf = {
      kind: "millerLoop",
      g1: {
        typeCbor: Buffer.from("9f09ff", "hex"),
        payloadCbor: Buffer.from("40", "hex"),
      },
      g2: {
        typeCbor: Buffer.from("9f0aff", "hex"),
        payloadCbor: Buffer.from("40", "hex"),
      },
    } as const;
    expect(
      midgardCekCoreStepWitnessData({
        kind: "executeBuiltinBlsFinal",
        leftRoot: hash(1),
        rightRoot: hash(2),
        leftExpression: leaf,
        rightExpression: leaf,
        result: constant,
      }).index,
    ).toBe(37);
    expect(
      midgardCekCoreStepWitnessData({
        kind: "executeBuiltinFailure",
        tag: 4n,
        arguments: [constant],
      }).index,
    ).toBe(38);
    expect(
      midgardCekCoreStepWitnessData({
        kind: "executeBuiltinTypeFailure",
        tag: 0n,
        arguments: [constant],
      }).index,
    ).toBe(39);
  });
});
