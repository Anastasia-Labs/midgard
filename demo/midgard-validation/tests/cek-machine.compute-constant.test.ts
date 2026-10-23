import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  dataToCbor,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import { MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES } from "../src/index.js";
import {
  data,
  discard,
  lam,
  prepareMidgardCekProgram,
  runMidgardCekProgram,
} from "./helpers/cek-uplc-terms.js";

/**
 * A `computeConstant` step names the value root of the constant term in
 * focus, and the verifier accepts exactly the post state that returns that
 * root, whatever the size of the constant behind it. The program
 * `(lam ctx [(lam _ (con unit ())) (con data P)])` computes P, then its Unit
 * result, over a context constant.
 */

const CONTEXT = new DataConstr(0n, [new DataI(7n)]);

const stepsOf = (payload: Data) => {
  const { steps } = runMidgardCekProgram(
    prepareMidgardCekProgram(lam(discard(data(payload))), CONTEXT),
    "proof",
  );
  const constants = steps.filter(
    (step) => step.witness.kind === "computeConstant",
  );
  const context = steps.find(
    (step) => step.witness.kind === "computeContextConstant",
  );
  if (constants.length !== 2 || context === undefined)
    throw new Error("expected two constant steps and one context step");
  return { computed: constants[0]!, unit: constants[1]!, context };
};

const rootOf = (step: ReturnType<typeof stepsOf>["computed"]) => {
  if (
    step.witness.kind !== "computeConstant" &&
    step.witness.kind !== "computeContextConstant"
  )
    throw new Error("not a constant step");
  return step.witness.valueRoot;
};

/** A Data list of 1,200 integers of 9 CBOR bytes each, past 10,800 bytes. */
const LONG_LIST = new DataList(
  Array.from(
    { length: 1_200 },
    (_, index) => new DataI(2n ** 60n + BigInt(index)),
  ),
);

describe.each([
  ["a Data byte string of 64 bytes", new DataB(Buffer.alloc(64, 0x5a)), false],
  ["a Data list past the direct payload bound", LONG_LIST, true],
] as const)("CEK computeConstant over %s", (_, payload, pastTheBound) => {
  const { computed, unit, context } = stepsOf(payload);
  const valueRoot = rootOf(computed);
  const otherRoot = rootOf(unit);

  it("sizes the payload as named", () => {
    expect(
      Buffer.from(dataToCbor(payload)).length >
        MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
    ).toBe(pastTheBound);
  });

  it("is accepted as the executor records it", () => {
    expect(
      verifyMidgardCekCoreStep(computed.pre, computed.post, computed.witness),
    ).toBe(true);
  });

  it("is refused when it names another constant's root", () => {
    expect(
      verifyMidgardCekCoreStep(computed.pre, computed.post, {
        kind: "computeConstant",
        valueRoot: otherRoot,
      }),
    ).toBe(false);
  });

  it("is refused when it names another constant's root and returns it", () => {
    expect(
      verifyMidgardCekCoreStep(
        computed.pre,
        { ...computed.post, focusRoot: otherRoot },
        { kind: "computeConstant", valueRoot: otherRoot },
      ),
    ).toBe(false);
  });

  it("is refused when it returns another constant's root", () => {
    expect(
      verifyMidgardCekCoreStep(
        computed.pre,
        { ...computed.post, focusRoot: otherRoot },
        { kind: "computeConstant", valueRoot },
      ),
    ).toBe(false);
  });

  it("is refused over a context constant term", () => {
    expect(
      verifyMidgardCekCoreStep(context.pre, context.post, {
        kind: "computeConstant",
        valueRoot: rootOf(context),
      }),
    ).toBe(false);
  });
});
