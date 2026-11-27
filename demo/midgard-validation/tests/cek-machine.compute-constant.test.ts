import { describe, expect, it } from "vitest";

import { verifyMidgardCekCoreStep } from "../src/cek-machine.js";
import {
  encodeMidgardCekCanonicalConstant,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
} from "../src/index.js";
import { UPLCConst } from "../src/uplc-constant.js";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
} from "../src/uplc-data.js";
import {
  discard,
  lam,
  prepareMidgardCekProgram,
  runMidgardCekProgram,
} from "./helpers/cek-uplc-terms.js";

/**
 * A `computeConstant` step names the value root of the constant term in
 * focus, and the verifier accepts exactly the post state that returns that
 * root, whatever the size of the constant behind it, a single byte-string or
 * integer leaf past the direct payload bound included. The program
 * `(lam ctx [(lam _ (con unit ())) C])` computes C, then its Unit result,
 * over a context constant.
 */

const CONTEXT = new DataConstr(0n, [new DataI(7n)]);

const stepsOf = (constant: UPLCConst) => {
  const { steps } = runMidgardCekProgram(
    prepareMidgardCekProgram(lam(discard(constant)), CONTEXT),
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

const LEAF_BYTES = 12_000;
/** An integer of 10,000 bytes, whose canonical CBOR is past the bound. */
const longInteger = (low: bigint): bigint => 2n ** (8n * 10_000n) + low;
const dataConstant = (payload: Data): UPLCConst => UPLCConst.data(payload);

describe.each([
  [
    "a Data byte string of 64 bytes",
    dataConstant(new DataB(Buffer.alloc(64, 0x5a))),
    dataConstant(new DataB(Buffer.alloc(64, 0x5b))),
    false,
  ],
  [
    "a Data list past the direct payload bound",
    dataConstant(LONG_LIST),
    dataConstant(new DataList([...LONG_LIST.list, new DataI(1n)])),
    true,
  ],
  [
    "a Data byte string of 12,000 bytes",
    dataConstant(new DataB(Buffer.alloc(LEAF_BYTES, 0x5a))),
    dataConstant(new DataB(Buffer.alloc(LEAF_BYTES, 0x5b))),
    true,
  ],
  [
    "a Data integer past the direct payload bound",
    dataConstant(new DataI(longInteger(5n))),
    dataConstant(new DataI(longInteger(6n))),
    true,
  ],
  [
    "a byte string of 12,000 bytes",
    UPLCConst.byteString(new DataB(Buffer.alloc(LEAF_BYTES, 0x5a)).bytes),
    UPLCConst.byteString(new DataB(Buffer.alloc(LEAF_BYTES, 0x5b)).bytes),
    true,
  ],
  [
    "an integer past the direct payload bound",
    UPLCConst.int(longInteger(5n)),
    UPLCConst.int(longInteger(6n)),
    true,
  ],
  [
    "a string of 12,000 bytes",
    UPLCConst.str("a".repeat(LEAF_BYTES)),
    UPLCConst.str("b".repeat(LEAF_BYTES)),
    true,
  ],
] as const)(
  "CEK computeConstant over %s",
  (_, constant, other, pastTheBound) => {
    // Built on first use, so a constant that does not build fails each row.
    const build = () => {
      const steps = stepsOf(constant);
      return {
        ...steps,
        valueRoot: rootOf(steps.computed),
        otherRoot: rootOf(steps.unit),
      };
    };
    let built: ReturnType<typeof build> | undefined;
    const run = () => (built ??= build());

    it("sizes the payload as named", () => {
      expect(
        encodeMidgardCekCanonicalConstant(constant).payloadCbor.length >
          MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
      ).toBe(pastTheBound);
    });

    it("is accepted as the executor records it", () => {
      const { computed } = run();
      expect(
        verifyMidgardCekCoreStep(computed.pre, computed.post, computed.witness),
      ).toBe(true);
    });

    it("is refused when it names another constant's root", () => {
      const { computed, otherRoot } = run();
      expect(
        verifyMidgardCekCoreStep(computed.pre, computed.post, {
          kind: "computeConstant",
          valueRoot: otherRoot,
        }),
      ).toBe(false);
    });

    it("is refused when it names the root of another constant of its size", () => {
      const { computed, valueRoot } = run();
      const sibling = rootOf(stepsOf(other).computed);
      expect(Buffer.from(sibling).equals(Buffer.from(valueRoot))).toBe(false);
      expect(
        verifyMidgardCekCoreStep(computed.pre, computed.post, {
          kind: "computeConstant",
          valueRoot: sibling,
        }),
      ).toBe(false);
    });

    it("is refused when it names another constant's root and returns it", () => {
      const { computed, otherRoot } = run();
      expect(
        verifyMidgardCekCoreStep(
          computed.pre,
          { ...computed.post, focusRoot: otherRoot },
          { kind: "computeConstant", valueRoot: otherRoot },
        ),
      ).toBe(false);
    });

    it("is refused when it returns another constant's root", () => {
      const { computed, valueRoot, otherRoot } = run();
      expect(
        verifyMidgardCekCoreStep(
          computed.pre,
          { ...computed.post, focusRoot: otherRoot },
          { kind: "computeConstant", valueRoot },
        ),
      ).toBe(false);
    });

    it("is refused over a context constant term", () => {
      const { context } = run();
      expect(
        verifyMidgardCekCoreStep(context.pre, context.post, {
          kind: "computeConstant",
          valueRoot: rootOf(context),
        }),
      ).toBe(false);
    });
  },
);
