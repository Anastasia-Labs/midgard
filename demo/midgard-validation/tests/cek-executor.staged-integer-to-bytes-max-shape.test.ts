import { Builtin } from "@harmoniclabs/uplc";
import { describe, it } from "vitest";

import {
  bool,
  constant,
  expectStagedFailureProved,
  integer,
  small,
} from "./helpers/cek-staged-failure-rows.js";

/**
 * integerToByteString of width 0 over the longest input byteStringToInteger
 * builds and integerToByteString then converts within the transaction budget
 * fails after its charge as the PV11 evaluator charges it. Its integers take
 * seconds to size in proof mode, so it runs in its own file.
 */

const INTEGER_TO_BYTES = 73n;

/** 44,200 bytes cost 9,990,402,019 cpu with the conversion, 44,240 bytes
 * 10,008,120,929. */
const LONGEST = 44_200;

describe("an integerToByteString step over the longest staged input", () => {
  it("is proved with the PV11 evaluator's verdict and charge", () => {
    expectStagedFailureProved(
      INTEGER_TO_BYTES,
      Builtin.integerToByteString,
      [constant(bool(false)), small(0n), integer(LONGEST)],
      "evaluator",
    );
  });
});
