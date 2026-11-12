/** The validation-trace dispute runner at a writeBits failure (arm 36, core
 * group 7), from Init to the award
 * (support/validation-trace-dispute-one-step-loop-case.ts); the step reads
 * no staging. Group 7's roots route each to the writeBits loop, which walks
 * the indices to the one outside the input:
 *
 * - over 200 indices, past the 64 the direct route takes, the last outside
 *   the input, in three hops;
 * - over the longest input the loop takes, 4,096 bytes (512 words), and 200
 *   indices, the 121st outside it, in two hops: the first reveals the whole
 *   input, and the last opens that index mid-list, so the hop carries the 79
 *   indices after it as its tail. Each move fits the transaction limits on
 *   the emulator, not only by the cost model the hops are planned by. */
import {
  midgardCekWriteBitsHopData,
  midgardCekWriteBitsLoopOfWitness,
  planMidgardCekWriteBitsHops,
} from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  WRITE_BITS_FAILURE_SCRIPT,
  WRITE_BITS_WIDE_FAILURE_SCRIPT,
} from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a writeBits failure proved by the writeBits loop", () =>
  loopCase({
    hex: WRITE_BITS_FAILURE_SCRIPT,
    failure: "writeBits",
    hops: 3,
    stagedBytes: [],
  }));

describe("validation-trace dispute runner: a writeBits failure over the longest input the loop takes, outside it mid-list", () =>
  loopCase({
    hex: WRITE_BITS_WIDE_FAILURE_SCRIPT,
    failure: "writeBits",
    hops: 2,
    stagedBytes: [],
    more: (drive) =>
      it("lands the last hop with the indices after the one outside the input as its tail", async () => {
        const witness = await drive.stepWitness();
        if (witness.kind !== "executeBuiltinWriteBitsFailure")
          throw new Error("the step is not a writeBits failure");
        const last = planMidgardCekWriteBitsHops(
          midgardCekWriteBitsLoopOfWitness(witness),
        ).at(-1)!;
        expect(last).toMatchObject({ end: 121, next: null });
        expect(last.hop.tail.length).toBe(79n);
        // The transaction carries the hop in its canonical, definite-length
        // encoding.
        expect(drive.cborOf("VerifyArm stagedWriteBitsLoop hop 2")).toContain(
          CML.PlutusData.from_cbor_hex(
            Data.to(midgardCekWriteBitsHopData(last.hop)),
          ).to_canonical_cbor_hex(),
        );
      }),
  }));
