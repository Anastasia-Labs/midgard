/** The validation-trace dispute runner at a staged byteStringToInteger step
 * (arm 31, semantics group 11) over a 16,384-byte byte string built at run
 * time, proved by the staged conversion stage, from Init to the award and
 * the spend back of the staging outputs
 * (support/validation-trace-dispute-one-step-loop-case.ts). The thread at
 * the staged conversion stage is found as ours by the stage's address. */
import { describe, expect, it } from "vitest";

import { validationTraceOneStepHolderAt } from "../src/validation-trace-dispute/one-step-holders.js";
import { CONVERSION_SCRIPT } from "./support/validation-trace-dispute-loop-scripts.js";
import { loopCase } from "./support/validation-trace-dispute-one-step-loop-case.js";

describe("validation-trace dispute runner: a staged byteStringToInteger step proved by the staged conversion stage", () =>
  loopCase({
    hex: CONVERSION_SCRIPT,
    group: 11,
    hops: 1,
    stagedBytes: [16_384],
    more: (drive) =>
      it("finds the deployed staged conversion stage as a holder of the one-step resolution", () => {
        const { scene, deployment } = drive.dispute();
        const { spendingScriptAddress, spendingScriptHash } =
          scene.chain.cekCoreStages.stagedConversion;
        const holder = validationTraceOneStepHolderAt(
          deployment,
          spendingScriptAddress,
        );
        expect(holder?.holder).toBe("stagedConversion");
        expect(holder?.scriptHash).toBe(spendingScriptHash);
        expect(holder?.reference?.scriptRef).toBeDefined();
      }),
  }));
