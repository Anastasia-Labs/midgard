import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  openValidationTraceV1,
  type ValidationMachineReplayInput,
  type ValidationTraceLeavesV1,
} from "../src/index.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { DataB } from "../src/uplc-data.js";
import { app, discard } from "./helpers/cek-uplc-terms.js";
import {
  MIDGARD_TX_EX_UNITS_CAP,
  plutusV3SpendInput,
} from "./helpers/plutus-spend-input.js";
import { builtin } from "./helpers/uplc-builtins.js";
import {
  claimedRejected,
  plutusProgram,
} from "./phase-b.indeterminate.fixture.js";

/**
 * A step a windowed trace read or the opening keeps is a step the consensus
 * verifier accepts. The script appends a byte string to a string, each payload fitting
 * one direct payload and the two together not: L1 fails it, and the verifier
 * refuses the type-failure step the commit replay generates for it (as the
 * proof-mode run refuses it, cek-executor.builtin-type-failures.test.ts), so
 * keeping that step fails the read while keeping any other step does not.
 */

const LARGE = 6_000;
const TYPE_FAILURE = "executeBuiltinTypeFailure";

describe("openValidationTraceV1 steps: kept CEK steps pass the consensus verifier", () => {
  let input: ValidationMachineReplayInput;
  let opened: ValidationTraceLeavesV1;
  let typeFailure: number;

  beforeAll(async () => {
    input = claimedRejected(
      await plutusV3SpendInput(
        plutusProgram(
          discard(
            app(
              builtin("appendByteString"),
              UPLCConst.byteString(
                new DataB(new Uint8Array(LARGE).fill(7)).bytes,
              ),
              UPLCConst.str("x".repeat(LARGE)),
            ),
          ),
        ),
        MIDGARD_TX_EX_UNITS_CAP,
      ),
    );
    opened = await Effect.runPromise(openValidationTraceV1(input));
    const failing: number[] = [];
    await Effect.runPromise(
      opened.steps((programCounter, witness) => {
        if (
          witness.auxiliary?.kind === "cekCoreStep" &&
          witness.auxiliary.step.witness.kind === TYPE_FAILURE
        )
          failing.push(programCounter);
        return false;
      }),
    );
    expect(failing).toHaveLength(1);
    typeFailure = failing[0]!;
  });

  it("refuses to keep the type-failure step the verifier rejects", async () => {
    const read = await Effect.runPromise(
      Effect.either(
        opened.steps((programCounter) => programCounter === typeFailure),
      ),
    );
    expect(read._tag).toBe("Left");
    expect(read._tag === "Left" ? read.left.message : "").toContain(
      `generated CEK ${TYPE_FAILURE} transition does not pass the consensus verifier`,
    );
  });

  it("refuses to keep it at the opening too", async () => {
    const read = await Effect.runPromise(
      Effect.either(
        openValidationTraceV1(input, {
          keepWitnesses: (programCounter) => programCounter === typeFailure,
        }),
      ),
    );
    expect(read._tag).toBe("Left");
    expect(read._tag === "Left" ? read.left.message : "").toContain(
      `generated CEK ${TYPE_FAILURE} transition does not pass the consensus verifier`,
    );
  });

  it("keeps the CEK step before it", async () => {
    const kept = await Effect.runPromise(
      opened.steps((programCounter) => programCounter === typeFailure - 1),
    );
    expect(Object.keys(kept.states)).toStrictEqual([String(typeFailure - 1)]);
    expect(kept.witnesses[typeFailure - 1]?.auxiliary?.kind).toBe(
      "cekCoreStep",
    );
  });
});
