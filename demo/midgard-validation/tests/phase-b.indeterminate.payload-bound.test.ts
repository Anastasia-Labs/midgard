import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  type PhaseBConfig,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import { type ProgramSpend } from "./phase-b.indeterminate.fixture.js";
import { payloadBoundRows } from "./phase-b.payload-bound.fixture.js";

/**
 * Builtin steps past the proof evaluator's 9,215-byte bounds. Cardano
 * accepts each script past a bound, and so does verdict mode, which runs the
 * PV11 evaluator and has no such bound: such a script is accepted, and one
 * that then fails on its own is rejected. Proof mode proves a byte-string or
 * string builtin past the bound on the staged route, and serialiseData past
 * it on the serialise loop, with the same verdicts. One byte under each
 * bound, both modes return the script's own verdict.
 */

const config: PhaseBConfig = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
  enforceScriptBudget: true,
};

const MODES = ["verdict", "proof"] as const;
type Mode = (typeof MODES)[number];

const runPhaseB = (spend: ProgramSpend, mode: Mode) =>
  Effect.runPromise(
    Effect.either(
      runPhaseBValidationWithPatch([spend.candidate], spend.preState, {
        ...config,
        cekExecutionMode: mode,
      }),
    ),
  );

const expectAccepted = async (spend: ProgramSpend, mode: Mode) => {
  const result = await runPhaseB(spend, mode);
  expect(Either.isRight(result)).toBe(true);
  if (Either.isLeft(result)) return;
  expect(result.right.rejected).toStrictEqual([]);
  expect(result.right.accepted).toHaveLength(1);
};

const expectRejected = async (spend: ProgramSpend, mode: Mode) => {
  const result = await runPhaseB(spend, mode);
  expect(Either.isRight(result)).toBe(true);
  if (Either.isLeft(result)) return;
  expect(result.right.accepted).toStrictEqual([]);
  expect(result.right.rejected.map((rejected) => rejected.code)).toStrictEqual([
    RejectCodes.PlutusScriptInvalid,
  ]);
};

const ROWS = payloadBoundRows(0x41);

describe("Phase B: a builtin step past the proof evaluator's payload bounds", () => {
  describe("verdict mode", () => {
    it.each(ROWS)("$label is accepted, as Cardano accepts it", ({ over }) =>
      expectAccepted(over, "verdict"),
    );

    it.each(ROWS)(
      "$label, failing on its own, is rejected",
      ({ overFailing }) => expectRejected(overFailing, "verdict"),
    );
  });

  describe("proof mode", () => {
    it.each(ROWS)("$label is accepted, as Cardano accepts it", ({ over }) =>
      expectAccepted(over, "proof"),
    );

    it.each(ROWS)(
      "$label, failing on its own, is rejected",
      ({ overFailing }) => expectRejected(overFailing, "proof"),
    );
  });

  describe.each(MODES)("%s mode, one byte under the bound", (mode) => {
    it.each(ROWS)("$label, one byte smaller, is accepted", ({ under }) =>
      expectAccepted(under, mode),
    );

    it.each(ROWS)(
      "$label, one byte smaller and failing on its own, is rejected",
      ({ failing }) => expectRejected(failing, mode),
    );
  });
});
