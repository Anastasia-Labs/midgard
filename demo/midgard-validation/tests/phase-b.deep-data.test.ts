import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import {
  type PhaseBConfig,
  RejectCodes,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import { evaluateStoredMidgardPlutusScript } from "../src/plutus-script-evaluator.js";
import {
  deepCanonicalData,
  deepestRedeemerDepth,
  deepRows,
  deepSpend,
  SERIALISED_REDEEMER_DEPTH,
  serialisingProgram,
  shallowSpend,
  walkingProgram,
} from "./phase-b.deep-data.fixture.js";
import { type ProgramSpend } from "./phase-b.indeterminate.fixture.js";

/**
 * Data nested thousands of levels deep, up to the deepest a transaction can
 * carry, reaches the script's own verdict in Phase B verdict mode: accepted
 * when the script accepts, rejected when it fails. Nothing throws, nothing is
 * left without a verdict, and a shallow transaction evaluated next in the same
 * process still gets its verdict.
 */

const config: PhaseBConfig = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
  enforceScriptBudget: true,
  cekExecutionMode: "verdict",
};

const runPhaseB = (spend: ProgramSpend) =>
  Effect.runPromise(
    Effect.either(
      runPhaseBValidationWithPatch([spend.candidate], spend.preState, config),
    ),
  );

const expectAccepted = async (spend: ProgramSpend) => {
  const result = await runPhaseB(spend);
  expect(
    Either.isRight(result),
    Either.isLeft(result) ? String(result.left) : "",
  ).toBe(true);
  if (Either.isLeft(result)) return;
  expect(result.right.rejected).toStrictEqual([]);
  expect(result.right.accepted).toHaveLength(1);
};

const expectRejected = async (spend: ProgramSpend) => {
  const result = await runPhaseB(spend);
  expect(
    Either.isRight(result),
    Either.isLeft(result) ? String(result.left) : "",
  ).toBe(true);
  if (Either.isLeft(result)) return;
  expect(result.right.accepted).toStrictEqual([]);
  expect(result.right.rejected.map((rejected) => rejected.code)).toStrictEqual([
    RejectCodes.PlutusScriptInvalid,
  ]);
};

describe("Phase B verdict mode on deeply nested Data", () => {
  it.each(deepRows())(
    "$label is accepted by its script, then a shallow spend still is",
    async ({ accepting }) => {
      await expectAccepted(accepting(0x31));
      await expectAccepted(shallowSpend(0x32));
    },
    60_000,
  );

  it.each(deepRows())(
    "$label is rejected by its failing script, then a shallow spend is accepted",
    async ({ failing }) => {
      await expectRejected(failing(0x33));
      await expectAccepted(shallowSpend(0x34));
    },
    60_000,
  );

  it.each(["list", "constr", "map"] as const)(
    "a shallow spend still gets its verdict after any evaluation of the deepest %s redeemer",
    async (shape) => {
      const redeemer = deepCanonicalData(shape, deepestRedeemerDepth(shape));
      // Whatever the deep evaluation returns, it must not decide the next one.
      await runPhaseB(deepSpend(0x35, walkingProgram(), { redeemer }));
      await expectAccepted(shallowSpend(0x36));
    },
    60_000,
  );
});

describe("the PV11 evaluator on serialiseData of deeply nested Data", () => {
  const depth = SERIALISED_REDEEMER_DEPTH;
  const redeemer = deepCanonicalData("list", depth);

  /**
   * Runs `program` on a script context whose redeemer field is
   * `spendRedeemer`. Serialising Data this deep costs more than a
   * transaction's step budget, so the script runs straight on the evaluator
   * Phase B uses, with a budget that covers it.
   */
  const evaluateWithRedeemer = (
    program: ReturnType<typeof serialisingProgram>,
    spendRedeemer: Buffer,
  ) =>
    evaluateStoredMidgardPlutusScript({
      scriptBytes: program.scriptBytes,
      scriptLanguage: "PlutusV3",
      contextCbor: Buffer.concat([
        Buffer.from("d8799fd87980", "hex"),
        spendRedeemer,
        Buffer.from("d87980ff", "hex"),
      ]),
      executionBudget: { cpu: 20_000_000_000n, memory: 16_500_000n },
      executionIndex: 0n,
      maxSteps: 1,
    });

  const expectShallowAccepted = () => {
    const shallow = deepCanonicalData("list", 1);
    expect(
      evaluateWithRedeemer(serialisingProgram(shallow), shallow).kind,
    ).toBe("accepted");
  };

  it(`serialises a list redeemer ${depth.toString()} deep to its own bytes, then evaluates a shallow one`, () => {
    expect(
      evaluateWithRedeemer(serialisingProgram(redeemer), redeemer).kind,
    ).toBe("accepted");
    expectShallowAccepted();
  });

  it(`fails a script expecting a list one level shallower than a redeemer ${depth.toString()} deep, then evaluates a shallow one`, () => {
    const verdict = evaluateWithRedeemer(
      serialisingProgram(deepCanonicalData("list", depth - 1)),
      redeemer,
    );
    expect(verdict.kind).toBe("script_invalid");
    if (verdict.kind === "script_invalid")
      expect(verdict.detail).toContain("execution failed (2)");
    expectShallowAccepted();
  });
});
