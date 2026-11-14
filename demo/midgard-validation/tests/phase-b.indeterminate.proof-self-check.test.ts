import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { Effect, Either } from "effect";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  buildDeterministicValidationMachineTrace,
  type PhaseBConfig,
  RejectCodes,
  runPhaseBValidationWithPatch,
  ScriptEvaluationIndeterminateError,
  ValidationTraceStopped,
} from "../src/index.js";
import {
  MIDGARD_TX_EX_UNITS_CAP,
  plutusV3SpendInput,
} from "./helpers/plutus-spend-input.js";
import {
  acceptingProgram,
  claimedRejected,
  failingProgram,
  plutusProgramSpend,
  type ProgramSpend,
} from "./phase-b.indeterminate.fixture.js";

/**
 * E8 fault injection: a proof-mode run checks every transition it generates
 * against the consensus core-step verifier. When that check fails (injected
 * here), the run has no verdict: Phase B fails with a
 * `ScriptEvaluationIndeterminateError`, never a script rejection, and a
 * proof-mode trace build stops as unavailable.
 */
const fault = vi.hoisted(() => ({ injected: false }));

vi.mock("../src/cek-machine.js", async (importOriginal) => {
  const original =
    await importOriginal<typeof import("../src/cek-machine.js")>();
  return {
    ...original,
    verifyMidgardCekCoreStep: (
      ...args: Parameters<typeof original.verifyMidgardCekCoreStep>
    ) => !fault.injected && original.verifyMidgardCekCoreStep(...args),
  };
});

const config: PhaseBConfig = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
  enforceScriptBudget: true,
  cekExecutionMode: "proof",
};

const runPhaseB = (spend: ProgramSpend) =>
  Effect.runPromise(
    Effect.either(
      runPhaseBValidationWithPatch([spend.candidate], spend.preState, config),
    ),
  );

describe("proof-mode self-check fault", () => {
  const accepting = plutusProgramSpend(0x74, acceptingProgram());
  const failing = plutusProgramSpend(0x75, failingProgram());

  afterEach(() => {
    fault.injected = false;
  });

  it("keeps the script's own verdicts while the check passes", async () => {
    const accepted = await runPhaseB(accepting);
    expect(Either.isRight(accepted)).toBe(true);
    if (Either.isRight(accepted)) {
      expect(accepted.right.accepted).toHaveLength(1);
    }
    const rejected = await runPhaseB(failing);
    expect(Either.isRight(rejected)).toBe(true);
    if (Either.isRight(rejected)) {
      expect(rejected.right.rejected.map((entry) => entry.code)).toStrictEqual([
        RejectCodes.PlutusScriptInvalid,
      ]);
    }
  });

  it.each([
    ["an accepting", accepting],
    ["a failing", failing],
  ])(
    "is indeterminate for %s script when the check fails",
    async (_, spend) => {
      fault.injected = true;
      const result = await runPhaseB(spend);
      expect(Either.isLeft(result)).toBe(true);
      if (Either.isRight(result)) return;
      expect(result.left).toBeInstanceOf(ScriptEvaluationIndeterminateError);
      expect(
        (result.left as ScriptEvaluationIndeterminateError).txId.equals(
          spend.candidate.ledgerTx.txId,
        ),
      ).toBe(true);
    },
  );
});

describe("proof-mode trace build under a self-check fault", () => {
  const build = async (program: Parameters<typeof plutusV3SpendInput>[0]) =>
    Effect.runPromise(
      Effect.either(
        buildDeterministicValidationMachineTrace(
          claimedRejected(
            await plutusV3SpendInput(program, MIDGARD_TX_EX_UNITS_CAP),
          ),
        ),
      ),
    );

  afterEach(() => {
    fault.injected = false;
  });

  it(
    "builds the claimed rejection of a failing script while the check passes",
    { timeout: 60_000 },
    async () => {
      const result = await build(failingProgram());
      expect(Either.isRight(result)).toBe(true);
      if (Either.isRight(result)) expect(result.right.verdict).toBe("rejected");
    },
  );

  it(
    "stops as unavailable, never building the claimed rejection, when the check fails",
    { timeout: 60_000 },
    async () => {
      fault.injected = true;
      const result = await build(acceptingProgram());
      expect(Either.isLeft(result)).toBe(true);
      if (Either.isRight(result)) return;
      expect(result.left).toBeInstanceOf(ValidationTraceStopped);
      expect((result.left as ValidationTraceStopped).reason).toBe(
        "unavailable",
      );
    },
  );
});
