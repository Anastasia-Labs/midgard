import {
  encodeCbor,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
} from "@al-ft/midgard-core";
import { Effect, Either } from "effect";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { runScriptExecution } from "../src/phase-b.script-run.js";
import { MidgardUplcCapabilityError } from "../src/plutus-script-evaluator.capability-error.js";
import {
  evaluateStoredMidgardPlutusScript,
  type MidgardUplcEvalProgramResult,
} from "../src/plutus-script-evaluator.js";
const dependency = vi.hoisted(() => ({ evaluate: vi.fn() }));
vi.mock("@lucid-evolution/uplc", () => ({
  eval_program_raw: dependency.evaluate,
}));
const flat = Buffer.from("010000480081", "hex");
const evaluated = (): MidgardUplcEvalProgramResult => ({
  ok: true,
  cpu: 1n,
  mem: 1n,
  error_kind: 0,
  logs: [],
  free: vi.fn(),
});
const run = () => ({
  scriptBytes: encodeCbor(flat),
  scriptLanguage: "PlutusV3" as const,
  contextCbor: Buffer.from([0]),
  executionBudget: { cpu: 10n, memory: 10n },
  executionIndex: 0n,
  maxSteps: 10,
});
const capabilityFailure = (
  action: () => unknown,
): MidgardUplcCapabilityError => {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(MidgardUplcCapabilityError);
    if (error instanceof MidgardUplcCapabilityError) return error;
    throw error;
  }
  throw new Error("expected an infrastructure failure");
};

beforeEach(() => {
  dependency.evaluate.mockReset().mockImplementation(evaluated);
});

describe("UPLC dependency failure boundary", () => {
  it.each(["PlutusV3", "MidgardV1"] as const)(
    "passes exact non-minimal stored payload and real cost model for %s",
    (scriptLanguage) => {
      const request = {
        ...run(),
        scriptLanguage,
        scriptBytes: Buffer.concat([Buffer.from([0x58, flat.length]), flat]),
      };
      expect(evaluateStoredMidgardPlutusScript(request).kind).toBe("accepted");
      expect(dependency.evaluate).toHaveBeenCalledExactlyOnceWith(
        flat,
        [request.contextCbor],
        3,
        11,
        BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
        10n,
        10n,
        false,
      );
    },
  );
  it.each(["020000480081", "0101000101"])(
    "does not invoke the evaluator for decoded phase-two failure %s",
    (hex) => {
      const request = {
        ...run(),
        scriptBytes: encodeCbor(Buffer.from(hex, "hex")),
      };
      expect(evaluateStoredMidgardPlutusScript(request).kind).toBe(
        "script_invalid",
      );
      expect(dependency.evaluate).not.toHaveBeenCalled();
    },
  );

  it("preserves evaluator call and cleanup exceptions as causes", () => {
    const cause = new Error("evaluator unavailable");
    dependency.evaluate.mockImplementation(() => {
      throw cause;
    });
    expect(
      capabilityFailure(() => evaluateStoredMidgardPlutusScript(run())).cause,
    ).toBe(cause);

    dependency.evaluate.mockReturnValue({
      ...evaluated(),
      free: () => {
        throw cause;
      },
    });
    expect(
      capabilityFailure(() => evaluateStoredMidgardPlutusScript(run())).cause,
    ).toBe(cause);
  });

  it("keeps execution failure as a verdict and invalid evaluator data as infrastructure failure", () => {
    dependency.evaluate.mockReturnValue({
      ...evaluated(),
      ok: false,
      error_kind: 2,
    });
    expect(evaluateStoredMidgardPlutusScript(run())).toMatchObject({
      kind: "script_invalid",
    });
    dependency.evaluate.mockReturnValue({
      ...evaluated(),
      ok: true,
      error_kind: 2,
    });
    expect(
      capabilityFailure(() => evaluateStoredMidgardPlutusScript(run())).cause,
    ).toBeInstanceOf(TypeError);
  });

  it("reports the local evaluator's failure as no verdict, never a defect", () => {
    const cause = new Error("evaluator unavailable");
    dependency.evaluate.mockImplementation(() => {
      throw cause;
    });
    const result = Effect.runSync(Effect.either(runScriptExecution(run(), {})));
    expect(Either.isRight(result)).toBe(true);
    if (Either.isRight(result)) {
      expect(result.right.result.kind).toBe("indeterminate");
      const failure =
        result.right.result.kind === "indeterminate"
          ? result.right.result.cause
          : undefined;
      expect(failure).toBeInstanceOf(MidgardUplcCapabilityError);
      expect((failure as MidgardUplcCapabilityError).cause).toBe(cause);
    }
  });
});
