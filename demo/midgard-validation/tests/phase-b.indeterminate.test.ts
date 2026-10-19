import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { DataI } from "@harmoniclabs/plutus-data";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { encodeMidgardCekPlutusData } from "../src/cek-constant.js";
import {
  commitValidationTraceV1,
  evaluateMidgardCekVerdict,
  type MidgardCekVerdictRequest,
  type PhaseBConfig,
  RejectCodes,
  replayValidationVerdictEvent,
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
  doublingProgram,
  failingProgram,
  type PlutusProgram,
  plutusProgramSpend,
  type ProgramSpend,
} from "./phase-b.indeterminate.fixture.js";

/**
 * E8: a limit or fault of the evaluator itself is INDETERMINATE (no
 * verdict), never a rejection. Phase B fails with a
 * `ScriptEvaluationIndeterminateError` naming the transaction, and the trace
 * builder stops as unavailable. A script that fails on its own is still
 * rejected. The verdict evaluator runs Cardano's machine under the ledger
 * budget, so the proof evaluator's step and payload bounds are limits of
 * proof mode only.
 */

const config: PhaseBConfig = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  nowCardanoSlotNo: 100n,
  bucketConcurrency: 1,
  enforceScriptBudget: true,
};

const runPhaseB = (
  spend: ProgramSpend,
  overrides: Partial<PhaseBConfig> = {},
) =>
  Effect.runPromise(
    Effect.either(
      runPhaseBValidationWithPatch([spend.candidate], spend.preState, {
        ...config,
        ...overrides,
      }),
    ),
  );

const expectIndeterminate = (
  result: Awaited<ReturnType<typeof runPhaseB>>,
  spend: ProgramSpend,
) => {
  expect(Either.isLeft(result)).toBe(true);
  if (Either.isRight(result)) return undefined;
  expect(result.left).toBeInstanceOf(ScriptEvaluationIndeterminateError);
  const indeterminate = result.left as ScriptEvaluationIndeterminateError;
  expect(indeterminate.txId.equals(spend.candidate.ledgerTx.txId)).toBe(true);
  return indeterminate;
};

const expectScriptRejected = (
  result: Awaited<ReturnType<typeof runPhaseB>>,
) => {
  expect(Either.isRight(result)).toBe(true);
  if (Either.isLeft(result)) return;
  expect(result.right.accepted).toStrictEqual([]);
  expect(result.right.rejected.map((rejected) => rejected.code)).toStrictEqual([
    RejectCodes.PlutusScriptInvalid,
  ]);
};

const expectAccepted = (result: Awaited<ReturnType<typeof runPhaseB>>) => {
  expect(Either.isRight(result)).toBe(true);
  if (Either.isLeft(result)) return;
  expect(result.right.rejected).toStrictEqual([]);
  expect(result.right.accepted).toHaveLength(1);
};

describe("Phase B: an evaluator limit is indeterminate, never a rejection", () => {
  const accepting = plutusProgramSpend(0x71, acceptingProgram());
  const failing = plutusProgramSpend(0x72, failingProgram());
  const doubling = plutusProgramSpend(0x73, doublingProgram());

  it("accepts and rejects on the script's own outcome", async () => {
    expectAccepted(await runPhaseB(accepting));
    expectScriptRejected(await runPhaseB(failing));
  });

  it("decides by the ledger budget, not the proof evaluator's step bound", async () => {
    expectAccepted(await runPhaseB(accepting, { maxScriptExecutionSteps: 2 }));
  });

  it("is indeterminate when a proof-mode run reaches the step bound", async () => {
    expectIndeterminate(
      await runPhaseB(accepting, {
        maxScriptExecutionSteps: 2,
        cekExecutionMode: "proof",
      }),
      accepting,
    );
  });

  it("still rejects a script that fails within the step bound", async () => {
    expectScriptRejected(
      await runPhaseB(failing, { maxScriptExecutionSteps: 1_000 }),
    );
  });

  it("accepts a program Cardano accepts past the proof evaluator's payload bound", async () => {
    expectAccepted(await runPhaseB(doubling));
  });

  it("is indeterminate when a proof-mode builtin passes the payload bound", async () => {
    expectIndeterminate(
      await runPhaseB(doubling, { cekExecutionMode: "proof" }),
      doubling,
    );
    expectScriptRejected(
      await runPhaseB(failing, { cekExecutionMode: "proof" }),
    );
  });

  it("is indeterminate when another evaluator reports no verdict, keeping its cause", async () => {
    const cause = new RangeError("Array buffer allocation failed");
    const indeterminate = expectIndeterminate(
      await runPhaseB(accepting, {
        evaluateCekVerdict: () =>
          Effect.succeed({ kind: "indeterminate", detail: "fault", cause }),
      }),
      accepting,
    );
    expect(indeterminate?.cause).toBe(cause);
  });
});

const verdictRequest = (
  program: PlutusProgram,
  overrides: Partial<MidgardCekVerdictRequest> = {},
): MidgardCekVerdictRequest => ({
  scriptBytes: program.scriptBytes,
  scriptLanguage: "PlutusV3",
  contextCbor: encodeMidgardCekPlutusData(new DataI(42n)),
  executionIndex: 0n,
  maxSteps: 1_000,
  ...overrides,
});

describe("verdict evaluator: no verdict on bytes it cannot read", () => {
  it("returns the script's own verdict on well-formed input", () => {
    expect(
      evaluateMidgardCekVerdict(verdictRequest(acceptingProgram())).kind,
    ).toBe("accepted");
    expect(
      evaluateMidgardCekVerdict(verdictRequest(failingProgram())).kind,
    ).toBe("script_invalid");
  });

  it("is indeterminate on carried script bytes it cannot decode", () => {
    expect(
      evaluateMidgardCekVerdict(
        verdictRequest(acceptingProgram(), {
          scriptBytes: Uint8Array.of(0xde, 0xad),
        }),
      ).kind,
    ).toBe("indeterminate");
  });

  it("decides by the ledger budget, not the step bound", () => {
    expect(
      evaluateMidgardCekVerdict(
        verdictRequest(acceptingProgram(), { maxSteps: 2 }),
      ).kind,
    ).toBe("accepted");
    expect(
      evaluateMidgardCekVerdict(
        verdictRequest(failingProgram(), { maxSteps: 1_000 }),
      ).kind,
    ).toBe("script_invalid");
  });
});

describe("trace builder: a claimed rejection is never built from an evaluator limit", () => {
  const commit = (input: Parameters<typeof commitValidationTraceV1>[0]) =>
    Effect.runPromise(Effect.either(commitValidationTraceV1(input)));

  it(
    "builds a rejected trace for a script that fails on its own",
    { timeout: 60_000 },
    async () => {
      const input = claimedRejected(
        await plutusV3SpendInput(failingProgram(), MIDGARD_TX_EX_UNITS_CAP),
      );
      const result = await commit(input);
      expect(Either.isRight(result)).toBe(true);
      if (Either.isLeft(result)) return;
      expect(result.right.descriptor.verdict).toBe("rejected");
    },
  );

  it(
    "refuses a claimed rejection of a program Cardano accepts past the payload bound",
    { timeout: 60_000 },
    async () => {
      const input = claimedRejected(
        await plutusV3SpendInput(doublingProgram(), MIDGARD_TX_EX_UNITS_CAP),
      );
      const result = await commit(input);
      expect(Either.isLeft(result)).toBe(true);
      if (Either.isRight(result)) return;
      expect(result.left).toBeInstanceOf(ValidationTraceStopped);
      expect((result.left as ValidationTraceStopped).reason).toBe(
        "disagreement",
      );
    },
  );
});

describe("verdict event replay: the verdict is Cardano's", () => {
  const replay = async (program: PlutusProgram) =>
    Effect.runPromise(
      Effect.either(
        replayValidationVerdictEvent(
          await plutusV3SpendInput(program, MIDGARD_TX_EX_UNITS_CAP),
        ),
      ),
    );

  it("replays the script's own verdicts", async () => {
    const accepted = await replay(acceptingProgram());
    expect(Either.isRight(accepted)).toBe(true);
    if (Either.isRight(accepted)) {
      expect(accepted.right.verdict).toBe("accepted");
    }
    const rejected = await replay(failingProgram());
    expect(Either.isRight(rejected)).toBe(true);
    if (Either.isRight(rejected)) {
      expect(rejected.right.verdict).toBe("rejected");
      expect(rejected.right.rejection?.code).toBe(
        RejectCodes.PlutusScriptInvalid,
      );
    }
  });

  it("replays Cardano's verdict on a program past the proof evaluator's payload bound", async () => {
    const result = await replay(doublingProgram());
    expect(Either.isRight(result)).toBe(true);
    if (Either.isRight(result)) expect(result.right.verdict).toBe("accepted");
  });
});
