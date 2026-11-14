/**
 * The trace walk compares each step's outcome with replay, both ways and
 * coordinates included: a producer that stops where replay went on, or goes
 * on through the phase replay rejected in, is a disagreement. The producers
 * here are scripted, so each test pins one comparison of the driver.
 */
import {
  deriveMidgardValidationContextCborV1,
  type MidgardValidationPhaseName,
} from "@al-ft/midgard-core";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type { MidgardRejectionReasonV1 } from "@al-ft/midgard-core/validation-trace";
import { Effect, Either } from "effect";
import { describe, expect, it } from "vitest";

import { RejectCodes, type RejectedTx } from "../src/types.js";
import { classifyValidationTraceFailure } from "../src/validation-machine/trace-builder.js";
import { ValidationTraceStopped } from "../src/validation-machine/trace-builder-stop.js";
import type { ValidationReplay } from "../src/validation-machine/trace-replay.js";
import { RetainingValidationTraceSink } from "../src/validation-machine/trace-sink.js";
import {
  openTraceWalk,
  phaseContinues,
  type PhaseOutcome,
  phaseStops,
  TRACE_WALK_STEPS,
  type TraceWalkStep,
  ValidationTraceDisagreement,
} from "../src/validation-machine/trace-walk.js";
import type { ValidationMachineReplayInput } from "../src/validation-machine/types.js";
import { makeNativeTx } from "./validation-fixtures.js";
import { context } from "./validation-machine.validate-boundary-abi-and-collect-auxiliary-kinds.js";

const transaction = makeNativeTx({ version: 1n });

const input: ValidationMachineReplayInput = {
  l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
  ...context,
  transactionId: transaction.txId,
  canonicalTransactionCbor: transaction.txCbor,
  priorUtxosRoot: "03".repeat(32),
  postUtxosRoot: "03".repeat(32),
  ledgerWitnessEntries: [],
  expectedLedgerOps: [],
  expectedVerdict: "accepted",
  expectedRejectionCode: null,
};

const contextHeader = {
  endTimeMs: BigInt(context.blockTimeMs),
  blockSlot: context.blockSlot,
  expectedNetworkId: context.expectedNetworkId,
  minFeeA: context.minFeeA,
  minFeeB: context.minFeeB,
};

type ReplayRejection = Pick<RejectedTx, "code" | "subject">;

const replayOf = (
  rejection: (ReplayRejection & { readonly phase: string }) | null,
): ValidationReplay => ({
  rejection:
    rejection === null
      ? null
      : {
          txId: transaction.txId,
          code: rejection.code,
          detail: null,
          consensusPhase: rejection.phase as MidgardValidationPhaseName,
          ...(rejection.subject === undefined
            ? {}
            : { subject: rejection.subject }),
        },
  ledgerOps: [],
  contextHeader,
  priorLedgerRoot: Buffer.alloc(32, 0x03),
  transaction: {
    input,
    contextCbor: deriveMidgardValidationContextCborV1(contextHeader),
    phaseALedgerTx: null,
    scriptWitnesses: [],
    ledgerState: new Map(),
    ledgerDescriptorState: new Map(),
    scriptEvaluations: [],
  },
});

/**
 * Walks every step with `produce`'s outcome, then finishes. Returns the
 * completed walk or its failure, and the steps whose producer ran.
 */
const walkWith = async (
  replay: ValidationReplay,
  produce: (step: TraceWalkStep) => PhaseOutcome,
) => {
  const ran: TraceWalkStep[] = [];
  let sink: RetainingValidationTraceSink | undefined;
  const result = await Effect.runPromise(
    Effect.either(
      Effect.gen(function* () {
        const { walk, finish } = yield* openTraceWalk(input, replay, {
          mode: "proof",
          open: (frame) => (sink = new RetainingValidationTraceSink(frame)),
        });
        for (const { step } of TRACE_WALK_STEPS) {
          yield* walk.phase(step, () =>
            Effect.sync(() => {
              ran.push(step);
              return produce(step);
            }),
          );
        }
        return yield* finish();
      }),
    ),
  );
  return { result, ran, sink: sink! };
};

const disagreement = (
  result: Either.Either<unknown, Error>,
): ValidationTraceDisagreement => {
  if (Either.isRight(result)) throw new Error("the walk did not disagree");
  expect(result.left).toBeInstanceOf(ValidationTraceDisagreement);
  return result.left as ValidationTraceDisagreement;
};

const stoppedReason = (
  result: Either.Either<
    { readonly rejectionReason: MidgardRejectionReasonV1 | null },
    Error
  >,
): MidgardRejectionReasonV1 | null => {
  if (Either.isLeft(result)) throw result.left;
  return result.right.rejectionReason;
};

/**
 * Per phase: a reason a producer stops on there, and the replay rejection
 * that agrees with it. `continued` is what the walk does when replay rejects
 * there and the producer finds nothing: a disagreement, or a stop on
 * replay's reason where the phase's last step is steered by that arm.
 */
const PHASES: readonly {
  readonly phase: MidgardValidationPhaseName;
  readonly stop: MidgardRejectionReasonV1;
  readonly rejection: ReplayRejection;
  readonly continued: "disagreement" | "steered";
}[] = [
  {
    phase: "canonicalDecode",
    stop: { FieldItemWidthIllegal: { field_index: 2n, item_index: 1n } },
    rejection: {
      code: RejectCodes.InvalidFieldType,
      subject: { arm: "FieldItemWidthIllegal", fieldIndex: 2n, itemIndex: 1n },
    },
    continued: "disagreement",
  },
  {
    phase: "compactBinding",
    stop: "NetworkIdMismatch",
    rejection: { code: RejectCodes.NetworkIdMismatch },
    continued: "steered",
  },
  {
    phase: "staticLedgerRules",
    stop: "FeeBelowMinimum",
    rejection: { code: RejectCodes.MinFee },
    continued: "steered",
  },
  {
    phase: "inputSets",
    stop: "EmptyInputs",
    rejection: { code: RejectCodes.EmptyInputs },
    continued: "disagreement",
  },
  {
    phase: "signatures",
    stop: { RequiredSignerUnsigned: { signer_index: 1n } },
    rejection: {
      code: RejectCodes.MissingRequiredWitness,
      subject: { arm: "RequiredSignerUnsigned", index: 1n },
    },
    continued: "disagreement",
  },
  {
    phase: "phaseANativeScripts",
    stop: { WitnessNativeScriptFalse: { script_index: 0n } },
    rejection: {
      code: RejectCodes.NativeScriptInvalid,
      subject: { arm: "WitnessNativeScriptFalse", index: 0n },
    },
    continued: "disagreement",
  },
  {
    phase: "phaseAScriptPreconditions",
    stop: { ObserverOrderInvalid: { observer_index: 1n } },
    rejection: {
      code: RejectCodes.InvalidFieldType,
      subject: { arm: "ObserverOrderInvalid", index: 1n },
    },
    continued: "steered",
  },
  {
    phase: "resolveInputs",
    stop: { InputNotFound: { source_kind: 0n, input_index: -1n } },
    rejection: {
      code: RejectCodes.InputNotFound,
      subject: { arm: "InputNotFound", sourceKind: 0n, index: 0n },
    },
    continued: "disagreement",
  },
  {
    phase: "scriptSources",
    stop: { RedeemerMalformed: { redeemer_index: 0n } },
    rejection: {
      code: RejectCodes.InvalidFieldType,
      subject: { arm: "RedeemerMalformed", index: 0n },
    },
    continued: "disagreement",
  },
  {
    phase: "nativeScripts",
    stop: {
      ExecutionNativeScriptFalse: { purpose_kind: 0n, purpose_index: 0n },
    },
    rejection: {
      code: RejectCodes.NativeScriptInvalid,
      subject: {
        arm: "ExecutionNativeScriptFalse",
        purposeKind: 0n,
        purposeIndex: 0n,
      },
    },
    continued: "disagreement",
  },
  {
    phase: "scriptIntegrity",
    stop: "ScriptIntegrityHashMismatch",
    rejection: {
      code: RejectCodes.InvalidFieldType,
      subject: { arm: "ScriptIntegrityHashMismatch" },
    },
    continued: "steered",
  },
  {
    phase: "cek",
    stop: { PlutusExecutionFailed: { purpose_kind: 1n, purpose_index: 0n } },
    rejection: {
      code: RejectCodes.PlutusScriptInvalid,
      subject: {
        arm: "PlutusExecutionFailed",
        purposeKind: 1n,
        purposeIndex: 0n,
      },
    },
    continued: "disagreement",
  },
  {
    phase: "valueAndMint",
    stop: "ValueNotPreserved",
    rejection: { code: RejectCodes.ValueNotPreserved },
    continued: "disagreement",
  },
];

/** A phase's steps, in walk order. */
const stepsOf = (phase: MidgardValidationPhaseName): TraceWalkStep[] =>
  TRACE_WALK_STEPS.filter((entry) => entry.phase === phase).map(
    (entry) => entry.step,
  );

describe("trace walk driver", () => {
  it("walks every phase, in order, of a phase table", () => {
    expect(PHASES.flatMap(({ phase }) => stepsOf(phase))).toEqual(
      TRACE_WALK_STEPS.map((entry) => entry.step),
    );
  });

  it("accepts a walk that goes on wherever replay accepted", async () => {
    const { result, ran, sink } = await walkWith(
      replayOf(null),
      () => phaseContinues,
    );
    expect(Either.isRight(result)).toBe(true);
    if (Either.isRight(result)) {
      expect(result.right.verdict).toBe("accepted");
      expect(result.right.rejectionReason).toBeNull();
    }
    expect(ran).toHaveLength(TRACE_WALK_STEPS.length);
    expect(sink.witnesses.map((witness) => witness.phase)).toEqual([
      "terminal",
    ]);
  });

  describe.each(PHASES)("$phase", ({ phase, stop, rejection, continued }) => {
    it.each(stepsOf(phase))(
      "disagrees when the walk stops at %s but replay accepted",
      async (step) => {
        const { result } = await walkWith(replayOf(null), (current) =>
          current === step
            ? phaseStops(stop, "scripted fault")
            : phaseContinues,
        );
        expect(disagreement(result).message).toBe(
          `scripted fault: the walk stops on ${typeof stop === "string" ? stop : Object.keys(stop)[0]} in ${phase} but replay accepted`,
        );
      },
    );

    it(
      continued === "disagreement"
        ? "disagrees when replay rejects there but the walk goes on"
        : "stops on replay's reason where replay steers the step",
      async () => {
        const { result, ran } = await walkWith(
          replayOf({ ...rejection, phase }),
          () => phaseContinues,
        );
        const last = stepsOf(phase).at(-1)!;
        expect(ran.at(-1)).toBe(last);
        if (continued === "disagreement") {
          expect(disagreement(result).message).toBe(
            `replay rejected at ${phase}/${rejection.code} but the walk found no fault in ${phase}`,
          );
        } else {
          expect(stoppedReason(result)).toEqual(stop);
        }
      },
    );

    it("ends on the producer's reason where replay agrees", async () => {
      const last = stepsOf(phase).at(-1)!;
      const { result, ran, sink } = await walkWith(
        replayOf({ ...rejection, phase }),
        (step) =>
          step === last ? phaseStops(stop, "scripted fault") : phaseContinues,
      );
      expect(stoppedReason(result)).toEqual(stop);
      expect(ran.at(-1)).toBe(last);
      expect(sink.witnesses.at(-1)?.phase).toBe("terminal");
    });
  });

  it("disagrees when the walk stops on replay's arm with other coordinates", async () => {
    const { result } = await walkWith(
      replayOf({
        code: RejectCodes.MissingRequiredWitness,
        subject: { arm: "RequiredSignerUnsigned", index: 1n },
        phase: "signatures",
      }),
      (step) =>
        step === "signatures"
          ? phaseStops(
              { RequiredSignerUnsigned: { signer_index: 0n } },
              "scripted fault",
            )
          : phaseContinues,
    );
    expect(disagreement(result).message).toBe(
      "scripted fault: the walk stops on RequiredSignerUnsigned but replay recorded RequiredSignerUnsigned with different coordinates",
    );
  });

  it("disagrees when the walk stops on another arm of replay's code", async () => {
    const { result } = await walkWith(
      replayOf({
        code: RejectCodes.InvalidFieldType,
        subject: { arm: "RedeemerMalformed", index: 0n },
        phase: "scriptSources",
      }),
      (step) =>
        step === "scriptSources"
          ? phaseStops(
              { UnusedRedeemer: { redeemer_index: 0n } },
              "scripted fault",
            )
          : phaseContinues,
    );
    expect(disagreement(result).message).toBe(
      "scripted fault: the walk stops on UnusedRedeemer but replay recorded RedeemerMalformed with different coordinates",
    );
  });

  it("disagrees when the walk stops before the phase replay rejected in", async () => {
    const { result } = await walkWith(
      replayOf({
        code: RejectCodes.MissingRequiredWitness,
        subject: {
          arm: "ScriptSourceMissing",
          purposeKind: 0n,
          purposeIndex: 0n,
        },
        phase: "scriptSources",
      }),
      (step) =>
        step === "signatures"
          ? phaseStops(
              { RequiredSignerUnsigned: { signer_index: 0n } },
              "scripted fault",
            )
          : phaseContinues,
    );
    expect(disagreement(result).message).toBe(
      `scripted fault: the walk stops on RequiredSignerUnsigned in signatures but replay rejected at scriptSources/${RejectCodes.MissingRequiredWitness}`,
    );
  });

  it("disagrees when replay rejects in a steered phase on an arm the phase does not steer", async () => {
    const { result } = await walkWith(
      replayOf({
        code: RejectCodes.InvalidFieldType,
        subject: { arm: "RedeemerMalformed", index: 0n },
        phase: "scriptIntegrity",
      }),
      () => phaseContinues,
    );
    expect(disagreement(result).message).toBe(
      `replay rejected at scriptIntegrity/${RejectCodes.InvalidFieldType} but the walk found no fault in scriptIntegrity`,
    );
  });

  it.each([
    {
      arm: "ScriptIntegrityHashMissing",
      step: "phaseAScriptPreconditions",
      rejection: {
        code: RejectCodes.InvalidFieldType,
        subject: { arm: "ScriptIntegrityHashMissing" },
      },
    },
    {
      arm: "ObserversForbiddenOnUntaggedNetwork",
      step: "phaseAScriptPreconditions",
      rejection: {
        code: RejectCodes.InvalidFieldType,
        subject: { arm: "ObserversForbiddenOnUntaggedNetwork" },
      },
    },
    {
      arm: "ValidityIntervalExcludesBlockSlot",
      step: "validityInterval",
      rejection: { code: RejectCodes.ValidityIntervalMismatch },
    },
    {
      arm: "ValidityIntervalMalformed",
      step: "inputSets",
      rejection: { code: RejectCodes.InvalidValidityIntervalFormat },
    },
  ] as const)(
    "stops at $step on replay's $arm, which that step steers",
    async ({ arm, step, rejection }) => {
      const { phase } = TRACE_WALK_STEPS.find((entry) => entry.step === step)!;
      const { result, ran } = await walkWith(
        replayOf({ ...rejection, phase }),
        () => phaseContinues,
      );
      expect(stoppedReason(result)).toBe(arm);
      expect(ran.at(-1)).toBe(step);
    },
  );

  it("refuses a step out of order", async () => {
    const result = await Effect.runPromise(
      Effect.either(
        Effect.gen(function* () {
          const { walk } = yield* openTraceWalk(input, replayOf(null), {
            mode: "proof",
            open: (frame) => new RetainingValidationTraceSink(frame),
          });
          yield* walk.phase("compactBinding", () =>
            Effect.succeed(phaseContinues),
          );
        }),
      ),
    );
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isLeft(result))
      expect(result.left.message).toBe(
        "trace walk step compactBinding is out of order: the next step is canonicalDecode",
      );
  });

  it("classifies a disagreement as a disagreement", async () => {
    const result = await Effect.runPromise(
      Effect.either(
        Effect.fail(new ValidationTraceDisagreement("scripted")).pipe(
          classifyValidationTraceFailure(input),
        ),
      ),
    );
    expect(Either.isLeft(result)).toBe(true);
    if (Either.isRight(result)) return;
    expect(result.left).toBeInstanceOf(ValidationTraceStopped);
    expect((result.left as ValidationTraceStopped).reason).toBe("disagreement");
    expect(result.left.message).toBe("scripted");
  });
});
