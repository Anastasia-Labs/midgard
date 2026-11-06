/** The one-step resolution of a phase step and of a CEK context step, each
 * of whose evidence may read a field of the disputed transaction: inline
 * within the inline limit of the semantic resolver that opens it, less the
 * bytes its evidence carries beside the field past where that limit was
 * measured, and past it by reference, from our publications of it
 * (one-step-carriage.ts). A
 * field read by reference is named at placeholder positions until the move
 * that reads it is built: the resolution then names our publications among
 * that move's reference inputs (`rebind`), and every move from the one
 * that commits the evidence on carries the evidence so bound. */
import type { MidgardValidationMachineState } from "@al-ft/midgard-core";
import type { MidgardFieldCarriage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  buildValidationOneStepArgument,
  type ValidationMachineFieldCarriageResolver,
  type ValidationTraceSteps,
} from "@al-ft/midgard-validation";

import { commonOf } from "./one-step-argument.js";
import {
  carriedArgument,
  fixedCarriageResolver,
  type ValidationTraceCarriedField,
} from "./one-step-carriage.js";
import {
  CEK_OBSERVER_FIELD,
  cekContextRouteOf,
  planValidationTraceContextStep,
} from "./one-step-context.js";
import {
  CEK_CONTEXT_STAGE_HOLDERS,
  type ValidationTraceOneStepHolder,
} from "./one-step-holders.js";
import {
  phaseInlineMaxPreimageBytes,
  planValidationTracePhaseStep,
  type ValidationTracePhase,
} from "./one-step-phase.js";
import {
  inlineMaxPreimageBytesBeside,
  measuredAt,
  stepIndexBytes,
  type ValidationTraceInlineLimitAt,
} from "./one-step-phase-evidence.js";
import type {
  ValidationTraceContextStepPlan,
  ValidationTracePhasePlan,
} from "./one-step-plan.js";

/** The longest required-observer field preimage a CEK observer step's
 * resolution carries inline: its context moves carry it whole, and the
 * longest of them lands within the transaction size limit less its
 * 512-byte reserve even when our wallet pays it
 * (the validation-trace dispute one-step CEK observer inline-cap test). */
export const VALIDATION_TRACE_CEK_OBSERVER_INLINE_MAX_PREIMAGE_BYTES = 8_913;

/** That limit, with what its step carried beside the field there (at 297
 * observers). What a CEK observer step carries beside the field varies from
 * one observer count to the next, as well as with the transaction's other
 * fields. */
export const VALIDATION_TRACE_CEK_OBSERVER_INLINE_LIMIT: ValidationTraceInlineLimitAt =
  measuredAt(
    VALIDATION_TRACE_CEK_OBSERVER_INLINE_MAX_PREIMAGE_BYTES,
    2_002,
    55_351,
  );

/** The bytes the moves of the step from state `stateIndex` whose evidence
 * is that of `argument` carry beside the field it reads by reference. */
const besideBytesOf = (
  { transitionCbor, auxiliaryCbor }: Argument,
  stateIndex: number,
) => transitionCbor.length + auxiliaryCbor.length + stepIndexBytes(stateIndex);

type Argument = ReturnType<typeof buildValidationOneStepArgument>;

type Reason = Readonly<{ reason: string }>;

/**
 * The plan `plan` makes of the argument `build` builds over a carriage
 * resolver, carrying by reference the field it reads past the inline limit
 * `inlineMax` names for it; with that field, the holder `reader` names
 * whose move opens it, and the plan again bound to our publications of it.
 */
const withCarried = <
  P extends ValidationTracePhasePlan | ValidationTraceContextStepPlan,
>({
  build,
  inlineMax,
  plan,
  reader,
}: {
  readonly build: (resolve: ValidationMachineFieldCarriageResolver) => Argument;
  readonly inlineMax: (
    argument: Argument,
    field: ValidationTraceCarriedField,
  ) => number | undefined;
  readonly plan: (
    argument: Argument,
    carried: Buffer | undefined,
  ) => P | Reason;
  readonly reader: (planned: P) => ValidationTraceOneStepHolder;
}): P | Reason => {
  const { argument, field } = carriedArgument(build, inlineMax);
  const planned = plan(argument, field?.preimage);
  if ("reason" in planned || field === undefined) return planned;
  const rebind = (carriage: MidgardFieldCarriage) => {
    const rebound = plan(
      build(fixedCarriageResolver(field, carriage)),
      field.preimage,
    );
    if ("reason" in rebound) throw new Error(rebound.reason);
    return rebound;
  };
  return Object.freeze({
    ...planned,
    carried: Object.freeze({ field, reader: reader(planned), rebind }),
  }) as P;
};

/** The resolution of the step of `phase` from state `stateIndex` of
 * `trace`; or why the runner does not drive it. */
export const planCarriedPhaseStep = ({
  trace,
  stateIndex,
  preState,
  phase,
  redeemerChain,
  successorWitnessCbor,
}: {
  readonly trace: ValidationTraceSteps;
  readonly stateIndex: number;
  readonly preState: MidgardValidationMachineState;
  readonly phase: ValidationTracePhase;
  readonly redeemerChain: boolean;
  readonly successorWitnessCbor: Uint8Array | undefined;
}): ValidationTracePhasePlan | Reason =>
  withCarried<ValidationTracePhasePlan>({
    build: (resolveFieldCarriage) =>
      buildValidationOneStepArgument({
        trace,
        stateIndex,
        resolveFieldCarriage,
      }),
    inlineMax: (argument) =>
      phaseInlineMaxPreimageBytes(
        phase,
        argument.semanticResolverIndex,
        besideBytesOf(argument, stateIndex),
      ),
    plan: (argument, carried) => {
      const common = commonOf(stateIndex, preState, argument);
      const step = planValidationTracePhaseStep({
        ...common,
        phase,
        redeemerChain,
        successorWitnessCbor,
        carried,
      });
      return "reason" in step
        ? step
        : Object.freeze({ ...common, kind: "phase" as const, phase: step });
    },
    reader: ({ phase: { semantic } }) => semantic,
  });

/** The resolution of the CEK context step from state `stateIndex`, whose
 * one-step argument `build` builds over a carriage resolver, from the
 * witness `witnessCbor` to the state whose witness is
 * `successorWitnessCbor`; or why the runner does not drive it. */
export const planCarriedContextStep = ({
  build,
  stateIndex,
  preState,
  witnessCbor,
  successorWitnessCbor,
}: {
  readonly build: (resolve: ValidationMachineFieldCarriageResolver) => Argument;
  readonly stateIndex: number;
  readonly preState: MidgardValidationMachineState;
  readonly witnessCbor: string;
  readonly successorWitnessCbor: string;
}): ValidationTraceContextStepPlan | Reason =>
  withCarried<ValidationTraceContextStepPlan>({
    build,
    inlineMax: (argument, { fieldIndex }) =>
      fieldIndex === CEK_OBSERVER_FIELD &&
      cekContextRouteOf(witnessCbor, argument.auxiliaryCbor.toString("hex")) ===
        "observerAuthenticate"
        ? inlineMaxPreimageBytesBeside(
            VALIDATION_TRACE_CEK_OBSERVER_INLINE_LIMIT,
            besideBytesOf(argument, stateIndex),
          )
        : undefined,
    plan: (argument, carried) => {
      const common = commonOf(stateIndex, preState, argument);
      const context = planValidationTraceContextStep({
        witnessCbor,
        auxiliaryCbor: common.auxiliaryCbor,
        successorWitnessCbor,
        carried,
      });
      return "driven" in context
        ? {
            reason: `a CEK context step at its ${context.stage} stage${carried === undefined ? "" : " reading a field by reference"}`,
          }
        : Object.freeze({ ...common, kind: "context" as const, context });
    },
    reader: () => CEK_CONTEXT_STAGE_HOLDERS.observerAuthenticate,
  });
