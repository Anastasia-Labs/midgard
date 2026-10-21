/** The one-step resolution of a step of a validation phase whose resolver
 * is the generic phase resolver and whose semantic resolver verifies the
 * transition alone: past the boundary, the phase resolver commits the
 * transition and its auxiliary evidence (none, for these steps) and hands
 * the step to the semantic resolver its evidence selects, which awards it
 * in one hop. */
import * as SDK from "@al-ft/midgard-sdk";

import { NO_AUXILIARY_WITNESS_CBOR } from "./one-step-canonical-decode.js";
import { constr, continued, integer, stepDatum } from "./one-step-data.js";

/** The holders of each phase's resolution, with the deployment manifest's
 * name for each. */
export const PHASE_CONTRACT_NAMES = Object.freeze({
  compactBindingPrepare: "validationTraceDisputeCompactBindingPrepare",
  compactBindingSemantic: "validationTraceDisputeCompactBindingSemantic",
  staticLedgerRulesPrepare: "validationTraceDisputeStaticLedgerRulesPrepare",
  staticLedgerRulesSemantic: "validationTraceDisputeStaticLedgerRulesSemantic",
} as const);

export type ValidationTracePhaseHolder = keyof typeof PHASE_CONTRACT_NAMES;

export const PHASE_HOLDERS = Object.freeze(
  Object.keys(PHASE_CONTRACT_NAMES),
) as readonly ValidationTracePhaseHolder[];

export const isValidationTracePhaseHolder = (
  holder: string,
): holder is ValidationTracePhaseHolder =>
  Object.hasOwn(PHASE_CONTRACT_NAMES, holder);

/** Each phase the runner drives: its `resolver_index`
 * (validation-resolution-v1), its resolver's holder, and the holder of each
 * of its semantic resolvers, by the group-local index the evidence selects. */
export const PHASE_RESOLUTIONS = Object.freeze({
  compactBinding: {
    resolverIndex: 1,
    prepare: "compactBindingPrepare",
    semantics: ["compactBindingSemantic"],
  },
  staticLedgerRules: {
    resolverIndex: 2,
    prepare: "staticLedgerRulesPrepare",
    semantics: ["staticLedgerRulesSemantic"],
  },
} as const satisfies Readonly<
  Partial<
    Record<
      keyof typeof SDK.VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUPS,
      Readonly<{
        resolverIndex: number;
        prepare: ValidationTracePhaseHolder;
        semantics: readonly ValidationTracePhaseHolder[];
      }>
    >
  >
>);

export type ValidationTracePhase = keyof typeof PHASE_RESOLUTIONS;

export const isValidationTracePhase = (
  phase: string,
): phase is ValidationTracePhase => Object.hasOwn(PHASE_RESOLUTIONS, phase);

type Chain = Pick<
  SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"],
  "prepareResolvers" | "semanticResolvers"
>;

/** The validator of each phase holder on `chain`. */
export const phaseValidators = (
  chain: Chain,
): Readonly<Record<ValidationTracePhaseHolder, SDK.SpendingValidator>> => {
  const validators: Partial<
    Record<ValidationTracePhaseHolder, SDK.SpendingValidator>
  > = {};
  for (const [phase, { resolverIndex, prepare, semantics }] of Object.entries(
    PHASE_RESOLUTIONS,
  ) as [
    ValidationTracePhase,
    (typeof PHASE_RESOLUTIONS)[ValidationTracePhase],
  ][]) {
    validators[prepare] = chain.prepareResolvers[resolverIndex];
    semantics.forEach((holder, local) => {
      const index = SDK.VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUPS[phase][local];
      const resolver =
        index === undefined ? undefined : chain.semanticResolvers[index];
      if (resolver === undefined)
        throw new Error(
          `validation-trace deployment has no ${phase} semantic resolver ${local.toString()}`,
        );
      validators[holder] = resolver;
    });
  }
  return Object.freeze(
    validators as Record<ValidationTracePhaseHolder, SDK.SpendingValidator>,
  );
};

/** A phase step as its resolution needs it: the holders the thread visits
 * past the boundary. */
export type ValidationTracePhaseStep = Readonly<{
  phase: ValidationTracePhase;
  prepare: ValidationTracePhaseHolder;
  semantic: ValidationTracePhaseHolder;
}>;

/**
 * The resolution of the step of `phase` whose one-step argument selects
 * `resolverIndex` and the group-local `semanticResolverIndex` with the
 * auxiliary evidence `auxiliaryCbor`; or why the runner does not drive it.
 */
export const planValidationTracePhaseStep = ({
  phase,
  resolverIndex,
  semanticResolverIndex,
  auxiliaryCbor,
}: {
  readonly phase: ValidationTracePhase;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly auxiliaryCbor: string;
}): ValidationTracePhaseStep | Readonly<{ reason: string }> => {
  const resolution = PHASE_RESOLUTIONS[phase];
  if (resolverIndex !== resolution.resolverIndex)
    throw new Error(`validation-trace ${phase} step selects another resolver`);
  const semantic: ValidationTracePhaseHolder | undefined =
    resolution.semantics[semanticResolverIndex];
  if (semantic === undefined)
    return {
      reason: `a ${phase} step for semantic resolver ${semanticResolverIndex.toString()}`,
    };
  // These semantic resolvers verify the transition beside no auxiliary
  // evidence, which is all the resolver may then have committed.
  if (auxiliaryCbor !== NO_AUXILIARY_WITNESS_CBOR)
    throw new Error(
      `validation-trace ${phase} step carries auxiliary evidence its semantic resolver does not take`,
    );
  return Object.freeze({ phase, prepare: resolution.prepare, semantic });
};

/** The semantic resolver's hop of the thread of prover `prover`: it
 * verifies the transition `transitionCbor` and awards the step. */
export const phaseSemanticHop = ({
  prover,
  transitionCbor,
}: {
  readonly prover: string;
  readonly transitionCbor: string;
}) => ({
  next: "award" as const,
  datum: stepDatum(prover, constr(0, [integer(1)])),
  redeemer: (inputIndex: bigint, outputIndex: bigint) =>
    continued(
      constr(0, [integer(inputIndex), integer(outputIndex), transitionCbor]),
    ),
  proverOnly: false,
});

/** Where each phase holder's datum keeps our successor's hash: the
 * resolution's challenger successor hash, behind the prepared state at the
 * semantic resolver. */
export const PHASE_CHALLENGER_SUCCESSOR_HASH = Object.freeze(
  Object.fromEntries(
    Object.values(PHASE_RESOLUTIONS).flatMap(({ prepare, semantics }) => [
      [prepare, [1, 0, 3]],
      ...semantics.map((holder) => [holder, [1, 0, 1, 3]] as const),
    ]),
  ) as Readonly<Record<ValidationTracePhaseHolder, readonly number[]>>,
);
