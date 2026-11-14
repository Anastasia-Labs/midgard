/** The one-step resolution of a script-sources redeemer normalization step:
 * past the phase resolver, the redeemer normalization semantic resolver
 * binds the step's redeemer item into its envelope, and the shared redeemer
 * item chain verifies the item step, its traversal and outer normalizers on
 * the current control (and, for a step that advances, on the next), its
 * source authenticator, the executor the step selects and its execution
 * settlement, which awards the step. The chain's normalizers, source
 * authenticator and first executors are the CEK item chain's own holders
 * (one-step-context-item-holders.ts); its invalid-item executors and its
 * settlement are its own. The chain's later states carry the prepared
 * resolution no further than the normalizers, so a chain holder's hop reads
 * it from the thread's own L1 history, where the payment into the semantic
 * resolver wrote it (one-step-context-item-hops.ts). */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { deriveScriptSourcesRedeemerItemPlan } from "../redeemer-item-plan.js";
import type { ValidationTraceHop } from "./one-step-builders.js";
import {
  CONTEXT_ITEM_EXECUTOR_HOLDERS,
  type ValidationTraceContextItemHolder,
} from "./one-step-context-item-holders.js";
import { stepDatum } from "./one-step-data.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/** The phase holder of the redeemer normalization semantic resolver, the
 * chain's entry. */
export const SCRIPT_SOURCES_REDEEMER_SEMANTIC =
  "scriptSourcesRedeemerNormalizationSemantic" as const;

/** The chain's executors past the CEK item chain's, in its authenticated
 * roster order: the invalid header, tail, data and scalar data executors. */
const INVALID_EXECUTOR_HOLDERS = [
  "redeemerItemInvalidHeaderExecutor",
  "redeemerItemInvalidTailExecutor",
  "redeemerItemInvalidDataExecutor",
  "redeemerItemInvalidScalarDataExecutor",
] as const;

/** The chain's own holders, with the deployment manifest's name for each. */
export const SCRIPT_SOURCES_REDEEMER_CONTRACT_NAMES = Object.freeze({
  redeemerItemInvalidHeaderExecutor:
    "validationTraceDisputeRedeemerItemInvalidHeaderExecutor",
  redeemerItemInvalidTailExecutor:
    "validationTraceDisputeRedeemerItemInvalidTailExecutor",
  redeemerItemInvalidDataExecutor:
    "validationTraceDisputeRedeemerItemInvalidDataExecutor",
  redeemerItemInvalidScalarDataExecutor:
    "validationTraceDisputeRedeemerItemInvalidScalarDataExecutor",
  redeemerItemSettlement: "validationTraceDisputeRedeemerItemSettlement",
} as const);

export type ValidationTraceScriptSourcesRedeemerOwnHolder =
  keyof typeof SCRIPT_SOURCES_REDEEMER_CONTRACT_NAMES;

/** The holders the chain shares with the CEK item chain. */
const SHARED_HOLDERS = [
  "redeemerItemTraversalNormalizer",
  "redeemerItemOuterNormalizer",
  "redeemerItemSourceAuthenticator",
  ...CONTEXT_ITEM_EXECUTOR_HOLDERS,
] as const satisfies readonly ValidationTraceContextItemHolder[];

/** Each holder of the chain past its entry. */
export const SCRIPT_SOURCES_REDEEMER_HOLDERS = Object.freeze([
  ...SHARED_HOLDERS,
  ...INVALID_EXECUTOR_HOLDERS,
  "redeemerItemSettlement",
] as const);

export type ValidationTraceScriptSourcesRedeemerHolder =
  (typeof SCRIPT_SOURCES_REDEEMER_HOLDERS)[number];

const OWN: ReadonlySet<string> = new Set(
  Object.keys(SCRIPT_SOURCES_REDEEMER_CONTRACT_NAMES),
);
const SHARED: ReadonlySet<string> = new Set(SHARED_HOLDERS);

/** Whether the CEK item chain also visits `holder`. */
export const isSharedRedeemerItemHolder = (holder: string): boolean =>
  SHARED.has(holder);

/** Whether `holder` is the chain's entry or a holder past it. */
export const isScriptSourcesRedeemerChainHolder = (holder: string): boolean =>
  holder === SCRIPT_SOURCES_REDEEMER_SEMANTIC ||
  SHARED.has(holder) ||
  OWN.has(holder);

/** Whether `holder` is one of the chain's own holders. */
export const isScriptSourcesRedeemerOwnHolder = (
  holder: string,
): holder is ValidationTraceScriptSourcesRedeemerOwnHolder => OWN.has(holder);

type Stages =
  SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"]["scriptSourcesStageOneRedeemerStages"];

/** The validator of each holder of the chain past its entry. */
export const scriptSourcesRedeemerValidators = (
  stages: Stages,
): Readonly<
  Record<ValidationTraceScriptSourcesRedeemerHolder, SDK.SpendingValidator>
> => {
  const validators: Partial<
    Record<ValidationTraceScriptSourcesRedeemerHolder, SDK.SpendingValidator>
  > = {
    redeemerItemTraversalNormalizer: stages.traversalNormalizer,
    redeemerItemOuterNormalizer: stages.outerNormalizer,
    redeemerItemSourceAuthenticator: stages.sourceAuthenticator,
    redeemerItemSettlement: stages.settlement,
  };
  for (const [index, holder] of [
    ...CONTEXT_ITEM_EXECUTOR_HOLDERS,
    ...INVALID_EXECUTOR_HOLDERS,
  ].entries()) {
    const executor = stages.executors[index];
    if (executor === undefined)
      throw new Error(
        "validation-trace script-sources redeemer executor roster is short",
      );
    validators[holder] = executor;
  }
  return Object.freeze(
    validators as Record<
      ValidationTraceScriptSourcesRedeemerHolder,
      SDK.SpendingValidator
    >,
  );
};

/** What a hop through the chain reads: the deployment's chain and its
 * identity, and, at a holder past the entry, the prepared resolution the
 * thread carried into the entry, as Data CBOR. */
export type ValidationTraceScriptSourcesRedeemerReads = Readonly<{
  stages: Stages;
  deploymentId: string;
  preparedCbor: string | undefined;
}>;

/** The chain reads of a hop on `deployment`, its prepared resolution
 * `preparedCbor`. */
export const scriptSourcesRedeemerReads = (
  deployment: Pick<ValidationTraceDisputeDeployment, "chain" | "init">,
  preparedCbor: string | undefined,
): ValidationTraceScriptSourcesRedeemerReads =>
  Object.freeze({
    stages: deployment.chain.scriptSourcesStageOneRedeemerStages,
    deploymentId: SDK.deriveValidationTraceDeploymentId(
      deployment.init.catalogue.policyId,
    ),
    preparedCbor,
  });

/** Where the prepared resolution keeps our successor's hash. */
export const PREPARED_CHALLENGER_SUCCESSOR_HASH = [1, 3] as const;

/** The canonical CBOR of the Data `cbor` encodes. */
const canonical = (cbor: string) => Data.to(Data.from(cbor));

/**
 * The hop of the thread whose datum is `datum`, of prover `prover`, at the
 * chain's entry or its holder `holder`, for the step whose transition and
 * auxiliary evidence are `transitionCbor` and `auxiliaryCbor`, reading
 * `reads`: the stage of the chain's plan that holder checks with the
 * thread's state as its input, to the holder of the stage after it, or to
 * the award past the settlement.
 */
export const scriptSourcesRedeemerHop = ({
  holder,
  datum,
  prover,
  transitionCbor,
  auxiliaryCbor,
  reads,
}: {
  readonly holder: string;
  readonly datum: string;
  readonly prover: string;
  readonly transitionCbor: string;
  readonly auxiliaryCbor: string;
  readonly reads: ValidationTraceScriptSourcesRedeemerReads | undefined;
}): ValidationTraceHop => {
  if (reads === undefined)
    throw new Error(
      `validation-trace ${holder} hop has no script-sources redeemer chain`,
    );
  // The state the thread's holder checks.
  const state = plutusConstrFieldCbor(datum, [1, 0]);
  const entry = holder === SCRIPT_SOURCES_REDEEMER_SEMANTIC;
  const prepared = entry ? state : reads.preparedCbor;
  if (prepared === undefined)
    throw new Error(
      `validation-trace ${holder} hop has no prepared script-sources resolution`,
    );
  const validators = scriptSourcesRedeemerValidators(reads.stages);
  const stages = deriveScriptSourcesRedeemerItemPlan({
    preparedResolution: Data.from(prepared),
    transition: Data.from(transitionCbor),
    auxiliary: Data.from(auxiliaryCbor),
    stages: { ...reads.stages, entry: reads.stages.envelope },
    deploymentId: reads.deploymentId,
  });
  const here = entry
    ? reads.stages.envelope.spendingScriptHash
    : validators[holder as ValidationTraceScriptSourcesRedeemerHolder]
        ?.spendingScriptHash;
  const at = stages.findIndex(
    ({ validator, inputState }) =>
      validator.spendingScriptHash === here &&
      Data.to(inputState) === canonical(state),
  );
  const stage = stages[at];
  if (stage === undefined)
    throw new Error(
      `validation-trace ${holder} holds a thread at no stage of its redeemer item step`,
    );
  const following = stages[at + 1]?.validator.spendingScriptHash;
  const next =
    following === undefined
      ? "award"
      : SCRIPT_SOURCES_REDEEMER_HOLDERS.find(
          (found) => validators[found].spendingScriptHash === following,
        );
  if (next === undefined)
    throw new Error(
      "validation-trace script-sources redeemer stage leads to no holder",
    );
  return {
    next,
    datum: stepDatum(prover, Data.to(stage.outputState)),
    redeemer: (inputIndex, outputIndex) =>
      Data.to(stage.spendRedeemer(inputIndex, outputIndex)),
    proverOnly: true,
  };
};

/** The chain's references, by holder, when the deployment publishes the
 * chain past its entry. */
export type ValidationTraceScriptSourcesRedeemerReferences = Readonly<
  Record<ValidationTraceScriptSourcesRedeemerHolder, UTxO>
>;
