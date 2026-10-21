/** Where a thread of ours stands during the one-step resolution of a CEK step
 * the runner drives: the boundary game-v1 EnterResolution sends it to, the
 * CEK phase resolver, then for a core step the CEK core step binder, the arm
 * its step selects and the settle, or for a script selection the selection
 * resolver, and the award. Each holder is a deployment contract, found by
 * its spending address; the thread token, minted once for its defender, is
 * what makes an output there ours. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  CEK_CORE_SEMANTIC_RESOLVER_INDEX,
  CEK_RESOLVER_INDEX,
  CEK_SELECTION_SEMANTIC_RESOLVER_INDEX,
  SINGLE_HOP_CORE_ARMS,
  type SingleHopCoreGroup,
} from "./one-step-plan.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

export type ValidationTraceArmHolder =
  (typeof SINGLE_HOP_CORE_ARMS)[SingleHopCoreGroup];

/** A holder of the thread past the game, in the order the thread visits. */
export type ValidationTraceOneStepHolder =
  | "boundary"
  | "prepare"
  | "core"
  | ValidationTraceArmHolder
  | "selection"
  | "settle"
  | "award";

export type ValidationTraceOneStepContract = Readonly<{
  holder: ValidationTraceOneStepHolder;
  address: string;
  scriptHash: string;
  /** Its published reference script; undefined for the award's, read apart. */
  reference: UTxO | undefined;
}>;

/**
 * The deployment manifest's name for each holder a thread of ours visits
 * between the game and the award: the boundary, then the one-step
 * resolvers. A watcher's funding terms name these holders' addresses, so
 * its funding permit admits the moves that pay a thread to them.
 */
export const VALIDATION_TRACE_ONE_STEP_CONTRACT_NAMES = Object.freeze({
  boundary: "validationTraceDisputeBoundary",
  prepare: "validationTraceDisputeCekPrepare",
  core: "validationTraceDisputeCekCoreStepSemantic",
  compute: "validationTraceDisputeCekCoreCompute",
  machine: "validationTraceDisputeCekCoreMachine",
  mapConversion: "validationTraceDisputeCekCoreMapConversion",
  caseConstant: "validationTraceDisputeCekCoreCaseConstant",
  settle: "validationTraceDisputeCekCoreSettle",
  selection: "validationTraceDisputeCekExecutionSelectionSemantic",
} as const satisfies Readonly<
  Record<Exclude<ValidationTraceOneStepHolder, "award">, string>
>);

type Validator = Readonly<{
  spendingScriptAddress: string;
  spendingScriptHash: string;
}>;

/** The CEK group's semantic resolver at `cekIndex`. */
const cekSemanticResolver = (
  deployment: ValidationTraceDisputeDeployment,
  cekIndex: number,
  label: string,
): Validator => {
  const index = SDK.VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUPS.cek[cekIndex];
  const resolver =
    index === undefined ? undefined : deployment.chain.semanticResolvers[index];
  if (resolver === undefined)
    throw new Error(`validation-trace deployment has no ${label}`);
  return resolver;
};

/** The CEK core step binder: the CEK group's core step semantic resolver. */
export const cekCoreSemanticResolver = (
  deployment: ValidationTraceDisputeDeployment,
): Validator =>
  cekSemanticResolver(
    deployment,
    CEK_CORE_SEMANTIC_RESOLVER_INDEX,
    "CEK core step binder",
  );

/** The CEK group's execution selection semantic resolver. */
export const cekSelectionSemanticResolver = (
  deployment: ValidationTraceDisputeDeployment,
): Validator =>
  cekSemanticResolver(
    deployment,
    CEK_SELECTION_SEMANTIC_RESOLVER_INDEX,
    "CEK execution selection resolver",
  );

/**
 * Whether the runner drives CEK script selections on `deployment`: it
 * installs the selection resolver and its yields, and names the script-decode
 * deployment whose records a selection reads.
 */
export const validationTraceDrivesSelections = (
  deployment: ValidationTraceDisputeDeployment,
): boolean =>
  deployment.scriptDecode !== undefined &&
  deployment.references.oneStep?.cekSelection !== undefined;

/**
 * The holders of the one-step resolution the runner drives, with their
 * published reference scripts; empty when the deployment does not name them.
 */
export const validationTraceOneStepContracts = (
  deployment: ValidationTraceDisputeDeployment,
): readonly ValidationTraceOneStepContract[] => {
  const references = deployment.references.oneStep;
  if (references === undefined) return [];
  const { chain } = deployment;
  const contract = (
    holder: ValidationTraceOneStepHolder,
    validator: Validator,
    reference: UTxO | undefined,
  ): ValidationTraceOneStepContract =>
    Object.freeze({
      holder,
      address: validator.spendingScriptAddress,
      scriptHash: validator.spendingScriptHash,
      reference,
    });
  const prepare = chain.prepareResolvers[CEK_RESOLVER_INDEX];
  const selection = references.cekSelection;
  return Object.freeze([
    contract("boundary", chain.boundary, references.boundary),
    contract("prepare", prepare, references.cekPrepare),
    contract("core", cekCoreSemanticResolver(deployment), references.cekCore),
    ...Object.values(SINGLE_HOP_CORE_ARMS).map((arm) =>
      contract(arm, chain.cekCoreStages[arm], references.arms[arm]),
    ),
    contract("settle", chain.cekCoreStages.settle, references.settle),
    ...(selection === undefined
      ? []
      : [
          contract(
            "selection",
            cekSelectionSemanticResolver(deployment),
            selection.semantic,
          ),
        ]),
    contract("award", chain.award, undefined),
  ]);
};

/** The holder at `address`, or undefined when no holder is there. */
export const validationTraceOneStepHolderAt = (
  deployment: ValidationTraceDisputeDeployment,
  address: string,
): ValidationTraceOneStepContract | undefined =>
  validationTraceOneStepContracts(deployment).find(
    (contract) => contract.address === address,
  );

/** The holder named `holder`; throws when the deployment does not name it. */
export const validationTraceOneStepHolder = (
  deployment: ValidationTraceDisputeDeployment,
  holder: ValidationTraceOneStepHolder,
): ValidationTraceOneStepContract => {
  const found = validationTraceOneStepContracts(deployment).find(
    (contract) => contract.holder === holder,
  );
  if (found === undefined)
    throw new Error(
      `validation-trace deployment does not install the one-step ${holder}`,
    );
  return found;
};
