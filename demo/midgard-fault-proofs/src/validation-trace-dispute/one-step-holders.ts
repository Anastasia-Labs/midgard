/** Where a thread of ours stands during the one-step resolution of a CEK core
 * step the runner drives: the boundary game-v1 EnterResolution sends it to,
 * the CEK phase resolver, the CEK core step binder, the arm its step selects,
 * the settle, and the award. Each holder is a deployment contract, found by
 * its spending address; the thread token, minted once for its defender, is
 * what makes an output there ours. */
import * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  CEK_CORE_SEMANTIC_RESOLVER_INDEX,
  CEK_RESOLVER_INDEX,
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
  | "settle"
  | "award";

export type ValidationTraceOneStepContract = Readonly<{
  holder: ValidationTraceOneStepHolder;
  address: string;
  scriptHash: string;
  /** Its published reference script; undefined for the award's, read apart. */
  reference: UTxO | undefined;
}>;

type Validator = Readonly<{
  spendingScriptAddress: string;
  spendingScriptHash: string;
}>;

/** The CEK core step binder: the CEK group's core step semantic resolver. */
export const cekCoreSemanticResolver = (
  deployment: ValidationTraceDisputeDeployment,
): Validator => {
  const index =
    SDK.VALIDATION_TRACE_SEMANTIC_RESOLVER_GROUPS.cek[
      CEK_CORE_SEMANTIC_RESOLVER_INDEX
    ];
  const resolver =
    index === undefined ? undefined : deployment.chain.semanticResolvers[index];
  if (resolver === undefined)
    throw new Error("validation-trace deployment has no CEK core step binder");
  return resolver;
};

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
  return Object.freeze([
    contract("boundary", chain.boundary, references.boundary),
    contract("prepare", prepare, references.cekPrepare),
    contract("core", cekCoreSemanticResolver(deployment), references.cekCore),
    ...Object.values(SINGLE_HOP_CORE_ARMS).map((arm) =>
      contract(arm, chain.cekCoreStages[arm], references.arms[arm]),
    ),
    contract("settle", chain.cekCoreStages.settle, references.settle),
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
