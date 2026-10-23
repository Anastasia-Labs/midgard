/** The plan a thread of ours resolves, read from the thread where its
 * holder keeps our successor's hash, and planned from our trace. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import type * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { CANONICAL_DECODE_CHALLENGER_SUCCESSOR_HASH } from "./one-step-canonical-decode.js";
import { isValidationTraceContextHolder } from "./one-step-context-hops.js";
import type { ValidationTraceRedeemerItemCarried } from "./one-step-context-item-hops.js";
import {
  type ValidationTraceOneStepContract,
  validationTraceOneStepDrives,
} from "./one-step-holders.js";
import { PHASE_CHALLENGER_SUCCESSOR_HASH } from "./one-step-phase.js";
import {
  planValidationTraceOneStep,
  planValidationTraceThread,
  type ValidationTraceOneStepPlan,
  type ValidationTraceOneStepPlanning,
} from "./one-step-plan.js";
import { PREPARED_CHALLENGER_SUCCESSOR_HASH } from "./one-step-script-sources-redeemer.js";
import type { ValidationTraceStagingLookup } from "./one-step-staged.js";
import { isThreadMaterial } from "./threads.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/** A plan, or the reason the runner does not drive the step, as an error. */
export const planned = (
  planning: ValidationTraceOneStepPlanning,
): ValidationTraceOneStepPlan => {
  if (planning.kind !== "plan")
    throw new Error(`validation-trace one step: ${planning.reason}`);
  return planning.plan;
};

/** Where each holder past the boundary keeps our successor's hash: the
 * selection and context step resolvers hold the state the core step binder
 * would, and each context holder past them nests the state its predecessor
 * checked one level deeper; past the item bind stage, the verified item step
 * nests the staged context once more, and its hash or value result once
 * more again; past the observer authenticate, the pending opening nests
 * the staged context once more, and past the redeemer select authenticate,
 * the selection once more, and the purpose or initial pending again, and
 * past the finalize authenticate, the pending finalization once more. A
 * holder of the shared item chain keeps it in the state the thread carried
 * into the chain's entry alone (redeemerItemCarried). */
const CHALLENGER_SUCCESSOR_HASH = {
  prepare: [1, 0, 3],
  core: [1, 0, 1, 3],
  selection: [1, 0, 1, 3],
  context: [1, 0, 1, 3],
  bound: [1, 0, 0, 1, 3],
  contextControl: [1, 0, 0, 1, 3],
  contextStage: [1, 0, 0, 0, 1, 3],
  contextSettle: [1, 0, 0, 0, 0, 1, 3],
  contextItemReturn: [1, 0, 0, 0, 0, 1, 3],
  contextItemSelectionHash: [1, 0, 0, 0, 0, 1, 3],
  contextItemSelectionFinish: [1, 0, 0, 0, 0, 1, 3],
  contextItemDataHash: [1, 0, 0, 0, 0, 1, 3],
  contextItemDataFinishDescriptor: [1, 0, 0, 0, 0, 1, 3],
  contextItemFinalize: [1, 0, 0, 0, 0, 1, 3],
  contextItemSelectionContinue: [1, 0, 0, 0, 0, 0, 1, 3],
  contextItemDataContinue: [1, 0, 0, 0, 0, 0, 1, 3],
  contextItemDataFinishValue: [1, 0, 0, 0, 0, 0, 1, 3],
  contextObserverFold: [1, 0, 0, 0, 0, 1, 3],
  contextRedeemerSelectInitialize: [1, 0, 0, 0, 0, 1, 3],
  contextRedeemerSelectHash: [1, 0, 0, 0, 0, 0, 1, 3],
  contextRedeemerSelectFinish: [1, 0, 0, 0, 0, 0, 1, 3],
  contextFinalizeSpend: [1, 0, 0, 0, 0, 1, 3],
  ...CANONICAL_DECODE_CHALLENGER_SUCCESSOR_HASH,
  ...PHASE_CHALLENGER_SUCCESSOR_HASH,
} as const;

/** Where the state each item chain's entry took keeps our successor's
 * hash: the CEK item step's pending step, and the script-sources redeemer
 * normalization step's prepared resolution. */
const CARRIED_CHALLENGER_SUCCESSOR_HASH = {
  cek: [0, 0, 0, 1, 3],
  scriptSources: PREPARED_CHALLENGER_SUCCESSOR_HASH,
} as const;

const challengerSuccessorPath = (
  holder: ValidationTraceOneStepContract["holder"],
): readonly number[] =>
  holder in CHALLENGER_SUCCESSOR_HASH
    ? CHALLENGER_SUCCESSOR_HASH[
        holder as keyof typeof CHALLENGER_SUCCESSOR_HASH
      ]
    : isValidationTraceContextHolder(holder)
      ? CHALLENGER_SUCCESSOR_HASH.contextStage
      : CHALLENGER_SUCCESSOR_HASH.bound;

/**
 * The plan the thread `utxo` at `holder` resolves, from our trace in
 * `material`: at the boundary, by its case thread `caseThread`; past it, by
 * our successor's hash its holder keeps, or, along the shared item chain,
 * the state it carried into the chain's entry, `carried`, keeps; a staged
 * step past the boundary staged where `staging` says.
 */
export const resolvingPlan = async ({
  deployment,
  material,
  utxo,
  holder,
  caseThread,
  carried,
  staging,
}: {
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly material: ValidationTraceClaimMaterial | undefined;
  readonly utxo: UTxO;
  readonly holder: ValidationTraceOneStepContract["holder"];
  readonly caseThread: SDK.CaseThread | undefined;
  readonly carried: ValidationTraceRedeemerItemCarried | undefined;
  readonly staging?: ValidationTraceStagingLookup | undefined;
}): Promise<ValidationTraceOneStepPlan> => {
  const drives = validationTraceOneStepDrives(deployment);
  if (caseThread !== undefined) {
    if (!isThreadMaterial(caseThread, material))
      throw new Error(
        "validation-trace one step has no trace matching its thread's challenger root",
      );
    return planned(
      await planValidationTraceThread(material, caseThread, drives),
    );
  }
  if (material === undefined)
    throw new Error("validation-trace one step has no trace of ours");
  return planned(
    await planValidationTraceOneStep({
      material,
      challengerHighHash: Data.from<string>(
        carried === undefined
          ? plutusConstrFieldCbor(utxo.datum!, [
              ...challengerSuccessorPath(holder),
            ])
          : plutusConstrFieldCbor(carried.stateCbor, [
              ...CARRIED_CHALLENGER_SUCCESSOR_HASH[carried.carrier],
            ]),
      ),
      ...drives,
      ...(staging === undefined ? {} : { staging }),
    }),
  );
};
