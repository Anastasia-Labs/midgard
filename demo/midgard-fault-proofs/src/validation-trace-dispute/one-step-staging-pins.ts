import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { workflowProofMaterialsHolder } from "../workflow/proof-materials-view.holder.js";
import {
  type ProofMaterialsAccess,
  proofMaterialsAccess,
} from "../workflow/proof-materials-view.js";
import type { SignedValidationTraceStaging } from "./one-step-staging-journal.js";

/**
 * Keeps the staging outputs `signed` names out of the proof-materials
 * sweep while the workflow `materials` holds for lives: a thread's binder
 * fixes the out-refs it reads, so only our own spend back, which knows
 * where each thread stands, may take one.
 */
export const pinValidationTraceStagings = (
  { view, holder }: ProofMaterialsAccess,
  signed: readonly SignedValidationTraceStaging[],
): void => {
  if (signed.length === 0) return;
  view.need(
    holder,
    signed.flatMap(({ datumCbor }) =>
      datumCbor === undefined ? [] : [datumCbor],
    ),
    signed.map(({ outRef }) => outRef),
  );
};

/** Pins `signed` for the workflow of `context`, through its signer's view. */
export const pinWorkflowValidationTraceStagings = (
  {
    signer,
    lucid,
  }: Readonly<{ signer: ResolvedProverSigner; lucid: LucidEvolution }>,
  context: FraudProofWorkflowAdapterContext,
  signed: readonly SignedValidationTraceStaging[],
): void => {
  if (signed.length === 0) return;
  pinValidationTraceStagings(
    proofMaterialsAccess({
      signer,
      lucid,
      holder: workflowProofMaterialsHolder(context),
    }),
    signed,
  );
};
