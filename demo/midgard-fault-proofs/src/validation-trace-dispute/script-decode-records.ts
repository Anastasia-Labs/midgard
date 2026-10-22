/** The decode records of the scripts our trace selects, on a deployment that
 * drives CEK script selections. Each is made through the shared script-decode
 * prerequisite as journaled actions of our workflow: before Init, so no
 * thread's clock runs while it is made, and before any resolution move should
 * one be missing by then. */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { ResolvedProverSigner } from "../runtime.js";
import { findScriptDecodeRecord } from "../script-decode/record.js";
import type { FraudProofFamilyWorkflowAdapter } from "../workflow/orchestrator.js";
import type { FraudProofAuthenticatedPublicationObserver } from "../workflow/raw-l1-publication-observation.js";
import {
  createScriptDecodePrerequisitePort,
  withScriptDecodePrerequisite,
} from "../workflow/script-decode-prerequisite.js";
import {
  parseValidationTraceAction,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "./actions.js";
import { parseValidationTraceDisputeArtifact } from "./artifact.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import { validationTraceDrivesSelections } from "./one-step-holders.js";
import { unrecordedSelectedScript } from "./selected-scripts.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

const CATEGORY = "validationTraceDispute" as const;

/**
 * `base`, which on a deployment that drives CEK script selections first
 * records the decode of each script our trace selects that has no record;
 * `base` itself on any other deployment. `materialOf` is the claim material
 * of a block, as its artifact was last prepared or validated.
 */
export const withValidationTraceScriptDecodeRecords = ({
  base,
  lucid,
  signer,
  deployment,
  publications,
  now,
  materialOf,
}: {
  readonly base: FraudProofFamilyWorkflowAdapter;
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly publications: FraudProofAuthenticatedPublicationObserver | undefined;
  readonly now: () => number;
  readonly materialOf: (
    headerHash: string,
  ) => ValidationTraceClaimMaterial | undefined;
}): FraudProofFamilyWorkflowAdapter => {
  const { scriptDecode } = deployment;
  if (
    scriptDecode === undefined ||
    !validationTraceDrivesSelections(deployment)
  )
    return base;
  if (publications === undefined)
    throw new Error(
      "validation-trace script-decode records need a publication observer",
    );
  return withScriptDecodePrerequisite({
    category: CATEGORY,
    base,
    prerequisite: createScriptDecodePrerequisitePort({
      category: CATEGORY,
      lucid,
      signer,
      deployment: () => scriptDecode,
      publications,
      now,
      itemForAction: async ({ action, artifact }) => {
        if (action.input.schemaVersion !== VALIDATION_TRACE_DISPUTE_ACTION)
          return null;
        const { stage } = parseValidationTraceAction(action);
        if (
          stage !== "init" &&
          stage !== "enter_resolution" &&
          stage !== "resolve"
        )
          return null;
        const { headerHash } = parseValidationTraceDisputeArtifact(artifact);
        const material = materialOf(headerHash);
        if (material === undefined)
          throw new Error(
            `validation-trace ${stage} has no claim material to find the scripts it selects`,
          );
        return await unrecordedSelectedScript({
          selections: await material.selections(),
          scriptItem: material.scriptItem,
          recorded: async (scriptHashHex) =>
            (await findScriptDecodeRecord({
              lucid,
              deployment: scriptDecode,
              scriptHashHex,
            })) !== undefined,
        });
      },
    }),
  });
};
