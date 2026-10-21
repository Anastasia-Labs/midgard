/** What a CEK script selection's hop reads, which the runner gathers
 * before it builds the hop (one-step-moves.ts). */
import type { UTxO } from "@lucid-evolution/lucid";

import { findScriptDecodeRecord } from "../script-decode/record.js";
import type { ValidationTraceClaimMaterial } from "./claim-material.js";
import type { ValidationTraceMoveInput } from "./moves.js";
import type { ValidationTraceSelectionReads } from "./one-step-builders.js";
import type { ValidationTraceOneStepPlan } from "./one-step-plan.js";

/**
 * What the selection resolver's hop of `plan` reads on `deployment`: the
 * selection's yields and the selected script's decode record, which the
 * script-decode prerequisite has made before the thread entered its
 * resolution.
 */
export const validationTraceSelectionReads = async ({
  input,
  plan,
}: {
  readonly input: Pick<ValidationTraceMoveInput, "lucid" | "deployment"> & {
    readonly material: ValidationTraceClaimMaterial | undefined;
  };
  readonly plan: ValidationTraceOneStepPlan;
}): Promise<ValidationTraceSelectionReads> => {
  const { lucid, deployment, material } = input;
  const references = deployment.references.oneStep?.cekSelection;
  const { scriptDecode } = deployment;
  if (
    plan.kind !== "selection" ||
    references === undefined ||
    scriptDecode === undefined ||
    material === undefined
  )
    throw new Error(
      "validation-trace selection hop has no CEK script selection to drive",
    );
  let record: UTxO | undefined;
  if (plan.script !== undefined) {
    record = await findScriptDecodeRecord({
      lucid,
      deployment: scriptDecode,
      scriptHashHex: plan.script.scriptHashHex,
    });
    if (record === undefined)
      throw new Error(
        `validation-trace selection's script ${plan.script.scriptHashHex} has no decode record`,
      );
  }
  const { cekSelectionAuthenticate, cekSelectionSuccessor } =
    deployment.chain.yields;
  return Object.freeze({
    trace: await material.trace(),
    yields: {
      references: [references.authenticate, references.successor] as const,
      scripts: [
        cekSelectionAuthenticate.withdrawalScript,
        cekSelectionSuccessor.withdrawalScript,
      ] as const,
    },
    record,
    scriptDecodePolicyId: scriptDecode.validator.policyId,
  });
};
