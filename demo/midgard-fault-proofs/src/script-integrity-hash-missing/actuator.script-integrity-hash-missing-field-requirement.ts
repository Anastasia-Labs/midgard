import { type FaultProofFieldOpeningPlan } from "../field-opening.js";
import { direct } from "./actuator.resolve-field.js";
import { admitScriptIntegrityHashMissingArtifact } from "./artifact.js";

export const scriptIntegrityHashMissingFieldRequirement = ({
  actionStage,
  artifact,
  owner,
}: {
  readonly actionStage: unknown;
  readonly artifact: unknown;
  readonly owner: string;
}): FaultProofFieldOpeningPlan | null => {
  const admitted = admitScriptIntegrityHashMissingArtifact(artifact, owner);
  if (actionStage === "step_03" && direct(admitted)) return null;
  if (["step_03", "step_04", "step_05"].includes(String(actionStage)))
    return admitted.scriptPlan;
  return actionStage === "step_06" ? admitted.redeemerPlan : null;
};
