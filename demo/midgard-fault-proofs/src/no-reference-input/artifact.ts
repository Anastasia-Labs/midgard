import { inputExistenceForcedArtifacts } from "../input-existence/forced-artifact.js";
import { NO_REFERENCE_INPUT_KIND } from "./wrongful-rejection.js";

export const NO_REFERENCE_INPUT_FORCED_ARTIFACT =
  NO_REFERENCE_INPUT_KIND.forcedArtifact;
export const {
  write: noReferenceInputForcedArtifact,
  admit: admitNoReferenceInputForcedArtifact,
} = inputExistenceForcedArtifacts(NO_REFERENCE_INPUT_KIND);
