import { inputExistenceForcedArtifacts } from "../input-existence/forced-artifact.js";
import { NON_EXISTENT_INPUT_KIND } from "./wrongful-rejection.js";

export const {
  write: nonExistentInputForcedArtifact,
  admit: admitNonExistentInputForcedArtifact,
} = inputExistenceForcedArtifacts(NON_EXISTENT_INPUT_KIND);
