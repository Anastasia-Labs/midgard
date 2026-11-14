import * as SDK from "@al-ft/midgard-sdk";

import type { InputExistenceKind } from "../input-existence/kind.js";
import {
  type InputExistenceForcedSource,
  inputExistenceWrongfulRejection,
  type PreparedInputExistenceWrongfulRejection,
} from "../input-existence/wrongful-rejection.js";

export const NON_EXISTENT_INPUT_WRONGFUL_REJECTION_VIOLATION_ID =
  "non-existent-input-wrongful-rejection" as const;
/** A spent input the forced transaction names as not found. */
export const NON_EXISTENT_INPUT_KIND = {
  category: "nonExistentInput",
  name: "non-existent-input",
  violationId: NON_EXISTENT_INPUT_WRONGFUL_REJECTION_VIOLATION_ID,
  forcedArtifact: "midgard-non-existent-input-forced-artifact-v1",
  forcedSourceSchema: SDK.NonExistentInputForcedSourcePayloadSchema,
  sourceKind: 0n,
  fieldIndex: 0,
} as const satisfies InputExistenceKind;
export type NonExistentInputForcedSource = InputExistenceForcedSource;
export type PreparedNonExistentInputWrongfulRejection =
  PreparedInputExistenceWrongfulRejection;
export const {
  forcedSourceMaterial: nonExistentInputForcedSourceMaterial,
  prepare: prepareNonExistentInputWrongfulRejection,
  detect: detectNonExistentInputWrongfulRejections,
} = inputExistenceWrongfulRejection(NON_EXISTENT_INPUT_KIND);
