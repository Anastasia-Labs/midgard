import * as SDK from "@al-ft/midgard-sdk";

import type { InputExistenceKind } from "../input-existence/kind.js";
import {
  type InputExistenceForcedSource,
  inputExistenceWrongfulRejection,
  type PreparedInputExistenceWrongfulRejection,
} from "../input-existence/wrongful-rejection.js";

export const NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID =
  "no-reference-input-wrongful-rejection" as const;
/** A reference input the forced transaction names as not found. */
export const NO_REFERENCE_INPUT_KIND = {
  category: "noReferenceInput",
  name: "no-reference-input",
  violationId: NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID,
  forcedArtifact: "midgard-no-reference-input-forced-artifact-v1",
  forcedSourceSchema: SDK.NoReferenceInputForcedSourcePayloadSchema,
  sourceKind: 1n,
  fieldIndex: 1,
} as const satisfies InputExistenceKind;
export type NoReferenceInputForcedSource = InputExistenceForcedSource;
export type PreparedNoReferenceInputWrongfulRejection =
  PreparedInputExistenceWrongfulRejection;
export const {
  forcedSourceMaterial: noReferenceInputForcedSourceMaterial,
  prepare: prepareNoReferenceInputWrongfulRejection,
  detect: detectNoReferenceInputWrongfulRejections,
} = inputExistenceWrongfulRejection(NO_REFERENCE_INPUT_KIND);
