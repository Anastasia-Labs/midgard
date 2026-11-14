/** The application record of a direct walk family, derived from its walk. */
import { bundleCursorFamilyApplicationRecord } from "../workflow/family-application-registry.bundle-cursor-family-application-record.js";
import type {
  CHUNKED_VERIFY_WITNESS_ROLES,
  FamilyCategory,
  FamilyDefinition,
} from "../workflow/family-definition.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import type {
  DirectWalkWorkflow,
  DirectWalkWorkflowConfig,
} from "./walk-family.js";

/** A direct walk family's record: it needs the replay context and reads its
 * reference scripts from the shared bundle; its workflow is assembled from
 * the walk's definition and run by the shared runner. */
export const directWalkApplicationRecord = <
  Category extends FamilyCategory,
  StepCount extends number,
>(walk: {
  readonly definition: FamilyDefinition<
    Category,
    (typeof CHUNKED_VERIFY_WITNESS_ROLES)[number],
    true,
    StepCount
  >;
}) =>
  bundleCursorFamilyApplicationRecord<
    Category,
    DirectWalkWorkflowConfig<Category, StepCount>,
    DirectWalkWorkflow<Category, StepCount>
  >(walk.definition, {
    requires: ["replayContext"],
    constructWorkflow: (config) =>
      assembleManifestBoundFamilyWorkflow(walk.definition, config),
    execute: runOrResumeManifestBoundFamilyWorkflow,
  });
