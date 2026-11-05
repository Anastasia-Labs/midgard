import { FAMILY_APPLICATION_REGISTRY } from "../../src/workflow/family-application-registry.js";
import type {
  FamilyCategory,
  FamilyDefinition,
} from "../../src/workflow/family-definition.js";

/**
 * The definition of every registered family the shared assembly builds, keyed
 * by category: each record the registry derives from a definition carries it.
 */
export const REGISTERED_FAMILY_DEFINITIONS: ReadonlyMap<
  FamilyCategory,
  FamilyDefinition<FamilyCategory>
> = new Map(
  Object.values(FAMILY_APPLICATION_REGISTRY).flatMap((record) =>
    record.definition === undefined
      ? []
      : [
          [
            record.category,
            record.definition as FamilyDefinition<FamilyCategory>,
          ] as const,
        ],
  ),
);
