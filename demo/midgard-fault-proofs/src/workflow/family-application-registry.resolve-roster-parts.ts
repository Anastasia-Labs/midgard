import type { UTxO } from "@lucid-evolution/lucid";

import type { CompleteCanonicalReplayContext } from "./complete-replay.js";
import {
  type FamilyApplicationRequirement,
  type FamilyCommonInfrastructure,
  type FamilyResolvedReferenceScripts,
  type FamilyRosterDefinition,
  familyStepRole,
} from "./family-application.js";
import {
  familyStepContractNames,
  type FaultProofWitnessRole,
  type ManifestBoundFamilyWorkflow,
  type ManifestBoundFamilyWorkflowConfig,
} from "./family-definition.js";
import { type LinearFamilyCategory } from "./linear-family-spec.js";

export const requiredReference = (
  references: FamilyResolvedReferenceScripts,
  role: string,
): UTxO => {
  const reference = references[role];
  if (reference === undefined) {
    throw new Error(`family application roster omitted ${role}`);
  }
  return reference;
};

/**
 * The given roster roles resolved into a record keyed by those roles, for a
 * hand-written record whose config groups a fixed set of roles under one key.
 */
export const resolvedRoles = <Role extends string>(
  references: FamilyResolvedReferenceScripts,
  roles: readonly Role[],
): Readonly<Record<Role, UTxO>> =>
  Object.freeze(
    Object.fromEntries(
      roles.map((role) => [role, requiredReference(references, role)]),
    ) as Record<Role, UTxO>,
  );

/** The role names of a contract-name table, typed as its keys. */
export const rolesOf = <Table extends Readonly<Record<string, string>>>(
  table: Table,
): readonly (keyof Table & string)[] =>
  Object.keys(table) as (keyof Table & string)[];

/**
 * A hand-written record's step roster: `step01`… in the order of the family's
 * own step contract-name list.
 */
export const stepRoster = (
  contractNames: readonly string[],
): Readonly<Record<string, string>> =>
  Object.freeze(
    Object.fromEntries(
      contractNames.map((contractName, index) => [
        familyStepRole(index + 1),
        contractName,
      ]),
    ),
  );

/**
 * The resolved step references of a hand-written record, as the tuple its
 * family's config declares (a two- or four-step chain).
 */
export function resolvedSteps(
  references: FamilyResolvedReferenceScripts,
  count: 2,
): readonly [UTxO, UTxO];

export function resolvedSteps(
  references: FamilyResolvedReferenceScripts,
  count: 4,
): readonly [UTxO, UTxO, UTxO, UTxO];

export function resolvedSteps(
  references: FamilyResolvedReferenceScripts,
  count: 2 | 4,
): readonly UTxO[] {
  return Object.freeze(
    Array.from({ length: count }, (_, index) =>
      requiredReference(references, familyStepRole(index + 1)),
    ),
  );
}

/**
 * The predecessor replay context, laid into a family's config only when its
 * record requires it and the host supplied one. The shared loop refuses a
 * required context that is absent before `bindConfig` runs; here the field
 * stays optional so a reconciliation-only invocation, which is exempt from
 * the requirement, still binds.
 */
export const optionalReplayContext = (
  requires: readonly FamilyApplicationRequirement[],
  infrastructure: FamilyCommonInfrastructure,
): Readonly<{ replayContext?: CompleteCanonicalReplayContext }> =>
  requires.includes("replayContext") &&
  infrastructure.replayContext !== undefined
    ? { replayContext: infrastructure.replayContext }
    : {};

/**
 * The predecessor replay context whenever the host supplied one, for a family
 * that replays the predecessor only when the classifier admitted one and
 * proves from the challenged block alone otherwise.
 */
export const suppliedReplayContext = (
  infrastructure: FamilyCommonInfrastructure,
): Readonly<{ replayContext?: CompleteCanonicalReplayContext }> =>
  infrastructure.replayContext === undefined
    ? {}
    : { replayContext: infrastructure.replayContext };

/**
 * The infrastructure a cursor family requires.
 */
export type CursorFamilyRequirements = Readonly<{
  requires: readonly FamilyApplicationRequirement[];
}>;

/**
 * The config fields a cursor family reads from the infrastructure its record
 * requires: the replay context. The shared loop refuses a required part that
 * is absent before `bindConfig` runs, except on a reconciliation-only
 * invocation, which never reaches the evidence pipeline.
 */
export const requiredInfrastructureFields = (
  family: CursorFamilyRequirements,
): ((
  infrastructure: FamilyCommonInfrastructure,
) => Readonly<Record<string, unknown>>) => {
  return (infrastructure) =>
    Object.freeze(optionalReplayContext(family.requires, infrastructure));
};

/**
 * A definition's roster resolved into the parts a config lays out: the chain
 * steps in step order, the witness roles, the certificate when the definition
 * binds it, and the declared auxiliary reference scripts.
 */
export type ResolvedRosterParts = Readonly<{
  steps: readonly UTxO[];
  witnesses: Readonly<Record<string, UTxO>>;
  fieldPreimageCertificateMint?: UTxO;
  auxiliary: Readonly<Record<string, UTxO>>;
}>;

export const resolveRosterParts = (
  definition: FamilyRosterDefinition,
  references: FamilyResolvedReferenceScripts,
): ResolvedRosterParts =>
  Object.freeze({
    steps: Object.freeze(
      familyStepContractNames(definition).map((_, index) =>
        requiredReference(references, familyStepRole(index + 1)),
      ),
    ),
    witnesses: Object.freeze(
      Object.fromEntries(
        definition.witnessRoles.map((role) => [
          role,
          requiredReference(references, role),
        ]),
      ),
    ),
    ...(definition.fieldPreimageCertificate
      ? {
          fieldPreimageCertificateMint: requiredReference(
            references,
            "fieldPreimageCertificateMint",
          ),
        }
      : {}),
    auxiliary: Object.freeze(
      Object.fromEntries(
        Object.keys(definition.auxiliaryReferenceScripts ?? {}).map((role) => [
          role,
          requiredReference(references, role),
        ]),
      ),
    ),
  });

/**
 * The `{steps, witnesses, certificate}` bundle most cursor families read
 * their reference scripts from; a family with auxiliary scripts adds them
 * under its own key.
 */
export const referenceScriptBundle = ({
  steps,
  witnesses,
  fieldPreimageCertificateMint,
}: ResolvedRosterParts) =>
  Object.freeze({
    steps,
    witnesses,
    ...(fieldPreimageCertificateMint === undefined
      ? {}
      : { fieldPreimageCertificateMint }),
  });

/**
 * The linear families that may only execute against a classifier-admitted
 * predecessor replay context. Stated once, here, and read by both the
 * load-time and the decision-time check.
 */
export const LINEAR_FAMILY_REQUIREMENTS: Readonly<
  Partial<Record<LinearFamilyCategory, readonly FamilyApplicationRequirement[]>>
> = Object.freeze({
  nonExistentInput: Object.freeze(["replayContext"] as const),
  noReferenceInput: Object.freeze(["replayContext"] as const),
});

export type WidenedLinearConfig = ManifestBoundFamilyWorkflowConfig<
  LinearFamilyCategory,
  FaultProofWitnessRole,
  boolean
>;

export type WidenedLinearWorkflow = ManifestBoundFamilyWorkflow<
  LinearFamilyCategory,
  boolean
>;
