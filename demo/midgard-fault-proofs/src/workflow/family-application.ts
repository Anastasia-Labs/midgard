/**
 * Family application record: the per-family data a host needs to install and
 * run one fault family, declared once beside the family's constructor.
 *
 * A definition (`family-definition.ts`) says how a family's workflow is
 * assembled from an already-bound deployment. A record says how a host gets
 * there: which deployed contracts the family spends by reference, which
 * optional parts of the host's common infrastructure it requires, how the
 * resolved reference scripts are laid into the family's own manifest-bound
 * config, how the workflow is constructed and executed, and whether the
 * constructed workflow binds the admitted decision digest.
 *
 * The registry table holds the records; the runtime's runner table derives
 * one factory per registered record, and the watcher's fault-proof application
 * launches those families through that table.
 */

import "./actuation-permit.js";
import "./family-definition.js";
import "./family-application.assert-family-definition-roster.js";
import "./family-application.apply-family-application-record.js";
export {
  type AppliedFamilyApplication,
  applyFamilyApplicationRecord,
  type FamilyApplicationInvocation,
  type FamilyApplicationRosterRecord,
  type FamilyReferenceScriptResolver,
  type ResolvedFamilyApplicationReferences,
  resolveFamilyApplicationReferences,
} from "./family-application.apply-family-application-record.js";
export {
  assertFamilyDefinitionRoster,
  assertManifestBoundWorkflowIdentity,
  defineFamilyApplication,
  FAMILY_APPLICATION_RECORD,
  type FamilyApplicationRecord,
  type FamilyApplicationRequirement,
  type FamilyApplicationWorkflowIdentity,
  type FamilyCommonInfrastructure,
  familyDefinitionRoster,
  type FamilyHistoricalNativeScriptAuthority,
  type FamilyResolvedReferenceScripts,
  type FamilyRosterDefinition,
  familyStepRole,
  type FamilyValidationChallengePort,
  type LinearFamilyApplicationRecord,
} from "./family-application.assert-family-definition-roster.js";
