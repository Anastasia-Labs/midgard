import { VALIDATION_TRACE_ONE_STEP_REFERENCE_SCRIPTS } from "../validation-trace-dispute/one-step-references.js";
import {
  createManifestBoundValidationTraceDisputeWorkflow,
  type ManifestBoundValidationTraceDisputeWorkflow,
  type ManifestBoundValidationTraceDisputeWorkflowConfig,
  runOrResumeManifestBoundValidationTraceDisputeWorkflow,
  VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS,
} from "../validation-trace-dispute/workflow.js";
import { defineFamilyApplication } from "./family-application.js";
import { commonBoundConfigFields } from "./family-application-registry.linear-family-application-record.js";
import {
  optionalReplayContext,
  resolvedRoles,
  rolesOf,
} from "./family-application-registry.resolve-roster-parts.js";

const WITNESS_ROLES = Object.freeze([
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const);

const REQUIRES = Object.freeze(["replayContext"] as const);

/**
 * The validation-trace dispute has no `FamilyDefinition`: its thread walks
 * the opener, the pending claim and the case rather than its published chain
 * step by step, and its case is played through the game, timeout and award
 * scripts. It re-executes the challenged block's scripts on the admitted
 * predecessor, so it requires the replay context. Its state-queue removal
 * spends the removal set through the deployment binding, so the roster
 * carries no removal roles. Its one-step resolution's holders and yields,
 * and the script-decode deployment a CEK script selection reads, are roster
 * entries named by their manifest members.
 */
export const VALIDATION_TRACE_DISPUTE_FAMILY_APPLICATION_RECORD =
  defineFamilyApplication<
    "validationTraceDispute",
    ManifestBoundValidationTraceDisputeWorkflowConfig,
    ManifestBoundValidationTraceDisputeWorkflow
  >({
    category: "validationTraceDispute",
    roster: Object.freeze({
      ...VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS,
      ...Object.fromEntries(WITNESS_ROLES.map((role) => [role, role])),
      ...VALIDATION_TRACE_ONE_STEP_REFERENCE_SCRIPTS,
    }),
    requires: REQUIRES,
    bindConfig: ({ infrastructure, references }) =>
      Object.freeze({
        ...commonBoundConfigFields(infrastructure),
        ...optionalReplayContext(REQUIRES, infrastructure),
        referenceScripts: Object.freeze({
          ...resolvedRoles(
            references,
            rolesOf(VALIDATION_TRACE_DISPUTE_REFERENCE_SCRIPTS),
          ),
          witnesses: resolvedRoles(references, WITNESS_ROLES),
          oneStep: resolvedRoles(
            references,
            rolesOf(VALIDATION_TRACE_ONE_STEP_REFERENCE_SCRIPTS),
          ),
        }),
      }),
    constructWorkflow: createManifestBoundValidationTraceDisputeWorkflow,
    execute: async ({ workflow, sources, journal }) =>
      await runOrResumeManifestBoundValidationTraceDisputeWorkflow({
        workflow,
        sources,
        journal,
      }),
    bindsDecisionDigest: false,
  });
