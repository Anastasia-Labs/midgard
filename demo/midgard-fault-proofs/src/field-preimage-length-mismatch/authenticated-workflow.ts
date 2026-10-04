import {
  FieldPreimageLengthStep01DatumSchema,
  FieldPreimageLengthStep02DatumSchema,
  FieldPreimageLengthStep03DatumSchema,
} from "@al-ft/midgard-sdk";

import {
  defineFamily,
  type FamilyAssemblyContext,
} from "../workflow/family-definition.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import { FIELD_PREIMAGE_LENGTH_CURSOR_SPEC } from "./workflow-spec.js";
export {
  planFieldPreimageLengthCarriage,
  resolveFieldPreimageLengthCarriage,
} from "./carriage.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { executeManifestBoundFamilyRecovery } from "../workflow/manifest-bound-family-recovery.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import {
  FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS,
  fieldPreimageLengthConfigFromBinding,
  type LoadManifestBoundFieldPreimageLengthConfig,
  type ManifestBoundFieldPreimageLengthConfig,
} from "./config.js";
import { createFieldPreimageLengthRecoveryPorts } from "./recovery.js";

export const FIELD_PREIMAGE_LENGTH_AUTHENTICATED_WORKFLOW =
  "midgard-field-preimage-length-mismatch-production-workflow-v1" as const;

/**
 * Evidence derived by the installed retained-DA authority. It contains no
 * caller verdict: direction, claim, source inclusion, forced reason and
 * membership are all authenticated outputs consumed by the real builders.
 */
export type ManifestBoundFieldPreimageLengthWorkflowConfig =
  LoadManifestBoundFieldPreimageLengthConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundFieldPreimageLengthWorkflow = Readonly<{
  workflowVersion: typeof FIELD_PREIMAGE_LENGTH_AUTHENTICATED_WORKFLOW;
  config: ManifestBoundFieldPreimageLengthConfig;
  binding: ManifestBoundFieldPreimageLengthConfig["binding"];
  l1: FraudProofFamilyL1ObservationPort<"fieldPreimageLengthMismatch">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;
type BoundContext = FamilyAssemblyContext<
  "fieldPreimageLengthMismatch",
  (typeof WITNESS_ROLES)[number],
  true,
  4
>;
type WorkflowExtension = Pick<
  ManifestBoundFieldPreimageLengthWorkflow,
  "workflowVersion" | "config" | "stateQueueMutationLeaseCoordinator"
>;
const bindFamily = (context: BoundContext) => {
  const config = fieldPreimageLengthConfigFromBinding({
    binding: context.binding,
    lucid: context.lucid,
    signer: context.signer,
    referenceScripts: {
      step01: context.references.steps[0],
      step02Accepted: context.references.steps[1],
      step02Forced: context.references.steps[2],
      step03: context.references.steps[3],
      witnesses: context.references.witnesses,
      fieldPreimageCertificateMint:
        context.references.fieldPreimageCertificateMint,
    },
  });
  const workflow = {
    workflowVersion: FIELD_PREIMAGE_LENGTH_AUTHENTICATED_WORKFLOW,
    config,
    binding: context.binding,
    l1: context.l1,
    stateQueueMutationLeaseCoordinator:
      context.stateQueueMutationLeaseCoordinator,
  };
  return { workflow, ports: createFieldPreimageLengthRecoveryPorts(workflow) };
};
const bound = new WeakMap<BoundContext, ReturnType<typeof bindFamily>>();
const boundFor = (context: BoundContext) => {
  const existing = bound.get(context);
  if (existing !== undefined) return existing;
  const created = bindFamily(context);
  bound.set(context, created);
  return created;
};
export const FIELD_PREIMAGE_LENGTH_FAMILY_DEFINITION = defineFamily<
  "fieldPreimageLengthMismatch",
  (typeof WITNESS_ROLES)[number],
  true,
  4
>({
  category: "fieldPreimageLengthMismatch",
  stepDatumSchemas: [
    FieldPreimageLengthStep01DatumSchema,
    FieldPreimageLengthStep02DatumSchema,
    FieldPreimageLengthStep02DatumSchema,
    FieldPreimageLengthStep03DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: FIELD_PREIMAGE_LENGTH_CURSOR_SPEC,
    stepContractNames: [
      FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS.step01,
      FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS.step02Accepted,
      FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS.step02Forced,
      FIELD_PREIMAGE_LENGTH_MANIFEST_CONTRACTS.step03,
    ],
    transactionPort: (context) => boundFor(context).ports.transactions,
  },
  fieldCarriage: [
    {
      requirementForAction: (context, input) =>
        boundFor(context).ports.requirementForAction(input),
    },
  ],
  extend: (context): WorkflowExtension => boundFor(context).workflow,
});

/** Installation factory: binds deployment/L1 authority and accepts no proof. */
export const createManifestBoundFieldPreimageLengthWorkflow = async (
  input: ManifestBoundFieldPreimageLengthWorkflowConfig,
): Promise<ManifestBoundFieldPreimageLengthWorkflow> => {
  const workflow = await assembleManifestBoundFamilyWorkflow(
    FIELD_PREIMAGE_LENGTH_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02Accepted,
          input.referenceScripts.step02Forced,
          input.referenceScripts.step03,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
      },
    },
  );
  return Object.freeze({
    ...(workflow as typeof workflow & WorkflowExtension),
    decisionDigest: input.decisionDigest,
  });
};

/** Central-journal execute surface used by a ProductionWorkflowAdapterRunnerV1. */
export const executeManifestBoundFieldPreimageLengthWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundFieldPreimageLengthWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    replayer: FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY,
  });

/** Stable construct/execute pair consumed by the compiled production runtime. */
export const FIELD_PREIMAGE_LENGTH_WORKFLOW_SURFACE = Object.freeze({
  workflowVersion: FIELD_PREIMAGE_LENGTH_AUTHENTICATED_WORKFLOW,
  constructWorkflow: createManifestBoundFieldPreimageLengthWorkflow,
  execute: executeManifestBoundFieldPreimageLengthWorkflow,
});

export type { AuthenticatedFieldPreimageLengthEvidence } from "./evidence.js";
