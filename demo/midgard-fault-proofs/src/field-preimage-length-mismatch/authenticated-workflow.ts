import { resolveFieldPreimageLengthCarriage } from "./carriage.js";
export {
  planFieldPreimageLengthCarriage,
  resolveFieldPreimageLengthCarriage,
} from "./carriage.js";
import {
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { FIELD_PREIMAGE_LENGTH_MISMATCH_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { releaseFinalityAuthorityFromDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { createFraudProofFamilyAuthenticatedL1TerminalVerifier } from "../workflow/family-l1-observation.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { executeManifestBoundFamilyRecovery } from "../workflow/manifest-bound-family-recovery.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowTerminalVerifier,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  createConcreteFieldPreimageLengthLucidBuilders,
  type LoadManifestBoundFieldPreimageLengthConfig,
  loadManifestBoundFieldPreimageLengthConfig,
  type ManifestBoundFieldPreimageLengthConfig,
  runManifestBoundFieldPreimageLengthWorkflow,
} from "./config.js";
import {
  type AuthenticatedFieldPreimageLengthEvidence,
  detectAuthenticatedFieldPreimageLengthEvidence,
} from "./evidence.js";
import { createFieldPreimageLengthRecoveryAdapter } from "./recovery.js";
import type {
  FieldPreimageLengthJournal,
  PreparedFieldPreimageLengthWorkflow,
} from "./workflow.js";

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

/** Installation factory: binds deployment/L1 authority and accepts no proof. */
export const createManifestBoundFieldPreimageLengthWorkflow = async (
  input: ManifestBoundFieldPreimageLengthWorkflowConfig,
): Promise<ManifestBoundFieldPreimageLengthWorkflow> => {
  const config = await loadManifestBoundFieldPreimageLengthConfig(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition,
  });
  const workflow = Object.freeze({
    workflowVersion: FIELD_PREIMAGE_LENGTH_AUTHENTICATED_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
  return Object.freeze({
    ...workflow,
    ...createFieldPreimageLengthRecoveryAdapter(workflow),
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority: releaseFinalityAuthorityFromDeploymentBinding(
      config.binding,
    ),
  });
};

export type FieldPreimageLengthJournalPort = Readonly<{
  load: () => Promise<FieldPreimageLengthJournal | null>;
  save: (journal: FieldPreimageLengthJournal) => Promise<void>;
  observeConfirmed: (
    action: "init" | "dispatch" | "authenticate" | "finalize" | "remove",
    transactionId: string,
  ) => Promise<boolean>;
  begin?: (
    action:
      | "init"
      | "dispatch"
      | "authenticate"
      | "finalize"
      | "remove"
      | "publication"
      | "certificate",
  ) => Promise<void>;
  boundary?: (
    action: "init" | "dispatch" | "authenticate" | "finalize" | "remove",
    prepared: PreparedFieldPreimageLengthWorkflow,
  ) => FraudProofPreSubmitBoundary;
  auxiliaryBoundary?: (
    kind: "publication" | "certificate",
  ) => FraudProofPreSubmitBoundary;
  auxiliaryConfirmed?: (
    kind: "publication" | "certificate",
    txHashes: readonly string[],
  ) => Promise<void>;
}>;

const runAuthenticatedFieldPreimageLengthWorkflow = async ({
  workflow,
  evidence,
  journal,
}: {
  readonly workflow: ManifestBoundFieldPreimageLengthWorkflow;
  readonly evidence: AuthenticatedFieldPreimageLengthEvidence;
  readonly journal: FieldPreimageLengthJournalPort;
}): Promise<FieldPreimageLengthJournal> => {
  const headerHash = workflow.config.binding.definition.headerHash;
  if (evidence.prepared.headerHash !== headerHash) {
    throw new Error("authenticated evidence targets a different bound header");
  }
  const persisted = await journal.load();
  if (
    persisted !== null &&
    persisted.prepared.evidenceDigest !== evidence.prepared.evidenceDigest
  ) {
    throw new Error(
      "persisted field-preimage-length evidence digest differs from authenticated retained DA",
    );
  }
  let current: FieldPreimageLengthJournal =
    persisted ??
    Object.freeze({
      prepared: evidence.prepared,
      confirmed: Object.freeze([]),
      transactionIds: Object.freeze({}),
    });
  const builders = createConcreteFieldPreimageLengthLucidBuilders({
    resolveStage: async ({ action, prepared }) => {
      const observed = await workflow.l1.observe({
        headerHash: workflow.binding.definition.headerHash,
      });
      if (observed.stage.kind === "removed") {
        throw new Error(
          "fieldPreimageLengthMismatch raw L1 reports an already removed block",
        );
      }
      const needsCarriage =
        (prepared.direction === "wrongfulAcceptance" &&
          (action === "dispatch" || action === "authenticate")) ||
        (prepared.direction === "wrongfulRejection" &&
          action === "authenticate");
      const carriage = needsCarriage
        ? await resolveFieldPreimageLengthCarriage({
            workflow,
            evidence,
            journal,
          })
        : undefined;
      await journal.begin?.(action);
      return {
        fraudulentBlockOutRef: observed.stage.stateQueueBlockOutRef,
        ...(observed.stage.kind === "step"
          ? {
              threadOutRef: observed.stage.threadOutRef,
              stateQueueBlockOutRef: observed.stage.stateQueueBlockOutRef,
            }
          : {}),
        ...evidence.stageEvidence,
        ...(carriage === undefined
          ? {}
          : prepared.direction === "wrongfulAcceptance"
            ? {
                acceptedClaimResolver: carriage.claimResolver,
                acceptedCarriageReferenceInputs: carriage.carriageReferences,
              }
            : {
                forcedClaimResolver: carriage.claimResolver,
                forcedCarriageReferenceInputs: carriage.carriageReferences,
              }),
      };
    },
    remove: async (context) => {
      await journal.begin?.("remove");
      return (
        await submitRemoveFraudulentBlock({
          lucid: context.config.lucid,
          blueprint: context.config.binding.blueprint,
          deploymentInfo: context.config.binding.deploymentInfo,
          network: context.config.binding.network,
          signer: context.config.signer,
          fraudCategory: "fieldPreimageLengthMismatch",
          fraudulentHeaderHash: context.config.binding.definition.headerHash,
          requireReferenceScripts: true,
          awaitConfirmation: true,
          stateQueueMutationLeaseCoordinator:
            workflow.stateQueueMutationLeaseCoordinator,
          preSubmitBoundary: context.preSubmitBoundary,
        })
      ).txHash;
    },
    boundary: journal.boundary,
  });
  current = await runManifestBoundFieldPreimageLengthWorkflow({
    config: workflow.config,
    builders,
    load: async () => current,
    save: async (next) => {
      current = next;
      await journal.save(next);
    },
    observeConfirmed: journal.observeConfirmed,
  });
  return current;
};

/**
 * Watcher-facing execution surface. The invocation cannot supply prepared
 * evidence. On first run it derives it from authenticated L1 plus public DA;
 * on resume the newly derived digest must equal the persisted journal digest.
 */
export const runOrResumeManifestBoundFieldPreimageLengthWorkflow =
  async (input: {
    readonly workflow: ManifestBoundFieldPreimageLengthWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FieldPreimageLengthJournalPort;
  }): Promise<FieldPreimageLengthJournal> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "fieldPreimageLengthMismatch runner rejects caller-authored evidence inputs",
      );
    }
    if (input.sources.length === 0) {
      throw new Error(
        "fieldPreimageLengthMismatch requires public retained DA",
      );
    }
    const headerHash = input.workflow.config.binding.definition.headerHash;
    const observation = await observeFraudProofWorkflowHeader(
      input.workflow.l1,
      { headerHash },
    );
    const evidence = await detectAuthenticatedFieldPreimageLengthEvidence({
      observation,
      sources: input.sources,
    });
    return await runAuthenticatedFieldPreimageLengthWorkflow({
      workflow: input.workflow,
      evidence,
      journal: input.journal,
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
