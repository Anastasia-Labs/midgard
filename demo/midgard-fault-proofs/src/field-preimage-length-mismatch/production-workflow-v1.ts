import {
  decodeMidgardNativeTxCompact,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { isMidgardWitnessSetField } from "@al-ft/midgard-sdk";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldCarriage,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening-v1.js";
import {
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation-v1.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary-v1.js";
import { createFieldPreimageLengthCentralJournalAdapter } from "./central-journal-v1.js";
import { fieldPreimageLengthCommittedClaim } from "./prepare-accepted-v1.js";
import {
  createConcreteFieldPreimageLengthLucidBuilders,
  type LoadManifestBoundFieldPreimageLengthConfig,
  loadManifestBoundFieldPreimageLengthConfig,
  type ManifestBoundFieldPreimageLengthConfig,
  runManifestBoundFieldPreimageLengthWorkflow,
} from "./production-config-v1.js";
import {
  type AuthenticatedFieldPreimageLengthEvidence,
  detectAuthenticatedFieldPreimageLengthEvidence,
} from "./production-evidence-v1.js";
import type {
  FieldPreimageLengthJournal,
  PreparedFieldPreimageLengthWorkflow,
} from "./workflow-v1.js";

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
  return Object.freeze({
    workflowVersion: FIELD_PREIMAGE_LENGTH_AUTHENTICATED_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

export type FieldPreimageLengthJournalPort = Readonly<{
  load: () => Promise<FieldPreimageLengthJournal | null>;
  save: (journal: FieldPreimageLengthJournal) => Promise<void>;
  observeConfirmed: (
    action: "init" | "dispatch" | "authenticate" | "finalize" | "remove",
    transactionId: string,
  ) => Promise<boolean>;
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

export const resolveFieldPreimageLengthCarriage = async ({
  workflow,
  evidence,
  journal,
}: {
  readonly workflow: ManifestBoundFieldPreimageLengthWorkflow;
  readonly evidence: AuthenticatedFieldPreimageLengthEvidence;
  readonly journal: FieldPreimageLengthJournalPort;
}) => {
  const compact = decodeMidgardNativeTxCompact(
    Buffer.from(evidence.fieldMaterial.nativeTxCompactCbor, "hex"),
  );
  const witnessSet = decodeMidgardNativeTxWitnessSetCompact(
    Buffer.from(evidence.fieldMaterial.witnessSetCompactCbor, "hex"),
  );
  const witnessField = isMidgardWitnessSetField(evidence.prepared.fieldIndex);
  const planned = planFaultProofFieldOpening({
    fieldIndex: evidence.prepared.fieldIndex,
    anchorTxId: evidence.prepared.transactionId,
    nativeTxCompactCbor: evidence.fieldMaterial.nativeTxCompactCbor,
    itemCbors: evidence.fieldMaterial.itemCbors.map((item) =>
      Buffer.from(item, "hex"),
    ),
    owner: workflow.config.signer.paymentKeyHash,
    ...(witnessField
      ? {
          witnessSet: {
            addr_tx_wits_hash: witnessSet.addrTxWitsHash.toString("hex"),
            script_tx_wits_hash: witnessSet.scriptTxWitsHash.toString("hex"),
            redeemer_tx_wits_hash:
              witnessSet.redeemerTxWitsHash.toString("hex"),
          },
          anchorWitnessSetHash:
            compact.transactionWitnessSetHash.toString("hex"),
        }
      : {}),
    label: "fieldPreimageLengthMismatch authenticated field",
  });
  let publications = await resolveFaultProofFieldCarriagePublications({
    lucid: workflow.config.lucid,
    publisherAddress: workflow.config.signer.address,
    planned,
  });
  if (publications === undefined) {
    if (journal.auxiliaryBoundary === undefined) {
      throw new Error(
        "fieldPreimageLengthMismatch non-inline carriage requires a durable production journal",
      );
    }
    publications = await publishFaultProofFieldCarriage({
      lucid: workflow.config.lucid,
      signer: workflow.config.signer,
      planned,
      publisherAddress: workflow.config.signer.address,
      label: "fieldPreimageLengthMismatch authenticated field",
      preSubmitBoundary: journal.auxiliaryBoundary("publication"),
    });
  }
  await journal.auxiliaryConfirmed?.(
    "publication",
    publications.map(({ txHash }) => txHash),
  );
  let certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: workflow.config.lucid,
    network: workflow.config.binding.network,
    planned,
    certificatePolicyId:
      workflow.config.contracts.fieldPreimageCertificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    if (journal.auxiliaryBoundary === undefined) {
      throw new Error(
        "fieldPreimageLengthMismatch certification requires a durable production journal",
      );
    }
    const certified = await certifyFaultProofFieldCarriage({
      lucid: workflow.config.lucid,
      network: workflow.config.binding.network,
      signer: workflow.config.signer,
      planned,
      certificatePolicyId:
        workflow.config.contracts.fieldPreimageCertificate.policyId,
      certificateMintingScript:
        workflow.config.contracts.fieldPreimageCertificate.mintingScript,
      certificateReferenceScriptUtxo:
        workflow.config.referenceScripts.fieldPreimageCertificateMint,
      chunkUtxos: publications,
      compactCbor: evidence.fieldMaterial.nativeTxCompactCbor,
      witnessSetCompactCbor: evidence.fieldMaterial.witnessSetCompactCbor,
      preSubmitBoundary: journal.auxiliaryBoundary("certificate"),
    });
    certificate = certified.certificateUtxo;
  }
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error(
      "fieldPreimageLengthMismatch field certificate disappeared",
    );
  }
  if (certificate !== undefined) {
    await journal.auxiliaryConfirmed?.("certificate", [certificate.txHash]);
  }
  const carriageReferences = [
    ...publications,
    ...(certificate === undefined ? [] : [certificate]),
  ];
  const claimResolver = (
    completeReferenceInputs: readonly (typeof carriageReferences)[number][],
  ) =>
    fieldPreimageLengthCommittedClaim({
      fieldIndex: evidence.prepared.fieldIndex,
      witnessSetCompactCbor: Buffer.from(
        evidence.fieldMaterial.witnessSetCompactCbor,
        "hex",
      ),
      carriage: faultProofFieldCarriage({
        planned,
        referenceInputs: completeReferenceInputs,
        certificatePolicyId:
          workflow.config.contracts.fieldPreimageCertificate.policyId,
        label: "fieldPreimageLengthMismatch authenticated field",
      }),
    });
  return Object.freeze({ carriageReferences, claimResolver });
};

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
    remove: async (context) =>
      (
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
      ).txHash,
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
    const observation = await input.workflow.l1.observeHeader({ headerHash });
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
}): Promise<FieldPreimageLengthJournal> => {
  const headerHash = workflow.config.binding.definition.headerHash;
  const evidence = await detectAuthenticatedFieldPreimageLengthEvidence({
    observation: await workflow.l1.observeHeader({ headerHash }),
    sources,
  });
  const central = createFieldPreimageLengthCentralJournalAdapter({
    store: journal,
    deploymentFingerprint: workflow.config.binding.deploymentFingerprint,
    decisionDigest: workflow.decisionDigest,
    prepared: evidence.prepared,
    observeConfirmed: async (_action, txHash) =>
      await workflow.l1.transactionConfirmed({ headerHash, txHash }),
  });
  return await runAuthenticatedFieldPreimageLengthWorkflow({
    workflow,
    evidence,
    journal: {
      ...central.journal,
      boundary: central.boundary,
      auxiliaryBoundary: central.auxiliaryBoundary,
      auxiliaryConfirmed: central.auxiliaryConfirmed,
    },
  });
};

/** Stable construct/execute pair consumed by the compiled production runtime. */
export const FIELD_PREIMAGE_LENGTH_WORKFLOW_SURFACE = Object.freeze({
  workflowVersion: FIELD_PREIMAGE_LENGTH_AUTHENTICATED_WORKFLOW,
  constructWorkflow: createManifestBoundFieldPreimageLengthWorkflow,
  execute: executeManifestBoundFieldPreimageLengthWorkflow,
});

export type { AuthenticatedFieldPreimageLengthEvidence } from "./production-evidence-v1.js";
