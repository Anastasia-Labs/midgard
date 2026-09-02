import {
  decodeMidgardNativeTxCompactV1,
  decodeMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import { isMidgardWitnessSetFieldV1 } from "@al-ft/midgard-sdk";

import {
  certifyFaultProofFieldCarriageV1,
  faultProofFieldCarriageV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import {
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../workflow/family-l1-observation-v1.js";
import type { FraudProofWorkflowJournalStoreV1 } from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import { createFieldPreimageLengthCentralJournalAdapterV1 } from "./central-journal-v1.js";
import { fieldPreimageLengthCommittedClaimV1 } from "./prepare-accepted-v1.js";
import {
  createConcreteFieldPreimageLengthLucidBuildersV1,
  type LoadManifestBoundFieldPreimageLengthConfigV1,
  loadManifestBoundFieldPreimageLengthConfigV1,
  type ManifestBoundFieldPreimageLengthConfigV1,
  runManifestBoundFieldPreimageLengthWorkflowV1,
} from "./production-config-v1.js";
import {
  type AuthenticatedFieldPreimageLengthProductionEvidenceV1,
  detectAuthenticatedFieldPreimageLengthProductionEvidenceV1,
} from "./production-evidence-v1.js";
import type {
  FieldPreimageLengthJournalV1,
  PreparedFieldPreimageLengthWorkflowV1,
} from "./workflow-v1.js";

export const FIELD_PREIMAGE_LENGTH_PRODUCTION_WORKFLOW_V1 =
  "midgard-field-preimage-length-mismatch-production-workflow-v1" as const;

/**
 * Evidence derived by the installed retained-DA authority. It contains no
 * caller verdict: direction, claim, source inclusion, forced reason and
 * membership are all authenticated outputs consumed by the real builders.
 */
export type ManifestBoundFieldPreimageLengthWorkflowConfigV1 =
  LoadManifestBoundFieldPreimageLengthConfigV1 &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundFieldPreimageLengthWorkflowV1 = Readonly<{
  workflowVersion: typeof FIELD_PREIMAGE_LENGTH_PRODUCTION_WORKFLOW_V1;
  config: ManifestBoundFieldPreimageLengthConfigV1;
  binding: ManifestBoundFieldPreimageLengthConfigV1["binding"];
  l1: FraudProofFamilyL1ObservationPortV1<"fieldPreimageLengthMismatch">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Installation factory: binds deployment/L1 authority and accepts no proof. */
export const createManifestBoundFieldPreimageLengthWorkflowV1 = async (
  input: ManifestBoundFieldPreimageLengthWorkflowConfigV1,
): Promise<ManifestBoundFieldPreimageLengthWorkflowV1> => {
  const config = await loadManifestBoundFieldPreimageLengthConfigV1(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition,
  });
  return Object.freeze({
    workflowVersion: FIELD_PREIMAGE_LENGTH_PRODUCTION_WORKFLOW_V1,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

export type FieldPreimageLengthProductionJournalPortV1 = Readonly<{
  load: () => Promise<FieldPreimageLengthJournalV1 | null>;
  save: (journal: FieldPreimageLengthJournalV1) => Promise<void>;
  observeConfirmed: (
    action: "init" | "dispatch" | "authenticate" | "finalize" | "remove",
    transactionId: string,
  ) => Promise<boolean>;
  boundary?: (
    action: "init" | "dispatch" | "authenticate" | "finalize" | "remove",
    prepared: PreparedFieldPreimageLengthWorkflowV1,
  ) => FraudProofPreSubmitBoundaryV1;
  auxiliaryBoundary?: (
    kind: "publication" | "certificate",
  ) => FraudProofPreSubmitBoundaryV1;
  auxiliaryConfirmed?: (
    kind: "publication" | "certificate",
    txHashes: readonly string[],
  ) => Promise<void>;
}>;

export const resolveFieldPreimageLengthProductionCarriageV1 = async ({
  workflow,
  evidence,
  journal,
}: {
  readonly workflow: ManifestBoundFieldPreimageLengthWorkflowV1;
  readonly evidence: AuthenticatedFieldPreimageLengthProductionEvidenceV1;
  readonly journal: FieldPreimageLengthProductionJournalPortV1;
}) => {
  const compact = decodeMidgardNativeTxCompactV1(
    Buffer.from(evidence.fieldMaterial.nativeTxCompactCbor, "hex"),
  );
  const witnessSet = decodeMidgardNativeTxWitnessSetCompactV1(
    Buffer.from(evidence.fieldMaterial.witnessSetCompactCbor, "hex"),
  );
  const witnessField = isMidgardWitnessSetFieldV1(evidence.prepared.fieldIndex);
  const planned = planFaultProofFieldOpeningV1({
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
  let publications = await resolveFaultProofFieldCarriagePublicationsV1({
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
    publications = await publishFaultProofFieldCarriageV1({
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
  let certificate = await resolveFaultProofFieldPreimageCertificateV1({
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
    const certified = await certifyFaultProofFieldCarriageV1({
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
    fieldPreimageLengthCommittedClaimV1({
      fieldIndex: evidence.prepared.fieldIndex,
      witnessSetCompactCbor: Buffer.from(
        evidence.fieldMaterial.witnessSetCompactCbor,
        "hex",
      ),
      carriage: faultProofFieldCarriageV1({
        planned,
        referenceInputs: completeReferenceInputs,
        certificatePolicyId:
          workflow.config.contracts.fieldPreimageCertificate.policyId,
        label: "fieldPreimageLengthMismatch authenticated field",
      }),
    });
  return Object.freeze({ carriageReferences, claimResolver });
};

const runAuthenticatedFieldPreimageLengthWorkflowV1 = async ({
  workflow,
  evidence,
  journal,
}: {
  readonly workflow: ManifestBoundFieldPreimageLengthWorkflowV1;
  readonly evidence: AuthenticatedFieldPreimageLengthProductionEvidenceV1;
  readonly journal: FieldPreimageLengthProductionJournalPortV1;
}): Promise<FieldPreimageLengthJournalV1> => {
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
  let current: FieldPreimageLengthJournalV1 =
    persisted ??
    Object.freeze({
      prepared: evidence.prepared,
      confirmed: Object.freeze([]),
      transactionIds: Object.freeze({}),
    });
  const builders = createConcreteFieldPreimageLengthLucidBuildersV1({
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
        ? await resolveFieldPreimageLengthProductionCarriageV1({
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
  current = await runManifestBoundFieldPreimageLengthWorkflowV1({
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
export const runOrResumeManifestBoundFieldPreimageLengthWorkflowV1 =
  async (input: {
    readonly workflow: ManifestBoundFieldPreimageLengthWorkflowV1;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FieldPreimageLengthProductionJournalPortV1;
  }): Promise<FieldPreimageLengthJournalV1> => {
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
    const evidence =
      await detectAuthenticatedFieldPreimageLengthProductionEvidenceV1({
        observation,
        sources: input.sources,
      });
    return await runAuthenticatedFieldPreimageLengthWorkflowV1({
      workflow: input.workflow,
      evidence,
      journal: input.journal,
    });
  };

/** Central-journal execute surface used by a ProductionWorkflowAdapterRunnerV1. */
export const executeManifestBoundFieldPreimageLengthWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundFieldPreimageLengthWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FieldPreimageLengthJournalV1> => {
  const headerHash = workflow.config.binding.definition.headerHash;
  const evidence =
    await detectAuthenticatedFieldPreimageLengthProductionEvidenceV1({
      observation: await workflow.l1.observeHeader({ headerHash }),
      sources,
    });
  const central = createFieldPreimageLengthCentralJournalAdapterV1({
    store: journal,
    deploymentFingerprint: workflow.config.binding.deploymentFingerprint,
    decisionDigest: workflow.decisionDigest,
    prepared: evidence.prepared,
    observeConfirmed: async (_action, txHash) =>
      await workflow.l1.transactionConfirmed({ headerHash, txHash }),
  });
  return await runAuthenticatedFieldPreimageLengthWorkflowV1({
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
export const FIELD_PREIMAGE_LENGTH_PRODUCTION_WORKFLOW_SURFACE_V1 =
  Object.freeze({
    workflowVersion: FIELD_PREIMAGE_LENGTH_PRODUCTION_WORKFLOW_V1,
    constructWorkflow: createManifestBoundFieldPreimageLengthWorkflowV1,
    execute: executeManifestBoundFieldPreimageLengthWorkflowV1,
  });

export type { AuthenticatedFieldPreimageLengthProductionEvidenceV1 } from "./production-evidence-v1.js";
