import {
  decodeMidgardAddressWitnessItem,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import {
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX,
  NativeScriptInvalidStep02DatumSchema,
  NativeScriptInvalidStep03DatumSchema,
  NativeScriptInvalidStep04DatumSchema,
  NativeScriptInvalidStep05DatumSchema,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { parseSubmitStep01TxInclusion } from "../step-support.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import { NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import { defineFamily } from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import type { NativeScriptInvalidContracts } from "./contracts.js";
import { nativeScriptInvalidUsesDirectRoute } from "./evidence-machine.js";
import { submitNativeScriptInvalidInit } from "./submit-init.js";
import { submitNativeScriptInvalidStep01 } from "./submit-step-01.js";
import { submitNativeScriptInvalidStep01Forced } from "./submit-step-01-forced.js";
import { submitNativeScriptInvalidStep02 } from "./submit-step-02.js";
import { submitNativeScriptInvalidStep03 } from "./submit-step-03.js";
import { submitNativeScriptInvalidStep03StartSignerScan } from "./submit-step-03-staged.js";
import { submitNativeScriptInvalidStep04 } from "./submit-step-04.js";
import { submitNativeScriptInvalidStep05 } from "./submit-step-05.js";
import {
  admitNativeScriptInvalidWorkflowArtifact,
  prepareNativeScriptInvalidWorkflowArtifact,
} from "./workflow-artifact.js";
import { NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "./workflow-spec.js";

export type NativeScriptInvalidWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"nativeScriptInvalid">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: NativeScriptInvalidContracts;
  references: NativeScriptInvalidWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const buffers = (values: readonly string[]): readonly Uint8Array[] =>
  values.map((value) => Buffer.from(value, "hex"));

const witnessSet = (
  admitted: Awaited<
    ReturnType<typeof admitNativeScriptInvalidWorkflowArtifact>
  >,
) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    (admitted.forced !== undefined
      ? decodeMidgardForcedTxFullFromCanonicalCbor
      : decodeMidgardNativeTxFullFromCanonicalCbor)(
      Buffer.from(admitted.prepared.nativeTxCanonicalCbor, "hex"),
    ).witnessSet,
  );
  return {
    addr_tx_wits_hash: Buffer.from(compact.addrTxWitsHash).toString("hex"),
    script_tx_wits_hash: Buffer.from(compact.scriptTxWitsHash).toString("hex"),
    redeemer_tx_wits_hash: Buffer.from(compact.redeemerTxWitsHash).toString(
      "hex",
    ),
  };
};

const scriptFieldPlan = (
  admitted: Awaited<
    ReturnType<typeof admitNativeScriptInvalidWorkflowArtifact>
  >,
  owner: string,
) =>
  planFaultProofFieldOpening({
    anchorSourceKind: admitted.forced !== undefined ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: buffers(admitted.prepared.scriptWitnessItemCbors),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    anchorWitnessSetHash: admitted.witnessSetHash,
    label: "native-script-invalid field 6",
  });

const signerFieldPlan = (
  admitted: Awaited<
    ReturnType<typeof admitNativeScriptInvalidWorkflowArtifact>
  >,
  owner: string,
) =>
  planFaultProofFieldOpening({
    anchorSourceKind: admitted.forced !== undefined ? 1n : 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: buffers(admitted.prepared.addrWitnessItemCbors),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    anchorWitnessSetHash: admitted.witnessSetHash,
    label: "native-script-invalid field 7",
  });

const isDirect = (
  admitted: Awaited<
    ReturnType<typeof admitNativeScriptInvalidWorkflowArtifact>
  >,
): boolean =>
  nativeScriptInvalidUsesDirectRoute({
    signerCount: admitted.prepared.addrWitnessItemCbors.length,
    scriptBytes: decodeMidgardVersionedScript(
      Buffer.from(admitted.prepared.scriptItemCbor, "hex"),
    ).scriptBytes.length,
  });

const resolveField = async ({
  config,
  plan,
}: {
  readonly config: BoundConfig;
  readonly plan: ReturnType<typeof scriptFieldPlan>;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("native-script-invalid field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.binding.network,
    planned: plan,
    certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
  });
  if (plan.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("native-script-invalid field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
};

const transactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"nativeScriptInvalid"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "nativeScriptInvalid",
  prepare: async ({ evidence, classification }) =>
    await prepareNativeScriptInvalidWorkflowArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitNativeScriptInvalidWorkflowArtifact(artifact);
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error(
        "native-script-invalid artifact changed the bound header",
      );
    }
    const input = cursorFamilyActionInput({
      category: "nativeScriptInvalid",
      action,
    });
    const stage = input.stage;
    const threadOutRef = () => cursorStringField(input, "threadOutRef");
    const categoryId = config.binding.resolvedContracts.category.categoryId;
    const common = {
      lucid: config.lucid,
      contracts: config.contracts,
      categoryId,
      signer: config.signer,
      witnessSet: witnessSet(admitted),
      nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
      awaitConfirmation: false,
    } as const;
    if (stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudulentBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: admitted.artifact.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (stage === "step_01" && admitted.forced !== undefined) {
      const forced = admitted.forced;
      return {
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep01Forced({
              ...common,
              threadOutRef: threadOutRef(),
              state: forced.evidence.state,
              forcedSource: forced.forcedSource,
              referenceScriptUtxo: config.references.steps[0],
              preSubmitBoundary,
            });
          },
        ),
      };
    }
    if (stage === "step_01") {
      const txInclusion = admitted.prepared.txInclusion;
      if (txInclusion === undefined)
        throw new Error("native-script-invalid: accepted inclusion is absent");
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: txInclusion.txMembershipProofCbor,
      });
      if (chunks === undefined) {
        throw new Error("native-script-invalid transaction proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: parseSubmitStep01TxInclusion(txInclusion),
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (stage === "step_02") {
      const carriage = await resolveField({
        config,
        plan: scriptFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep02({
              ...common,
              threadOutRef: threadOutRef(),
              scriptWitnessItems: buffers(
                admitted.prepared.scriptWitnessItemCbors,
              ),
              scriptIndex: admitted.prepared.scriptIndex,
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[1],
              preSubmitBoundary,
            });
          },
        ),
      });
    }
    if (stage === "step_03") {
      const carriage = await resolveField({
        config,
        plan: signerFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      const direct = isDirect(admitted);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            const args = {
              ...common,
              threadOutRef: threadOutRef(),
              scriptItemCbor: Buffer.from(
                admitted.prepared.scriptItemCbor,
                "hex",
              ),
              addressWitnessItems: buffers(
                admitted.prepared.addrWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[2],
              preSubmitBoundary,
            } as const;
            if (direct) {
              await submitNativeScriptInvalidStep03({
                ...args,
                addressWitnessVerificationKeys:
                  admitted.prepared.addrWitnessItemCbors.map(
                    (item) =>
                      decodeMidgardAddressWitnessItem(Buffer.from(item, "hex"))
                        .verificationKey,
                  ),
                witnessReferenceScripts: config.references.witnesses,
              });
            } else {
              await submitNativeScriptInvalidStep03StartSignerScan(args);
            }
          },
        ),
      });
    }
    if (stage === "step_04") {
      const carriage = await resolveField({
        config,
        plan: signerFieldPlan(admitted, config.signer.paymentKeyHash),
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep04({
              ...common,
              threadOutRef: threadOutRef(),
              addressWitnessItems: buffers(
                admitted.prepared.addrWitnessItemCbors,
              ),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.references.steps[3],
              preSubmitBoundary,
            });
          },
        ),
      });
    }
    if (stage === "step_05") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep05({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              scriptItemCbor: Buffer.from(
                admitted.prepared.scriptItemCbor,
                "hex",
              ),
              addressWitnessItems: buffers(
                admitted.prepared.addrWitnessItemCbors,
              ),
              referenceScriptUtxo: config.references.steps[4],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (stage === "remove") {
      return await captureCursorRemoval({
        category: "nativeScriptInvalid",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: admitted.artifact.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    }
    throw new Error(`native-script-invalid unsupported stage ${stage}`);
  },
});

export type ManifestBoundNativeScriptInvalidWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NativeScriptInvalidWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundNativeScriptInvalidWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"nativeScriptInvalid">;
  l1: FraudProofFamilyL1ObservationPort<"nativeScriptInvalid">;
  transactions: CursorFamilyTransactionPort<"nativeScriptInvalid">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION = defineFamily({
  category: "nativeScriptInvalid",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    NativeScriptInvalidStep02DatumSchema,
    NativeScriptInvalidStep03DatumSchema,
    NativeScriptInvalidStep04DatumSchema,
    NativeScriptInvalidStep05DatumSchema,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "chunkedVerifyWithdraw",
    "pexcludesWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofNativeScriptInvalid",
      "fraudProofNativeScriptInvalidStep02",
      "fraudProofNativeScriptInvalidStep03",
      "fraudProofNativeScriptInvalidStep04",
      "fraudProofNativeScriptInvalidStep05",
    ],
    transactionPort: (context) => {
      const { binding, certificate } = context;
      const chain = binding.resolvedContracts.contracts.nativeScriptInvalid;
      const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
      if (
        chain === undefined ||
        stateQueuePolicyId === undefined ||
        certificate === null
      ) {
        throw new Error(
          "native-script-invalid manifest omitted required contracts",
        );
      }

      const contracts: NativeScriptInvalidContracts = Object.freeze({
        steps: chain.steps,
        computationThread:
          binding.resolvedContracts.contracts.computationThread,
        fraudProof: {
          policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
          mintingScript:
            binding.resolvedContracts.contracts.fraudProof.mintingScript,
          spendingScriptAddress:
            binding.resolvedContracts.contracts.fraudProof
              .spendingScriptAddress,
        },
        hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
        stateQueuePolicyId,
        fieldPreimageCertificatePolicyId: certificate.policyId,
      });

      return transactionPort({ ...context, contracts });
    },
  },
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const { certificate, references } = context;

        const input = cursorFamilyActionInput({
          category: "nativeScriptInvalid",
          action,
        });
        const admitted =
          await admitNativeScriptInvalidWorkflowArtifact(artifact);
        const planned =
          input.stage === "step_02"
            ? scriptFieldPlan(admitted, context.signer.paymentKeyHash)
            : input.stage === "step_03" || input.stage === "step_04"
              ? signerFieldPlan(admitted, context.signer.paymentKeyHash)
              : null;
        if (planned === null) return null;
        return {
          planned,
          compactCbor: admitted.prepared.nativeTxCompactCbor,
          certificate: {
            policyId: certificate.policyId,
            mintingScript: certificate.mintingScript,
            referenceScriptUtxo: references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
  proofChunk: async (_context, { action, artifact }) => {
    if (action.input.stage !== "step_01") return null;
    const admitted = await admitNativeScriptInvalidWorkflowArtifact(artifact);
    return admitted.prepared.txInclusion?.txMembershipProofCbor ?? null;
  },
});

export const createManifestBoundNativeScriptInvalidWorkflow = (
  config: ManifestBoundNativeScriptInvalidWorkflowConfig,
): Promise<ManifestBoundNativeScriptInvalidWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    NATIVE_SCRIPT_INVALID_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundNativeScriptInvalidWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundNativeScriptInvalidWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["nativeScriptInvalid"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
