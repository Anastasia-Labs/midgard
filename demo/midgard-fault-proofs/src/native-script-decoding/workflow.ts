import * as SDK from "@al-ft/midgard-sdk";
import {
  FraudProofComputationThreadStepDatum,
  NativeScriptDecodingStep02DatumSchema,
  NativeScriptDecodingStep03AdvanceOrCloseDatumSchema,
  NativeScriptDecodingStep03BindDescriptorDatumSchema,
  NativeScriptDecodingStep03OpenSubjectDatumSchema,
  NativeScriptDecodingStep04DatumSchema,
} from "@al-ft/midgard-sdk";
import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { parseOutRef, type ResolvedProverSigner } from "../runtime.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../step-support.js";
import { submitInit } from "../submit-init.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import {
  type CompleteCanonicalReplayContext,
  NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
} from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { NATIVE_SCRIPT_DECODING_CURSOR_SPEC } from "../workflow/cursor-family-spec.js";
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
import {
  admitNativeScriptDecodingWorkflowArtifact,
  prepareNativeScriptDecodingWorkflowArtifact,
} from "./artifact.js";
import type { NativeScriptDecodingContracts } from "./contracts.js";
import {
  submitNativeScriptDecodingStep01BindNormal,
  submitNativeScriptDecodingStep01RecordForced,
} from "./submit-native-script-decoding-step-01.js";
import { submitNativeScriptDecodingStep02 } from "./submit-native-script-decoding-step-02.js";
import {
  submitNativeScriptDecodingStep03AdvanceOrCloseClose,
  submitNativeScriptDecodingStep03AdvanceOrCloseSegment,
  submitNativeScriptDecodingStep03BindDescriptor,
  submitNativeScriptDecodingStep03OpenSubject,
} from "./submit-native-script-decoding-step-03.js";
import { submitNativeScriptDecodingStep04 } from "./submit-native-script-decoding-step-04.js";
export type NativeScriptDecodingWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScripts>;
  fieldPreimageCertificateMint: UTxO;
}>;
type BoundConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"nativeScriptDecoding">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: NativeScriptDecodingContracts;
  references: NativeScriptDecodingWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;
type Prepared = Awaited<
  ReturnType<typeof admitNativeScriptDecodingWorkflowArtifact>
>;
const sourceInclusion = async (a: Prepared) => {
  if (a.coordinate.sourceKind !== 0 || !("validity" in a.material.compact))
    throw new Error("normal inclusion requires a normal source");
  const entry = a.current.transactions[a.coordinate.sourceIndex]!;
  return parseSubmitStep01TxInclusion({
    nativeTxId: a.txId,
    nativeTx: nativeTxFromCoreCompact(a.material.compact),
    nativeTxCompactCbor: a.material.proofSource.compactCbor.toString("hex"),
    l2TransactionSourceCbor: entry.valueBytes.toString("hex"),
    transactionsPhasRoot: a.current.rootData.transactions.phasRoot,
    txMembershipProofCbor: Data.to(
      await keyValuePhasProof(
        {
          ...a.current.rootData.transactions,
          root: a.current.rootData.transactions.phasRoot,
        },
        entry.keyBytes,
        entry.valueBytes,
      ),
      SDK.Proof,
    ),
  });
};
const subjectPlan = (a: Prepared, owner: string) =>
  planFaultProofFieldOpening({
    anchorSourceKind: a.coordinate.sourceKind === 1 ? 1n : 0n,
    fieldIndex: Number(a.coordinate.outpointSourceKind),
    anchorTxId: a.txId,
    nativeTxCompactCbor: a.material.proofSource.compactCbor.toString("hex"),
    itemCbors: a.subjectFieldInputs.map(SDK.encodeMidgardTxInputCanonical),
    owner,
    publish: true,
    label: "native-script-decoding subject",
  });
const namesField = (a: Prepared) =>
  (a.coordinate.outpointSourceKind === "0" ||
    a.coordinate.outpointSourceKind === "1") &&
  BigInt(a.coordinate.outpointCursor) >= 0n;
export const createNativeScriptDecodingTransactionPort = (
  config: BoundConfig,
): CursorFamilyTransactionPort<"nativeScriptDecoding"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "nativeScriptDecoding",
  prepare: async (args) =>
    await prepareNativeScriptDecodingWorkflowArtifact({
      ...args,
      replayContext: config.replayContext,
    }),
  capture: async ({ action, artifact }) => {
    const a = await admitNativeScriptDecodingWorkflowArtifact(artifact);
    if (a.current.headerHash !== config.binding.definition.headerHash)
      throw new Error("nativeScriptDecoding bound header changed");
    const input = cursorFamilyActionInput({
      category: "nativeScriptDecoding",
      action,
    });
    if (input.stage === "remove")
      return await captureCursorRemoval({
        category: "nativeScriptDecoding",
        lucid: config.lucid,
        blueprint: config.binding.blueprint,
        deploymentInfo: config.binding.deploymentInfo,
        network: config.binding.network,
        signer: config.signer,
        headerHash: a.current.headerHash,
        input,
        stateQueueMutationLeaseCoordinator:
          config.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          config.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      });
    return {
      transaction: await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          if (input.stage === "init") {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "nativeScriptDecoding",
              fraudulentBlockOutRef: cursorStringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: a.current.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
            return;
          }
          const threadOutRef = cursorStringField(input, "threadOutRef");
          const common = {
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.binding.resolvedContracts.category.categoryId,
            signer: config.signer,
            threadOutRef,
            preSubmitBoundary,
            awaitConfirmation: false,
          };
          switch (input.stage) {
            case "step_01":
              if (a.coordinate.sourceKind === 1)
                await submitNativeScriptDecodingStep01RecordForced({
                  ...common,
                  direction: a.direction,
                  referenceScriptUtxo: config.references.steps[0],
                });
              else {
                const inclusion = await sourceInclusion(a);
                await submitNativeScriptDecodingStep01BindNormal({
                  ...common,
                  blueprint: config.binding.blueprint,
                  network: config.binding.network,
                  stateQueueBlockOutRef: cursorStringField(
                    input,
                    "stateQueueBlockOutRef",
                  ),
                  txInclusion: inclusion,
                  publishedProofChunks: await resolvePublishedProofChunks({
                    lucid: config.lucid,
                    address: config.signer.address,
                    proofCbor: inclusion.txMembershipProofCbor,
                  }),
                  referenceScriptUtxo: config.references.steps[0],
                  witnessReferenceScripts: config.references.witnesses,
                });
              }
              return;
            case "step_02":
              await submitNativeScriptDecodingStep02({
                ...common,
                reconstruction: a.current,
                forcedOrderKey:
                  "ForcedTransactionEventKey" in a.eventKey
                    ? a.eventKey.ForcedTransactionEventKey.tx_order_id
                    : undefined,
                chosenOutpoint:
                  a.direction === 0n
                    ? {
                        sourceKind: BigInt(a.coordinate.outpointSourceKind),
                        cursor: BigInt(a.coordinate.outpointCursor),
                      }
                    : undefined,
                referenceScriptUtxo: config.references.steps[1],
              });
              return;
            case "step_03": {
              const planned = namesField(a)
                ? subjectPlan(a, config.signer.paymentKeyHash)
                : null;
              const published =
                planned === null
                  ? []
                  : await resolveFaultProofFieldCarriagePublications({
                      lucid: config.lucid,
                      planned,
                      publisherAddress: config.signer.address,
                    });
              const certificate =
                planned === null
                  ? undefined
                  : await resolveFaultProofFieldPreimageCertificate({
                      lucid: config.lucid,
                      planned,
                      certificatePolicyId:
                        config.contracts.fieldPreimageCertificatePolicyId,
                      network: config.binding.network,
                    });
              await submitNativeScriptDecodingStep03OpenSubject({
                ...common,
                nativeTxCompactCbor:
                  a.material.proofSource.compactCbor.toString("hex"),
                subjectFieldInputs: a.subjectFieldInputs,
                publishCarriage: true,
                publishedCarriageUtxos: published,
                certificateUtxo: certificate ?? undefined,
                referenceScriptUtxo: config.references.steps[2],
              });
              return;
            }
            case "step_04": {
              if (a.descriptorCbor === null || a.ledgerTrie === null)
                throw new Error(
                  "nativeScriptDecoding missing descriptor evidence",
                );
              const outpoint =
                a.subjectFieldInputs[Number(a.coordinate.outpointCursor)]!;
              await submitNativeScriptDecodingStep03BindDescriptor({
                ...common,
                outpointKeyCbor: Buffer.from(
                  SDK.encodeMidgardTxInputCanonical(outpoint),
                ).toString("hex"),
                descriptorCbor: a.descriptorCbor.toString("hex"),
                ledgerTrie: a.ledgerTrie,
                plan: a.plan ?? undefined,
                referenceScriptItemBytes:
                  a.referenceScriptItemBytes ?? undefined,
                referenceScriptUtxo: config.references.steps[3],
              });
              return;
            }
            case "step_05": {
              if (a.plan === null || a.referenceScriptItemBytes === null)
                throw new Error("nativeScriptDecoding missing scan evidence");
              const utxos = await config.lucid.utxosByOutRef([
                parseOutRef(threadOutRef, "nativeScriptDecoding thread"),
              ]);
              if (utxos.length !== 1 || !utxos[0]!.datum)
                throw new Error("nativeScriptDecoding scan thread absent");
              const datum = Data.from(
                utxos[0]!.datum,
                SDK.NativeScriptDecodingStep03AdvanceOrCloseDatum,
              );
              if (datum.data === null)
                throw new Error("nativeScriptDecoding absent scan datum");
              const hash = datum.data.machine_state_hash;
              const segment = a.plan.segments.find(
                (item) => item.controlBefore.hashHex === hash,
              );
              if (segment !== undefined)
                await submitNativeScriptDecodingStep03AdvanceOrCloseSegment({
                  ...common,
                  segment,
                  referenceScriptItemBytes: a.referenceScriptItemBytes,
                  referenceScriptUtxo: config.references.steps[4],
                });
              else {
                if (a.plan.verdict.control?.hashHex !== hash)
                  throw new Error(
                    "nativeScriptDecoding scan checkpoint differs from deterministic plan",
                  );
                await submitNativeScriptDecodingStep03AdvanceOrCloseClose({
                  ...common,
                  verdict: a.plan.verdict,
                  referenceScriptItemBytes: a.referenceScriptItemBytes,
                  referenceScriptUtxo: config.references.steps[4],
                });
              }
              return;
            }
            case "step_06":
              await submitNativeScriptDecodingStep04({
                ...common,
                referenceScriptUtxo: config.references.steps[5],
                witnessReferenceScripts: config.references.witnesses,
              });
              return;
            default:
              throw new Error(
                `nativeScriptDecoding unsupported stage ${input.stage}`,
              );
          }
        },
      ),
    };
  },
});
export type ManifestBoundNativeScriptDecodingWorkflowConfig = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NativeScriptDecodingWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundNativeScriptDecodingWorkflow = Readonly<{
  replayContext?: CompleteCanonicalReplayContext;
  binding: FraudProofWorkflowDeploymentBinding<"nativeScriptDecoding">;
  l1: FraudProofFamilyL1ObservationPort<"nativeScriptDecoding">;
  transactions: CursorFamilyTransactionPort<"nativeScriptDecoding">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const NATIVE_SCRIPT_DECODING_FAMILY_DEFINITION = defineFamily({
  category: "nativeScriptDecoding",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    NativeScriptDecodingStep02DatumSchema,
    NativeScriptDecodingStep03OpenSubjectDatumSchema,
    NativeScriptDecodingStep03BindDescriptorDatumSchema,
    NativeScriptDecodingStep03AdvanceOrCloseDatumSchema,
    NativeScriptDecodingStep04DatumSchema,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
    "chunkedVerifyWithdraw",
    "pexcludesWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: NATIVE_SCRIPT_DECODING_CURSOR_SPEC,
    stepContractNames: [
      "fraudProofNativeScriptDecoding",
      "fraudProofNativeScriptDecodingStep02",
      "fraudProofNativeScriptDecodingStep03OpenSubject",
      "fraudProofNativeScriptDecodingStep03BindDescriptor",
      "fraudProofNativeScriptDecodingStep03AdvanceOrClose",
      "fraudProofNativeScriptDecodingStep04",
    ],
    transactionPort: (context) => {
      const { binding, certificate } = context;
      const chain = binding.resolvedContracts.contracts.nativeScriptDecoding;
      const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
      if (
        chain === undefined ||
        stateQueuePolicyId === undefined ||
        certificate === null
      ) {
        throw new Error(
          "native-script-decoding manifest omitted required contracts",
        );
      }

      const contracts: NativeScriptDecodingContracts = Object.freeze({
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

      return createNativeScriptDecodingTransactionPort({
        ...context,
        contracts,
      });
    },
  },
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        const { certificate, references } = context;

        const input = cursorFamilyActionInput({
          category: "nativeScriptDecoding",
          action,
        });
        const admitted =
          await admitNativeScriptDecodingWorkflowArtifact(artifact);
        const planned =
          input.stage === "step_03" && namesField(admitted)
            ? subjectPlan(admitted, context.signer.paymentKeyHash)
            : null;
        if (planned === null) return null;
        return {
          planned,
          compactCbor:
            admitted.material.proofSource.compactCbor.toString("hex"),
          witnessSetCompactCbor:
            admitted.material.proofSource.witnessSetCompactCbor.toString("hex"),
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
    const admitted = await admitNativeScriptDecodingWorkflowArtifact(artifact);
    return admitted.coordinate.sourceKind === 0
      ? (await sourceInclusion(admitted)).txMembershipProofCbor
      : null;
  },
});

export const createManifestBoundNativeScriptDecodingWorkflow = (
  config: ManifestBoundNativeScriptDecodingWorkflowConfig,
): Promise<ManifestBoundNativeScriptDecodingWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    NATIVE_SCRIPT_DECODING_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundNativeScriptDecodingWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundNativeScriptDecodingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> =>
  await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: NATIVE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
    replayContext: workflow.replayContext,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["nativeScriptDecoding"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
