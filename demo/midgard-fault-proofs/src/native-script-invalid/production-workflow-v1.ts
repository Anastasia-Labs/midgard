import {
  decodeMidgardAddressWitnessItem,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
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
} from "../field-opening-v1.js";
import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { parseSubmitStep01TxInclusion } from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import { NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation-v1.js";
import type { FraudProofWorkflowJournalStore } from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator-v1.js";
import {
  createCursorFamilyWorkflowAdapter,
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/production-cursor-family-adapter-v1.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/production-cursor-family-runtime-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "../workflow/production-field-carriage-prerequisite-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  withProofChunkPrerequisite,
} from "../workflow/production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthority } from "../workflow/release-finality-policy-v1.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary-v1.js";
import type { NativeScriptInvalidContracts } from "./contracts-v1.js";
import { nativeScriptInvalidUsesDirectRoute } from "./evidence-machine-v1.js";
import {
  admitNativeScriptInvalidArtifact,
  prepareNativeScriptInvalidArtifact,
} from "./production-artifact-v1.js";
import { submitNativeScriptInvalidInit } from "./submit-init-v1.js";
import { submitNativeScriptInvalidStep01 } from "./submit-step-01-v1.js";
import { submitNativeScriptInvalidStep02 } from "./submit-step-02-v1.js";
import { submitNativeScriptInvalidStep03StartSignerScan } from "./submit-step-03-staged-v1.js";
import { submitNativeScriptInvalidStep03 } from "./submit-step-03-v1.js";
import { submitNativeScriptInvalidStep04 } from "./submit-step-04-v1.js";
import { submitNativeScriptInvalidStep05 } from "./submit-step-05-v1.js";
import { NATIVE_SCRIPT_INVALID_CURSOR_SPEC } from "./workflow-spec-v1.js";

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
  admitted: ReturnType<typeof admitNativeScriptInvalidArtifact>,
) => {
  const compact = deriveMidgardNativeTxWitnessSetCompact(
    decodeMidgardNativeTxFullFromCanonicalCbor(
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
  admitted: ReturnType<typeof admitNativeScriptInvalidArtifact>,
  owner: string,
) =>
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.scriptWitnesses,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: buffers(admitted.prepared.scriptWitnessItemCbors),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    anchorWitnessSetHash:
      admitted.prepared.txInclusion.nativeTx.witness_set_hash,
    label: "native-script-invalid field 6",
  });

const signerFieldPlan = (
  admitted: ReturnType<typeof admitNativeScriptInvalidArtifact>,
  owner: string,
) =>
  planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: admitted.prepared.badTxId,
    nativeTxCompactCbor: admitted.prepared.nativeTxCompactCbor,
    itemCbors: buffers(admitted.prepared.addrWitnessItemCbors),
    owner,
    publish: true,
    witnessSet: witnessSet(admitted),
    anchorWitnessSetHash:
      admitted.prepared.txInclusion.nativeTx.witness_set_hash,
    label: "native-script-invalid field 7",
  });

const isDirect = (
  admitted: ReturnType<typeof admitNativeScriptInvalidArtifact>,
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
    await prepareNativeScriptInvalidArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitNativeScriptInvalidArtifact(artifact);
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
    if (stage === "step_01") {
      const chunks = await resolvePublishedProofChunks({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.txInclusion.txMembershipProofCbor,
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
              txInclusion: parseSubmitStep01TxInclusion(
                admitted.prepared.txInclusion,
              ),
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

export const createManifestBoundNativeScriptInvalidWorkflow = async (
  config: ManifestBoundNativeScriptInvalidWorkflowConfig,
): Promise<ManifestBoundNativeScriptInvalidWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "nativeScriptInvalid",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      NativeScriptInvalidStep02DatumSchema,
      NativeScriptInvalidStep03DatumSchema,
      NativeScriptInvalidStep04DatumSchema,
      NativeScriptInvalidStep05DatumSchema,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.nativeScriptInvalid;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  const certificate = binding.fieldPreimageCertificate;
  if (
    chain === undefined ||
    stateQueuePolicyId === undefined ||
    certificate === null
  ) {
    throw new Error(
      "native-script-invalid manifest omitted required contracts",
    );
  }
  const stepNames = [
    "fraudProofNativeScriptInvalid",
    "fraudProofNativeScriptInvalidStep02",
    "fraudProofNativeScriptInvalidStep03",
    "fraudProofNativeScriptInvalidStep04",
    "fraudProofNativeScriptInvalidStep05",
  ] as const;
  const steps = stepNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as NativeScriptInvalidWorkflowReferenceScripts["steps"];
  const witness = <Name extends keyof FaultProofWitnessReferenceScripts>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name],
    });
  const references: NativeScriptInvalidWorkflowReferenceScripts = Object.freeze(
    {
      steps: Object.freeze(steps),
      witnesses: Object.freeze({
        computationThreadMint: witness(
          "computationThreadMint",
          "computationThreadMint",
        ),
        fraudProofMint: witness("fraudProofMint", "fraudProofMint"),
        phasMembershipWithdraw: witness(
          "phasMembershipWithdraw",
          "phasMembershipWithdraw",
        ),
        chunkedVerifyWithdraw: witness(
          "chunkedVerifyWithdraw",
          "chunkedVerifyWithdraw",
        ),
        pexcludesWithdraw: witness("pexcludesWithdraw", "pexcludesWithdraw"),
      }),
      fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fieldPreimageCertificateMint",
        utxo: config.referenceScripts.fieldPreimageCertificateMint,
      }),
    },
  );
  const contracts: NativeScriptInvalidContracts = Object.freeze({
    steps: chain.steps,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined) {
    throw new Error("native-script-invalid raw L1 authority is unavailable");
  }
  const bound: BoundConfig = {
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  };
  const transactions = transactionPort(bound);
  let adapter = createCursorFamilyWorkflowAdapter({
    spec: NATIVE_SCRIPT_INVALID_CURSOR_SPEC,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
    category: "nativeScriptInvalid",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      const input = cursorFamilyActionInput({
        category: "nativeScriptInvalid",
        action,
      });
      const admitted = admitNativeScriptInvalidArtifact(artifact);
      const planned =
        input.stage === "step_02"
          ? scriptFieldPlan(admitted, config.signer.paymentKeyHash)
          : input.stage === "step_03" || input.stage === "step_04"
            ? signerFieldPlan(admitted, config.signer.paymentKeyHash)
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
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withFieldCarriagePrerequisite({
    category: "nativeScriptInvalid",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const txProofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "nativeScriptInvalid",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) =>
      action.input.stage === "step_01"
        ? admitNativeScriptInvalidArtifact(artifact).prepared.txInclusion
            .txMembershipProofCbor
        : null,
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProofChunkPrerequisite({
    category: "nativeScriptInvalid",
    base: adapter,
    prerequisite: txProofPrerequisite,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBinding(binding),
  });
};

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
    observation: await workflow.l1.observeHeader({
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
