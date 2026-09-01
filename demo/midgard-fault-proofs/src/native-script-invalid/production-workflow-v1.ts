import {
  decodeMidgardAddressWitnessItemV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  FraudProofComputationThreadStepDatum,
  MIDGARD_FIELD_INDEX_V1,
  NativeScriptInvalidStep02DatumSchema,
  NativeScriptInvalidStep03DatumSchema,
  NativeScriptInvalidStep04DatumSchema,
  NativeScriptInvalidStep05DatumSchema,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import {
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import { resolvePublishedProofChunksV1 } from "../publish-proof-chunks.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { parseSubmitStep01TxInclusion } from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import { NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY_V1 } from "../workflow/complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "../workflow/deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "../workflow/family-l1-observation-v1.js";
import type { FraudProofWorkflowJournalStoreV1 } from "../workflow/journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "../workflow/local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "../workflow/orchestrator-v1.js";
import {
  createProductionCursorFamilyWorkflowAdapterV1,
  PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionCursorFamilyTransactionPortV1,
} from "../workflow/production-cursor-family-adapter-v1.js";
import {
  captureProductionCursorRemovalV1,
  productionCursorFamilyActionInputV1,
  productionCursorStringFieldV1,
} from "../workflow/production-cursor-family-runtime-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriageRequirementV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "../workflow/production-field-carriage-prerequisite-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePortV1,
  withProductionProofChunkPrerequisiteV1,
} from "../workflow/production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "../workflow/release-finality-policy-v1.js";
import { captureLocallyEvaluatedTransactionV1 } from "../workflow/transaction-boundary-v1.js";
import type { NativeScriptInvalidContractsV1 } from "./contracts-v1.js";
import { nativeScriptInvalidUsesDirectRouteV1 } from "./evidence-machine-v1.js";
import {
  admitProductionNativeScriptInvalidArtifactV1,
  prepareProductionNativeScriptInvalidArtifactV1,
} from "./production-artifact-v1.js";
import { submitNativeScriptInvalidInit } from "./submit-init-v1.js";
import { submitNativeScriptInvalidStep01 } from "./submit-step-01-v1.js";
import { submitNativeScriptInvalidStep02 } from "./submit-step-02-v1.js";
import { submitNativeScriptInvalidStep03StartSignerScan } from "./submit-step-03-staged-v1.js";
import { submitNativeScriptInvalidStep03 } from "./submit-step-03-v1.js";
import { submitNativeScriptInvalidStep04 } from "./submit-step-04-v1.js";
import { submitNativeScriptInvalidStep05 } from "./submit-step-05-v1.js";
import { NATIVE_SCRIPT_INVALID_CURSOR_SPEC_V1 } from "./workflow-spec-v1.js";

export type NativeScriptInvalidWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO, UTxO];
  witnesses: Required<FaultProofWitnessReferenceScriptsV1>;
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"nativeScriptInvalid">;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  contracts: NativeScriptInvalidContractsV1;
  references: NativeScriptInvalidWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

const buffers = (values: readonly string[]): readonly Uint8Array[] =>
  values.map((value) => Buffer.from(value, "hex"));

const witnessSet = (
  admitted: ReturnType<typeof admitProductionNativeScriptInvalidArtifactV1>,
) => {
  const compact = deriveMidgardNativeTxWitnessSetCompactV1(
    decodeMidgardNativeTxFullV1FromCanonicalCbor(
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
  admitted: ReturnType<typeof admitProductionNativeScriptInvalidArtifactV1>,
  owner: string,
) =>
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.scriptWitnesses,
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
  admitted: ReturnType<typeof admitProductionNativeScriptInvalidArtifactV1>,
  owner: string,
) =>
  planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.addressWitnesses,
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
  admitted: ReturnType<typeof admitProductionNativeScriptInvalidArtifactV1>,
): boolean =>
  nativeScriptInvalidUsesDirectRouteV1({
    signerCount: admitted.prepared.addrWitnessItemCbors.length,
    scriptBytes: decodeMidgardVersionedScript(
      Buffer.from(admitted.prepared.scriptItemCbor, "hex"),
    ).scriptBytes.length,
  });

const resolveField = async ({
  config,
  plan,
}: {
  readonly config: BoundConfigV1;
  readonly plan: ReturnType<typeof scriptFieldPlan>;
}) => {
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: plan,
  });
  if (publications === undefined) {
    throw new Error("native-script-invalid field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
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
  config: BoundConfigV1,
): ProductionCursorFamilyTransactionPortV1<"nativeScriptInvalid"> => ({
  portVersion: PRODUCTION_CURSOR_FAMILY_TRANSACTION_PORT_V1,
  category: "nativeScriptInvalid",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionNativeScriptInvalidArtifactV1({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionNativeScriptInvalidArtifactV1(artifact);
    if (admitted.artifact.headerHash !== config.binding.definition.headerHash) {
      throw new Error(
        "native-script-invalid artifact changed the bound header",
      );
    }
    const input = productionCursorFamilyActionInputV1({
      category: "nativeScriptInvalid",
      action,
    });
    const stage = input.stage;
    const threadOutRef = () =>
      productionCursorStringFieldV1(input, "threadOutRef");
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
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudulentBlockOutRef: productionCursorStringFieldV1(
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
      const chunks = await resolvePublishedProofChunksV1({
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.prepared.txInclusion.txMembershipProofCbor,
      });
      if (chunks === undefined) {
        throw new Error("native-script-invalid transaction proof disappeared");
      }
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitNativeScriptInvalidStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts: config.contracts,
              categoryId,
              signer: config.signer,
              threadOutRef: threadOutRef(),
              stateQueueBlockOutRef: productionCursorStringFieldV1(
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
        transaction: await captureLocallyEvaluatedTransactionV1(
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
        transaction: await captureLocallyEvaluatedTransactionV1(
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
                      decodeMidgardAddressWitnessItemV1(
                        Buffer.from(item, "hex"),
                      ).verificationKey,
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
        transaction: await captureLocallyEvaluatedTransactionV1(
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
        transaction: await captureLocallyEvaluatedTransactionV1(
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
      return await captureProductionCursorRemovalV1({
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

export type ManifestBoundNativeScriptInvalidWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: NativeScriptInvalidWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundNativeScriptInvalidWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"nativeScriptInvalid">;
  l1: FraudProofFamilyL1ObservationPortV1<"nativeScriptInvalid">;
  transactions: ProductionCursorFamilyTransactionPortV1<"nativeScriptInvalid">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundNativeScriptInvalidWorkflowV1 = async (
  config: ManifestBoundNativeScriptInvalidWorkflowConfigV1,
): Promise<ManifestBoundNativeScriptInvalidWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
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
  assertManifestBoundWorkflowSignerV1({
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
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as NativeScriptInvalidWorkflowReferenceScriptsV1["steps"];
  const witness = <Name extends keyof FaultProofWitnessReferenceScriptsV1>(
    name: Name,
    contractName: string,
  ) =>
    requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName,
      utxo: config.referenceScripts.witnesses[name],
    });
  const references: NativeScriptInvalidWorkflowReferenceScriptsV1 =
    Object.freeze({
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
      fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fieldPreimageCertificateMint",
        utxo: config.referenceScripts.fieldPreimageCertificateMint,
      }),
    });
  const contracts: NativeScriptInvalidContractsV1 = Object.freeze({
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
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.rawL1 === undefined) {
    throw new Error("native-script-invalid raw L1 authority is unavailable");
  }
  const bound: BoundConfigV1 = {
    binding,
    lucid: config.lucid,
    signer: config.signer,
    contracts,
    references,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  };
  const transactions = transactionPort(bound);
  let adapter = createProductionCursorFamilyWorkflowAdapterV1({
    spec: NATIVE_SCRIPT_INVALID_CURSOR_SPEC_V1,
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: "nativeScriptInvalid",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      const input = productionCursorFamilyActionInputV1({
        category: "nativeScriptInvalid",
        action,
      });
      const admitted = admitProductionNativeScriptInvalidArtifactV1(artifact);
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
      } satisfies ProductionFieldCarriageRequirementV1;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionFieldCarriagePrerequisiteV1({
    category: "nativeScriptInvalid",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const txProofPrerequisite = createAuthenticatedProofChunkPrerequisitePortV1({
    category: "nativeScriptInvalid",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) =>
      action.input.stage === "step_01"
        ? admitProductionNativeScriptInvalidArtifactV1(artifact).prepared
            .txInclusion.txMembershipProofCbor
        : null,
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "nativeScriptInvalid",
    base: adapter,
    prerequisite: txProofPrerequisite,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter,
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  });
};

export const runOrResumeManifestBoundNativeScriptInvalidWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundNativeScriptInvalidWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> =>
  await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation: await workflow.l1.observeHeader({
      headerHash: workflow.binding.definition.headerHash,
    }),
    sources,
    replayer: NATIVE_SCRIPT_INVALID_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["nativeScriptInvalid"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
