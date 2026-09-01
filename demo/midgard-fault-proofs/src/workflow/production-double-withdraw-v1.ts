import {
  DOUBLE_WITHDRAW_VIOLATION_ID_V1,
  DoubleWithdrawStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { DoubleWithdrawContractsV1 } from "../double-withdraw/contracts-v1.js";
import { submitDoubleWithdrawInit } from "../double-withdraw/submit-double-withdraw-init.js";
import {
  parseSubmitDoubleWithdrawInclusionV1,
  submitDoubleWithdrawStep01,
} from "../double-withdraw/submit-double-withdraw-step-01.js";
import { submitDoubleWithdrawStep02 } from "../double-withdraw/submit-double-withdraw-step-02.js";
import {
  admitCanonicalEvidenceForProofBuildV1,
  type CanonicalEvidenceBuilderInputV1,
} from "../evidence/prepare-from-evidence-v1.js";
import {
  type PreparedDoubleWithdrawOutputV1,
  prepareDoubleWithdrawFromCommittedLeavesV1,
} from "../prepare-double-withdraw.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSignerV1,
  bindFraudProofWorkflowDeploymentV1,
  type FraudProofWorkflowDeploymentBindingV1,
  releaseFinalityAuthorityFromDeploymentBindingV1,
  requireManifestBoundReferenceScriptUtxoV1,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifierV1,
  createFraudProofFamilyLocalKupmiosL1ObservationPortV1,
  type FraudProofFamilyL1ObservationPortV1,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStoreV1,
  type JournalJsonObjectV1,
  normalizeJournalJsonV1,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfigV1 } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistryV1,
  type FraudProofFamilyWorkflowAdapterV1,
  type FraudProofWorkflowActionV1,
  type FraudProofWorkflowRunResultV1,
  type FraudProofWorkflowTerminalVerifierV1,
  runFraudProofWorkflowFromRetainedDaV1,
} from "./orchestrator-v1.js";
import {
  createProductionLinearFamilyWorkflowAdapterV1,
  PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionLinearFamilyTransactionPortV1,
} from "./production-linear-family-adapter-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_DOUBLE_WITHDRAW_ARTIFACT_V1 =
  "midgard-production-double-withdraw-artifact-v1" as const;

type DoubleWithdrawArtifactEntryV1 = Readonly<{
  keyCbor: string;
  valueCbor: string;
}>;

export type ProductionDoubleWithdrawArtifactV1 = JournalJsonObjectV1 & {
  readonly schemaVersion: typeof PRODUCTION_DOUBLE_WITHDRAW_ARTIFACT_V1;
  readonly headerHash: string;
  readonly committedWithdrawalsRoot: string;
  readonly withdrawalCount: number;
  readonly firstLeafIndex: number;
  readonly secondLeafIndex: number;
  readonly entries: readonly DoubleWithdrawArtifactEntryV1[];
};

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error(`${label} must be a plain object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exactKeys = (
  value: Readonly<Record<string, unknown>>,
  expected: readonly string[],
  label: string,
): void => {
  const actual = Object.keys(value).sort();
  const canonical = [...expected].sort();
  if (
    actual.length !== canonical.length ||
    actual.some((key, index) => key !== canonical[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
};

const canonicalHex = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const natural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} is not a non-negative safe integer`);
  }
  return value as number;
};

const parseArtifact = (value: unknown): ProductionDoubleWithdrawArtifactV1 => {
  const artifact = record(value, "double-withdraw artifact");
  exactKeys(
    artifact,
    [
      "schemaVersion",
      "headerHash",
      "committedWithdrawalsRoot",
      "withdrawalCount",
      "firstLeafIndex",
      "secondLeafIndex",
      "entries",
    ],
    "double-withdraw artifact",
  );
  if (artifact.schemaVersion !== PRODUCTION_DOUBLE_WITHDRAW_ARTIFACT_V1) {
    throw new Error("double-withdraw artifact version changed");
  }
  if (!Array.isArray(artifact.entries) || artifact.entries.length === 0) {
    throw new Error("double-withdraw artifact has no withdrawal leaves");
  }
  const entries = Object.freeze(
    artifact.entries.map((value, index) => {
      const entry = record(value, `double-withdraw entry ${index.toString()}`);
      exactKeys(
        entry,
        ["keyCbor", "valueCbor"],
        `double-withdraw entry ${index.toString()}`,
      );
      return Object.freeze({
        keyCbor: canonicalHex(
          entry.keyCbor,
          EVEN_HEX,
          `double-withdraw entry ${index.toString()} key`,
        ),
        valueCbor: canonicalHex(
          entry.valueCbor,
          EVEN_HEX,
          `double-withdraw entry ${index.toString()} value`,
        ),
      });
    }),
  );
  const withdrawalCount = natural(
    artifact.withdrawalCount,
    "double-withdraw withdrawal count",
  );
  if (withdrawalCount !== entries.length) {
    throw new Error(
      "double-withdraw artifact count differs from its withdrawal leaves",
    );
  }
  return Object.freeze({
    schemaVersion: PRODUCTION_DOUBLE_WITHDRAW_ARTIFACT_V1,
    headerHash: canonicalHex(
      artifact.headerHash,
      HEX_28,
      "double-withdraw header",
    ),
    committedWithdrawalsRoot: canonicalHex(
      artifact.committedWithdrawalsRoot,
      HEX_32,
      "double-withdraw withdrawals root",
    ),
    withdrawalCount,
    firstLeafIndex: natural(
      artifact.firstLeafIndex,
      "double-withdraw first leaf index",
    ),
    secondLeafIndex: natural(
      artifact.secondLeafIndex,
      "double-withdraw second leaf index",
    ),
    entries,
  });
};

type AdmittedDoubleWithdrawArtifactV1 = Readonly<{
  artifact: ProductionDoubleWithdrawArtifactV1;
  prepared: PreparedDoubleWithdrawOutputV1;
  firstInclusion: ReturnType<typeof parseSubmitDoubleWithdrawInclusionV1>;
  secondInclusion: ReturnType<typeof parseSubmitDoubleWithdrawInclusionV1>;
}>;

/** Rebuilds the counted root, deterministic pair, and both MPF proofs. */
export const admitProductionDoubleWithdrawArtifactV1 = async (
  value: unknown,
): Promise<AdmittedDoubleWithdrawArtifactV1> => {
  const artifact = parseArtifact(value);
  const first = artifact.entries[artifact.firstLeafIndex];
  const second = artifact.entries[artifact.secondLeafIndex];
  if (
    first === undefined ||
    second === undefined ||
    artifact.firstLeafIndex >= artifact.secondLeafIndex
  ) {
    throw new Error("double-withdraw artifact selected an invalid leaf pair");
  }
  const prepared = await prepareDoubleWithdrawFromCommittedLeavesV1({
    headerHash: artifact.headerHash,
    committedWithdrawalsRoot: artifact.committedWithdrawalsRoot,
    withdrawalCount: BigInt(artifact.withdrawalCount),
    entries: artifact.entries.map(({ keyCbor, valueCbor }) => [
      keyCbor,
      valueCbor,
    ]),
    firstWithdrawalIdCbor: first.keyCbor,
    secondWithdrawalIdCbor: second.keyCbor,
  });
  if (
    prepared.firstLeaf.index !== artifact.firstLeafIndex ||
    prepared.secondLeaf.index !== artifact.secondLeafIndex
  ) {
    throw new Error("double-withdraw artifact pair changed on re-derivation");
  }
  return Object.freeze({
    artifact,
    prepared,
    firstInclusion: parseSubmitDoubleWithdrawInclusionV1(
      prepared.firstInclusion,
    ),
    secondInclusion: parseSubmitDoubleWithdrawInclusionV1(
      prepared.secondInclusion,
    ),
  });
};

const detectionIdForPrepared = (
  prepared: PreparedDoubleWithdrawOutputV1,
): string =>
  `${DOUBLE_WITHDRAW_VIOLATION_ID_V1}:${prepared.firstLeaf.index.toString()}:${prepared.secondLeaf.index.toString()}:${prepared.firstLeaf.withdrawalIdCbor}:${prepared.secondLeaf.withdrawalIdCbor}`;

const selectedPairFromClassification = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  > & { readonly category: "doubleWithdraw" },
): Readonly<{
  firstLeafIndex: number;
  secondLeafIndex: number;
  firstKeyCbor: string;
  secondKeyCbor: string;
}> => {
  const [violationId, first, second, firstKeyCbor, secondKeyCbor, ...surplus] =
    classification.selected.detectionId.split(":");
  if (
    violationId !== DOUBLE_WITHDRAW_VIOLATION_ID_V1 ||
    surplus.length !== 0 ||
    !/^(?:0|[1-9][0-9]*)$/u.test(first ?? "") ||
    !/^(?:0|[1-9][0-9]*)$/u.test(second ?? "") ||
    !EVEN_HEX.test(firstKeyCbor ?? "") ||
    !EVEN_HEX.test(secondKeyCbor ?? "")
  ) {
    throw new Error("double-withdraw classification has a malformed pair id");
  }
  const firstLeafIndex = Number(first);
  const secondLeafIndex = Number(second);
  if (
    !Number.isSafeInteger(firstLeafIndex) ||
    !Number.isSafeInteger(secondLeafIndex) ||
    firstLeafIndex >= secondLeafIndex ||
    classification.selected.position !== BigInt(secondLeafIndex)
  ) {
    throw new Error("double-withdraw classification has an invalid pair order");
  }
  return {
    firstLeafIndex,
    secondLeafIndex,
    firstKeyCbor: firstKeyCbor!,
    secondKeyCbor: secondKeyCbor!,
  };
};

const prepareArtifactFromEvidenceV1 = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInputV1 & {
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  > & { readonly category: "doubleWithdraw" };
}): Promise<ProductionDoubleWithdrawArtifactV1> => {
  const admitted = admitCanonicalEvidenceForProofBuildV1(evidence);
  if (
    classification.headerHash !== admitted.headerHash ||
    classification.selected.violationId !== DOUBLE_WITHDRAW_VIOLATION_ID_V1
  ) {
    throw new Error(
      "double-withdraw classification differs from canonical evidence",
    );
  }
  const entries = evidence.reconstruction.rootData.withdrawals.entries.map(
    ({ key, value }) => ({
      keyCbor: key.toString("hex"),
      valueCbor: value.toString("hex"),
    }),
  );
  const selected = selectedPairFromClassification(classification);
  if (
    entries[selected.firstLeafIndex]?.keyCbor !== selected.firstKeyCbor ||
    entries[selected.secondLeafIndex]?.keyCbor !== selected.secondKeyCbor
  ) {
    throw new Error(
      "double-withdraw classification keys differ from the committed leaves",
    );
  }
  const prepared = await prepareDoubleWithdrawFromCommittedLeavesV1({
    headerHash: admitted.headerHash,
    committedWithdrawalsRoot: evidence.header.withdrawalsRoot,
    withdrawalCount: evidence.header.withdrawalCount,
    entries: entries.map(({ keyCbor, valueCbor }) => [keyCbor, valueCbor]),
    firstWithdrawalIdCbor: selected.firstKeyCbor,
    secondWithdrawalIdCbor: selected.secondKeyCbor,
  });
  if (
    classification.selected.position !== BigInt(prepared.secondLeaf.index) ||
    classification.selected.detectionId !== detectionIdForPrepared(prepared)
  ) {
    throw new Error(
      "double-withdraw classification changed its deterministic committed pair",
    );
  }
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_DOUBLE_WITHDRAW_ARTIFACT_V1,
    headerHash: admitted.headerHash,
    committedWithdrawalsRoot: evidence.header.withdrawalsRoot,
    withdrawalCount: entries.length,
    firstLeafIndex: prepared.firstLeaf.index,
    secondLeafIndex: prepared.secondLeaf.index,
    entries,
  }) as ProductionDoubleWithdrawArtifactV1;
  await admitProductionDoubleWithdrawArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type DoubleWithdrawWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
  };
}>;

type BoundDoubleWithdrawTransactionsConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"doubleWithdraw">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: DoubleWithdrawContractsV1;
  category: FraudProofWorkflowDeploymentBindingV1<"doubleWithdraw">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBindingV1<"doubleWithdraw">["catalogue"];
  referenceScripts: DoubleWithdrawWorkflowReferenceScriptsV1;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  deploymentInfo: unknown;
}>;

type DoubleWithdrawBuilderSetV1 = Readonly<{
  init: typeof submitDoubleWithdrawInit;
  step01: typeof submitDoubleWithdrawStep01;
  step02: typeof submitDoubleWithdrawStep02;
  remove: typeof submitRemoveFraudulentBlock;
}>;

const productionBuilders: DoubleWithdrawBuilderSetV1 = Object.freeze({
  init: submitDoubleWithdrawInit,
  step01: submitDoubleWithdrawStep01,
  step02: submitDoubleWithdrawStep02,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowActionV1,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "double-withdraw workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "doubleWithdraw" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("double-withdraw workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  name: string,
): string => {
  const value = input[name];
  if (typeof value !== "string") {
    throw new Error(`double-withdraw workflow action omitted ${name}`);
  }
  return value;
};

const createBoundTransactionPortV1 = ({
  config,
  builders,
}: {
  readonly config: BoundDoubleWithdrawTransactionsConfigV1;
  readonly builders: DoubleWithdrawBuilderSetV1;
}): ProductionLinearFamilyTransactionPortV1<"doubleWithdraw"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "doubleWithdraw",
  prepare: async ({ evidence, classification }) =>
    await prepareArtifactFromEvidenceV1({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitProductionDoubleWithdrawArtifactV1(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "double-withdraw artifact targets a different manifest-bound header",
      );
    }
    const input = requiredAction(action);
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (preSubmitBoundary) => {
          await builders.init({
            lucid: config.lucid,
            blueprint: config.blueprint,
            network: config.network,
            contracts: config.contracts,
            category: config.category,
            catalogue: config.catalogue,
            signer: config.signer,
            fraudulentBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            fraudulentHeaderHash: config.headerHash,
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_01") {
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (preSubmitBoundary) => {
          await builders.step01({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            inclusion: admitted.firstInclusion,
            referenceScriptUtxo: config.referenceScripts.steps[0],
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "step_02") {
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (preSubmitBoundary) => {
          await builders.step02({
            lucid: config.lucid,
            contracts: config.contracts,
            categoryId: config.category.categoryId,
            network: config.network,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            stateQueueBlockOutRef: stringField(input, "stateQueueBlockOutRef"),
            inclusion: admitted.secondInclusion,
            referenceScriptUtxo: config.referenceScripts.steps[1],
            witnessReferenceScripts: config.referenceScripts.witnesses,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      );
      return Object.freeze({ transaction });
    }
    if (input.stage === "remove") {
      let mutationLease: StateQueueMutationLease | undefined;
      const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
        acquire: async () => {
          const acquired =
            await config.stateQueueMutationLeaseCoordinator.acquire();
          mutationLease = acquired;
          return acquired;
        },
      };
      const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
      const fraudProofOutRef = stringField(input, "fraudProofOutRef");
      const transaction = await captureLocallyEvaluatedTransactionV1(
        async (boundary) => {
          await builders.remove({
            lucid: config.lucid,
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            signer: config.signer,
            fraudCategory: "doubleWithdraw",
            fraudulentHeaderHash: config.headerHash,
            requireReferenceScripts: true,
            stateQueueMutationLeaseCoordinator: retainingCoordinator,
            fraudProverRewardLovelace: config.fraudProverRewardLovelace,
            preSubmitBoundary: async (built) => {
              if (
                !workflowTransactionInputOutRefsV1(built.signed).includes(
                  nextRemovalOutRef,
                )
              ) {
                throw new Error(
                  "double-withdraw removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefsV1(
                  built.signed,
                ).includes(fraudProofOutRef)
              ) {
                throw new Error(
                  "double-withdraw removal does not reference the authenticated retained proof token",
                );
              }
              await boundary(built);
            },
          });
        },
      );
      return Object.freeze({
        transaction,
        ...(mutationLease === undefined ? {} : { mutationLease }),
      });
    }
    throw new Error(
      `double-withdraw workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundDoubleWithdrawWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: DoubleWithdrawWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundDoubleWithdrawWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"doubleWithdraw">;
  l1: FraudProofFamilyL1ObservationPortV1<"doubleWithdraw">;
  transactions: ProductionLinearFamilyTransactionPortV1<"doubleWithdraw">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundDoubleWithdrawWorkflowV1 = async (
  config: ManifestBoundDoubleWithdrawWorkflowConfigV1,
): Promise<ManifestBoundDoubleWithdrawWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "doubleWithdraw",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      DoubleWithdrawStep02Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  const chain = binding.resolvedContracts.contracts.doubleWithdraw;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error(
      "double-withdraw manifest binding omitted required contracts",
    );
  }
  const references: DoubleWithdrawWorkflowReferenceScriptsV1 = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofDoubleWithdraw",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofDoubleWithdrawStep02",
        utxo: config.referenceScripts.steps[1],
      }),
    ] as const),
    witnesses: Object.freeze({
      computationThreadMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "computationThreadMint",
        utxo: config.referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofMint",
        utxo: config.referenceScripts.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
      }),
    }),
  });
  const contracts: DoubleWithdrawContractsV1 = Object.freeze({
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
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  const transactions = createBoundTransactionPortV1({
    config: {
      lucid: config.lucid,
      blueprint: binding.blueprint,
      network: binding.network,
      signer: config.signer,
      headerHash: binding.definition.headerHash,
      contracts,
      category: binding.resolvedContracts.category,
      catalogue: binding.catalogue,
      referenceScripts: references,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
      fraudProverRewardLovelace: BigInt(
        binding.releaseEconomics.policy.fraudProverRewardLovelace,
      ),
      deploymentInfo: binding.deploymentInfo,
    },
    builders: productionBuilders,
  });
  return Object.freeze({
    binding,
    l1,
    transactions,
    adapter: createProductionLinearFamilyWorkflowAdapterV1({
      category: "doubleWithdraw",
      l1,
      transactions,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
    }),
    terminalVerifier:
      createFraudProofFamilyAuthenticatedL1TerminalVerifierV1(l1),
    releaseFinalityAuthority:
      releaseFinalityAuthorityFromDeploymentBindingV1(binding),
  });
};

export const runOrResumeManifestBoundDoubleWithdrawWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundDoubleWithdrawWorkflowV1;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStoreV1;
}): Promise<FraudProofWorkflowRunResultV1> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDaV1({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["doubleWithdraw"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};

export const unsafeCreateDoubleWithdrawTransactionPortForTest = (input: {
  readonly config: BoundDoubleWithdrawTransactionsConfigV1;
  readonly builders: DoubleWithdrawBuilderSetV1;
}): ProductionLinearFamilyTransactionPortV1<"doubleWithdraw"> =>
  createBoundTransactionPortV1(input);
