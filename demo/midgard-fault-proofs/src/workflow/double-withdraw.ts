import {
  DOUBLE_WITHDRAW_VIOLATION_ID,
  DoubleWithdrawStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { DoubleWithdrawContracts } from "../double-withdraw/contracts.js";
import { submitDoubleWithdrawInit } from "../double-withdraw/submit-double-withdraw-init.js";
import {
  parseSubmitDoubleWithdrawInclusion,
  submitDoubleWithdrawStep01,
} from "../double-withdraw/submit-double-withdraw-step-01.js";
import { submitDoubleWithdrawStep02 } from "../double-withdraw/submit-double-withdraw-step-02.js";
import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalEvidenceBuilderInput,
} from "../evidence/prepare-from-evidence.js";
import {
  type PreparedDoubleWithdrawOutput,
  prepareDoubleWithdrawFromCommittedLeaves,
} from "../prepare-double-withdraw.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  defineLinearFamily,
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { type JournalJsonObject, normalizeJournalJson } from "./journal.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const DOUBLE_WITHDRAW_ARTIFACT =
  "midgard-production-double-withdraw-artifact-v1" as const;

type DoubleWithdrawArtifactEntry = Readonly<{
  keyCbor: string;
  valueCbor: string;
}>;

export type DoubleWithdrawArtifact = JournalJsonObject & {
  readonly schemaVersion: typeof DOUBLE_WITHDRAW_ARTIFACT;
  readonly headerHash: string;
  readonly committedWithdrawalsRoot: string;
  readonly withdrawalCount: number;
  readonly firstLeafIndex: number;
  readonly secondLeafIndex: number;
  readonly entries: readonly DoubleWithdrawArtifactEntry[];
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

const parseArtifact = (value: unknown): DoubleWithdrawArtifact => {
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
  if (artifact.schemaVersion !== DOUBLE_WITHDRAW_ARTIFACT) {
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
    schemaVersion: DOUBLE_WITHDRAW_ARTIFACT,
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

type AdmittedDoubleWithdrawArtifact = Readonly<{
  artifact: DoubleWithdrawArtifact;
  prepared: PreparedDoubleWithdrawOutput;
  firstInclusion: ReturnType<typeof parseSubmitDoubleWithdrawInclusion>;
  secondInclusion: ReturnType<typeof parseSubmitDoubleWithdrawInclusion>;
}>;

/** Rebuilds the counted root, deterministic pair, and both MPF proofs. */
export const admitDoubleWithdrawArtifact = async (
  value: unknown,
): Promise<AdmittedDoubleWithdrawArtifact> => {
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
  const prepared = await prepareDoubleWithdrawFromCommittedLeaves({
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
    firstInclusion: parseSubmitDoubleWithdrawInclusion(prepared.firstInclusion),
    secondInclusion: parseSubmitDoubleWithdrawInclusion(
      prepared.secondInclusion,
    ),
  });
};

const detectionIdForPrepared = (
  prepared: PreparedDoubleWithdrawOutput,
): string =>
  `${DOUBLE_WITHDRAW_VIOLATION_ID}:${prepared.firstLeaf.index.toString()}:${prepared.secondLeaf.index.toString()}:${prepared.firstLeaf.withdrawalIdCbor}:${prepared.secondLeaf.withdrawalIdCbor}`;

const selectedPairFromClassification = (
  classification: Extract<
    CanonicalBlockClassification,
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
    violationId !== DOUBLE_WITHDRAW_VIOLATION_ID ||
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

const prepareArtifactFromEvidence = async ({
  evidence,
  classification,
}: CanonicalEvidenceBuilderInput & {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  > & { readonly category: "doubleWithdraw" };
}): Promise<DoubleWithdrawArtifact> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  if (
    classification.headerHash !== admitted.headerHash ||
    classification.selected.violationId !== DOUBLE_WITHDRAW_VIOLATION_ID
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
  const prepared = await prepareDoubleWithdrawFromCommittedLeaves({
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
  const artifact = normalizeJournalJson({
    schemaVersion: DOUBLE_WITHDRAW_ARTIFACT,
    headerHash: admitted.headerHash,
    committedWithdrawalsRoot: evidence.header.withdrawalsRoot,
    withdrawalCount: entries.length,
    firstLeafIndex: prepared.firstLeaf.index,
    secondLeafIndex: prepared.secondLeaf.index,
    entries,
  }) as DoubleWithdrawArtifact;
  await admitDoubleWithdrawArtifact(artifact);
  return Object.freeze(artifact);
};

const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;

export type DoubleWithdrawWorkflowReferenceScripts =
  LinearFamilyReferenceScripts<
    "doubleWithdraw",
    (typeof WITNESS_ROLES)[number],
    false
  >;

type AssemblyContext = LinearFamilyAssemblyContext<
  "doubleWithdraw",
  (typeof WITNESS_ROLES)[number],
  false
>;

type BoundDoubleWithdrawTransactionsConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  network: FraudProofWorkflowDeploymentBinding<"doubleWithdraw">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: DoubleWithdrawContracts;
  category: FraudProofWorkflowDeploymentBinding<"doubleWithdraw">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"doubleWithdraw">["catalogue"];
  referenceScripts: DoubleWithdrawWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
  deploymentInfo: unknown;
}>;

type DoubleWithdrawBuilderSet = Readonly<{
  init: typeof submitDoubleWithdrawInit;
  step01: typeof submitDoubleWithdrawStep01;
  step02: typeof submitDoubleWithdrawStep02;
  remove: typeof submitRemoveFraudulentBlock;
}>;

const productionBuilders: DoubleWithdrawBuilderSet = Object.freeze({
  init: submitDoubleWithdrawInit,
  step01: submitDoubleWithdrawStep01,
  step02: submitDoubleWithdrawStep02,
  remove: submitRemoveFraudulentBlock,
});

const requiredAction = (
  action: FraudProofWorkflowAction,
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

const createBoundTransactionPort = ({
  config,
  builders,
}: {
  readonly config: BoundDoubleWithdrawTransactionsConfig;
  readonly builders: DoubleWithdrawBuilderSet;
}): LinearFamilyTransactionPort<"doubleWithdraw"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "doubleWithdraw",
  prepare: async ({ evidence, classification }) =>
    await prepareArtifactFromEvidence({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitDoubleWithdrawArtifact(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "double-withdraw artifact targets a different manifest-bound header",
      );
    }
    const input = requiredAction(action);
    if (input.stage === "init") {
      const transaction = await captureLocallyEvaluatedTransaction(
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
      const transaction = await captureLocallyEvaluatedTransaction(
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
      const transaction = await captureLocallyEvaluatedTransaction(
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
      const transaction = await captureLocallyEvaluatedTransaction(
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
                !workflowTransactionInputOutRefs(built.signed).includes(
                  nextRemovalOutRef,
                )
              ) {
                throw new Error(
                  "double-withdraw removal does not consume the authenticated next queue input",
                );
              }
              if (
                !workflowTransactionReferenceInputOutRefs(
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

export type ManifestBoundDoubleWithdrawWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "doubleWithdraw",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type ManifestBoundDoubleWithdrawWorkflow =
  ManifestBoundLinearFamilyWorkflow<"doubleWithdraw", false>;

const contracts = (context: AssemblyContext): DoubleWithdrawContracts => {
  const { binding } = context;
  const chain = binding.resolvedContracts.contracts.doubleWithdraw;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error(
      "double-withdraw manifest binding omitted required contracts",
    );
  }
  return Object.freeze({
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
};

export const DOUBLE_WITHDRAW_FAMILY_DEFINITION = defineLinearFamily({
  category: "doubleWithdraw",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    DoubleWithdrawStep02Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: () => DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: (context) =>
      createBoundTransactionPort({
        config: {
          lucid: context.lucid,
          blueprint: context.binding.blueprint,
          network: context.binding.network,
          signer: context.signer,
          headerHash: context.binding.definition.headerHash,
          contracts: contracts(context),
          category: context.binding.resolvedContracts.category,
          catalogue: context.binding.catalogue,
          referenceScripts: context.references,
          stateQueueMutationLeaseCoordinator:
            context.stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            context.binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
          deploymentInfo: context.binding.deploymentInfo,
        },
        builders: productionBuilders,
      }),
  },
});

export const createManifestBoundDoubleWithdrawWorkflow = (
  config: ManifestBoundDoubleWithdrawWorkflowConfig,
): Promise<ManifestBoundDoubleWithdrawWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    DOUBLE_WITHDRAW_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundDoubleWithdrawWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;

export const unsafeCreateDoubleWithdrawTransactionPortForTest = (input: {
  readonly config: BoundDoubleWithdrawTransactionsConfig;
  readonly builders: DoubleWithdrawBuilderSet;
}): LinearFamilyTransactionPort<"doubleWithdraw"> =>
  createBoundTransactionPort(input);
