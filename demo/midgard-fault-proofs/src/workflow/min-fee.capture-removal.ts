import {
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  MIN_FEE_VIOLATION_ID,
  minimumFeeFromProofSource,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import {
  type CanonicalEvidenceBuilderInput,
  prepareMinFeeFromCanonicalEvidence,
} from "../evidence/prepare-from-evidence.js";
import { planFaultProofFieldOpening } from "../field-opening.js";
import type { MinFeeContracts } from "../min-fee-contracts.js";
import {
  admitMinFeeForcedArtifact,
  MIN_FEE_FORCED_ARTIFACT,
} from "../min-fee-forced-artifact.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { CanonicalBlockClassification } from "./classification.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type LinearFamilyAssemblyContext,
  type LinearFamilyReferenceScripts,
} from "./family-definition.js";
import { type JournalJsonObject, normalizeJournalJson } from "./journal.js";
import {
  type AdmittedMinFeeArtifact,
  HEX_28,
  HEX_32,
  MIN_FEE_ARTIFACT,
  type MinFeeArtifact,
  NATURAL,
  parseArtifact,
  parseFieldItems,
  record,
  witnessSetCore,
} from "./min-fee.parse-artifact.js";
import type { FraudProofWorkflowAction } from "./orchestrator.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const admitMinFeeArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedMinFeeArtifact => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("min-fee carriage owner must be a 28-byte key hash");
  }
  const parsed = parseArtifact(value);
  const fieldPlans = parsed.fieldItemCbors.map((items, fieldIndex) =>
    planFaultProofFieldOpening({
      anchorSourceKind: 0n,
      fieldIndex,
      anchorTxId: parsed.artifact.nativeTxId,
      nativeTxCompactCbor: parsed.artifact.nativeTxCompactCbor,
      itemCbors: items,
      owner: carriageOwner,
      publish: false,
      ...(fieldIndex < 6
        ? {}
        : {
            witnessSet: parsed.witnessSet,
            anchorWitnessSetHash: parsed.inclusion.nativeTx.witness_set_hash,
          }),
      label: `min-fee artifact field ${fieldIndex.toString()}`,
    }),
  );
  const boundary = minimumFeeFromProofSource({
    sourceKind: "normal",
    source: {
      compactCbor: Buffer.from(parsed.artifact.nativeTxCompactCbor, "hex"),
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
        witnessSetCore(parsed.witnessSet),
      ),
      fieldPreimageLengthsCbor: encodeMidgardNativeTxProofFieldLengths(
        fieldPlans.map((plan) => plan.preimage.length),
      ),
    },
    minFeeA: BigInt(parsed.artifact.minFeeA),
    minFeeB: BigInt(parsed.artifact.minFeeB),
  });
  const fee = parsed.inclusion.nativeTx.body.fee;
  const expectedDetection = `${MIN_FEE_VIOLATION_ID}:${parsed.artifact.position.toString()}:${parsed.artifact.nativeTxId}:${fee.toString()}:${boundary.minimumFee.toString()}`;
  if (
    fee >= boundary.minimumFee ||
    parsed.artifact.fee !== fee.toString() ||
    parsed.artifact.canonicalTxSize !== boundary.canonicalTxSize.toString() ||
    parsed.artifact.minimumFee !== boundary.minimumFee.toString() ||
    parsed.artifact.shortfall !== (boundary.minimumFee - fee).toString() ||
    parsed.artifact.detectionId !== expectedDetection
  ) {
    throw new Error("min-fee artifact does not re-derive its exact violation");
  }
  return Object.freeze({ ...parsed, fieldPlans: Object.freeze(fieldPlans) });
};

export const admitWorkflowArtifact = async (
  artifact: JournalJsonObject,
  owner: string,
) => {
  if (artifact.schemaVersion !== MIN_FEE_FORCED_ARTIFACT)
    return { ...admitMinFeeArtifact(artifact, owner), forced: null };
  const forced = await admitMinFeeForcedArtifact(artifact);
  const evidence = forced.evidence;
  const fieldItemCbors = parseFieldItems(evidence.fieldItemCbors);
  return {
    forced,
    inclusion: null,
    witnessSet: evidence.witnessSet,
    fieldItemCbors,
    artifact: {
      headerHash: forced.headerHash,
      nativeTxId: forced.transactionId,
      nativeTxCompactCbor: evidence.nativeTxCompactCbor,
      txMembershipProofCbor: "",
    },
    fieldPlans: fieldItemCbors.map((items, fieldIndex) =>
      planFaultProofFieldOpening({
        anchorSourceKind: 1n,
        fieldIndex,
        anchorTxId: forced.transactionId,
        nativeTxCompactCbor: evidence.nativeTxCompactCbor,
        itemCbors: items,
        owner,
        publish: true,
        ...(fieldIndex < 6
          ? {}
          : {
              witnessSet: evidence.witnessSet,
              anchorWitnessSetHash: evidence.state.bad_tx.witness_set_hash,
            }),
        label: `min-fee forced field ${fieldIndex}`,
      }),
    ),
  };
};

const selectedTxId = (
  classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
): string => {
  if (
    classification.category !== "minFee" ||
    classification.selected.violationId !== MIN_FEE_VIOLATION_ID
  ) {
    throw new Error("min-fee workflow received another classification");
  }
  const fields = classification.selected.detectionId.split(":");
  if (
    fields.length !== 5 ||
    fields[0] !== MIN_FEE_VIOLATION_ID ||
    !NATURAL.test(fields[1] ?? "") ||
    !HEX_32.test(fields[2] ?? "") ||
    !NATURAL.test(fields[3] ?? "") ||
    !NATURAL.test(fields[4] ?? "") ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("min-fee classification identity is malformed");
  }
  return fields[2]!;
};

export const prepareMinFeeArtifact = async ({
  evidence,
  classification,
  categoryId,
}: CanonicalEvidenceBuilderInput & {
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
  readonly categoryId: string;
}): Promise<MinFeeArtifact> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("min-fee classification differs from canonical evidence");
  }
  const prepared = await prepareMinFeeFromCanonicalEvidence({
    evidence,
    txId: selectedTxId(classification),
    categoryId,
  });
  const detectionId = `${MIN_FEE_VIOLATION_ID}:${classification.selected.position.toString()}:${prepared.tx.nodeTxId}:${prepared.tx.fee.toString()}:${prepared.tx.minimumFee.toString()}`;
  if (classification.selected.detectionId !== detectionId) {
    throw new Error("min-fee prepared evidence changed classification");
  }
  const artifact = normalizeJournalJson({
    schemaVersion: MIN_FEE_ARTIFACT,
    headerHash: prepared.headerHash,
    detectionId,
    position: Number(classification.selected.position),
    nativeTxId: prepared.tx.nodeTxId,
    nativeTxCompactCbor: prepared.tx.nativeTxCompactCbor,
    l2TransactionSourceCbor: prepared.tx.txInclusion.l2TransactionSourceCbor,
    transactionsPhasRoot: prepared.transactionsPhasRoot,
    txMembershipProofCbor: prepared.tx.txInclusion.txMembershipProofCbor,
    witnessSet: prepared.tx.witnessSet,
    fieldItemCbors: prepared.tx.fieldItemCbors,
    minFeeA: prepared.minFeeA.toString(),
    minFeeB: prepared.minFeeB.toString(),
    fee: prepared.tx.fee.toString(),
    canonicalTxSize: prepared.tx.canonicalTxSize.toString(),
    minimumFee: prepared.tx.minimumFee.toString(),
    shortfall: prepared.tx.shortfall.toString(),
  }) as MinFeeArtifact;
  admitMinFeeArtifact(artifact);
  return Object.freeze(artifact);
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
  "chunkedVerifyWithdraw",
] as const;

export type AssemblyContext = LinearFamilyAssemblyContext<
  "minFee",
  (typeof WITNESS_ROLES)[number],
  true
>;

export type MinFeeWorkflowReferenceScripts = LinearFamilyReferenceScripts<
  "minFee",
  (typeof WITNESS_ROLES)[number],
  true
>;

export type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"minFee">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: MinFeeContracts;
  category: FraudProofWorkflowDeploymentBinding<"minFee">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"minFee">["catalogue"];
  referenceScripts: MinFeeWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

export const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "min-fee workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "minFee" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("min-fee workflow action changed identity");
  }
  return input;
};

export const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`min-fee workflow action omitted ${field}`);
  }
  return value;
};

export const captureRemoval = async ({
  config,
  input,
}: {
  readonly config: BoundConfig;
  readonly input: Readonly<Record<string, unknown>>;
}) => {
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
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        fraudCategory: "minFee",
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
              "min-fee removal changed its authenticated queue input",
            );
          }
          if (
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "min-fee removal omitted its authenticated proof token",
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
};
