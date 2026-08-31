import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
} from "@al-ft/midgard-core";
import {
  encodeMidgardAddressWitnessCanonicalV1,
  FraudProofComputationThreadStepDatum,
  INVALID_SIGNATURE_VIOLATION_ID_V1,
  invalidSignatureAddressWitnessesCommitmentV1,
  InvalidSignatureStep02Datum,
  invalidSignatureWitnessSetCommitmentV1,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardAddressWitness,
  type NativeTxWitnessSetCompact,
  verifyAddressWitness,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { prepareInvalidSignatureFromCanonicalEvidenceV1 } from "../evidence/prepare-from-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlanV1,
  planFaultProofFieldOpeningV1,
  resolveFaultProofFieldCarriagePublicationsV1,
  resolveFaultProofFieldPreimageCertificateV1,
} from "../field-opening-v1.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitInvalidSignatureStep01 } from "../submit-invalid-signature-step-01.js";
import { submitInvalidSignatureStep02 } from "../submit-invalid-signature-step-02.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassificationV1 } from "./classification-v1.js";
import { INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1 } from "./complete-replay-v1.js";
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
  createAuthenticatedFieldCarriagePrerequisitePortV1,
  type ProductionFieldCarriageRequirementV1,
  withProductionFieldCarriagePrerequisiteV1,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  createProductionLinearFamilyWorkflowAdapterV1,
  PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  type ProductionLinearFamilyTransactionPortV1,
} from "./production-linear-family-adapter-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePortV1,
  resolveDirectFirstProofChunksV1,
  withProductionProofChunkPrerequisiteV1,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthorityV1 } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransactionV1,
  workflowTransactionInputOutRefsV1,
  workflowTransactionReferenceInputOutRefsV1,
} from "./transaction-boundary-v1.js";

export const PRODUCTION_INVALID_SIGNATURE_ARTIFACT_V1 =
  "midgard-production-invalid-signature-artifact-v1" as const;

export type ProductionInvalidSignatureArtifactV1 = JournalJsonObjectV1 &
  Readonly<{
    schemaVersion: typeof PRODUCTION_INVALID_SIGNATURE_ARTIFACT_V1;
    headerHash: string;
    detectionId: string;
    position: number;
    nativeTxId: string;
    nativeTxCompactCbor: string;
    l2TransactionSourceCbor: string;
    transactionsPhasRoot: string;
    txMembershipProofCbor: string;
    witnessSet: Readonly<{
      addr_tx_wits_hash: string;
      script_tx_wits_hash: string;
      redeemer_tx_wits_hash: string;
    }>;
    addressWitnesses: readonly Readonly<{
      verification_key: string;
      signature: string;
    }>[];
    badWitnessIndex: number;
  }>;

type AdmittedArtifactV1 = Readonly<{
  artifact: ProductionInvalidSignatureArtifactV1;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  witnessSet: NativeTxWitnessSetCompact;
  addressWitnesses: readonly MidgardAddressWitness[];
  fieldPlan: FaultProofFieldOpeningPlanV1;
}>;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_64 = /^[0-9a-f]{128}$/u;
const EVEN_HEX = /^(?:[0-9a-f]{2})+$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

const hex = (value: unknown, pattern: RegExp, label: string): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is not canonical lowercase hex`);
  }
  return value;
};

const safeNatural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

const parseWitnessSet = (value: unknown): NativeTxWitnessSetCompact => {
  const parsed = exact(
    value,
    ["addr_tx_wits_hash", "script_tx_wits_hash", "redeemer_tx_wits_hash"],
    "invalid-signature witness set",
  );
  return Object.freeze({
    addr_tx_wits_hash: hex(
      parsed.addr_tx_wits_hash,
      HEX_32,
      "address-witness hash",
    ),
    script_tx_wits_hash: hex(
      parsed.script_tx_wits_hash,
      HEX_32,
      "script-witness hash",
    ),
    redeemer_tx_wits_hash: hex(
      parsed.redeemer_tx_wits_hash,
      HEX_32,
      "redeemer-witness hash",
    ),
  });
};

const parseAddressWitnesses = (
  value: unknown,
): readonly MidgardAddressWitness[] => {
  if (!Array.isArray(value)) {
    throw new Error("invalid-signature address witnesses must be an array");
  }
  return Object.freeze(
    value.map((item, index) => {
      const parsed = exact(
        item,
        ["verification_key", "signature"],
        `invalid-signature address witness ${index.toString()}`,
      );
      return Object.freeze({
        verification_key: hex(
          parsed.verification_key,
          HEX_32,
          `address witness ${index.toString()} verification key`,
        ),
        signature: hex(
          parsed.signature,
          HEX_64,
          `address witness ${index.toString()} signature`,
        ),
      });
    }),
  );
};

const proofSteps = (
  proof: ReturnType<typeof parseSubmitStep01TxInclusion>["txMembershipProof"],
) =>
  proof.map((step) => {
    if ("Branch" in step) {
      return {
        type: "branch" as const,
        skip: Number(step.Branch.skip),
        neighbors: step.Branch.neighbors,
      };
    }
    if ("Fork" in step) {
      return {
        type: "fork" as const,
        skip: Number(step.Fork.skip),
        neighbor: {
          nibble: Number(step.Fork.neighbor.nibble),
          prefix: step.Fork.neighbor.prefix,
          root: step.Fork.neighbor.root,
        },
      };
    }
    return {
      type: "leaf" as const,
      skip: Number(step.Leaf.skip),
      neighbor: { key: step.Leaf.key, value: step.Leaf.value },
    };
  });

const witnessSetCbor = (witnessSet: NativeTxWitnessSetCompact): string =>
  encodeMidgardNativeTxWitnessSetCompactV1({
    addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
    scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
    redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
  }).toString("hex");

export const admitProductionInvalidSignatureArtifactV1 = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifactV1 => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("invalid-signature carriage owner is malformed");
  }
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "transactionsPhasRoot",
      "txMembershipProofCbor",
      "witnessSet",
      "addressWitnesses",
      "badWitnessIndex",
    ],
    "invalid-signature artifact",
  );
  if (
    parsed.schemaVersion !== PRODUCTION_INVALID_SIGNATURE_ARTIFACT_V1 ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("invalid-signature artifact identity changed");
  }
  const witnessSet = parseWitnessSet(parsed.witnessSet);
  const addressWitnesses = parseAddressWitnesses(parsed.addressWitnesses);
  const artifact = Object.freeze({
    schemaVersion: PRODUCTION_INVALID_SIGNATURE_ARTIFACT_V1,
    headerHash: hex(parsed.headerHash, HEX_28, "artifact header hash"),
    detectionId: parsed.detectionId,
    position: safeNatural(parsed.position, "artifact position"),
    nativeTxId: hex(parsed.nativeTxId, HEX_32, "artifact transaction id"),
    nativeTxCompactCbor: hex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      "artifact compact transaction",
    ),
    l2TransactionSourceCbor: hex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      "artifact transaction source",
    ),
    transactionsPhasRoot: hex(
      parsed.transactionsPhasRoot,
      HEX_32,
      "artifact transactions PHAS root",
    ),
    txMembershipProofCbor: hex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      "artifact membership proof",
    ),
    witnessSet,
    addressWitnesses,
    badWitnessIndex: safeNatural(
      parsed.badWitnessIndex,
      "artifact bad witness index",
    ),
  }) satisfies ProductionInvalidSignatureArtifactV1;
  const inclusion = parseSubmitStep01TxInclusion({
    nativeTxId: artifact.nativeTxId,
    nativeTx: nativeTxFromCoreCompact(
      decodeMidgardNativeTxCompactV1(
        Buffer.from(artifact.nativeTxCompactCbor, "hex"),
      ),
    ),
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    l2TransactionSourceCbor: artifact.l2TransactionSourceCbor,
    transactionsPhasRoot: artifact.transactionsPhasRoot,
    txMembershipProofCbor: artifact.txMembershipProofCbor,
  });
  let openedRoot: Buffer | null;
  try {
    openedRoot = MpfProof.fromJSON(
      Buffer.from(artifact.nativeTxId, "hex"),
      Buffer.from(artifact.l2TransactionSourceCbor, "hex"),
      proofSteps(inclusion.txMembershipProof),
    ).verify(true);
  } catch {
    throw new Error("invalid-signature membership proof cannot be replayed");
  }
  if (
    openedRoot === null ||
    openedRoot.toString("hex") !== artifact.transactionsPhasRoot
  ) {
    throw new Error(
      "invalid-signature membership proof does not open its PHAS root",
    );
  }
  if (
    invalidSignatureWitnessSetCommitmentV1(witnessSet) !==
      inclusion.nativeTx.witness_set_hash ||
    invalidSignatureAddressWitnessesCommitmentV1(addressWitnesses) !==
      witnessSet.addr_tx_wits_hash
  ) {
    throw new Error(
      "invalid-signature witness material does not open the committed transaction",
    );
  }
  const badWitness = addressWitnesses[artifact.badWitnessIndex];
  if (
    badWitness === undefined ||
    verifyAddressWitness({ txId: artifact.nativeTxId, witness: badWitness }) ||
    artifact.detectionId !==
      `${INVALID_SIGNATURE_VIOLATION_ID_V1}:${artifact.position.toString()}:${artifact.badWitnessIndex.toString()}:${artifact.nativeTxId}:${badWitness.verification_key}`
  ) {
    throw new Error(
      "invalid-signature artifact does not re-derive its selected violation",
    );
  }
  const fieldPlan = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.addressWitnesses,
    anchorTxId: artifact.nativeTxId,
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    itemCbors: addressWitnesses.map(encodeMidgardAddressWitnessCanonicalV1),
    owner: carriageOwner,
    publish: false,
    witnessSet,
    anchorWitnessSetHash: inclusion.nativeTx.witness_set_hash,
    label: "invalid-signature artifact address witnesses",
  });
  return Object.freeze({
    artifact,
    inclusion,
    witnessSet,
    addressWitnesses,
    fieldPlan,
  });
};

const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  if (
    classification.category !== "invalidSignature" ||
    classification.selected.violationId !== INVALID_SIGNATURE_VIOLATION_ID_V1 ||
    fields.length !== 5 ||
    fields[0] !== INVALID_SIGNATURE_VIOLATION_ID_V1 ||
    !NATURAL.test(fields[1] ?? "") ||
    !NATURAL.test(fields[2] ?? "") ||
    !HEX_32.test(fields[3] ?? "") ||
    !HEX_32.test(fields[4] ?? "") ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("invalid-signature classification identity is malformed");
  }
  return Object.freeze({
    transactionIndex: Number(fields[1]),
    witnessIndex: Number(fields[2]),
    txId: fields[3]!,
    verificationKey: fields[4]!,
  });
};

export const prepareProductionInvalidSignatureArtifactV1 = async ({
  evidence,
  classification,
}: {
  readonly evidence: Parameters<
    typeof prepareInvalidSignatureFromCanonicalEvidenceV1
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassificationV1,
    { readonly decision: "fault_detected" }
  >;
}): Promise<ProductionInvalidSignatureArtifactV1> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "invalid-signature classification differs from canonical evidence",
    );
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareInvalidSignatureFromCanonicalEvidenceV1({
    evidence,
    txId: selected.txId,
  });
  if (
    prepared.tx.badAddrTxWitIndex !== selected.witnessIndex ||
    prepared.tx.badAddrTxWitVerificationKey !== selected.verificationKey
  ) {
    throw new Error(
      "invalid-signature prepared evidence changed the selected witness",
    );
  }
  const artifact = normalizeJournalJsonV1({
    schemaVersion: PRODUCTION_INVALID_SIGNATURE_ARTIFACT_V1,
    headerHash: prepared.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.transactionIndex,
    nativeTxId: prepared.tx.nodeTxId,
    nativeTxCompactCbor: prepared.tx.nativeTxCompactCbor,
    l2TransactionSourceCbor: prepared.tx.txInclusion.l2TransactionSourceCbor,
    transactionsPhasRoot: prepared.transactionsPhasRoot,
    txMembershipProofCbor: prepared.tx.txInclusion.txMembershipProofCbor,
    witnessSet: prepared.tx.badTxWitnessSetCompact,
    addressWitnesses: prepared.tx.addrTxWitsPreimage,
    badWitnessIndex: prepared.tx.badAddrTxWitIndex,
  }) as ProductionInvalidSignatureArtifactV1;
  admitProductionInvalidSignatureArtifactV1(artifact);
  return Object.freeze(artifact);
};

export type InvalidSignatureWorkflowReferenceScriptsV1 = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScriptsV1 & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfigV1 = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBindingV1<"invalidSignature">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: InvalidSignatureWorkflowReferenceScriptsV1;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBindingV1<"invalidSignature">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowActionV1,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "invalid-signature workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "invalidSignature" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("invalid-signature workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`invalid-signature workflow action omitted ${field}`);
  }
  return value;
};

const captureRemoval = async (
  config: BoundConfigV1,
  input: Readonly<Record<string, unknown>>,
) => {
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
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        fraudCategory: "invalidSignature",
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefsV1(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefsV1(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "invalid-signature removal changed its authenticated queue/proof inputs",
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

const resolveFieldCarriage = async (
  config: BoundConfigV1,
  admitted: AdmittedArtifactV1,
) => {
  const publications = await resolveFaultProofFieldCarriagePublicationsV1({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: admitted.fieldPlan,
  });
  if (publications === undefined) {
    throw new Error(
      "invalid-signature field publications disappeared after authenticated prerequisite",
    );
  }
  const certificate = await resolveFaultProofFieldPreimageCertificateV1({
    lucid: config.lucid,
    network: config.network,
    planned: admitted.fieldPlan,
    certificatePolicyId: config.certificate.policyId,
  });
  if (
    admitted.fieldPlan.plan.tier === "Certified" &&
    certificate === undefined
  ) {
    throw new Error(
      "invalid-signature field certificate disappeared after authenticated prerequisite",
    );
  }
  return Object.freeze({
    publications,
    certificates: certificate === undefined ? [] : [certificate],
  });
};

const createTransactionPort = (
  config: BoundConfigV1,
): ProductionLinearFamilyTransactionPortV1<"invalidSignature"> => ({
  portVersion: PRODUCTION_LINEAR_FAMILY_TRANSACTION_PORT_V1,
  category: "invalidSignature",
  prepare: async ({ evidence, classification }) =>
    await prepareProductionInvalidSignatureArtifactV1({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitProductionInvalidSignatureArtifactV1(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "invalid-signature artifact changed its manifest-bound header",
      );
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              fraudCategory: "invalidSignature",
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const chunks = await resolveDirectFirstProofChunksV1({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInvalidSignatureStep01({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.inclusion,
              badTxWitnessSetCompact: admitted.witnessSet,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveFieldCarriage(config, admitted);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransactionV1(
          async (preSubmitBoundary) => {
            await submitInvalidSignatureStep02({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              addrTxWitsPreimage: admitted.addressWitnesses,
              nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
              witnessSetCompact: admitted.witnessSet,
              badAddrTxWitIndex: BigInt(admitted.artifact.badWitnessIndex),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              certificatePolicyId: config.certificate.policyId,
              certificateUtxos: carriage.certificates,
              existingPublicationUtxos: carriage.publications,
              publishMissingCarriage: false,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `invalid-signature workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundInvalidSignatureWorkflowConfigV1 = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: InvalidSignatureWorkflowReferenceScriptsV1;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfigV1, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundInvalidSignatureWorkflowV1 = Readonly<{
  binding: FraudProofWorkflowDeploymentBindingV1<"invalidSignature">;
  l1: FraudProofFamilyL1ObservationPortV1<"invalidSignature">;
  transactions: ProductionLinearFamilyTransactionPortV1<"invalidSignature">;
  adapter: FraudProofFamilyWorkflowAdapterV1;
  terminalVerifier: FraudProofWorkflowTerminalVerifierV1;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthorityV1;
}>;

export const createManifestBoundInvalidSignatureWorkflowV1 = async (
  config: ManifestBoundInvalidSignatureWorkflowConfigV1,
): Promise<ManifestBoundInvalidSignatureWorkflowV1> => {
  const binding = await bindFraudProofWorkflowDeploymentV1({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "invalidSignature",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      InvalidSignatureStep02Datum,
    ],
  });
  assertManifestBoundWorkflowSignerV1({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error(
      "invalid-signature manifest omitted field-preimage certificate policy",
    );
  }
  const certificate = binding.fieldPreimageCertificate;
  const references: InvalidSignatureWorkflowReferenceScriptsV1 = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofInvalidSignature",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "fraudProofInvalidSignatureStep02",
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
      chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxoV1({
        binding,
        contractName: "chunkedVerifyWithdraw",
        utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
      }),
    }),
    fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxoV1({
      binding,
      contractName: "fieldPreimageCertificateMint",
      utxo: config.referenceScripts.fieldPreimageCertificateMint,
    }),
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPortV1({
    source: config.source,
    releaseFinality: binding.releaseFinality,
    releaseEconomics: binding.releaseEconomics,
    definition: binding.definition,
  });
  if (l1.publications === undefined) {
    throw new Error(
      "invalid-signature raw-L1 authority omitted publication observer",
    );
  }
  const transactions = createTransactionPort({
    lucid: config.lucid,
    blueprint: binding.blueprint,
    deploymentInfo: binding.deploymentInfo,
    network: binding.network,
    signer: config.signer,
    headerHash: binding.definition.headerHash,
    referenceScripts: references,
    certificate,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
    fraudProverRewardLovelace: BigInt(
      binding.releaseEconomics.policy.fraudProverRewardLovelace,
    ),
  });
  let adapter = createProductionLinearFamilyWorkflowAdapterV1({
    category: "invalidSignature",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePortV1({
    category: "invalidSignature",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      const input = record(
        action.input,
        "invalid-signature field prerequisite action",
      );
      if (input.stage !== "step_02") return null;
      const admitted = admitProductionInvalidSignatureArtifactV1(
        artifact,
        config.signer.paymentKeyHash,
      );
      return {
        planned: admitted.fieldPlan,
        compactCbor: admitted.artifact.nativeTxCompactCbor,
        witnessSetCompactCbor: witnessSetCbor(admitted.witnessSet),
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
    category: "invalidSignature",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePortV1({
    category: "invalidSignature",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const input = record(
        action.input,
        "invalid-signature proof prerequisite action",
      );
      return input.stage === "step_01"
        ? admitProductionInvalidSignatureArtifactV1(
            artifact,
            config.signer.paymentKeyHash,
          ).artifact.txMembershipProofCbor
        : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProductionProofChunkPrerequisiteV1({
    category: "invalidSignature",
    base: adapter,
    prerequisite: proofPrerequisite,
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

export const runOrResumeManifestBoundInvalidSignatureWorkflowV1 = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundInvalidSignatureWorkflowV1;
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
    replayer: INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY_V1,
    registry: createFraudProofWorkflowRegistryV1({
      adapters: [workflow.adapter],
      launchScope: ["invalidSignature"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
