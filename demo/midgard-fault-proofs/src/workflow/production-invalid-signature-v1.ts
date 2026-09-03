import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardNativeTxCompact,
  encodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";
import {
  encodeMidgardAddressWitnessCanonical,
  FraudProofComputationThreadStepDatum,
  INVALID_SIGNATURE_VIOLATION_ID,
  invalidSignatureAddressWitnessesCommitment,
  InvalidSignatureStep02Datum,
  invalidSignatureWitnessSetCommitment,
  MIDGARD_FIELD_INDEX,
  type MidgardAddressWitness,
  type NativeTxWitnessSetCompact,
  verifyAddressWitness,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { prepareInvalidSignatureFromCanonicalEvidence } from "../evidence/prepare-from-evidence-v1.js";
import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
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
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { CanonicalBlockClassification } from "./classification-v1.js";
import { INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay-v1.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding-v1.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation-v1.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal-v1.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source-v1.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator-v1.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "./production-field-carriage-prerequisite-v1.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./production-linear-family-adapter-v1.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "./production-proof-chunk-prerequisite-v1.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy-v1.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary-v1.js";

export const INVALID_SIGNATURE_ARTIFACT =
  "midgard-production-invalid-signature-artifact-v1" as const;

export type InvalidSignatureArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof INVALID_SIGNATURE_ARTIFACT;
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

type AdmittedArtifact = Readonly<{
  artifact: InvalidSignatureArtifact;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  witnessSet: NativeTxWitnessSetCompact;
  addressWitnesses: readonly MidgardAddressWitness[];
  fieldPlan: FaultProofFieldOpeningPlan;
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
  encodeMidgardNativeTxWitnessSetCompact({
    addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
    scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
    redeemerTxWitsHash: Buffer.from(witnessSet.redeemer_tx_wits_hash, "hex"),
  }).toString("hex");

export const admitInvalidSignatureArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifact => {
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
    parsed.schemaVersion !== INVALID_SIGNATURE_ARTIFACT ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("invalid-signature artifact identity changed");
  }
  const witnessSet = parseWitnessSet(parsed.witnessSet);
  const addressWitnesses = parseAddressWitnesses(parsed.addressWitnesses);
  const artifact = Object.freeze({
    schemaVersion: INVALID_SIGNATURE_ARTIFACT,
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
  }) satisfies InvalidSignatureArtifact;
  const inclusion = parseSubmitStep01TxInclusion({
    nativeTxId: artifact.nativeTxId,
    nativeTx: nativeTxFromCoreCompact(
      decodeMidgardNativeTxCompact(
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
    invalidSignatureWitnessSetCommitment(witnessSet) !==
      inclusion.nativeTx.witness_set_hash ||
    invalidSignatureAddressWitnessesCommitment(addressWitnesses) !==
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
      `${INVALID_SIGNATURE_VIOLATION_ID}:${artifact.position.toString()}:${artifact.badWitnessIndex.toString()}:${artifact.nativeTxId}:${badWitness.verification_key}`
  ) {
    throw new Error(
      "invalid-signature artifact does not re-derive its selected violation",
    );
  }
  const fieldPlan = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.addressWitnesses,
    anchorTxId: artifact.nativeTxId,
    nativeTxCompactCbor: artifact.nativeTxCompactCbor,
    itemCbors: addressWitnesses.map(encodeMidgardAddressWitnessCanonical),
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
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  if (
    classification.category !== "invalidSignature" ||
    classification.selected.violationId !== INVALID_SIGNATURE_VIOLATION_ID ||
    fields.length !== 5 ||
    fields[0] !== INVALID_SIGNATURE_VIOLATION_ID ||
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

export const prepareInvalidSignatureArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: Parameters<
    typeof prepareInvalidSignatureFromCanonicalEvidence
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<InvalidSignatureArtifact> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(
      "invalid-signature classification differs from canonical evidence",
    );
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareInvalidSignatureFromCanonicalEvidence({
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
  const artifact = normalizeJournalJson({
    schemaVersion: INVALID_SIGNATURE_ARTIFACT,
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
  }) as InvalidSignatureArtifact;
  admitInvalidSignatureArtifact(artifact);
  return Object.freeze(artifact);
};

export type InvalidSignatureWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO];
  witnesses: FaultProofWitnessReferenceScripts & {
    readonly computationThreadMint: UTxO;
    readonly fraudProofMint: UTxO;
    readonly phasMembershipWithdraw: UTxO;
    readonly chunkedVerifyWithdraw: UTxO;
  };
  fieldPreimageCertificateMint: UTxO;
}>;

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"invalidSignature">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: InvalidSignatureWorkflowReferenceScripts;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBinding<"invalidSignature">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowAction,
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
  config: BoundConfig,
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
  const transaction = await captureLocallyEvaluatedTransaction(
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
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
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
  config: BoundConfig,
  admitted: AdmittedArtifact,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: admitted.fieldPlan,
  });
  if (publications === undefined) {
    throw new Error(
      "invalid-signature field publications disappeared after authenticated prerequisite",
    );
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
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
  config: BoundConfig,
): LinearFamilyTransactionPort<"invalidSignature"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "invalidSignature",
  prepare: async ({ evidence, classification }) =>
    await prepareInvalidSignatureArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = admitInvalidSignatureArtifact(
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
        transaction: await captureLocallyEvaluatedTransaction(
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
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
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
        transaction: await captureLocallyEvaluatedTransaction(
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

export type ManifestBoundInvalidSignatureWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: InvalidSignatureWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundInvalidSignatureWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"invalidSignature">;
  l1: FraudProofFamilyL1ObservationPort<"invalidSignature">;
  transactions: LinearFamilyTransactionPort<"invalidSignature">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundInvalidSignatureWorkflow = async (
  config: ManifestBoundInvalidSignatureWorkflowConfig,
): Promise<ManifestBoundInvalidSignatureWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
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
  assertManifestBoundWorkflowSigner({
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
  const references: InvalidSignatureWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze([
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofInvalidSignature",
        utxo: config.referenceScripts.steps[0],
      }),
      requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofInvalidSignatureStep02",
        utxo: config.referenceScripts.steps[1],
      }),
    ] as const),
    witnesses: Object.freeze({
      computationThreadMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "computationThreadMint",
        utxo: config.referenceScripts.witnesses.computationThreadMint,
      }),
      fraudProofMint: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "fraudProofMint",
        utxo: config.referenceScripts.witnesses.fraudProofMint,
      }),
      phasMembershipWithdraw: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "phasMembershipWithdraw",
        utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
      }),
      chunkedVerifyWithdraw: requireManifestBoundReferenceScriptUtxo({
        binding,
        contractName: "chunkedVerifyWithdraw",
        utxo: config.referenceScripts.witnesses.chunkedVerifyWithdraw,
      }),
    }),
    fieldPreimageCertificateMint: requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName: "fieldPreimageCertificateMint",
      utxo: config.referenceScripts.fieldPreimageCertificateMint,
    }),
  });
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
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
  let adapter = createLinearFamilyWorkflowAdapter({
    category: "invalidSignature",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
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
      const admitted = admitInvalidSignatureArtifact(
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
      } satisfies FieldCarriageRequirement;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withFieldCarriagePrerequisite({
    category: "invalidSignature",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
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
        ? admitInvalidSignatureArtifact(artifact, config.signer.paymentKeyHash)
            .artifact.txMembershipProofCbor
        : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProofChunkPrerequisite({
    category: "invalidSignature",
    base: adapter,
    prerequisite: proofPrerequisite,
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

export const runOrResumeManifestBoundInvalidSignatureWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundInvalidSignatureWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  const observation = await workflow.l1.observeHeader({
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: INVALID_SIGNATURE_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["invalidSignature"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
