import { Proof as MpfProof } from "@aiken-lang/merkle-patricia-forestry";
import { decodeMidgardNativeTxCompact } from "@al-ft/midgard-core";
import {
  encodeMidgardTxInputCanonical,
  encodeMidgardTxOutputCanonical,
  FraudProofComputationThreadStepDatum,
  INPUT_NO_IDX_VIOLATION_ID,
  InputNoIdxStep02Datum,
  InputNoIdxStep03Datum,
  InputNoIdxStep04Datum,
  MIDGARD_FIELD_INDEX,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import {
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { prepareInputNoIdxFromCanonicalEvidence } from "../prepare-input-no-idx.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitInputNoIdxStep01 } from "../submit-input-no-idx-step-01.js";
import {
  parseSubmitInputNoIdxInputsPreimage,
  type SubmitInputNoIdxInputsPreimage,
  submitInputNoIdxStep02,
} from "../submit-input-no-idx-step-02.js";
import { submitInputNoIdxStep03 } from "../submit-input-no-idx-step-03.js";
import {
  parseSubmitInputNoIdxOutputsPreimage,
  type SubmitInputNoIdxOutputsPreimage,
  submitInputNoIdxStep04,
} from "../submit-input-no-idx-step-04.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { CanonicalBlockClassification } from "./classification.js";
import { INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
  type FraudProofWorkflowDeploymentBinding,
  releaseFinalityAuthorityFromDeploymentBinding,
  requireManifestBoundReferenceScriptUtxo,
} from "./deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "./family-l1-observation.js";
import {
  createAuthenticatedFieldCarriagePrerequisitePort,
  type FieldCarriageRequirement,
  withFieldCarriagePrerequisite,
} from "./field-carriage-prerequisite.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import {
  createLinearFamilyWorkflowAdapter,
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "./local-kupmios-http-ogmios-source.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowAction,
  type FraudProofWorkflowRunResult,
  type FraudProofWorkflowTerminalVerifier,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import {
  createAuthenticatedProofChunkPrerequisitePort,
  resolveDirectFirstProofChunks,
  withProofChunkPrerequisite,
} from "./proof-chunk-prerequisite.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

export const INPUT_NO_IDX_ARTIFACT =
  "midgard-production-input-no-idx-artifact-v1" as const;

type InclusionJson = Readonly<{
  nativeTxId: string;
  nativeTxCompactCbor: string;
  l2TransactionSourceCbor: string;
  transactionsPhasRoot: string;
  txMembershipProofCbor: string;
}>;

export type InputNoIdxArtifact = JournalJsonObject &
  Readonly<{
    schemaVersion: typeof INPUT_NO_IDX_ARTIFACT;
    headerHash: string;
    detectionId: string;
    position: number;
    badTx: InclusionJson;
    producingTx: InclusionJson;
    inputs: readonly Readonly<{ tx_id: string; output_index: string }>[];
    badInputsIndex: number;
    outputsPreimageCbor: readonly string[];
    badInputOutputIndex: string;
  }>;

type AdmittedArtifact = Readonly<{
  artifact: InputNoIdxArtifact;
  badInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  producingInclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
  inputs: SubmitInputNoIdxInputsPreimage;
  outputs: SubmitInputNoIdxOutputsPreimage;
  inputFieldPlan: FaultProofFieldOpeningPlan;
  outputFieldPlan: FaultProofFieldOpeningPlan;
}>;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
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

const naturalString = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !NATURAL.test(value)) {
    throw new Error(`${label} is not a canonical natural decimal`);
  }
  return value;
};

const safeNatural = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
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

const parseInclusion = (
  value: unknown,
  label: string,
): Readonly<{
  artifact: InclusionJson;
  inclusion: ReturnType<typeof parseSubmitStep01TxInclusion>;
}> => {
  const parsed = exact(
    value,
    [
      "nativeTxId",
      "nativeTxCompactCbor",
      "l2TransactionSourceCbor",
      "transactionsPhasRoot",
      "txMembershipProofCbor",
    ],
    label,
  );
  const artifact = Object.freeze({
    nativeTxId: hex(parsed.nativeTxId, HEX_32, `${label} tx id`),
    nativeTxCompactCbor: hex(
      parsed.nativeTxCompactCbor,
      EVEN_HEX,
      `${label} compact tx`,
    ),
    l2TransactionSourceCbor: hex(
      parsed.l2TransactionSourceCbor,
      EVEN_HEX,
      `${label} source`,
    ),
    transactionsPhasRoot: hex(
      parsed.transactionsPhasRoot,
      HEX_32,
      `${label} root`,
    ),
    txMembershipProofCbor: hex(
      parsed.txMembershipProofCbor,
      EVEN_HEX,
      `${label} proof`,
    ),
  });
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
    throw new Error(`${label} membership proof cannot be replayed`);
  }
  if (
    openedRoot === null ||
    openedRoot.toString("hex") !== artifact.transactionsPhasRoot
  ) {
    throw new Error(`${label} membership proof does not open its PHAS root`);
  }
  return Object.freeze({ artifact, inclusion });
};

const parseInputs = (
  value: unknown,
  badInputsIndex: number,
): Readonly<{
  json: readonly Readonly<{ tx_id: string; output_index: string }>[];
  parsed: SubmitInputNoIdxInputsPreimage;
}> => {
  if (!Array.isArray(value)) {
    throw new Error("input-no-idx artifact inputs must be an array");
  }
  const json = Object.freeze(
    value.map((entry, index) => {
      const parsed = exact(
        entry,
        ["tx_id", "output_index"],
        `input-no-idx artifact input ${index.toString()}`,
      );
      return Object.freeze({
        tx_id: hex(parsed.tx_id, HEX_32, "input transaction id"),
        output_index: naturalString(parsed.output_index, "input output index"),
      });
    }),
  );
  return Object.freeze({
    json,
    parsed: parseSubmitInputNoIdxInputsPreimage({
      inputsPreimage: json,
      badInputsIndex,
    }),
  });
};

const parseOutputCbors = (
  value: unknown,
): Readonly<{
  json: readonly string[];
  parsed: SubmitInputNoIdxOutputsPreimage;
}> => {
  if (!Array.isArray(value)) {
    throw new Error("input-no-idx artifact outputs must be an array");
  }
  const json = Object.freeze(
    value.map((item, index) =>
      hex(item, EVEN_HEX, `input-no-idx output ${index.toString()}`),
    ),
  );
  return Object.freeze({
    json,
    parsed: parseSubmitInputNoIdxOutputsPreimage({
      outputsPreimageCbor: json,
    }),
  });
};

export const admitInputNoIdxArtifact = (
  value: unknown,
  carriageOwner = "00".repeat(28),
): AdmittedArtifact => {
  if (!HEX_28.test(carriageOwner)) {
    throw new Error("input-no-idx carriage owner is malformed");
  }
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "headerHash",
      "detectionId",
      "position",
      "badTx",
      "producingTx",
      "inputs",
      "badInputsIndex",
      "outputsPreimageCbor",
      "badInputOutputIndex",
    ],
    "input-no-idx artifact",
  );
  if (
    parsed.schemaVersion !== INPUT_NO_IDX_ARTIFACT ||
    typeof parsed.detectionId !== "string" ||
    parsed.detectionId.trim() !== parsed.detectionId
  ) {
    throw new Error("input-no-idx artifact identity changed");
  }
  const headerHash = hex(parsed.headerHash, HEX_28, "artifact header hash");
  const position = safeNatural(parsed.position, "artifact position");
  const badInputsIndex = safeNatural(
    parsed.badInputsIndex,
    "artifact bad input index",
  );
  const badInputOutputIndex = naturalString(
    parsed.badInputOutputIndex,
    "artifact bad input output index",
  );
  const bad = parseInclusion(parsed.badTx, "input-no-idx bad transaction");
  const producing = parseInclusion(
    parsed.producingTx,
    "input-no-idx producing transaction",
  );
  if (
    bad.artifact.transactionsPhasRoot !==
    producing.artifact.transactionsPhasRoot
  ) {
    throw new Error(
      "input-no-idx inclusions do not share one transactions root",
    );
  }
  const inputs = parseInputs(parsed.inputs, badInputsIndex);
  const outputs = parseOutputCbors(parsed.outputsPreimageCbor);
  const badInput = inputs.parsed.inputsPreimage[badInputsIndex];
  if (
    badInput === undefined ||
    badInput.tx_id !== producing.artifact.nativeTxId ||
    badInput.output_index.toString() !== badInputOutputIndex ||
    badInput.output_index < BigInt(outputs.parsed.outputsPreimage.length)
  ) {
    throw new Error("input-no-idx artifact does not re-derive its violation");
  }
  const expectedDetection = `${INPUT_NO_IDX_VIOLATION_ID}:${position.toString()}:${badInputsIndex.toString()}:${bad.artifact.nativeTxId}:${producing.artifact.nativeTxId}:${badInputOutputIndex}:${outputs.parsed.outputsPreimage.length.toString()}`;
  if (parsed.detectionId !== expectedDetection) {
    throw new Error("input-no-idx artifact detection identity changed");
  }
  const inputFieldPlan = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: bad.artifact.nativeTxId,
    nativeTxCompactCbor: bad.artifact.nativeTxCompactCbor,
    itemCbors: inputs.parsed.inputsPreimage.map(encodeMidgardTxInputCanonical),
    owner: carriageOwner,
    label: "input-no-idx artifact spend inputs",
  });
  const outputFieldPlan = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.outputs,
    anchorTxId: producing.artifact.nativeTxId,
    nativeTxCompactCbor: producing.artifact.nativeTxCompactCbor,
    itemCbors: outputs.parsed.outputsPreimage.map(
      encodeMidgardTxOutputCanonical,
    ),
    owner: carriageOwner,
    label: "input-no-idx artifact outputs",
  });
  const artifact = Object.freeze({
    schemaVersion: INPUT_NO_IDX_ARTIFACT,
    headerHash,
    detectionId: parsed.detectionId,
    position,
    badTx: bad.artifact,
    producingTx: producing.artifact,
    inputs: inputs.json,
    badInputsIndex,
    outputsPreimageCbor: outputs.json,
    badInputOutputIndex,
  }) satisfies InputNoIdxArtifact;
  return Object.freeze({
    artifact,
    badInclusion: bad.inclusion,
    producingInclusion: producing.inclusion,
    inputs: inputs.parsed,
    outputs: outputs.parsed,
    inputFieldPlan,
    outputFieldPlan,
  });
};

const selectedIdentity = (
  classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >,
) => {
  const fields = classification.selected.detectionId.split(":");
  const position = Number(fields[1]);
  const badInputsIndex = Number(fields[2]);
  if (
    classification.category !== "nonExistentInputNoIndex" ||
    classification.selected.violationId !== INPUT_NO_IDX_VIOLATION_ID ||
    fields.length !== 7 ||
    fields[0] !== INPUT_NO_IDX_VIOLATION_ID ||
    !NATURAL.test(fields[1] ?? "") ||
    !NATURAL.test(fields[2] ?? "") ||
    !HEX_32.test(fields[3] ?? "") ||
    !HEX_32.test(fields[4] ?? "") ||
    !NATURAL.test(fields[5] ?? "") ||
    !NATURAL.test(fields[6] ?? "") ||
    !Number.isSafeInteger(position) ||
    !Number.isSafeInteger(badInputsIndex) ||
    classification.selected.position !== BigInt(fields[1]!)
  ) {
    throw new Error("input-no-idx classification identity is malformed");
  }
  return Object.freeze({
    position,
    badInputsIndex,
    badTxId: fields[3]!,
    producingTxId: fields[4]!,
    badInputOutputIndex: fields[5]!,
    producingTxOutputCount: fields[6]!,
  });
};

export const prepareInputNoIdxArtifact = async ({
  evidence,
  classification,
}: {
  readonly evidence: Parameters<
    typeof prepareInputNoIdxFromCanonicalEvidence
  >[0]["evidence"];
  readonly classification: Extract<
    CanonicalBlockClassification,
    { readonly decision: "fault_detected" }
  >;
}): Promise<InputNoIdxArtifact> => {
  if (
    classification.headerHash !== evidence.headerHash ||
    classification.selected.position > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error("input-no-idx classification differs from evidence");
  }
  const selected = selectedIdentity(classification);
  const prepared = await prepareInputNoIdxFromCanonicalEvidence({
    evidence,
    badTxId: selected.badTxId,
    badInputsIndex: selected.badInputsIndex,
  });
  if (
    prepared.producingTxInclusion.nativeTxId !== selected.producingTxId ||
    prepared.step04.badInputOutputIndex !== selected.badInputOutputIndex ||
    prepared.outputsPreimage.length.toString() !==
      selected.producingTxOutputCount
  ) {
    throw new Error("input-no-idx prepared evidence changed classification");
  }
  const inclusionJson = (inclusion: typeof prepared.badTxInclusion) => ({
    nativeTxId: inclusion.nativeTxId,
    nativeTxCompactCbor: inclusion.nativeTxCompactCbor,
    l2TransactionSourceCbor: inclusion.l2TransactionSourceCbor,
    transactionsPhasRoot: inclusion.transactionsPhasRoot,
    txMembershipProofCbor: inclusion.txMembershipProofCbor,
  });
  const artifact = normalizeJournalJson({
    schemaVersion: INPUT_NO_IDX_ARTIFACT,
    headerHash: prepared.headerHash,
    detectionId: classification.selected.detectionId,
    position: selected.position,
    badTx: inclusionJson(prepared.badTxInclusion),
    producingTx: inclusionJson(prepared.producingTxInclusion),
    inputs: prepared.step02.inputsPreimage.map((input) => ({
      tx_id: input.tx_id,
      output_index: input.output_index.toString(),
    })),
    badInputsIndex: prepared.step02.badInputsIndex,
    outputsPreimageCbor: prepared.step04.outputsPreimageCbor,
    badInputOutputIndex: prepared.step04.badInputOutputIndex,
  }) as InputNoIdxArtifact;
  admitInputNoIdxArtifact(artifact);
  return Object.freeze(artifact);
};

export type InputNoIdxWorkflowReferenceScripts = Readonly<{
  steps: readonly [UTxO, UTxO, UTxO, UTxO];
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
  network: FraudProofWorkflowDeploymentBinding<"nonExistentInputNoIndex">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  referenceScripts: InputNoIdxWorkflowReferenceScripts;
  certificate: NonNullable<
    FraudProofWorkflowDeploymentBinding<"nonExistentInputNoIndex">["fieldPreimageCertificate"]
  >;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "input-no-idx workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "nonExistentInputNoIndex" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("input-no-idx workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`input-no-idx workflow action omitted ${field}`);
  }
  return value;
};

const resolveField = async (
  config: BoundConfig,
  planned: FaultProofFieldOpeningPlan,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned,
  });
  if (publications === undefined) {
    throw new Error("input-no-idx field publications disappeared");
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.network,
    planned,
    certificatePolicyId: config.certificate.policyId,
  });
  if (planned.plan.tier === "Certified" && certificate === undefined) {
    throw new Error("input-no-idx field certificate disappeared");
  }
  return Object.freeze({ publications, certificate });
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
        fraudCategory: "nonExistentInputNoIndex",
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
              "input-no-idx removal changed its authenticated inputs",
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

const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"nonExistentInputNoIndex"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "nonExistentInputNoIndex",
  prepare: async ({ evidence, classification }) =>
    await prepareInputNoIdxArtifact({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted = admitInputNoIdxArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("input-no-idx artifact changed manifest-bound header");
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
              fraudCategory: "nonExistentInputNoIndex",
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
    if (input.stage === "step_01" || input.stage === "step_03") {
      const stepIndex = input.stage === "step_01" ? 0 : 2;
      const inclusion =
        input.stage === "step_01"
          ? admitted.badInclusion
          : admitted.producingInclusion;
      const proofCbor =
        input.stage === "step_01"
          ? admitted.artifact.badTx.txMembershipProofCbor
          : admitted.artifact.producingTx.txMembershipProofCbor;
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            const common = {
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
              txInclusion: inclusion,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[stepIndex],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (input.stage === "step_01") {
              await submitInputNoIdxStep01(common);
            } else {
              await submitInputNoIdxStep03(common);
            }
          },
        ),
      });
    }
    if (input.stage === "step_02" || input.stage === "step_04") {
      const stepIndex = input.stage === "step_02" ? 1 : 3;
      const plan =
        input.stage === "step_02"
          ? admitted.inputFieldPlan
          : admitted.outputFieldPlan;
      const carriage = await resolveField(config, plan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            const common = {
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : { certificateUtxo: carriage.certificate }),
              referenceScriptUtxo: config.referenceScripts.steps[stepIndex],
              preSubmitBoundary,
              awaitConfirmation: false,
            } as const;
            if (input.stage === "step_02") {
              await submitInputNoIdxStep02({
                ...common,
                inputsPreimage: admitted.inputs,
                nativeTxCompactCbor:
                  admitted.artifact.badTx.nativeTxCompactCbor,
              });
            } else {
              await submitInputNoIdxStep04({
                ...common,
                outputsPreimage: admitted.outputs,
                nativeTxCompactCbor:
                  admitted.artifact.producingTx.nativeTxCompactCbor,
                witnessReferenceScripts: config.referenceScripts.witnesses,
              });
            }
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `input-no-idx workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundInputNoIdxWorkflowConfig = Readonly<{
  manifest: unknown;
  blueprintJson: string;
  deploymentInfo: unknown;
  headerHash: string;
  lucid: LucidEvolution;
  signer: ResolvedProverSigner;
  referenceScripts: InputNoIdxWorkflowReferenceScripts;
  source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
}>;

export type ManifestBoundInputNoIdxWorkflow = Readonly<{
  binding: FraudProofWorkflowDeploymentBinding<"nonExistentInputNoIndex">;
  l1: FraudProofFamilyL1ObservationPort<"nonExistentInputNoIndex">;
  transactions: LinearFamilyTransactionPort<"nonExistentInputNoIndex">;
  adapter: FraudProofFamilyWorkflowAdapter;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
}>;

export const createManifestBoundInputNoIdxWorkflow = async (
  config: ManifestBoundInputNoIdxWorkflowConfig,
): Promise<ManifestBoundInputNoIdxWorkflow> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: config.manifest,
    blueprintJson: config.blueprintJson,
    deploymentInfo: config.deploymentInfo,
    category: "nonExistentInputNoIndex",
    headerHash: config.headerHash,
    proverCredential: config.signer.paymentKeyHash,
    stepDatumSchemas: [
      FraudProofComputationThreadStepDatum,
      InputNoIdxStep02Datum,
      InputNoIdxStep03Datum,
      InputNoIdxStep04Datum,
    ],
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: config.signer.address,
    paymentKeyHash: config.signer.paymentKeyHash,
  });
  if (binding.fieldPreimageCertificate === null) {
    throw new Error(
      "input-no-idx manifest omitted field-preimage certificate policy",
    );
  }
  const certificate = binding.fieldPreimageCertificate;
  const contractNames = [
    "fraudProofNonExistentInputNoIndex",
    "fraudProofNonExistentInputNoIndexStep02",
    "fraudProofNonExistentInputNoIndexStep03",
    "fraudProofNonExistentInputNoIndexStep04",
  ] as const;
  const steps = contractNames.map((contractName, index) =>
    requireManifestBoundReferenceScriptUtxo({
      binding,
      contractName,
      utxo: config.referenceScripts.steps[index]!,
    }),
  ) as unknown as readonly [UTxO, UTxO, UTxO, UTxO];
  const references: InputNoIdxWorkflowReferenceScripts = Object.freeze({
    steps: Object.freeze(steps),
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
    throw new Error("input-no-idx raw-L1 authority omitted publications");
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
    category: "nonExistentInputNoIndex",
    l1,
    transactions,
    stateQueueMutationLeaseCoordinator:
      config.stateQueueMutationLeaseCoordinator,
  });
  const fieldPrerequisite = createAuthenticatedFieldCarriagePrerequisitePort({
    category: "nonExistentInputNoIndex",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    requirementForAction: ({ action, artifact }) => {
      const input = record(action.input, "input-no-idx field prerequisite");
      const admitted = admitInputNoIdxArtifact(
        artifact,
        config.signer.paymentKeyHash,
      );
      const planned =
        input.stage === "step_02"
          ? admitted.inputFieldPlan
          : input.stage === "step_04"
            ? admitted.outputFieldPlan
            : null;
      if (planned === null) return null;
      return {
        planned,
        compactCbor:
          input.stage === "step_02"
            ? admitted.artifact.badTx.nativeTxCompactCbor
            : admitted.artifact.producingTx.nativeTxCompactCbor,
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
    category: "nonExistentInputNoIndex",
    base: adapter,
    prerequisite: fieldPrerequisite,
  });
  const proofPrerequisite = createAuthenticatedProofChunkPrerequisitePort({
    category: "nonExistentInputNoIndex",
    lucid: config.lucid,
    network: binding.network,
    signer: config.signer,
    publications: l1.publications,
    proofCborForAction: ({ action, artifact }) => {
      const input = record(action.input, "input-no-idx proof prerequisite");
      const admitted = admitInputNoIdxArtifact(
        artifact,
        config.signer.paymentKeyHash,
      );
      return input.stage === "step_01"
        ? admitted.artifact.badTx.txMembershipProofCbor
        : input.stage === "step_03"
          ? admitted.artifact.producingTx.txMembershipProofCbor
          : null;
    },
    transactionConfirmed: async ({ headerHash, txHash }) =>
      await l1.transactionConfirmed({ headerHash, txHash }),
  });
  adapter = withProofChunkPrerequisite({
    category: "nonExistentInputNoIndex",
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

export const runOrResumeManifestBoundInputNoIdxWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundInputNoIdxWorkflow;
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
    replayer: INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["nonExistentInputNoIndex"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
