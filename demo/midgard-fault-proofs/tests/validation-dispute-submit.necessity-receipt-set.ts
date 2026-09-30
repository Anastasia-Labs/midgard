import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import {
  encodeCbor,
  encodeMidgardCekProgramMaterialSidecar,
} from "@al-ft/midgard-core";
import { ValidationAuxiliaryWitness } from "@al-ft/midgard-sdk";
import {
  buildMidgardCanonicalCekProgram,
  type CekProgramMaterialNecessityReceiptSet,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");

export const blueprint = JSON.parse(
  readFileSync(blueprintPath, "utf8"),
) as unknown;

export const encodeRuntimeSchema = Data.to as unknown as (
  value: unknown,
  schema: unknown,
) => string;

export const cekSelectionFixture = () => {
  const program = buildMidgardCanonicalCekProgram(
    Buffer.from("010100200101", "hex"),
  );
  const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecar([
    ...program.material.values(),
  ]);
  const selectedScript = encodeCbor([3n, program.envelopeCbor]);
  const auxiliaryCbor = Buffer.from(
    Data.to(
      {
        NativeExecutionScanWitness: {
          execution_index: 0n,
          language_tag: 3n,
          purpose_kind: 0n,
          purpose_index: 0n,
          script_hash: "11".repeat(28),
          subject: "22".repeat(32),
          purpose_siblings: [],
          source_index: 0n,
          origin_kind: 0n,
          source_key: "00",
          script_total_length: BigInt(selectedScript.length),
          script_item_commitment: "33".repeat(32),
          source_siblings: [],
          redeemer_leaf: "44".repeat(32),
          execution_siblings: [],
          first_chunk_proof: {
            version: 1n,
            field_index: 6n,
            item_index: 0n,
            total_length: BigInt(selectedScript.length),
            chunk_index: 0n,
            chunk: selectedScript.toString("hex"),
            frontier: [],
            siblings: [],
          },
        },
      },
      ValidationAuxiliaryWitness,
    ),
    "hex",
  );
  return {
    program,
    programMaterialSidecarCbor,
    auxiliaryCbor,
    routeMaterial: {
      envelopeCbor: program.envelopeCbor,
      programMaterialSidecarCbor,
      programEnvelopeHash: Buffer.from(program.envelopeHash),
    },
  };
};

const targetProtocolParameters = {
  digest: "04".repeat(32),
  maxTxSize: 16_384,
  maxValueSize: 5_000,
  maxExecutionMemoryUnits: "14000000",
  maxExecutionCpuUnits: "10000000000",
  coinsPerUtxoByte: "4310",
  maturityWindowMilliseconds: 300_000,
} as const;

const concreteTransactionReceipt = <
  Role extends
    | "publication"
    | "proof"
    | "proofConsumption"
    | "proofContinuation",
>({
  role,
  seed,
  transactionBytes = 12_000,
  maximumValueBytes = 1_000,
  executionMemoryUnits = "9000000",
  executionCpuUnits = "3000000000",
  programMaterialOutputIndices = [],
  programMaterialConsumedInputOutRefs = [],
  programMaterialReferenceInputOutRefs = [],
  confirmationMilliseconds = 10_000,
}: {
  readonly role: Role;
  readonly seed: number;
  readonly transactionBytes?: number;
  readonly maximumValueBytes?: number;
  readonly executionMemoryUnits?: string;
  readonly executionCpuUnits?: string;
  readonly programMaterialOutputIndices?: readonly number[];
  readonly programMaterialConsumedInputOutRefs?: readonly string[];
  readonly programMaterialReferenceInputOutRefs?: readonly string[];
  readonly confirmationMilliseconds?: number;
}) => {
  const txId = (seed + 0x40).toString(16).padStart(2, "0").repeat(32);
  return {
    role,
    signedTxSha256: seed.toString(16).padStart(2, "0").repeat(32),
    txId,
    transactionBytes,
    transactionByteMargin:
      targetProtocolParameters.maxTxSize - transactionBytes,
    maximumValueBytes,
    maximumValueByteMargin:
      targetProtocolParameters.maxValueSize - maximumValueBytes,
    feeLovelace: "500000",
    minAdaLovelace: "2000000",
    executionMemoryUnits,
    executionMemoryMargin: (
      (BigInt(targetProtocolParameters.maxExecutionMemoryUnits) * 4n) / 5n -
      BigInt(executionMemoryUnits)
    ).toString(),
    executionCpuUnits,
    executionCpuMargin: (
      (BigInt(targetProtocolParameters.maxExecutionCpuUnits) * 4n) / 5n -
      BigInt(executionCpuUnits)
    ).toString(),
    inputCount: Math.max(2, programMaterialConsumedInputOutRefs.length),
    referenceInputCount: Math.max(
      2,
      programMaterialReferenceInputOutRefs.length,
    ),
    outputCount: role === "publication" ? 3 : 1,
    programMaterialInputCount: programMaterialConsumedInputOutRefs.length,
    programMaterialReferenceInputCount:
      programMaterialReferenceInputOutRefs.length,
    programMaterialOutputOutRefs: programMaterialOutputIndices.map(
      (index) => `${txId}#${index.toString()}`,
    ),
    programMaterialConsumedInputOutRefs,
    programMaterialReferenceInputOutRefs,
    confirmationMilliseconds,
  };
};

export const routeTimingComponents = {
  dataAvailabilityFetchMilliseconds: 1_000,
  evidenceConstructionMilliseconds: 2_000,
  retryMilliseconds: 3_000,
  rollbackAllowanceMilliseconds: 4_000,
  settlementMilliseconds: 5_000,
  removalMilliseconds: 6_000,
} as const;

export const necessityReceiptSet = (
  programEnvelopeHash: Uint8Array,
): CekProgramMaterialNecessityReceiptSet => {
  const singlePublication = concreteTransactionReceipt({
    role: "publication",
    seed: 2,
    programMaterialOutputIndices: [0],
  });
  const multiPublicationA = concreteTransactionReceipt({
    role: "publication",
    seed: 4,
    maximumValueBytes: 5_001,
    programMaterialOutputIndices: [0, 1],
  });
  const multiPublicationB = concreteTransactionReceipt({
    role: "publication",
    seed: 5,
    programMaterialOutputIndices: [0],
  });
  const incrementalPublication = concreteTransactionReceipt({
    role: "publication",
    seed: 7,
    programMaterialOutputIndices: [0, 1, 2],
  });
  return {
    schemaVersion: 1,
    sourceRevision: "01".repeat(20),
    programEnvelopeHash: Buffer.from(programEnvelopeHash).toString("hex"),
    validatorIdentities: [
      {
        title: "CEK resolver",
        generatedHash: "02".repeat(28),
        appliedHash: "03".repeat(28),
      },
      {
        title: "Fraud-proof mint",
        generatedHash: "04".repeat(28),
        appliedHash: "05".repeat(28),
      },
    ],
    targetProtocolParameters,
    routeAttempts: [
      {
        route: "directProof",
        transactions: [
          concreteTransactionReceipt({
            role: "proof",
            seed: 1,
            transactionBytes: 16_500,
          }),
        ],
        ...routeTimingComponents,
        maturityWindowMarginMilliseconds: 119_000,
        fit: false,
        limitingConstraint: { type: "maxTxSize", measuredMargin: "-116" },
        minimumMultiOutputCount: null,
      },
      {
        route: "completeSinglePublicationReference",
        transactions: [
          singlePublication,
          concreteTransactionReceipt({
            role: "proofConsumption",
            seed: 3,
            executionMemoryUnits: "11300000",
            programMaterialReferenceInputOutRefs:
              singlePublication.programMaterialOutputOutRefs,
          }),
        ],
        ...routeTimingComponents,
        maturityWindowMarginMilliseconds: 109_000,
        fit: false,
        limitingConstraint: {
          type: "maxExecutionMemoryUnits",
          measuredMargin: "-100000",
        },
        minimumMultiOutputCount: null,
      },
      {
        route: "minimumMultiOutputReconstruction",
        transactions: [
          multiPublicationA,
          multiPublicationB,
          concreteTransactionReceipt({
            role: "proofConsumption",
            seed: 6,
            programMaterialConsumedInputOutRefs:
              multiPublicationA.programMaterialOutputOutRefs.slice(0, 1),
            programMaterialReferenceInputOutRefs: [
              ...multiPublicationA.programMaterialOutputOutRefs.slice(1),
              ...multiPublicationB.programMaterialOutputOutRefs,
            ],
          }),
        ],
        ...routeTimingComponents,
        maturityWindowMarginMilliseconds: 99_000,
        fit: false,
        limitingConstraint: { type: "maxValueSize", measuredMargin: "-1" },
        minimumMultiOutputCount: 3,
      },
      {
        route: "incrementalTraversal",
        transactions: [
          incrementalPublication,
          concreteTransactionReceipt({
            role: "proofConsumption",
            seed: 8,
            programMaterialReferenceInputOutRefs:
              incrementalPublication.programMaterialOutputOutRefs.slice(0, 1),
          }),
          concreteTransactionReceipt({
            role: "proofContinuation",
            seed: 9,
            programMaterialReferenceInputOutRefs:
              incrementalPublication.programMaterialOutputOutRefs.slice(1, 2),
          }),
          concreteTransactionReceipt({
            role: "proofContinuation",
            seed: 10,
            programMaterialReferenceInputOutRefs:
              incrementalPublication.programMaterialOutputOutRefs.slice(2),
          }),
        ],
        ...routeTimingComponents,
        maturityWindowMarginMilliseconds: 89_000,
        fit: true,
        limitingConstraint: null,
        minimumMultiOutputCount: null,
      },
    ],
  };
};
