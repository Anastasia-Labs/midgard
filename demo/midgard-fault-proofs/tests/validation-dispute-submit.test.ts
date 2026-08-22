import { readFileSync } from "node:fs";
import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join, resolve } from "node:path";

import {
  buildMidgardValidationTraceTree,
  encodeCbor,
  encodeMidgardCekProgramMaterialSidecarV1,
  MIDGARD_VALIDATION_DISPUTE_RESPONSE_WINDOW_MS,
} from "@al-ft/midgard-core";
import {
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_V1_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  Proof,
  ValidationAuxiliaryWitnessV1,
  ValidationAwardSpendRedeemerV1,
  ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema,
  type ValidationMachineStateV1,
  ValidationOneStepWitnessV1,
  ValidationPrepareSelectedSpendRedeemerV1Schema,
} from "@al-ft/midgard-sdk";
import {
  buildMidgardCanonicalCekProgramV1,
  type CekProgramMaterialNecessityReceiptSetV1,
} from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { parseExactAikenDataCbor } from "../src/aiken-blueprint-data.js";
import {
  readValidationDisputeCborFile,
  validationCekProgramMaterialReferenceOutRefsFromFiles,
  validationOneStepArgumentFromFiles,
} from "../src/validation-dispute/from-files.js";
import {
  encodeScriptSourcesStageOneSpendRedeemerV1,
  encodeValidationSemanticResolutionRedeemerV1,
  openValidationDisputeAfterSourceVerification,
  refreshExpiredValidationDisputeValidityRange,
  requireValidationCanonicalDecodePrepareReferenceScriptOutRef,
  requireValidationCekSemanticReferenceScriptOutRef,
  requireValidationItemObserveReferenceScriptOutRef,
  requireValidationItemSemanticReferenceScriptOutRef,
  selectValidationCompleteItemCarriageV1,
  validateCekSubmissionEvidenceV1,
  VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1,
  VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY,
  validationCekSemanticReferenceScriptDeploymentEntryV1,
  validationDisputeTimeoutValidityRange,
  validationDisputeValidityRange,
  validationOneStepEvidenceHashV1,
  validationSemanticResolverGlobalIndexV1,
} from "../src/validation-dispute/submit.js";

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  resolve(process.cwd(), "../../onchain/aiken/plutus.json");
const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown;
const encodeRuntimeSchema = Data.to as unknown as (
  value: unknown,
  schema: unknown,
) => string;

const cekSelectionFixture = () => {
  const program = buildMidgardCanonicalCekProgramV1(
    Buffer.from("010100200101", "hex"),
  );
  const programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecarV1([
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
      ValidationAuxiliaryWitnessV1,
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

const routeTimingComponents = {
  dataAvailabilityFetchMilliseconds: 1_000,
  evidenceConstructionMilliseconds: 2_000,
  retryMilliseconds: 3_000,
  rollbackAllowanceMilliseconds: 4_000,
  settlementMilliseconds: 5_000,
  removalMilliseconds: 6_000,
} as const;

const necessityReceiptSet = (
  programEnvelopeHash: Uint8Array,
): CekProgramMaterialNecessityReceiptSetV1 => {
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

type MutableNecessityReceiptSetFixture = {
  validatorIdentities: Array<{
    title: string;
    generatedHash: string;
    appliedHash: string;
  }>;
  targetProtocolParameters: Record<string, unknown>;
  routeAttempts: Array<{
    route: string;
    transactions: Array<Record<string, unknown>>;
    dataAvailabilityFetchMilliseconds: number;
    evidenceConstructionMilliseconds: number;
    retryMilliseconds: number;
    rollbackAllowanceMilliseconds: number;
    settlementMilliseconds: number;
    removalMilliseconds: number;
    maturityWindowMarginMilliseconds: number;
    fit: boolean;
    limitingConstraint: Record<string, unknown> | null;
    minimumMultiOutputCount: number | null;
  }>;
} & Record<string, unknown>;

const mutateNecessityReceiptSet = (
  receiptSet: CekProgramMaterialNecessityReceiptSetV1,
  mutate: (draft: MutableNecessityReceiptSetFixture) => void,
): CekProgramMaterialNecessityReceiptSetV1 => {
  const draft = JSON.parse(
    JSON.stringify(receiptSet),
  ) as MutableNecessityReceiptSetFixture;
  mutate(draft);
  return draft as unknown as CekProgramMaterialNecessityReceiptSetV1;
};

describe("validation-dispute transaction validity", () => {
  it("uses a bounded closed range with the validator timestamp at its upper bound", () => {
    expect(validationDisputeValidityRange(1_000_000)).toEqual({
      validFrom: 940_000,
      validTo: 1_060_000,
    });
  });

  it("refreshes an expired staged range against current ledger time", () => {
    const range = { validFrom: 940_000, validTo: 1_060_000 };
    expect(
      refreshExpiredValidationDisputeValidityRange({
        range,
        currentLedgerTime: 1_059_999,
      }),
    ).toBe(range);
    expect(
      refreshExpiredValidationDisputeValidityRange({
        range,
        currentLedgerTime: 1_080_000,
      }),
    ).toEqual({
      validFrom: 1_020_000,
      validTo: 1_140_000,
    });
  });

  it("places timeout lower bound strictly after the response deadline", () => {
    expect(validationDisputeTimeoutValidityRange(1_000_000, 990_000)).toEqual({
      validFrom: 990_001,
      validTo: 1_060_000,
    });
    expect(() =>
      validationDisputeTimeoutValidityRange(1_000_000, 1_000_000),
    ).toThrow(/has not passed/);
  });

  it("selects direct then automatic reference carriage at measured boundaries", () => {
    const direct =
      MIDGARD_V1_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
    const publication =
      MIDGARD_CONSENSUS_LIMITS_V1.maxSinglePublicationCompleteItemBytes;
    expect(selectValidationCompleteItemCarriageV1(direct)).toBe("direct");
    expect(selectValidationCompleteItemCarriageV1(direct + 1)).toBe(
      "reference",
    );
    expect(selectValidationCompleteItemCarriageV1(publication)).toBe(
      "reference",
    );
    expect(() =>
      selectValidationCompleteItemCarriageV1(publication + 1),
    ).toThrow(/single-publication envelope/u);
  });

  it("requires the published item-semantic reference script from deployment info", () => {
    const scriptHash = "ab".repeat(28);
    const otherScriptHash = "cd".repeat(28);
    const refScriptUTxO = { txHash: "12".repeat(32), outputIndex: 3 };
    expect(
      requireValidationItemSemanticReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash,
            refScriptUTxO,
          },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toEqual(refScriptUTxO);
    expect(() =>
      requireValidationItemSemanticReferenceScriptOutRef({
        deploymentInfo: {},
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(
      /missing "validationTraceDisputeItemSemantic"; publish the V1 canonical-decode item-semantic reference script/u,
    );
    expect(() =>
      requireValidationItemSemanticReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash,
            refScriptUTxO: null,
          },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(/is missing refScriptUTxO; publish the V1 canonical-decode/u);
    expect(() =>
      requireValidationItemSemanticReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_ITEM_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash: otherScriptHash,
            refScriptUTxO,
          },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(/script hash mismatch/u);
  });

  it("requires the published item-observe reference script from deployment info", () => {
    const scriptHash = "ab".repeat(28);
    const otherScriptHash = "cd".repeat(28);
    const refScriptUTxO = { txHash: "12".repeat(32), outputIndex: 3 };
    expect(
      requireValidationItemObserveReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash,
            refScriptUTxO,
          },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toEqual(refScriptUTxO);
    expect(() =>
      requireValidationItemObserveReferenceScriptOutRef({
        deploymentInfo: {},
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(
      /missing "validationTraceDisputeItemObserve"; publish the V1 canonical-decode item-observe reference script/u,
    );
    expect(() =>
      requireValidationItemObserveReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash,
            refScriptUTxO: null,
          },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(/is missing refScriptUTxO; publish the V1 canonical-decode/u);
    expect(() =>
      requireValidationItemObserveReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_ITEM_OBSERVE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]: {
            scriptHash: otherScriptHash,
            refScriptUTxO,
          },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(/script hash mismatch/u);
  });

  it("requires the published canonical-decode prepare reference script from deployment info", () => {
    const scriptHash = "ab".repeat(28);
    const otherScriptHash = "cd".repeat(28);
    const refScriptUTxO = { txHash: "12".repeat(32), outputIndex: 3 };
    expect(
      requireValidationCanonicalDecodePrepareReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]:
            {
              scriptHash,
              refScriptUTxO,
            },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toEqual(refScriptUTxO);
    expect(() =>
      requireValidationCanonicalDecodePrepareReferenceScriptOutRef({
        deploymentInfo: {},
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(
      /missing "validationTraceDisputeCanonicalDecodePrepare"; publish the V1 canonical-decode prepare reference script/u,
    );
    expect(() =>
      requireValidationCanonicalDecodePrepareReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]:
            {
              scriptHash,
              refScriptUTxO: null,
            },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(/is missing refScriptUTxO; publish the V1 canonical-decode/u);
    expect(() =>
      requireValidationCanonicalDecodePrepareReferenceScriptOutRef({
        deploymentInfo: {
          [VALIDATION_CANONICAL_DECODE_PREPARE_REFERENCE_SCRIPT_DEPLOYMENT_ENTRY]:
            {
              scriptHash: otherScriptHash,
              refScriptUTxO,
            },
        },
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(/script hash mismatch/u);
  });

  it("requires the published CEK semantic-resolver reference scripts from deployment info", () => {
    const scriptHash = "ab".repeat(28);
    const otherScriptHash = "cd".repeat(28);
    const refScriptUTxO = { txHash: "12".repeat(32), outputIndex: 3 };
    expect(
      VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1,
    ).toEqual({
      1: "validationTraceDisputeCekExecutionSelectionSemantic",
      2: "validationTraceDisputeCekContextStepSemantic",
      3: "validationTraceDisputeCekCoreStepSemantic",
    });
    // The finish resolver fits the envelope and attaches inline.
    expect(validationCekSemanticReferenceScriptDeploymentEntryV1(0)).toBe(
      undefined,
    );
    expect(validationCekSemanticReferenceScriptDeploymentEntryV1(4)).toBe(
      undefined,
    );
    expect(() =>
      requireValidationCekSemanticReferenceScriptOutRef({
        deploymentInfo: {},
        semanticResolverIndex: 0,
        expectedScriptHash: scriptHash,
      }),
    ).toThrow(/CEK semantic resolver 0 is not published by reference/u);
    for (const semanticResolverIndex of [1, 2, 3] as const) {
      const entryName =
        VALIDATION_CEK_SEMANTIC_REFERENCE_SCRIPT_DEPLOYMENT_ENTRIES_V1[
          semanticResolverIndex
        ];
      expect(
        validationCekSemanticReferenceScriptDeploymentEntryV1(
          semanticResolverIndex,
        ),
      ).toBe(entryName);
      expect(
        requireValidationCekSemanticReferenceScriptOutRef({
          deploymentInfo: {
            [entryName]: {
              scriptHash,
              refScriptUTxO,
            },
          },
          semanticResolverIndex,
          expectedScriptHash: scriptHash,
        }),
      ).toEqual(refScriptUTxO);
      expect(() =>
        requireValidationCekSemanticReferenceScriptOutRef({
          deploymentInfo: {},
          semanticResolverIndex,
          expectedScriptHash: scriptHash,
        }),
      ).toThrow(
        new RegExp(
          `missing "${entryName}"; publish the V1 CEK semantic-resolver reference script`,
          "u",
        ),
      );
      expect(() =>
        requireValidationCekSemanticReferenceScriptOutRef({
          deploymentInfo: {
            [entryName]: {
              scriptHash,
              refScriptUTxO: null,
            },
          },
          semanticResolverIndex,
          expectedScriptHash: scriptHash,
        }),
      ).toThrow(
        /is missing refScriptUTxO; publish the V1 CEK semantic-resolver/u,
      );
      expect(() =>
        requireValidationCekSemanticReferenceScriptOutRef({
          deploymentInfo: {
            [entryName]: {
              scriptHash: otherScriptHash,
              refScriptUTxO,
            },
          },
          semanticResolverIndex,
          expectedScriptHash: scriptHash,
        }),
      ).toThrow(/script hash mismatch/u);
    }
  });

  it("plumbs caller-confirmed CEK publication outrefs through the file-backed tooling", () => {
    expect(validationCekProgramMaterialReferenceOutRefsFromFiles({})).toBe(
      undefined,
    );
    expect(
      validationCekProgramMaterialReferenceOutRefsFromFiles({
        validationCekSinglePublicationOutRef: `${"56".repeat(32)}#0`,
      }),
    ).toEqual({ singlePublication: `${"56".repeat(32)}#0` });
    expect(
      validationCekProgramMaterialReferenceOutRefsFromFiles({
        validationCekSinglePublicationOutRef: `${"56".repeat(32)}#0`,
        validationCekMinimumMultiOutputOutRefs: [
          `${"57".repeat(32)}#1`,
          `${"58".repeat(32)}#0`,
        ],
      }),
    ).toEqual({
      singlePublication: `${"56".repeat(32)}#0`,
      minimumMultiOutput: [`${"57".repeat(32)}#1`, `${"58".repeat(32)}#0`],
    });
    expect(() =>
      validationCekProgramMaterialReferenceOutRefsFromFiles({
        validationCekMinimumMultiOutputOutRefs: [],
      }),
    ).toThrow(/at least one txHash#outputIndex entry in root order/u);
  });

  it("starts the response deadline at the authenticated source upper bound", () => {
    const operator = buildMidgardValidationTraceTree(
      [Buffer.alloc(32, 1), Buffer.alloc(32, 2), Buffer.alloc(32, 3)],
      "accepted",
    );
    const challenger = buildMidgardValidationTraceTree(
      [Buffer.alloc(32, 1), Buffer.alloc(32, 2), Buffer.alloc(32, 4)],
      "accepted",
    );
    const sourceValidityRange = {
      validFrom: 1_000_000,
      validTo: 1_000_101,
    };

    const dispute = openValidationDisputeAfterSourceVerification({
      operatorDescriptor: operator.descriptor,
      challengerDescriptor: challenger.descriptor,
      openTimeUpper: 1_000_000n,
      challengedBlockEndTime: 1_000_000n,
      sourceValidityRange,
    });

    expect(dispute.responseDeadline).toBe(
      sourceValidityRange.validTo -
        1 +
        MIDGARD_VALIDATION_DISPUTE_RESPONSE_WINDOW_MS,
    );
  });

  it("rejects absent, invalid, time-travelling, and stale source timing", () => {
    const operator = buildMidgardValidationTraceTree(
      [Buffer.alloc(32, 1), Buffer.alloc(32, 2), Buffer.alloc(32, 3)],
      "accepted",
    );
    const challenger = buildMidgardValidationTraceTree(
      [Buffer.alloc(32, 1), Buffer.alloc(32, 2), Buffer.alloc(32, 4)],
      "accepted",
    );
    const base = {
      operatorDescriptor: operator.descriptor,
      challengerDescriptor: challenger.descriptor,
      openTimeUpper: 1_000_000n,
      challengedBlockEndTime: 1_000_000n,
    };

    expect(() =>
      openValidationDisputeAfterSourceVerification({
        ...base,
        sourceValidityRange: undefined as never,
      }),
    ).toThrow(/validity range/u);
    expect(() =>
      openValidationDisputeAfterSourceVerification({
        ...base,
        sourceValidityRange: { validFrom: 1_000_000, validTo: 1_000_000 },
      }),
    ).toThrow(/validity range/u);
    expect(() =>
      openValidationDisputeAfterSourceVerification({
        ...base,
        openTimeUpper: 1_000_100n,
        sourceValidityRange: { validFrom: 999_900, validTo: 1_000_001 },
      }),
    ).toThrow(/cannot precede/u);
    expect(() =>
      openValidationDisputeAfterSourceVerification({
        ...base,
        challengedBlockEndTime: 0n,
        sourceValidityRange: {
          validFrom: MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs - 100,
          validTo: MIDGARD_CONSENSUS_LIMITS_V1.blockMaturityMs + 1,
        },
      }),
    ).toThrow(/cannot complete before the challenged block matures/u);
  });

  it("hashes exact canonical one-step evidence and rejects ambiguous data", () => {
    const emptyConstructor = Buffer.from("d87980", "hex");
    expect(
      validationOneStepEvidenceHashV1({
        transitionCbor: emptyConstructor,
        auxiliaryCbor: emptyConstructor,
      }),
    ).toBe("a9ee2618651193d3a6c6c658f3f3d19f6a296103ac660e0071b45d903bc1e192");
    expect(() =>
      validationOneStepEvidenceHashV1({
        transitionCbor: Buffer.from("d8799fff", "hex"),
        auxiliaryCbor: emptyConstructor,
      }),
    ).toThrow(/not exact canonical V1 Plutus Data/u);
    expect(() =>
      validationOneStepEvidenceHashV1({
        transitionCbor: new Uint8Array(),
        auxiliaryCbor: emptyConstructor,
      }),
    ).toThrow(/non-empty/u);
    expect(() =>
      validationOneStepEvidenceHashV1({
        transitionCbor: new Uint8Array(16 * 1024),
        auxiliaryCbor: emptyConstructor,
      }),
    ).toThrow(/strictly below the L1 proof envelope/u);
  });

  it("matches the exact prepare, semantic, and award Aiken redeemer ABIs", () => {
    const state: ValidationMachineStateV1 = {
      machine_version: 1n,
      event_key_hash: "01".repeat(32),
      transaction_id: "02".repeat(32),
      transaction_commitment: "03".repeat(32),
      validation_context_hash: "04".repeat(32),
      source_kind: "Forced",
      prior_ledger_root: "05".repeat(32),
      phase: "CanonicalDecode",
      program_counter: 0n,
      work_root: "06".repeat(32),
      execution_cpu: 0n,
      execution_memory: 0n,
      verdict: "Pending",
      rejection_code_hash: "00".repeat(32),
      ledger_delta_root: "07".repeat(32),
    };
    const transition = {
      work_witness_cbor: "8100",
      claimed_successor: { ...state, program_counter: 1n },
    };
    const auxiliary = "NoAuxiliaryWitness" as const;
    const redeemers = [
      {
        definition:
          "midgard/validation_resolver_v1/PrepareSelectedSpendRedeemer",
        cbor: encodeRuntimeSchema(
          {
            Continue: [
              {
                input_index: 0n,
                output_index: 0n,
                semantic_resolver_index: 0n,
                transition,
                auxiliary,
              },
            ],
          },
          ValidationPrepareSelectedSpendRedeemerV1Schema,
        ),
      },
      {
        definition: "midgard/validation_award_v1/SpendRedeemer",
        cbor: Data.to(
          {
            Continue: [
              {
                input_index: 0n,
                output_index: 0n,
                fraud_proof_mint_redeemer_index: 0n,
              },
            ],
          },
          ValidationAwardSpendRedeemerV1,
        ),
      },
    ] as const;
    for (const redeemer of redeemers) {
      expect(
        parseExactAikenDataCbor({
          blueprint,
          definitionName: redeemer.definition,
          cbor: redeemer.cbor,
          maxBytes: 16 * 1024 - 1,
        }),
      ).toBeInstanceOf(Constr);
    }

    // Option B (#620): the canonical-decode prepare redeemer is the single
    // four-field transition-only arm — the by-evidence-hash arm is retired.
    // The checked-in blueprint still carries the pre-#620 definition until the
    // wave's blueprint regeneration lands (plutus.json is never regenerated in
    // this lane), so pin the exact wire bytes instead of parsing against the
    // stale definition; the Aiken twin pins the decode side of the same wire.
    const canonicalDecodePrepareCbor = encodeRuntimeSchema(
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 0n,
            semantic_resolver_index: 0n,
            transition,
          },
        ],
      },
      ValidationCanonicalDecodePrepareSelectedSpendRedeemerV1Schema,
    );
    expect(canonicalDecodePrepareCbor).toBe(
      Data.to(
        new Constr(1, [
          new Constr(0, [
            0n,
            0n,
            0n,
            Data.from(Data.to(transition, ValidationOneStepWitnessV1)),
          ]),
        ]),
      ),
    );

    const transitionCbor = Buffer.from(
      Data.to(transition, ValidationOneStepWitnessV1),
      "hex",
    );
    const transitionData = Data.from(
      Data.to(transition, ValidationOneStepWitnessV1),
    );
    // R5 item 1: the cek index is a `prepare_selected` family. Semantic 0
    // (finish) is transition-only; semantic 1 (execution selection) carries
    // the auxiliary plus the material route as its last field; semantics 2/3
    // (context step, core step) carry the auxiliary / the core step only.
    const cekSelection = cekSelectionFixture();
    const cekSelectionAuxiliary = Data.from(
      cekSelection.auxiliaryCbor.toString("hex"),
    ) as Constr<unknown>;
    expect(cekSelectionAuxiliary.index).toBe(11);
    expect(cekSelectionAuxiliary.fields).toHaveLength(16);
    const cekSelectionRedeemer = Data.from(
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 11,
          semanticResolverIndex: 1,
          transitionCbor,
          auxiliaryCbor: cekSelection.auxiliaryCbor,
          cekRouteMaterial: cekSelection.routeMaterial,
        },
        inputIndex: 0n,
        outputIndex: 0n,
        materialRoute: {
          MinimumMultiOutputCekMaterial: {
            envelope_cbor: "0102",
            reference_input_indices: [7n, 2n],
          },
        },
      }).toString("hex"),
    ) as Constr<unknown>;
    expect(cekSelectionRedeemer.index).toBe(1);
    const cekSelectionAction = cekSelectionRedeemer
      .fields[0] as Constr<unknown>;
    expect(cekSelectionAction.index).toBe(0);
    expect(cekSelectionAction.fields).toHaveLength(5);
    expect(cekSelectionAction.fields[2]).toEqual(transitionData);
    expect(cekSelectionAction.fields[3]).toEqual(cekSelectionAuxiliary);
    const cekRoute = cekSelectionAction.fields[4] as Constr<unknown>;
    expect(cekRoute.index).toBe(3);
    expect(cekRoute.fields[1]).toEqual([7n, 2n]);
    // The route is mandatory for 11/1 and refused everywhere else.
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 11,
          semanticResolverIndex: 1,
          transitionCbor,
          auxiliaryCbor: cekSelection.auxiliaryCbor,
          cekRouteMaterial: cekSelection.routeMaterial,
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow(/requires a material route/u);
    const noAuxiliaryCbor = Buffer.from(Data.to(new Constr(0, [])), "hex");
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 11,
          semanticResolverIndex: 0,
          transitionCbor,
          auxiliaryCbor: noAuxiliaryCbor,
        },
        inputIndex: 0n,
        outputIndex: 0n,
        materialRoute: "NoCekMaterial",
      }),
    ).toThrow(/permitted only for the CEK execution-selection/u);
    const cekFinishRedeemer = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument: {
        resolverIndex: 11,
        semanticResolverIndex: 0,
        transitionCbor,
        auxiliaryCbor: noAuxiliaryCbor,
      },
      inputIndex: 0n,
      outputIndex: 0n,
    });
    expect(cekFinishRedeemer.toString("hex")).toBe(
      Data.to(new Constr(1, [new Constr(0, [0n, 0n, transitionData])])),
    );
    // `cek_core_step_semantic_v1.VerifyCoreStep { …, step }` spreads the
    // single `CekCoreStepWitness` field.
    const cekMachineState = new Constr(0, [0n, 0n, "", "", "", 0n, 0n, 0n]);
    const coreStepEvidence = new Constr(0, [
      cekMachineState,
      cekMachineState,
      new Constr(0, [0n]),
    ]);
    const coreStepWitness = new Constr(12, [coreStepEvidence]);
    const cekCoreStepRedeemer = Data.from(
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 11,
          semanticResolverIndex: 3,
          transitionCbor,
          auxiliaryCbor: Buffer.from(Data.to(coreStepWitness as never), "hex"),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }).toString("hex"),
    ) as Constr<unknown>;
    const cekCoreStepAction = cekCoreStepRedeemer.fields[0] as Constr<unknown>;
    expect(cekCoreStepAction.fields).toHaveLength(4);
    expect(cekCoreStepAction.fields[3]).toEqual(coreStepEvidence);
    // A context-step witness is refused by the core-step resolver and
    // accepted by the context-step resolver (tag 14 `CekOutputContextItem`).
    const contextFinalizeWitness = Buffer.from(
      Data.to(new Constr(14, [0n, "00", []]) as never),
      "hex",
    );
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 11,
          semanticResolverIndex: 3,
          transitionCbor,
          auxiliaryCbor: contextFinalizeWitness,
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow(/validation Cek auxiliary witness/u);
    const cekContextStepRedeemer = Data.from(
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 11,
          semanticResolverIndex: 2,
          transitionCbor,
          auxiliaryCbor: contextFinalizeWitness,
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }).toString("hex"),
    ) as Constr<unknown>;
    const cekContextStepAction = cekContextStepRedeemer
      .fields[0] as Constr<unknown>;
    expect(cekContextStepAction.fields).toHaveLength(4);
    expect(cekContextStepAction.fields[3]).toEqual(
      new Constr(14, [0n, "00", []]),
    );

    // R5 item 1: the ValueAndMint index is a `prepare_selected` family whose
    // eleven semantics follow the control-stage order; the witness-carrying
    // stages spread their auxiliary fields, the others are transition-only.
    const valueAndMintShapes = [
      { semantic: 0, auxiliary: new Constr(0, []), fields: 3 },
      { semantic: 1, auxiliary: new Constr(0, []), fields: 3 },
      {
        semantic: 2,
        auxiliary: new Constr(7, [0n, "00", "11".repeat(32), "22"]),
        fields: 7,
      },
      { semantic: 4, auxiliary: new Constr(0, []), fields: 3 },
      {
        semantic: 5,
        auxiliary: new Constr(38, [0n, "00", []]),
        fields: 6,
      },
      { semantic: 7, auxiliary: new Constr(0, []), fields: 3 },
      { semantic: 9, auxiliary: new Constr(0, []), fields: 3 },
      { semantic: 10, auxiliary: new Constr(0, []), fields: 3 },
    ] as const;
    for (const shape of valueAndMintShapes) {
      const redeemer = Data.from(
        encodeValidationSemanticResolutionRedeemerV1({
          oneStepArgument: {
            resolverIndex: 12,
            semanticResolverIndex: shape.semantic,
            transitionCbor,
            auxiliaryCbor: Buffer.from(
              Data.to(shape.auxiliary as never),
              "hex",
            ),
          },
          inputIndex: 0n,
          outputIndex: 0n,
        }).toString("hex"),
      ) as Constr<unknown>;
      const action = redeemer.fields[0] as Constr<unknown>;
      expect(action.fields).toHaveLength(shape.fields);
    }
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 12,
          semanticResolverIndex: 2,
          transitionCbor,
          auxiliaryCbor: noAuxiliaryCbor,
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow(/validation ValueAndMint auxiliary witness/u);

    // #597: the tag-30 `TransactionFieldItemWitness` carries one field — a
    // `FieldCarriageV1` — so a fitting fixture is a tier-1 `Inline` carriage,
    // `Constr(0, [preimage])`, not the retired collection-proof pair.
    const completeItemCarriage = new Constr(0, ["00"]);
    const completeItemAuxiliaryCbor = Buffer.from(
      Data.to(new Constr(30, [completeItemCarriage]) as never),
      "hex",
    );
    // Option B (#620): the item-semantic redeemer is the single three-field
    // transition-only `Verify` — the carriage argument and the
    // `VerifyReference` arm are retired, so there is no reference-input route
    // to request any more. The checked-in blueprint still carries the
    // pre-#620 two-arm definition until the wave's blueprint regeneration
    // lands, so pin the exact wire bytes; the Aiken twin pins the decode side.
    const itemSemanticCbor = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument: {
        resolverIndex: 0,
        semanticResolverIndex: 1,
        transitionCbor,
        auxiliaryCbor: completeItemAuxiliaryCbor,
      },
      inputIndex: 0n,
      outputIndex: 0n,
    });
    expect(itemSemanticCbor.toString("hex")).toBe(
      Data.to(
        new Constr(1, [
          new Constr(0, [
            0n,
            0n,
            Data.from(Data.to(transition, ValidationOneStepWitnessV1)),
          ]),
        ]),
      ),
    );
    // A non-item auxiliary refuses at the ingress shape gate.
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 0,
          semanticResolverIndex: 1,
          transitionCbor,
          auxiliaryCbor: Buffer.from(Data.to(new Constr(0, [])), "hex"),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow(/must carry an authenticated chunk or complete item/u);
    // Option B (#620): a chunk-shaped auxiliary passes the ingress shape gate
    // (chunk-or-item) but has no wire to ride at the item-semantic stage —
    // the retired four-field `Verify` was the only shape it could ever have
    // targeted — so redeemer construction refuses it fail-closed.
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 0,
          semanticResolverIndex: 1,
          transitionCbor,
          auxiliaryCbor: Buffer.from(
            // A well-typed `TransactionFieldChunkWitness`:
            // (field_index, item_index, tier-1 `Inline` carriage).
            Data.to(new Constr(1, [0n, 0n, new Constr(0, ["00"])]) as never),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow(/cannot construct the selected semantic redeemer/u);

    // C21-STAGE4 Option B′ disposition: resolver 8 / semantic resolver 0
    // does not consume a TransactionFieldItemWitness any more. Option A made
    // the stage-four fold proof-only (tag 29), so admitting the proof-item
    // datum's tag-30 item through a reference-input ABI would create a route
    // that cannot be semantically equivalent to the direct proof. #620 then
    // deleted the reference-route encoder entirely (the retired
    // `VerifyReference` arm was its only consumer), so the fail-closed
    // refusal is now structural: the encoder takes no reference-input index.
    // Keep the direct deployed ABI parseable.
    const stageFourAuxiliaryCbor = Buffer.from(
      Data.to(new Constr(29, [completeItemCarriage]) as never),
      "hex",
    );
    const stageFourDirect = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument: {
        resolverIndex: 8,
        semanticResolverIndex: 0,
        transitionCbor,
        auxiliaryCbor: stageFourAuxiliaryCbor,
      },
      inputIndex: 0n,
      outputIndex: 0n,
    });
    expect(
      parseExactAikenDataCbor({
        blueprint,
        definitionName:
          "fraud_proofs/validation_trace/script_sources_non_output_semantic_v1/SpendRedeemer",
        cbor: stageFourDirect.toString("hex"),
        maxBytes: 16 * 1024 - 1,
      }),
    ).toBeInstanceOf(Constr);

    const sourceFields = [
      0n,
      0n,
      "00",
      3n,
      "11".repeat(28),
      100n,
      "22".repeat(32),
      [],
    ] as const;
    const redeemerChunkProof = new Constr(0, [
      1n,
      8n,
      0n,
      8n,
      0n,
      "8400004100820101",
      [],
      [],
    ]);
    const redeemerItemControl = new Constr(0, [
      1n,
      0n,
      0n,
      0n,
      1n,
      8n,
      "22".repeat(32),
      -1n,
      -1n,
      -1n,
      -1n,
      0n,
      0n,
      -1n,
      -1n,
      new Constr(1, []),
    ]);
    const redeemerItemWitness = new Constr(0, [
      new Constr(0, []),
      new Constr(0, [redeemerChunkProof]),
      new Constr(1, []),
    ]);
    for (const selected of [
      {
        index: 10,
        auxiliary: new Constr(9, [...sourceFields]),
        module: "script_sources_stage_nine_mismatch_semantic_v1",
      },
      {
        index: 11,
        auxiliary: new Constr(9, [
          ...sourceFields.slice(0, 3),
          0n,
          ...sourceFields.slice(4),
        ]),
        module: "script_sources_stage_nine_native_match_semantic_v1",
      },
      {
        index: 12,
        auxiliary: new Constr(9, [...sourceFields]),
        module: "script_sources_stage_nine_effectful_match_semantic_v1",
      },
      {
        index: 13,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_nine_missing_semantic_v1",
      },
      {
        index: 14,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_one_finish_semantic_v1",
      },
      {
        index: 15,
        auxiliary: new Constr(29, [completeItemCarriage]),
        module: "script_sources_stage_one_redeemer_semantic_v1",
      },
      {
        index: 16,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_eleven_finish_semantic_v1",
      },
      {
        index: 17,
        auxiliary: new Constr(9, [...sourceFields]),
        module: "script_sources_stage_eleven_source_semantic_v1",
      },
      {
        index: 18,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_twelve_finish_semantic_v1",
      },
      {
        index: 19,
        auxiliary: new Constr(10, [0n, 1n, 8n, "22".repeat(32), []]),
        module: "script_sources_stage_twelve_redeemer_semantic_v1",
      },
      {
        index: 20,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_ten_missing_semantic_v1",
      },
      {
        index: 21,
        auxiliary: new Constr(10, [0n, 1n, 8n, "22".repeat(32), []]),
        module: "script_sources_stage_ten_mismatch_semantic_v1",
      },
      {
        index: 22,
        auxiliary: new Constr(18, [
          new Constr(1, []),
          redeemerItemControl,
          redeemerItemWitness,
        ]),
        module: "script_sources_stage_ten_match_semantic_v1",
      },
      {
        index: 23,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_eight_finish_semantic_v1",
      },
      {
        index: 24,
        auxiliary: new Constr(8, [0n, 0n, "11".repeat(28), "00", []]),
        module: "script_sources_stage_eight_purpose_semantic_v1",
      },
      {
        index: 25,
        auxiliary: new Constr(1, [0n, 0n, completeItemCarriage]),
        module: "script_sources_stage_seven_observer_semantic_v1",
      },
      {
        index: 26,
        auxiliary: new Constr(8, [
          3n,
          0n,
          "11".repeat(28),
          "11".repeat(28),
          [],
        ]),
        module: "script_sources_stage_seven_receive_semantic_v1",
      },
      {
        index: 27,
        auxiliary: new Constr(0, []),
        module: "script_sources_stage_seven_finish_semantic_v1",
      },
    ] as const) {
      const auxiliaryCbor = Buffer.from(
        Data.to(selected.auxiliary as never),
        "hex",
      );
      const cbor = encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 8,
          semanticResolverIndex: selected.index,
          transitionCbor,
          auxiliaryCbor,
        },
        inputIndex: 0n,
        outputIndex: 0n,
      });
      expect(
        parseExactAikenDataCbor({
          blueprint,
          definitionName: `fraud_proofs/validation_trace/${selected.module}/SpendRedeemer`,
          cbor: cbor.toString("hex"),
          maxBytes: 16 * 1024 - 1,
        }),
      ).toBeInstanceOf(Constr);
    }
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 8,
          semanticResolverIndex: 13,
          transitionCbor,
          auxiliaryCbor: Buffer.from(
            Data.to(new Constr(9, [...sourceFields]) as never),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow("does not match the selected ScriptSources proof family");
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 8,
          semanticResolverIndex: 10,
          transitionCbor,
          auxiliaryCbor: Buffer.from(
            Data.to(new Constr(11, [...sourceFields]) as never),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow();

    const nativeChunkProof = new Constr(0, [
      1n,
      7n,
      0n,
      3n,
      0n,
      "010203",
      [],
      [],
    ]);
    const nativeDescriptorFields = [
      0n,
      0n,
      0n,
      0n,
      "33".repeat(28),
      "44".repeat(32),
      [],
      0n,
      0n,
      "55".repeat(32),
      3n,
      "66".repeat(32),
      [],
      "",
      [],
      new Constr(0, [nativeChunkProof]),
      [],
    ] as const;
    for (const selected of [
      {
        index: 0,
        auxiliary: new Constr(0, []),
        module: "native_scripts_terminal_semantic_v1",
      },
      {
        index: 1,
        auxiliary: new Constr(37, [...nativeDescriptorFields]),
        module: "native_scripts_native_semantic_v1",
      },
      {
        index: 2,
        auxiliary: new Constr(37, [
          nativeDescriptorFields[0],
          3n,
          ...nativeDescriptorFields.slice(2, 15),
          new Constr(1, []),
          [],
        ]),
        module: "native_scripts_effectful_semantic_v1",
      },
    ] as const) {
      const cbor = encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 9,
          semanticResolverIndex: selected.index,
          transitionCbor,
          auxiliaryCbor: Buffer.from(
            Data.to(selected.auxiliary as never),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      });
      expect(
        parseExactAikenDataCbor({
          blueprint,
          definitionName: `fraud_proofs/validation_trace/${selected.module}/SpendRedeemer`,
          cbor: cbor.toString("hex"),
          maxBytes: 16 * 1024 - 1,
        }),
      ).toBeInstanceOf(Constr);
    }
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 9,
          semanticResolverIndex: 2,
          transitionCbor,
          auxiliaryCbor: Buffer.from(
            Data.to(new Constr(37, [...nativeDescriptorFields]) as never),
            "hex",
          ),
        },
        inputIndex: 0n,
        outputIndex: 0n,
      }),
    ).toThrow(
      "validation NativeScripts effectful first chunk must be constructor 1 with 0 fields",
    );
  });

  it("maps and encodes the split ScriptSources stage-one route without replacing the legacy route", () => {
    // The stage-one redeemer envelope is the last semantic resolver, after
    // the fourteen indices' 90 kind resolvers (75 before R5 item 1 added the
    // four cek and eleven ValueAndMint kinds).
    expect(validationSemanticResolverGlobalIndexV1(8, 28)).toBe(90);
    expect(validationSemanticResolverGlobalIndexV1(8, 15)).toBe(47);

    const state: ValidationMachineStateV1 = {
      machine_version: 1n,
      event_key_hash: "01".repeat(32),
      transaction_id: "02".repeat(32),
      transaction_commitment: "03".repeat(32),
      validation_context_hash: "04".repeat(32),
      source_kind: "Forced",
      prior_ledger_root: "05".repeat(32),
      phase: "ScriptSources",
      program_counter: 0n,
      work_root: "06".repeat(32),
      execution_cpu: 0n,
      execution_memory: 0n,
      verdict: "Pending",
      rejection_code_hash: "00".repeat(32),
      ledger_delta_root: "07".repeat(32),
    };
    const transitionData = Data.from(
      Data.to(
        {
          work_witness_cbor: "8100",
          claimed_successor: { ...state, program_counter: 1n },
        },
        ValidationOneStepWitnessV1,
      ),
    );
    const none = new Constr(1, []);
    const summary = new Constr(0, ["11".repeat(32), 1n, 1n]);
    const sequence = new Constr(0, ["12".repeat(32), 0n, 0n, 0n]);
    const frame = new Constr(0, [
      3n,
      0n,
      "",
      0n,
      0n,
      "",
      1n,
      0n,
      [],
      0n,
      sequence,
    ]);
    const traversalControl = new Constr(0, [
      1n,
      6n,
      0n,
      1n,
      1n,
      "13".repeat(32),
      none,
      none,
      none,
      none,
    ]);
    const itemControl = new Constr(0, [
      1n,
      0n,
      2n,
      0n,
      1n,
      1n,
      "14".repeat(32),
      0n,
      0n,
      0n,
      0n,
      0n,
      1n,
      0n,
      0n,
      new Constr(0, [traversalControl]),
    ]);
    const foldMapAction = new Constr(7, [frame, 0n, summary, summary, [], []]);
    const auxiliary = new Constr(18, [
      none,
      itemControl,
      new Constr(0, [new Constr(2, [foldMapAction]), none, none]),
    ]);
    if (!(transitionData instanceof Constr)) {
      throw new Error("test transition must be a constructor");
    }
    const resolution = new Constr(0, [
      1n,
      transitionData.fields[1]!,
      "24".repeat(32),
      "25".repeat(32),
    ]);
    const envelope = new Constr(0, [
      1n,
      "15",
      "16".repeat(32),
      new Constr(0, [1n, resolution, "17".repeat(32)]),
      "18".repeat(32),
      0n,
      "19".repeat(32),
      "1a".repeat(32),
      "1b".repeat(32),
      "1c".repeat(32),
      0n,
      1n,
      "1d".repeat(28),
      "1e".repeat(28),
      "1f".repeat(28),
      "20".repeat(28),
      "21".repeat(28),
      "22".repeat(32),
    ]);
    const redeemers = [
      {
        definition:
          "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_envelope_v1/SpendRedeemer",
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "envelope",
          inputIndex: 0n,
          outputIndex: 0n,
          transition: transitionData,
          auxiliary,
          expectedNextItemControlHash: "23".repeat(32),
          family: 0,
        }),
      },
      {
        definition:
          "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_traversal_normalizer_v1/SpendRedeemer",
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "traversal",
          inputIndex: 0n,
          outputIndex: 0n,
          auxiliary,
          currentItemControl: itemControl,
          traversalAction: foldMapAction,
        }),
      },
      {
        definition:
          "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_outer_normalizer_v1/SpendRedeemer",
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "outer",
          inputIndex: 0n,
          outputIndex: 0n,
        }),
      },
      ...[
        "script_sources_stage_one_redeemer_fold_map_executor_v1",
        "script_sources_stage_one_redeemer_finalize_frame_executor_v1",
      ].map((module) => ({
        definition: `fraud_proofs/validation_trace/${module}/SpendRedeemer`,
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "executor",
          inputIndex: 0n,
          outputIndex: 0n,
          traversalAction: module.includes("fold_map")
            ? foldMapAction
            : new Constr(8, [frame, none]),
        }),
      })),
      {
        definition:
          "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_execution_settlement_v1/SpendRedeemer",
        cbor: encodeScriptSourcesStageOneSpendRedeemerV1({
          stage: "settlement",
          inputIndex: 0n,
          outputIndex: 0n,
          envelope,
        }),
      },
    ];
    for (const redeemer of redeemers) {
      expect(
        parseExactAikenDataCbor({
          blueprint,
          definitionName: redeemer.definition,
          cbor: redeemer.cbor,
          maxBytes: 16 * 1024 - 1,
        }),
      ).toBeInstanceOf(Constr);
    }
    expect(() =>
      encodeScriptSourcesStageOneSpendRedeemerV1({
        stage: "envelope",
        inputIndex: 0n,
        outputIndex: 0n,
        transition: transitionData,
        auxiliary,
        expectedNextItemControlHash: "23".repeat(32),
        family: 2,
      }),
    ).toThrow(/FoldMap or FinalizeFrame/u);
  });

  it("emits the transition-only 3-field complete-item Verify redeemer", () => {
    // Option B (#620, superseding #597's 4-field carriage form): the
    // canonical_decode_item_semantic_v1 ABI's `Verify` arm takes 3 fields —
    // input_index, output_index, transition. The carriage is no longer a
    // redeemer field (content is proven once, at the observe stage's §8.8
    // door) and the `VerifyReference` arm is retired. The checked-in
    // blueprint still carries the pre-#620 definition until the wave's
    // blueprint regeneration lands, so pin the exact emitted wire; the Aiken
    // twin pins the decode side.
    const state: ValidationMachineStateV1 = {
      machine_version: 1n,
      event_key_hash: "01".repeat(32),
      transaction_id: "02".repeat(32),
      transaction_commitment: "03".repeat(32),
      validation_context_hash: "04".repeat(32),
      source_kind: "Forced",
      prior_ledger_root: "05".repeat(32),
      phase: "CanonicalDecode",
      program_counter: 0n,
      work_root: "06".repeat(32),
      execution_cpu: 0n,
      execution_memory: 0n,
      verdict: "Pending",
      rejection_code_hash: "00".repeat(32),
      ledger_delta_root: "07".repeat(32),
    };
    const transitionCbor = Buffer.from(
      Data.to(
        {
          work_witness_cbor: "8100",
          claimed_successor: { ...state, program_counter: 1n },
        },
        ValidationOneStepWitnessV1,
      ),
      "hex",
    );
    const itemCarriage = new Constr(0, ["0102030405"]);
    const oneStepArgument = {
      resolverIndex: 0,
      semanticResolverIndex: 1,
      transitionCbor,
      auxiliaryCbor: Buffer.from(
        Data.to(new Constr(30, [itemCarriage]) as never),
        "hex",
      ),
    };
    const directRedeemer = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument,
      inputIndex: 5n,
      outputIndex: 7n,
    });
    const direct = Data.from(directRedeemer.toString("hex"));
    expect(direct).toBeInstanceOf(Constr);
    const directOuter = direct as Constr<unknown>;
    expect(directOuter.index).toBe(1);
    expect(directOuter.fields).toHaveLength(1);
    const directAction = directOuter.fields[0] as Constr<unknown>;
    expect(directAction).toBeInstanceOf(Constr);
    expect(directAction.index).toBe(0);
    expect(directAction.fields).toHaveLength(3);
    expect(directAction.fields[0]).toBe(5n);
    expect(directAction.fields[1]).toBe(7n);
    expect(directAction.fields[2]).toEqual(
      Data.from(transitionCbor.toString("hex")),
    );
  });

  it("encodes resolver-7 non-membership evidence into the exact semantic ABI", () => {
    const state: ValidationMachineStateV1 = {
      machine_version: 1n,
      event_key_hash: "01".repeat(32),
      transaction_id: "02".repeat(32),
      transaction_commitment: "03".repeat(32),
      validation_context_hash: "04".repeat(32),
      source_kind: "Forced",
      prior_ledger_root: "05".repeat(32),
      phase: "CanonicalDecode",
      program_counter: 0n,
      work_root: "06".repeat(32),
      execution_cpu: 0n,
      execution_memory: 0n,
      verdict: "Pending",
      rejection_code_hash: "00".repeat(32),
      ledger_delta_root: "07".repeat(32),
    };
    const transitionCbor = Buffer.from(
      Data.to(
        {
          work_witness_cbor: "8100",
          claimed_successor: { ...state, program_counter: 1n },
        },
        ValidationOneStepWitnessV1,
      ),
      "hex",
    );

    // Canonical divergent-leaf fixture from transition-trace.test.ak; unlike
    // an empty proof, it remains valid when RF-002's terminal-key check is applied.
    const nonMembershipProof: Proof = [
      {
        Leaf: {
          skip: 0n,
          key: "ee155ace9c40292074cb6aff8c9ccdd273c81648ff1149ef36bcea6ebb8a3e25",
          value:
            "55951e629cad560ea5f8be280c35d8788ee84324b842fee1b41c546efb62d2d5",
        },
      },
    ];
    const proofData = Data.from(Data.to(nonMembershipProof, Proof));
    const sourceKind = 0n;
    const key = "02";
    const nextScheduleHash = "11".repeat(32);
    const auxiliaryCbor = Buffer.from(
      Data.to(
        new Constr(6, [sourceKind, key, nextScheduleHash, proofData]) as never,
      ),
      "hex",
    );

    const redeemer = encodeValidationSemanticResolutionRedeemerV1({
      oneStepArgument: {
        resolverIndex: 7,
        semanticResolverIndex: 5,
        transitionCbor,
        auxiliaryCbor,
      },
      inputIndex: 5n,
      outputIndex: 7n,
    });
    const decoded = Data.from(redeemer.toString("hex"));
    expect(decoded).toBeInstanceOf(Constr);
    const outer = decoded as Constr<unknown>;
    expect(outer.index).toBe(1);
    expect(outer.fields).toHaveLength(1);
    const action = outer.fields[0];
    expect(action).toBeInstanceOf(Constr);
    const actionData = action as Constr<unknown>;
    expect(actionData.index).toBe(0);
    expect(actionData.fields).toHaveLength(7);
    expect(actionData.fields[0]).toBe(5n);
    expect(actionData.fields[1]).toBe(7n);
    expect(actionData.fields[2]).toEqual(
      Data.from(transitionCbor.toString("hex")),
    );
    expect(actionData.fields[3]).toBe(sourceKind);
    expect(actionData.fields[4]).toBe(key);
    expect(actionData.fields[5]).toBe(nextScheduleHash);
    expect(actionData.fields[6]).toEqual(proofData);
    expect(
      parseExactAikenDataCbor({
        blueprint,
        definitionName:
          "fraud_proofs/validation_trace/resolve_inputs_non_membership_semantic_v1/SpendRedeemer",
        cbor: redeemer.toString("hex"),
        maxBytes: 16 * 1024 - 1,
      }),
    ).toBeInstanceOf(Constr);

    const replayAuxiliaryCbor = Buffer.from(
      Data.to(
        new Constr(7, [sourceKind, key, nextScheduleHash, "00"]) as never,
      ),
      "hex",
    );
    expect(() =>
      encodeValidationSemanticResolutionRedeemerV1({
        oneStepArgument: {
          resolverIndex: 7,
          semanticResolverIndex: 5,
          transitionCbor,
          auxiliaryCbor: replayAuxiliaryCbor,
        },
        inputIndex: 5n,
        outputIndex: 7n,
      }),
    ).toThrow(/ResolveInputs auxiliary witness/u);
  });

  it("validates selection-only CEK route material and necessity receipts", () => {
    const fixture = cekSelectionFixture();
    const receiptSet = necessityReceiptSet(fixture.program.envelopeHash);
    const argument = {
      resolverIndex: 11,
      semanticResolverIndex: 1,
      transitionCbor: Buffer.from("d87980", "hex"),
      auxiliaryCbor: fixture.auxiliaryCbor,
      cekRouteMaterial: fixture.routeMaterial,
      cekIncrementalNecessityReceiptSet: receiptSet,
    } as const;
    expect(validateCekSubmissionEvidenceV1(argument)).toEqual({
      cekRouteMaterial: fixture.routeMaterial,
      cekIncrementalNecessityReceiptSet: receiptSet,
    });
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekRouteMaterial: undefined,
        cekIncrementalNecessityReceiptSet: undefined,
      }),
    ).toThrow(/requires complete route material/u);

    const noAuxiliaryCbor = Buffer.from(
      Data.to("NoAuxiliaryWitness", ValidationAuxiliaryWitnessV1),
      "hex",
    );
    expect(
      validateCekSubmissionEvidenceV1({
        resolverIndex: 11,
        semanticResolverIndex: 0,
        transitionCbor: argument.transitionCbor,
        auxiliaryCbor: noAuxiliaryCbor,
      }),
    ).toEqual({});
    for (const resolverIndex of [11, 12]) {
      expect(() =>
        validateCekSubmissionEvidenceV1({
          ...argument,
          resolverIndex,
          auxiliaryCbor:
            resolverIndex === 11 ? noAuxiliaryCbor : argument.auxiliaryCbor,
        }),
      ).toThrow(/permitted only for an exact program-selection witness/u);
    }
    // Route material rides only the execution-selection semantic (11/1): the
    // same selection witness under another cek semantic index is refused.
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        semanticResolverIndex: 2,
      }),
    ).toThrow(/permitted only for an exact program-selection witness/u);

    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: {
          ...receiptSet,
          programEnvelopeHash: "ff".repeat(32),
        },
      }),
    ).toThrow(/another program envelope/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            [draft.routeAttempts[0], draft.routeAttempts[1]] = [
              draft.routeAttempts[1]!,
              draft.routeAttempts[0]!,
            ];
          },
        ),
      }),
    ).toThrow(/directProof rejected attempt/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[1]!.transactions.reverse();
          },
        ),
      }),
    ).toThrow(/invalid transaction-role grammar/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[0]!.transactions.push({
              ...draft.routeAttempts[0]!.transactions[0]!,
              signedTxSha256: "19".repeat(32),
              txId: "59".repeat(32),
            });
          },
        ),
      }),
    ).toThrow(/invalid transaction-role grammar/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[0]!.transactions[0]!.signedTxSha256 = "01";
          },
        ),
      }),
    ).toThrow(/32-byte lowercase hex/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[0]!.transactions[0]!.programMaterialInputCount = 1;
          },
        ),
      }),
    ).toThrow(/invalid program-material input counts/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[1]!.transactions[0]!.programMaterialOutputOutRefs =
              [`${"ff".repeat(32)}#0`];
          },
        ),
      }),
    ).toThrow(/must bind increasing output indices of its txId/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[1]!.transactions[1]!.programMaterialReferenceInputOutRefs =
              [`${"fe".repeat(32)}#0`];
          },
        ),
      }),
    ).toThrow(/material outrefs do not match/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[0]!.transactions[0]!.txId = "01";
          },
        ),
      }),
    ).toThrow(/32-byte lowercase hex/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[0]!.transactions[0]!.transactionByteMargin =
              -115;
          },
        ),
      }),
    ).toThrow(/target-inconsistent measured margin/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[0]!.transactions[0]!.executionMemoryMargin =
              "5000000";
          },
        ),
      }),
    ).toThrow(/target-inconsistent measured margin/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[1]!.maturityWindowMarginMilliseconds += 1;
          },
        ),
      }),
    ).toThrow(/invalid maturity-window margin/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[1]!.rollbackAllowanceMilliseconds += 1;
          },
        ),
      }),
    ).toThrow(/invalid maturity-window margin/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            delete (
              draft.routeAttempts[1]! as unknown as Record<string, unknown>
            ).settlementMilliseconds;
          },
        ),
      }),
    ).toThrow(/must contain exactly/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[0]!.limitingConstraint!.measuredMargin = "-115";
          },
        ),
      }),
    ).toThrow(/invalid limiting measured margin/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[3]!.limitingConstraint = {
              type: "maxTxSize",
              measuredMargin: "4384",
            };
          },
        ),
      }),
    ).toThrow(/fit attempt contains a failed constraint/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            [
              draft.routeAttempts[3]!.transactions[1],
              draft.routeAttempts[3]!.transactions[2],
            ] = [
              draft.routeAttempts[3]!.transactions[2]!,
              draft.routeAttempts[3]!.transactions[1]!,
            ];
          },
        ),
      }),
    ).toThrow(/invalid transaction-role grammar/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            const publication = draft.routeAttempts[3]!.transactions[0]!;
            const finalContinuation =
              draft.routeAttempts[3]!.transactions.at(-1)!;
            finalContinuation.programMaterialReferenceInputOutRefs = [
              (publication.programMaterialOutputOutRefs as string[])[1]!,
            ];
          },
        ),
      }),
    ).toThrow(/omit published material sources/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            const publication = draft.routeAttempts[3]!.transactions[0]!;
            const continuation = draft.routeAttempts[3]!.transactions[2]!;
            continuation.programMaterialReferenceInputOutRefs = [
              (publication.programMaterialOutputOutRefs as string[])[0]!,
              ...(continuation.programMaterialReferenceInputOutRefs as string[]),
            ];
            continuation.programMaterialReferenceInputCount = 2;
          },
        ),
      }),
    ).not.toThrow();
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            const reconstruction = draft.routeAttempts[2]!.transactions[2]!;
            const referenceOutRefs = [
              ...(reconstruction.programMaterialReferenceInputOutRefs as string[]),
            ];
            referenceOutRefs[0] = (
              reconstruction.programMaterialConsumedInputOutRefs as string[]
            )[0]!;
            reconstruction.programMaterialReferenceInputOutRefs =
              referenceOutRefs;
          },
        ),
      }),
    ).toThrow(/consumed and reference inputs must be disjoint/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[2]!.minimumMultiOutputCount = 4;
          },
        ),
      }),
    ).toThrow(/do not match the exact minimum/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[2]!.minimumMultiOutputCount = 1;
          },
        ),
      }),
    ).toThrow(/must be at least two/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[0]!.minimumMultiOutputCount = 2;
          },
        ),
      }),
    ).toThrow(/invalid for the selected route/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.validatorIdentities.reverse();
          },
        ),
      }),
    ).toThrow(/strictly sorted without duplicates/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.validatorIdentities[1]!.title =
              draft.validatorIdentities[0]!.title;
          },
        ),
      }),
    ).toThrow(/strictly sorted without duplicates/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.validatorIdentities.length = 0;
          },
        ),
      }),
    ).toThrow(/requires validator identities/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.validatorIdentities[0]!.generatedHash = "02".repeat(27);
          },
        ),
      }),
    ).toThrow(/28-byte lowercase hex/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            draft.routeAttempts[1]!.transactions[0]!.signedTxSha256 =
              draft.routeAttempts[0]!.transactions[0]!.signedTxSha256;
          },
        ),
      }),
    ).toThrow(/duplicate transaction identities/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            delete draft.routeAttempts[0]!.transactions[0]!.feeLovelace;
          },
        ),
      }),
    ).toThrow(/must contain exactly/u);
    expect(() =>
      validateCekSubmissionEvidenceV1({
        ...argument,
        cekIncrementalNecessityReceiptSet: mutateNecessityReceiptSet(
          receiptSet,
          (draft) => {
            delete draft.targetProtocolParameters.maxValueSize;
          },
        ),
      }),
    ).toThrow(/target protocol parameters must contain exactly/u);
  });

  it("reads explicit CEK route files and rejects partial or unexpected fields", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-cek-route-"));
    const fixture = cekSelectionFixture();
    const receiptSet = necessityReceiptSet(fixture.program.envelopeHash);
    const paths = {
      transition: join(directory, "transition.cbor"),
      auxiliary: join(directory, "auxiliary.cbor"),
      envelope: join(directory, "envelope.cbor"),
      sidecar: join(directory, "sidecar.cbor"),
      receipts: join(directory, "receipts.json"),
    };
    await Promise.all([
      writeFile(paths.transition, "d87980\n"),
      writeFile(paths.auxiliary, `${fixture.auxiliaryCbor.toString("hex")}\n`),
      writeFile(
        paths.envelope,
        `${fixture.program.envelopeCbor.toString("hex")}\n`,
      ),
      writeFile(
        paths.sidecar,
        `${fixture.programMaterialSidecarCbor.toString("hex")}\n`,
      ),
      writeFile(paths.receipts, `${JSON.stringify(receiptSet)}\n`),
    ]);
    const base = {
      validationTransitionCborPath: paths.transition,
      validationAuxiliaryCborPath: paths.auxiliary,
      validationResolverIndex: 11,
      validationSemanticResolverIndex: 1,
    } as const;
    await expect(
      validationOneStepArgumentFromFiles({
        ...base,
        validationCekEnvelopeCborPath: paths.envelope,
        validationCekProgramMaterialSidecarCborPath: paths.sidecar,
        validationCekIncrementalNecessityReceiptSetPath: paths.receipts,
      }),
    ).resolves.toMatchObject({
      resolverIndex: 11,
      cekRouteMaterial: fixture.routeMaterial,
      cekIncrementalNecessityReceiptSet: receiptSet,
    });
    await expect(
      validationOneStepArgumentFromFiles({
        ...base,
        validationCekEnvelopeCborPath: paths.envelope,
      }),
    ).rejects.toThrow(/both explicit envelope and program-material sidecar/u);
    await expect(
      validationOneStepArgumentFromFiles({
        ...base,
        validationCekIncrementalNecessityReceiptSetPath: paths.receipts,
      }),
    ).rejects.toThrow(/require explicit CEK route material paths/u);
    await expect(validationOneStepArgumentFromFiles(base)).rejects.toThrow(
      /requires complete route material/u,
    );
    await expect(
      validationOneStepArgumentFromFiles({
        ...base,
        validationResolverIndex: 12,
        validationCekEnvelopeCborPath: paths.envelope,
        validationCekProgramMaterialSidecarCborPath: paths.sidecar,
      }),
    ).rejects.toThrow(/permitted only for an exact program-selection witness/u);
  });

  it("reads exact lowercase CBOR files and rejects ambiguous wrappers", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-dispute-cbor-"));
    const rawPath = join(directory, "raw.cbor");
    const wrappedPath = join(directory, "wrapped.json");
    const ambiguousPath = join(directory, "ambiguous.json");
    await Promise.all([
      writeFile(rawPath, "d87980\n"),
      writeFile(wrappedPath, '{"cborHex":"d87980"}\n'),
      writeFile(ambiguousPath, '{"cborHex":"d87980","unexpected":true}\n'),
    ]);
    await expect(
      readValidationDisputeCborFile(rawPath, "fixture"),
    ).resolves.toBe("d87980");
    await expect(
      readValidationDisputeCborFile(wrappedPath, "fixture"),
    ).resolves.toBe("d87980");
    await expect(
      readValidationDisputeCborFile(ambiguousPath, "fixture"),
    ).rejects.toThrow(/exactly a cborHex field/u);
  });
});
