import {
  buildMidgardBoundedItemV1,
  buildMidgardValidationMerkleMembershipV1,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_V1_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyExecutionSourceScriptDecodingFindingV1,
  type ExecutionSourceDescriptorV1,
  executionSourceScriptDecodingCheckpointV1,
  executionSourceScriptDecodingEvidenceClosesV1,
  executionSourceScriptDecodingEvidenceIdentityV1,
  ExecutionSourceScriptDecodingResultClassesV1,
  executionSourceScriptDecodingViolationIdV1,
  nextExecutionSourceScriptDecodingActionV1,
  prepareExecutionSourceScriptDecodingEvidenceV1,
} from "../src/execution-source-script-decoding/family-v1.js";
import { createExecutionSourceScriptDecodingActuatorV1 } from "../src/execution-source-script-decoding/production-actuator-v1.js";
import {
  createManifestBoundExecutionSourceScriptDecodingWorkflowV1,
  EXECUTION_SOURCE_SCRIPT_DECODING_PRODUCTION_CONFIG_KEYS_V1,
  EXECUTION_SOURCE_SCRIPT_DECODING_STEP_DATUM_SCHEMAS_V1,
  runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflowV1,
} from "../src/execution-source-script-decoding/production-v1.js";
import {
  detectExecutionSourceScriptDecodingAcceptedRawReplayV1,
  selectExecutionSourceScriptDecodingCanonicalFindingV1,
} from "../src/execution-source-script-decoding/replay-v1.js";
import {
  ExecutionSourceStep02DatumV1Schema,
  ExecutionSourceStep03DatumV1Schema,
  ExecutionSourceStep04DatumV1Schema,
  ExecutionSourceStep05DatumV1Schema,
} from "../src/execution-source-script-decoding/schemas-v1.js";
import {
  cancelExecutionSourceScriptDecodingWorkflowV1,
  type ExecutionSourceScriptDecodingCursorV1,
  type ExecutionSourceScriptDecodingJournalEntryV1,
  runExecutionSourceScriptDecodingWorkflowV1,
} from "../src/execution-source-script-decoding/workflow-v1.js";

const txId = "00".repeat(32);
const scriptHash = "22".repeat(28);
const accepted = acceptedVerdictSubjectV1(txId);
const forced = (constructor: string, executionIndex = 0) =>
  forcedVerdictSubjectV1({
    transactionId: txId,
    sourceKey: { transactionId: "33".repeat(32), outputIndex: 0n },
    rejectionReason: {
      [constructor]: { execution_index: BigInt(executionIndex) },
    } as never,
  });
const signatureItem = Buffer.concat([
  Buffer.from("82005820", "hex"),
  Buffer.from("8200581c", "hex"),
  Buffer.alloc(28, 0x99),
]);
const malformedItem = Buffer.from("820043820700", "hex");

const descriptor = (item: Buffer): ExecutionSourceDescriptorV1 => {
  const bounded = buildMidgardBoundedItemV1({
    fieldIndex: 6,
    itemIndex: 0,
    bytes: item,
  });
  const purposeLeaf = hashMidgardScriptPurposeLeafV1({
    purposeKind: 0,
    purposeIndex: 0n,
    scriptHash: Buffer.from(scriptHash, "hex"),
    subject: Buffer.from("aa", "hex"),
  });
  const sourceLeaf = hashMidgardInlineScriptSourceLeafV1({
    sourceIndex: 0n,
    scriptLanguageTag: 0,
    scriptHash: Buffer.from(scriptHash, "hex"),
    scriptTotalLength: item.length,
    itemCommitment: bounded.commitment,
  });
  const executionLeaf = hashMidgardScriptExecutionLeafV1({
    languageTag: 0,
    purposeLeaf,
    sourceLeaf,
  });
  return {
    sourceIndex: 0,
    originKind: 0,
    sourceKeyHex: "00",
    languageTag: 0,
    scriptHashHex: scriptHash,
    scriptItemHex: item.toString("hex"),
    purposeKind: 0,
    purposeIndex: 0,
    purposeSubjectHex: "aa",
    redeemerLeafHex: "",
    purposeMembership: buildMidgardValidationMerkleMembershipV1(
      [purposeLeaf],
      0,
    ),
    sourceMembership: buildMidgardValidationMerkleMembershipV1([sourceLeaf], 0),
    executionMembership: buildMidgardValidationMerkleMembershipV1(
      [executionLeaf],
      0,
    ),
  };
};
const evidence = (item: Buffer, subject = accepted) =>
  prepareExecutionSourceScriptDecodingEvidenceV1({
    finding: { subject, executionIndex: 0 },
    descriptor: descriptor(item),
  });

describe("executionSourceScriptDecoding V1", () => {
  it("admits infrastructure-only production configuration", async () => {
    expect(EXECUTION_SOURCE_SCRIPT_DECODING_PRODUCTION_CONFIG_KEYS_V1).toEqual([
      "manifest",
      "blueprintJson",
      "deploymentInfo",
      "headerHash",
      "lucid",
      "signer",
      "source",
      "decisionDigest",
      "stateQueueMutationLeaseCoordinator",
      "referenceScripts",
    ]);
    await expect(
      createManifestBoundExecutionSourceScriptDecodingWorkflowV1({
        manifest: {},
        blueprintJson: "{}",
        deploymentInfo: {},
        headerHash: "00".repeat(28),
        lucid: {} as never,
        signer: {} as never,
        source: {} as never,
        decisionDigest: "00".repeat(32),
        stateQueueMutationLeaseCoordinator: {} as never,
        referenceScripts: {} as never,
        submit: async () => "00".repeat(32),
      } as never),
    ).rejects.toThrow(/callback authority/u);
  });

  it("binds generic Init then the exact five physical datum ABIs", () => {
    expect(EXECUTION_SOURCE_SCRIPT_DECODING_STEP_DATUM_SCHEMAS_V1).toEqual([
      FraudProofComputationThreadStepDatum,
      ExecutionSourceStep02DatumV1Schema,
      ExecutionSourceStep03DatumV1Schema,
      ExecutionSourceStep04DatumV1Schema,
      ExecutionSourceStep05DatumV1Schema,
    ]);
  });

  it("owns every transaction capture action inside the family actuator", () => {
    const actuator = createExecutionSourceScriptDecodingActuatorV1({
      binding: {} as never,
      lucid: {} as never,
      signer: {} as never,
      contracts: {} as never,
      references: {} as never,
      stateQueueMutationLeaseCoordinator: {} as never,
    });
    expect(Object.keys(actuator)).toEqual(["capture"]);
  });

  it("rejects caller-authored evidence at the production runner boundary", async () => {
    await expect(
      runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflowV1({
        workflow: {} as never,
        sources: [],
        journal: {} as never,
        evidence: evidence(malformedItem),
      } as never),
    ).rejects.toThrow(/caller-authored evidence/u);
  });
  it("binds the frozen ID reason arms and exact execution coordinate", () => {
    expect(
      [0, 1, 2].map((value) =>
        executionSourceScriptDecodingViolationIdV1(value as 0 | 1 | 2),
      ),
    ).toEqual([
      "execution-native-script-malformed",
      "execution-native-script-node-limit",
      "execution-native-script-depth-limit",
    ]);
    for (const [constructor, resultClass] of [
      ["ExecutionNativeScriptMalformed", 0],
      ["ExecutionNativeScriptNodeLimit", 1],
      ["ExecutionNativeScriptDepthLimit", 2],
    ] as const) {
      expect(
        classifyExecutionSourceScriptDecodingFindingV1({
          subject: forced(constructor),
          executionIndex: 0,
        }).accusedClass,
      ).toBe(resultClass);
    }
    expect(() =>
      classifyExecutionSourceScriptDecodingFindingV1({
        subject: forced("ExecutionNativeScriptMalformed", 1),
        executionIndex: 0,
      }),
    ).toThrow(/coordinate differs/u);
  });

  it("authenticates purpose, source, and execution memberships", () => {
    const exact = evidence(signatureItem);
    expect(exact.itemLength).toBe(signatureItem.length);
    expect(exact.chunkProofCount).toBe(1);
    const substituted = descriptor(signatureItem);
    expect(() =>
      prepareExecutionSourceScriptDecodingEvidenceV1({
        finding: { subject: accepted, executionIndex: 0 },
        descriptor: {
          ...substituted,
          sourceMembership: {
            ...substituted.sourceMembership,
            leafHash: Buffer.alloc(32, 0x55),
          },
        },
      }),
    ).toThrow(/source frontier membership was substituted/u);
  });

  it("refuses root, descriptor-coordinate, and raw-item substitution", () => {
    const exact = descriptor(malformedItem);
    expect(() =>
      prepareExecutionSourceScriptDecodingEvidenceV1({
        finding: { subject: accepted, executionIndex: 0 },
        descriptor: {
          ...exact,
          purposeMembership: {
            ...exact.purposeMembership,
            frontier: {
              ...exact.purposeMembership.frontier,
              peaks: exact.purposeMembership.frontier.peaks.map((peak) => ({
                ...peak,
                hash: Buffer.alloc(32, 0xee),
              })),
            },
          },
        },
      }),
    ).toThrow(/purpose frontier membership was substituted/u);
    expect(() =>
      prepareExecutionSourceScriptDecodingEvidenceV1({
        finding: { subject: accepted, executionIndex: 0 },
        descriptor: { ...exact, purposeIndex: 1 },
      }),
    ).toThrow(/purpose frontier membership was substituted/u);
    expect(() =>
      prepareExecutionSourceScriptDecodingEvidenceV1({
        finding: { subject: accepted, executionIndex: 0 },
        descriptor: { ...exact, scriptItemHex: signatureItem.toString("hex") },
      }),
    ).toThrow(/source frontier membership was substituted/u);
  });

  it("closes malformed acceptance and decodable wrongful rejection only", () => {
    expect(
      executionSourceScriptDecodingEvidenceClosesV1(evidence(malformedItem)),
    ).toBe(true);
    expect(
      executionSourceScriptDecodingEvidenceClosesV1(evidence(signatureItem)),
    ).toBe(false);
    expect(
      executionSourceScriptDecodingEvidenceClosesV1(
        evidence(signatureItem, forced("ExecutionNativeScriptMalformed")),
      ),
    ).toBe(true);
    expect(
      executionSourceScriptDecodingEvidenceClosesV1(
        evidence(malformedItem, forced("ExecutionNativeScriptMalformed")),
      ),
    ).toBe(false);
  });

  it("binds resume checkpoints and identity to source and successor", () => {
    const exact = evidence(signatureItem);
    const first = executionSourceScriptDecodingCheckpointV1({
      evidence: exact,
      controlCbor: exact.initialControlCbor,
      nextExpectedScriptHash: "44".repeat(28),
    });
    const next = executionSourceScriptDecodingCheckpointV1({
      evidence: exact,
      controlCbor: exact.initialControlCbor,
      nextExpectedScriptHash: "45".repeat(28),
    });
    expect(first).toMatch(/^[0-9a-f]{64}$/u);
    expect(next).not.toBe(first);
    expect(executionSourceScriptDecodingEvidenceIdentityV1(exact)).toContain(
      exact.executionLeafHex,
    );
    expect(nextExecutionSourceScriptDecodingActionV1("scan")).toBe(
      "submitScanOrResume",
    );
  });

  it("materializes the maximum 32768-byte nine-chunk source item", () => {
    const payload = Buffer.alloc(32_761, 0);
    const item = Buffer.concat([
      Buffer.from([0x82, 0x00, 0x5a, 0x00, 0x00, 0x7f, 0xf9]),
      payload,
    ]);
    expect(item.length).toBe(32_768);
    const maximum = evidence(item);
    expect(maximum.chunkProofCount).toBe(9);
    expect(maximum.resultClass).toBe(
      ExecutionSourceScriptDecodingResultClassesV1.Malformed,
    );
  });

  it("retains malformed accepted field-6 bytes before canonical decoding", () => {
    const raw = materializeMidgardNativeTxFromCanonicalV1({
      version: MIDGARD_NATIVE_TX_V1_VERSION,
      validity: "TxIsValid",
      body: {
        spendInputsPreimageCbor: EMPTY_CBOR_LIST,
        referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
        outputsPreimageCbor: EMPTY_CBOR_LIST,
        requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
        requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
        mintPreimageCbor: EMPTY_CBOR_LIST,
        scriptIntegrityHash: EMPTY_NULL_ROOT,
        auxiliaryDataHash: EMPTY_NULL_ROOT,
        fee: 1_000n,
        validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
        validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
        networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
      },
      witnessSet: {
        addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
        scriptTxWitsPreimageCbor: encodeCbor([malformedItem]),
        redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      },
    });
    const findings = detectExecutionSourceScriptDecodingAcceptedRawReplayV1({
      headerHash: "ab".repeat(28),
      position: 3n,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(raw),
      authenticatedDescriptors: [
        { executionIndex: 0, descriptor: descriptor(malformedItem) },
      ],
    });
    expect(findings).toHaveLength(1);
    expect(findings[0]?.detection.violationId).toBe(
      "execution-native-script-malformed",
    );
    expect(() =>
      detectExecutionSourceScriptDecodingAcceptedRawReplayV1({
        headerHash: "ab".repeat(28),
        position: 3n,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonicalV1(raw),
        authenticatedDescriptors: [
          { executionIndex: 0, descriptor: descriptor(signatureItem) },
        ],
      }),
    ).toThrow(/raw field-6 source item changed/u);
    expect(
      selectExecutionSourceScriptDecodingCanonicalFindingV1([
        {
          ...findings[0]!,
          detection: { ...findings[0]!.detection, position: 9n },
        },
        findings[0]!,
      ]).detection.position,
    ).toBe(3n);
  });

  it("journals intent before submit and resumes the exact self-loop checkpoint", async () => {
    const exact = evidence(malformedItem);
    const entries: ExecutionSourceScriptDecodingJournalEntryV1[] = [];
    let cursor: ExecutionSourceScriptDecodingCursorV1 = {
      stage: "scan",
      threadOutRef: `${"11".repeat(32)}#0`,
      checkpointHash: "22".repeat(32),
      controlCbor: "80",
    };
    let intentVisibleAtSubmit = false;
    const next = {
      ...cursor,
      threadOutRef: `${"33".repeat(32)}#0`,
      checkpointHash: "44".repeat(32),
      controlCbor: "8100",
    };
    await runExecutionSourceScriptDecodingWorkflowV1({
      evidence: exact,
      journal: {
        load: async () => entries,
        append: async (entry) => void entries.push(entry),
      },
      transactions: {
        observe: async () => cursor,
        transactionConfirmed: async () => true,
        capture: async () => ({
          txHash: "55".repeat(32),
          target: next,
          submit: async () => {
            intentVisibleAtSubmit = entries.at(-1)?.phase === "intent";
            cursor = next;
            return "55".repeat(32);
          },
        }),
      },
    });
    expect(intentVisibleAtSubmit).toBe(true);
    expect(entries.map(({ phase }) => phase)).toEqual(["intent", "submitted"]);
    await runExecutionSourceScriptDecodingWorkflowV1({
      evidence: exact,
      journal: {
        load: async () => entries,
        append: async (entry) => void entries.push(entry),
      },
      transactions: {
        observe: async () => cursor,
        transactionConfirmed: async () => true,
        capture: async () => {
          throw new Error("must reconcile before another capture");
        },
      },
    });
    expect(entries.at(-1)?.phase).toBe("confirmed");
  });

  it("refuses restart transaction/cursor substitution", async () => {
    const exact = evidence(malformedItem);
    const identity = executionSourceScriptDecodingEvidenceIdentityV1(exact);
    const source = {
      stage: "scan",
      threadOutRef: "a",
      checkpointHash: "11".repeat(32),
      controlCbor: "80",
    } as const;
    const target = {
      ...source,
      threadOutRef: "b",
      checkpointHash: "22".repeat(32),
    };
    const entries: ExecutionSourceScriptDecodingJournalEntryV1[] = [
      {
        sequence: 0,
        identity,
        action: "submitScanOrResume",
        phase: "intent",
        source,
        target,
        txHash: "33".repeat(32),
      },
    ];
    await expect(
      runExecutionSourceScriptDecodingWorkflowV1({
        evidence: exact,
        journal: { load: async () => entries, append: async () => undefined },
        transactions: {
          observe: async () => ({ ...target, checkpointHash: "44".repeat(32) }),
          transactionConfirmed: async () => true,
          capture: async () => {
            throw new Error("unreachable");
          },
        },
      }),
    ).rejects.toThrow(/cursor\/checkpoint substitution/u);
  });

  it.each(["step01", "step02", "step03", "scan"] as const)(
    "cancels authenticated nonterminal %s",
    async (stage) => {
      const exact = evidence(malformedItem);
      const entries: ExecutionSourceScriptDecodingJournalEntryV1[] = [];
      let cursor: ExecutionSourceScriptDecodingCursorV1 = {
        stage,
        threadOutRef: `${"66".repeat(32)}#0`,
        checkpointHash: "77".repeat(32),
        controlCbor: "80",
      };
      await expect(
        cancelExecutionSourceScriptDecodingWorkflowV1({
          evidence: exact,
          journal: {
            load: async () => entries,
            append: async (entry) => void entries.push(entry),
          },
          transactions: {
            observe: async () => cursor,
            transactionConfirmed: async () => true,
            capture: async ({ action }) => ({
              txHash: "88".repeat(32),
              target: { ...cursor, stage: "cancelled" },
              submit: async () => {
                expect(action).toBe("cancel");
                cursor = { ...cursor, stage: "cancelled" };
                return "88".repeat(32);
              },
            }),
          },
        }),
      ).resolves.toBe("cancelled");
      expect(entries.map(({ phase }) => phase)).toEqual([
        "intent",
        "submitted",
        "confirmed",
      ]);
    },
  );
});
