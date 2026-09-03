import {
  buildMidgardBoundedItem,
  buildMidgardValidationMerkleMembership,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyExecutionSourceScriptDecodingFinding,
  type ExecutionSourceDescriptor,
  executionSourceScriptDecodingCheckpoint,
  executionSourceScriptDecodingEvidenceCloses,
  executionSourceScriptDecodingEvidenceIdentity,
  ExecutionSourceScriptDecodingResultClasses,
  executionSourceScriptDecodingViolationId,
  nextExecutionSourceScriptDecodingAction,
  prepareExecutionSourceScriptDecodingEvidence,
} from "../src/execution-source-script-decoding/family-v1.js";
import { createExecutionSourceScriptDecodingActuator } from "../src/execution-source-script-decoding/production-actuator-v1.js";
import {
  createManifestBoundExecutionSourceScriptDecodingWorkflow,
  EXECUTION_SOURCE_SCRIPT_DECODING_CONFIG_KEYS,
  EXECUTION_SOURCE_SCRIPT_DECODING_STEP_DATUM_SCHEMAS,
  runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflow,
} from "../src/execution-source-script-decoding/production-v1.js";
import {
  detectExecutionSourceScriptDecodingAcceptedRawReplay,
  selectExecutionSourceScriptDecodingCanonicalFinding,
} from "../src/execution-source-script-decoding/replay-v1.js";
import {
  ExecutionSourceStep02DatumSchema,
  ExecutionSourceStep03DatumSchema,
  ExecutionSourceStep04DatumSchema,
  ExecutionSourceStep05DatumSchema,
} from "../src/execution-source-script-decoding/schemas-v1.js";
import {
  cancelExecutionSourceScriptDecodingWorkflow,
  type ExecutionSourceScriptDecodingCursor,
  type ExecutionSourceScriptDecodingJournalEntry,
  runExecutionSourceScriptDecodingWorkflow,
} from "../src/execution-source-script-decoding/workflow-v1.js";

const txId = "00".repeat(32);
const scriptHash = "22".repeat(28);
const accepted = acceptedVerdictSubject(txId);
const forced = (constructor: string, executionIndex = 0) =>
  forcedVerdictSubject({
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

const descriptor = (item: Buffer): ExecutionSourceDescriptor => {
  const bounded = buildMidgardBoundedItem({
    fieldIndex: 6,
    itemIndex: 0,
    bytes: item,
  });
  const purposeLeaf = hashMidgardScriptPurposeLeaf({
    purposeKind: 0,
    purposeIndex: 0n,
    scriptHash: Buffer.from(scriptHash, "hex"),
    subject: Buffer.from("aa", "hex"),
  });
  const sourceLeaf = hashMidgardInlineScriptSourceLeaf({
    sourceIndex: 0n,
    scriptLanguageTag: 0,
    scriptHash: Buffer.from(scriptHash, "hex"),
    scriptTotalLength: item.length,
    itemCommitment: bounded.commitment,
  });
  const executionLeaf = hashMidgardScriptExecutionLeaf({
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
    purposeMembership: buildMidgardValidationMerkleMembership([purposeLeaf], 0),
    sourceMembership: buildMidgardValidationMerkleMembership([sourceLeaf], 0),
    executionMembership: buildMidgardValidationMerkleMembership(
      [executionLeaf],
      0,
    ),
  };
};
const evidence = (item: Buffer, subject = accepted) =>
  prepareExecutionSourceScriptDecodingEvidence({
    finding: { subject, executionIndex: 0 },
    descriptor: descriptor(item),
  });

describe("executionSourceScriptDecoding V1", () => {
  it("admits infrastructure-only production configuration", async () => {
    expect(EXECUTION_SOURCE_SCRIPT_DECODING_CONFIG_KEYS).toEqual([
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
      createManifestBoundExecutionSourceScriptDecodingWorkflow({
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
    expect(EXECUTION_SOURCE_SCRIPT_DECODING_STEP_DATUM_SCHEMAS).toEqual([
      FraudProofComputationThreadStepDatum,
      ExecutionSourceStep02DatumSchema,
      ExecutionSourceStep03DatumSchema,
      ExecutionSourceStep04DatumSchema,
      ExecutionSourceStep05DatumSchema,
    ]);
  });

  it("owns every transaction capture action inside the family actuator", () => {
    const actuator = createExecutionSourceScriptDecodingActuator({
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
      runOrResumeManifestBoundExecutionSourceScriptDecodingWorkflow({
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
        executionSourceScriptDecodingViolationId(value as 0 | 1 | 2),
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
        classifyExecutionSourceScriptDecodingFinding({
          subject: forced(constructor),
          executionIndex: 0,
        }).accusedClass,
      ).toBe(resultClass);
    }
    expect(() =>
      classifyExecutionSourceScriptDecodingFinding({
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
      prepareExecutionSourceScriptDecodingEvidence({
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
      prepareExecutionSourceScriptDecodingEvidence({
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
      prepareExecutionSourceScriptDecodingEvidence({
        finding: { subject: accepted, executionIndex: 0 },
        descriptor: { ...exact, purposeIndex: 1 },
      }),
    ).toThrow(/purpose frontier membership was substituted/u);
    expect(() =>
      prepareExecutionSourceScriptDecodingEvidence({
        finding: { subject: accepted, executionIndex: 0 },
        descriptor: { ...exact, scriptItemHex: signatureItem.toString("hex") },
      }),
    ).toThrow(/source frontier membership was substituted/u);
  });

  it("closes malformed acceptance and decodable wrongful rejection only", () => {
    expect(
      executionSourceScriptDecodingEvidenceCloses(evidence(malformedItem)),
    ).toBe(true);
    expect(
      executionSourceScriptDecodingEvidenceCloses(evidence(signatureItem)),
    ).toBe(false);
    expect(
      executionSourceScriptDecodingEvidenceCloses(
        evidence(signatureItem, forced("ExecutionNativeScriptMalformed")),
      ),
    ).toBe(true);
    expect(
      executionSourceScriptDecodingEvidenceCloses(
        evidence(malformedItem, forced("ExecutionNativeScriptMalformed")),
      ),
    ).toBe(false);
  });

  it("binds resume checkpoints and identity to source and successor", () => {
    const exact = evidence(signatureItem);
    const first = executionSourceScriptDecodingCheckpoint({
      evidence: exact,
      controlCbor: exact.initialControlCbor,
      nextExpectedScriptHash: "44".repeat(28),
    });
    const next = executionSourceScriptDecodingCheckpoint({
      evidence: exact,
      controlCbor: exact.initialControlCbor,
      nextExpectedScriptHash: "45".repeat(28),
    });
    expect(first).toMatch(/^[0-9a-f]{64}$/u);
    expect(next).not.toBe(first);
    expect(executionSourceScriptDecodingEvidenceIdentity(exact)).toContain(
      exact.executionLeafHex,
    );
    expect(nextExecutionSourceScriptDecodingAction("scan")).toBe(
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
      ExecutionSourceScriptDecodingResultClasses.Malformed,
    );
  });

  it("retains malformed accepted field-6 bytes before canonical decoding", () => {
    const raw = materializeMidgardNativeTxFromCanonical({
      version: MIDGARD_NATIVE_TX_VERSION,
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
    const findings = detectExecutionSourceScriptDecodingAcceptedRawReplay({
      headerHash: "ab".repeat(28),
      position: 3n,
      canonicalTransactionCbor: encodeMidgardNativeTxCanonical(raw),
      authenticatedDescriptors: [
        { executionIndex: 0, descriptor: descriptor(malformedItem) },
      ],
    });
    expect(findings).toHaveLength(1);
    expect(findings[0]?.detection.violationId).toBe(
      "execution-native-script-malformed",
    );
    expect(() =>
      detectExecutionSourceScriptDecodingAcceptedRawReplay({
        headerHash: "ab".repeat(28),
        position: 3n,
        canonicalTransactionCbor: encodeMidgardNativeTxCanonical(raw),
        authenticatedDescriptors: [
          { executionIndex: 0, descriptor: descriptor(signatureItem) },
        ],
      }),
    ).toThrow(/raw field-6 source item changed/u);
    expect(
      selectExecutionSourceScriptDecodingCanonicalFinding([
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
    const entries: ExecutionSourceScriptDecodingJournalEntry[] = [];
    let cursor: ExecutionSourceScriptDecodingCursor = {
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
    await runExecutionSourceScriptDecodingWorkflow({
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
    await runExecutionSourceScriptDecodingWorkflow({
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
    const identity = executionSourceScriptDecodingEvidenceIdentity(exact);
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
    const entries: ExecutionSourceScriptDecodingJournalEntry[] = [
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
      runExecutionSourceScriptDecodingWorkflow({
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
      const entries: ExecutionSourceScriptDecodingJournalEntry[] = [];
      let cursor: ExecutionSourceScriptDecodingCursor = {
        stage,
        threadOutRef: `${"66".repeat(32)}#0`,
        checkpointHash: "77".repeat(32),
        controlCbor: "80",
      };
      await expect(
        cancelExecutionSourceScriptDecodingWorkflow({
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
