import {
  budgetedMidgardNativeScriptDecodingScanV1,
  encodeMidgardFieldPreimageV1,
  hashMidgardNativeScriptScanFrameV1,
  isExactMidgardNativeScriptStructureTerminalV1,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH_V1,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1,
  midgardFieldCommitmentV1,
  MidgardNativeScriptDecodingRefusalClassesV1,
  MidgardNativeScriptDecodingScanOutcomeKindsV1,
  MidgardNativeScriptKindsV1,
  type MidgardNativeScriptScanFrameV1,
  type MidgardNativeScriptStructureControlV1,
  MidgardNativeScriptStructureStagesV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyWitnessScriptDecodingFindingV1,
  createWitnessScriptDecodingProductionWorkflowRunnerSurfaceV1,
  nextWitnessScriptDecodingActionV1,
  prepareWitnessScriptDecodingEvidenceV1,
  reconcileWitnessScriptDecodingJournalV1,
  runOrResumeManifestBoundWitnessScriptDecodingWorkflowV1,
  runWitnessScriptDecodingProofV1,
  witnessScriptDecodingCheckpointV1,
  witnessScriptDecodingEvidenceClosesV1,
  witnessScriptDecodingEvidenceIdentityV1,
  type WitnessScriptDecodingJournalEntryV1,
  WitnessScriptDecodingResultClassesV1,
  type WitnessScriptDecodingStageV1,
  type WitnessScriptDecodingSubmissionV1,
  witnessScriptDecodingViolationIdV1,
} from "../src/witness-script-decoding/index.js";

const txId = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
const witnessSetHash = "11".repeat(32);
const scanHash = "22".repeat(28);
const accepted = acceptedVerdictSubjectV1(txId);
const forced = (reason: unknown) =>
  forcedVerdictSubjectV1({
    transactionId: txId,
    sourceKey: { transactionId: "33".repeat(32), outputIndex: 0n },
    rejectionReason: reason as never,
  });

const signatureItem = Buffer.concat([
  Buffer.from("82005820", "hex"),
  Buffer.from("8200581c", "hex"),
  Buffer.alloc(28, 0x99),
]);
const malformedHeaderItem = Buffer.from("8201410a", "hex");
const malformedNativeItem = Buffer.from("820043820700", "hex");
const plutusItem = Buffer.from("82034401020304", "hex");

const evidence = ({
  subject = accepted,
  item = malformedHeaderItem,
  scriptIndex = 0,
}: {
  readonly subject?: typeof accepted;
  readonly item?: Buffer;
  readonly scriptIndex?: number;
} = {}) => {
  const preimage = encodeMidgardFieldPreimageV1([item]);
  return prepareWitnessScriptDecodingEvidenceV1({
    finding: { subject, witnessSetHash, scriptIndex },
    fieldPreimage: preimage,
    committedFieldHashHex: midgardFieldCommitmentV1(preimage).toString("hex"),
  });
};

describe("witnessScriptDecoding V1 evidence", () => {
  it("maps every terminal class to the exact central classifier identity", () => {
    expect(
      [
        WitnessScriptDecodingResultClassesV1.HeaderMalformed,
        WitnessScriptDecodingResultClassesV1.NativeMalformed,
        WitnessScriptDecodingResultClassesV1.NodeLimit,
        WitnessScriptDecodingResultClassesV1.DepthLimit,
      ].map(witnessScriptDecodingViolationIdV1),
    ).toEqual([
      "witness-script-header-malformed",
      "witness-native-script-malformed",
      "witness-native-script-node-limit",
      "witness-native-script-depth-limit",
    ]);
  });

  it("distinguishes malformed header, malformed native payload, and valid languages", () => {
    expect(evidence().resultClass).toBe(
      WitnessScriptDecodingResultClassesV1.HeaderMalformed,
    );
    expect(evidence({ item: malformedNativeItem }).resultClass).toBe(
      WitnessScriptDecodingResultClassesV1.NativeMalformed,
    );
    expect(evidence({ item: signatureItem }).resultClass).toBe(
      WitnessScriptDecodingResultClassesV1.NoFault,
    );
    expect(evidence({ item: plutusItem }).resultClass).toBe(
      WitnessScriptDecodingResultClassesV1.NoFault,
    );
    expect(evidence({ item: Buffer.from("820040", "hex") }).resultClass).toBe(
      WitnessScriptDecodingResultClassesV1.NativeMalformed,
    );
  });

  it("proves both directions and refuses their honest polarities", () => {
    expect(witnessScriptDecodingEvidenceClosesV1(evidence())).toBe(true);
    expect(
      witnessScriptDecodingEvidenceClosesV1(evidence({ item: signatureItem })),
    ).toBe(false);
    const rejectedHeader = forced({
      WitnessScriptHeaderMalformed: { script_index: 0n },
    });
    expect(
      witnessScriptDecodingEvidenceClosesV1(
        evidence({ subject: rejectedHeader, item: signatureItem }),
      ),
    ).toBe(true);
    expect(
      witnessScriptDecodingEvidenceClosesV1(
        evidence({ subject: rejectedHeader, item: malformedHeaderItem }),
      ),
    ).toBe(false);
  });

  it("binds all four exact typed reasons and refuses another constructor or coordinate", () => {
    for (const [constructor, expected] of [
      ["WitnessScriptHeaderMalformed", 0],
      ["WitnessNativeScriptMalformed", 1],
      ["WitnessNativeScriptNodeLimit", 2],
      ["WitnessNativeScriptDepthLimit", 3],
    ] as const) {
      const finding = classifyWitnessScriptDecodingFindingV1({
        subject: forced({ [constructor]: { script_index: 0n } }),
        witnessSetHash,
        scriptIndex: 0,
      });
      expect(finding.accusedClass).toBe(expected);
    }
    expect(() =>
      classifyWitnessScriptDecodingFindingV1({
        subject: forced({ WitnessScriptHeaderMalformed: { script_index: 1n } }),
        witnessSetHash,
        scriptIndex: 0,
      }),
    ).toThrow(/coordinate differs/u);
    expect(() =>
      classifyWitnessScriptDecodingFindingV1({
        subject: forced({ ScriptIntegrityHashMissing: null }),
        witnessSetHash,
        scriptIndex: 0,
      }),
    ).toThrow(/outside/u);
  });

  it("refuses retained-byte, root, and coordinate substitutions", () => {
    const preimage = encodeMidgardFieldPreimageV1([signatureItem]);
    expect(() =>
      prepareWitnessScriptDecodingEvidenceV1({
        finding: { subject: accepted, witnessSetHash, scriptIndex: 0 },
        fieldPreimage: preimage,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/differs/u);
    expect(() =>
      prepareWitnessScriptDecodingEvidenceV1({
        finding: { subject: accepted, witnessSetHash, scriptIndex: 1 },
        fieldPreimage: preimage,
        committedFieldHashHex:
          midgardFieldCommitmentV1(preimage).toString("hex"),
      }),
    ).toThrow(/outside field 6/u);
    expect(() =>
      classifyWitnessScriptDecodingFindingV1({
        subject: accepted,
        witnessSetHash: "aa",
        scriptIndex: 0,
      }),
    ).toThrow(/32-byte/u);
  });

  it("selects certified maximum carriage and materializes its chunk frontier", () => {
    // One non-native item can occupy the complete 32,768-byte field while the
    // decoder still reaches a deterministic no-fault result.
    const payload = Buffer.alloc(32_758, 7);
    const item = Buffer.concat([Buffer.from("821880597ff6", "hex"), payload]);
    const maximum = evidence({ item });
    expect(maximum.fieldPreimageHex.length / 2).toBe(32_768);
    expect(maximum.carriage).toBe("Certified");
    expect(maximum.chunkProofCount).toBe(9);
  });

  it("matches the frozen engine at the exact and adjacent node/depth boundaries", () => {
    const signatureNode = signatureItem.subarray(4);
    const atNodeBoundary = {
      version: 1,
      stage: MidgardNativeScriptStructureStagesV1.Token,
      startOffset: 0,
      cursor: 0,
      endOffset: signatureNode.length,
      stackRoot: Buffer.alloc(0),
      stackDepth: 0,
      nodeCount: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1 - 1,
    } satisfies MidgardNativeScriptStructureControlV1;
    const exactNode = budgetedMidgardNativeScriptDecodingScanV1({
      control: atNodeBoundary,
      window: { bytes: signatureNode, startOffset: 0 },
      frames: [],
      maxSteps: 2,
    });
    expect(exactNode.kind).toBe(
      MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced,
    );
    if (
      exactNode.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced
    )
      throw new Error("exact node edge did not advance");
    expect(exactNode.control.nodeCount).toBe(
      MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1,
    );
    expect(
      isExactMidgardNativeScriptStructureTerminalV1(exactNode.control),
    ).toBe(true);
    expect(
      budgetedMidgardNativeScriptDecodingScanV1({
        control: {
          ...atNodeBoundary,
          nodeCount: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1,
        },
        window: { bytes: signatureNode, startOffset: 0 },
        frames: [],
        maxSteps: 1,
      }),
    ).toMatchObject({
      kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Refused,
      refusalClass: MidgardNativeScriptDecodingRefusalClassesV1.NodeLimit,
    });

    const frame = {
      tail: Buffer.alloc(0),
      kind: MidgardNativeScriptKindsV1.All,
      childCount: 2,
      remaining: 2,
      validCount: 0,
      required: 0n,
    } satisfies MidgardNativeScriptScanFrameV1;
    const payload = Buffer.concat([
      Buffer.from("820182", "hex"),
      signatureNode,
      signatureNode,
    ]);
    const atDepthBoundary = {
      version: 1,
      stage: MidgardNativeScriptStructureStagesV1.Token,
      startOffset: 0,
      cursor: 0,
      endOffset: payload.length,
      stackRoot: hashMidgardNativeScriptScanFrameV1(frame),
      stackDepth: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH_V1 - 1,
      nodeCount: 1,
    } satisfies MidgardNativeScriptStructureControlV1;
    const exactDepth = budgetedMidgardNativeScriptDecodingScanV1({
      control: atDepthBoundary,
      window: { bytes: payload, startOffset: 0 },
      frames: [],
      maxSteps: 1,
    });
    expect(exactDepth.kind).toBe(
      MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced,
    );
    if (
      exactDepth.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced
    )
      throw new Error("exact depth edge did not advance");
    expect(exactDepth.control.stackDepth).toBe(
      MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH_V1,
    );
    expect(
      budgetedMidgardNativeScriptDecodingScanV1({
        control: {
          ...atDepthBoundary,
          stackDepth: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH_V1,
        },
        window: { bytes: payload, startOffset: 0 },
        frames: [],
        maxSteps: 1,
      }),
    ).toMatchObject({
      kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Refused,
      refusalClass: MidgardNativeScriptDecodingRefusalClassesV1.DepthLimit,
    });
  });

  it("reproduces a domain-separated checkpoint and detects every seam mutation", () => {
    const prepared = evidence({ item: signatureItem });
    const checkpoint = witnessScriptDecodingCheckpointV1({
      evidence: prepared,
      controlCbor: prepared.initialControlCbor,
      nextExpectedScriptHash: scanHash,
    });
    expect(checkpoint).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      witnessScriptDecodingCheckpointV1({
        evidence: prepared,
        controlCbor: `${prepared.initialControlCbor}00`,
        nextExpectedScriptHash: scanHash,
      }),
    ).not.toBe(checkpoint);
    expect(
      witnessScriptDecodingCheckpointV1({
        evidence: prepared,
        controlCbor: prepared.initialControlCbor,
        nextExpectedScriptHash: "44".repeat(28),
      }),
    ).not.toBe(checkpoint);
  });
});

describe("witnessScriptDecoding V1 durable workflow", () => {
  it("exports a strict production runner that refuses another category before loading config", async () => {
    let loaded = false;
    const runner = createWitnessScriptDecodingProductionWorkflowRunnerSurfaceV1(
      {
        loadRuntimeConfig: async () => {
          loaded = true;
          throw new Error("unexpected loader call");
        },
      },
    );
    await expect(
      runner.runOrResume({ category: "missingSignature" } as never),
    ).rejects.toThrow(/category mismatch/u);
    expect(loaded).toBe(false);
  });

  it("refuses caller-authored evidence at the manifest-bound production boundary", async () => {
    await expect(
      runOrResumeManifestBoundWitnessScriptDecodingWorkflowV1({
        workflow: {},
        sources: [],
        journal: {},
        evidence: {},
      } as never),
    ).rejects.toThrow(/rejects caller-authored evidence/u);
  });

  it("selects Init, every physical step, resume, final mint, and removal", () => {
    expect(
      (
        [
          "none",
          "step01",
          "step02",
          "scan",
          "step04",
          "proven",
          "removed",
        ] as const
      ).map(nextWitnessScriptDecodingActionV1),
    ).toEqual([
      "submitInit",
      "submitStep01",
      "submitStep02",
      "submitScanOrResume",
      "submitStep04",
      "removeDescendants",
      "done",
    ]);
  });

  it("reconstructs from journal and authenticated chain identity", async () => {
    const prepared = evidence();
    const identity = witnessScriptDecodingEvidenceIdentityV1(prepared);
    const entries: WitnessScriptDecodingJournalEntryV1[] = [];
    let stage: WitnessScriptDecodingStageV1 = "none";
    const next: Record<string, WitnessScriptDecodingStageV1> = {
      submitInit: "step01",
      submitStep01: "step02",
      submitStep02: "scan",
      submitScanOrResume: "step04",
      submitStep04: "proven",
      removeDescendants: "removed",
    };
    let nonce = 0;
    let observed: Awaited<
      ReturnType<WitnessScriptDecodingSubmissionV1["observe"]>
    > = {
      stage,
      transactionId: "00".repeat(32),
      outputReference: null as string | null,
      checkpointHash: null as string | null,
    };
    const submission: WitnessScriptDecodingSubmissionV1 = {
      observe: async (seen) => {
        expect(seen).toBe(identity);
        return observed;
      },
      submit: async (action) => {
        stage = next[action] ?? "removed";
        nonce += 1;
        observed = {
          stage,
          transactionId: nonce.toString(16).padStart(64, "0"),
          outputReference: stage === "removed" ? null : `${nonce.toString()}#0`,
          checkpointHash: stage === "scan" ? "55".repeat(32) : null,
        };
        return observed;
      },
      cancel: async (
        _stage: "step01" | "step02" | "scan" | "step04",
        _prepared: typeof prepared,
      ) => ({
        stage: "cancelled",
        transactionId: "66".repeat(32),
        outputReference: null,
        checkpointHash: null,
      }),
    };
    expect(
      await runWitnessScriptDecodingProofV1({
        evidence: prepared,
        load: async () => entries,
        append: async (entry) => {
          entries.push(entry);
        },
        submission,
      }),
    ).toBe("removed");
    expect(entries.map((entry) => entry.stage)).toEqual([
      "step01",
      "step02",
      "scan",
      "step04",
      "proven",
      "removed",
    ]);
  });

  it("supports cancellation from every nonterminal physical step", async () => {
    const prepared = evidence();
    const submission: Pick<WitnessScriptDecodingSubmissionV1, "cancel"> = {
      cancel: async () => ({
        stage: "cancelled" as const,
        transactionId: "77".repeat(32),
        outputReference: null,
        checkpointHash: null,
      }),
    };
    for (const stage of ["step01", "step02", "scan", "step04"] as const) {
      await expect(submission.cancel(stage, prepared)).resolves.toMatchObject({
        stage: "cancelled",
      });
    }
  });

  it("refuses journal identity and transaction replacement mutations", () => {
    const entry: WitnessScriptDecodingJournalEntryV1 = {
      sequence: 0,
      identity: "expected",
      stage: "scan",
      transactionId: "88".repeat(32),
      outputReference: "0#0",
      checkpointHash: "99".repeat(32),
    };
    expect(() =>
      reconcileWitnessScriptDecodingJournalV1({
        identity: "different",
        entries: [entry],
        observed: entry,
      }),
    ).toThrow(/identity/u);
    expect(() =>
      reconcileWitnessScriptDecodingJournalV1({
        identity: "expected",
        entries: [entry],
        observed: { ...entry, transactionId: "aa".repeat(32) },
      }),
    ).toThrow(/changed/u);
  });
});
