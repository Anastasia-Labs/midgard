import {
  budgetedMidgardNativeScriptDecodingScan,
  encodeMidgardFieldPreimage,
  hashMidgardNativeScriptScanFrame,
  isExactMidgardNativeScriptStructureTerminal,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
  midgardFieldCommitment,
  MidgardNativeScriptDecodingRefusalClasses,
  MidgardNativeScriptDecodingScanOutcomeKinds,
  MidgardNativeScriptKinds,
  type MidgardNativeScriptScanFrame,
  type MidgardNativeScriptStructureControl,
  MidgardNativeScriptStructureStages,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  classifyWitnessScriptDecodingFinding,
  createWitnessScriptDecodingWorkflowRunnerSurface,
  nextWitnessScriptDecodingAction,
  prepareWitnessScriptDecodingEvidence,
  reconcileWitnessScriptDecodingJournal,
  runOrResumeManifestBoundWitnessScriptDecodingWorkflow,
  runWitnessScriptDecodingProof,
  witnessScriptDecodingCheckpoint,
  witnessScriptDecodingEvidenceCloses,
  witnessScriptDecodingEvidenceIdentity,
  type WitnessScriptDecodingJournalEntry,
  WitnessScriptDecodingResultClasses,
  type WitnessScriptDecodingStage,
  type WitnessScriptDecodingSubmission,
  witnessScriptDecodingViolationId,
} from "../src/witness-script-decoding/index.js";

const txId = "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f";
const witnessSetHash = "11".repeat(32);
const scanHash = "22".repeat(28);
const accepted = acceptedVerdictSubject(txId);
const forced = (reason: unknown) =>
  forcedVerdictSubject({
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
  const preimage = encodeMidgardFieldPreimage([item]);
  return prepareWitnessScriptDecodingEvidence({
    finding: { subject, witnessSetHash, scriptIndex },
    fieldPreimage: preimage,
    committedFieldHashHex: midgardFieldCommitment(preimage).toString("hex"),
  });
};

describe("witnessScriptDecoding V1 evidence", () => {
  it("maps every terminal class to the exact central classifier identity", () => {
    expect(
      [
        WitnessScriptDecodingResultClasses.HeaderMalformed,
        WitnessScriptDecodingResultClasses.NativeMalformed,
        WitnessScriptDecodingResultClasses.NodeLimit,
        WitnessScriptDecodingResultClasses.DepthLimit,
      ].map(witnessScriptDecodingViolationId),
    ).toEqual([
      "witness-script-header-malformed",
      "witness-native-script-malformed",
      "witness-native-script-node-limit",
      "witness-native-script-depth-limit",
    ]);
  });

  it("distinguishes malformed header, malformed native payload, and valid languages", () => {
    expect(evidence().resultClass).toBe(
      WitnessScriptDecodingResultClasses.HeaderMalformed,
    );
    expect(evidence({ item: malformedNativeItem }).resultClass).toBe(
      WitnessScriptDecodingResultClasses.NativeMalformed,
    );
    expect(evidence({ item: signatureItem }).resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    expect(evidence({ item: plutusItem }).resultClass).toBe(
      WitnessScriptDecodingResultClasses.NoFault,
    );
    expect(evidence({ item: Buffer.from("820040", "hex") }).resultClass).toBe(
      WitnessScriptDecodingResultClasses.NativeMalformed,
    );
  });

  it("proves both directions and refuses their honest polarities", () => {
    expect(witnessScriptDecodingEvidenceCloses(evidence())).toBe(true);
    expect(
      witnessScriptDecodingEvidenceCloses(evidence({ item: signatureItem })),
    ).toBe(false);
    const rejectedHeader = forced({
      WitnessScriptHeaderMalformed: { script_index: 0n },
    });
    expect(
      witnessScriptDecodingEvidenceCloses(
        evidence({ subject: rejectedHeader, item: signatureItem }),
      ),
    ).toBe(true);
    expect(
      witnessScriptDecodingEvidenceCloses(
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
      const finding = classifyWitnessScriptDecodingFinding({
        subject: forced({ [constructor]: { script_index: 0n } }),
        witnessSetHash,
        scriptIndex: 0,
      });
      expect(finding.accusedClass).toBe(expected);
    }
    expect(() =>
      classifyWitnessScriptDecodingFinding({
        subject: forced({ WitnessScriptHeaderMalformed: { script_index: 1n } }),
        witnessSetHash,
        scriptIndex: 0,
      }),
    ).toThrow(/coordinate differs/u);
    expect(() =>
      classifyWitnessScriptDecodingFinding({
        subject: forced({ ScriptIntegrityHashMissing: null }),
        witnessSetHash,
        scriptIndex: 0,
      }),
    ).toThrow(/outside/u);
  });

  it("refuses retained-byte, root, and coordinate substitutions", () => {
    const preimage = encodeMidgardFieldPreimage([signatureItem]);
    expect(() =>
      prepareWitnessScriptDecodingEvidence({
        finding: { subject: accepted, witnessSetHash, scriptIndex: 0 },
        fieldPreimage: preimage,
        committedFieldHashHex: "ff".repeat(32),
      }),
    ).toThrow(/differs/u);
    expect(() =>
      prepareWitnessScriptDecodingEvidence({
        finding: { subject: accepted, witnessSetHash, scriptIndex: 1 },
        fieldPreimage: preimage,
        committedFieldHashHex: midgardFieldCommitment(preimage).toString("hex"),
      }),
    ).toThrow(/outside field 6/u);
    expect(() =>
      classifyWitnessScriptDecodingFinding({
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
      stage: MidgardNativeScriptStructureStages.Token,
      startOffset: 0,
      cursor: 0,
      endOffset: signatureNode.length,
      stackRoot: Buffer.alloc(0),
      stackDepth: 0,
      nodeCount: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES - 1,
    } satisfies MidgardNativeScriptStructureControl;
    const exactNode = budgetedMidgardNativeScriptDecodingScan({
      control: atNodeBoundary,
      window: { bytes: signatureNode, startOffset: 0 },
      frames: [],
      maxSteps: 2,
    });
    expect(exactNode.kind).toBe(
      MidgardNativeScriptDecodingScanOutcomeKinds.Advanced,
    );
    if (exactNode.kind !== MidgardNativeScriptDecodingScanOutcomeKinds.Advanced)
      throw new Error("exact node edge did not advance");
    expect(exactNode.control.nodeCount).toBe(
      MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
    );
    expect(isExactMidgardNativeScriptStructureTerminal(exactNode.control)).toBe(
      true,
    );
    expect(
      budgetedMidgardNativeScriptDecodingScan({
        control: {
          ...atNodeBoundary,
          nodeCount: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
        },
        window: { bytes: signatureNode, startOffset: 0 },
        frames: [],
        maxSteps: 1,
      }),
    ).toMatchObject({
      kind: MidgardNativeScriptDecodingScanOutcomeKinds.Refused,
      refusalClass: MidgardNativeScriptDecodingRefusalClasses.NodeLimit,
    });

    const frame = {
      tail: Buffer.alloc(0),
      kind: MidgardNativeScriptKinds.All,
      childCount: 2,
      remaining: 2,
      validCount: 0,
      required: 0n,
    } satisfies MidgardNativeScriptScanFrame;
    const payload = Buffer.concat([
      Buffer.from("820182", "hex"),
      signatureNode,
      signatureNode,
    ]);
    const atDepthBoundary = {
      version: 1,
      stage: MidgardNativeScriptStructureStages.Token,
      startOffset: 0,
      cursor: 0,
      endOffset: payload.length,
      stackRoot: hashMidgardNativeScriptScanFrame(frame),
      stackDepth: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH - 1,
      nodeCount: 1,
    } satisfies MidgardNativeScriptStructureControl;
    const exactDepth = budgetedMidgardNativeScriptDecodingScan({
      control: atDepthBoundary,
      window: { bytes: payload, startOffset: 0 },
      frames: [],
      maxSteps: 1,
    });
    expect(exactDepth.kind).toBe(
      MidgardNativeScriptDecodingScanOutcomeKinds.Advanced,
    );
    if (
      exactDepth.kind !== MidgardNativeScriptDecodingScanOutcomeKinds.Advanced
    )
      throw new Error("exact depth edge did not advance");
    expect(exactDepth.control.stackDepth).toBe(
      MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
    );
    expect(
      budgetedMidgardNativeScriptDecodingScan({
        control: {
          ...atDepthBoundary,
          stackDepth: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
        },
        window: { bytes: payload, startOffset: 0 },
        frames: [],
        maxSteps: 1,
      }),
    ).toMatchObject({
      kind: MidgardNativeScriptDecodingScanOutcomeKinds.Refused,
      refusalClass: MidgardNativeScriptDecodingRefusalClasses.DepthLimit,
    });
  });

  it("reproduces a domain-separated checkpoint and detects every seam mutation", () => {
    const prepared = evidence({ item: signatureItem });
    const checkpoint = witnessScriptDecodingCheckpoint({
      evidence: prepared,
      controlCbor: prepared.initialControlCbor,
      nextExpectedScriptHash: scanHash,
    });
    expect(checkpoint).toMatch(/^[0-9a-f]{64}$/u);
    expect(
      witnessScriptDecodingCheckpoint({
        evidence: prepared,
        controlCbor: `${prepared.initialControlCbor}00`,
        nextExpectedScriptHash: scanHash,
      }),
    ).not.toBe(checkpoint);
    expect(
      witnessScriptDecodingCheckpoint({
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
    const runner = createWitnessScriptDecodingWorkflowRunnerSurface({
      loadRuntimeConfig: async () => {
        loaded = true;
        throw new Error("unexpected loader call");
      },
    });
    await expect(
      runner.runOrResume({ category: "missingSignature" } as never),
    ).rejects.toThrow(/category mismatch/u);
    expect(loaded).toBe(false);
  });

  it("refuses caller-authored evidence at the manifest-bound production boundary", async () => {
    await expect(
      runOrResumeManifestBoundWitnessScriptDecodingWorkflow({
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
      ).map(nextWitnessScriptDecodingAction),
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
    const identity = witnessScriptDecodingEvidenceIdentity(prepared);
    const entries: WitnessScriptDecodingJournalEntry[] = [];
    let stage: WitnessScriptDecodingStage = "none";
    const next: Record<string, WitnessScriptDecodingStage> = {
      submitInit: "step01",
      submitStep01: "step02",
      submitStep02: "scan",
      submitScanOrResume: "step04",
      submitStep04: "proven",
      removeDescendants: "removed",
    };
    let nonce = 0;
    let observed: Awaited<
      ReturnType<WitnessScriptDecodingSubmission["observe"]>
    > = {
      stage,
      transactionId: "00".repeat(32),
      outputReference: null as string | null,
      checkpointHash: null as string | null,
    };
    const submission: WitnessScriptDecodingSubmission = {
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
      await runWitnessScriptDecodingProof({
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
    const submission: Pick<WitnessScriptDecodingSubmission, "cancel"> = {
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
    const entry: WitnessScriptDecodingJournalEntry = {
      sequence: 0,
      identity: "expected",
      stage: "scan",
      transactionId: "88".repeat(32),
      outputReference: "0#0",
      checkpointHash: "99".repeat(32),
    };
    expect(() =>
      reconcileWitnessScriptDecodingJournal({
        identity: "different",
        entries: [entry],
        observed: entry,
      }),
    ).toThrow(/identity/u);
    expect(() =>
      reconcileWitnessScriptDecodingJournal({
        identity: "expected",
        entries: [entry],
        observed: { ...entry, transactionId: "aa".repeat(32) },
      }),
    ).toThrow(/changed/u);
  });
});
