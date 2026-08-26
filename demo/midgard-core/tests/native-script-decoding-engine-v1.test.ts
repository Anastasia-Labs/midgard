import { describe, expect, it } from "vitest";

import {
  bindMidgardNativeScriptDecodingMachineV1,
  budgetedMidgardNativeScriptDecodingScanV1,
  buildMidgardNativeScriptDecodingTraceV1,
  encodeMidgardNativeScript,
  encodeMidgardNativeScriptStructureControlV1,
  hashMidgardNativeScriptDecodingControlV1,
  hashMidgardNativeScriptScanFrameV1,
  isExactMidgardNativeScriptStructureTerminalV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH_V1,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1,
  MidgardNativeScriptDecodingBindKindsV1,
  MidgardNativeScriptDecodingRefusalClassesV1,
  MidgardNativeScriptDecodingScanOutcomeKindsV1,
  midgardNativeScriptDecodingScanWindowForCursorV1,
  MidgardNativeScriptDecodingTraceOutcomeKindsV1,
  MidgardNativeScriptKindsV1,
  type MidgardNativeScriptScanFrameV1,
  type MidgardNativeScriptStructureControlV1,
  MidgardNativeScriptStructureStagesV1,
} from "../src/index.js";

// Fixtures mirror `thread_fixture_v1.ak`; every asserted outcome below is
// pinned by `engine.test.ak` on the Aiken side, so the two engines are held
// to the same observable behavior over the same bytes.

const signerKey = Buffer.alloc(28, 0x55);
const signatureNode = Buffer.concat([
  Buffer.from("8200581c", "hex"),
  signerKey,
]);
const signatureItem = Buffer.concat([
  Buffer.from("82005820", "hex"),
  signatureNode,
]);
const allOfTwoItem = Buffer.concat([
  Buffer.from("82005843", "hex"),
  Buffer.from("820182", "hex"),
  signatureNode,
  signatureNode,
]);
const malformedPayloadItem = Buffer.from("820043820700", "hex");
const malformedWrapperItem = Buffer.from("8201410a", "hex");
const emptyPayloadItem = Buffer.from("820040", "hex");
const plutusScriptItem = Buffer.from("82034401020304", "hex");

const boundControlOf = (
  item: Buffer,
): MidgardNativeScriptStructureControlV1 => {
  const bind = bindMidgardNativeScriptDecodingMachineV1({
    firstChunk: item,
    totalLength: item.length,
  });
  if (bind.kind !== MidgardNativeScriptDecodingBindKindsV1.Bound) {
    throw new Error(`fixture did not bind: ${bind.kind}`);
  }
  return bind.control;
};

const fullWindow = (item: Buffer) => ({ bytes: item, startOffset: 0 });

const allOfTwoFirstFrame = {
  tail: Buffer.alloc(0),
  kind: MidgardNativeScriptKindsV1.All,
  childCount: 2,
  remaining: 2,
  validCount: 0,
  required: 0n,
} satisfies MidgardNativeScriptScanFrameV1;

describe("native script decoding engine V1", () => {
  it("dispatches the machine bind on the wrapper", () => {
    const control = boundControlOf(signatureItem);
    expect(control.startOffset).toBe(4);
    expect(control.cursor).toBe(4);
    expect(control.endOffset).toBe(signatureItem.length);

    expect(
      bindMidgardNativeScriptDecodingMachineV1({
        firstChunk: malformedWrapperItem,
        totalLength: malformedWrapperItem.length,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingBindKindsV1.Malformed,
    });
    expect(
      bindMidgardNativeScriptDecodingMachineV1({
        firstChunk: emptyPayloadItem,
        totalLength: emptyPayloadItem.length,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingBindKindsV1.Malformed,
    });
    expect(
      bindMidgardNativeScriptDecodingMachineV1({
        firstChunk: plutusScriptItem,
        totalLength: plutusScriptItem.length,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingBindKindsV1.NonNative,
      languageTag: 3,
    });
    // Language 128 is in the wrapper domain but is not the native machine's.
    expect(
      bindMidgardNativeScriptDecodingMachineV1({
        firstChunk: Buffer.from("821880410a", "hex"),
        totalLength: 5,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingBindKindsV1.NonNative,
      languageTag: 128,
    });
    // A non-minimal language head (`18 00` for 0) fails `canonical_head`.
    expect(
      bindMidgardNativeScriptDecodingMachineV1({
        firstChunk: Buffer.from("821800410a", "hex"),
        totalLength: 5,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingBindKindsV1.Malformed,
    });
    // A payload head that does not end exactly at the item length.
    expect(
      bindMidgardNativeScriptDecodingMachineV1({
        firstChunk: signatureItem,
        totalLength: signatureItem.length + 1,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingBindKindsV1.Malformed,
    });
  });

  it("scans a signature script to the exact terminal", () => {
    const outcome = budgetedMidgardNativeScriptDecodingScanV1({
      control: boundControlOf(signatureItem),
      window: fullWindow(signatureItem),
      frames: [],
      maxSteps: 3,
    });
    if (
      outcome.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced
    ) {
      throw new Error("expected an advanced outcome");
    }
    expect(isExactMidgardNativeScriptStructureTerminalV1(outcome.control)).toBe(
      true,
    );
    expect(outcome.control.nodeCount).toBe(1);
    expect(outcome.framesConsumed).toBe(0);
  });

  it("scans a container through its frame witnesses", () => {
    const outcome = budgetedMidgardNativeScriptDecodingScanV1({
      control: boundControlOf(allOfTwoItem),
      window: fullWindow(allOfTwoItem),
      frames: [allOfTwoFirstFrame, { ...allOfTwoFirstFrame, remaining: 1 }],
      maxSteps: 6,
    });
    if (
      outcome.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced
    ) {
      throw new Error("expected an advanced outcome");
    }
    expect(isExactMidgardNativeScriptStructureTerminalV1(outcome.control)).toBe(
      true,
    );
    expect(outcome.control.nodeCount).toBe(3);
    expect(outcome.framesConsumed).toBe(2);
  });

  it("resumes from a budget stop to the same terminal", () => {
    const mid = budgetedMidgardNativeScriptDecodingScanV1({
      control: boundControlOf(allOfTwoItem),
      window: fullWindow(allOfTwoItem),
      frames: [],
      maxSteps: 2,
    });
    if (mid.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced) {
      throw new Error("expected an advanced outcome");
    }
    expect(mid.control.stage).toBe(MidgardNativeScriptStructureStagesV1.Frame);
    const outcome = budgetedMidgardNativeScriptDecodingScanV1({
      control: mid.control,
      window: fullWindow(allOfTwoItem),
      frames: [allOfTwoFirstFrame, { ...allOfTwoFirstFrame, remaining: 1 }],
      maxSteps: 10,
    });
    if (
      outcome.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced
    ) {
      throw new Error("expected an advanced outcome");
    }
    expect(isExactMidgardNativeScriptStructureTerminalV1(outcome.control)).toBe(
      true,
    );
    expect(outcome.control.nodeCount).toBe(3);
  });

  it("refuses a malformed payload with class 0", () => {
    expect(
      budgetedMidgardNativeScriptDecodingScanV1({
        control: boundControlOf(malformedPayloadItem),
        window: fullWindow(malformedPayloadItem),
        frames: [],
        maxSteps: 1,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Refused,
      refusalClass: MidgardNativeScriptDecodingRefusalClassesV1.Malformed,
      framesConsumed: 0,
    });
  });

  it("classifies the node limit as class 1", () => {
    // Crafted at the ceiling, as `engine.test.ak` does: no capped item can
    // reach 16,384 nodes authentically.
    const control = {
      version: 1,
      stage: MidgardNativeScriptStructureStagesV1.Token,
      startOffset: 0,
      cursor: 0,
      endOffset: signatureNode.length,
      stackRoot: Buffer.alloc(0),
      stackDepth: 0,
      nodeCount: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES_V1,
    } satisfies MidgardNativeScriptStructureControlV1;
    expect(
      budgetedMidgardNativeScriptDecodingScanV1({
        control,
        window: { bytes: signatureNode, startOffset: 0 },
        frames: [],
        maxSteps: 1,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Refused,
      refusalClass: MidgardNativeScriptDecodingRefusalClassesV1.NodeLimit,
      framesConsumed: 0,
    });
  });

  it("classifies the depth limit as class 2", () => {
    const payload = allOfTwoItem.subarray(4);
    const control = {
      version: 1,
      stage: MidgardNativeScriptStructureStagesV1.Token,
      startOffset: 0,
      cursor: 0,
      endOffset: payload.length,
      stackRoot: hashMidgardNativeScriptScanFrameV1(allOfTwoFirstFrame),
      stackDepth: MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH_V1,
      nodeCount: 1,
    } satisfies MidgardNativeScriptStructureControlV1;
    expect(
      budgetedMidgardNativeScriptDecodingScanV1({
        control,
        window: { bytes: Buffer.from(payload), startOffset: 0 },
        frames: [],
        maxSteps: 1,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Refused,
      refusalClass: MidgardNativeScriptDecodingRefusalClassesV1.DepthLimit,
      framesConsumed: 0,
    });
  });

  it("stops on a truncated window instead of refusing", () => {
    const control = boundControlOf(signatureItem);
    expect(
      budgetedMidgardNativeScriptDecodingScanV1({
        control,
        window: { bytes: signatureItem.subarray(0, 14), startOffset: 0 },
        frames: [],
        maxSteps: 5,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced,
      control,
      framesConsumed: 0,
    });
  });

  it("stops without a window", () => {
    const control = boundControlOf(signatureItem);
    expect(
      budgetedMidgardNativeScriptDecodingScanV1({
        control,
        window: null,
        frames: [],
        maxSteps: 5,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced,
      control,
      framesConsumed: 0,
    });
  });

  it("aborts on a frame witness that does not hash-chain", () => {
    const mid = budgetedMidgardNativeScriptDecodingScanV1({
      control: boundControlOf(allOfTwoItem),
      window: fullWindow(allOfTwoItem),
      frames: [],
      maxSteps: 2,
    });
    if (mid.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced) {
      throw new Error("expected an advanced outcome");
    }
    expect(() =>
      budgetedMidgardNativeScriptDecodingScanV1({
        control: mid.control,
        window: fullWindow(allOfTwoItem),
        frames: [{ ...allOfTwoFirstFrame, childCount: 3, remaining: 3 }],
        maxSteps: 10,
      }),
    ).toThrow(/hash-chain/u);
  });

  it("commits the machine control under the Aiken domain vector", () => {
    const controlCbor = encodeMidgardNativeScriptStructureControlV1(
      boundControlOf(signatureItem),
    );
    expect(controlCbor.toString("hex")).toBe("88010004041824400000");
    // Cross-engine golden: `engine.hash_machine_control_v1` over the same
    // control bytes, extracted from the pinned Aiken engine.
    expect(
      hashMidgardNativeScriptDecodingControlV1(controlCbor).toString("hex"),
    ).toBe("3c5b5c4d4bef20c0edad15ac4d77ab313982b5f0e8955b54372ebfe8fd085356");
  });

  it("traces a wrongful-acceptance item to its refusal", () => {
    const trace = buildMidgardNativeScriptDecodingTraceV1(malformedPayloadItem);
    expect(trace.bind.kind).toBe(MidgardNativeScriptDecodingBindKindsV1.Bound);
    expect(trace.steps).toHaveLength(0);
    if (
      trace.outcome?.kind !==
      MidgardNativeScriptDecodingTraceOutcomeKindsV1.Refused
    ) {
      throw new Error("expected a refused outcome");
    }
    expect(trace.outcome.refusalClass).toBe(
      MidgardNativeScriptDecodingRefusalClassesV1.Malformed,
    );
    // The Verdict-step property: a single-step fold from the traced control
    // exhibits exactly the traced refusal.
    expect(
      budgetedMidgardNativeScriptDecodingScanV1({
        control: trace.outcome.control,
        window: fullWindow(malformedPayloadItem),
        frames: [],
        maxSteps: 1,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptDecodingScanOutcomeKindsV1.Refused,
      refusalClass: MidgardNativeScriptDecodingRefusalClassesV1.Malformed,
      framesConsumed: 0,
    });
  });

  it("traces a canonical item to the exact terminal with its frames", () => {
    const trace = buildMidgardNativeScriptDecodingTraceV1(allOfTwoItem);
    if (
      trace.outcome?.kind !==
      MidgardNativeScriptDecodingTraceOutcomeKindsV1.Terminal
    ) {
      throw new Error("expected a terminal outcome");
    }
    expect(trace.steps).toHaveLength(6);
    expect(trace.outcome.control.nodeCount).toBe(3);
    const frames = trace.steps.flatMap((step) =>
      step.frame === null ? [] : [step.frame],
    );
    expect(frames).toStrictEqual([
      allOfTwoFirstFrame,
      { ...allOfTwoFirstFrame, remaining: 1 },
    ]);
    // The traced frames drive the budgeted fold to the same terminal.
    const outcome = budgetedMidgardNativeScriptDecodingScanV1({
      control: boundControlOf(allOfTwoItem),
      window: fullWindow(allOfTwoItem),
      frames,
      maxSteps: trace.steps.length,
    });
    if (
      outcome.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced
    ) {
      throw new Error("expected an advanced outcome");
    }
    expect(outcome.control).toStrictEqual(trace.outcome.control);
  });

  it("returns non-bound traces without a scan outcome", () => {
    expect(
      buildMidgardNativeScriptDecodingTraceV1(malformedWrapperItem),
    ).toStrictEqual({
      bind: { kind: MidgardNativeScriptDecodingBindKindsV1.Malformed },
      steps: [],
      outcome: null,
    });
    const plutusTrace =
      buildMidgardNativeScriptDecodingTraceV1(plutusScriptItem);
    expect(plutusTrace.bind).toStrictEqual({
      kind: MidgardNativeScriptDecodingBindKindsV1.NonNative,
      languageTag: 3,
    });
    expect(plutusTrace.outcome).toBeNull();
  });

  it("folds a multi-chunk item in windowed lockstep to the traced terminal", () => {
    // A wide canonical script large enough for three chunks, wrapped as a
    // versioned tag-0 item.
    const payload = encodeMidgardNativeScript({
      type: "all",
      scripts: Array.from({ length: 260 }, () => ({
        type: "sig",
        keyHash: signerKey,
      })),
    });
    expect(payload.length).toBeGreaterThan(
      2 * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
    );
    const payloadHead = Buffer.from([
      0x59,
      (payload.length >> 8) & 0xff,
      payload.length & 0xff,
    ]);
    const item = Buffer.concat([
      Buffer.from("8200", "hex"),
      payloadHead,
      payload,
    ]);

    const trace = buildMidgardNativeScriptDecodingTraceV1(item);
    if (
      trace.outcome?.kind !==
      MidgardNativeScriptDecodingTraceOutcomeKindsV1.Terminal
    ) {
      throw new Error("expected a terminal outcome");
    }
    expect(trace.outcome.control.nodeCount).toBe(261);

    const frames = trace.steps.flatMap((step) =>
      step.frame === null ? [] : [step.frame],
    );
    let control = boundControlOf(item);
    let frameIndex = 0;
    let folds = 0;
    while (!isExactMidgardNativeScriptStructureTerminalV1(control)) {
      folds += 1;
      expect(folds).toBeLessThan(200);
      const window =
        control.cursor < item.length
          ? midgardNativeScriptDecodingScanWindowForCursorV1({
              itemBytes: item,
              cursor: control.cursor,
            })
          : null;
      const outcome = budgetedMidgardNativeScriptDecodingScanV1({
        control,
        window,
        frames: frames.slice(frameIndex),
        maxSteps: 16,
      });
      if (
        outcome.kind !== MidgardNativeScriptDecodingScanOutcomeKindsV1.Advanced
      ) {
        throw new Error("expected an advanced outcome");
      }
      expect(outcome.control).not.toStrictEqual(control);
      control = outcome.control;
      frameIndex += outcome.framesConsumed;
    }
    expect(control).toStrictEqual(trace.outcome.control);
    expect(frameIndex).toBe(frames.length);
  });

  it("derives the authenticated window geometry for a cursor", () => {
    const item = Buffer.alloc(3 * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1 + 100);
    const first = midgardNativeScriptDecodingScanWindowForCursorV1({
      itemBytes: item,
      cursor: 0,
    });
    expect(first.startOffset).toBe(0);
    expect(first.bytes.length).toBe(2 * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
    const second = midgardNativeScriptDecodingScanWindowForCursorV1({
      itemBytes: item,
      cursor: MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
    });
    expect(second.startOffset).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
    expect(second.bytes.length).toBe(2 * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
    const last = midgardNativeScriptDecodingScanWindowForCursorV1({
      itemBytes: item,
      cursor: 3 * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1 + 1,
    });
    expect(last.startOffset).toBe(3 * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
    expect(last.bytes.length).toBe(100);
    expect(() =>
      midgardNativeScriptDecodingScanWindowForCursorV1({
        itemBytes: item,
        cursor: item.length,
      }),
    ).toThrow(/outside/u);
  });
});
