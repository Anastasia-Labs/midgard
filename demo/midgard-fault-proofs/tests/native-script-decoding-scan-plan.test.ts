/**
 * `native-script-decoding` segment-planner unit suite (offchain plan
 * §8.2(3)). Pure unit tests — no emulator: the planner's contract is that
 * every plan it emits replays exactly on the engine twin's
 * `budgeted_scan_v1`, so the suite replays published plans from the outside
 * with nothing but the plan's own window/frames/budget, and cross-checks
 * the planner's pinned execution constants against the exec ledger JSON so
 * a ledger re-pin cannot silently split the planner from the measurement.
 */
import { readFileSync } from "node:fs";

import {
  budgetedMidgardNativeScriptDecodingScan,
  buildMidgardNativeScriptDecodingTrace,
  encodeMidgardNativeScript,
  encodeMidgardNativeScriptStructureControl,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardNativeScript,
  MidgardNativeScriptDecodingDirections,
  MidgardNativeScriptDecodingRefusalClasses,
  MidgardNativeScriptDecodingScanOutcomeKinds,
  midgardNativeScriptDecodingScanWindowForCursor,
  MidgardNativeScriptStructureStages,
} from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  buildNativeScriptDecodingScanPlan,
  NATIVE_SCRIPT_DECODING_DEFAULT_MAX_STEPS_PER_TX,
  NATIVE_SCRIPT_DECODING_EXEC_PINS,
  NativeScriptDecodingPlanRoutes,
  type NativeScriptDecodingScanPlan,
} from "../src/native-script-decoding/scan-plan.js";

const DIRECTION_A = MidgardNativeScriptDecodingDirections.WrongfulAcceptance;
const DIRECTION_B = MidgardNativeScriptDecodingDirections.WrongfulRejection;

const signerKey = Buffer.alloc(28, 0x55);
const signatureNodeHex = `8200581c${signerKey.toString("hex")}`;

/** Wrap a payload as the versioned tag-0 item (`[0, payload-bytes]`). */
const itemFromPayload = (payload: Buffer): Buffer => {
  const head =
    payload.length <= 23
      ? Buffer.from([0x40 + payload.length])
      : payload.length < 256
        ? Buffer.from([0x58, payload.length])
        : Buffer.from([
            0x59,
            (payload.length >> 8) & 0xff,
            payload.length & 0xff,
          ]);
  return Buffer.concat([Buffer.from("8200", "hex"), head, payload]);
};

/** `all(all(...all(sig)))` with `depth` container nodes. */
const deepChainScript = (depth: number): MidgardNativeScript => {
  let script: MidgardNativeScript = { type: "sig", keyHash: signerKey };
  for (let level = 0; level < depth; level += 1) {
    script = { type: "all", scripts: [script] };
  }
  return script;
};

const deepChainItem = (depth: number): Buffer =>
  itemFromPayload(encodeMidgardNativeScript(deepChainScript(depth)));

/**
 * Replay every plan entry from the outside, using only what the plan
 * publishes (window chunk index, frames, budget) plus the item bytes — the
 * same inputs a Scan transaction carries.
 */
const replayPlanAgainstItem = (
  plan: NativeScriptDecodingScanPlan,
  itemBytes: Buffer,
): void => {
  for (const segment of plan.segments) {
    const outcome = budgetedMidgardNativeScriptDecodingScan({
      control: segment.controlBefore.control,
      window:
        segment.window === null
          ? null
          : midgardNativeScriptDecodingScanWindowForCursor({
              itemBytes,
              cursor:
                segment.window.chunkIndex * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
            }),
      frames: segment.frames,
      maxSteps: segment.stepBudget,
    });
    if (outcome.kind !== MidgardNativeScriptDecodingScanOutcomeKinds.Advanced) {
      throw new Error("segment replay refused");
    }
    expect(
      Buffer.from(
        encodeMidgardNativeScriptStructureControl(outcome.control),
      ).toString("hex"),
    ).toStrictEqual(segment.controlAfter.cborHex);
    expect(outcome.framesConsumed).toStrictEqual(segment.frames.length);
  }
};

const chainIsContinuous = (plan: NativeScriptDecodingScanPlan): void => {
  for (let index = 1; index < plan.segments.length; index += 1) {
    expect(plan.segments[index]!.controlBefore.cborHex).toStrictEqual(
      plan.segments[index - 1]!.controlAfter.cborHex,
    );
  }
  if (plan.segments.length > 0 && plan.verdict.control !== null) {
    expect(plan.verdict.control.cborHex).toStrictEqual(
      plan.segments.at(-1)!.controlAfter.cborHex,
    );
  }
};

describe("native-script-decoding scan planner (offchain plan §5.2/§5.3)", () => {
  it("pins its execution constants to the exec ledger JSON", () => {
    type LedgerRow = { readonly mem: number; readonly cpu: number };
    type LedgerFile = {
      readonly basis: {
        readonly memoryUnits: number;
        readonly cpuUnits: number;
      };
      readonly modules: readonly {
        readonly module: string;
        readonly rows: Readonly<Record<string, LedgerRow>>;
      }[];
    };
    const ledger = JSON.parse(
      readFileSync(
        new URL(
          "../../../onchain/aiken/scripts/native-script-decoding-engine-exec-ledger-v1.json",
          import.meta.url,
        ),
        "utf8",
      ),
    ) as LedgerFile;
    const rowsOf = (moduleSuffix: string): Record<string, LedgerRow> => {
      const module = ledger.modules.find((entry) =>
        entry.module.endsWith(moduleSuffix),
      );
      if (module === undefined) {
        throw new Error(`ledger module ${moduleSuffix} is missing`);
      }
      return { ...module.rows };
    };
    const pins = NATIVE_SCRIPT_DECODING_EXEC_PINS;
    expect(ledger.basis.memoryUnits).toStrictEqual(pins.basisMemoryUnits);
    expect(ledger.basis.cpuUnits).toStrictEqual(pins.basisCpuUnits);

    const fold = rowsOf("exec_measure_v1.test");
    const deep9 = fold["decoding_measure_deep_9_nodes"]!;
    const deep17 = fold["decoding_measure_deep_17_nodes"]!;
    expect(deep17.mem - deep9.mem).toStrictEqual(pins.deepMemSlopeNumerator);
    expect(deep17.cpu - deep9.cpu).toStrictEqual(pins.deepCpuSlopeNumerator);
    expect(pins.slopeDenominator).toStrictEqual(17 - 9);

    const bindDescriptor = rowsOf(
      "native_script_decoding/step_03_bind_descriptor",
    );
    const advanceOrClose = rowsOf(
      "native_script_decoding/step_03_advance_or_close",
    );
    expect(
      advanceOrClose[
        "advance_or_close_closes_direction_b_at_the_exact_terminal"
      ]!.cpu,
    ).toStrictEqual(pins.scanStepEnvelopeCpuUnits);
    expect(
      advanceOrClose["advance_or_close_closes_a_direction_a_refusal"],
    ).toStrictEqual({
      mem: pins.verdictWrongfulAcceptance.mem,
      cpu: pins.verdictWrongfulAcceptance.cpu,
      basisFit: "within",
    });
    expect(
      advanceOrClose[
        "advance_or_close_closes_direction_b_at_the_exact_terminal"
      ],
    ).toStrictEqual({
      mem: pins.verdictWrongfulRejection.mem,
      cpu: pins.verdictWrongfulRejection.cpu,
      basisFit: "within",
    });
    expect(
      bindDescriptor[
        "bind_descriptor_closes_a_non_native_direction_b_descriptor"
      ],
    ).toStrictEqual({
      mem: pins.descriptorContradictionClose.mem,
      cpu: pins.descriptorContradictionClose.cpu,
      basisFit: "within",
    });
  });

  it("derives the default 16-step budget and refuses over-basis overrides", () => {
    expect(NATIVE_SCRIPT_DECODING_DEFAULT_MAX_STEPS_PER_TX).toStrictEqual(16);
    // A deep chain with 26 primitive steps: 12 container tokens + the leaf
    // token + 12 frame pops + finalize.
    const item = deepChainItem(12);
    expect(() =>
      buildNativeScriptDecodingScanPlan({
        itemBytes: item,
        direction: DIRECTION_B,
        maxStepsPerTx: 17,
      }),
    ).toThrow(/predicted over the execution basis/);
    expect(() =>
      buildNativeScriptDecodingScanPlan({
        itemBytes: item,
        direction: DIRECTION_B,
        maxStepsPerTx: 0,
      }),
    ).toThrow(/positive integer/);
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: DIRECTION_B,
    });
    expect(plan.maxStepsPerTx).toStrictEqual(16);
    expect(plan.segments.map((segment) => segment.stepBudget)).toStrictEqual([
      16, 10,
    ]);
    for (const segment of plan.segments) {
      expect(segment.predictedMemoryUnits).toBeLessThanOrEqual(
        NATIVE_SCRIPT_DECODING_EXEC_PINS.basisMemoryUnits,
      );
      expect(segment.predictedCpuUnits).toBeLessThanOrEqual(
        NATIVE_SCRIPT_DECODING_EXEC_PINS.basisCpuUnits,
      );
    }
  });

  it("plans direction B to the exact terminal with a windowless verdict", () => {
    const item = deepChainItem(3);
    const trace = buildMidgardNativeScriptDecodingTrace(item);
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: DIRECTION_B,
    });
    expect(plan.route).toStrictEqual(NativeScriptDecodingPlanRoutes.Machine);
    expect(plan.chunkCount).toStrictEqual(1);
    expect(plan.segments).toHaveLength(1);
    const segment = plan.segments[0]!;
    // 4 tokens + 3 frame pops + finalize.
    expect(segment.stepBudget).toStrictEqual(8);
    expect(segment.frames).toHaveLength(3);
    expect(segment.window).toStrictEqual({ chunkIndex: 0, needNext: false });
    if (trace.bind.kind !== "bound") {
      throw new Error("fixture failed to bind");
    }
    expect(segment.controlBefore.cborHex).toStrictEqual(
      Buffer.from(
        encodeMidgardNativeScriptStructureControl(trace.bind.control),
      ).toString("hex"),
    );
    expect(plan.verdict.window).toBeNull();
    expect(plan.verdict.refusalClass).toBeNull();
    expect(plan.verdict.control).not.toBeNull();
    expect(plan.verdict.control!.control.stage).toStrictEqual(
      MidgardNativeScriptStructureStages.Terminal,
    );
    expect(plan.verdict.control!.hashHex).toMatch(/^[0-9a-f]{64}$/);
    chainIsContinuous(plan);
    replayPlanAgainstItem(plan, item);
  });

  it("plans direction A to stop one step short and pin the refusal", () => {
    // `all` of a valid signature node and an undecodable child: the machine
    // advances three steps (container token, leaf token, frame) and refuses
    // the fourth token.
    const midScanItem = itemFromPayload(
      Buffer.from(`820182${signatureNodeHex}820700`, "hex"),
    );
    const midScanPlan = buildNativeScriptDecodingScanPlan({
      itemBytes: midScanItem,
      direction: DIRECTION_A,
    });
    expect(midScanPlan.route).toStrictEqual(
      NativeScriptDecodingPlanRoutes.Machine,
    );
    expect(midScanPlan.segments).toHaveLength(1);
    expect(midScanPlan.segments[0]!.stepBudget).toStrictEqual(3);
    expect(midScanPlan.segments[0]!.frames).toHaveLength(1);
    expect(midScanPlan.verdict.refusalClass).toStrictEqual(
      MidgardNativeScriptDecodingRefusalClasses.Malformed,
    );
    // The refusing control is token-stage, so the verdict carries a window.
    expect(midScanPlan.verdict.window).toStrictEqual({
      chunkIndex: 0,
      needNext: false,
    });
    expect(midScanPlan.verdict.control!.control.stage).toStrictEqual(
      MidgardNativeScriptStructureStages.Token,
    );
    chainIsContinuous(midScanPlan);
    replayPlanAgainstItem(midScanPlan, midScanItem);

    // A payload that refuses on its very first token: no scan segments at
    // all — the verdict transaction exhibits the refusal from the bind
    // control directly.
    const immediateItem = Buffer.from("820043820700", "hex");
    const immediatePlan = buildNativeScriptDecodingScanPlan({
      itemBytes: immediateItem,
      direction: DIRECTION_A,
    });
    expect(immediatePlan.segments).toHaveLength(0);
    expect(immediatePlan.verdict.refusalClass).toStrictEqual(
      MidgardNativeScriptDecodingRefusalClasses.Malformed,
    );
    const immediateTrace = buildMidgardNativeScriptDecodingTrace(immediateItem);
    if (immediateTrace.bind.kind !== "bound") {
      throw new Error("fixture failed to bind");
    }
    expect(immediatePlan.verdict.control!.cborHex).toStrictEqual(
      Buffer.from(
        encodeMidgardNativeScriptStructureControl(immediateTrace.bind.control),
      ).toString("hex"),
    );
  });

  it("never lets a segment span a window change on a multi-chunk item", () => {
    const payload = encodeMidgardNativeScript({
      type: "all",
      scripts: Array.from({ length: 260 }, () => ({
        type: "sig",
        keyHash: signerKey,
      })),
    });
    const item = itemFromPayload(payload);
    expect(item.length).toBeGreaterThan(2 * MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: DIRECTION_B,
    });
    expect(plan.chunkCount).toStrictEqual(3);
    // 261 tokens + 260 frame steps + finalize.
    const totalSteps = plan.segments.reduce(
      (sum, segment) => sum + segment.stepBudget,
      0,
    );
    expect(totalSteps).toStrictEqual(522);
    const totalFrames = plan.segments.reduce(
      (sum, segment) => sum + segment.frames.length,
      0,
    );
    expect(totalFrames).toStrictEqual(260);
    const windowChunks = new Set<number>();
    for (const segment of plan.segments) {
      expect(segment.stepBudget).toBeGreaterThanOrEqual(1);
      expect(segment.stepBudget).toBeLessThanOrEqual(plan.maxStepsPerTx);
      if (segment.window !== null) {
        windowChunks.add(segment.window.chunkIndex);
        expect(segment.window.needNext).toStrictEqual(
          segment.window.chunkIndex + 1 < plan.chunkCount,
        );
      }
    }
    expect([...windowChunks].sort()).toStrictEqual([0, 1, 2]);
    expect(plan.verdict.window).toBeNull();
    chainIsContinuous(plan);
    replayPlanAgainstItem(plan, item);

    // Under the default budget this fixture's chunk crossings happen to ride
    // the budget cuts exactly (16 steps = 8 signature nodes = 256 bytes of
    // cadence, phase-aligned with the 4,095-byte chunk edge), so force the
    // misalignment with a 13-step policy: at least one cut must then be
    // caused by the window change alone — a short segment (under budget)
    // whose successor reads a different chunk.
    const misaligned = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: DIRECTION_B,
      maxStepsPerTx: 13,
    });
    const windowForcedCut = misaligned.segments.some(
      (segment, index) =>
        index + 1 < misaligned.segments.length &&
        segment.stepBudget < misaligned.maxStepsPerTx &&
        segment.window !== null &&
        misaligned.segments[index + 1]!.window !== null &&
        misaligned.segments[index + 1]!.window!.chunkIndex !==
          segment.window.chunkIndex,
    );
    expect(windowForcedCut).toStrictEqual(true);
    chainIsContinuous(misaligned);
    replayPlanAgainstItem(misaligned, item);
  });

  it("refuses direction mismatches and routes bind short circuits", () => {
    // Undecodable wrapper (language tag outside {0, 3, 128}).
    const malformedWrapper = Buffer.from("8201410a", "hex");
    const malformedPlan = buildNativeScriptDecodingScanPlan({
      itemBytes: malformedWrapper,
      direction: DIRECTION_A,
    });
    expect(malformedPlan.route).toStrictEqual(
      NativeScriptDecodingPlanRoutes.BindMalformed,
    );
    expect(malformedPlan.segments).toHaveLength(0);
    expect(malformedPlan.verdict.control).toBeNull();
    expect(malformedPlan.verdict.window).toStrictEqual({
      chunkIndex: 0,
      needNext: false,
    });
    expect(() =>
      buildNativeScriptDecodingScanPlan({
        itemBytes: malformedWrapper,
        direction: DIRECTION_B,
      }),
    ).toThrow(/malformed at bind/);

    // Empty tag-0 payload is the same bind-level malformation.
    expect(
      buildNativeScriptDecodingScanPlan({
        itemBytes: Buffer.from("820040", "hex"),
        direction: DIRECTION_A,
      }).route,
    ).toStrictEqual(NativeScriptDecodingPlanRoutes.BindMalformed);

    // Plutus language tag: only the direction-B contradiction close.
    const plutusItem = Buffer.from("82034401020304", "hex");
    const contradictionPlan = buildNativeScriptDecodingScanPlan({
      itemBytes: plutusItem,
      direction: DIRECTION_B,
    });
    expect(contradictionPlan.route).toStrictEqual(
      NativeScriptDecodingPlanRoutes.DescriptorContradiction,
    );
    expect(contradictionPlan.languageTag).toStrictEqual(3);
    expect(contradictionPlan.segments).toHaveLength(0);
    expect(() =>
      buildNativeScriptDecodingScanPlan({
        itemBytes: plutusItem,
        direction: DIRECTION_A,
      }),
    ).toThrow(/non-native language tag/);

    // Machine-route polarity: a canonical item has no direction-A fault, a
    // refusing item has no direction-B fault.
    const canonicalItem = itemFromPayload(Buffer.from(signatureNodeHex, "hex"));
    expect(() =>
      buildNativeScriptDecodingScanPlan({
        itemBytes: canonicalItem,
        direction: DIRECTION_A,
      }),
    ).toThrow(/no wrongful acceptance/);
    expect(() =>
      buildNativeScriptDecodingScanPlan({
        itemBytes: itemFromPayload(
          Buffer.from(`820182${signatureNodeHex}820700`, "hex"),
        ),
        direction: DIRECTION_B,
      }),
    ).toThrow(/no wrongful rejection/);
  });

  it("honours a narrower policy budget", () => {
    const item = deepChainItem(3);
    const plan = buildNativeScriptDecodingScanPlan({
      itemBytes: item,
      direction: DIRECTION_B,
      maxStepsPerTx: 4,
    });
    expect(plan.maxStepsPerTx).toStrictEqual(4);
    expect(plan.segments.map((segment) => segment.stepBudget)).toStrictEqual([
      4, 4,
    ]);
    chainIsContinuous(plan);
    replayPlanAgainstItem(plan, item);
  });
});
