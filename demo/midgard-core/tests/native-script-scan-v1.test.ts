import { describe, expect, it } from "vitest";

import {
  advanceMidgardNativeScriptStructureTokenV1,
  buildMidgardNativeScriptStructureTraceV1,
  decodeMidgardNativeScriptStructureControlV1,
  encodeMidgardNativeScript,
  encodeMidgardNativeScriptStructureControlV1,
  initialMidgardNativeScriptStructureControlV1,
  isExactMidgardNativeScriptStructureTerminalV1,
  type MidgardNativeScript,
  MidgardNativeScriptStructureResultKindsV1,
  MidgardNativeScriptStructureStagesV1,
} from "../src/index.js";

const signature = (fill: number): MidgardNativeScript => ({
  type: "sig",
  keyHash: Buffer.alloc(28, fill),
});

describe("native script syntax scan V1", () => {
  it("iteratively scans canonical trees beyond the retired count caps", () => {
    const script: MidgardNativeScript = {
      type: "all",
      scripts: Array.from({ length: 40 }, (_, index) => signature(index)),
    };
    const bytes = encodeMidgardNativeScript(script);
    const trace = buildMidgardNativeScriptStructureTraceV1(bytes);
    const terminal = trace.at(-1)!.next;

    expect(terminal.nodeCount).toBe(41);
    expect(terminal.stackDepth).toBe(0);
    expect(isExactMidgardNativeScriptStructureTerminalV1(terminal)).toBe(true);
  });

  it("iteratively scans canonical trees beyond the retired depth cap", () => {
    let script = signature(0x44);
    for (let depth = 0; depth < 20; depth += 1) {
      script = { type: "all", scripts: [script] };
    }
    const trace = buildMidgardNativeScriptStructureTraceV1(
      encodeMidgardNativeScript(script),
    );
    const terminal = trace.at(-1)!.next;

    expect(terminal.nodeCount).toBe(21);
    expect(Math.max(...trace.map(({ control }) => control.stackDepth))).toBe(
      20,
    );
    expect(isExactMidgardNativeScriptStructureTerminalV1(terminal)).toBe(true);
  });

  it("round-trips canonical controls and emits the Aiken vector", () => {
    const bytes = encodeMidgardNativeScript({
      type: "atLeast",
      required: 1n,
      scripts: [signature(0x44), { type: "before", slot: 42n }],
    });
    const terminal =
      buildMidgardNativeScriptStructureTraceV1(bytes).at(-1)!.next;
    const controlCbor = encodeMidgardNativeScriptStructureControlV1(terminal);

    expect(
      decodeMidgardNativeScriptStructureControlV1(controlCbor),
    ).toStrictEqual(terminal);
    expect(controlCbor.toString("hex")).toBe("8801030018281828400003");
  });

  it("reports authenticated malformed syntax and trailing bytes", () => {
    const malformed = Buffer.from([0x82, 0x00, 0x41, 0x44]);
    const initial = initialMidgardNativeScriptStructureControlV1({
      startOffset: 0,
      totalLength: malformed.length,
    });
    expect(
      advanceMidgardNativeScriptStructureTokenV1({
        control: initial,
        window: malformed,
        windowOffset: 0,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptStructureResultKindsV1.Invalid,
    });

    const validWithTrailing = Buffer.concat([
      encodeMidgardNativeScript(signature(0x44)),
      Buffer.from([0]),
    ]);
    expect(() =>
      buildMidgardNativeScriptStructureTraceV1(validWithTrailing),
    ).toThrow(/invalid/u);
  });

  it("fails closed for the wrong phase, span, and window", () => {
    const bytes = encodeMidgardNativeScript(signature(0x44));
    const initial = initialMidgardNativeScriptStructureControlV1({
      startOffset: 0,
      totalLength: bytes.length,
    });
    expect(
      advanceMidgardNativeScriptStructureTokenV1({
        control: {
          ...initial,
          stage: MidgardNativeScriptStructureStagesV1.Frame,
        },
        window: bytes,
        windowOffset: 0,
      }),
    ).toBeNull();
    expect(
      advanceMidgardNativeScriptStructureTokenV1({
        control: initial,
        window: bytes.subarray(0, bytes.length - 1),
        windowOffset: 0,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptStructureResultKindsV1.Invalid,
    });
    expect(
      advanceMidgardNativeScriptStructureTokenV1({
        control: { ...initial, endOffset: initial.endOffset - 1 },
        window: bytes,
        windowOffset: 0,
      }),
    ).toStrictEqual({
      kind: MidgardNativeScriptStructureResultKindsV1.Invalid,
    });
  });
});
