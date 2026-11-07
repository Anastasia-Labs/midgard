import {
  buildMidgardLedgerOutputScanTrace,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { outputReferenceScriptControlData } from "../src/output-reference-script-decoding/output-reference-script-decoding.js";
import { findOutputReferenceScriptControlIndex } from "../src/output-reference-script-decoding/submit-step-03.js";

const controls = () => {
  const output = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 1)]),
    value: { lovelace: 2_000_000n, assets: new Map() },
  });
  const trace = buildMidgardLedgerOutputScanTrace(output);
  return [trace.initial, ...trace.steps.map((step) => step.next)];
};

describe("output scan checkpoint lookup", () => {
  it("encodes only matching coordinates, retaining exact checkpoint equality", () => {
    const trace = controls();
    const target = trace.at(-1)!;
    const state = outputReferenceScriptControlData(target);
    const encode = vi.spyOn(Data, "to");
    try {
      expect(findOutputReferenceScriptControlIndex(trace, state)).toBe(
        trace.length - 1,
      );
      expect(encode).toHaveBeenCalledTimes(2);
      // Matching coordinates alone are never sufficient admission.
      expect(
        findOutputReferenceScriptControlIndex(trace, {
          ...state,
          lovelace: state.lovelace + 1n,
        }),
      ).toBe(-1);
      expect(
        findOutputReferenceScriptControlIndex(trace, {
          ...state,
          cursor: state.cursor + 1n,
        }),
      ).toBe(-1);
      // The same evidence objects may contain mutable buffers: no stale encoding cache.
      target.address.fill(2);
      expect(findOutputReferenceScriptControlIndex(trace, state)).toBe(-1);
      expect(
        findOutputReferenceScriptControlIndex(
          trace,
          outputReferenceScriptControlData(target),
        ),
      ).toBe(trace.length - 1);
    } finally {
      encode.mockRestore();
    }
  });
});
