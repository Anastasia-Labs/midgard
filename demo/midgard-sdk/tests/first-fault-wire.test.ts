import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  NetworkOrderContext,
  PurposeCoordinate,
  purposeCoordinateOfReason,
} from "../src/fraud-proof/first-fault.js";
import type { RejectionReason } from "../src/rejection-reason.js";

/** The same encodings are pinned on-chain in first-fault-v1.test.ak. */
describe("first-fault wire twins", () => {
  it("encodes a purpose coordinate and the network-order context as on-chain", () => {
    expect(Data.to({ kind: 3n, index: 7n }, PurposeCoordinate)).toBe(
      "d8799f0307ff",
    );
    expect(Data.to({ body_network_mismatch: true }, NetworkOrderContext)).toBe(
      "d8799fd87a80ff",
    );
  });

  it("projects only source and redeemer absence onto their purpose coordinate", () => {
    const payload = { purpose_kind: 2n, purpose_index: 5n };
    const coordinateBearing: readonly [RejectionReason, boolean][] = [
      [{ ScriptSourceMissing: payload }, true],
      [{ RedeemerMissing: payload }, true],
      [{ ExecutionNativeScriptMalformed: payload }, false],
      [{ ExecutionNativeScriptNodeLimit: payload }, false],
      [{ ExecutionNativeScriptDepthLimit: payload }, false],
      [{ ExecutionNativeScriptFalse: payload }, false],
      [{ ReceivePurposePlutusV3Forbidden: payload }, false],
      [{ PlutusExecutionFailed: payload }, false],
    ];
    for (const [reason, projected] of coordinateBearing)
      expect(purposeCoordinateOfReason(reason)).toStrictEqual(
        projected ? { kind: 2n, index: 5n } : null,
      );
    expect(purposeCoordinateOfReason("ScriptIntegrityHashMismatch")).toBe(null);
  });

  it("refuses a reason outside the rejection arm table", () => {
    expect(() =>
      purposeCoordinateOfReason({
        NotAnArm: { purpose_kind: 0n, purpose_index: 0n },
      } as unknown as RejectionReason),
    ).toThrow("unknown rejection arm");
  });
});
