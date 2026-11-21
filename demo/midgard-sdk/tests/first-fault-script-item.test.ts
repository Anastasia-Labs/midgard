import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  type FaultPosition,
  leafAdmitsFault,
  rejectionReasonPhase,
} from "../src/fraud-proof/first-fault.js";
import { forcedVerdictSubject } from "../src/fraud-proof/proof-thread-substrate.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const scriptItem = (kind: bigint): RejectionReason => ({
  PlutusScriptUndecodable: { item_kind: kind, item_index: 0n, class: 1n },
});
const context = { body_network_mismatch: false };
const canonical: FaultPosition = {
  phase: "canonical",
  position: { kind: "item", fieldIndex: 4n, itemIndex: 0n },
};
const resolution: FaultPosition = {
  phase: "resolve",
  position: {
    sourceKind: 0n,
    inputIndex: 0n,
    subcheck: 0,
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x11),
      outputIndex: 1,
    }),
  },
};
const purpose: FaultPosition = {
  phase: "purpose",
  position: { coordinate: { kind: 0n, index: 0n }, subcheck: "SourceRequired" },
};
const admits = (kind: bigint, observed: FaultPosition) =>
  leafAdmitsFault(
    forcedVerdictSubject({
      transactionId: "11".repeat(32),
      sourceKey: { transactionId: "77".repeat(32), outputIndex: 0n },
      rejectionReason: scriptItem(kind),
    }),
    observed.phase === "canonical" ? null : context,
    observed,
    "NotRequired",
  );

// A witness script item (kind 0) is decoded in phase 5 and an output reference
// script item (kind 1) in phase 8. Callers negate the result, so false convicts.
describe("first-fault phase of an undecodable Plutus script item", () => {
  it("places a witness script item after canonical faults and before resolution", () => {
    expect(admits(0n, canonical)).toBe(false);
    expect(admits(0n, resolution)).toBe(true);
  });
  it("places an output reference script item after resolution and with direct purposes", () => {
    expect(admits(1n, resolution)).toBe(false);
    expect(admits(1n, purpose)).toBe(true);
  });
  it.each([2n, -1n])("gives item kind %s no phase", (kind) => {
    expect(() => rejectionReasonPhase(scriptItem(kind), context)).toThrow(
      /unknown script item kind/,
    );
    expect(() => admits(kind, resolution)).toThrow(/unknown script item kind/);
  });
});
