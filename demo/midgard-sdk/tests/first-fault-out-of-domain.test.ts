import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  type ClaimedFaultOrder,
  leafAdmitsFault,
  type ResolutionFaultPosition,
} from "../src/fraud-proof/first-fault.js";
import { forcedVerdictSubject } from "../src/fraud-proof/proof-thread-substrate.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const subject = (rejectionReason: RejectionReason) =>
  forcedVerdictSubject({
    transactionId: "11".repeat(32),
    sourceKey: { transactionId: "77".repeat(32), outputIndex: 0n },
    rejectionReason,
  });
const point = (
  sourceKind: bigint,
  inputIndex: bigint,
  outputIndex: number,
  subcheck = 0,
): ResolutionFaultPosition => ({
  sourceKind,
  inputIndex,
  subcheck,
  outRef: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x11),
    outputIndex,
  }),
});
const context = { body_network_mismatch: false };

describe("first-fault out-of-domain resolution receipt", () => {
  const admits = (reason: RejectionReason, claimed: ClaimedFaultOrder) =>
    leafAdmitsFault(
      subject(reason),
      context,
      { phase: "resolve", position: point(1n, 0n, 9, 5) },
      claimed,
    );
  const outOfDomain = (
    sourceKind: bigint,
    inputIndex: bigint,
  ): ClaimedFaultOrder => ({
    ResolutionOutOfDomain: { sourceKind, inputIndex },
  });
  it("admits no fault for the leaf's own out-of-domain coordinate", () => {
    expect(
      admits(
        {
          ResolvedReferenceScriptNodeLimit: {
            source_kind: 1n,
            input_index: 5n,
          },
        },
        outOfDomain(1n, 5n),
      ),
    ).toBe(false);
    expect(
      admits(
        { SpendInputSignerMissing: { input_index: 2n } },
        outOfDomain(0n, 2n),
      ),
    ).toBe(false);
  });
  it.each([
    [
      { InputNotFound: { source_kind: 1n, input_index: 5n } },
      outOfDomain(1n, 4n),
    ],
    [{ SpendInputSignerMissing: { input_index: 5n } }, outOfDomain(1n, 5n)],
    ["ValidityIntervalExcludesBlockSlot", outOfDomain(0n, 0n)],
    [{ OutputNonCanonical: { output_index: 0n } }, outOfDomain(0n, 0n)],
    [{ RequiredSignerUnsigned: { signer_index: 0n } }, outOfDomain(0n, 0n)],
    [{ InputNotFound: { source_kind: 0n, input_index: 0n } }, "NotRequired"],
    [
      { RequiredSignerUnsigned: { signer_index: 0n } },
      { ResolutionReopened: point(0n, 0n, 1) },
    ],
  ] satisfies [RejectionReason, ClaimedFaultOrder][])(
    "refuses a missing, unused or substituted resolution receipt: case %#",
    (reason, claimed) => {
      expect(() => admits(reason, claimed)).toThrow();
    },
  );
  it("refuses an out-of-domain receipt for a purpose fault", () => {
    expect(() =>
      leafAdmitsFault(
        subject({ RedeemerMissing: { purpose_kind: 0n, purpose_index: 0n } }),
        context,
        {
          phase: "purpose",
          position: {
            coordinate: { kind: 0n, index: 0n },
            subcheck: "SourceRequired",
          },
        },
        outOfDomain(0n, 0n),
      ),
    ).toThrow();
  });
});
