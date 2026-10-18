import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  leafAdmitsFault,
  type ResolutionFaultPosition,
  resolutionPositionPrecedesOrEquals,
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
): ResolutionFaultPosition => ({
  sourceKind,
  inputIndex,
  subcheck: 0,
  outRef: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0x11),
    outputIndex,
  }),
});
const resolve = (
  reason: RejectionReason,
  observed: ResolutionFaultPosition,
  claimed: ResolutionFaultPosition,
) =>
  leafAdmitsFault(
    subject(reason),
    { body_network_mismatch: false },
    { phase: "resolve", position: observed },
    { ResolutionReopened: claimed },
  );

// Inside one field the out-ref decides, so a later raw index can come first.
describe("first-fault resolution order inside one field", () => {
  it("puts a smaller out-ref at a later raw index first", () => {
    expect(
      resolutionPositionPrecedesOrEquals(point(1n, 9n, 3), point(1n, 0n, 5)),
    ).toBe(true);
    expect(
      resolve(
        { InputNotFound: { source_kind: 1n, input_index: 9n } },
        point(1n, 0n, 5),
        point(1n, 9n, 3),
      ),
    ).toBe(true);
  });
  it("puts a larger out-ref at an earlier raw index after", () => {
    expect(
      resolutionPositionPrecedesOrEquals(point(1n, 0n, 5), point(1n, 9n, 3)),
    ).toBe(false);
    expect(
      resolve(
        { InputNotFound: { source_kind: 1n, input_index: 0n } },
        point(1n, 9n, 3),
        point(1n, 0n, 5),
      ),
    ).toBe(false);
  });
});
