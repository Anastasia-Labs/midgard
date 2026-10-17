import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import {
  canonicalPositionPrecedesOrEquals,
  leafAdmitsFault,
  rejectionReasonPhase,
  type ResolutionFaultPosition,
  resolutionPositionPrecedesOrEquals,
} from "../src/fraud-proof/first-fault.js";
import { forcedVerdictSubject } from "../src/fraud-proof/proof-thread-substrate.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const subject = (rejectionReason: RejectionReason | null) =>
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
const resolve = (
  reason: RejectionReason,
  observed: ResolutionFaultPosition,
  claimed: ResolutionFaultPosition | null,
) =>
  leafAdmitsFault(
    subject(reason),
    { phase: "resolve", position: observed },
    claimed,
  );

describe("first-fault traversal TypeScript twin", () => {
  it("visits canonical items before their field-end length check", () => {
    expect(
      canonicalPositionPrecedesOrEquals(
        { kind: "item", fieldIndex: 4n, itemIndex: 200n },
        { kind: "fieldEnd", fieldIndex: 4n },
      ),
    ).toBe(true);
    expect(
      leafAdmitsFault(
        subject({ FieldPreimageLengthMismatch: { field_index: 4n } }),
        {
          phase: "canonical",
          position: { kind: "item", fieldIndex: 4n, itemIndex: 200n },
        },
      ),
    ).toBe(false);
  });
  it("visits an earlier field end before a later field item", () => {
    expect(
      leafAdmitsFault(
        subject({ FieldPreimageLengthMismatch: { field_index: 3n } }),
        {
          phase: "canonical",
          position: { kind: "item", fieldIndex: 4n, itemIndex: 0n },
        },
      ),
    ).toBe(true);
  });
  it("uses ascending outrefs before subchecks, kinds, or raw ordinals", () => {
    expect(
      resolve(
        { InputNotFound: { source_kind: 1n, input_index: 0n } },
        point(0n, 200n, 1, 5),
        point(1n, 0n, 2),
      ),
    ).toBe(false);
    expect(
      resolve(
        {
          ResolvedReferenceScriptDepthLimit: {
            source_kind: 1n,
            input_index: 7n,
          },
        },
        point(0n, 0n, 2),
        point(1n, 7n, 1, 4),
      ),
    ).toBe(true);
    expect(
      resolve(
        { InputNotFound: { source_kind: 0n, input_index: 0n } },
        point(1n, 200n, 1),
        point(0n, 0n, 2),
      ),
    ).toBe(false);
  });
  it.each([1, 2, 3, 4, 5])(
    "visits same-input absence before subcheck %i",
    (subcheck) => {
      expect(
        resolutionPositionPrecedesOrEquals(
          point(0n, 9n, 2),
          point(0n, 9n, 2, subcheck),
        ),
      ).toBe(true);
      expect(
        resolutionPositionPrecedesOrEquals(
          point(0n, 9n, 2, subcheck),
          point(0n, 9n, 2),
        ),
      ).toBe(false);
    },
  );
  it("admits the exact true first fault", () => {
    const observed = point(1n, 8n, 3);
    expect(
      resolve(
        { InputNotFound: { source_kind: 1n, input_index: 8n } },
        observed,
        observed,
      ),
    ).toBe(true);
  });
  it("rejects a substituted original coordinate", () => {
    expect(() =>
      resolve(
        { InputNotFound: { source_kind: 0n, input_index: 3n } },
        point(1n, 0n, 1),
        point(0n, 4n, 2),
      ),
    ).toThrow(/original coordinate/);
  });
  it("rejects a substituted subcheck", () => {
    expect(() =>
      resolve(
        { InputNotFound: { source_kind: 0n, input_index: 3n } },
        point(1n, 0n, 1),
        point(0n, 3n, 2, 5),
      ),
    ).toThrow(/original coordinate/);
  });
  it("rejects duplicate outrefs at different raw positions", () => {
    expect(() =>
      resolutionPositionPrecedesOrEquals(point(0n, 0n, 1), point(1n, 0n, 1, 1)),
    ).toThrow(/duplicate outrefs/);
  });
  it("places static rules before InputSets independent of wire tags", () => {
    expect(rejectionReasonPhase("NetworkIdMismatch")).toBeLessThan(
      rejectionReasonPhase("EmptyInputs"),
    );
  });
  it.each([
    "ValidityIntervalExcludesBlockSlot",
    { RequiredSignerUnsigned: { signer_index: 0n } },
  ] satisfies RejectionReason[])(
    "admits earlier checks without an unused input opening: %s",
    (reason) => {
      expect(resolve(reason, point(0n, 0n, 1), null)).toBe(true);
    },
  );
  it("refutes a later-phase claim without a claimed input", () => {
    expect(
      resolve(
        { OutputNonCanonical: { output_index: 0n } },
        point(0n, 0n, 1),
        null,
      ),
    ).toBe(false);
  });
});
