import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import {
  MIDGARD_REJECTION_REASON_V1_ARMS,
  type MidgardRejectionReasonV1,
} from "@al-ft/midgard-core/validation-trace";
import { describe, expect, it } from "vitest";

import {
  canonicalPositionPrecedesOrEquals,
  type ClaimedFaultOrder,
  leafAdmitsFault,
  type NetworkOrderContext,
  purposeCoordinateIsValid,
  purposeCoordinateOfReason,
  type PurposeFaultPosition,
  purposePositionPrecedesOrEquals,
  type PurposeSubcheck,
  rejectionReasonPhase,
  type ResolutionFaultPosition,
  resolutionPositionPrecedesOrEquals,
} from "../src/fraud-proof/first-fault.js";
import { forcedVerdictSubject } from "../src/fraud-proof/proof-thread-substrate.js";
import {
  type RejectionReason,
  rejectionReasonFromStructural,
} from "../src/rejection-reason.js";

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
    { body_network_mismatch: false },
    { phase: "resolve", position: observed },
    claimed === null ? "NotRequired" : { ResolutionReopened: claimed },
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
        null,
        {
          phase: "canonical",
          position: { kind: "item", fieldIndex: 4n, itemIndex: 200n },
        },
        "NotRequired",
      ),
    ).toBe(false);
  });
  it("visits an earlier field end before a later field item", () => {
    expect(
      leafAdmitsFault(
        subject({ FieldPreimageLengthMismatch: { field_index: 3n } }),
        null,
        {
          phase: "canonical",
          position: { kind: "item", fieldIndex: 4n, itemIndex: 0n },
        },
        "NotRequired",
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
    expect(
      rejectionReasonPhase("NetworkIdMismatch", {
        body_network_mismatch: true,
      }),
    ).toBeLessThan(
      rejectionReasonPhase("EmptyInputs", { body_network_mismatch: true }),
    );
  });
  it("classifies a matching-body NetworkIdMismatch after resolution", () => {
    expect(
      rejectionReasonPhase("NetworkIdMismatch", {
        body_network_mismatch: false,
      }),
    ).toBe(8);
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

const context: NetworkOrderContext = { body_network_mismatch: false };
const purpose = (
  kind: bigint,
  index: bigint,
  subcheck: PurposeSubcheck,
): PurposeFaultPosition => ({ coordinate: { kind, index }, subcheck });
const existing = (kind: bigint, index: bigint): ClaimedFaultOrder => ({
  PurposeReopened: { ExistingPurpose: { coordinate: { kind, index } } },
});
const purposeAdmits = (
  reason: RejectionReason | null,
  observed: PurposeFaultPosition,
  claimed: ClaimedFaultOrder = "NotRequired",
) =>
  leafAdmitsFault(
    subject(reason),
    context,
    { phase: "purpose", position: observed },
    claimed,
  );

describe("direct-purpose traversal and strict receipts", () => {
  it("orders subchecks inside each pointer", () => {
    expect(
      purposeAdmits(
        { ScriptSourceMissing: { purpose_kind: 0n, purpose_index: 1n } },
        purpose(0n, 0n, "RedeemerRequired"),
        existing(0n, 1n),
      ),
    ).toBe(false);
    expect(
      purposeAdmits(
        { RedeemerMissing: { purpose_kind: 0n, purpose_index: 0n } },
        purpose(0n, 1n, "SourceRequired"),
        existing(0n, 0n),
      ),
    ).toBe(true);
  });
  it("orders source then redeemer within the same pointer", () => {
    expect(
      purposeAdmits(
        { RedeemerMissing: { purpose_kind: 0n, purpose_index: 0n } },
        purpose(0n, 0n, "SourceRequired"),
        existing(0n, 0n),
      ),
    ).toBe(false);
    expect(
      purposeAdmits(
        { ScriptSourceMissing: { purpose_kind: 0n, purpose_index: 0n } },
        purpose(0n, 0n, "RedeemerRequired"),
        existing(0n, 0n),
      ),
    ).toBe(true);
    expect(
      purposeAdmits(
        { RedeemerMissing: { purpose_kind: 0n, purpose_index: 0n } },
        purpose(0n, 0n, "RedeemerRequired"),
        existing(0n, 0n),
      ),
    ).toBe(true);
  });
  it.each([0n, 1n, 2n])(
    "orders kind %s before the next kind regardless of index",
    (kind) => {
      expect(
        purposePositionPrecedesOrEquals(
          purpose(kind, 99n, "RedeemerRequired"),
          purpose(kind + 1n, 0n, "SourceRequired"),
        ),
      ).toBe(true);
      expect(
        purposePositionPrecedesOrEquals(
          purpose(kind + 1n, 0n, "SourceRequired"),
          purpose(kind, 99n, "RedeemerRequired"),
        ),
      ).toBe(false);
    },
  );
  it.each([
    { kind: -1n, index: 0n },
    { kind: 4n, index: 0n },
    { kind: 0n, index: -1n },
  ])(
    "preserves invalid signed coordinate $kind/$index in an impossible receipt",
    (coordinate) => {
      const reason: RejectionReason = {
        ScriptSourceMissing: {
          purpose_kind: coordinate.kind,
          purpose_index: coordinate.index,
        },
      };
      expect(purposeCoordinateOfReason(reason)).toEqual(coordinate);
      expect(purposeCoordinateIsValid(coordinate)).toBe(false);
      expect(
        purposeAdmits(reason, purpose(0n, 0n, "SourceRequired"), {
          PurposeReopened: { ImpossiblePurpose: { coordinate } },
        }),
      ).toBe(false);
      expect(() =>
        purposeAdmits(
          reason,
          purpose(0n, 0n, "SourceRequired"),
          existing(coordinate.kind, coordinate.index),
        ),
      ).toThrow(/invalid authenticated purpose/);
    },
  );
  it.each([
    "NotRequired",
    { ResolutionReopened: point(0n, 0n, 1) },
    existing(0n, 1n),
    existing(1n, 0n),
  ] satisfies ClaimedFaultOrder[])(
    "refuses a missing, wrong-kind or substituted receipt: case %#",
    (claimed) => {
      expect(() =>
        purposeAdmits(
          { RedeemerMissing: { purpose_kind: 0n, purpose_index: 0n } },
          purpose(0n, 0n, "SourceRequired"),
          claimed,
        ),
      ).toThrow();
    },
  );
  it("refuses a substituted impossible coordinate", () => {
    expect(() =>
      purposeAdmits(
        { ScriptSourceMissing: { purpose_kind: -1n, purpose_index: 0n } },
        purpose(0n, 0n, "SourceRequired"),
        {
          PurposeReopened: {
            ImpossiblePurpose: { coordinate: { kind: 4n, index: 0n } },
          },
        },
      ),
    ).toThrow(/coordinate differs/);
  });
  it.each([
    null,
    "EmptyInputs",
    "ValueNotPreserved",
  ] satisfies (RejectionReason | null)[])(
    "refuses unused receipts for %s",
    (reason) => {
      expect(() =>
        purposeAdmits(
          reason,
          purpose(0n, 0n, "SourceRequired"),
          existing(0n, 0n),
        ),
      ).toThrow(/unused/);
    },
  );
  it("does not admit an accepted leaf", () => {
    expect(purposeAdmits(null, purpose(0n, 0n, "SourceRequired"))).toBe(false);
  });
  it("refuses an invalid observed coordinate even for an earlier leaf", () => {
    expect(() =>
      purposeAdmits("EmptyInputs", purpose(4n, 0n, "SourceRequired")),
    ).toThrow(/invalid authenticated purpose/);
  });
});

// Fixed phase groups come from the traversal specification, not the comparator.
const phases = [
  [0, ["FieldPreimageLengthMismatch", "FieldItemWidthIllegal"]],
  [2, ["NetworkIdMismatch", "FeeBelowMinimum"]],
  [3, ["EmptyInputs", "DuplicateInput", "ValidityIntervalMalformed"]],
  [4, ["AddressWitnessSignatureInvalid", "RequiredSignerUnsigned"]],
  [
    5,
    [
      "WitnessScriptHeaderMalformed",
      "WitnessNativeScriptMalformed",
      "WitnessNativeScriptNodeLimit",
      "WitnessNativeScriptDepthLimit",
      "WitnessNativeScriptFalse",
    ],
  ],
  [
    6,
    [
      "ScriptIntegrityHashMissing",
      "ObserversForbiddenOnUntaggedNetwork",
      "ObserverOrderInvalid",
    ],
  ],
  [
    7,
    [
      "ValidityIntervalExcludesBlockSlot",
      "InputNotFound",
      "InputSpentOutputNonCanonical",
      "ResolvedReferenceScriptMalformed",
      "ResolvedReferenceScriptNodeLimit",
      "ResolvedReferenceScriptDepthLimit",
      "SpendInputSignerMissing",
    ],
  ],
  [
    8,
    [
      "RedeemerMalformed",
      "OutputNonCanonical",
      "OutputReferenceScriptMalformed",
      "OutputReferenceScriptNodeLimit",
      "OutputReferenceScriptDepthLimit",
      "ProtectedOutputSignerMissing",
      "MintDeclaredAssetLimit",
      "MintItemMalformed",
      "ScriptSourceMissing",
      "RedeemerMissing",
      "UnusedScriptWitness",
      "UnusedRedeemer",
    ],
  ],
  [
    9,
    [
      "ExecutionNativeScriptMalformed",
      "ExecutionNativeScriptNodeLimit",
      "ExecutionNativeScriptDepthLimit",
      "ExecutionNativeScriptFalse",
    ],
  ],
  [10, ["ScriptIntegrityHashMismatch"]],
  [11, ["ReceivePurposePlutusV3Forbidden", "PlutusExecutionFailed"]],
  [
    12,
    [
      "InputAssetAccumulationLimit",
      "OutputAssetAccumulationLimit",
      "MintAssetAccumulationLimit",
      "OutputBelowMinAda",
      "ValueNotPreserved",
    ],
  ],
] as const;
const expectedPhases = new Map<string, number>(
  phases.flatMap(([phase, names]) => names.map((name) => [name, phase])),
);
const everyReason = MIDGARD_REJECTION_REASON_V1_ARMS.map((arm) => ({
  name: arm.name,
  reason: rejectionReasonFromStructural(
    (arm.fields.length === 0
      ? arm.name
      : {
          [arm.name]: Object.fromEntries(
            arm.fields.map((field) => [field, 0n]),
          ),
        }) as MidgardRejectionReasonV1,
  ),
}));
describe("complete N2 classification", () => {
  it("covers exactly all 48 normative arms", () => {
    expect(expectedPhases.size).toBe(48);
    expect([...expectedPhases.keys()].sort()).toEqual(
      MIDGARD_REJECTION_REASON_V1_ARMS.map((arm) => arm.name).sort(),
    );
  });
  it.each(everyReason)(
    "classifies $name and places it around a direct-purpose observation",
    ({ name, reason }) => {
      const phase = expectedPhases.get(name)!;
      expect(
        rejectionReasonPhase(reason, { body_network_mismatch: true }),
      ).toBe(phase);
      expect(rejectionReasonPhase(reason, context)).toBe(
        name === "NetworkIdMismatch" ? 8 : phase,
      );
      if (name === "ScriptSourceMissing" || name === "RedeemerMissing") return;
      const expected =
        phase < 8 ||
        (phase === 8 &&
          name !== "UnusedScriptWitness" &&
          name !== "UnusedRedeemer");
      expect(purposeAdmits(reason, purpose(0n, 0n, "SourceRequired"))).toBe(
        expected,
      );
    },
  );
});
