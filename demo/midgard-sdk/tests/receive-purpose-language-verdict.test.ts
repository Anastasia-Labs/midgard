/**
 * The off-chain twin of the receive-purpose-language terminal and family
 * entry: a leaf recording `ReceivePurposePlutusV3Forbidden` at a receive
 * coordinate convicts unless the fact there is a forbidden execution; any
 * other leaf, accepted ones included, convicts on a forbidden execution its
 * recorded reason does not precede. A recorded coordinate that is not a
 * receive coordinate contradicts the arm.
 */
import { describe, expect, it } from "vitest";

import type {
  DirectClaimedPredicateFact,
  DirectClaimFinalState,
} from "../src/fraud-proof/direct-claim.js";
import type { DirectSourceSelection } from "../src/fraud-proof/direct-purpose-workers.js";
import type { DirectScriptBound } from "../src/fraud-proof/direct-script-derivation.js";
import type { DirectSourceBoundIndex } from "../src/fraud-proof/direct-source-workers.js";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "../src/fraud-proof/proof-thread-substrate.js";
import {
  receivePurposeLanguageContradiction,
  receivePurposeLanguageConvicts,
  receivePurposeLanguageFactCoordinate,
  receivePurposeLanguageRecordedCoordinate,
} from "../src/fraud-proof/receive-purpose-language.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const hex = (byte: number, length: number) =>
  byte.toString(16).padStart(2, "0").repeat(length);

const leaf = (reason: RejectionReason | null): DirectScriptBound => ({
  subject: {
    version: 1n,
    direction:
      reason === null
        ? PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
        : PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
    source_kind:
      reason === null
        ? PROOF_THREAD_SOURCE_KIND_ACCEPTED
        : PROOF_THREAD_SOURCE_KIND_FORCED,
    transaction_id: hex(4, 32),
    source_key: reason === null ? "" : hex(5, 36),
    rejection_reason: reason,
  },
  compact_cbor: hex(8, 5),
  network_order_context: { body_network_mismatch: false },
  event_pre_root: hex(9, 32),
});

const boundIndex = (
  reason: RejectionReason | null,
): DirectSourceBoundIndex => ({
  bound: leaf(reason),
  source_index: { root: hex(10, 32), count: 1n },
});

const arm = (kind: bigint, index: bigint): RejectionReason => ({
  ReceivePurposePlutusV3Forbidden: { purpose_kind: kind, purpose_index: index },
});

const receive = (
  reason: RejectionReason | null,
  index: bigint,
  languageTag: bigint | null,
): DirectSourceSelection => ({
  selection: {
    bound: leaf(reason),
    source_index: { root: hex(10, 32), count: 1n },
    coordinate: { kind: 3n, index },
    purpose: {
      coordinate: { kind: 3n, index },
      script_hash: hex(16, 28),
      subject: hex(16, 28),
    },
  },
  source:
    languageTag === null
      ? null
      : {
          field_index: 6n,
          item_index: 0n,
          source_key: hex(20, 2),
          language_tag: languageTag,
          script_hash: hex(16, 28),
          total_length: 5n,
          item_commitment: hex(24, 32),
        },
});

type Fact =
  | "ImpossiblePurpose"
  | "SourceAbsent"
  | "SourcePresent"
  | "RedeemerAbsent"
  | "RedeemerPresent";

/** The fact `name` about receive purpose `index`. */
const fact = (
  reason: RejectionReason | null,
  name: Fact,
  index: bigint,
): DirectClaimedPredicateFact => {
  if (name === "ImpossiblePurpose")
    return { ImpossiblePurpose: { coordinate: { kind: 3n, index } } };
  if (name === "SourceAbsent")
    return { SourceAbsent: { selection: receive(reason, index, null) } };
  if (name === "SourcePresent")
    return { SourcePresent: { selection: receive(reason, index, 0n) } };
  return {
    [name]: {
      decision: {
        authenticated: receive(reason, index, 3n),
        redeemer_missing: name === "RedeemerAbsent",
      },
    },
  } as DirectClaimedPredicateFact;
};

const final = (
  reason: RejectionReason | null,
  name: Fact,
  index: bigint,
): DirectClaimFinalState => ({
  context: { bound: boundIndex(reason), observed: null, target: "LeafPurpose" },
  claim: fact(reason, name, index),
});

describe("receive-purpose-language verdict twin", () => {
  it.each([
    [
      "the arm, its purpose impossible",
      arm(3n, 0n),
      "ImpossiblePurpose",
      0n,
      true,
    ],
    ["the arm, its source absent", arm(3n, 0n), "SourceAbsent", 0n, true],
    [
      "the arm, its source not PlutusV3",
      arm(3n, 0n),
      "SourcePresent",
      0n,
      true,
    ],
    [
      "the arm, no redeemer reaches it",
      arm(3n, 0n),
      "RedeemerAbsent",
      0n,
      true,
    ],
    [
      "the arm, its execution forbidden",
      arm(3n, 0n),
      "RedeemerPresent",
      0n,
      false,
    ],
    [
      "the arm later, an earlier execution forbidden",
      arm(3n, 1n),
      "RedeemerPresent",
      0n,
      true,
    ],
    [
      "the arm earlier, a later execution forbidden",
      arm(3n, 0n),
      "RedeemerPresent",
      1n,
      false,
    ],
    ["accepted, an execution forbidden", null, "RedeemerPresent", 0n, true],
    [
      "accepted, no redeemer reaches the execution",
      null,
      "RedeemerAbsent",
      0n,
      false,
    ],
    ["accepted, a non-PlutusV3 source", null, "SourcePresent", 0n, false],
    [
      "PlutusExecutionFailed at the same purpose, its execution forbidden",
      { PlutusExecutionFailed: { purpose_kind: 3n, purpose_index: 0n } },
      "RedeemerPresent",
      0n,
      true,
    ],
    [
      "PlutusExecutionFailed at an earlier purpose, an execution forbidden",
      { PlutusExecutionFailed: { purpose_kind: 2n, purpose_index: 0n } },
      "RedeemerPresent",
      0n,
      false,
    ],
    [
      "ScriptIntegrityHashMissing, an execution forbidden",
      "ScriptIntegrityHashMissing",
      "RedeemerPresent",
      0n,
      false,
    ],
  ] as const)("%s", (_label, reason, name, index, convicts) => {
    expect(
      receivePurposeLanguageConvicts(
        final(reason as RejectionReason | null, name, index),
      ),
    ).toBe(convicts);
  });

  it("reads the receive coordinate of every fact", () => {
    for (const name of [
      "ImpossiblePurpose",
      "SourceAbsent",
      "SourcePresent",
      "RedeemerAbsent",
      "RedeemerPresent",
    ] as const)
      expect(
        receivePurposeLanguageFactCoordinate(fact(null, name, 4n)),
      ).toStrictEqual({ kind: 3n, index: 4n });
    expect(() => receivePurposeLanguageFactCoordinate("NotRequired")).toThrow(
      /not a receive-language fact/u,
    );
  });

  it("contradicts a recorded coordinate that is not a receive coordinate", () => {
    const bound = boundIndex(arm(2n, 1n));
    const contradiction = receivePurposeLanguageContradiction(bound);
    expect(contradiction.claim).toStrictEqual({
      ImpossiblePurpose: { coordinate: { kind: 2n, index: 1n } },
    });
    expect(receivePurposeLanguageConvicts(contradiction)).toBe(true);
  });

  it("refuses a contradiction for a receive coordinate or another reason", () => {
    expect(() =>
      receivePurposeLanguageContradiction(boundIndex(arm(3n, 0n))),
    ).toThrow(/receive coordinate takes the receive route/u);
    expect(() =>
      receivePurposeLanguageContradiction(
        boundIndex("ScriptIntegrityHashMissing"),
      ),
    ).toThrow(/records another reason/u);
  });

  it("reads the recorded coordinate only from the arm", () => {
    expect(
      receivePurposeLanguageRecordedCoordinate(boundIndex(arm(3n, 2n))),
    ).toStrictEqual({ kind: 3n, index: 2n });
    expect(receivePurposeLanguageRecordedCoordinate(boundIndex(null))).toBe(
      null,
    );
    expect(
      receivePurposeLanguageRecordedCoordinate(
        boundIndex({
          RedeemerMissing: { purpose_kind: 3n, purpose_index: 2n },
        }),
      ),
    ).toBe(null);
  });
});
