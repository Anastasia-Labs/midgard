/**
 * The off-chain twin of the shared claim terminal's verdict, over the
 * missing-source and missing-redeemer leaves: an accepted leaf convicts on
 * an observed missing source; a rejected leaf convicts when its own purpose
 * predicate is refuted, or when an observed missing source precedes its
 * reason, and never when the leaf's reason is at or before it. Over an
 * unused-redeemer walk's pointer: a leaf naming that redeemer is refuted when
 * the redeemer is consumed or past the field; any other leaf convicts on an
 * unused redeemer whose audit precedes its reason.
 */
import { describe, expect, it } from "vitest";

import type {
  DirectClaimedPredicateFact,
  DirectClaimFinalState,
} from "../src/fraud-proof/direct-claim.js";
import { directClaimConvicts } from "../src/fraud-proof/direct-claim-verdict.js";
import type { DirectSourceSelection } from "../src/fraud-proof/direct-purpose-workers.js";
import type { DirectScriptBound } from "../src/fraud-proof/direct-script-derivation.js";
import {
  PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
  PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
  PROOF_THREAD_SOURCE_KIND_ACCEPTED,
  PROOF_THREAD_SOURCE_KIND_FORCED,
} from "../src/fraud-proof/proof-thread-substrate.js";
import type { RejectionReason } from "../src/rejection-reason.js";

const hex = (byte: number, length: number) =>
  byte.toString(16).padStart(2, "0").repeat(length);

const bound = (reason: RejectionReason | null): DirectScriptBound => ({
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

const observer = (
  leaf: DirectScriptBound,
  index: bigint,
  sourced: boolean,
): DirectSourceSelection => ({
  selection: {
    bound: leaf,
    source_index: { root: hex(10, 32), count: 1n },
    coordinate: { kind: 2n, index },
    purpose: {
      coordinate: { kind: 2n, index },
      script_hash: hex(16, 28),
      subject: hex(16, 28),
    },
  },
  source: sourced
    ? {
        field_index: 6n,
        item_index: 0n,
        source_key: hex(20, 2),
        language_tag: 3n,
        script_hash: hex(16, 28),
        total_length: 5n,
        item_commitment: hex(24, 32),
      }
    : null,
});

const ssm = (index: bigint): RejectionReason => ({
  ScriptSourceMissing: { purpose_kind: 2n, purpose_index: index },
});
const rm = (index: bigint): RejectionReason => ({
  RedeemerMissing: { purpose_kind: 2n, purpose_index: index },
});

/** The terminal state for `reason`, with a missing source observed at
 * `observed` (or no observation), and the leaf's own fact `claim`. */
const final = (
  reason: RejectionReason | null,
  observed: { index: bigint; sourced?: boolean } | null,
  claim: DirectClaimedPredicateFact,
): DirectClaimFinalState => {
  const leaf = bound(reason);
  return {
    context: {
      bound: { bound: leaf, source_index: { root: hex(10, 32), count: 1n } },
      observed:
        observed === null
          ? null
          : {
              authenticated: observer(
                leaf,
                observed.index,
                observed.sourced ?? false,
              ),
              redeemer_missing: false,
            },
      target: "LeafPurpose",
    },
    claim,
  };
};

const fact = (arm: string): DirectClaimedPredicateFact => {
  const selection = observer(bound(null), 0n, arm === "SourcePresent");
  if (
    arm === "SourceAbsent" ||
    arm === "SourcePresent" ||
    arm === "NativeSource"
  )
    return { [arm]: { selection } } as DirectClaimedPredicateFact;
  if (arm === "ImpossiblePurpose")
    return { ImpossiblePurpose: { coordinate: { kind: 9n, index: 0n } } };
  if (arm === "RedeemerRepeated" || arm === "RedeemerOutOfRange") return arm;
  if (arm === "RedeemerAbsent" || arm === "RedeemerPresent")
    return {
      [arm]: {
        decision: {
          authenticated: selection,
          redeemer_missing: arm === "RedeemerAbsent",
        },
      },
    } as DirectClaimedPredicateFact;
  return "NotRequired";
};

describe("direct claim verdict over a missing script source", () => {
  it.each([
    [
      "accepted, observed source absent",
      null,
      { index: 0n },
      "NotRequired",
      true,
    ],
    [
      "accepted, observed source present",
      null,
      { index: 0n, sourced: true },
      "NotRequired",
      false,
    ],
    [
      "ScriptSourceMissing, its source present",
      ssm(0n),
      null,
      "SourcePresent",
      true,
    ],
    [
      "ScriptSourceMissing, its source absent",
      ssm(0n),
      null,
      "SourceAbsent",
      false,
    ],
    [
      "ScriptSourceMissing, its purpose impossible",
      ssm(0n),
      null,
      "ImpossiblePurpose",
      true,
    ],
    [
      "ScriptSourceMissing, an earlier source absent",
      ssm(1n),
      { index: 0n },
      "SourceAbsent",
      true,
    ],
    [
      "ScriptSourceMissing, the same source absent",
      ssm(0n),
      { index: 0n },
      "SourceAbsent",
      false,
    ],
    [
      "ScriptSourceMissing, only a later source absent",
      ssm(0n),
      { index: 1n },
      "SourceAbsent",
      false,
    ],
    [
      "RedeemerMissing, an earlier source absent",
      rm(1n),
      { index: 0n },
      "RedeemerAbsent",
      true,
    ],
    [
      "RedeemerMissing, its own source absent",
      rm(0n),
      { index: 0n },
      "RedeemerAbsent",
      true,
    ],
    [
      "RedeemerMissing, only a later source absent",
      rm(0n),
      { index: 1n },
      "RedeemerAbsent",
      false,
    ],
    [
      "PlutusExecutionFailed, a source absent",
      { PlutusExecutionFailed: { purpose_kind: 2n, purpose_index: 0n } },
      { index: 0n },
      "NotRequired",
      true,
    ],
    [
      "ScriptIntegrityHashMissing, a source absent",
      "ScriptIntegrityHashMissing",
      { index: 0n },
      "NotRequired",
      false,
    ],
  ] as const)("%s", (_label, reason, observed, claim, convicts) => {
    expect(
      directClaimConvicts(
        final(reason as RejectionReason | null, observed, fact(claim)),
      ),
    ).toBe(convicts);
  });

  it("refuses a redeemer fact on a missing-source leaf", () => {
    expect(() =>
      directClaimConvicts(final(ssm(0n), null, fact("RedeemerPresent"))),
    ).toThrow(/not a fact of the leaf's predicate/u);
  });
});

/** The terminal state of an unused-redeemer walk that names redeemer
 * `index` and ends in `claim`, judged against `reason`'s leaf. */
const pointerFinal = (
  reason: RejectionReason | null,
  index: bigint,
  claim: DirectClaimedPredicateFact,
): DirectClaimFinalState => ({
  ...final(reason, null, claim),
  context: {
    ...final(reason, null, claim).context,
    target: {
      RedeemerPointer: {
        redeemer_index: index,
        coordinate: { kind: 2n, index: 0n },
      },
    },
  },
});

const ur = (index: bigint): RejectionReason => ({
  UnusedRedeemer: { redeemer_index: index },
});

describe("direct claim verdict over a redeemer pointer", () => {
  it.each([
    ["accepted, a repeated pointer", null, 0n, "RedeemerRepeated", true],
    ["accepted, a native purpose", null, 0n, "NativeSource", true],
    ["accepted, no purpose", null, 0n, "ImpossiblePurpose", true],
    ["accepted, a consumed redeemer", null, 0n, "SourcePresent", false],
    ["accepted, a purpose without a source", null, 0n, "SourceAbsent", false],
    ["accepted, past the field", null, 0n, "RedeemerOutOfRange", false],
    ["its own redeemer consumed", ur(1n), 1n, "SourcePresent", true],
    ["its own redeemer past the field", ur(1n), 1n, "RedeemerOutOfRange", true],
    ["its own redeemer repeated", ur(1n), 1n, "RedeemerRepeated", false],
    ["its own redeemer native", ur(1n), 1n, "NativeSource", false],
    ["its own redeemer without a source", ur(1n), 1n, "SourceAbsent", false],
    ["an earlier redeemer unused", ur(1n), 0n, "RedeemerRepeated", true],
    ["a later redeemer unused", ur(1n), 2n, "RedeemerRepeated", false],
    ["an earlier redeemer consumed", ur(1n), 0n, "SourcePresent", false],
    [
      "an unused witness, any redeemer unused",
      { UnusedScriptWitness: { script_index: 9n } },
      0n,
      "RedeemerRepeated",
      false,
    ],
    ["a purpose reason, a redeemer unused", rm(0n), 0n, "NativeSource", false],
    [
      "a later phase, a redeemer unused",
      "ScriptIntegrityHashMismatch",
      4n,
      "ImpossiblePurpose",
      true,
    ],
  ] as const)("%s", (_label, reason, index, claim, convicts) => {
    expect(
      directClaimConvicts(
        pointerFinal(reason as RejectionReason | null, index, fact(claim)),
      ),
    ).toBe(convicts);
  });

  it("refuses a redeemer decision on a pointer target", () => {
    expect(() =>
      directClaimConvicts(pointerFinal(null, 0n, fact("RedeemerAbsent"))),
    ).toThrow(/not a redeemer-pointer fact/u);
  });

  it("refuses a walk fact on the leaf's own purpose", () => {
    expect(() =>
      directClaimConvicts(final(ssm(0n), null, "RedeemerRepeated")),
    ).toThrow(/not a fact of the leaf's predicate/u);
  });
});
