import {
  forcedRejectionReason,
  ForcedRejectionStopped,
} from "@al-ft/midgard-fault-proofs";
import {
  OperatorVerdictSchema,
  rejectionReasonArmOf,
} from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  isWatcherForcedOperatorVerdict,
  WATCHER_FORCED_TX_VALID,
  watcherForcedOperatorVerdict,
} from "../../src/indexers/user-event-indexer.js";
import { canonicalRejectionArm } from "../../src/verification/block-replay.replay-forced-transition-effect.js";
import { WatcherBlockReplayError } from "../../src/verification/block-replay.watcher-block-replay-result.js";
import { userEventForcedOperatorVerdictForClassification } from "../support/user-event-forced-order-fixture.js";

/**
 * The #640 forced-inclusion verdict vocabulary, at the watcher's boundary.
 *
 * `ForcedInclusionTxV1.verdict` carries an `OperatorVerdictV1`, and the watcher
 * projects it onto a JSON-safe constructor tag
 * (`watcherForcedOperatorVerdict`) because its classification records are
 * canonical-JSON digested and the reason payloads are `bigint` ordinals. That
 * projection is the hinge the forced classification comparison in
 * `bindForcedTransitionEffectV1` turns on, so it is pinned here directly rather
 * than only through the W25 replay fixtures.
 *
 * This file is deliberately independent of those fixtures' ledger state: the
 * watcher's forced replay fixtures are currently blocked short of the verdict
 * comparison by the `E_MIN_ADA` floor that #618/#627 added to Phase B
 * (`makeOutput(10n)` outputs sit far below it), which is a #617-wave
 * propagation gap and not a verdict-vocabulary property. The pins below hold
 * whether or not that gap is repaired.
 */

/** The verdict tags the watcher's forced-order fixtures commit. */
const FIXTURE_CLASSIFICATIONS = Object.freeze([
  "ForcedTxValid",
  "InputNotFound",
  "AddressWitnessSignatureInvalid",
  "WitnessNativeScriptFalse",
  "ExecutionNativeScriptFalse",
  "PlutusExecutionFailed",
  "FeeBelowMinimum",
  "ValueNotPreserved",
] as const);

/**
 * The exact Plutus Data encoding of each fixture verdict.
 *
 * Constructor index `i` in 0..6 tags as `121 + i` (`d879`..`d87f`) and `i >= 7`
 * as `1280 + (i - 7)`, non-empty constructor fields serialise as indefinite
 * arrays (`9f`..`ff`), and zero-field constructors compact to tag + `80`. These
 * are wire-normative: the constructor order of `RejectionReasonV1` fixes the
 * forced leaf's indices, so a reordered, inserted, or removed arm moves these
 * bytes and must break this test.
 */
const FIXTURE_VERDICT_CBOR = Object.freeze({
  ForcedTxValid: "d87980",
  InputNotFound: "d87a9fd9050b9f0000ffff",
  AddressWitnessSignatureInvalid: "d87a9fd905009f00ffff",
  WitnessNativeScriptFalse: "d87a9fd905069f00ffff",
  ExecutionNativeScriptFalse: "d87a9fd9051f9f00ffff",
  PlutusExecutionFailed: "d87a9fd905229f00ffff",
  FeeBelowMinimum: "d87a9fd87f80ff",
  ValueNotPreserved: "d87a9fd9052780ff",
} as const);

/**
 * Every `RejectionReasonV1` constructor tag, in the schema's wire order.
 *
 * Restated here rather than imported so that the watcher's accepted vocabulary
 * is pinned against the catalogue independently of the SDK helper it delegates
 * membership to — an arm silently dropped from the twin's code bridge would
 * otherwise narrow what the watcher accepts with nothing to notice.
 */
const REJECTION_REASON_ARMS = Object.freeze([
  "FieldPreimageLengthMismatch",
  "FieldItemWidthIllegal",
  "EmptyInputs",
  "DuplicateInput",
  "ValidityIntervalMalformed",
  "NetworkIdMismatch",
  "FeeBelowMinimum",
  "AddressWitnessSignatureInvalid",
  "RequiredSignerUnsigned",
  "WitnessScriptHeaderMalformed",
  "WitnessNativeScriptMalformed",
  "WitnessNativeScriptNodeLimit",
  "WitnessNativeScriptDepthLimit",
  "WitnessNativeScriptFalse",
  "ScriptIntegrityHashMissing",
  "ObserversForbiddenOnUntaggedNetwork",
  "ObserverOrderInvalid",
  "ValidityIntervalExcludesBlockSlot",
  "InputNotFound",
  "InputSpentOutputNonCanonical",
  "ResolvedReferenceScriptMalformed",
  "ResolvedReferenceScriptNodeLimit",
  "ResolvedReferenceScriptDepthLimit",
  "SpendInputSignerMissing",
  "RedeemerMalformed",
  "OutputNonCanonical",
  "OutputReferenceScriptMalformed",
  "OutputReferenceScriptNodeLimit",
  "OutputReferenceScriptDepthLimit",
  "ProtectedOutputSignerMissing",
  "MintDeclaredAssetLimit",
  "ScriptSourceMissing",
  "RedeemerMissing",
  "UnusedScriptWitness",
  "UnusedRedeemer",
  "ExecutionNativeScriptMalformed",
  "ExecutionNativeScriptNodeLimit",
  "ExecutionNativeScriptDepthLimit",
  "ExecutionNativeScriptFalse",
  "ScriptIntegrityHashMismatch",
  "ReceivePurposePlutusV3Forbidden",
  "PlutusExecutionFailed",
  "InputAssetAccumulationLimit",
  "OutputAssetAccumulationLimit",
  "MintAssetAccumulationLimit",
  "OutputBelowMinAda",
  "ValueNotPreserved",
] as const);

/** The spellings the retired six-member `MidgardTxValidity` classification used. */
const RETIRED_VALIDITY_SPELLINGS = Object.freeze([
  "TxIsValid",
  "TxIsInvalid",
  "NonExistentInputUtxo",
  "InvalidSignature",
  "FailedScript",
  "FeeTooLow",
  "UnbalancedTx",
] as const);

const encodeVerdict = (classification: string): string =>
  Data.to(
    userEventForcedOperatorVerdictForClassification(classification) as never,
    OperatorVerdictSchema as never,
  );

describe("forced operator verdict vocabulary", () => {
  it("serialises every fixture verdict to its exact wire bytes", () => {
    expect(
      Object.fromEntries(
        FIXTURE_CLASSIFICATIONS.map((classification) => [
          classification,
          encodeVerdict(classification),
        ]),
      ),
    ).toStrictEqual({ ...FIXTURE_VERDICT_CBOR });
  });

  it("round-trips every fixture verdict from the wire back to its tag", () => {
    for (const classification of FIXTURE_CLASSIFICATIONS) {
      const decoded = Data.from(
        FIXTURE_VERDICT_CBOR[classification],
        OperatorVerdictSchema as never,
      );
      expect(watcherForcedOperatorVerdict(decoded), classification).toBe(
        classification,
      );
    }
  });

  it("accepts the whole 47-arm vocabulary plus the accepting literal", () => {
    expect(REJECTION_REASON_ARMS).toHaveLength(47);
    expect(new Set(REJECTION_REASON_ARMS).size).toBe(47);
    expect(WATCHER_FORCED_TX_VALID).toBe("ForcedTxValid");
    expect(isWatcherForcedOperatorVerdict(WATCHER_FORCED_TX_VALID)).toBe(true);
    for (const arm of REJECTION_REASON_ARMS) {
      expect(isWatcherForcedOperatorVerdict(arm), arm).toBe(true);
    }
  });

  it("refuses the retired validity spellings and every non-tag value", () => {
    for (const spelling of RETIRED_VALIDITY_SPELLINGS) {
      expect(isWatcherForcedOperatorVerdict(spelling), spelling).toBe(false);
      expect(watcherForcedOperatorVerdict(spelling), spelling).toBeNull();
    }
    for (const value of [
      null,
      undefined,
      0n,
      "",
      "ForcedTxInvalid",
      {},
      { ForcedTxInvalid: {} },
      { ForcedTxInvalid: { reason: "TxIsInvalid" } },
      { ForcedTxInvalid: { reason: {} } },
      { ForcedTxValid: [] },
    ]) {
      expect(watcherForcedOperatorVerdict(value)).toBeNull();
      expect(isWatcherForcedOperatorVerdict(value)).toBe(false);
    }
  });

  it("admits every tag the forced rejection writer emits", () => {
    // Forced replay compares the committed verdict's tag against the tag of
    // `forcedRejectionReason`, the one writer the node commits with, so every
    // tag it can produce must be in the watcher's accepted vocabulary. A
    // located code is written from its subject, whose tag is one of the arms
    // pinned above; every unsupported code or missing subject stops replay.
    for (const code of Object.values(RejectCodes)) {
      let tag: string;
      try {
        tag = rejectionReasonArmOf(
          forcedRejectionReason({ code, consensusPhase: "canonicalDecode" }),
        );
      } catch (error) {
        expect(error, code).toBeInstanceOf(ForcedRejectionStopped);
        continue;
      }
      expect(isWatcherForcedOperatorVerdict(tag), code).toBe(true);
      expect(tag, code).not.toBe(WATCHER_FORCED_TX_VALID);
    }
  });
  it("uses the exact machine arm for a supported located rejection", () => {
    expect(
      canonicalRejectionArm({
        code: RejectCodes.PlutusScriptInvalid,
        subject: { arm: "PlutusExecutionFailed", index: 7n },
        consensusPhase: "cek",
      }),
    ).toBe("PlutusExecutionFailed");
  });

  it.each([
    [RejectCodes.PlutusEvaluationUnavailable, "forced_evaluation_unavailable"],
    [RejectCodes.AuxDataForbidden, "forced_rejection_unsupported"],
    [RejectCodes.InvalidOutput, "forced_rejection_unsupported"],
  ] as const)(
    "stops replay for %s instead of producing a verdict",
    (code, expected) => {
      let caught: unknown;
      try {
        canonicalRejectionArm({ code, consensusPhase: "canonicalDecode" });
      } catch (error) {
        caught = error;
      }
      expect(caught).toBeInstanceOf(WatcherBlockReplayError);
      expect((caught as WatcherBlockReplayError).code).toBe(expected);
      expect((caught as WatcherBlockReplayError).path).toContain(code);
    },
  );
});
