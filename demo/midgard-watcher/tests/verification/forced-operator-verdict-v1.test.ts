import { OperatorVerdictV1Schema } from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  isWatcherForcedOperatorVerdictV1,
  WATCHER_FORCED_TX_VALID_V1,
  watcherForcedOperatorVerdictV1,
} from "../../src/indexers/user-event-indexer.js";
import { watcherBlockReplayForcedValidityForRejectCodeV1 } from "../../src/verification/block-replay.js";
import { w15ForcedOperatorVerdictForClassificationV1 } from "../support/w15-authority-scenarios.js";

/**
 * The #640 forced-inclusion verdict vocabulary, at the watcher's boundary.
 *
 * `ForcedInclusionTxV1.verdict` carries an `OperatorVerdictV1`, and the watcher
 * projects it onto a JSON-safe constructor tag
 * (`watcherForcedOperatorVerdictV1`) because its classification records are
 * canonical-JSON digested and the reason payloads are `bigint` ordinals. That
 * projection is the hinge the forced classification comparison in
 * `bindForcedTransitionEffectV1` turns on, so it is pinned here directly rather
 * than only through the W25/W26 replay fixtures.
 *
 * This file is deliberately independent of those fixtures' ledger state: the
 * watcher's forced replay fixtures are currently blocked short of the verdict
 * comparison by the `E_MIN_ADA` floor that #618/#627 added to Phase B
 * (`makeOutput(10n)` outputs sit far below it), which is a #617-wave
 * propagation gap and not a verdict-vocabulary property. The pins below hold
 * whether or not that gap is repaired.
 */

/** The verdict tags the canonical rejection-to-forced-verdict partition emits. */
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
const REJECTION_REASON_ARMS_V1 = Object.freeze([
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
const RETIRED_VALIDITY_SPELLINGS_V1 = Object.freeze([
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
    w15ForcedOperatorVerdictForClassificationV1(classification) as never,
    OperatorVerdictV1Schema as never,
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
        OperatorVerdictV1Schema as never,
      );
      expect(watcherForcedOperatorVerdictV1(decoded), classification).toBe(
        classification,
      );
    }
  });

  it("accepts the whole 47-arm vocabulary plus the accepting literal", () => {
    expect(REJECTION_REASON_ARMS_V1).toHaveLength(47);
    expect(new Set(REJECTION_REASON_ARMS_V1).size).toBe(47);
    expect(WATCHER_FORCED_TX_VALID_V1).toBe("ForcedTxValid");
    expect(isWatcherForcedOperatorVerdictV1(WATCHER_FORCED_TX_VALID_V1)).toBe(
      true,
    );
    for (const arm of REJECTION_REASON_ARMS_V1) {
      expect(isWatcherForcedOperatorVerdictV1(arm), arm).toBe(true);
    }
  });

  it("refuses the retired validity spellings and every non-tag value", () => {
    for (const spelling of RETIRED_VALIDITY_SPELLINGS_V1) {
      expect(isWatcherForcedOperatorVerdictV1(spelling), spelling).toBe(false);
      expect(watcherForcedOperatorVerdictV1(spelling), spelling).toBeNull();
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
      expect(watcherForcedOperatorVerdictV1(value)).toBeNull();
      expect(isWatcherForcedOperatorVerdictV1(value)).toBe(false);
    }
  });

  it("partitions the canonical reject codes onto the reason tags", () => {
    // The class boundaries are the ones the partition has always published;
    // #640 re-spells each class as the tag the forced leaf now carries, and
    // E_NATIVE_SCRIPT_INVALID is the one phase-split code: the node
    // classifier commits WitnessNativeScriptFalse when Phase A rejects and
    // ExecutionNativeScriptFalse when Phase B does, so the replay's
    // representative must split identically or exact-arm comparison flags
    // honest operators.
    for (const phase of ["phaseA", "phaseB"] as const) {
      expect(
        watcherBlockReplayForcedValidityForRejectCodeV1(
          RejectCodes.InputNotFound,
          phase,
        ),
      ).toBe("InputNotFound");
      for (const code of [
        RejectCodes.InvalidSignature,
        RejectCodes.MissingRequiredWitness,
      ]) {
        expect(
          watcherBlockReplayForcedValidityForRejectCodeV1(code, phase),
          code,
        ).toBe("AddressWitnessSignatureInvalid");
      }
      for (const code of [
        RejectCodes.PlutusScriptInvalid,
        RejectCodes.PlutusEvaluationUnavailable,
      ]) {
        expect(
          watcherBlockReplayForcedValidityForRejectCodeV1(code, phase),
          code,
        ).toBe("PlutusExecutionFailed");
      }
      expect(
        watcherBlockReplayForcedValidityForRejectCodeV1(
          RejectCodes.MinFee,
          phase,
        ),
      ).toBe("FeeBelowMinimum");
      // The catch-all: every code outside the named classes, which is what
      // the retired `UnbalancedTx` arm covered.
      expect(
        watcherBlockReplayForcedValidityForRejectCodeV1(
          RejectCodes.ValueNotPreserved,
          phase,
        ),
      ).toBe("ValueNotPreserved");
      expect(
        watcherBlockReplayForcedValidityForRejectCodeV1(
          RejectCodes.TxSize,
          phase,
        ),
      ).toBe("ValueNotPreserved");
    }
    expect(
      watcherBlockReplayForcedValidityForRejectCodeV1(
        RejectCodes.NativeScriptInvalid,
        "phaseA",
      ),
    ).toBe("WitnessNativeScriptFalse");
    expect(
      watcherBlockReplayForcedValidityForRejectCodeV1(
        RejectCodes.NativeScriptInvalid,
        "phaseB",
      ),
    ).toBe("ExecutionNativeScriptFalse");
  });

  it("emits only tags the watcher classification vocabulary admits", () => {
    for (const phase of ["phaseA", "phaseB"] as const) {
      for (const code of Object.values(RejectCodes)) {
        const tag = watcherBlockReplayForcedValidityForRejectCodeV1(
          code,
          phase,
        );
        expect(isWatcherForcedOperatorVerdictV1(tag), code).toBe(true);
        expect(tag, code).not.toBe(WATCHER_FORCED_TX_VALID_V1);
        expect(encodeVerdict(tag), code).toBe(
          FIXTURE_VERDICT_CBOR[tag as keyof typeof FIXTURE_VERDICT_CBOR],
        );
      }
    }
  });
});
