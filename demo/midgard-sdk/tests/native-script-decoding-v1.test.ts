/**
 * Aiken/TypeScript byte twins for the `native-script-decoding` family (#635,
 * #633 `ResolvedReferenceScript*` corner).
 *
 * Every expectation below is an **absolute** hex constant measured out of the
 * Aiken family modules
 * `onchain/aiken/lib/midgard/fraud-proofs/native-script-decoding/
 * step-0{1,2,3,4}.ak` and `engine.ak` (via `cbor.serialise` over that family's
 * own `thread_fixture_v1` fixtures). Nothing here compares one TypeScript
 * derivation against another: if either side's encoding moves, the literal
 * stops matching.
 *
 * The family is reached by direct module import rather than through
 * `src/fraud-proof/catalogue.ts`, because its catalogue category is not
 * registered yet (Q2 — the expected id at registration is `0000000d`).
 */
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1,
  NATIVE_SCRIPT_DECODING_LANGUAGE_UNBOUND_V1,
  NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND_V1,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED_V1,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL_V1,
  NativeScriptDecodingBindStateV1,
  nativeScriptDecodingBoundScanStateV1,
  nativeScriptDecodingPreBindScanStateV1,
  type NativeScriptDecodingScanThreadStateV1,
  NativeScriptDecodingScanThreadStateV1 as ScanThreadStateV1Type,
  NativeScriptDecodingStep01Args,
  NativeScriptDecodingStep02Args,
  NativeScriptDecodingStep03Args,
  NativeScriptDecodingStep03Datum,
  NativeScriptDecodingStep04Args,
  nativeScriptDecodingThreadTokenAssetNameV1,
} from "../src/fraud-proof/native-script-decoding-v1.js";
import type { EventKey, HeaderV1 } from "../src/ledger-state.js";

// ## Fixture constants (twins of `thread_fixture_v1.ak`)

const h28Y = "99".repeat(28);
const h32C = "55".repeat(32);
const h32D = "66".repeat(32);
const emptyRoot =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
/** `82005820` ‖ `8200581c` ‖ signer key — the 36-byte signature script item. */
const signatureItemHex = `820058208200581c${h28Y}`;
const itemCommitment =
  "6bff835259aa6488e4be91ec05775b3848f7411e1ce007c9d3b276fe52008063";
const itemFrontierHash =
  "aff78a64851ee72157acb4cb7308510b303bb341c4d20b6b8fe477860fefdf22";
/** blake2b-256 of the trie-key bytes `#"8258205555"`. */
const outpointKeyHash =
  "8b082d7beb1f65650ed4b145cf2e03f7a0a39d9d52fa3d2510fdb7ba82a6ae5f";

// Roots measured from `thread_claim_v1(L2TransactionEventKey{h32_c},
// L2Transaction, h32_c, None)` — single-leaf tries plus the counted-root
// commitment, hard-coded rather than recomputed so a drift in either the trie
// or the counted-root domain separation breaks the pin.
const transitionTraceRoot =
  "f14cc0f22c139520a73105ef6cca4b325cfb042aa16492d5826dab35e49e87c9";
const eventToStepRoot =
  "a23eadaf7c31920ea7218896d09c97a91b311bf980c168f98a66c4292e852247";
const eventToStepPhasRoot =
  "736fb30ed5c23191c70896a851064b5527db9a3f0f3c25f8f12d21c1f090a213";
const transitionStepPhasRoot =
  "388fc3299df4f11afdf6101679c3e07625cd874e4566bbea1fc8fe14e4119f45";

const preBindState = nativeScriptDecodingPreBindScanStateV1({
  direction: NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  sourceKind: NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL_V1,
  verifiedTxId: h32C,
  txOrderId: "",
  scanReasonClass: NATIVE_SCRIPT_DECODING_CLASS_PENDING_V1,
  priorLedgerRoot: h32C,
  outpointSourceKind: NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND_V1,
  outpointCursor: 0n,
});

const boundState: NativeScriptDecodingScanThreadStateV1 = Effect.runSync(
  nativeScriptDecodingBoundScanStateV1({
    state: preBindState,
    outpointKeyBytes: "8258205555",
    referenceScriptLanguage: 0n,
    outputIndex: 0n,
    referenceScriptTotalLength: 36n,
    referenceScriptItemCommitment: itemCommitment,
  }),
);

describe("native-script-decoding thread token", () => {
  it("concatenates category id and challenged header hash", () => {
    expect(nativeScriptDecodingThreadTokenAssetNameV1("0000000d", h28Y)).toBe(
      `0000000d${h28Y}`,
    );
  });

  it("rejects malformed category ids and header hashes", () => {
    expect(() =>
      nativeScriptDecodingThreadTokenAssetNameV1("0000d", h28Y),
    ).toThrow(/category id/);
    expect(() =>
      nativeScriptDecodingThreadTokenAssetNameV1("0000000D", h28Y),
    ).toThrow(/category id/);
    expect(() =>
      nativeScriptDecodingThreadTokenAssetNameV1("0000000d", h32C),
    ).toThrow(/header hash/);
  });
});

describe("native-script-decoding thread states", () => {
  it("pre-bind scan state matches engine.pre_bind_scan_state_v1 bytes (v1)", () => {
    expect(preBindState.outpoint_key_hash).toBe("");
    expect(preBindState.reference_script_language).toBe(
      NATIVE_SCRIPT_DECODING_LANGUAGE_UNBOUND_V1,
    );
    expect(Data.to(preBindState, ScanThreadStateV1Type)).toBe(
      `d8799f00005820${h32C}40205820${h32C}000040212020404020ff`,
    );
  });

  it("bound scan state matches engine.bound_scan_state_v1 bytes (v2)", () => {
    expect(boundState.outpoint_key_hash).toBe(outpointKeyHash);
    expect(Data.to(boundState, ScanThreadStateV1Type)).toBe(
      `d8799f00005820${h32C}40205820${h32C}00005820${outpointKeyHash}000018245820${itemCommitment}4020ff`,
    );
  });

  it("bind state matches step-01's BindStateV1 bytes (v3)", () => {
    const bindState: NativeScriptDecodingBindStateV1 = {
      direction: NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1,
      source_kind: NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED_V1,
      verified_tx_id: "",
    };
    expect(Data.to(bindState, NativeScriptDecodingBindStateV1)).toBe(
      "d8799f010140ff",
    );
  });
});

describe("native-script-decoding step arguments", () => {
  it("step-01 RecordForcedSource matches Aiken constructor 1 (v4)", () => {
    const args: NativeScriptDecodingStep01Args = {
      RecordForcedSource: {
        direction: NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1,
        input_index: 0n,
        output_index: 0n,
      },
    };
    expect(Data.to(args, NativeScriptDecodingStep01Args)).toBe(
      "d87a9f010000ff",
    );
  });

  it("step-02 args match the full Aiken wire shape (v9)", () => {
    const eventKey: EventKey = { L2TransactionEventKey: { tx_id: h32C } };
    const header: HeaderV1 = {
      prevUtxosRoot: h32C,
      utxosRoot: h32D,
      withdrawalsRoot: emptyRoot,
      forcedTransactionsRoot: emptyRoot,
      transactionsRoot: emptyRoot,
      depositsRoot: emptyRoot,
      transitionTraceRoot,
      eventToStepRoot,
      validationTracesRoot: emptyRoot,
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
      validationTraceCount: 0n,
      startTime: 10n,
      endTime: 20n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: h28Y,
      operatorVkey: h28Y,
      protocolVersion: 1n,
    };
    const args: NativeScriptDecodingStep02Args = {
      input_index: 0n,
      output_index: 0n,
      header,
      event_to_step_membership: {
        domain: "EventToStepRootDomain",
        root: eventToStepRoot,
        phas_root: eventToStepPhasRoot,
        count: 1n,
        key: eventKey,
        value: { step_index: 0n, phase: "L2Transaction" },
        proof: [],
      },
      transition_step_membership: {
        domain: "TransitionTraceRootDomain",
        root: transitionTraceRoot,
        phas_root: transitionStepPhasRoot,
        count: 1n,
        key: 0n,
        value: {
          schema_version: 1n,
          step_index: 0n,
          event_key: eventKey,
          phase: "L2Transaction",
          pre_utxos_root: h32C,
          post_utxos_root: h32D,
        },
        proof: [],
      },
      forced_membership: null,
      chosen_outpoint_source_kind:
        NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND_V1,
      chosen_outpoint_cursor: 3n,
    };
    expect(Data.to(args, NativeScriptDecodingStep02Args)).toBe(
      `d8799f0000d8799f5820${h32C}5820${h32D}5820${emptyRoot}5820${emptyRoot}5820${emptyRoot}5820${emptyRoot}5820${transitionTraceRoot}5820${eventToStepRoot}5820${emptyRoot}000000000101000a1400000000581c${h28Y}581c${h28Y}01ffd8799fd87e805820${eventToStepRoot}5820${eventToStepPhasRoot}01d87b9f5820${h32C}ffd8799f00d87b80ff80ffd8799fd87d805820${transitionTraceRoot}5820${transitionStepPhasRoot}0100d8799f0100d87b9f5820${h32C}ffd87b805820${h32C}5820${h32D}ff80ffd87a800003ff`,
    );
  });

  it("step-03 Scan args match the full Aiken wire shape (v5)", () => {
    const args: NativeScriptDecodingStep03Args = {
      Scan: {
        input_index: 0n,
        output_index: 0n,
        control_cbor: "88010004041824400000",
        chunk_proof: {
          version: 1n,
          field_index: 2n,
          item_index: 0n,
          total_length: 36n,
          chunk_index: 0n,
          chunk: signatureItemHex,
          frontier: [{ height: 0n, hash: itemFrontierHash }],
          siblings: [],
        },
        next_chunk_proof: null,
        frames: [
          {
            tail: "",
            kind: 1n,
            child_count: 2n,
            remaining: 2n,
            valid_count: 0n,
            required: 0n,
          },
        ],
        step_budget: 16n,
      },
    };
    expect(Data.to(args, NativeScriptDecodingStep03Args)).toBe(
      `d87a9f00004a88010004041824400000d8799fd8799f0102001824005824${signatureItemHex}9fd8799f005820${itemFrontierHash}ffff80ffffd87a809fd8799f400102020000ffff10ff`,
    );
  });

  it("step-03 BindOutOfDomain args ride the appended fourth tag (v8)", () => {
    // The #633 §7.2 closing arm is appended LAST so the
    // BindOutpoint/Scan/Verdict tags are unmoved: constructor index 3.
    // The negative-ordinal and unknown-source-kind faces carry no opening.
    const args: NativeScriptDecodingStep03Args = {
      BindOutOfDomain: {
        input_index: 0n,
        output_index: 1n,
        subject_field_opening: null,
      },
    };
    expect(Data.to(args, NativeScriptDecodingStep03Args)).toBe(
      "d87c9f0001d87a80ff",
    );
  });

  it("step-04 args match the Aiken wire shape (v6)", () => {
    const args: NativeScriptDecodingStep04Args = {
      input_index: 0n,
      output_index: 1n,
      fraud_proof_mint_redeemer_index: 2n,
    };
    expect(Data.to(args, NativeScriptDecodingStep04Args)).toBe(
      "d8799f000102ff",
    );
  });
});

describe("native-script-decoding step datums", () => {
  it("step-03 datum wraps the bound state under StepDatum (v7)", () => {
    const datum: NativeScriptDecodingStep03Datum = {
      fraud_prover: h28Y,
      data: boundState,
    };
    expect(Data.to(datum, NativeScriptDecodingStep03Datum)).toBe(
      `d8799f581c${h28Y}d8799fd8799f00005820${h32C}40205820${h32C}00005820${outpointKeyHash}000018245820${itemCommitment}4020ffffff`,
    );
  });
});
