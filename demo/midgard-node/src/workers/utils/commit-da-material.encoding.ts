import {
  computeMidgardNativeTxId,
  deriveMidgardForcedTxProofSource,
  type MidgardForcedTxFull,
} from "@al-ft/midgard-core/codec";
import { type MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

import { encodeTransactionRootValue } from "../../mpf/ledger-hydration.js";
import {
  encodeEventToStepValueCbor,
  encodeTransitionEventKeyCbor,
  encodeTransitionIntegerCbor,
  encodeTransitionStepCbor,
} from "../../mpf/transition-cbor.js";
import {
  DA_PAYLOAD_UPPER_BOUND_HEADER,
  DA_PAYLOAD_UPPER_BOUND_HEADER_HASH,
} from "./commit-block-planner.select-commit-tx-candidates.js";

const UINT64_MAX = 0xffff_ffff_ffff_ffffn;
const HASH32 = "ff".repeat(32);
export const daEntryTupleBytes = (key: Uint8Array, value: Uint8Array): bigint =>
  BigInt(
    SDK.daPayloadEntryEncodedSize([
      Buffer.from(key).toString("hex"),
      Buffer.from(value).toString("hex"),
    ]),
  );

/** Widest native header/count integers, fixed hash widths, every outer wrapper,
 * and one extra byte for each of the nine possibly nonempty entry lists. */
export const additiveDaFixedBytes = (): bigint => {
  const header = DA_PAYLOAD_UPPER_BOUND_HEADER;
  const payload: SDK.DaPayload = {
    version: SDK.DA_PAYLOAD_VERSION,
    block_body: {
      header_hash: DA_PAYLOAD_UPPER_BOUND_HEADER_HASH,
      header,
      withdrawals: [],
      forced_transactions: [],
      transactions: [],
      transaction_preimages: [],
      forced_transaction_preimages: [],
      deposits: [],
      transition_trace: [],
      event_to_step: [],
      validation_traces: [],
      validation_trace_witnesses: [],
      counts: {
        withdrawalCount: UINT64_MAX,
        forcedTransactionCount: UINT64_MAX,
        l2TransactionCount: UINT64_MAX,
        depositCount: UINT64_MAX,
        totalEventCount: UINT64_MAX,
        transitionStepCount: UINT64_MAX,
        validationTraceCount: UINT64_MAX,
      },
    },
  };
  return BigInt(SDK.daPayloadEncodedSize(payload) + 9);
};

export const additiveEventTraceBytes = (
  eventKey: SDK.EventKey,
  phase: SDK.TransitionPhase,
  validation: boolean,
): bigint => {
  const key = encodeTransitionEventKeyCbor(eventKey);
  let bytes =
    daEntryTupleBytes(
      encodeTransitionIntegerCbor(UINT64_MAX),
      encodeTransitionStepCbor({
        schema_version: SDK.TRANSITION_STEP_SCHEMA_VERSION,
        step_index: UINT64_MAX,
        event_key: eventKey,
        phase,
        pre_utxos_root: HASH32,
        post_utxos_root: HASH32,
      }),
    ) +
    daEntryTupleBytes(
      key,
      encodeEventToStepValueCbor({ step_index: UINT64_MAX, phase }),
    );
  if (validation) {
    const descriptor: SDK.ValidationTraceDescriptor = {
      schema_version: 1n,
      machine_version: 1n,
      trace_root: HASH32,
      step_count: UINT64_MAX,
      initial_state_hash: HASH32,
      terminal_state_hash: HASH32,
      verdict: "Rejected",
      rejection_code_hash: HASH32,
    };
    bytes += daEntryTupleBytes(
      key,
      Buffer.from(
        LucidData.to(descriptor, SDK.ValidationTraceDescriptor),
        "hex",
      ),
    );
  }
  return bytes;
};

/** Every reachable rejection arm carries at most four ordinal integers. Their
 * magnitudes are bounded by admitted field/collection counts, below uint64.
 * DuplicateInput supplies all four; one additional byte covers a constructor
 * tag above the short-tag range. No hash/string/expected value is a payload. */
export const forcedInclusionValueBytesUpperBound = (
  transaction: MidgardForcedTxFull,
): Buffer => {
  const source = deriveMidgardForcedTxProofSource(transaction);
  const value: SDK.ForcedInclusionTxV1 = {
    tx_id: computeMidgardNativeTxId(transaction.compact).toString("hex"),
    submitted_source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: {
      ForcedTxInvalid: {
        reason: {
          DuplicateInput: {
            first_field_index: UINT64_MAX,
            first_item_index: UINT64_MAX,
            second_field_index: UINT64_MAX,
            second_item_index: UINT64_MAX,
          },
        },
      },
    },
  };
  const encoded = Buffer.from(
    aikenSerialisedPlutusDataCbor(LucidData.to(value, SDK.ForcedInclusionTxV1)),
    "hex",
  );
  return Buffer.alloc(encoded.length + 1);
};

export const withdrawalValueBytesUpperBound = (raw: Buffer): Buffer => {
  const info = LucidData.from(raw.toString("hex"), SDK.WithdrawalInfo);
  // Every other current validity arm is empty; the spent arm adds a tx id.
  const widest: SDK.WithdrawalInfo = {
    ...info,
    validity: { SpentWithdrawalUtxo: { l2_tx_id: HASH32 } },
  };
  return Buffer.from(
    aikenSerialisedPlutusDataCbor(LucidData.to(widest, SDK.WithdrawalInfo)),
    "hex",
  );
};

export const ordinarySourceBytes = (
  id: Buffer,
  cbor: Buffer,
  profile: MidgardConsensusProfile,
): bigint =>
  daEntryTupleBytes(id, encodeTransactionRootValue(cbor, profile)) +
  daEntryTupleBytes(id, cbor);
