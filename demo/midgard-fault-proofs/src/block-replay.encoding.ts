/**
 * The block replay's canonical encoders: transition-trace integers, phases,
 * event keys, steps and event-to-step values, a transaction-root value, and
 * the canonical phase order. The node's block builder and every replayer
 * (node foreign-block import, watcher ledger) share these.
 */

import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
  encodeCborBytes,
  encodeCborInteger,
  encodeCborTagRaw,
} from "@al-ft/midgard-core/codec";
import {
  isMidgardConsensusProfile,
  MIDGARD_CONSENSUS_PROFILE,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import { aikenSerialisedPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";

const encodeUnsignedBigEndian = (value: bigint): Buffer => {
  if (value <= 0n) return Buffer.from([0]);
  const bytes: number[] = [];
  for (let remaining = value; remaining > 0n; remaining >>= 8n) {
    bytes.push(Number(remaining & 0xffn));
  }
  bytes.reverse();
  return Buffer.from(bytes);
};

export const encodeTransitionIntegerCbor = (value: bigint): Buffer => {
  const magnitude = value >= 0n ? value : -1n - value;
  if (magnitude <= 0xffff_ffff_ffff_ffffn) {
    return encodeCborInteger(value);
  }
  return encodeCborTagRaw(
    value >= 0n ? 2n : 3n,
    encodeCborBytes(encodeUnsignedBigEndian(magnitude)),
  );
};

const encodeTransitionConstr = (
  alternative: 0 | 1 | 2 | 3,
  fields: readonly Buffer[],
): Buffer => {
  const tag = Buffer.from([0xd8, 0x79 + alternative]);
  return fields.length === 0
    ? Buffer.concat([tag, Buffer.from([0x80])])
    : Buffer.concat([tag, Buffer.from([0x9f]), ...fields, Buffer.from([0xff])]);
};

const encodeTransitionFixedBytes = (value: string, fieldName: string): Buffer =>
  encodeCborBytes(
    Buffer.from(normalizeHex(value, { fieldName, byteLength: 32 }), "hex"),
  );

const encodeTransitionOutputReference = (
  outputReference: SDK.OutputReference,
): Buffer =>
  encodeTransitionConstr(0, [
    encodeTransitionFixedBytes(
      outputReference.transactionId,
      "transition event transaction id",
    ),
    encodeTransitionIntegerCbor(outputReference.outputIndex),
  ]);

export const encodeTransitionPhaseCbor = (
  phase: SDK.TransitionPhase,
): Buffer => {
  switch (phase) {
    case "Withdrawal":
      return encodeTransitionConstr(0, []);
    case "ForcedTransaction":
      return encodeTransitionConstr(1, []);
    case "L2Transaction":
      return encodeTransitionConstr(2, []);
    case "Deposit":
      return encodeTransitionConstr(3, []);
  }
};

export const encodeTransitionEventKeyCbor = (
  eventKey: SDK.EventKey,
): Buffer => {
  if ("WithdrawalEventKey" in eventKey) {
    return encodeTransitionConstr(0, [
      encodeTransitionOutputReference(
        eventKey.WithdrawalEventKey.withdrawal_id,
      ),
    ]);
  }
  if ("ForcedTransactionEventKey" in eventKey) {
    return encodeTransitionConstr(1, [
      encodeTransitionOutputReference(
        eventKey.ForcedTransactionEventKey.tx_order_id,
      ),
    ]);
  }
  if ("L2TransactionEventKey" in eventKey) {
    return encodeTransitionConstr(2, [
      encodeTransitionFixedBytes(
        eventKey.L2TransactionEventKey.tx_id,
        "transition event transaction hash",
      ),
    ]);
  }
  return encodeTransitionConstr(3, [
    encodeTransitionOutputReference(eventKey.DepositEventKey.deposit_id),
  ]);
};

export const encodeTransitionStepCbor = (value: SDK.TransitionStep): Buffer =>
  encodeTransitionConstr(0, [
    encodeTransitionIntegerCbor(value.schema_version),
    encodeTransitionIntegerCbor(value.step_index),
    encodeTransitionEventKeyCbor(value.event_key),
    encodeTransitionPhaseCbor(value.phase),
    encodeTransitionFixedBytes(
      value.pre_utxos_root,
      "transition pre UTxO root",
    ),
    encodeTransitionFixedBytes(
      value.post_utxos_root,
      "transition post UTxO root",
    ),
  ]);

export const encodeEventToStepValueCbor = (
  value: SDK.EventToStepValue,
): Buffer =>
  encodeTransitionConstr(0, [
    encodeTransitionIntegerCbor(value.step_index),
    encodeTransitionPhaseCbor(value.phase),
  ]);

/** The canonical order of a block's phases: withdrawals, forced
 * transactions, L2 transactions, then deposits. */
export const transitionPhaseRank = (phase: SDK.TransitionPhase): number => {
  switch (phase) {
    case "Withdrawal":
      return 0;
    case "ForcedTransaction":
      return 1;
    case "L2Transaction":
      return 2;
    case "Deposit":
      return 3;
  }
};

export const encodeTransactionRootValue = (
  txCanonicalCbor: Buffer,
  consensusProfile: MidgardConsensusProfile = MIDGARD_CONSENSUS_PROFILE,
): Buffer => {
  if (!isMidgardConsensusProfile(consensusProfile)) {
    throw new Error("Refusing transaction under a non-V1 consensus profile");
  }
  const violation = validateMidgardConsensusTxCbor(txCanonicalCbor);
  if (violation !== null) {
    throw new Error(
      `Refusing transaction outside the exact canonical V1 consensus profile: ${violation.code} ${violation.featureId} ${violation.detail}`,
    );
  }
  const source =
    deriveMidgardNativeTxProofSourceFromCanonicalCbor(txCanonicalCbor);
  const transactionId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  const value: SDK.L2TransactionSource = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    },
  };
  return Buffer.from(
    aikenSerialisedPlutusDataCbor(LucidData.to(value, SDK.L2TransactionSource)),
    "hex",
  );
};
