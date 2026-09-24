import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxId,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_EMPTY_FIELD_COMMITMENT } from "@al-ft/midgard-core/codec/native-tx-field-access";
import { deriveMidgardTxFieldPreimages } from "@al-ft/midgard-core/consensus-validation";
import type { OperatorVerdict, RejectionReason } from "@al-ft/midgard-sdk";

/**
 * A forced order's datum payload **and** the §8 carriage vector its mint redeemer
 * supplies, which §8.11 makes one claim: the vector is positional over the
 * payload's non-empty slots and MUST be exhausted exactly, so a payload handed to
 * a fixture without its vector is a payload no tx-order mint would authenticate.
 *
 * `carriage` is deliberately *not* a datum field — §8.7's content addressing is
 * why `TxOrderPayloadV1` carries no carriage identity — so builders project
 * the three fields that are, and this type is the pair they pass around.
 */
export type GenuineUserEventForcedPayload = Readonly<{
  tx_id: string;
  transaction_commitment: string;
  submitted_source: Readonly<{
    compact_cbor: string;
    witness_set_compact_cbor: string;
    field_preimage_lengths_cbor: string;
  }>;
  carriage: readonly unknown[];
}>;

/**
 * The payload and §8 carriage for a forced order over `canonicalTxCbor`.
 *
 * The carriage is one `Inline` entry per non-empty slot, in ascending field index,
 * carrying that field's own §5.1 preimage — which is what a real order under
 * §8.11 supplies when its fields fit the transaction's byte budget, and what the
 * indexer's exhaustion re-derivation counts. Shared rather than hand-rolled per
 * test file so the two cannot drift apart from each other or from the rule.
 */
export const genuineUserEventForcedPayloadForCanonicalTx = (
  canonicalTxCbor: Uint8Array,
): GenuineUserEventForcedPayload => {
  const source =
    deriveMidgardForcedTxProofSourceFromCanonicalCbor(canonicalTxCbor);
  return Object.freeze({
    tx_id: computeMidgardNativeTxId(
      decodeMidgardForcedTxFullFromCanonicalCbor(canonicalTxCbor),
    ).toString("hex"),
    transaction_commitment:
      computeMidgardForcedTxProofCommitment(source).toString("hex"),
    submitted_source: Object.freeze({
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    }),
    carriage: Object.freeze(
      deriveMidgardTxFieldPreimages(canonicalTxCbor, "forced")
        .filter(
          (field) => !field.expectedHash.equals(MIDGARD_EMPTY_FIELD_COMMITMENT),
        )
        .map((field) =>
          Object.freeze({
            Inline: Object.freeze({
              preimage: field.preimageCbor.toString("hex"),
            }),
          }),
        ),
    ),
  });
};

/**
 * Test-side inverse of the watcher's verdict projection
 * (`watcherForcedOperatorVerdictV1`): rebuilds the on-chain `OperatorVerdict`
 * a forced leaf carries from the classification tag the indexer projected it
 * onto. Every reason a fixture uses carries the zero subject coordinate, which
 * is what the #640 forced-leaf mapping prescribes, so the round trip through
 * the indexer is exact.
 */
export const userEventForcedOperatorVerdictForClassification = (
  classification: string,
): OperatorVerdict => {
  if (classification === "ForcedTxValid") {
    return "ForcedTxValid";
  }
  const reason: RejectionReason | null =
    classification === "InputNotFound"
      ? { InputNotFound: { source_kind: 0n, input_index: 0n } }
      : classification === "AddressWitnessSignatureInvalid"
        ? { AddressWitnessSignatureInvalid: { witness_index: 0n } }
        : classification === "WitnessNativeScriptFalse"
          ? { WitnessNativeScriptFalse: { script_index: 0n } }
          : classification === "ExecutionNativeScriptFalse"
            ? { ExecutionNativeScriptFalse: { execution_index: 0n } }
            : classification === "PlutusExecutionFailed"
              ? { PlutusExecutionFailed: { execution_index: 0n } }
              : classification === "FeeBelowMinimum"
                ? "FeeBelowMinimum"
                : classification === "ValueNotPreserved"
                  ? "ValueNotPreserved"
                  : null;
  if (reason === null) {
    throw new Error(
      `w15 fixtures carry no forced verdict for classification ${classification}`,
    );
  }
  return { ForcedTxInvalid: { reason } };
};
