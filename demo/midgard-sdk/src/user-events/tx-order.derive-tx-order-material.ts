import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxId,
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSource,
} from "@al-ft/midgard-core/codec";
import {
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  encodeMidgardFieldArrayHeader,
  midgardFieldCommitment,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  deriveMidgardTxFieldPreimages,
  MidgardForcedTxAdmissionStopped,
  validateMidgardConsensusForcedTxCbor,
} from "@al-ft/midgard-core/consensus-validation";
import { getAddressDetails, LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { ForcedTxProofSource } from "../ledger-state.js";
import { UserEventBuildError } from "./internals.js";
import {
  type TxOrderFieldCarriage,
  type TxOrderMaterial,
} from "./tx-order.submit-tx-order-config.js";

/**
 * Derives a forced order's transaction binding and the §8 carriage its material
 * requires.
 *
 * `owner` is the §8.6 min-Ada reclaim authority a tier-3 plan records, and under
 * #594's ruling it is the **order creator's own payment key hash**: raw carriage
 * and certificates alike are reclaimed by an ordinary key spend at any time after
 * the mint, with no reclaim contract and no time gate, so the authority has to be
 * a key the creator can sign with. `buildTxOrderV1` passes the wallet's payment
 * credential for exactly that reason.
 *
 * The plans this returns are the §8.4 length partition's — tier 1 for anything
 * that fits a redeemer, tier 2 up to `K`, tier 3 above it. Which of tiers 1–2 a
 * given field actually uses in the order transaction is a budget question, not a
 * length question, and is answered by {@link planTxOrderMaterialCarriage}.
 */
export const deriveTxOrderMaterial = ({
  submittedTxCbor,
  owner,
}: {
  readonly submittedTxCbor: Uint8Array;
  readonly owner: Uint8Array;
}): TxOrderMaterial => {
  const violation = validateMidgardConsensusForcedTxCbor(submittedTxCbor);
  if (violation !== null) {
    throw new MidgardForcedTxAdmissionStopped(violation);
  }
  const tx = decodeMidgardForcedTxFullFromCanonicalCbor(submittedTxCbor);
  const transactionId = computeMidgardNativeTxId(tx.compact);
  const proofSource = deriveMidgardForcedTxProofSource(tx);
  const source: ForcedTxProofSource = {
    compact_cbor: proofSource.compactCbor.toString("hex"),
    witness_set_compact_cbor: proofSource.witnessSetCompactCbor.toString("hex"),
    field_preimage_lengths_cbor:
      proofSource.fieldPreimageLengthsCbor.toString("hex"),
  };
  const carriage: TxOrderFieldCarriage[] = [];
  // §5.1's empty field is the one-byte definite-array header `80`. The on-chain
  // `next_non_empty_field` decides the same thing by comparing the committed hash
  // against `empty_field_commitment`; since #585 the two spellings agree by
  // construction, and testing the bytes keeps this loop independent of whether a
  // payload's declared hashes are trustworthy yet.
  const emptyFieldPreimage = encodeMidgardFieldArrayHeader(0);
  for (const field of deriveMidgardTxFieldPreimages(
    submittedTxCbor,
    "forced",
  )) {
    if (field.preimageCbor.equals(emptyFieldPreimage)) {
      continue;
    }
    const commitment = midgardFieldCommitment(field.preimageCbor);
    // §4: the compact structure's field hash *is* this commitment. Asserting it
    // here is what makes a producer bug surface at the producer rather than as an
    // unsatisfiable dispute later.
    if (!commitment.equals(field.expectedHash)) {
      throw new Error(
        `V1 ${field.fieldName} §4 commitment does not match the compact structure's field hash`,
      );
    }
    carriage.push({
      fieldIndex: field.fieldIndex,
      fieldName: field.fieldName,
      preimage: field.preimageCbor,
      commitment: commitment.toString("hex"),
      plan: planMidgardFieldCarriage({
        owner,
        txId: transactionId,
        fieldIndex: field.fieldIndex,
        preimage: field.preimageCbor,
      }),
    });
  }
  return {
    transactionId: transactionId.toString("hex"),
    transactionCommitment:
      computeMidgardForcedTxProofCommitment(proofSource).toString("hex"),
    submitted_source: source,
    carriage: Object.freeze(carriage),
  };
};

/**
 * The order transaction's allowance for everything that is not inline carriage:
 * body framing, the nonce input, the order output and its datum, the NFT mint,
 * the witness registration certificate, the hub reference input, the fee and the
 * signature.
 *
 * A round number, and **unmeasured** — the same status
 * {@link MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1} records for its own
 * 2,048-byte allowance. It is written out here rather than borrowed from that
 * constant because the two subtractions are not the same subtraction: that one
 * bounds *one* preimage in *any* step's redeemer, this one bounds the *sum* of an
 * order's inline preimages against *this* transaction's other content. They agree
 * numerically today, and a re-pin of either must not silently move the other.
 */
const MIDGARD_TX_ORDER_MACHINERY_ALLOWANCE_BYTES = 2_048;

/**
 * The order transaction's own reserve for inline (tier-1) carriage, in bytes —
 * the **aggregate** over every inline field, not a per-field bound.
 *
 * #594's ruling is explicit that there is **no on-chain threshold constant** for
 * the inline/reference split: the L1 transaction limit is the gate, and choosing
 * the split is an off-chain planning concern. So this is a planning reserve and
 * not a consensus bound — the authority on whether an order fits is the built
 * transaction, which the builder completes and which fails on its own if this
 * reserve was too generous.
 *
 * Aggregate is the operative word and is why this is derived rather than aliased:
 * nine fields that each fit a redeemer alone do not fit one together, so a
 * per-field constant is the wrong shape for this decision even when it carries
 * the right number.
 */
export const MIDGARD_TX_ORDER_INLINE_CARRIAGE_RESERVE_BYTES =
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes -
  MIDGARD_TX_ORDER_MACHINERY_ALLOWANCE_BYTES;

/** One field of an order's material, with the carriage the order will use. */
export type TxOrderPlannedFieldCarriage = {
  readonly fieldIndex: number;
  readonly fieldName: string;
  readonly preimage: Buffer;
  readonly commitment: string;
  /**
   * The plan for the carriage this order actually uses. Its `tier` is the one
   * the mint redeemer will name, so `publications` is exactly what has to exist
   * on-chain *before* the order transaction is built.
   */
  readonly plan: MidgardFieldCarriagePlan;
};

/**
 * A forced order's carriage decision, per non-empty field.
 *
 * `inline` and `referenced` partition {@link TxOrderMaterial.carriage}, both in
 * ascending field index, and their concatenation in that order is *not* the
 * redeemer vector — the redeemer is positional over all non-empty fields, so it
 * is assembled by merging the two back into field order. {@link carriage} is that
 * merge, and is what a builder walks.
 */
export type TxOrderCarriagePlan = {
  readonly carriage: readonly TxOrderPlannedFieldCarriage[];
  /** Fields riding in the order transaction's own mint redeemer. */
  readonly inline: readonly TxOrderPlannedFieldCarriage[];
  /** Fields whose preimages must be published before the order is built. */
  readonly referenced: readonly TxOrderPlannedFieldCarriage[];
  /** Total inline preimage bytes, against the reserve that admitted them. */
  readonly inlineBytes: number;
  readonly inlineReserveBytes: number;
};

/**
 * Chooses, per non-empty field, whether the order carries its preimage inline or
 * references a predeployed publication (#594 AC6).
 *
 * The rule is simplest-fitting-first (GOAL_SPEC §3.2) under one aggregate
 * reserve, taken in **descending preimage size**: the largest field is offered
 * the reserve first, so a small field never spends budget the large one then
 * cannot have and get itself published for nothing. Fields above `K` are tier 3
 * by §8.4's partition and never compete for the reserve at all; fields the
 * reserve cannot take are demoted to tier 2 and published at the creator's own
 * wallet address, which does not cost the order transaction a byte because
 * referenced datums are not part of it.
 *
 * `owner` must be the creator's payment key hash — see
 * {@link deriveTxOrderMaterial} on why reclaim authority is a key and not a
 * script.
 */
export const planTxOrderMaterialCarriage = ({
  material,
  owner,
  inlineReserveBytes = MIDGARD_TX_ORDER_INLINE_CARRIAGE_RESERVE_BYTES,
}: {
  readonly material: TxOrderMaterial;
  readonly owner: Uint8Array;
  readonly inlineReserveBytes?: number;
}): TxOrderCarriagePlan => {
  if (!Number.isSafeInteger(inlineReserveBytes) || inlineReserveBytes < 0) {
    throw new Error(
      `inlineReserveBytes must be a non-negative integer, got ${String(inlineReserveBytes)}`,
    );
  }
  const transactionId = Buffer.from(material.transactionId, "hex");
  const inlineCandidates = [...material.carriage]
    .filter((field) => field.plan.tier === "Inline")
    .sort(
      (left, right) =>
        right.preimage.length - left.preimage.length ||
        left.fieldIndex - right.fieldIndex,
    );
  const inlineFieldIndices = new Set<number>();
  let inlineBytes = 0;
  for (const field of inlineCandidates) {
    if (inlineBytes + field.preimage.length > inlineReserveBytes) {
      continue;
    }
    inlineBytes += field.preimage.length;
    inlineFieldIndices.add(field.fieldIndex);
  }
  const carriage = material.carriage.map(
    (field): TxOrderPlannedFieldCarriage => ({
      fieldIndex: field.fieldIndex,
      fieldName: field.fieldName,
      preimage: field.preimage,
      commitment: field.commitment,
      plan:
        field.plan.tier === "Inline" && inlineFieldIndices.has(field.fieldIndex)
          ? field.plan
          : planMidgardFieldCarriage({
              owner,
              txId: transactionId,
              fieldIndex: field.fieldIndex,
              preimage: field.preimage,
              publish: true,
            }),
    }),
  );
  return {
    carriage: Object.freeze(carriage),
    inline: Object.freeze(
      carriage.filter((field) => field.plan.tier === "Inline"),
    ),
    referenced: Object.freeze(
      carriage.filter((field) => field.plan.tier !== "Inline"),
    ),
    inlineBytes,
    inlineReserveBytes,
  };
};

/**
 * The order creator's payment key hash — the §8.5/§8.7 reclaim authority.
 *
 * A key, never a script. #594's ruling makes reclaim an ordinary key spend at any
 * time after the mint, with no reclaim contract and no time gate, so an address
 * whose payment credential is a script has no way to reclaim the min-Ada it
 * locked in carriage and the refusal belongs here rather than at the first
 * attempt to spend it.
 */
export const requireTxOrderCreatorKeyHashProgram = (
  lucid: LucidEvolution,
): Effect.Effect<Buffer, UserEventBuildError> =>
  Effect.gen(function* () {
    const address = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new UserEventBuildError({
          message: "V1 tx order requires a wallet to own its §8 carriage",
          cause,
        }),
    });
    const details = yield* Effect.try({
      try: () => getAddressDetails(address),
      catch: (cause) =>
        new UserEventBuildError({
          message: `Failed to parse the tx-order creator address ${address}`,
          cause,
        }),
    });
    const paymentCredential = details.paymentCredential;
    if (paymentCredential === undefined || paymentCredential.type !== "Key") {
      return yield* Effect.fail(
        new UserEventBuildError({
          message:
            "V1 tx-order §8 carriage is reclaimed by an ordinary key spend, so the " +
            "creator's payment credential must be a key hash",
          cause: address,
        }),
      );
    }
    return Buffer.from(paymentCredential.hash, "hex");
  });
