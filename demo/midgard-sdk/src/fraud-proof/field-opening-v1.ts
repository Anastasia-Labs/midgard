/**
 * The TypeScript wire twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/field-opening-v1.ak` — how a family
 * step **downstream of its own step-01** reaches one of the nine committed
 * fields of the transaction its computation thread is disputing
 * (`docs/spec/midgard-tx.md` §4, §8.8, §10).
 *
 * Every rebound Q1x family step speaks exactly two shapes, and they are the
 * whole of what #575 changed off-chain:
 *
 *   * **thread state carries an anchor** ({@link NativeTxAnchorV1Schema}) — the
 *     §2.5 transaction id, plus the `witness_set_hash` for a family whose later
 *     step reads a witness-set field. It is *not* the retired per-field
 *     collection commitment (`..._spend_inputs_hash` and its siblings), which no
 *     validator reads any more.
 *   * **the step redeemer carries an opening** ({@link FieldOpeningV1Schema}) —
 *     the compact transaction bytes plus one §8 carriage tier. It is *not* a
 *     reproduced `..._preimage: List<…>` argument; that argument is gone from
 *     every rebound step, and a builder still emitting one produces a redeemer
 *     whose constructor arity the validator cannot decode. That is exactly the
 *     `Spend[0] the validator crashed / exited prematurely` signature the
 *     fault-proofs suite showed while the builders were stale.
 *
 * **Constructor order is frozen consensus wire format.** `Data.Enum` pins the
 * Constr index positionally, so the array order below *is* the on-chain tag
 * order: `BodyAnchor` 0 / `WitnessAnchor` 1, and `BodyFieldOpening` 0 /
 * `WitnessFieldOpening` 1. Reordering either array silently re-tags every datum
 * and redeemer built through this module. `tests/field-opening-v1.test.ts`
 * pins both against exact CBOR rather than trusting this comment.
 *
 * **Why the anchor is the transaction id and not a field hash.** Under §4's
 * plain hashing a field commitment carries no field index and no domain tag, so
 * a hash on its own says nothing about which slot it came from — and carrying
 * one forward is what forced every downstream step to re-hash a whole reproduced
 * item list to use it (the Q1X-F6 defect, #551). The id costs the same 32 bytes
 * of thread state and buys the door instead.
 *
 * **Why `WitnessAnchor` has a second component.** §3's transaction-id preimage
 * is the body alone, so the id does not commit the witness set: a prover may
 * hand over the genuine body followed by any trailing `witness_set_hash` it
 * likes and the bytes still re-derive to the committed id. The anchored hash has
 * to arrive from somewhere the prover does not choose, and thread state is the
 * only such place. Fields 0–5 use `BodyAnchor`; fields 6–8 use `WitnessAnchor`.
 * The door refuses the mismatch in both directions, so the choice is not a
 * stylistic one — see {@link nativeTxAnchorV1ForField}.
 */

import { Data } from "@lucid-evolution/lucid";

import { H32Schema } from "@/common.js";
import {
  type FieldCarriageV1,
  FieldCarriageV1Schema,
} from "@/native-tx-field-access-v1.js";

import {
  type NativeTxWitnessSetCompact,
  NativeTxWitnessSetCompactSchema,
} from "./native.js";

// ---------------------------------------------------------------------------
// §2.5's positional field table
// ---------------------------------------------------------------------------

/**
 * §2.5's field indices, named. The on-chain constants live in
 * `field-opening-v1.ak`; these are their twins, and a builder names one of them
 * rather than writing a bare integer — §4 removed field-index domain
 * separation, so fields 0/1 and 3/4 commit identically for identical items and
 * the index is the *only* thing distinguishing a reference-input opening from a
 * spend-input one.
 */
export const MIDGARD_FIELD_INDEX_V1 = Object.freeze({
  spendInputs: 0,
  referenceInputs: 1,
  outputs: 2,
  requiredObservers: 3,
  requiredSigners: 4,
  mint: 5,
  scriptWitnesses: 6,
  addressWitnesses: 7,
  redeemers: 8,
});

/**
 * §2.5's split point: fields 0–5 are the body's, 6–8 the witness set's. Named
 * off the table rather than restated as its own literal, so the boundary and the
 * indices cannot drift apart — the same construction the Aiken module uses.
 */
export const MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX_V1 =
  MIDGARD_FIELD_INDEX_V1.scriptWitnesses;

/** Whether `fieldIndex` lives in the witness set (§2.5 fields 6–8). */
export const isMidgardWitnessSetFieldV1 = (fieldIndex: number): boolean =>
  fieldIndex >= MIDGARD_FIRST_WITNESS_SET_FIELD_INDEX_V1;

// ---------------------------------------------------------------------------
// NativeTxAnchorV1 — what a family's step-01 writes into thread state
// ---------------------------------------------------------------------------

/** Fields 0–5. 32 bytes of thread state. */
export const BodyAnchorV1Schema = Data.Object({
  tx_id: H32Schema,
});
export type BodyAnchorV1 = Data.Static<typeof BodyAnchorV1Schema>;
export const BodyAnchorV1 = BodyAnchorV1Schema as unknown as BodyAnchorV1;

/**
 * Fields 6–8. 64 bytes of thread state — the same width the retired
 * per-collection-hash idiom cost, spent on a value that covers all three
 * witness-set fields instead of one.
 */
export const WitnessAnchorV1Schema = Data.Object({
  tx_id: H32Schema,
  witness_set_hash: H32Schema,
});
export type WitnessAnchorV1 = Data.Static<typeof WitnessAnchorV1Schema>;
export const WitnessAnchorV1 =
  WitnessAnchorV1Schema as unknown as WitnessAnchorV1;

/**
 * `midgard/fraud_proofs/field_opening_v1.NativeTxAnchorV1` — what a family's own
 * step-01 must put in thread state for a downstream step to re-open a field.
 *
 * Constructor order is wire format: `BodyAnchor` 0, `WitnessAnchor` 1.
 */
export const NativeTxAnchorV1Schema = Data.Enum([
  Data.Object({ BodyAnchor: BodyAnchorV1Schema }),
  Data.Object({ WitnessAnchor: WitnessAnchorV1Schema }),
]);
export type NativeTxAnchorV1 = Data.Static<typeof NativeTxAnchorV1Schema>;
export const NativeTxAnchorV1 =
  NativeTxAnchorV1Schema as unknown as NativeTxAnchorV1;

// ---------------------------------------------------------------------------
// FieldOpeningV1 — what a downstream step's redeemer carries
// ---------------------------------------------------------------------------

/**
 * Fields 0–5. No witness set is consulted, so none is carried — a step reading
 * the body cannot be handed one to ignore.
 */
export const BodyFieldOpeningV1Schema = Data.Object({
  native_tx_compact_cbor: Data.Bytes(),
  carriage: FieldCarriageV1Schema,
});
export type BodyFieldOpeningV1 = Data.Static<typeof BodyFieldOpeningV1Schema>;
export const BodyFieldOpeningV1 =
  BodyFieldOpeningV1Schema as unknown as BodyFieldOpeningV1;

/**
 * Fields 6–8. Both `witness_set` and `native_tx_compact_cbor`'s trailing
 * `witness_set_hash` are unauthenticated on arrival — §3's id preimage is the
 * body alone — and are checked against the thread's `WitnessAnchor` before
 * anything is read.
 */
export const WitnessFieldOpeningV1Schema = Data.Object({
  native_tx_compact_cbor: Data.Bytes(),
  witness_set: NativeTxWitnessSetCompactSchema,
  carriage: FieldCarriageV1Schema,
});
export type WitnessFieldOpeningV1 = Data.Static<
  typeof WitnessFieldOpeningV1Schema
>;
export const WitnessFieldOpeningV1 =
  WitnessFieldOpeningV1Schema as unknown as WitnessFieldOpeningV1;

/**
 * `midgard/fraud_proofs/field_opening_v1.FieldOpeningV1` — everything a
 * downstream step needs to re-open one field, and nothing it does not.
 *
 * Constructor order is wire format: `BodyFieldOpening` 0, `WitnessFieldOpening`
 * 1.
 */
export const FieldOpeningV1Schema = Data.Enum([
  Data.Object({ BodyFieldOpening: BodyFieldOpeningV1Schema }),
  Data.Object({ WitnessFieldOpening: WitnessFieldOpeningV1Schema }),
]);
export type FieldOpeningV1 = Data.Static<typeof FieldOpeningV1Schema>;
export const FieldOpeningV1 = FieldOpeningV1Schema as unknown as FieldOpeningV1;

// ---------------------------------------------------------------------------
// Constructors — the only supported way to build either shape
// ---------------------------------------------------------------------------

/**
 * The error a builder gets when it asks for a shape the door would refuse.
 *
 * Refusing here rather than at submission is the whole point: every condition
 * below is one the on-chain door aborts on unconditionally (§7.3
 * abort-never-clamp), and a builder that discovers it from a `Spend[0] the
 * validator crashed` trace has already paid for a transaction.
 */
export class MidgardFieldOpeningError extends Error {
  override readonly name = "MidgardFieldOpeningError";

  constructor(
    message: string,
    readonly detail?: string,
  ) {
    super(detail === undefined ? message : `${message} (${detail})`);
  }
}

const exactFieldIndex = (fieldIndex: number): number => {
  if (
    !Number.isSafeInteger(fieldIndex) ||
    fieldIndex < 0 ||
    fieldIndex > MIDGARD_FIELD_INDEX_V1.redeemers
  ) {
    throw new MidgardFieldOpeningError(
      "§2.5 names exactly nine committed fields",
      `field_index=${fieldIndex}`,
    );
  }
  return fieldIndex;
};

/**
 * The §2.5 anchor a step-01 must write for a thread whose later steps read
 * `fieldIndex`.
 *
 * The arm is a function of the field, not a caller's preference:
 * `anchored_native_tx` `expect`s `BodyAnchor` for a body opening and
 * `WitnessAnchor` for a witness one, so a thread anchored under the wrong arm
 * cannot advance at all. Deriving it here means a family names its field and
 * gets the arm, instead of choosing one and being right by inspection.
 *
 * `witnessSetHash` is required for fields 6–8 and refused for 0–5 — the door
 * carries no field for it on the body arm, so supplying one would be a value
 * that silently does nothing.
 */
export const nativeTxAnchorV1ForField = ({
  fieldIndex,
  txId,
  witnessSetHash,
}: {
  readonly fieldIndex: number;
  readonly txId: string;
  readonly witnessSetHash?: string;
}): NativeTxAnchorV1 => {
  const field = exactFieldIndex(fieldIndex);
  if (isMidgardWitnessSetFieldV1(field)) {
    if (witnessSetHash === undefined) {
      throw new MidgardFieldOpeningError(
        "a witness-set field (§2.5 6–8) anchors on `WitnessAnchor`, which needs the block-committed `witness_set_hash`",
        `field_index=${field}`,
      );
    }
    return { WitnessAnchor: { tx_id: txId, witness_set_hash: witnessSetHash } };
  }
  if (witnessSetHash !== undefined) {
    throw new MidgardFieldOpeningError(
      "a body field (§2.5 0–5) anchors on `BodyAnchor`, which carries no `witness_set_hash`",
      `field_index=${field}`,
    );
  }
  return { BodyAnchor: { tx_id: txId } };
};

/**
 * The redeemer-side opening for one field, with the prover's chosen §8 carriage.
 *
 * Like the anchor, the arm is derived from the field rather than chosen:
 * `field_pairs_with` refuses a body opening read at a witness index (the door
 * would otherwise be handed the module's internal placeholder witness set as if
 * it were the transaction's) and a witness opening read at a body index (where
 * it would be silently ignored).
 *
 * **Tier 3 is refused for fields 6–8** (`docs/spec/midgard-tx.md` §8.3 erratum
 * E2 limit 3). A §8.6 certificate is minted against a `witness_set_hash` taken
 * off the tail of the *minter's own* redeemer, and §3's id preimage does not
 * reach that tail, so for a witness-set field the certificate binds the preimage
 * to nothing the disputing thread anchored.
 * `field_opening_v1.carriage_reaches_the_anchor` refuses it on-chain,
 * unconditionally.
 *
 * **This refusal is hardening, not the repair.** It constrains honest builders
 * and does nothing whatever to an adversary, who does not build transactions
 * through this module. The repair is to fold the field commitment into the §8.6
 * asset name so a certificate token cannot be borrowed for a preimage the named
 * transaction never committed; that changes a frozen wire format and a landed
 * minting policy, and it is
 * [#606](https://github.com/Anastasia-Labs/midgard/issues/606)'s (deferred there
 * from #604 by owner ruling, 2026-08-14, because the change is a blueprint
 * regeneration and #579's single regeneration was spent). Nothing here may be
 * cited as closing E2 limit 3.
 */
export const fieldOpeningV1ForField = ({
  fieldIndex,
  nativeTxCompactCbor,
  carriage,
  witnessSet,
}: {
  readonly fieldIndex: number;
  readonly nativeTxCompactCbor: string;
  readonly carriage: FieldCarriageV1;
  readonly witnessSet?: NativeTxWitnessSetCompact;
}): FieldOpeningV1 => {
  const field = exactFieldIndex(fieldIndex);
  if (isMidgardWitnessSetFieldV1(field)) {
    if (witnessSet === undefined) {
      throw new MidgardFieldOpeningError(
        "a witness-set field (§2.5 6–8) opens on `WitnessFieldOpening`, which carries the transaction's `NativeTxWitnessSetCompact`",
        `field_index=${field}`,
      );
    }
    if ("Certified" in carriage) {
      throw new MidgardFieldOpeningError(
        "§8.3 erratum E2 limit 3: a witness-set field may not be carried under tier 3 — the §8.6 certificate binds it to nothing the thread anchored, and `carriage_reaches_the_anchor` refuses it on-chain",
        `field_index=${field}`,
      );
    }
    return {
      WitnessFieldOpening: {
        native_tx_compact_cbor: nativeTxCompactCbor,
        witness_set: witnessSet,
        carriage,
      },
    };
  }
  if (witnessSet !== undefined) {
    throw new MidgardFieldOpeningError(
      "a body field (§2.5 0–5) opens on `BodyFieldOpening`, which consults no witness set",
      `field_index=${field}`,
    );
  }
  return {
    BodyFieldOpening: {
      native_tx_compact_cbor: nativeTxCompactCbor,
      carriage,
    },
  };
};
