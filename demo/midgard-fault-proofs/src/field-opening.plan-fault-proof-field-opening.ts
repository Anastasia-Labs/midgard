import {
  computeHash32,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxCompact,
  encodeMidgardFieldPreimage,
  encodeMidgardNativeTxWitnessSetCompact,
  type MidgardFieldCarriagePlan,
  midgardFieldCommitment,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core";
import { decodeMidgardForcedTxCompact } from "@al-ft/midgard-core/codec/forced";
import {
  isMidgardWitnessSetField,
  MIDGARD_FIELD_INDEX,
  type NativeTxWitnessSetCompact,
} from "@al-ft/midgard-sdk";

import { requireRecord } from "./json-file.js";

/**
 * The `--native-tx-compact` file every re-derived step-02-class command takes:
 * the §2.5 compact structure of the transaction its thread is disputing.
 *
 * It is a file of its own rather than a key added to each family's existing
 * preimage file because it is a different piece of material — the disputed
 * *transaction*, not one of its fields — and every family needs exactly the one
 * shape. Nothing is trusted about it: the bytes are checked against the anchor
 * the on-chain thread datum carries before any redeemer is built.
 */
export const parseNativeTxCompactCbor = (
  value: unknown,
  label: string,
): string => {
  const record = requireRecord(value, label);
  const cbor = record.nativeTxCompactCbor;
  if (typeof cbor !== "string" || !/^([0-9a-fA-F]{2})+$/u.test(cbor)) {
    throw new Error(
      `${label}.nativeTxCompactCbor must be a hexadecimal string.`,
    );
  }
  return cbor.toLowerCase();
};

/**
 * One field of one disputed transaction, planned but not yet placed in a
 * transaction.
 *
 * A plan is deliberately separate from the opening it becomes: the tier decides
 * whether anything has to be published *before* the step transaction can be
 * built, and a builder that discovered that while assembling its redeemer would
 * have no way to act on it.
 */
export type FaultProofFieldOpeningPlan = {
  readonly sourceKind: 0n | 1n;
  readonly fieldIndex: number;
  /** The §2.5 anchor these bytes were checked against. */
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  /** §5.1's envelope over the canonical item bytes. */
  readonly preimage: Buffer;
  readonly itemCount: number;
  /** §4's flat commitment — the value the door will re-derive. */
  readonly commitment: string;
  readonly plan: MidgardFieldCarriagePlan;
  /** Present for §2.5 fields 6–8 only, where the door reads the witness set. */
  readonly witnessSet?: NativeTxWitnessSetCompact;
  /**
   * `blake2b_256` of the compact witness set above, present with it. It is the
   * value `WitnessAnchor` carries and the one the compact structure names, both
   * of which this plan has already checked it against.
   */
  readonly witnessSetHash?: string;
};

const requireHash32 = (value: string, label: string): string => {
  const normalized = value.toLowerCase();
  if (!/^[0-9a-f]{64}$/u.test(normalized)) {
    throw new Error(`${label} must be a 32-byte lowercase hexadecimal hash.`);
  }
  return normalized;
};

/**
 * §4's committed hash for one field, read **positionally** off the disputed
 * transaction's own compact structures.
 *
 * The twin of `native_tx_field_access_v1.field_commitment_at`, and the reason a
 * builder can check its preimage against the right slot: a flat hash carries no
 * field index, so the index has to come from the structure rather than from the
 * hash.
 */
const committedFieldCommitment = ({
  fieldIndex,
  compact,
  witnessSet,
}: {
  readonly fieldIndex: number;
  readonly compact:
    | ReturnType<typeof decodeMidgardNativeTxCompact>
    | ReturnType<typeof decodeMidgardForcedTxCompact>;
  readonly witnessSet?: NativeTxWitnessSetCompact;
}): string => {
  if (isMidgardWitnessSetField(fieldIndex)) {
    if (witnessSet === undefined) {
      throw new Error(
        `§2.5 field ${fieldIndex.toString()} lives in the witness set, whose compact structure was not supplied.`,
      );
    }
    // §2.5's witness-set order, which is *not* the compact record's declaration
    // order: field 6 is `script_tx_wits`, 7 `addr_tx_wits`, 8 `redeemer_tx_wits`.
    // Naming the indices off the shared table rather than writing 6/7/8 here is
    // what keeps the two spellings from drifting.
    const byFieldIndex: Readonly<Record<number, string>> = {
      [MIDGARD_FIELD_INDEX.scriptWitnesses]: witnessSet.script_tx_wits_hash,
      [MIDGARD_FIELD_INDEX.addressWitnesses]: witnessSet.addr_tx_wits_hash,
      [MIDGARD_FIELD_INDEX.redeemers]: witnessSet.redeemer_tx_wits_hash,
    };
    const hash = byFieldIndex[fieldIndex];
    if (hash === undefined) {
      throw new Error(
        `§2.5 names no witness-set field ${fieldIndex.toString()}.`,
      );
    }
    return hash.toLowerCase();
  }
  const body = compact.transactionBody;
  const hashes = [
    body.spendInputsHash,
    body.referenceInputsHash,
    body.outputsHash,
    body.requiredObserversHash,
    body.requiredSignersHash,
    body.mintHash,
  ];
  const hash = hashes[fieldIndex];
  if (hash === undefined) {
    throw new Error(`§2.5 names no body field ${fieldIndex.toString()}.`);
  }
  return Buffer.from(hash).toString("hex");
};

/**
 * Plans one field opening, after checking every pairing the door checks.
 *
 * `anchorTxId` is read from the **on-chain** thread datum by the caller, never
 * from the material handed to the submitter — that is what makes the compact
 * bytes below the disputed transaction's rather than the prover's.
 *
 * `itemCbors` are the field's canonical §5.3 item bytes, in committed order.
 * They are the same complete list the retired `..._preimage` redeemer argument
 * carried; what changed is that they are now enveloped by §5.1 and travel under
 * a carriage tier instead of being reproduced as a typed list the validator
 * re-hashed.
 */
export const planFaultProofFieldOpening = ({
  anchorSourceKind,
  fieldIndex,
  anchorTxId,
  nativeTxCompactCbor,
  itemCbors,
  owner,
  publish = false,
  witnessSet,
  anchorWitnessSetHash,
  label,
}: {
  readonly fieldIndex: number;
  readonly anchorTxId: string;
  readonly anchorSourceKind: 0n | 1n;
  readonly nativeTxCompactCbor: string;
  readonly itemCbors: readonly Uint8Array[];
  /** §8.6 min-Ada reclaim authority; no consuming step reads it. */
  readonly owner: string;
  readonly publish?: boolean;
  readonly witnessSet?: NativeTxWitnessSetCompact;
  /**
   * The `witness_set_hash` thread state anchored (`WitnessAnchor`'s second
   * component), for §2.5 fields 6–8.
   *
   * Optional in the signature and **required** for a witness-set field, because
   * it is the one check the transaction id cannot make for itself: §3's id
   * preimage is the body alone, so without it a prover supplies the genuine body
   * — which re-derives to the anchored id — followed by a `witness_set_hash` of
   * its own choosing, and then "authenticates" any witness set against that.
   * `anchored_native_tx` refuses exactly this, and so does the check below.
   */
  readonly anchorWitnessSetHash?: string;
  readonly label: string;
}): FaultProofFieldOpeningPlan => {
  const anchoredTxId = requireHash32(anchorTxId, `${label} anchored tx id`);
  const compactCbor = nativeTxCompactCbor.toLowerCase();
  if (!/^([0-9a-f]{2})+$/u.test(compactCbor)) {
    throw new Error(`${label} compact transaction CBOR must be hexadecimal.`);
  }
  const compactBytes = Buffer.from(compactCbor, "hex");
  // 1. `verify_native_tx_compact_cbor_v1`: the bytes the redeemer will carry
  //    must be the transaction the thread anchored, not a second transaction
  //    with a convenient field.
  const compact = (
    anchorSourceKind === 1n
      ? decodeMidgardForcedTxCompact
      : decodeMidgardNativeTxCompact
  )(compactBytes);
  // The id is derived from the compact structure alone (§3), which is why the
  // door can make this check from the same bytes the prover supplies.
  const derivedTxId = computeMidgardNativeTxId(compact).toString("hex");
  if (derivedTxId !== anchoredTxId) {
    throw new Error(
      `${label} compact transaction CBOR re-derives to ${derivedTxId}, which is not the anchored transaction id ${anchoredTxId}.`,
    );
  }

  let witnessSetHash: string | undefined;
  if (isMidgardWitnessSetField(fieldIndex)) {
    if (witnessSet === undefined) {
      throw new Error(
        `${label} opens §2.5 field ${fieldIndex.toString()}, which lives in the witness set, so the transaction's compact witness set must be supplied.`,
      );
    }
    // 2. `authenticated_field_view`'s witness-set check: the supplied witness
    //    set must be the one the compact structure names. §3's id preimage is
    //    the body alone, so nothing above this point covers it.
    witnessSetHash = computeHash32(
      encodeMidgardNativeTxWitnessSetCompact({
        addrTxWitsHash: Buffer.from(witnessSet.addr_tx_wits_hash, "hex"),
        scriptTxWitsHash: Buffer.from(witnessSet.script_tx_wits_hash, "hex"),
        redeemerTxWitsHash: Buffer.from(
          witnessSet.redeemer_tx_wits_hash,
          "hex",
        ),
      }),
    ).toString("hex");
    const committedWitnessSetHash = Buffer.from(
      compact.transactionWitnessSetHash,
    ).toString("hex");
    if (witnessSetHash !== committedWitnessSetHash) {
      throw new Error(
        `${label} compact witness set hashes to ${witnessSetHash}, which is not the ${committedWitnessSetHash} the compact transaction commits.`,
      );
    }
    // 3. `anchored_native_tx`'s second check, and the whole reason
    //    `WitnessAnchor` carries a second component: the compact structure's
    //    trailing `witness_set_hash` is outside §3's id preimage, so it is the
    //    prover's until thread state says otherwise.
    if (anchorWitnessSetHash === undefined) {
      throw new Error(
        `${label} opens §2.5 field ${fieldIndex.toString()}, which anchors on \`WitnessAnchor\`, so the thread's committed witness_set_hash must be supplied.`,
      );
    }
    if (committedWitnessSetHash !== anchorWitnessSetHash.toLowerCase()) {
      throw new Error(
        `${label} compact transaction names witness_set_hash ${committedWitnessSetHash}, which is not the ${anchorWitnessSetHash.toLowerCase()} the thread anchored.`,
      );
    }
  } else if (anchorWitnessSetHash !== undefined) {
    throw new Error(
      `${label} opens §2.5 field ${fieldIndex.toString()}, a body field, which anchors on \`BodyAnchor\` and carries no witness_set_hash.`,
    );
  }

  const preimage = encodeMidgardFieldPreimage(
    itemCbors.map((item) => Buffer.from(item)),
  );
  const commitment = midgardFieldCommitment(preimage).toString("hex");
  // 3. `field_commitment_at`: these items are this transaction's field
  //    `fieldIndex`, not some other field of the same transaction. Under §4 the
  //    two are indistinguishable by hash alone — fields 0/1 and 3/4 commit
  //    identically for identical items — so the slot has to be named.
  const committed = committedFieldCommitment({
    fieldIndex,
    compact,
    ...(witnessSet === undefined ? {} : { witnessSet }),
  });
  if (commitment !== committed) {
    throw new Error(
      `${label} §5.1 preimage commits to ${commitment}, which is not the ${committed} the disputed transaction commits at §2.5 field ${fieldIndex.toString()}.`,
    );
  }

  return {
    fieldIndex,
    nativeTxId: anchoredTxId,
    sourceKind: anchorSourceKind,
    nativeTxCompactCbor: compactCbor,
    preimage,
    itemCount: itemCbors.length,
    commitment,
    plan: planMidgardFieldCarriage({
      owner: Buffer.from(owner, "hex"),
      txId: Buffer.from(anchoredTxId, "hex"),
      fieldIndex,
      preimage,
      publish,
    }),
    ...(witnessSet === undefined ? {} : { witnessSet }),
    ...(witnessSetHash === undefined ? {} : { witnessSetHash }),
  };
};
