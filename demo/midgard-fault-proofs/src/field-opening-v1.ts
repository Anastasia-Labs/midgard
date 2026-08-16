/**
 * The builder-side half of the §8.8 field-opening door — what a rebound
 * fraud-proof step submitter needs in order to hand a validator one of the nine
 * committed fields of the transaction its computation thread is disputing.
 *
 * `@al-ft/midgard-sdk`'s `fraud-proof/field-opening-v1.ts` owns the *wire*
 * shapes (`NativeTxAnchorV1`, `FieldOpeningV1`, the §2.5 field table). This
 * module owns the *transaction* half: deriving the §5.1 preimage from canonical
 * item bytes, planning its §8 carriage, publishing that carriage when the tier
 * calls for it, and resolving the positional reference-input indices against the
 * transaction that will actually run the door.
 *
 * **Everything here refuses early what the door would abort on.** Each of the
 * checks below is one the on-chain door applies unconditionally (§7.3
 * abort-never-clamp), and a builder that discovers it from a
 * `Spend[0] the validator crashed` trace has already paid for a transaction and
 * — for a fault proof — may have burned an unrepeatable computation thread:
 *
 *   * the compact bytes re-derive to the id thread state anchored
 *     (`verify_native_tx_compact_cbor_v1`);
 *   * the §5.1 preimage hashes to the commitment the compact structure carries
 *     *at this field index* (`field_commitment_at`) — §4 removed field-index
 *     domain separation, so a preimage that opens the wrong slot is otherwise a
 *     perfectly well-formed value;
 *   * a witness-set field (§2.5 6–8) carries the transaction's own compact
 *     witness set, whose hash is the anchored `witness_set_hash`;
 *   * the §2.5 half pairs with the field (`field_pairs_with`, applied by the
 *     SDK's `fieldOpeningV1ForField`; the former fields-6–8 tier-3 refusal
 *     lifted with #606's welded-hash repair).
 *
 * **The tier is never a caller's argument.** §8.4 partitions on the preimage's
 * own length, and `planMidgardFieldCarriageV1` is the only thing here that
 * decides it. `publish` is the single choice §8 leaves open — it demotes a
 * tier-1 preimage to a tier-2 publication so the step's own redeemer does not
 * have to carry the bytes — and it changes which transaction pays, never what
 * the door authenticates.
 */

import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxCompactV1,
  encodeMidgardFieldPreimageV1,
  encodeMidgardNativeTxWitnessSetCompactV1,
  layOutMidgardFieldCarriageV1,
  type MidgardFieldCarriagePlanV1,
  midgardFieldCommitmentV1,
  planMidgardFieldCarriageV1,
} from "@al-ft/midgard-core";
import {
  buildUnsignedFieldPreimagePublicationV1Program,
  type FieldCarriageV1,
  type FieldOpeningV1,
  fieldOpeningV1ForField,
  fieldPreimagePublicationDatumCborV1,
  isMidgardWitnessSetFieldV1,
  MIDGARD_FIELD_INDEX_V1,
  type NativeTxWitnessSetCompact,
  resolveCertificateReferenceIndexV1,
  resolveChunkReferenceIndicesV1,
} from "@al-ft/midgard-sdk";
import {
  coreToTxOutput,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { requireRecord } from "./json-file.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  type ResolvedProverSigner,
} from "./runtime.js";

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
export const parseNativeTxCompactCborV1 = (
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
export type FaultProofFieldOpeningPlanV1 = {
  readonly fieldIndex: number;
  /** The §2.5 anchor these bytes were checked against. */
  readonly nativeTxId: string;
  readonly nativeTxCompactCbor: string;
  /** §5.1's envelope over the canonical item bytes. */
  readonly preimage: Buffer;
  readonly itemCount: number;
  /** §4's flat commitment — the value the door will re-derive. */
  readonly commitment: string;
  readonly plan: MidgardFieldCarriagePlanV1;
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
const committedFieldCommitmentV1 = ({
  fieldIndex,
  compact,
  witnessSet,
}: {
  readonly fieldIndex: number;
  readonly compact: ReturnType<typeof decodeMidgardNativeTxCompactV1>;
  readonly witnessSet?: NativeTxWitnessSetCompact;
}): string => {
  if (isMidgardWitnessSetFieldV1(fieldIndex)) {
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
      [MIDGARD_FIELD_INDEX_V1.scriptWitnesses]: witnessSet.script_tx_wits_hash,
      [MIDGARD_FIELD_INDEX_V1.addressWitnesses]: witnessSet.addr_tx_wits_hash,
      [MIDGARD_FIELD_INDEX_V1.redeemers]: witnessSet.redeemer_tx_wits_hash,
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
export const planFaultProofFieldOpeningV1 = ({
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
}): FaultProofFieldOpeningPlanV1 => {
  const anchoredTxId = requireHash32(anchorTxId, `${label} anchored tx id`);
  const compactCbor = nativeTxCompactCbor.toLowerCase();
  if (!/^([0-9a-f]{2})+$/u.test(compactCbor)) {
    throw new Error(`${label} compact transaction CBOR must be hexadecimal.`);
  }
  const compactBytes = Buffer.from(compactCbor, "hex");
  // 1. `verify_native_tx_compact_cbor_v1`: the bytes the redeemer will carry
  //    must be the transaction the thread anchored, not a second transaction
  //    with a convenient field.
  const compact = decodeMidgardNativeTxCompactV1(compactBytes);
  // The id is derived from the compact structure alone (§3), which is why the
  // door can make this check from the same bytes the prover supplies.
  const derivedTxId = computeMidgardNativeTxIdV1(compact).toString("hex");
  if (derivedTxId !== anchoredTxId) {
    throw new Error(
      `${label} compact transaction CBOR re-derives to ${derivedTxId}, which is not the anchored transaction id ${anchoredTxId}.`,
    );
  }

  let witnessSetHash: string | undefined;
  if (isMidgardWitnessSetFieldV1(fieldIndex)) {
    if (witnessSet === undefined) {
      throw new Error(
        `${label} opens §2.5 field ${fieldIndex.toString()}, which lives in the witness set, so the transaction's compact witness set must be supplied.`,
      );
    }
    // 2. `authenticated_field_view`'s witness-set check: the supplied witness
    //    set must be the one the compact structure names. §3's id preimage is
    //    the body alone, so nothing above this point covers it.
    witnessSetHash = computeHash32(
      encodeMidgardNativeTxWitnessSetCompactV1({
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

  const preimage = encodeMidgardFieldPreimageV1(
    itemCbors.map((item) => Buffer.from(item)),
  );
  const commitment = midgardFieldCommitmentV1(preimage).toString("hex");
  // 3. `field_commitment_at`: these items are this transaction's field
  //    `fieldIndex`, not some other field of the same transaction. Under §4 the
  //    two are indistinguishable by hash alone — fields 0/1 and 3/4 commit
  //    identically for identical items — so the slot has to be named.
  const committed = committedFieldCommitmentV1({
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
    nativeTxCompactCbor: compactCbor,
    preimage,
    itemCount: itemCbors.length,
    commitment,
    plan: planMidgardFieldCarriageV1({
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

/**
 * The §8 carriage as the step's redeemer must spell it, with every positional
 * index resolved by **content** against the door-running transaction's complete
 * reference-input set (§8.7).
 *
 * Tier 1 indexes nothing, so `referenceInputs` is ignored there and a caller
 * that has none may omit it. Above tier 1 the set must be the transaction's
 * whole reference-input list rather than only the carriage: positional indices
 * count into the ledger's canonically-sorted list, and a step references more
 * than its carriage.
 */
export const faultProofFieldCarriageV1 = ({
  planned,
  referenceInputs = [],
  certificatePolicyId,
  label,
}: {
  readonly planned: FaultProofFieldOpeningPlanV1;
  readonly referenceInputs?: readonly UTxO[];
  readonly certificatePolicyId?: string;
  readonly label: string;
}): FieldCarriageV1 => {
  const { plan } = planned;
  if (plan.tier === "Inline") {
    if (plan.inlinePreimage === null) {
      throw new Error(`${label} tier-1 plan carries no preimage.`);
    }
    return { Inline: { preimage: plan.inlinePreimage.toString("hex") } };
  }
  const chunkIndices = resolveChunkReferenceIndicesV1({
    plan,
    referenceInputs,
  });
  if (plan.tier === "RawUtxo") {
    const refInputIndex = chunkIndices[0];
    if (refInputIndex === undefined || chunkIndices.length !== 1) {
      throw new Error(
        `${label} tier-2 carriage must resolve exactly one publication.`,
      );
    }
    return { RawUtxo: { ref_input_index: BigInt(refInputIndex) } };
  }
  if (plan.certificate === null) {
    throw new Error(`${label} tier-3 plan carries no §8.6 certificate.`);
  }
  if (certificatePolicyId === undefined) {
    throw new Error(
      `${label} is tier-3 carriage, which names its §8.6 manifest by policy id, but none was supplied.`,
    );
  }
  return {
    Certified: {
      cert_ref_input_index: BigInt(
        resolveCertificateReferenceIndexV1({
          certificatePolicyId,
          txIdHex: plan.txId.toString("hex"),
          fieldIndex: plan.fieldIndex,
          fieldHashHex: plan.commitment.toString("hex"),
          referenceInputs,
          label,
        }),
      ),
      chunk_ref_input_indices: chunkIndices.map((index) => BigInt(index)),
    },
  };
};

/**
 * The whole opening a rebound step's redeemer carries: the compact bytes, the
 * §2.5-derived arm, and the resolved carriage.
 *
 * The arm is `fieldOpeningV1ForField`'s to choose, not this module's — a family
 * names its field and gets `BodyFieldOpening` or `WitnessFieldOpening`, which is
 * what keeps the §2.5 pairing off the caller's list of things to be right about.
 */
export const faultProofFieldOpeningV1 = ({
  planned,
  referenceInputs = [],
  certificatePolicyId,
  label,
}: {
  readonly planned: FaultProofFieldOpeningPlanV1;
  readonly referenceInputs?: readonly UTxO[];
  readonly certificatePolicyId?: string;
  readonly label: string;
}): FieldOpeningV1 =>
  fieldOpeningV1ForField({
    fieldIndex: planned.fieldIndex,
    nativeTxCompactCbor: planned.nativeTxCompactCbor,
    carriage: faultProofFieldCarriageV1({
      planned,
      referenceInputs,
      ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
      label,
    }),
    ...(planned.witnessSet === undefined
      ? {}
      : { witnessSet: planned.witnessSet }),
  });

/**
 * Publishes whatever a plan requires to exist on-chain before the step
 * transaction can reference it, and returns the confirmed carriage UTxOs.
 *
 * Empty under tier 1 — the preimage rides in the step's own redeemer and
 * nothing is published. Under tier 2 this is the whole of the carriage; under
 * tier 3 it is the chunks, and the §8.6 certificate is a separate mint the
 * caller supplies (`buildUnsignedFieldPreimageCertificationV1Program`), because
 * it needs the certificate minting policy from the deployment and a fault-proof
 * step builder holds no such role.
 *
 * §8.7: publication is permissionless and content-addressed, so a chunk that
 * already exists at the publisher's address is reused rather than republished.
 */
export const publishFaultProofFieldCarriageV1 = async ({
  lucid,
  signer,
  planned,
  publisherAddress,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly planned: FaultProofFieldOpeningPlanV1;
  readonly publisherAddress: string;
  readonly label: string;
}): Promise<readonly UTxO[]> => {
  const publications = planned.plan.publications;
  if (publications.length === 0) {
    return [];
  }
  signer.selectWallet(lucid);
  const published: UTxO[] = [];
  for (const publication of publications) {
    const datumCbor = fieldPreimagePublicationDatumCborV1(publication.bytes);
    // Compared byte-for-byte against the datum, exactly as
    // `resolveChunkReferenceIndicesV1` does when it locates the same UTxO at the
    // door. Matching on anything looser here would reuse a publication the
    // resolver then fails to find.
    const existing = (await lucid.utxosAt(publisherAddress)).find(
      (utxo) => utxo.datum === datumCbor,
    );
    if (existing !== undefined) {
      published.push(existing);
      continue;
    }
    const unsigned = await Effect.runPromise(
      buildUnsignedFieldPreimagePublicationV1Program(lucid, {
        publication: {
          chunkIndex: publication.chunkIndex,
          datumCbor,
          byteLength: publication.bytes.length,
          digestHex: publication.digest.toString("hex"),
        },
        publisherAddress,
      }),
    );
    const signed = await unsigned.sign.withWallet().complete();
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    const outputs = signed.toTransaction().body().outputs();
    let outputIndex: number | undefined;
    for (let index = 0; index < outputs.len(); index += 1) {
      if (
        outputIndex === undefined &&
        coreToTxOutput(outputs.get(index)).datum === datumCbor
      ) {
        outputIndex = index;
      }
    }
    if (outputIndex === undefined) {
      throw new Error(
        `${label} §8 carriage publication did not produce an output carrying chunk ${publication.chunkIndex.toString()}.`,
      );
    }
    published.push(
      await fetchUtxoByOutRef({
        lucid,
        outRef: { txHash, outputIndex },
        label: `${label} §8 carriage publication`,
      }),
    );
  }
  return published;
};

/**
 * The reference inputs a step transaction must read for one planned opening, in
 * §8.4 order.
 *
 * Under tier 3 the certificate comes first and the chunks follow, which is the
 * order `layOutMidgardFieldCarriageV1` produces and the order a certificate's
 * digest vector is written in. Deriving the ordering from the layout rather than
 * from a hand-written list is what keeps the two from drifting.
 */
export const faultProofFieldCarriageReferenceOrderV1 = (
  planned: FaultProofFieldOpeningPlanV1,
): readonly number[] =>
  layOutMidgardFieldCarriageV1({ plan: planned.plan }).referenceInputIndices;
