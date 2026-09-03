import {
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialEntry,
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekProgramMaterialSidecar,
  hashMidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  midgardCekProgramMaterialKindTag,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import {
  computeMidgardNativeTxId,
  computeMidgardNativeTxProofCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxProofSource,
} from "@al-ft/midgard-core/codec";
import {
  type MidgardFieldCarriagePlan,
  planMidgardFieldCarriage,
} from "@al-ft/midgard-core/codec/native-tx-carriage-v1";
import {
  encodeMidgardFieldArrayHeader,
  midgardFieldCommitment,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile-v1";
import {
  deriveMidgardTxFieldPreimages,
  validateMidgardConsensusTxCbor,
} from "@al-ft/midgard-core/consensus-validation-v1";
import {
  type Assets,
  CML,
  Data,
  getAddressDetails,
  LucidEvolution,
  type ProtocolParameters,
  TxSignBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  Bech32DeserializationError,
  CredentialSchema,
  HashingError,
  LucidError,
  makeReturn,
  MidgardValidators,
  OutputReference,
  outputReferenceFromUTxO,
  POSIXTimeSchema,
} from "../common.js";
import {
  resolveCertificateReferenceIndex,
  resolveChunkReferenceIndices,
} from "../fraud-proof/field-preimage-carriage-v1.js";
import { HubOracleError } from "../hub-oracle.js";
import { authenticateUTxOs, AuthenticUTxO } from "../internals.js";
import {
  CardanoDatum,
  CardanoDatumSchema,
  CekProgramMaterialDatum,
  CekProgramMaterialDatumSchema,
  NativeTxProofSource,
  TxOrderEventSchema,
} from "../ledger-state.js";
import {
  type FieldCarriage,
  FieldCarriageSchema,
} from "../native-tx-field-access-v1.js";
import { OperatorVerdictSchema } from "../rejection-reason-v1.js";
import { RawRootMembershipProofSchema } from "../transition-trace.js";
import {
  buildCompletedUserEventMintTxProgram,
  encodeUserEventWitnessMintOrBurnRedeemer,
  fetchUserEventUTxOsProgram,
  outputReferenceToPlutusDataCbor,
  prepareUserEventMintContext,
  userEventAuthenticateMintRedeemer,
  UserEventBuildError,
  userEventCborFieldsFromInlineDatum,
  UserEventExtraFields,
  UserEventFetchConfig,
  UserEventMintRedeemerSchema,
} from "./internals.js";

export const TxOrderRefundAddressSchema = Data.Object({
  paymentCredential: CredentialSchema,
  stakeCredential: Data.Nullable(
    Data.Enum([
      Data.Object({
        Inline: Data.Tuple([CredentialSchema]),
      }),
      Data.Object({
        Pointer: Data.Object({
          slotNumber: Data.Integer(),
          transactionIndex: Data.Integer(),
          certificateIndex: Data.Integer(),
        }),
      }),
    ]),
  ),
});
export type TxOrderRefundAddress = Data.Static<
  typeof TxOrderRefundAddressSchema
>;
export const TxOrderRefundAddress =
  TxOrderRefundAddressSchema as unknown as TxOrderRefundAddress;

export const TxOrderDatumSchema = Data.Object({
  event: TxOrderEventSchema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
  refund_address: TxOrderRefundAddressSchema,
  refund_datum: CardanoDatumSchema,
});
export type TxOrderDatum = Data.Static<typeof TxOrderDatumSchema>;
export const TxOrderDatum = TxOrderDatumSchema as unknown as TxOrderDatum;

type PlutusDataSchema = Parameters<typeof Data.Nullable>[0];

const encodeCanonicalPlutusData = <A>(
  value: A,
  schema: PlutusDataSchema,
): Buffer => Buffer.from(Data.to(value as never, schema as never), "hex");

const decodeCanonicalPlutusData = <A>(
  bytes: Uint8Array,
  schema: PlutusDataSchema,
  label: string,
): A => {
  const input = Buffer.from(bytes);
  const decoded = Data.from(input.toString("hex"), schema as never) as A;
  if (!encodeCanonicalPlutusData(decoded, schema).equals(input)) {
    throw new Error(`${label} CBOR must use its exact canonical encoding`);
  }
  return decoded;
};

export const encodeTxOrderDatumCbor = (datum: TxOrderDatum): Buffer =>
  encodeCanonicalPlutusData(datum, TxOrderDatumSchema);

export const decodeTxOrderDatumCbor = (bytes: Uint8Array): TxOrderDatum =>
  decodeCanonicalPlutusData(bytes, TxOrderDatumSchema, "TxOrderDatumV1");

export const decodeCekProgramMaterialDatumCbor = (
  bytes: Uint8Array,
): CekProgramMaterialDatum =>
  decodeCanonicalPlutusData(
    bytes,
    CekProgramMaterialDatumSchema,
    "CekProgramMaterialDatumV1",
  );

export const CEK_SINGLE_PUBLICATION_DATUM_VERSION = 1n;

/** Exact datum ABI for one immutable, reference-only complete CEK graph. */
export const CekSinglePublicationDatumSchema = Data.Object({
  version: Data.Integer(),
  program_envelope_hash: Data.Bytes({ minLength: 32, maxLength: 32 }),
  sidecar_cbor: Data.Bytes(),
});
export type CekSinglePublicationDatum = Data.Static<
  typeof CekSinglePublicationDatumSchema
>;
export const CekSinglePublicationDatum =
  CekSinglePublicationDatumSchema as unknown as CekSinglePublicationDatum;

const assertCekSinglePublicationDatum = (
  datum: CekSinglePublicationDatum,
): void => {
  if (datum.version !== CEK_SINGLE_PUBLICATION_DATUM_VERSION) {
    throw new Error("CEK single-publication datum must use version 1");
  }
};

export const encodeCekSinglePublicationDatumCbor = (
  datum: CekSinglePublicationDatum,
): Buffer => {
  assertCekSinglePublicationDatum(datum);
  const encoded = encodeCanonicalPlutusData(
    datum,
    CekSinglePublicationDatumSchema,
  );
  if (
    encoded.length >
    MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableCompleteItemPublicationDatumBytes
  ) {
    throw new Error(
      "CEK single-publication datum exceeds the reliable complete-item datum envelope",
    );
  }
  return encoded;
};

export const decodeCekSinglePublicationDatumCbor = (
  bytes: Uint8Array,
): CekSinglePublicationDatum => {
  const datum = decodeCanonicalPlutusData(
    bytes,
    CekSinglePublicationDatumSchema,
    "CekSinglePublicationDatumV1",
  ) as CekSinglePublicationDatum;
  assertCekSinglePublicationDatum(datum);
  // Reuse the encoder so decoded data is also bounded by the pinned
  // single-publication datum envelope.
  encodeCekSinglePublicationDatumCbor(datum);
  return Object.freeze({ ...datum });
};

export const TxOrderSpendRedeemerSchema = Data.Object({
  input_index: Data.Integer(),
  output_index: Data.Integer(),
  hub_ref_input_index: Data.Integer(),
  settlement_ref_input_index: Data.Integer(),
  burn_redeemer_index: Data.Integer(),
  membership_proof: RawRootMembershipProofSchema,
  inclusion_proof_script_withdraw_redeemer_index: Data.Integer(),
  validity_override: OperatorVerdictSchema,
});
export type TxOrderSpendRedeemer = Data.Static<
  typeof TxOrderSpendRedeemerSchema
>;
export const TxOrderSpendRedeemer =
  TxOrderSpendRedeemerSchema as unknown as TxOrderSpendRedeemer;

/**
 * The tx-order minting policy's redeemer — `midgard/user_events/tx_order_v1`'s
 * `MintRedeemer` (#594).
 *
 * It wraps the shared user-event mint redeemer rather than widening it, because
 * that type is the deposit, withdrawal and witness policies' redeemer too and
 * none of them has material to carry.
 *
 * `material_carriage` is positional over the order's **non-empty** fields in
 * ascending field index — not `(field_index, carriage)` pairs. §4 removed
 * field-index domain separation, so a supplied index would have to be checked
 * against the slot it claims before it could be used, and the on-chain walk
 * already knows that slot from the nine commitments. The vector must be
 * exhausted exactly: neither a missing nor a spare entry is admitted.
 */
export const TxOrderMintRedeemerSchema = Data.Object({
  event: UserEventMintRedeemerSchema,
  material_carriage: Data.Array(FieldCarriageSchema),
});
export type TxOrderMintRedeemer = Data.Static<typeof TxOrderMintRedeemerSchema>;
export const TxOrderMintRedeemer =
  TxOrderMintRedeemerSchema as unknown as TxOrderMintRedeemer;

export const encodeTxOrderMintRedeemerCbor = (
  redeemer: TxOrderMintRedeemer,
): Buffer => encodeCanonicalPlutusData(redeemer, TxOrderMintRedeemerSchema);

export type TxOrderUTxOV1 = AuthenticUTxO<TxOrderDatum, UserEventExtraFields>;

export const utxosToTxOrderUTxOs = (
  utxos: UTxO[],
  nftPolicy: string,
): Effect.Effect<TxOrderUTxOV1[]> =>
  authenticateUTxOs<TxOrderDatum, UserEventExtraFields>(
    utxos,
    nftPolicy,
    TxOrderDatum,
    (datum, utxo) => {
      decodeTxOrderDatumCbor(Buffer.from(utxo.datum!, "hex"));
      return {
        ...userEventCborFieldsFromInlineDatum(utxo),
        inclusionTime: new Date(Number(datum.inclusion_time)),
      };
    },
  );

export const fetchTxOrderUTxOsProgram = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
): Effect.Effect<TxOrderUTxOV1[], LucidError> =>
  fetchUserEventUTxOsProgram(lucid, config, (utxos: UTxO[]) =>
    utxosToTxOrderUTxOs(utxos, config.eventPolicyId),
  );

export const fetchTxOrderUTxOs = (
  lucid: LucidEvolution,
  config: UserEventFetchConfig,
) => makeReturn(fetchTxOrderUTxOsProgram(lucid, config));

export type SubmitTxOrderReferenceScripts = {
  readonly txOrderMinting: UTxO;
};

export type SubmitTxOrderConfig = {
  /** Exact bounded canonical native-V1 transaction bytes. */
  readonly nativeTxCbor: string;
  /** Reserved while the order's §8 field carriage is prepared. */
  readonly nonceInput: UTxO;
  readonly refundAddress: TxOrderRefundAddress;
  readonly refundDatum?: CardanoDatum;
  readonly lovelace?: bigint;
  readonly referenceScripts?: SubmitTxOrderReferenceScripts;
  /**
   * The predeployed §8 carriage this order references — raw preimage/chunk UTxOs
   * at the creator's own wallet address, plus the §8.6 certificate UTxO for every
   * tier-3 field. They must already exist: reference inputs are resolved against
   * the UTxO set as it stands *before* this transaction, so a field cannot be
   * published and referenced in one go.
   *
   * Whatever is listed here is read by the order transaction and indexed
   * positionally in its mint redeemer, so a stale entry is not free — it shifts
   * every index after it. Pass exactly the carriage
   * {@link planTxOrderMaterialCarriage} says is referenced.
   */
  readonly carriageReferenceInputs?: readonly UTxO[];
  /**
   * The §8.6 certificate minting policy id, needed only when a field's preimage
   * exceeds `K` and is therefore tier 3.
   *
   * It is a config field rather than a `MidgardValidators` role because the
   * certificate validator is not in the frozen blueprint and has no deployment
   * registry entry yet — that is #579's single regeneration event (rider 2 of its
   * scope amendment). It moves into `contracts` with the role.
   */
  readonly fieldPreimageCertificatePolicyId?: string;
  /**
   * Overrides {@link MIDGARD_TX_ORDER_INLINE_CARRIAGE_RESERVE_BYTES}. Lower it
   * to force fields onto predeployed carriage; there is no consensus threshold to
   * violate, only this transaction's own byte budget.
   */
  readonly inlineCarriageReserveBytes?: number;
};

/**
 * One non-empty field of a forced order's material, with the §8 carriage its
 * preimage requires.
 *
 * `plan` comes straight from {@link planMidgardFieldCarriage}, so the tier is
 * §8.4's partition rather than this module's choice, and `publications` is the
 * exact set of raw carriage UTxOs a publisher has to create.
 */
export type TxOrderFieldCarriage = {
  readonly fieldIndex: number;
  readonly fieldName: string;
  /** The §5.1 enveloped field preimage. */
  readonly preimage: Buffer;
  /**
   * The §4 flat commitment over {@link preimage} — `blake2b_256` of the bytes,
   * which is what the §8 carriage plan and the on-chain door authenticate
   * against.
   *
   * Since #585 this is also, necessarily, the hash the compact structure beside
   * it carries: `deriveNativeTxBodyCompact` derives the nine field commitments
   * the same way. {@link deriveTxOrderMaterial} asserts the two equal per field
   * rather than leaving it implied — the equality is the whole point of the
   * reversion, and before #585 it was false.
   */
  readonly commitment: string;
  readonly plan: MidgardFieldCarriagePlan;
};

/**
 * What a forced order binds itself to: the §3 transaction id, the proof-source
 * triple its datum carries, and the §8 carriage of every field with material in
 * it.
 *
 * This replaced `deriveTxOrderFragmentBundleV1`, which produced counted per-item
 * `TxFieldPreimageV1` fragments for the retired publication receipt chain
 * (#587). The nine field preimages are the same bytes either way — what changed
 * is that a field is now committed by one flat hash over the whole preimage
 * (§4), so there are no per-item openings to publish and the unit of carriage is
 * the field, not the item.
 */
export type TxOrderMaterial = {
  readonly transactionId: string;
  readonly transactionCommitment: string;
  readonly source: NativeTxProofSource;
  /**
   * One entry per field whose §5.1 preimage is not the empty field `80`, in
   * ascending field index. Empty for a transaction with nine empty fields.
   */
  readonly carriage: readonly TxOrderFieldCarriage[];
};

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
  nativeTxCbor,
  owner,
}: {
  readonly nativeTxCbor: Uint8Array;
  readonly owner: Uint8Array;
}): TxOrderMaterial => {
  const violation = validateMidgardConsensusTxCbor(nativeTxCbor);
  if (violation !== null) {
    throw new Error(
      `${violation.code} ${violation.featureId}: ${violation.detail}`,
    );
  }
  const tx = decodeMidgardNativeTxFullFromCanonicalCbor(nativeTxCbor);
  const transactionId = computeMidgardNativeTxId(tx);
  const proofSource = deriveMidgardNativeTxProofSource(tx);
  const source: NativeTxProofSource = {
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
  for (const field of deriveMidgardTxFieldPreimages(nativeTxCbor)) {
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
      computeMidgardNativeTxProofCommitment(proofSource).toString("hex"),
    source,
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
const requireTxOrderCreatorKeyHashProgram = (
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

/**
 * Whether a referenced field's carriage is present in a reference-input set.
 *
 * Resolvability is exactly what the index-producing functions need, so it is
 * decided by trying them: a `throw` is the answer "not there". Duplicating their
 * matching rules here to answer the same question a second way is how the two
 * answers come to disagree.
 */
const carriageIsResolvable = (
  field: TxOrderPlannedFieldCarriage,
  referenceInputs: readonly UTxO[],
  certificatePolicyId: string | undefined,
): boolean => {
  try {
    resolveChunkReferenceIndices({ plan: field.plan, referenceInputs });
    if (field.plan.tier !== "Certified") {
      return true;
    }
    if (certificatePolicyId === undefined || field.plan.certificate === null) {
      return false;
    }
    resolveCertificateReferenceIndex({
      certificatePolicyId,
      txIdHex: field.plan.txId.toString("hex"),
      fieldIndex: field.plan.fieldIndex,
      fieldHashHex: field.plan.commitment.toString("hex"),
      referenceInputs,
      label: field.fieldName,
    });
    return true;
  } catch (cause) {
    // Only "not there" is an answer. Both resolvers signal a missing carriage
    // UTxO by throwing a plain `Error`, so anything else — a `TypeError` from a
    // malformed plan, an out-of-memory, an assertion from deeper in the SDK — is
    // a defect and must not be reported to the caller as an unpublished field.
    if (cause instanceof Error && cause.constructor === Error) {
      return false;
    }
    throw cause;
  }
};

/**
 * The mint redeemer's `material_carriage` vector for a planned order.
 *
 * Positional over the non-empty fields in ascending field index, which is the
 * order the on-chain walk reads the nine commitments in. Reference-input indices
 * are resolved against the transaction's **complete, canonically ordered**
 * reference-input set, because that is what the ledger hands the validator — the
 * same discipline every other positional redeemer in this package keeps.
 */
export const txOrderMaterialCarriageVector = ({
  plan,
  certificatePolicyId,
  referenceInputs,
}: {
  readonly plan: TxOrderCarriagePlan;
  /**
   * The §8.6 certificate minting policy id — the same deployment role the
   * tx-order mint takes as its second parameter.
   *
   * Optional because only tier-3 fields consult it, and **absent is refused
   * rather than defaulted**: a tier-3 entry locates its manifest by the
   * policy's constant-name token over the plan's datum identity (#606), so an
   * empty or otherwise stand-in policy id would emit a redeemer that names a
   * token nobody can hold. The refusal is here, at the one place that needs
   * the value, rather than at a caller that might forget it.
   */
  readonly certificatePolicyId?: string;
  readonly referenceInputs: readonly UTxO[];
}): readonly FieldCarriage[] =>
  plan.carriage.map((field): FieldCarriage => {
    if (field.plan.tier === "Inline") {
      return { Inline: { preimage: field.preimage.toString("hex") } };
    }
    const chunkIndices = resolveChunkReferenceIndices({
      plan: field.plan,
      referenceInputs,
    });
    if (field.plan.tier === "RawUtxo") {
      const [refInputIndex] = chunkIndices;
      if (refInputIndex === undefined) {
        throw new Error(
          `${field.fieldName} tier-2 carriage resolved no reference input`,
        );
      }
      return { RawUtxo: { ref_input_index: BigInt(refInputIndex) } };
    }
    if (field.plan.certificate === null) {
      throw new Error(
        `${field.fieldName} tier-3 carriage has no §8.6 certificate`,
      );
    }
    if (certificatePolicyId === undefined) {
      throw new Error(
        `${field.fieldName} is tier-3 carriage, which names its §8.6 manifest ` +
          "by policy id, but no `certificatePolicyId` was supplied",
      );
    }
    const certificateIndex = resolveCertificateReferenceIndex({
      certificatePolicyId,
      txIdHex: field.plan.txId.toString("hex"),
      fieldIndex: field.plan.fieldIndex,
      fieldHashHex: field.plan.commitment.toString("hex"),
      referenceInputs,
      label: field.fieldName,
    });
    return {
      Certified: {
        cert_ref_input_index: BigInt(certificateIndex),
        chunk_ref_input_indices: chunkIndices.map((index) => BigInt(index)),
      },
    };
  });

export type PublishCekProgramMaterialConfig = {
  readonly entries: readonly MidgardCekProgramMaterialEntry[];
  readonly lovelacePerEntry?: bigint;
};

export type CekProgramMaterialPublication = {
  readonly entry: MidgardCekProgramMaterialEntry;
  readonly datum: CekProgramMaterialDatum;
  readonly datumCbor: string;
};

export type CekSinglePublication = {
  readonly programEnvelopeHash: string;
  readonly datum: CekSinglePublicationDatum;
  readonly datumCbor: string;
};

/**
 * Derives the sole immutable datum for a complete CEK graph. Both inputs are
 * copied before validation so later caller mutation cannot alter publication
 * identity or bytes.
 */
export const deriveCekSinglePublication = ({
  envelopeCbor,
  sidecarCbor,
}: {
  readonly envelopeCbor: Uint8Array;
  readonly sidecarCbor: Uint8Array;
}): CekSinglePublication => {
  const exactEnvelopeCbor = Buffer.from(envelopeCbor);
  const exactSidecarCbor = Buffer.from(sidecarCbor);
  const envelope = decodeMidgardCekProgramEnvelope(exactEnvelopeCbor);
  const material = decodeMidgardCekProgramMaterialSidecar(exactSidecarCbor);
  if (
    !encodeMidgardCekProgramMaterialSidecar(material).equals(exactSidecarCbor)
  ) {
    throw new Error("CEK single-publication sidecar CBOR is not canonical");
  }
  verifyMidgardCekProgramMaterialBundle([envelope], material);
  const programEnvelopeHash = Buffer.from(
    hashMidgardCekProgramEnvelope(envelope),
  ).toString("hex");
  const datum: CekSinglePublicationDatum = Object.freeze({
    version: CEK_SINGLE_PUBLICATION_DATUM_VERSION,
    program_envelope_hash: programEnvelopeHash,
    sidecar_cbor: exactSidecarCbor.toString("hex"),
  });
  return Object.freeze({
    programEnvelopeHash,
    datum,
    datumCbor: encodeCekSinglePublicationDatumCbor(datum).toString("hex"),
  });
};

export type PublishCekSinglePublicationConfig = {
  readonly envelopeCbor: Uint8Array;
  readonly sidecarCbor: Uint8Array;
  /** May increase funding, but cannot underfund the exact minimum Ada. */
  readonly lovelace?: bigint;
};

const MIN_ADA_STABILIZATION_LIMIT = 8;

const resolveProtocolParameters = async (
  lucid: LucidEvolution,
): Promise<ProtocolParameters> => {
  const config = lucid.config();
  if (config.protocolParameters !== undefined) {
    return config.protocolParameters;
  }
  if (config.provider === undefined) {
    throw new Error("Lucid provider is not configured.");
  }
  return await config.provider.getProtocolParameters();
};

/**
 * Calculates the exact stabilized minimum Ada for a CEK material UTxO with
 * its actual script address and inline datum.
 */
export const minimumLovelaceForCekProgramMaterialPublication = ({
  contracts,
  publication,
  coinsPerUtxoByte,
}: {
  readonly contracts: Pick<MidgardValidators, "cekProgramMaterial">;
  readonly publication: CekProgramMaterialPublication;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  const address = CML.Address.from_bech32(
    contracts.cekProgramMaterial.spendingScriptAddress,
  );
  const datum = CML.DatumOption.new_datum(
    CML.PlutusData.from_cbor_hex(publication.datumCbor),
  );
  let lovelace = 0n;
  for (let attempt = 0; attempt < MIN_ADA_STABILIZATION_LIMIT; attempt += 1) {
    const required = CML.min_ada_required(
      CML.TransactionOutput.new(
        address,
        CML.Value.from_coin(lovelace),
        datum,
        undefined,
      ),
      coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error(
    "Failed to stabilize CEK program-material publication min-Ada calculation.",
  );
};

/**
 * Calculates the exact stabilized minimum Ada for an immutable complete CEK
 * material datum at its actual reference-only script address.
 */
export const minimumLovelaceForCekSinglePublication = ({
  contracts,
  publication,
  coinsPerUtxoByte,
}: {
  readonly contracts: Pick<MidgardValidators, "cekProgramMaterial">;
  readonly publication: CekSinglePublication;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  const address = CML.Address.from_bech32(
    contracts.cekProgramMaterial.spendingScriptAddress,
  );
  const datum = CML.DatumOption.new_datum(
    CML.PlutusData.from_cbor_hex(publication.datumCbor),
  );
  let lovelace = 0n;
  for (let attempt = 0; attempt < MIN_ADA_STABILIZATION_LIMIT; attempt += 1) {
    const required = CML.min_ada_required(
      CML.TransactionOutput.new(
        address,
        CML.Value.from_coin(lovelace),
        datum,
        undefined,
      ),
      coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error(
    "Failed to stabilize CEK single-publication min-Ada calculation.",
  );
};

export const deriveCekProgramMaterialPublications = (
  entries: readonly MidgardCekProgramMaterialEntry[],
): readonly CekProgramMaterialPublication[] => {
  if (entries.length === 0) {
    throw new Error("CEK program-material publication cannot be empty");
  }
  const seen = new Set<string>();
  return Object.freeze(
    entries.map((entry) => {
      const exact = decodeMidgardCekProgramMaterialEntry(
        encodeMidgardCekProgramMaterialEntry(entry),
      );
      const root = Buffer.from(exact.root).toString("hex");
      if (seen.has(root)) {
        throw new Error(`duplicate CEK program-material root ${root}`);
      }
      seen.add(root);
      const datum: CekProgramMaterialDatum = {
        kind: midgardCekProgramMaterialKindTag(exact.kind),
        root,
        preimage: exact.preimage.toString("hex"),
      };
      const datumCbor = Data.to(datum, CekProgramMaterialDatum);
      const datumBytes = Buffer.byteLength(datumCbor, "hex");
      if (datumBytes > MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes) {
        throw new Error(
          `CEK program-material datum ${root} exceeds the independently revealable L1 proof field bound`,
        );
      }
      return Object.freeze({ entry: exact, datum, datumCbor });
    }),
  );
};

export const buildUnsignedCekProgramMaterialProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekProgramMaterialConfig,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      const publications = deriveCekProgramMaterialPublications(config.entries);
      const protocolParameters = await resolveProtocolParameters(lucid);
      let tx = lucid.newTx();
      for (const publication of publications) {
        const minimumLovelace = minimumLovelaceForCekProgramMaterialPublication(
          {
            contracts,
            publication,
            coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
          },
        );
        tx = tx.pay.ToAddressWithData(
          contracts.cekProgramMaterial.spendingScriptAddress,
          { kind: "inline", value: publication.datumCbor },
          {
            lovelace:
              config.lovelacePerEntry === undefined
                ? minimumLovelace
                : config.lovelacePerEntry > minimumLovelace
                  ? config.lovelacePerEntry
                  : minimumLovelace,
          },
        );
      }
      return tx.complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: "Failed to publish V1 CEK program material",
        cause,
      }),
  });

/**
 * Publishes exactly one complete CEK graph as an immutable reference-only
 * inline datum. It has no spending path and therefore creates no mutable
 * state transition.
 */
export const buildUnsignedCekSinglePublicationProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekSinglePublicationConfig,
): Effect.Effect<TxSignBuilder, UserEventBuildError> =>
  Effect.tryPromise({
    try: async () => {
      const publication = deriveCekSinglePublication(config);
      const protocolParameters = await resolveProtocolParameters(lucid);
      const minimumLovelace = minimumLovelaceForCekSinglePublication({
        contracts,
        publication,
        coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
      });
      return lucid
        .newTx()
        .pay.ToAddressWithData(
          contracts.cekProgramMaterial.spendingScriptAddress,
          { kind: "inline", value: publication.datumCbor },
          {
            lovelace:
              config.lovelace === undefined
                ? minimumLovelace
                : config.lovelace > minimumLovelace
                  ? config.lovelace
                  : minimumLovelace,
          },
        )
        .complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new UserEventBuildError({
        message: "Failed to publish V1 complete CEK program material",
        cause,
      }),
  });

export type TxOrderBuildMetadata = {
  readonly txOrderAddress: string;
  readonly txOrderId: OutputReference;
  readonly authNonceCbor: string;
  readonly txOrderAuthUnit: string;
  readonly nonceInput: Pick<UTxO, "txHash" | "outputIndex">;
  readonly validTo: number;
  readonly inclusionTime: number;
};

const DEFAULT_TX_ORDER_LOVELACE = 3_000_000n;

export const buildUnsignedTxOrderTxWithMetadataProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitTxOrderConfig,
): Effect.Effect<
  {
    readonly tx: TxSignBuilder;
    readonly metadata: TxOrderBuildMetadata;
  },
  | HubOracleError
  | LucidError
  | Bech32DeserializationError
  | HashingError
  | UserEventBuildError
> =>
  Effect.gen(function* () {
    const nativeTxCbor = yield* Effect.try({
      try: () => {
        if (
          config.nativeTxCbor.length === 0 ||
          config.nativeTxCbor.length % 2 !== 0 ||
          !/^[0-9a-f]+$/iu.test(config.nativeTxCbor)
        ) {
          throw new Error(
            "nativeTxCbor must be non-empty, even-length hexadecimal",
          );
        }
        return Buffer.from(config.nativeTxCbor, "hex");
      },
      catch: (cause) =>
        new UserEventBuildError({
          message:
            "V1 tx order requires exact bounded canonical native V1 bytes",
          cause,
        }),
    });
    const context = yield* prepareUserEventMintContext({
      lucid,
      contracts,
      label: "tx order",
      eventPolicyId: contracts.txOrder.policyId,
      hubOraclePolicyField: "tx_order",
      hubOracleAddressField: "tx_order_addr",
      nonceInput: config.nonceInput,
    });
    const {
      eventUnit: txOrderUnit,
      hubOracleRefInput,
      inclusionTime,
      network,
      nonceInput,
      validTo,
      witnessScript,
      witnessScriptHash,
    } = context;
    const txOrderId = outputReferenceFromUTxO(nonceInput);
    const authNonceCbor = outputReferenceToPlutusDataCbor(nonceInput);
    const creatorPaymentKeyHash =
      yield* requireTxOrderCreatorKeyHashProgram(lucid);
    const carriageReferenceInputs = config.carriageReferenceInputs ?? [];
    const order = yield* Effect.try({
      try: () => {
        const material = deriveTxOrderMaterial({
          nativeTxCbor,
          // §8.5/§8.7 under #594's ruling: reclaim is an ordinary key spend at
          // any time after the mint, so the min-Ada authority a tier-3
          // certificate records has to be a key the creator can sign with. The
          // event witness script hash stood here while no plan was publishable
          // (#589); it never was the right authority.
          owner: creatorPaymentKeyHash,
        });
        const plan = planTxOrderMaterialCarriage({
          material,
          owner: creatorPaymentKeyHash,
          inlineReserveBytes: config.inlineCarriageReserveBytes,
        });
        // A tier-3 field needs the §8.6 policy id to name its manifest token, and
        // saying so here names the configuration that is missing instead of
        // failing later on a token nobody could have matched.
        if (
          config.fieldPreimageCertificatePolicyId === undefined &&
          plan.referenced.some((field) => field.plan.tier === "Certified")
        ) {
          throw new Error(
            `forced order carries a field larger than §8.3's K (${plan.referenced
              .filter((field) => field.plan.tier === "Certified")
              .map((field) => field.fieldName)
              .join(", ")}), which is tier-3 carriage, but ` +
              "`fieldPreimageCertificatePolicyId` was not configured — the §8.6 " +
              "certificate policy is what the door checks a manifest against",
          );
        }
        // Say which publications are missing here rather than let the door abort
        // at submission with no name attached. Every referenced field's carriage
        // has to exist *before* this transaction, because reference inputs are
        // resolved against the UTxO set as it stands before it.
        const unpublished = plan.referenced.filter(
          (field) =>
            !carriageIsResolvable(
              field,
              carriageReferenceInputs,
              config.fieldPreimageCertificatePolicyId,
            ),
        );
        if (unpublished.length > 0) {
          throw new Error(
            `forced order references predeployed §8 carriage for ${unpublished
              .map((field) => `${field.fieldName} (tier ${field.plan.tier})`)
              .join(
                ", ",
              )}, which is not among the reference inputs supplied in ` +
              "`carriageReferenceInputs`. Publish each field with " +
              "`buildUnsignedFieldPreimagePublicationV1Program` (and certify " +
              "tier-3 fields) before building the order.",
          );
        }
        return { material, plan };
      },
      catch: (cause) =>
        new UserEventBuildError({
          message: "Failed to derive V1 tx-order material carriage",
          cause,
        }),
    });
    const material = order.material;
    const txOrderDatum: TxOrderDatum = {
      event: {
        id: txOrderId,
        tx: {
          tx_id: material.transactionId,
          transaction_commitment: material.transactionCommitment,
          source: material.source,
        },
      },
      inclusion_time: BigInt(inclusionTime),
      witness: witnessScriptHash,
      refund_address: config.refundAddress,
      refund_datum: config.refundDatum ?? "NoDatum",
    };
    const txOrderDatumCBOR = Data.to(txOrderDatum, TxOrderDatum);
    const outputAssets: Assets = {
      lovelace: config.lovelace ?? DEFAULT_TX_ORDER_LOVELACE,
      [txOrderUnit]: 1n,
    };
    const referenceInputs = [
      hubOracleRefInput,
      ...(config.referenceScripts === undefined
        ? []
        : [config.referenceScripts.txOrderMinting]),
      // The order's §8 carriage. Read, never spent — §8.7's content addressing is
      // what makes that safe: a republished chunk with the same bytes is the same
      // carriage, so nothing here is identified by `OutputReference`.
      ...carriageReferenceInputs,
    ];
    const witnessRegistrationRedeemer =
      encodeUserEventWitnessMintOrBurnRedeemer(contracts.txOrder.policyId);
    const tx = yield* buildCompletedUserEventMintTxProgram({
      lucid,
      network,
      nonceInput,
      eventUnit: txOrderUnit,
      eventAddress: contracts.txOrder.spendingScriptAddress,
      eventDatumCbor: txOrderDatumCBOR,
      outputAssets,
      validTo,
      mintingPolicy: contracts.txOrder.mintingScript,
      attachMintingPolicy: config.referenceScripts === undefined,
      referenceInputs,
      hubOracleRefInput,
      witnessScript,
      witnessRegistrationRedeemer,
      label: "tx order",
      // #594: the tx-order policy's redeemer is `user_events.MintRedeemer` inside
      // its own `MintRedeemer`, beside the §8 carriage vector. The indices in that
      // vector are positional in the final transaction's reference-input set, so
      // they are resolved from `ctx` here and not from the order this builder
      // happened to list them in.
      encodeMintRedeemer: ({ layout, ctx }) =>
        Data.to(
          {
            // Delegated, not re-spelled: the four-field mapping is
            // `user_events.MintRedeemer`'s and lives with that type.
            event: userEventAuthenticateMintRedeemer(layout),
            material_carriage: [
              ...txOrderMaterialCarriageVector({
                plan: order.plan,
                certificatePolicyId: config.fieldPreimageCertificatePolicyId,
                referenceInputs: ctx.referenceInputs,
              }),
            ],
          } satisfies TxOrderMintRedeemer,
          TxOrderMintRedeemer,
        ),
    });
    return {
      tx,
      metadata: {
        txOrderAddress: contracts.txOrder.spendingScriptAddress,
        materialCarriage: order.plan,
        txOrderId,
        authNonceCbor,
        txOrderAuthUnit: txOrderUnit,
        nonceInput,
        validTo,
        inclusionTime,
      },
    };
  }).pipe(
    Effect.catchAllDefect((defect) =>
      Effect.fail(
        new LucidError({
          message: "Caught defect from V1 txOrderTxBuilder",
          cause: defect,
        }),
      ),
    ),
  );

export const unsignedTxOrderTxProgram = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: SubmitTxOrderConfig,
) =>
  buildUnsignedTxOrderTxWithMetadataProgram(lucid, contracts, config).pipe(
    Effect.map(({ tx }) => tx),
  );

export const buildUnsignedTxOrderTxProgram = unsignedTxOrderTxProgram;

export const unsignedTxOrderTx = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  txOrderParams: SubmitTxOrderConfig,
): Promise<TxSignBuilder> =>
  makeReturn(
    unsignedTxOrderTxProgram(lucid, contracts, txOrderParams),
  ).unsafeRun();

export const unsignedCekProgramMaterial = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekProgramMaterialConfig,
): Promise<TxSignBuilder> =>
  makeReturn(
    buildUnsignedCekProgramMaterialProgram(lucid, contracts, config),
  ).unsafeRun();

export const unsignedCekSinglePublication = (
  lucid: LucidEvolution,
  contracts: MidgardValidators,
  config: PublishCekSinglePublicationConfig,
): Promise<TxSignBuilder> =>
  makeReturn(
    buildUnsignedCekSinglePublicationProgram(lucid, contracts, config),
  ).unsafeRun();
