import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core/codec/native-tx-carriage";
import { MIDGARD_ENVELOPE_MEASUREMENTS } from "@al-ft/midgard-core/consensus-profile";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data, UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { CredentialSchema, POSIXTimeSchema } from "../common.js";
import { authenticateUTxOs, AuthenticUTxO } from "../internals.js";
import {
  CardanoDatum,
  CardanoDatumSchema,
  CekProgramMaterialDatum,
  CekProgramMaterialDatumSchema,
  ForcedTxProofSource,
  TxOrderEventSchema,
} from "../ledger-state.js";
import { FieldCarriageSchema } from "../native-tx-field-access.js";
import { OperatorVerdictSchema } from "../rejection-reason.js";
import { RawRootMembershipProofSchema } from "../transition-trace.js";
import {
  userEventCborFieldsFromInlineDatum,
  UserEventExtraFields,
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

export const TxOrderRefundAddress = asDataType<TxOrderRefundAddress>(
  TxOrderRefundAddressSchema,
);

export const TxOrderDatumSchema = Data.Object({
  event: TxOrderEventSchema,
  inclusion_time: POSIXTimeSchema,
  witness: Data.Bytes({ minLength: 28, maxLength: 28 }),
  refund_address: TxOrderRefundAddressSchema,
  refund_datum: CardanoDatumSchema,
});

export type TxOrderDatum = Data.Static<typeof TxOrderDatumSchema>;

export const TxOrderDatum = asDataType<TxOrderDatum>(TxOrderDatumSchema);

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

export const CekSinglePublicationDatum = asDataType<CekSinglePublicationDatum>(
  CekSinglePublicationDatumSchema,
);

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

export const TxOrderSpendRedeemer = asDataType<TxOrderSpendRedeemer>(
  TxOrderSpendRedeemerSchema,
);

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

export const TxOrderMintRedeemer = asDataType<TxOrderMintRedeemer>(
  TxOrderMintRedeemerSchema,
);

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

export type SubmitTxOrderReferenceScripts = {
  readonly txOrderMinting: UTxO;
};

export type SubmitTxOrderConfig = {
  /** Exact bounded canonical native-V1 transaction bytes. */
  readonly submittedTxCbor: string;
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
  readonly submitted_source: ForcedTxProofSource;
  /**
   * One entry per field whose §5.1 preimage is not the empty field `80`, in
   * ascending field index. Empty for a transaction with nine empty fields.
   */
  readonly carriage: readonly TxOrderFieldCarriage[];
};
