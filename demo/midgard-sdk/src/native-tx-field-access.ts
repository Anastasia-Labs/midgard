import {
  MIDGARD_EMPTY_FIELD_COMMITMENT,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS,
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME,
  MIDGARD_FIELD_VIEW_CONSTRUCTORS,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, VerificationKeyHashSchema } from "./common.js";

/**
 * The on-chain wire twins of `docs/spec/midgard-tx.md` §8.8 — the field-preimage
 * carriage and field-view sum types the single `authenticated_field_view` door
 * speaks, plus the §8.6 certificate datum an off-chain minter has to produce.
 *
 * Aiken source of truth:
 * `onchain/aiken/lib/midgard/native-tx-field-access-v1.ak`. Byte-level
 * derivations (the §5.1 envelope, the §4 flat commitment, the §8.4 chunk split,
 * the §8.6 asset name) live in
 * `@al-ft/midgard-core/codec/native-tx-field-access-v1` — the narrow subpath,
 * not the codec barrel, so declaring a carriage does not drag CML in — and the
 * two values a builder cannot compute for itself are re-exported here.
 *
 * **Constructor order is frozen consensus wire format.** `Data.Enum` pins the
 * Constr index positionally, so the array order below *is* the on-chain tag
 * order: `Inline` 0, `RawUtxo` 1, `Certified` 2, and `Whole` 0, `Chunked` 1.
 * Reordering these arrays silently re-tags every carriage a builder emits.
 *
 * Every Aiken variant here carries **named** fields, so each is spelled
 * `Data.Object({ Variant: Data.Object({ … }) })` — the inner object's keys
 * become the constructor's own fields. The `Data.Tuple([…])` spelling used
 * elsewhere in this package is for variants holding a single *anonymous* value
 * and would wrap the payload in a second Constr the door never decodes; the
 * exact-CBOR assertions in `tests/native-tx-field-access.test.ts` are what
 * keep that distinction honest.
 */

/** §8.1. Tier 1 — the step's own redeemer carries the preimage. */
export const InlineFieldCarriageSchema = Data.Object({
  preimage: Data.Bytes(),
});
export type InlineFieldCarriage = Data.Static<typeof InlineFieldCarriageSchema>;
export const InlineFieldCarriage = asDataType<InlineFieldCarriage>(
  InlineFieldCarriageSchema,
);

/**
 * §8.2. Tier 2 — one nothing-but-bytes inline datum at the prover's key
 * address, named by its positional reference-input index.
 */
export const RawUtxoFieldCarriageSchema = Data.Object({
  ref_input_index: Data.Integer(),
});
export type RawUtxoFieldCarriage = Data.Static<
  typeof RawUtxoFieldCarriageSchema
>;
export const RawUtxoFieldCarriage = asDataType<RawUtxoFieldCarriage>(
  RawUtxoFieldCarriageSchema,
);

/**
 * §8.4. Tier 3 — deterministic fixed-K chunks plus one certified
 * digest-manifest. `chunk_ref_input_indices` is all-chunks-positional: element
 * `k` is the reference-input index of chunk `k` (at most three chunks under
 * §8.3). Reference-input indexing is positional and redeemer-supplied.
 */
export const CertifiedFieldCarriageSchema = Data.Object({
  cert_ref_input_index: Data.Integer(),
  chunk_ref_input_indices: Data.Array(Data.Integer()),
});
export type CertifiedFieldCarriage = Data.Static<
  typeof CertifiedFieldCarriageSchema
>;
export const CertifiedFieldCarriage = asDataType<CertifiedFieldCarriage>(
  CertifiedFieldCarriageSchema,
);

/**
 * §8.8 `FieldCarriage` — how a field's preimage bytes reach the consuming
 * transaction. Constructor order mirrors the aiken sum exactly.
 */
export const FieldCarriageSchema = Data.Enum([
  Data.Object({ Inline: InlineFieldCarriageSchema }),
  Data.Object({ RawUtxo: RawUtxoFieldCarriageSchema }),
  Data.Object({ Certified: CertifiedFieldCarriageSchema }),
]);
export type FieldCarriage = Data.Static<typeof FieldCarriageSchema>;
export const FieldCarriage = asDataType<FieldCarriage>(FieldCarriageSchema);

/** Tiers 1–2: the whole preimage is present and hash-checked. */
export const WholeFieldViewSchema = Data.Object({
  bytes: Data.Bytes(),
  count: Data.Integer(),
  stride: Data.Integer(),
});
export type WholeFieldView = Data.Static<typeof WholeFieldViewSchema>;
export const WholeFieldView = asDataType<WholeFieldView>(WholeFieldViewSchema);

/**
 * Tier 3: chunks are present but unhashed until touched; the digest vector and
 * the item count come from the mint-verified certificate.
 */
export const ChunkedFieldViewSchema = Data.Object({
  chunks: Data.Array(Data.Bytes()),
  chunk_digests: Data.Array(H32Schema),
  count: Data.Integer(),
  stride: Data.Integer(),
});
export type ChunkedFieldView = Data.Static<typeof ChunkedFieldViewSchema>;
export const ChunkedFieldView = asDataType<ChunkedFieldView>(
  ChunkedFieldViewSchema,
);

/**
 * §8.8 `FieldView` — an authenticated field, ready to slice. Carriage tier is
 * an encoding detail validator logic never branches on; the view is what the
 * door hands back, so a builder that reconstructs a step's expected state needs
 * the same two tags in the same order.
 */
export const FieldViewSchema = Data.Enum([
  Data.Object({ Whole: WholeFieldViewSchema }),
  Data.Object({ Chunked: ChunkedFieldViewSchema }),
]);
export type FieldView = Data.Static<typeof FieldViewSchema>;
export const FieldView = asDataType<FieldView>(FieldViewSchema);

/**
 * §8.6 `FieldPreimageCertificate` — the tier-3 digest manifest, minted by the
 * permissionless multi-handler certificate validator and carried as the
 * manifest UTxO's inline datum. Field order is the aiken record's declaration
 * order.
 */
export const FieldPreimageCertificateSchema = Data.Object({
  owner: VerificationKeyHashSchema,
  tx_id: H32Schema,
  field_index: Data.Integer(),
  // The mint-welded §4 field commitment (#606, owner ruling 2026-08-16): the
  // policy checks `hash(concat(chunks))` against exactly this value, and the
  // door refuses a certificate whose welded hash is not the anchored
  // commitment.
  field_hash: H32Schema,
  total_length: Data.Integer(),
  chunk_digests: Data.Array(H32Schema),
});
export type FieldPreimageCertificate = Data.Static<
  typeof FieldPreimageCertificateSchema
>;
export const FieldPreimageCertificate = asDataType<FieldPreimageCertificate>(
  FieldPreimageCertificateSchema,
);

/**
 * §8.6. The `Certify` arm of the certification redeemer — the committed
 * structures the field hash is extracted from, plus the positional order of the
 * raw chunks.
 *
 * It carries no identity. Publication and certification are permissionless
 * (§8.7), so the policy checks content and never who supplied it: there is no
 * signer, no owner and no publisher field here, and the `owner` a certificate
 * records is read from the named output's inline datum rather than from this
 * redeemer.
 *
 * `compact_cbor` is the disputed L2 transaction's compact CBOR — the policy
 * re-derives the tx-id from it through the unchanged §3 derivation and never
 * accepts one. `witness_set_compact_cbor` is required for all nine fields, not
 * only 6–8, because certification happens once per field and one unconditional
 * code path is worth more than one saved hash. `chunk_ref_input_indices` is
 * all-chunks-positional and bounded at three by §8.3.
 */
export const CertifyFieldPreimageCertificateMintRedeemerSchema = Data.Object({
  compact_cbor: Data.Bytes(),
  witness_set_compact_cbor: Data.Bytes(),
  chunk_ref_input_indices: Data.Array(Data.Integer()),
  output_index: Data.Integer(),
});
export type CertifyFieldPreimageCertificateMintRedeemer = Data.Static<
  typeof CertifyFieldPreimageCertificateMintRedeemerSchema
>;
export const CertifyFieldPreimageCertificateMintRedeemer =
  asDataType<CertifyFieldPreimageCertificateMintRedeemer>(
    CertifyFieldPreimageCertificateMintRedeemerSchema,
  );

/**
 * §8.6 `FieldPreimageCertificateMintRedeemer` — the frozen mint-redeemer wire
 * format of the permissionless certificate validator.
 *
 * **Constructor order is frozen consensus wire format**: `Certify` is Constr 0
 * and `Retire` is Constr 1, because this off-chain minter emits the tag and the
 * compiled policy branches on it. Aiken source of truth:
 * `onchain/aiken/lib/midgard/native-tx-carriage-v1.ak` — a different module
 * from the rest of this file's twins, because §8.6's producer half lives with
 * the carriage producer rather than behind the access door.
 *
 * `Retire` is nullary, so it is spelled `Data.Literal` and encodes as a bare
 * `Constr 1 []`. It is the burn path; the owner's authority is checked by the
 * validator's `spend` handler, and all this arm owes is that a burn redeemer
 * cannot mint.
 */
export const FieldPreimageCertificateMintRedeemerSchema = Data.Enum([
  Data.Object({
    Certify: CertifyFieldPreimageCertificateMintRedeemerSchema,
  }),
  Data.Literal("Retire"),
]);
export type FieldPreimageCertificateMintRedeemer = Data.Static<
  typeof FieldPreimageCertificateMintRedeemerSchema
>;
export const FieldPreimageCertificateMintRedeemer =
  asDataType<FieldPreimageCertificateMintRedeemer>(
    FieldPreimageCertificateMintRedeemerSchema,
  );

/**
 * The frozen §8.8 Constr indexes, exported so a test can pin them rather than
 * trusting that nobody reorders the `Data.Enum` arrays above.
 */
export const FIELD_CARRIAGE_CONSTRUCTOR_INDEXES: Readonly<
  Record<(typeof MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS)[number], number>
> = Object.freeze({ Inline: 0, RawUtxo: 1, Certified: 2 });

export const FIELD_VIEW_CONSTRUCTOR_INDEXES: Readonly<
  Record<(typeof MIDGARD_FIELD_VIEW_CONSTRUCTORS)[number], number>
> = Object.freeze({ Whole: 0, Chunked: 1, ProvisionalWhole: 2 });

/**
 * The frozen §8.6 mint-redeemer Constr indexes. Same reason as the two above:
 * the tag is positional, and a reorder is a change no type checker sees.
 */
export const FIELD_PREIMAGE_CERTIFICATE_MINT_REDEEMER_CONSTRUCTOR_INDEXES: Readonly<
  Record<"Certify" | "Retire", number>
> = Object.freeze({ Certify: 0, Retire: 1 });

/**
 * §4/§5.1. The commitment every empty field carries: `blake2b_256(#"80")`.
 * Plain hashing puts no field index in the preimage, so all nine share it.
 * Re-exported as hex because on-chain datum comparisons in this package are
 * hex strings.
 */
export const EMPTY_FIELD_COMMITMENT_HEX: string =
  MIDGARD_EMPTY_FIELD_COMMITMENT.toString("hex");

/**
 * §8.6's constant certificate asset name (#606, owner ruling 2026-08-16), as
 * the hex string an `Assets` key needs. One constant for every certificate of
 * the policy — the retired `blake2b_256(field_index ‖ tx_id)` derivation's
 * information lives in the datum, and discovery is by enumerating the single
 * certificate address and filtering by datum.
 */
export const FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX: string =
  MIDGARD_FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME.toString("hex");
