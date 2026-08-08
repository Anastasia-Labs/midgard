import {
  MIDGARD_EMPTY_FIELD_COMMITMENT_V1,
  MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1,
  MIDGARD_FIELD_VIEW_CONSTRUCTORS_V1,
  midgardFieldPreimageCertificateAssetNameV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, VerificationKeyHashSchema } from "@/common.js";

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
 * exact-CBOR assertions in `tests/native-tx-field-access-v1.test.ts` are what
 * keep that distinction honest.
 */

/** §8.1. Tier 1 — the step's own redeemer carries the preimage. */
export const InlineFieldCarriageV1Schema = Data.Object({
  preimage: Data.Bytes(),
});
export type InlineFieldCarriageV1 = Data.Static<
  typeof InlineFieldCarriageV1Schema
>;
export const InlineFieldCarriageV1 =
  InlineFieldCarriageV1Schema as unknown as InlineFieldCarriageV1;

/**
 * §8.2. Tier 2 — one nothing-but-bytes inline datum at the prover's key
 * address, named by its positional reference-input index.
 */
export const RawUtxoFieldCarriageV1Schema = Data.Object({
  ref_input_index: Data.Integer(),
});
export type RawUtxoFieldCarriageV1 = Data.Static<
  typeof RawUtxoFieldCarriageV1Schema
>;
export const RawUtxoFieldCarriageV1 =
  RawUtxoFieldCarriageV1Schema as unknown as RawUtxoFieldCarriageV1;

/**
 * §8.4. Tier 3 — deterministic fixed-K chunks plus one certified
 * digest-manifest. `chunk_ref_input_indices` is all-chunks-positional: element
 * `k` is the reference-input index of chunk `k` (at most three chunks under
 * §8.3). Reference-input indexing is positional and redeemer-supplied.
 */
export const CertifiedFieldCarriageV1Schema = Data.Object({
  cert_ref_input_index: Data.Integer(),
  chunk_ref_input_indices: Data.Array(Data.Integer()),
});
export type CertifiedFieldCarriageV1 = Data.Static<
  typeof CertifiedFieldCarriageV1Schema
>;
export const CertifiedFieldCarriageV1 =
  CertifiedFieldCarriageV1Schema as unknown as CertifiedFieldCarriageV1;

/**
 * §8.8 `FieldCarriageV1` — how a field's preimage bytes reach the consuming
 * transaction. Constructor order mirrors the aiken sum exactly.
 */
export const FieldCarriageV1Schema = Data.Enum([
  Data.Object({ Inline: InlineFieldCarriageV1Schema }),
  Data.Object({ RawUtxo: RawUtxoFieldCarriageV1Schema }),
  Data.Object({ Certified: CertifiedFieldCarriageV1Schema }),
]);
export type FieldCarriageV1 = Data.Static<typeof FieldCarriageV1Schema>;
export const FieldCarriageV1 =
  FieldCarriageV1Schema as unknown as FieldCarriageV1;

/** Tiers 1–2: the whole preimage is present and hash-checked. */
export const WholeFieldViewV1Schema = Data.Object({
  bytes: Data.Bytes(),
  count: Data.Integer(),
  stride: Data.Integer(),
});
export type WholeFieldViewV1 = Data.Static<typeof WholeFieldViewV1Schema>;
export const WholeFieldViewV1 =
  WholeFieldViewV1Schema as unknown as WholeFieldViewV1;

/**
 * Tier 3: chunks are present but unhashed until touched; the digest vector and
 * the item count come from the mint-verified certificate.
 */
export const ChunkedFieldViewV1Schema = Data.Object({
  chunks: Data.Array(Data.Bytes()),
  chunk_digests: Data.Array(H32Schema),
  count: Data.Integer(),
  stride: Data.Integer(),
});
export type ChunkedFieldViewV1 = Data.Static<typeof ChunkedFieldViewV1Schema>;
export const ChunkedFieldViewV1 =
  ChunkedFieldViewV1Schema as unknown as ChunkedFieldViewV1;

/**
 * §8.8 `FieldViewV1` — an authenticated field, ready to slice. Carriage tier is
 * an encoding detail validator logic never branches on; the view is what the
 * door hands back, so a builder that reconstructs a step's expected state needs
 * the same two tags in the same order.
 */
export const FieldViewV1Schema = Data.Enum([
  Data.Object({ Whole: WholeFieldViewV1Schema }),
  Data.Object({ Chunked: ChunkedFieldViewV1Schema }),
]);
export type FieldViewV1 = Data.Static<typeof FieldViewV1Schema>;
export const FieldViewV1 = FieldViewV1Schema as unknown as FieldViewV1;

/**
 * §8.6 `FieldPreimageCertificateV1` — the tier-3 digest manifest, minted by the
 * permissionless multi-handler certificate validator and carried as the
 * manifest UTxO's inline datum. Field order is the aiken record's declaration
 * order.
 */
export const FieldPreimageCertificateV1Schema = Data.Object({
  owner: VerificationKeyHashSchema,
  tx_id: H32Schema,
  field_index: Data.Integer(),
  total_length: Data.Integer(),
  chunk_digests: Data.Array(H32Schema),
});
export type FieldPreimageCertificateV1 = Data.Static<
  typeof FieldPreimageCertificateV1Schema
>;
export const FieldPreimageCertificateV1 =
  FieldPreimageCertificateV1Schema as unknown as FieldPreimageCertificateV1;

/**
 * The frozen §8.8 Constr indexes, exported so a test can pin them rather than
 * trusting that nobody reorders the `Data.Enum` arrays above.
 */
export const FIELD_CARRIAGE_V1_CONSTRUCTOR_INDEXES: Readonly<
  Record<(typeof MIDGARD_FIELD_CARRIAGE_CONSTRUCTORS_V1)[number], number>
> = Object.freeze({ Inline: 0, RawUtxo: 1, Certified: 2 });

export const FIELD_VIEW_V1_CONSTRUCTOR_INDEXES: Readonly<
  Record<(typeof MIDGARD_FIELD_VIEW_CONSTRUCTORS_V1)[number], number>
> = Object.freeze({ Whole: 0, Chunked: 1 });

/**
 * §4/§5.1. The commitment every empty field carries: `blake2b_256(#"80")`.
 * Plain hashing puts no field index in the preimage, so all nine share it.
 * Re-exported as hex because on-chain datum comparisons in this package are
 * hex strings.
 */
export const EMPTY_FIELD_COMMITMENT_HEX_V1: string =
  MIDGARD_EMPTY_FIELD_COMMITMENT_V1.toString("hex");

/**
 * §8.6's deterministic certificate asset name, `blake2b_256(field_index ‖
 * tx_id)`, as the hex string a `Assets` key needs. Both bounds — `field_index`
 * in 0..8 and a 32-byte `tx_id` — are enforced by the core derivation.
 */
export const fieldPreimageCertificateAssetNameHexV1 = ({
  txId,
  fieldIndex,
}: {
  readonly txId: string;
  readonly fieldIndex: number;
}): string =>
  midgardFieldPreimageCertificateAssetNameV1({
    txId: Buffer.from(txId, "hex"),
    fieldIndex,
  }).toString("hex");
