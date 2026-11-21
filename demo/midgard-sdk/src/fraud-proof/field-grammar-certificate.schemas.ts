import {
  MIDGARD_FIELD_GRAMMAR_CERTIFICATE_ASSET_NAME,
  type MidgardFieldGrammarCertificate,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { H32Schema, VerificationKeyHashSchema } from "../common.js";
import { FieldCarriageSchema } from "../native-tx-field-access.js";

/**
 * The wire twins of the field grammar certificate validator
 * (`onchain/aiken/validators/field-grammar-certificate.ak`). A field grammar
 * certificate lets a step read one item of a variable-width field without
 * walking the whole field, so the step's cost no longer grows with the field's
 * item count.
 *
 * Aiken source of truth: the datum is `FieldGrammarCertificateV1` in
 * `lib/midgard/field-grammar-certificate-v1.ak` and the redeemers are in
 * `lib/midgard/field-grammar-certification-v1.ak`. **Constructor order is
 * frozen wire format**: `InProgress` 0 and `Complete` 1, `Open` 0 and `Retire`
 * 1, `Advance` 0 and `Reclaim` 1.
 */

/** The certificate token's constant asset name, as hex. */
export const FIELD_GRAMMAR_CERTIFICATE_ASSET_NAME_HEX =
  MIDGARD_FIELD_GRAMMAR_CERTIFICATE_ASSET_NAME.toString("hex");

const certificateFields = {
  owner: VerificationKeyHashSchema,
  tx_id: H32Schema,
  field_index: Data.Integer(),
  field_hash: H32Schema,
  total_length: Data.Integer(),
};

export const FieldGrammarCertificateSchema = Data.Enum([
  Data.Object({
    InProgress: Data.Object({
      ...certificateFields,
      grammar_checkpoint_hash: H32Schema,
      waypoints: Data.Bytes(),
    }),
  }),
  Data.Object({
    Complete: Data.Object({
      ...certificateFields,
      item_count: Data.Integer(),
      waypoints: Data.Bytes(),
    }),
  }),
]);
export type FieldGrammarCertificate = Data.Static<
  typeof FieldGrammarCertificateSchema
>;
export const FieldGrammarCertificate = asDataType<FieldGrammarCertificate>(
  FieldGrammarCertificateSchema,
);

const batchFields = {
  source_kind: Data.Integer({ minimum: 0, maximum: 1 }),
  compact_cbor: Data.Bytes(),
  witness_set_compact_cbor: Data.Bytes(),
};

export const FieldGrammarCertificateMintRedeemerSchema = Data.Enum([
  Data.Object({
    Open: Data.Object({
      ...batchFields,
      carriage: FieldCarriageSchema,
      batch: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
  Data.Literal("Retire"),
]);
export type FieldGrammarCertificateMintRedeemer = Data.Static<
  typeof FieldGrammarCertificateMintRedeemerSchema
>;
export const FieldGrammarCertificateMintRedeemer =
  asDataType<FieldGrammarCertificateMintRedeemer>(
    FieldGrammarCertificateMintRedeemerSchema,
  );

export const FieldGrammarCertificateSpendRedeemerSchema = Data.Enum([
  Data.Object({
    Advance: Data.Object({
      ...batchFields,
      checkpoint: Data.Bytes(),
      carriage: FieldCarriageSchema,
      batch: Data.Integer(),
      output_index: Data.Integer(),
    }),
  }),
  Data.Literal("Reclaim"),
]);
export type FieldGrammarCertificateSpendRedeemer = Data.Static<
  typeof FieldGrammarCertificateSpendRedeemerSchema
>;
export const FieldGrammarCertificateSpendRedeemer =
  asDataType<FieldGrammarCertificateSpendRedeemer>(
    FieldGrammarCertificateSpendRedeemerSchema,
  );

/** The inline datum the validator computes for a mirrored certificate. */
export const fieldGrammarCertificateDatumCbor = (
  certificate: MidgardFieldGrammarCertificate,
): string => {
  const shared = {
    owner: certificate.owner.toString("hex"),
    tx_id: certificate.txId.toString("hex"),
    field_index: BigInt(certificate.fieldIndex),
    field_hash: certificate.fieldHash.toString("hex"),
    total_length: BigInt(certificate.totalLength),
  };
  const waypoints = certificate.waypoints.toString("hex");
  return Data.to(
    certificate.kind === "Complete"
      ? {
          Complete: {
            ...shared,
            item_count: BigInt(certificate.itemCount),
            waypoints,
          },
        }
      : {
          InProgress: {
            ...shared,
            grammar_checkpoint_hash:
              certificate.grammarCheckpointHash.toString("hex"),
            waypoints,
          },
        },
    FieldGrammarCertificate,
  );
};
