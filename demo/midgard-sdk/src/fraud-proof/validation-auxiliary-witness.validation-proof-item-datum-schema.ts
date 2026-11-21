import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import { ValidationAuxiliaryWitnessSchema } from "./validation-auxiliary-witness.validation-auxiliary-witness-schema.js";

export type ValidationAuxiliaryWitness = Data.Static<
  typeof ValidationAuxiliaryWitnessSchema
>;

export const ValidationAuxiliaryWitness =
  asDataType<ValidationAuxiliaryWitness>(ValidationAuxiliaryWitnessSchema);

/**
 * A field preimage published once, at the proof-item script address, for the
 * `CanonicalDecode` complete-item steps of one disputed transaction to reach by
 * reference input instead of re-carrying it in every step's redeemer.
 *
 * **It publishes the field's whole §5.1 preimage, not one item.** Under the
 * retired counted scheme it published one item's bytes beside an `ItemProofV1`
 * opening them against the field commitment — an opening §4 made unsatisfiable
 * (#592). Under §8 the unit that authenticates is the whole preimage, so the
 * whole preimage is what a publication carries;
 * `canonical_decode_item_observe_v1`'s `ObserveReference` arm wraps it as
 * `Inline` carriage and the door hashes it once against the committed field
 * hash. `transaction_id` and `transaction_commitment` are what stop a look-alike
 * UTxO passing a preimage off as belonging to a different dispute.
 *
 * Aiken source of truth:
 * `onchain/aiken/lib/midgard/validation-machine/`.
 */
export const ValidationProofItemDatumSchema = Data.Object({
  version: Data.Integer(),
  transaction_id: Data.Bytes({ minLength: 32, maxLength: 32 }),
  transaction_commitment: Data.Bytes({ minLength: 32, maxLength: 32 }),
  field_preimage: Data.Bytes(),
});

export type ValidationProofItemDatum = Data.Static<
  typeof ValidationProofItemDatumSchema
>;

export const ValidationProofItemDatum = asDataType<ValidationProofItemDatum>(
  ValidationProofItemDatumSchema,
);
