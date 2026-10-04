import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  type MidgardFieldCarriage,
  midgardFieldCommitment,
  selectMidgardFieldCarriageTier,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { Data, type UTxO } from "@lucid-evolution/lucid";

import { resolveMidgardFieldCarriageAgainstReferenceInputs } from "./fraud-proof/field-preimage-carriage.js";
import { type ValidationAuxiliaryWitness } from "./fraud-proof/validation-auxiliary-witness.js";
import { buildValidationAuxiliaryWitnessSchema } from "./fraud-proof/validation-auxiliary-witness.validation-auxiliary-witness-schema.js";
import { type FieldCarriage } from "./native-tx-field-access.js";
import {
  retainedValidationTransactionSource,
  validateRetainedValidationTransactionIdentity,
} from "./retained-validation-auxiliary.transaction-source.js";
export {
  retainedValidationTransactionSource,
  validateRetainedValidationTransactionIdentity,
} from "./retained-validation-auxiliary.transaction-source.js";

/** A bounded reference to a field in the authenticated event transaction. */
export const RetainedFieldSourceSchema = Data.Object({
  field_index: Data.Integer(),
  total_length: Data.Integer(),
  commitment: Data.Bytes(),
});
export const RetainedValidationAuxiliaryWitnessSchema =
  buildValidationAuxiliaryWitnessSchema(
    Data.Object({ raw_field_source: RetainedFieldSourceSchema }),
  );
export type RetainedValidationAuxiliaryWitness = Data.Static<
  typeof RetainedValidationAuxiliaryWitnessSchema
>;
export type RetainedFieldSource = Data.Static<typeof RetainedFieldSourceSchema>;

export const retainedValidationFieldSource = (
  auxiliary: RetainedValidationAuxiliaryWitness,
): RetainedFieldSource | undefined => {
  if (typeof auxiliary !== "object") return undefined;
  if ("TransactionFieldChunkWitness" in auxiliary) {
    const witness = auxiliary.TransactionFieldChunkWitness;
    if (witness.field_index !== witness.carriage.raw_field_source.field_index)
      throw new Error("Retained field source index differs from its witness");
    return witness.carriage.raw_field_source;
  }
  if ("RequiredSignerItemWitness" in auxiliary) {
    const source =
      auxiliary.RequiredSignerItemWitness.carriage.raw_field_source;
    if (source.field_index !== 4n)
      throw new Error("Retained required signer source must be field 4");
    return source;
  }
  if ("TransactionRedeemerItemBeginWitness" in auxiliary)
    return auxiliary.TransactionRedeemerItemBeginWitness.carriage
      .raw_field_source;
  if ("TransactionFieldItemWitness" in auxiliary)
    return auxiliary.TransactionFieldItemWitness.carriage.raw_field_source;
  return undefined;
};

/** Derive exact submitted field bytes once per authenticated event, including forced defects. */
export const retainedValidationTransactionFields = (
  canonicalTransactionCbor: Uint8Array,
  sourceKind: "normal" | "forced",
): readonly Uint8Array[] =>
  retainedValidationTransactionSource(canonicalTransactionCbor, sourceKind)
    .fields;

export const validateRetainedValidationFieldSource = (
  auxiliary: RetainedValidationAuxiliaryWitness,
  fields: readonly Uint8Array[],
): void => {
  const source = retainedValidationFieldSource(auxiliary);
  if (source === undefined) return;
  if (source.field_index < 0n || source.field_index >= BigInt(fields.length))
    throw new Error("Retained field source index is outside the transaction");
  const field = fields[Number(source.field_index)]!;
  if (
    source.total_length !== BigInt(field.length) ||
    !midgardFieldCommitment(field).equals(Buffer.from(source.commitment, "hex"))
  )
    throw new Error(
      "Retained field source length or commitment differs from the authenticated transaction",
    );
};

/** Resolve against the consuming transaction's actual references before staging its evidence. */
export const materializeRetainedValidationAuxiliaryWitness = ({
  auxiliary,
  plan,
  transactionId,
  transactionCommitment,
  canonicalTransactionCbor,
  sourceKind,
  referenceInputs,
  certificatePolicyId,
}: {
  readonly auxiliary: RetainedValidationAuxiliaryWitness;
  readonly plan?: MidgardFieldCarriagePlan;
  readonly transactionId: Uint8Array;
  readonly transactionCommitment: Uint8Array;
  readonly canonicalTransactionCbor: Uint8Array;
  readonly sourceKind: "normal" | "forced";
  readonly referenceInputs: readonly UTxO[];
  readonly certificatePolicyId?: string;
}): ValidationAuxiliaryWitness => {
  const source = retainedValidationFieldSource(auxiliary);
  if (source === undefined) return auxiliary as ValidationAuxiliaryWitness;
  if (source.field_index < 0n || source.field_index > 8n)
    throw new Error("Retained field source index is outside the transaction");
  const transactionSource = retainedValidationTransactionSource(
    canonicalTransactionCbor,
    sourceKind,
  );
  validateRetainedValidationTransactionIdentity(
    transactionSource,
    transactionId,
    transactionCommitment,
  );
  validateRetainedValidationFieldSource(auxiliary, transactionSource.fields);
  const fieldPreimage = Buffer.from(
    transactionSource.fields[Number(source.field_index)]!,
  );
  if (plan === undefined || plan.fieldIndex !== Number(source.field_index))
    throw new Error("Retained field source needs its matching carriage plan");
  if (
    !plan.txId.equals(Buffer.from(transactionId)) ||
    plan.totalLength !== fieldPreimage.length ||
    !plan.commitment.equals(midgardFieldCommitment(fieldPreimage))
  )
    throw new Error(
      "Retained field carriage plan differs from its transaction source",
    );
  const planBytes =
    plan.inlinePreimage ??
    Buffer.concat(plan.publications.map((publication) => publication.bytes));
  if (!planBytes.equals(fieldPreimage))
    throw new Error(
      "Retained field carriage plan substituted its source bytes",
    );
  const resolved: MidgardFieldCarriage =
    resolveMidgardFieldCarriageAgainstReferenceInputs({
      plan,
      referenceInputs,
      ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
    });
  if (
    resolved.carriage !== selectMidgardFieldCarriageTier(fieldPreimage.length)
  )
    throw new Error("Resolved retained field carriage has the wrong tier");
  let carriage: FieldCarriage;
  switch (resolved.carriage) {
    case "Inline":
      if (!resolved.preimage.equals(fieldPreimage))
        throw new Error(
          "Resolved retained field carriage substituted its source bytes",
        );
      carriage = { Inline: { preimage: fieldPreimage.toString("hex") } };
      break;
    case "RawUtxo":
      carriage = {
        RawUtxo: { ref_input_index: BigInt(resolved.refInputIndex) },
      };
      break;
    case "Certified":
      carriage = {
        Certified: {
          cert_ref_input_index: BigInt(resolved.certRefInputIndex),
          chunk_ref_input_indices: resolved.chunkRefInputIndices.map(BigInt),
        },
      };
      break;
  }
  if (typeof auxiliary !== "object")
    throw new Error("Retained field source is missing");
  if ("TransactionFieldChunkWitness" in auxiliary)
    return {
      TransactionFieldChunkWitness: {
        ...auxiliary.TransactionFieldChunkWitness,
        carriage,
      },
    };
  if ("RequiredSignerItemWitness" in auxiliary)
    return {
      RequiredSignerItemWitness: {
        ...auxiliary.RequiredSignerItemWitness,
        carriage,
      },
    };
  if ("TransactionRedeemerItemBeginWitness" in auxiliary)
    return { TransactionRedeemerItemBeginWitness: { carriage } };
  return { TransactionFieldItemWitness: { carriage } };
};
