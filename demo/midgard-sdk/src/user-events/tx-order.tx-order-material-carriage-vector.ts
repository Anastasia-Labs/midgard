import { UTxO } from "@lucid-evolution/lucid";

import {
  resolveCertificateReferenceIndex,
  resolveChunkReferenceIndices,
} from "../fraud-proof/field-preimage-carriage.js";
import { type FieldCarriage } from "../native-tx-field-access.js";
import {
  type TxOrderCarriagePlan,
  type TxOrderPlannedFieldCarriage,
} from "./tx-order.derive-tx-order-material.js";

/**
 * Whether a referenced field's carriage is present in a reference-input set.
 *
 * Resolvability is exactly what the index-producing functions need, so it is
 * decided by trying them: a `throw` is the answer "not there". Duplicating their
 * matching rules here to answer the same question a second way is how the two
 * answers come to disagree.
 */
export const carriageIsResolvable = (
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
