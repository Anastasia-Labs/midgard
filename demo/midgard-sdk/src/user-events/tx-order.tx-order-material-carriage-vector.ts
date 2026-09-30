import {
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekProgramMaterialSidecar,
  hashMidgardCekProgramEnvelope,
  type MidgardCekProgramMaterialEntry,
  verifyMidgardCekProgramMaterialBundle,
} from "@al-ft/midgard-core/cek-proof";
import {
  CML,
  LucidEvolution,
  type ProtocolParameters,
  UTxO,
} from "@lucid-evolution/lucid";

import { MidgardValidators } from "../common.js";
import {
  resolveCertificateReferenceIndex,
  resolveChunkReferenceIndices,
} from "../fraud-proof/field-preimage-carriage.js";
import { CekProgramMaterialDatum } from "../ledger-state.js";
import { type FieldCarriage } from "../native-tx-field-access.js";
import {
  type TxOrderCarriagePlan,
  type TxOrderPlannedFieldCarriage,
} from "./tx-order.derive-tx-order-material.js";
import {
  CEK_SINGLE_PUBLICATION_DATUM_VERSION,
  CekSinglePublicationDatum,
  encodeCekSinglePublicationDatumCbor,
} from "./tx-order.submit-tx-order-config.js";

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

export const MIN_ADA_STABILIZATION_LIMIT = 8;

export const resolveProtocolParameters = async (
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
