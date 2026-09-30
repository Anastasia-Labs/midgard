import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core/codec/native-tx-carriage";
import {
  MIDGARD_MAX_TIER3_CHUNK_COUNT,
  type MidgardFieldPreimageCertificate,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import { CML, Data, type UTxO } from "@lucid-evolution/lucid";

import {
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
  FieldPreimageCertificate,
  FieldPreimageCertificateMintRedeemer,
} from "../native-tx-field-access.js";
import {
  fieldPreimagePublicationDatumCbor,
  stabilizedMinimumLovelace,
} from "./field-preimage-carriage.build-unsigned-field-preimage-publication-program.js";

const certificateDatumCbor = (
  certificate: MidgardFieldPreimageCertificate,
): string =>
  Data.to(
    {
      owner: certificate.owner.toString("hex"),
      tx_id: certificate.txId.toString("hex"),
      field_index: BigInt(certificate.fieldIndex),
      field_hash: certificate.fieldHash.toString("hex"),
      total_length: BigInt(certificate.totalLength),
      chunk_digests: certificate.chunkDigests.map((digest) =>
        digest.toString("hex"),
      ),
    },
    FieldPreimageCertificate,
  );

/** The §8.6 manifest a tier-3 plan mints, as the datum and token a builder needs. */
export type FieldPreimageCertification = {
  readonly datumCbor: string;
  readonly assetNameHex: string;
  readonly chunkCount: number;
};

/**
 * The certificate side of a tier-3 plan. Tiers 1–2 certify nothing — the flat
 * field hash authenticates the whole preimage directly (§8.2) — so this refuses
 * rather than inventing a manifest for them.
 */
export const deriveFieldPreimageCertification = (
  plan: MidgardFieldCarriagePlan,
): FieldPreimageCertification => {
  const certificate = plan.certificate;
  if (certificate === null) {
    throw new Error(
      "only tier-3 carriage is certified — tiers 1–2 authenticate against the flat field hash",
    );
  }
  if (certificate.chunkDigests.length > MIDGARD_MAX_TIER3_CHUNK_COUNT) {
    throw new Error("§8.3 bounds tier-3 carriage at three chunks");
  }
  return {
    datumCbor: certificateDatumCbor(certificate),
    // #606: one constant name for every certificate of the policy; the datum
    // (including the mint-welded `field_hash`) is where identity lives.
    assetNameHex: FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX,
    chunkCount: certificate.chunkDigests.length,
  };
};

export const minimumLovelaceForFieldPreimageCertificate = ({
  certificateAddress,
  certification,
  certificatePolicyId,
  coinsPerUtxoByte,
}: {
  readonly certificateAddress: string;
  readonly certification: FieldPreimageCertification;
  readonly certificatePolicyId: string;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  // The certificate output carries exactly one asset of the policy (§8.6), and
  // a token is worth ~180 lovelace of min-Ada on its own, so the value has to
  // be built with the asset present rather than as a bare coin.
  const assets = CML.MultiAsset.new();
  assets.set(
    CML.ScriptHash.from_hex(certificatePolicyId),
    CML.AssetName.from_hex(certification.assetNameHex),
    1n,
  );
  return stabilizedMinimumLovelace({
    addressBech32: certificateAddress,
    datumCbor: certification.datumCbor,
    coinsPerUtxoByte,
    assets,
    label: "field-preimage certificate",
  });
};

/**
 * §8.6's `Certify` redeemer. `chunk_ref_input_indices` is all-chunks-positional
 * and must be derived by {@link resolveChunkReferenceIndices}, which orders
 * the reference inputs the way the ledger does; this function only bounds and
 * validates what it is given.
 */
export const certifyFieldPreimageRedeemer = ({
  compactCbor,
  sourceKind,
  witnessSetCompactCbor,
  chunkRefInputIndices,
  outputIndex,
}: {
  readonly compactCbor: string;
  readonly sourceKind: 0n | 1n;
  readonly witnessSetCompactCbor: string;
  readonly chunkRefInputIndices: readonly number[];
  readonly outputIndex: number;
}): string => {
  if (chunkRefInputIndices.length > MIDGARD_MAX_TIER3_CHUNK_COUNT) {
    throw new Error("§8.3 bounds tier-3 carriage at three chunks");
  }
  if (
    chunkRefInputIndices.some(
      (index) => !Number.isSafeInteger(index) || index < 0,
    )
  ) {
    throw new Error("reference-input indices must be non-negative integers");
  }
  return Data.to(
    {
      Certify: {
        source_kind: sourceKind,
        compact_cbor: compactCbor,
        witness_set_compact_cbor: witnessSetCompactCbor,
        chunk_ref_input_indices: chunkRefInputIndices.map((index) =>
          BigInt(index),
        ),
        output_index: BigInt(outputIndex),
      },
    },
    FieldPreimageCertificateMintRedeemer,
  );
};

/** §8.6's `Retire` redeemer — the burn path; the spend handler holds authority. */
export const retireFieldPreimageCertificateRedeemer = (): string =>
  Data.to("Retire", FieldPreimageCertificateMintRedeemer);

/**
 * Locates each of a plan's chunks in a resolved reference-input set by
 * **content**, never by `OutputReference`.
 *
 * §8.7 makes content addressing mandatory: nothing in the dispute machine may
 * reference carriage by UTxO identity, precisely so that a republished chunk is
 * interchangeable with the one it replaced. Matching on the datum's bytes is
 * what makes healing transparent to a builder — the healed UTxO has a different
 * transaction id and the same content, and this function cannot tell the
 * difference, which is the point.
 */
export const resolveChunkReferenceIndices = ({
  plan,
  referenceInputs,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly referenceInputs: readonly UTxO[];
}): readonly number[] => {
  // Positional indices are indices into the **ledger's** reference-input list,
  // and the ledger holds reference inputs as a set serialised in canonical
  // `(txHash, outputIndex)` order — not in the order a builder happened to add
  // them. Sorting here is the same discipline `requireReferenceInputIndex` keeps
  // for every other positional redeemer in this package; without it the indices
  // are right until the first transaction whose UTxOs sort differently from the
  // order they were collected in, which is a defect that appears at random.
  //
  // The caller must therefore pass the transaction's *complete* reference-input
  // set. For a certification that is exactly the chunks, which is why the
  // builder below can pass them directly.
  const ordered = [...referenceInputs].sort(compareOutRefs);
  // Matched positions are consumed. Two chunks of one preimage can be
  // byte-identical — the §8.4 split is positional, not content-deduplicated, so
  // a preimage with a repeating `chunk_bytes_k`-byte period produces repeating
  // chunks —
  // and a plain `findIndex` would return the same position for both, naming one
  // UTxO twice in the redeemer and never naming the other. The certification
  // would then be checked against the wrong chunk for one of the two positions.
  // Consuming keeps the mapping injective, which is what a positional redeemer
  // over a *list* means.
  const claimed = new Set<number>();
  return plan.publications.map((publication) => {
    const expected = fieldPreimagePublicationDatumCbor(publication.bytes);
    const index = ordered.findIndex(
      (utxo, position) => !claimed.has(position) && utxo.datum === expected,
    );
    if (index === -1) {
      throw new Error(
        `chunk ${publication.chunkIndex.toString()} (${publication.digest.toString(
          "hex",
        )}) is not among the transaction's reference inputs`,
      );
    }
    claimed.add(index);
    return index;
  });
};

/**
 * Locates a tier-3 certificate in a resolved reference-input set by its
 * **datum**, never by `OutputReference` (§8.7) and — since #606's constant
 * asset name — never by token alone.
 *
 * The token names the policy and nothing else: every certificate of the
 * policy carries the same constant name, so two certificates for different
 * fields (or different transactions) are same-name tokens and the datum is
 * where identity lives. This resolver matches exactly what the door checks —
 * one unit of the policy's constant-name token, over a datum whose
 * `(tx_id, field_index, field_hash)` triple is the plan's — so a transaction
 * carrying several certificates resolves each field to its own manifest. Two
 * certificates matching the triple are interchangeable by construction (§8.7
 * healing; they differ at most in `owner`), so the first match is as good as
 * any — but it is the canonically-first, not the first the caller collected,
 * which is why the sort happens before the search rather than after it.
 *
 * The discrimination is pinned by
 * `demo/midgard-sdk/tests/field-preimage-certificate-selection.test.ts`,
 * which is the only place in the repo that puts **two** same-name certificates
 * in one reference-input set. Every other certificate fixture carries exactly
 * one, and against a single candidate a token-only resolver and this one are
 * indistinguishable.
 */
export const resolveCertificateReferenceIndex = ({
  certificatePolicyId,
  txIdHex,
  fieldIndex,
  fieldHashHex,
  referenceInputs,
  label,
}: {
  readonly certificatePolicyId: string;
  readonly txIdHex: string;
  readonly fieldIndex: number;
  readonly fieldHashHex: string;
  readonly referenceInputs: readonly UTxO[];
  readonly label: string;
}): number => {
  const unit = `${certificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX}`;
  const matchesDatum = (datum: string | null | undefined): boolean => {
    if (datum === null || datum === undefined) {
      return false;
    }
    try {
      const certificate = Data.from(datum, FieldPreimageCertificate);
      return (
        certificate.tx_id === txIdHex &&
        certificate.field_index === BigInt(fieldIndex) &&
        certificate.field_hash === fieldHashHex
      );
    } catch {
      return false;
    }
  };
  // Canonical `(txHash, outputIndex)` order, for the same reason
  // `resolveChunkReferenceIndices` sorts: positional indices are indices into
  // the ledger's reference-input list, not into the order a builder collected.
  const index = [...referenceInputs]
    .sort(compareOutRefs)
    .findIndex(
      (utxo) => (utxo.assets[unit] ?? 0n) === 1n && matchesDatum(utxo.datum),
    );
  if (index === -1) {
    throw new Error(
      `${label} §8.6 certificate (policy ${certificatePolicyId}, tx ${txIdHex}, field ${fieldIndex.toString()}) is not among the transaction's reference inputs`,
    );
  }
  return index;
};
