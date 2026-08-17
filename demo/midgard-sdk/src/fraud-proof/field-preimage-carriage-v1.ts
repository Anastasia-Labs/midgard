import {
  MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES_V1,
  midgardCarriagePublicationBytesV1,
  type MidgardFieldCarriagePlanV1,
  type MidgardFieldPublicationV1,
} from "@al-ft/midgard-core/codec/native-tx-carriage-v1";
import {
  MIDGARD_MAX_TIER3_CHUNK_COUNT_V1,
  MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1,
  type MidgardFieldCarriageV1,
  type MidgardFieldPreimageCertificateV1,
} from "@al-ft/midgard-core/codec/native-tx-field-access-v1";
import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  CML,
  Data,
  type LucidEvolution,
  type MintingPolicy,
  type ProtocolParameters,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1,
  FieldPreimageCertificateMintRedeemerV1,
  FieldPreimageCertificateV1,
} from "@/native-tx-field-access-v1.js";

/**
 * The off-chain publish, certify and heal tooling for the `docs/spec/midgard-tx.md`
 * §8 field-preimage carriage ladder — the transaction-shaped half of
 * `@al-ft/midgard-core/codec/native-tx-carriage-v1`.
 *
 * The core module decides *what* has to exist on-chain; this one builds the
 * transactions that make it exist. The split is the same one the Aiken side
 * keeps: `lib/midgard/native-tx-carriage-v1.ak` owns the content rules and
 * `validators/field-preimage-certificate.ak` owns the transaction shape.
 *
 * **The tier is branched on where the transactions differ, and nowhere else.**
 * Publishing nothing (tier 1), one UTxO (tier 2) or `n` chunks plus a
 * certificate (tier 3) are genuinely different transactions, and pretending
 * otherwise would only move the branch somewhere less visible. What never
 * branches is the *consumer*: a step takes the `carriage` and `referenceInputs`
 * that `layOutMidgardFieldCarriageV1` produced and reads items off the view,
 * and there is no tier-shaped argument anywhere on that path.
 *
 * **Publication is guarded (§8.3 erratum E1).** `chunk_bytes_k` is pinned at
 * 15,148 — E1's repaired value, the reserve-clearing publication frontier — so
 * an honest §8.4 split now produces chunks that all publish, a 15,148-byte chunk
 * measuring 15,872 signed bytes against a 16,384-byte `maxTxSize`. The guard
 * stays because the frontier is a *measurement*, not an invariant of the split:
 * {@link buildUnsignedFieldPreimagePublicationV1Program} checks every publication
 * against it before building, so a hand-assembled chunk list, a plan produced by
 * an older chunker, or a deliberately raised limit is refused rather than handed
 * back as a transaction the ledger will reject. Before the repair the guard
 * refused every tier-3 plan there was, which is the outage E1 records.
 *
 * **Nothing here is privileged (§8.7).** Publication and certification are
 * permissionless: no operator role, no allowlist, no signature required at
 * mint. That is what makes a yanked publication healable, and the healing
 * builders below are not a separate mechanism — they are the ordinary
 * publication builders run by a second identity over the same preimage, which
 * is the whole of what §8.7 promises.
 */

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
 * The smallest lovelace that keeps an output with this datum above the
 * per-byte minimum, found by the same fixpoint every other publication builder
 * in this package uses: min-Ada depends on the output's own serialised size,
 * which depends on the coin field, so raising the coin can raise the minimum
 * again.
 */
const stabilizedMinimumLovelace = ({
  addressBech32,
  datumCbor,
  coinsPerUtxoByte,
  assets,
  label,
}: {
  readonly addressBech32: string;
  readonly datumCbor: string;
  readonly coinsPerUtxoByte: bigint;
  readonly assets?: CML.MultiAsset;
  readonly label: string;
}): bigint => {
  const address = CML.Address.from_bech32(addressBech32);
  const datum = CML.DatumOption.new_datum(
    CML.PlutusData.from_cbor_hex(datumCbor),
  );
  let lovelace = 0n;
  for (let attempt = 0; attempt < MIN_ADA_STABILIZATION_LIMIT; attempt += 1) {
    const value =
      assets === undefined
        ? CML.Value.from_coin(lovelace)
        : CML.Value.new(lovelace, assets);
    const required = CML.min_ada_required(
      CML.TransactionOutput.new(address, value, datum, undefined),
      coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error(`Failed to stabilize ${label} min-Ada calculation.`);
};

/**
 * §8.5. Raw carriage is published as a **nothing-but-bytes inline datum** — a
 * Plutus Data byte string and nothing else, which is exactly what the on-chain
 * `raw_chunk_bytes` reads back through `un_b_data`. Any wrapper at all, however
 * harmless it looked, would make the datum undecodable to every consumer.
 */
export const fieldPreimagePublicationDatumCborV1 = (
  bytes: Uint8Array,
): string => Data.to(Buffer.from(bytes).toString("hex"));

/**
 * The read-back twin of {@link fieldPreimagePublicationDatumCborV1} — the
 * off-chain counterpart of the on-chain `un_b_data` on a raw carriage
 * reference input.
 *
 * It is what turns a resolved UTxO into the `inlineDatumBytes` the field-access
 * door expects, and it is fail-closed: a datum that is not a bare byte string
 * is not raw carriage, and returning something plausible for one would let
 * wrong bytes reach a hash check that would then merely fail later and less
 * informatively.
 */
export const fieldPreimagePublicationBytesV1 = (datumCbor: string): Buffer => {
  const decoded = Data.from(datumCbor);
  if (typeof decoded !== "string") {
    throw new Error(
      "raw carriage datum is not a nothing-but-bytes inline datum (§8.5)",
    );
  }
  return Buffer.from(decoded, "hex");
};

/** One raw carriage output, ready to pay to the publisher's own key address. */
export type FieldPreimagePublicationOutputV1 = {
  /** §8.4 chunk index; `0` for a tier-2 whole-preimage publication. */
  readonly chunkIndex: number;
  readonly datumCbor: string;
  readonly byteLength: number;
  /** `blake2b_256` of the published bytes — §8.7's content address. */
  readonly digestHex: string;
};

/**
 * The raw carriage a plan requires. Empty under tier 1, which carries its
 * preimage in the step's own redeemer and publishes nothing.
 */
export const fieldPreimagePublicationOutputsV1 = (
  plan: MidgardFieldCarriagePlanV1,
): readonly FieldPreimagePublicationOutputV1[] =>
  plan.publications.map((publication: MidgardFieldPublicationV1) => ({
    chunkIndex: publication.chunkIndex,
    datumCbor: fieldPreimagePublicationDatumCborV1(publication.bytes),
    byteLength: publication.bytes.length,
    digestHex: publication.digest.toString("hex"),
  }));

export const minimumLovelaceForFieldPreimagePublicationV1 = ({
  publisherAddress,
  output,
  coinsPerUtxoByte,
}: {
  readonly publisherAddress: string;
  readonly output: FieldPreimagePublicationOutputV1;
  readonly coinsPerUtxoByte: bigint;
}): bigint =>
  stabilizedMinimumLovelace({
    addressBech32: publisherAddress,
    datumCbor: output.datumCbor,
    coinsPerUtxoByte,
    label: "field-preimage publication",
  });

/**
 * §8.3 erratum E1's guard, as the one predicate that stands between a plan and
 * a transaction the ledger would reject.
 *
 * `maxPublicationBytes` is a bound on the **payload**, because that is what a
 * caller holds and can act on; it defaults to
 * {@link MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES_V1}, the largest payload whose
 * signed publication clears `maxTxSize` with the 512-byte reserve. The refusal
 * quotes the *transaction* size that payload would produce, so the message says
 * why rather than only what: this many payload bytes publish as a transaction
 * of this size, and that does not fit. The frontier measurement is the one
 * caller with a reason to raise the bound, and it passes its own explicitly
 * rather than reaching around the check — but only within the §5.4 aggregate
 * field cap, so raising it can reach the far side of the frontier and cannot
 * turn the guard off.
 */
export const assertFieldPreimagePublicationFitsV1 = ({
  publication,
  maxPublicationBytes = MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES_V1,
}: {
  readonly publication: FieldPreimagePublicationOutputV1;
  readonly maxPublicationBytes?: number;
}): void => {
  // The override is bounded, because an unbounded one is not a measurement
  // affordance but a way to switch the guard off from a caller's argument list.
  // §5.4 caps a whole transaction's field bytes at
  // `MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1`, so no admissible
  // publication is ever larger than that at any value of `K`; a caller asking
  // for more is not measuring the ladder's frontier, it is leaving the ladder.
  if (
    !Number.isSafeInteger(maxPublicationBytes) ||
    maxPublicationBytes < 1 ||
    maxPublicationBytes > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1
  ) {
    throw new Error(
      `maxPublicationBytes must be between 1 and the §5.4 aggregate field cap ` +
        `(${MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES_V1.toString()}); got ` +
        `${maxPublicationBytes.toString()}`,
    );
  }
  if (publication.byteLength <= maxPublicationBytes) {
    return;
  }
  const transactionBytes = midgardCarriagePublicationBytesV1(
    publication.byteLength,
  );
  throw new Error(
    `chunk ${publication.chunkIndex.toString()} is ${publication.byteLength.toString()} bytes, ` +
      `which publishes as a ${transactionBytes.toString()}-byte signed transaction and exceeds the ` +
      `${maxPublicationBytes.toString()}-byte publishable frontier (§8.3 erratum E1). ` +
      "`chunk_bytes_k` is pinned at 15,148, the publishable frontier (§8.3 E1's repair), so an " +
      "honest §8.4 split does not produce a chunk this large; check the chunk list's provenance.",
  );
};

/**
 * Publishes **one** raw carriage output: an ada-only UTxO at the publisher's
 * own key address carrying the bytes as a nothing-but-bytes inline datum
 * (§8.5).
 *
 * **One publication per transaction, not one per plan.** A chunk is sized to
 * fill a transaction: against a 16,384-byte `maxTxSize`, a full chunk's signed
 * publication measures 15,872 bytes at the repaired `K` (§8.3 erratum E1), which
 * leaves exactly the 512-byte reliability reserve and no room for a second chunk.
 * A builder that tried to share a transaction would fail at exactly the sizes
 * tier 3 exists for. A caller publishes a tier-3 plan by
 * iterating {@link fieldPreimagePublicationOutputsV1}; the publications are
 * independent, so a run interrupted half-way is resumed by publishing whatever
 * is missing, and §8.7's content addressing means it does not matter who
 * publishes which.
 *
 * **It refuses what it cannot publish.** At the repaired `K` an honest §8.4 plan
 * has no such chunk, so the guard is quiet on every real publication; what it
 * still catches is a chunk list that did not come from this chunker. Before E1's
 * repair the first chunk of *every* tier-3 plan tripped it, which is how the
 * outage that erratum records became visible at build time rather than at
 * submission. `maxPublicationBytes` exists for the frontier measurement and for
 * nothing else.
 */
export const buildUnsignedFieldPreimagePublicationV1Program = (
  lucid: LucidEvolution,
  {
    publication,
    publisherAddress,
    maxPublicationBytes,
  }: {
    readonly publication: FieldPreimagePublicationOutputV1;
    readonly publisherAddress: string;
    readonly maxPublicationBytes?: number;
  },
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      assertFieldPreimagePublicationFitsV1({
        publication,
        ...(maxPublicationBytes === undefined ? {} : { maxPublicationBytes }),
      });
      const protocolParameters = await resolveProtocolParameters(lucid);
      return await lucid
        .newTx()
        .pay.ToAddressWithData(
          publisherAddress,
          { kind: "inline", value: publication.datumCbor },
          {
            lovelace: minimumLovelaceForFieldPreimagePublicationV1({
              publisherAddress,
              output: publication,
              coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
            }),
          },
        )
        .complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new Error(
        `Failed to publish field-preimage carriage: ${
          cause instanceof Error ? cause.message : String(cause)
        }`,
      ),
  });

const certificateDatumCbor = (
  certificate: MidgardFieldPreimageCertificateV1,
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
    FieldPreimageCertificateV1,
  );

/** The §8.6 manifest a tier-3 plan mints, as the datum and token a builder needs. */
export type FieldPreimageCertificationV1 = {
  readonly datumCbor: string;
  readonly assetNameHex: string;
  readonly chunkCount: number;
};

/**
 * The certificate side of a tier-3 plan. Tiers 1–2 certify nothing — the flat
 * field hash authenticates the whole preimage directly (§8.2) — so this refuses
 * rather than inventing a manifest for them.
 */
export const deriveFieldPreimageCertificationV1 = (
  plan: MidgardFieldCarriagePlanV1,
): FieldPreimageCertificationV1 => {
  const certificate = plan.certificate;
  if (certificate === null) {
    throw new Error(
      "only tier-3 carriage is certified — tiers 1–2 authenticate against the flat field hash",
    );
  }
  if (certificate.chunkDigests.length > MIDGARD_MAX_TIER3_CHUNK_COUNT_V1) {
    throw new Error("§8.3 bounds tier-3 carriage at three chunks");
  }
  return {
    datumCbor: certificateDatumCbor(certificate),
    // #606: one constant name for every certificate of the policy; the datum
    // (including the mint-welded `field_hash`) is where identity lives.
    assetNameHex: FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1,
    chunkCount: certificate.chunkDigests.length,
  };
};

export const minimumLovelaceForFieldPreimageCertificateV1 = ({
  certificateAddress,
  certification,
  certificatePolicyId,
  coinsPerUtxoByte,
}: {
  readonly certificateAddress: string;
  readonly certification: FieldPreimageCertificationV1;
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
 * and must be derived by {@link resolveChunkReferenceIndicesV1}, which orders
 * the reference inputs the way the ledger does; this function only bounds and
 * validates what it is given.
 */
export const certifyFieldPreimageRedeemerV1 = ({
  compactCbor,
  witnessSetCompactCbor,
  chunkRefInputIndices,
  outputIndex,
}: {
  readonly compactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly chunkRefInputIndices: readonly number[];
  readonly outputIndex: number;
}): string => {
  if (chunkRefInputIndices.length > MIDGARD_MAX_TIER3_CHUNK_COUNT_V1) {
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
        compact_cbor: compactCbor,
        witness_set_compact_cbor: witnessSetCompactCbor,
        chunk_ref_input_indices: chunkRefInputIndices.map((index) =>
          BigInt(index),
        ),
        output_index: BigInt(outputIndex),
      },
    },
    FieldPreimageCertificateMintRedeemerV1,
  );
};

/** §8.6's `Retire` redeemer — the burn path; the spend handler holds authority. */
export const retireFieldPreimageCertificateRedeemerV1 = (): string =>
  Data.to("Retire", FieldPreimageCertificateMintRedeemerV1);

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
export const resolveChunkReferenceIndicesV1 = ({
  plan,
  referenceInputs,
}: {
  readonly plan: MidgardFieldCarriagePlanV1;
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
    const expected = fieldPreimagePublicationDatumCborV1(publication.bytes);
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
 * `demo/midgard-sdk/tests/field-preimage-certificate-selection-v1.test.ts`,
 * which is the only place in the repo that puts **two** same-name certificates
 * in one reference-input set. Every other certificate fixture carries exactly
 * one, and against a single candidate a token-only resolver and this one are
 * indistinguishable.
 */
export const resolveCertificateReferenceIndexV1 = ({
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
  const unit = `${certificatePolicyId}${FIELD_PREIMAGE_CERTIFICATE_ASSET_NAME_HEX_V1}`;
  const matchesDatum = (datum: string | null | undefined): boolean => {
    if (datum === null || datum === undefined) {
      return false;
    }
    try {
      const certificate = Data.from(datum, FieldPreimageCertificateV1);
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
  // `resolveChunkReferenceIndicesV1` sorts: positional indices are indices into
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

/**
 * Resolves one field's §8 carriage against the reference-input set of the
 * transaction **whose door will read it** (#600).
 *
 * The tier is §8.4's partition over the preimage's length — never a choice made
 * here — and the indices are resolved by content through the two resolvers
 * above, so nothing about a carriage produced here is asserted rather than
 * located.
 *
 * `referenceInputs` must be the door-running transaction's **complete**
 * reference-input set, not just the carriage UTxOs. Positional indices count
 * into the ledger's canonically-sorted list, and a dispute step references more
 * than its carriage — the published spending validator arrives the same way
 * (`readFrom`) and sorts into the same list. Passing only the carriage produces
 * indices that are right until the first transaction that references anything
 * else, which is every real one. {@link
 * assertMidgardFieldCarriageResolvesAtDoorV1} is the check that catches it.
 */
export const resolveMidgardFieldCarriageAgainstReferenceInputsV1 = ({
  plan,
  referenceInputs,
  certificatePolicyId,
}: {
  readonly plan: MidgardFieldCarriagePlanV1;
  readonly referenceInputs: readonly UTxO[];
  readonly certificatePolicyId?: string;
}): MidgardFieldCarriageV1 => {
  if (plan.tier === "Inline") {
    if (plan.inlinePreimage === null) {
      throw new Error(
        `tier-1 plan for field ${plan.fieldIndex.toString()} carries no preimage`,
      );
    }
    return { carriage: "Inline", preimage: plan.inlinePreimage };
  }
  const chunkIndices = resolveChunkReferenceIndicesV1({
    plan,
    referenceInputs,
  });
  if (plan.tier === "RawUtxo") {
    const refInputIndex = chunkIndices[0];
    if (refInputIndex === undefined || chunkIndices.length !== 1) {
      throw new Error(
        `tier-2 plan for field ${plan.fieldIndex.toString()} must resolve exactly one publication`,
      );
    }
    return { carriage: "RawUtxo", refInputIndex };
  }
  if (certificatePolicyId === undefined) {
    throw new Error(
      `tier-3 carriage for field ${plan.fieldIndex.toString()} requires the §8.6 certificate policy id`,
    );
  }
  if (plan.certificate === null) {
    throw new Error(
      `tier-3 plan for field ${plan.fieldIndex.toString()} carries no certificate`,
    );
  }
  return {
    carriage: "Certified",
    certRefInputIndex: resolveCertificateReferenceIndexV1({
      certificatePolicyId,
      txIdHex: plan.txId.toString("hex"),
      fieldIndex: plan.fieldIndex,
      fieldHashHex: plan.commitment.toString("hex"),
      referenceInputs,
      label: `field ${plan.fieldIndex.toString()}`,
    }),
    chunkRefInputIndices: chunkIndices,
  };
};

/**
 * Refuses a carriage whose positional indices would not resolve to the same
 * UTxOs in the transaction that actually runs the §8.8 door (#600, ruling D3-A).
 *
 * A carriage is fixed one transaction *before* the door reads it: the auxiliary
 * is hashed into `evidence_hash` by the PrepareSelected transaction
 * (`onchain/aiken/lib/midgard/validation-resolution-v1.ak:163-182`) and only
 * dereferenced later, at the stage where the door runs — for the complete-item
 * route that is `canonical_decode_item_observe_v1`, three transactions further
 * on. Between those points a builder can change the reference-input set without
 * noticing, and the failure is silent in the worst way: the indices are
 * well-formed, they simply name the wrong UTxOs, and the step fails on L1 after
 * the evidence hash is already staged and can only be re-staged.
 *
 * So the indices are re-resolved against the door transaction's final set and
 * any difference refuses here, off-chain, while re-staging is still free.
 *
 * **Its production call site is the observe-stage dispute builder**, in
 * `submitValidationDisputeSemanticResolution`
 * (`demo/midgard-fault-proofs/src/validation-dispute/submit.ts`), immediately
 * before the `Validation canonical item observation` stage is built. That stage
 * is the one place the §8.8 door runs for the complete-item path
 * (`validators/fraud-proofs/validation-trace/canonical-decode-item-observe-v1.ak`),
 * and its builder is the only caller that holds both the committed carriage —
 * read straight out of the staged auxiliary the PrepareSelected transaction
 * hashed into `evidence_hash` — and the complete reference-input set that
 * transaction will read. The guard is called there with exactly the UTxOs the
 * stage `readFrom`s, so a divergence refuses before a submission burns the
 * staged evidence.
 *
 * It is additionally pinned by its own unit tests
 * (`demo/midgard-sdk/tests/field-preimage-carriage-door-v1.test.ts`) and driven
 * over real ledger-resolved carriage by the tiers-2/3 emulator leg
 * (`demo/midgard-validation/tests/complete-item-carriage-tiers-emulator-v1.test.ts`),
 * which submits the same door transaction the guard admits and the one it
 * refuses.
 */
export const assertMidgardFieldCarriageResolvesAtDoorV1 = ({
  carriage,
  plan,
  doorReferenceInputs,
  certificatePolicyId,
  label,
}: {
  readonly carriage: MidgardFieldCarriageV1;
  readonly plan: MidgardFieldCarriagePlanV1;
  readonly doorReferenceInputs: readonly UTxO[];
  readonly certificatePolicyId?: string;
  readonly label: string;
}): void => {
  if (carriage.carriage === "Inline") {
    // Tier 1 indexes nothing, so there is nothing that can drift.
    return;
  }
  const atDoor = resolveMidgardFieldCarriageAgainstReferenceInputsV1({
    plan,
    referenceInputs: doorReferenceInputs,
    ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
  });
  const disagreement = ((): string | null => {
    if (atDoor.carriage !== carriage.carriage) {
      return `tier ${carriage.carriage} committed, ${atDoor.carriage} at the door`;
    }
    if (carriage.carriage === "RawUtxo" && atDoor.carriage === "RawUtxo") {
      return carriage.refInputIndex === atDoor.refInputIndex
        ? null
        : `reference-input index ${carriage.refInputIndex.toString()} committed, ${atDoor.refInputIndex.toString()} at the door`;
    }
    if (carriage.carriage === "Certified" && atDoor.carriage === "Certified") {
      if (carriage.certRefInputIndex !== atDoor.certRefInputIndex) {
        return `certificate index ${carriage.certRefInputIndex.toString()} committed, ${atDoor.certRefInputIndex.toString()} at the door`;
      }
      const committed = carriage.chunkRefInputIndices;
      const observed = atDoor.chunkRefInputIndices;
      return committed.length === observed.length &&
        committed.every((index, position) => index === observed[position])
        ? null
        : `chunk indices [${committed.join(",")}] committed, [${observed.join(",")}] at the door`;
    }
    return null;
  })();
  if (disagreement !== null) {
    throw new Error(
      `${label} §8 carriage does not resolve at the door-running transaction: ${disagreement}. ` +
        "The committed evidence hash binds the indices, so the reference-input set the door " +
        "sees must be the one they were resolved against (§8.7 content addressing, #600).",
    );
  }
};

/**
 * Certifies a tier-3 plan: one mint of the policy's constant-name token, one
 * certificate output at the validator's own address carrying the manifest as an
 * inline datum, and the plan's chunks as reference inputs.
 *
 * **The token is not content-addressed (#606, owner ruling 2026-08-16).** It
 * used to be — its name was `blake2b_256(field_index ‖ tx_id)` — and the
 * derivation is retired: every certificate of the policy now wears the same
 * fixed name, which carries no content and no security weight. What is
 * content-bound is the *datum*, whose `field_hash` the mint welds to the
 * verified `chunk_digests` and which the §8.8 door then holds against the
 * commitment it anchored itself. So a consumer of a certificate reads the
 * datum; the token only says which policy minted it (see
 * {@link resolveCertificateReferenceIndexV1}).
 *
 * The chunk UTxOs must already exist. That is not a builder limitation but a
 * ledger one, and it is worth stating because it is the reason last-chunk
 * publication and certification can never share a transaction: reference inputs
 * are resolved against the UTxO set as it stands *before* the transaction, so
 * an output the same transaction creates is not available to it.
 */
export const buildUnsignedFieldPreimageCertificationV1Program = (
  lucid: LucidEvolution,
  {
    plan,
    certificatePolicyId,
    certificateAddress,
    certificateScript,
    chunkUtxos,
    compactCbor,
    witnessSetCompactCbor,
  }: {
    readonly plan: MidgardFieldCarriagePlanV1;
    readonly certificatePolicyId: string;
    readonly certificateAddress: string;
    readonly certificateScript: MintingPolicy;
    readonly chunkUtxos: readonly UTxO[];
    readonly compactCbor: string;
    readonly witnessSetCompactCbor: string;
  },
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      const certification = deriveFieldPreimageCertificationV1(plan);
      if (chunkUtxos.length !== certification.chunkCount) {
        throw new Error(
          "certification must reference exactly the plan's chunks",
        );
      }
      const protocolParameters = await resolveProtocolParameters(lucid);
      const lovelace = minimumLovelaceForFieldPreimageCertificateV1({
        certificateAddress,
        certification,
        certificatePolicyId,
        coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
      });
      const unit = `${certificatePolicyId}${certification.assetNameHex}`;
      return await lucid
        .newTx()
        .readFrom([...chunkUtxos])
        .mintAssets(
          { [unit]: 1n },
          certifyFieldPreimageRedeemerV1({
            compactCbor,
            witnessSetCompactCbor,
            chunkRefInputIndices: resolveChunkReferenceIndicesV1({
              plan,
              referenceInputs: chunkUtxos,
            }),
            outputIndex: 0,
          }),
        )
        .pay.ToAddressWithData(
          certificateAddress,
          { kind: "inline", value: certification.datumCbor },
          { lovelace, [unit]: 1n },
        )
        .attach.MintingPolicy(certificateScript)
        .complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new Error(
        `Failed to certify field-preimage carriage: ${
          cause instanceof Error ? cause.message : String(cause)
        }`,
      ),
  });

// `layOutMidgardFieldCarriageV1` is deliberately **not** re-exported here. It
// is `@al-ft/midgard-core/codec/native-tx-carriage-v1`'s function and it is
// unchanged by anything in this module, so a pass-through would only give the
// same behaviour two import paths and two places to document it. A step builder
// imports it from the core package, as this file's own consumers do.
