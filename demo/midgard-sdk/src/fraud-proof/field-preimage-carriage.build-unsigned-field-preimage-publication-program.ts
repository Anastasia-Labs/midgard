import {
  MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
  midgardCarriagePublicationBytes,
  type MidgardFieldCarriagePlan,
  type MidgardFieldPublication,
} from "@al-ft/midgard-core/codec/native-tx-carriage";
import { MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  CML,
  Data,
  type LucidEvolution,
  type ProtocolParameters,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

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
 * {@link buildUnsignedFieldPreimagePublicationProgram} checks every publication
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
 * The smallest lovelace that keeps an output with this datum above the
 * per-byte minimum, found by the same fixpoint every other publication builder
 * in this package uses: min-Ada depends on the output's own serialised size,
 * which depends on the coin field, so raising the coin can raise the minimum
 * again.
 */
export const stabilizedMinimumLovelace = ({
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
export const fieldPreimagePublicationDatumCbor = (bytes: Uint8Array): string =>
  Data.to(Buffer.from(bytes).toString("hex"));

/**
 * The read-back twin of {@link fieldPreimagePublicationDatumCbor} — the
 * off-chain counterpart of the on-chain `un_b_data` on a raw carriage
 * reference input.
 *
 * It is what turns a resolved UTxO into the `inlineDatumBytes` the field-access
 * door expects, and it is fail-closed: a datum that is not a bare byte string
 * is not raw carriage, and returning something plausible for one would let
 * wrong bytes reach a hash check that would then merely fail later and less
 * informatively.
 */
export const fieldPreimagePublicationBytes = (datumCbor: string): Buffer => {
  const decoded = Data.from(datumCbor);
  if (typeof decoded !== "string") {
    throw new Error(
      "raw carriage datum is not a nothing-but-bytes inline datum (§8.5)",
    );
  }
  return Buffer.from(decoded, "hex");
};

/** One raw carriage output, ready to pay to the publisher's own key address. */
export type FieldPreimagePublicationOutput = {
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
export const fieldPreimagePublicationOutputs = (
  plan: MidgardFieldCarriagePlan,
): readonly FieldPreimagePublicationOutput[] =>
  plan.publications.map((publication: MidgardFieldPublication) => ({
    chunkIndex: publication.chunkIndex,
    datumCbor: fieldPreimagePublicationDatumCbor(publication.bytes),
    byteLength: publication.bytes.length,
    digestHex: publication.digest.toString("hex"),
  }));

export const minimumLovelaceForFieldPreimagePublication = ({
  publisherAddress,
  output,
  coinsPerUtxoByte,
}: {
  readonly publisherAddress: string;
  readonly output: FieldPreimagePublicationOutput;
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
 * {@link MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES}, the largest payload whose
 * signed publication clears `maxTxSize` with the 512-byte reserve. The refusal
 * quotes the *transaction* size that payload would produce, so the message says
 * why rather than only what: this many payload bytes publish as a transaction
 * of this size, and that does not fit. The frontier measurement is the one
 * caller with a reason to raise the bound, and it passes its own explicitly
 * rather than reaching around the check — but only within the §5.4 aggregate
 * field cap, so raising it can reach the far side of the frontier and cannot
 * turn the guard off.
 */
export const assertFieldPreimagePublicationFits = ({
  publication,
  maxPublicationBytes = MIDGARD_MAX_PUBLISHABLE_CARRIAGE_BYTES,
}: {
  readonly publication: FieldPreimagePublicationOutput;
  readonly maxPublicationBytes?: number;
}): void => {
  // The override is bounded, because an unbounded one is not a measurement
  // affordance but a way to switch the guard off from a caller's argument list.
  // §5.4 caps a whole transaction's field bytes at
  // `MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES`, so no admissible
  // publication is ever larger than that at any value of `K`; a caller asking
  // for more is not measuring the ladder's frontier, it is leaving the ladder.
  if (
    !Number.isSafeInteger(maxPublicationBytes) ||
    maxPublicationBytes < 1 ||
    maxPublicationBytes > MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES
  ) {
    throw new Error(
      `maxPublicationBytes must be between 1 and the §5.4 aggregate field cap ` +
        `(${MIDGARD_MAX_TRANSACTION_AGGREGATE_FIELD_BYTES.toString()}); got ` +
        `${maxPublicationBytes.toString()}`,
    );
  }
  if (publication.byteLength <= maxPublicationBytes) {
    return;
  }
  const transactionBytes = midgardCarriagePublicationBytes(
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
 * iterating {@link fieldPreimagePublicationOutputs}; the publications are
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
export const buildUnsignedFieldPreimagePublicationProgram = (
  lucid: LucidEvolution,
  {
    publication,
    publisherAddress,
    maxPublicationBytes,
  }: {
    readonly publication: FieldPreimagePublicationOutput;
    readonly publisherAddress: string;
    readonly maxPublicationBytes?: number;
  },
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      assertFieldPreimagePublicationFits({
        publication,
        ...(maxPublicationBytes === undefined ? {} : { maxPublicationBytes }),
      });
      // Publications and authenticated script references may share the prover
      // address. They are evidence, not transaction funding.
      const fundingInputs = (await lucid.wallet().getUtxos()).filter(
        (utxo) =>
          utxo.datum == null &&
          utxo.datumHash == null &&
          utxo.scriptRef == null &&
          Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
      );
      if (fundingInputs.length === 0)
        throw new Error(
          "Field-preimage publication requires plain-Ada funding",
        );
      const protocolParameters = await resolveProtocolParameters(lucid);
      return await lucid
        .newTx()
        .pay.ToAddressWithData(
          publisherAddress,
          { kind: "inline", value: publication.datumCbor },
          {
            lovelace: minimumLovelaceForFieldPreimagePublication({
              publisherAddress,
              output: publication,
              coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
            }),
          },
        )
        .complete({ localUPLCEval: true, presetWalletInputs: fundingInputs });
    },
    catch: (cause) =>
      new Error(
        `Failed to publish field-preimage carriage: ${
          cause instanceof Error ? cause.message : String(cause)
        }`,
      ),
  });
