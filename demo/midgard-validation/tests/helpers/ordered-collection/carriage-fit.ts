import {
  buildMidgardBoundedItem,
  commitMidgardBoundedItem,
  midgardBoundedItemChunkCount,
} from "@al-ft/midgard-core";
import { encodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec/native-tx-field-access";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_ENVELOPE_MEASUREMENTS,
} from "@al-ft/midgard-core/consensus-profile";
import { deriveValidationProofItemPublication } from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

export type MidgardCompleteItemCarriageFit = {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemBytes: number;
  readonly commitmentHex: string;
  /** Production carriage decision for the complete item. */
  readonly carriage: "direct" | "reference";
  /** Chunks a bounded fallback would need if the complete item did not fit. */
  readonly boundedFallbackChunkCount: number;
  readonly maxReliableDirectCompleteItemBytes: number;
  readonly maxSinglePublicationCompleteItemBytes: number;
  readonly maxL1TransactionBytes: number;
  readonly publicationDatumBytes: number;
  readonly publicationTransactionBytes: number;
  readonly fitsDirectCarriage: boolean;
  readonly fitsSinglePublicationCarriage: boolean;
  /** True only when neither complete route admits the item. */
  readonly requiresBoundedFallback: boolean;
};

/**
 * Measures whether a complete canonical proof item fits direct carriage and
 * single-publication reference carriage, before any bounded fallback is
 * considered. This is the §3.2 complete-item-first ordering: a fallback is
 * necessary only when both complete routes are measured to overflow.
 *
 * The publication side builds a real signed Conway transaction carrying the
 * published inline datum, using the validation-proof item publication framing. Since #597 that datum holds the field's
 * whole §5.1 preimage rather than one item beside an opening into it, so what is
 * published here is the single-item envelope of `itemCbor` — the smallest
 * genuine field a complete-item step can name, which keeps the measurement a
 * lower bound on the publication transaction rather than an invented shape.
 *
 * Direct carriage is bounded by the applied deployed-validator measurement
 * pinned in `MIDGARD_ENVELOPE_MEASUREMENTS`.
 */
export const measureMidgardCompleteItemCarriageFit = ({
  fieldIndex,
  itemIndex,
  itemCbor,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemCbor: Buffer;
}): MidgardCompleteItemCarriageFit => {
  const maxL1TransactionBytes =
    MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxBytes;
  const maxSinglePublicationCompleteItemBytes =
    MIDGARD_CONSENSUS_LIMITS.maxSinglePublicationCompleteItemBytes;
  const maxReliableDirectCompleteItemBytes =
    MIDGARD_ENVELOPE_MEASUREMENTS.maxReliableDirectCompleteItemBytes;
  const bounded = buildMidgardBoundedItem({
    fieldIndex,
    itemIndex,
    bytes: itemCbor,
  });
  const commitment = commitMidgardBoundedItem({
    fieldIndex,
    itemIndex,
    totalLength: itemCbor.length,
    frontier: bounded.frontier,
  });
  if (!commitment.equals(bounded.commitment)) {
    throw new Error(
      "Complete proof item commitment disagrees with its own bounded frontier",
    );
  }
  const fitsDirectCarriage =
    itemCbor.length <= maxReliableDirectCompleteItemBytes;
  const fitsItemPublicationBound =
    itemCbor.length <= maxSinglePublicationCompleteItemBytes;
  const publication = deriveValidationProofItemPublication({
    transactionId: "44".repeat(32),
    transactionCommitment: "55".repeat(32),
    fieldPreimage: encodeMidgardFieldPreimage([itemCbor]).toString("hex"),
  });
  const signingKey = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 4));
  const address = CML.Address.from_raw_bytes(
    Buffer.concat([
      Buffer.from([0x60]),
      Buffer.from(signingKey.to_public().hash().to_raw_bytes()),
    ]),
  );
  const scriptAddress = CML.Address.from_raw_bytes(
    Buffer.concat([Buffer.from([0x70]), Buffer.alloc(28, 0x66)]),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_raw_bytes(Buffer.alloc(32, 1)),
      0n,
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      scriptAddress,
      CML.Value.from_coin(70_000_000n),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(publication.datumCbor),
      ),
      undefined,
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      address,
      CML.Value.from_coin(1_000_000_000n),
      undefined,
      undefined,
    ),
  );
  const witnessSet = CML.TransactionWitnessSet.new();
  const vkeys = CML.VkeywitnessList.new();
  vkeys.add(
    CML.Vkeywitness.new(
      signingKey.to_public(),
      signingKey.sign(Buffer.alloc(32, 5)),
    ),
  );
  witnessSet.set_vkeywitnesses(vkeys);
  const publicationTransactionBytes = CML.Transaction.new(
    CML.TransactionBody.new(inputs, outputs, 1_000_000n),
    witnessSet,
    true,
    undefined,
  ).to_cbor_bytes().length;
  const fitsSinglePublicationCarriage =
    fitsItemPublicationBound &&
    publicationTransactionBytes <= maxL1TransactionBytes;
  return {
    fieldIndex,
    itemIndex,
    itemBytes: itemCbor.length,
    commitmentHex: commitment.toString("hex"),
    carriage:
      fitsItemPublicationBound &&
      itemCbor.length <= maxReliableDirectCompleteItemBytes
        ? "direct"
        : "reference",
    boundedFallbackChunkCount: midgardBoundedItemChunkCount(itemCbor.length),
    maxReliableDirectCompleteItemBytes,
    maxSinglePublicationCompleteItemBytes,
    maxL1TransactionBytes,
    publicationDatumBytes: publication.datumCbor.length / 2,
    publicationTransactionBytes,
    fitsDirectCarriage,
    fitsSinglePublicationCarriage,
    requiresBoundedFallback:
      !fitsDirectCarriage && !fitsSinglePublicationCarriage,
  };
};
