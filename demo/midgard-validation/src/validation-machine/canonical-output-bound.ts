import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { deriveMidgardTxFieldPreimages } from "@al-ft/midgard-core/consensus-validation";

/**
 * The first output of an outputs-field preimage past the canonical width
 * bound. The canonical machine rejects this before signatures/native-script
 * phases.
 */
export const oversizedOutputIndexInPreimage = (
  outputsPreimageCbor: Uint8Array,
): number | undefined => {
  const index = decodeMidgardFieldPreimage(outputsPreimageCbor).findIndex(
    (output) =>
      output.length > MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
  );
  return index < 0 ? undefined : index;
};

/** `oversizedOutputIndexInPreimage` over the bytes' derived outputs field. */
export const oversizedCanonicalOutputIndex = (
  bytes: Buffer,
  sourceKind: "normal" | "forced",
): number | undefined =>
  oversizedOutputIndexInPreimage(
    deriveMidgardTxFieldPreimages(bytes, sourceKind)[2]!.preimageCbor,
  );
