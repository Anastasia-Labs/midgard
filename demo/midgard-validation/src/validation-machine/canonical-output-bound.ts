import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { deriveMidgardTxFieldPreimages } from "@al-ft/midgard-core/consensus-validation";

/** The canonical machine rejects this before signatures/native-script phases. */
export const oversizedCanonicalOutputIndex = (
  bytes: Buffer,
  sourceKind: "normal" | "forced",
): number | undefined => {
  const field = deriveMidgardTxFieldPreimages(bytes, sourceKind)[2]!;
  const index = decodeMidgardFieldPreimage(field.preimageCbor).findIndex(
    (output) =>
      output.length > MIDGARD_CONSENSUS_LIMITS.maxLedgerOutputPreimageBytes,
  );
  return index < 0 ? undefined : index;
};
