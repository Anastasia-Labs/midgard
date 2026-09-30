import { type FieldOpening } from "@al-ft/midgard-sdk";

/** Replace the compact transaction bytes the opening is anchored to. */
export const mutateCompactSource = (
  opening: FieldOpening,
  nativeTxCompactCbor: string,
): FieldOpening => {
  if (!("BodyFieldOpening" in opening))
    throw new Error("body opening expected");
  return {
    BodyFieldOpening: {
      ...opening.BodyFieldOpening,
      native_tx_compact_cbor: nativeTxCompactCbor,
    },
  };
};
