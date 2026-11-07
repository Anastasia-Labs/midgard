import {
  advanceMidgardRedeemerItemProof,
  buildMidgardBoundedItem,
  buildMidgardBoundedItemChunkProof,
  initialMidgardRedeemerItemProofControl,
  type MidgardRedeemerItemProofWitness,
  readMidgardRedeemerItemProofSource,
} from "@al-ft/midgard-core";
import { expect, it } from "vitest";

it.each(["ff", "8400004100820a14"])(
  "distinguishes authenticated %s syntax from missing or forged source",
  (hex) => {
    const bytes = Buffer.from(hex, "hex"),
      item = buildMidgardBoundedItem({ fieldIndex: 8, itemIndex: 0, bytes });
    const control = initialMidgardRedeemerItemProofControl({
      mode: 1,
      itemIndex: 0,
      itemCount: 1,
      totalLength: bytes.length,
      itemCommitment: item.commitment,
    });
    const witness: MidgardRedeemerItemProofWitness = {
      action: { kind: "openHeader" },
      chunkProof: buildMidgardBoundedItemChunkProof(item, 0),
      nextChunkProof: null,
    };
    expect(readMidgardRedeemerItemProofSource({ control, witness })).toEqual({
      sourceBytes: bytes,
    });
    expect(advanceMidgardRedeemerItemProof({ control, witness }) === null).toBe(
      hex === "ff",
    );
    expect(
      readMidgardRedeemerItemProofSource({
        control,
        witness: { ...witness, chunkProof: null },
      }),
    ).toBeNull();
    expect(
      readMidgardRedeemerItemProofSource({
        control,
        witness: { ...witness, nextChunkProof: witness.chunkProof },
      }),
    ).toBeNull();
    expect(
      readMidgardRedeemerItemProofSource({
        control,
        witness: {
          ...witness,
          chunkProof: {
            ...witness.chunkProof!,
            chunk: Buffer.alloc(bytes.length),
          },
        },
      }),
    ).toBeNull();
  },
);
