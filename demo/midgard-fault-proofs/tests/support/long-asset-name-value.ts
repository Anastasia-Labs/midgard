import type { MidgardTxOutput, MidgardValue } from "@al-ft/midgard-core/codec";

/**
 * One policy of 141 32-byte asset names and one 23-byte tail name. With a
 * five-byte lovelace encoding the Cardano value is 5,000 bytes at tail
 * quantity 24 and 5,001 at 256, and an output carrying it runs across the
 * 4,095-byte chunk boundary.
 */
export const LONG_ASSET_NAME_POLICY = "55".repeat(28);
const LONG_NAMES = Array.from(
  { length: 141 },
  (_, index) =>
    [
      Buffer.concat([Buffer.from([index]), Buffer.alloc(31, 0xaa)]).toString(
        "hex",
      ),
      1n,
    ] as const,
);
const TAIL_NAME = `ff${"bb".repeat(22)}`;

export type LongAssetNameTail = 24n | 256n;

/** The whole value, or only its 141 long names or only its tail asset. */
export const longAssetNameValue = ({
  lovelace,
  tailQuantity,
  part = "all",
}: {
  readonly lovelace: bigint;
  readonly tailQuantity: LongAssetNameTail;
  readonly part?: "all" | "names" | "tail";
}): MidgardValue => ({
  lovelace,
  assets: new Map([
    [
      LONG_ASSET_NAME_POLICY,
      new Map([
        ...(part === "tail" ? [] : LONG_NAMES),
        ...(part === "names" ? [] : [[TAIL_NAME, tailQuantity] as const]),
      ]),
    ],
  ]),
});

/** 25 ADA keeps the five-byte lovelace encoding and clears the min-Ada floor
 * of an output carrying the long-name value. */
export const LONG_ASSET_NAME_LOVELACE = 25_000_000n;

/** A key-hash output carrying the whole long-name value. */
export const longAssetNameOutput = (
  tailQuantity: LongAssetNameTail,
  lovelace: bigint = LONG_ASSET_NAME_LOVELACE,
): MidgardTxOutput => ({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
  value: longAssetNameValue({ lovelace, tailQuantity }),
});
