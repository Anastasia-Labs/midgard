/**
 * Field items whose §5.1 preimage repeats its first chunk byte for byte as its
 * second, so a Certified-tier plan over them carries two identical chunks.
 */
import {
  encodeMidgardFieldPreimage,
  MIDGARD_CHUNK_BYTES_K,
} from "@al-ft/midgard-core";

/**
 * `item(fill)` builds one item whose free bytes are all `fill` and whose other
 * bytes do not depend on it; every item has the same length. The repeated
 * bytes are taken from free bytes only, so each item keeps its shape.
 */
export const repeatedFirstChunkItems = ({
  count,
  item,
}: {
  readonly count: number;
  readonly item: (fill: number) => Buffer;
}): Buffer[] => {
  const template = (fill: number, length = count) =>
    encodeMidgardFieldPreimage(Array.from({ length }, () => item(fill)));
  const low = template(0x00);
  const high = template(0xff);
  const bytes: (number | undefined)[] = [...low].map((byte, index) =>
    byte === high[index] ? byte : undefined,
  );
  for (let index = 0; index < MIDGARD_CHUNK_BYTES_K; index += 1) {
    const twin = index + MIDGARD_CHUNK_BYTES_K;
    const first = bytes[index];
    const second = bytes[twin];
    if (first !== undefined && second !== undefined && first !== second) {
      throw new Error(
        `item shape fixes byte ${index.toString()} of both chunks differently`,
      );
    }
    const shared = first ?? second ?? 0x01;
    bytes[index] = shared;
    bytes[twin] = shared;
  }
  const preimage = Buffer.from(bytes.map((byte) => byte ?? 0x02));
  const itemLength = item(0x00).length;
  const stride = template(0x00, count + 1).length - low.length;
  const headerLength = low.length - stride * count;
  const items = Array.from({ length: count }, (_, index) => {
    const end = headerLength + stride * (index + 1);
    return Buffer.from(preimage.subarray(end - itemLength, end));
  });
  if (!encodeMidgardFieldPreimage(items).equals(preimage)) {
    throw new Error("items do not re-encode to the repeated-chunk preimage");
  }
  return items;
};
