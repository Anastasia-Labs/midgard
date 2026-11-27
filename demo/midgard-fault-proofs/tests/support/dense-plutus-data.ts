export type DensePlutusDataShape =
  | "map-chain"
  | "generic-constructors"
  | "flat-integers"
  | "chunked-bytes";

/**
 * Canonical Data of exactly `length` bytes in the shapes that cost the most
 * to read per byte on chain: a chain of one-entry maps keyed by the empty
 * map; nested generic constructors (tag 102) whose alternative takes the
 * widest integer; a flat list of one-byte integers; and a bytestring longer
 * than 64 bytes, in 64-byte chunks and one shorter final chunk.
 */
export const densePlutusData = (
  shape: DensePlutusDataShape,
  length: number,
): Buffer => {
  switch (shape) {
    case "map-chain": {
      // `a1 a0` per level over an innermost value of one or two bytes.
      const levels = Math.floor((length - 1) / 2);
      const innermost =
        length - 2 * levels === 1
          ? Buffer.from("a0", "hex")
          : Buffer.from("1818", "hex");
      return Buffer.concat([
        Buffer.from("a1a0".repeat(levels), "hex"),
        innermost,
      ]);
    }
    case "generic-constructors": {
      const open = Buffer.from("d866821bffffffffffffffff9f", "hex");
      // At least one integer, so the innermost list is not empty.
      const depth = Math.floor((length - 3) / (open.length + 1));
      return Buffer.concat([
        Buffer.concat(Array.from({ length: depth }, () => open)),
        Buffer.from("9f", "hex"),
        Buffer.alloc(length - depth * (open.length + 1) - 2),
        Buffer.from("ff", "hex"),
        Buffer.alloc(depth, 0xff),
      ]);
    }
    case "flat-integers":
      return Buffer.concat([
        Buffer.from("9f", "hex"),
        Buffer.alloc(length - 2),
        Buffer.from("ff", "hex"),
      ]);
    case "chunked-bytes": {
      // `5f`, full 64-byte chunks, at most one shorter final chunk, `ff`.
      const full = Buffer.concat([
        Buffer.from("5840", "hex"),
        Buffer.alloc(64),
      ]);
      const chunks = Math.floor((length - 2) / full.length);
      const rest = length - 2 - chunks * full.length;
      // The final chunk's head is one byte below 24 bytes and two from 24.
      const finalBytes = rest <= 24 ? rest - 1 : rest - 2;
      if (
        chunks < 2 ||
        (rest > 0 && (finalBytes < 1 || (rest > 24 && finalBytes < 24)))
      )
        throw new Error("chunked bytestring length is out of reach");
      const final =
        rest === 0
          ? []
          : [
              Buffer.from(
                finalBytes < 24 ? [0x40 + finalBytes] : [0x58, finalBytes],
              ),
              Buffer.alloc(finalBytes),
            ];
      return Buffer.concat([
        Buffer.from("5f", "hex"),
        ...Array.from({ length: chunks }, () => full),
        ...final,
        Buffer.from("ff", "hex"),
      ]);
    }
  }
};
