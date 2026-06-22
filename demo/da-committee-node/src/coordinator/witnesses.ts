import { hexToBytes, normalizeHex } from "../utils/hex.js";

export type ParsedSignatureWitness = {
  readonly signerIndex: number;
  readonly signatureHex: string;
  readonly witnessHex: string;
};

export const parseSignatureWitness = (
  witnessHex: string,
): ParsedSignatureWitness => {
  const witness = hexToBytes(witnessHex, "signature witness", 65);
  return {
    signerIndex: witness[0]!,
    signatureHex: witness.subarray(1).toString("hex"),
    witnessHex: witness.toString("hex"),
  };
};

export const packSortedSignatureWitnesses = (
  witnessHexes: readonly string[],
): string => {
  const parsed = witnessHexes
    .map(parseSignatureWitness)
    .sort((left, right) => left.signerIndex - right.signerIndex);
  let previousIndex = -1;
  for (const witness of parsed) {
    if (witness.signerIndex === previousIndex) {
      throw new Error(
        `duplicate DA signature witness for signer ${witness.signerIndex.toString()}`,
      );
    }
    previousIndex = witness.signerIndex;
  }
  return parsed.map((witness) => witness.witnessHex).join("");
};

export const isSignerBitSet = (
  bitmapHex: string,
  signerIndex: number,
): boolean => {
  const bitmap = hexToBytes(bitmapHex, "attested signer bitmap", 32);
  assertSignerIndex(signerIndex);
  const byteIndex = Math.floor(signerIndex / 8);
  const bitInByte = signerIndex % 8;
  return (bitmap[byteIndex]! & (1 << (7 - bitInByte))) !== 0;
};

export const setSignerBit = (
  bitmapHex: string,
  signerIndex: number,
): string => {
  const bitmap = Buffer.from(
    hexToBytes(bitmapHex, "attested signer bitmap", 32),
  );
  assertSignerIndex(signerIndex);
  const byteIndex = Math.floor(signerIndex / 8);
  const bitInByte = signerIndex % 8;
  bitmap[byteIndex] = bitmap[byteIndex]! | (1 << (7 - bitInByte));
  return bitmap.toString("hex");
};

export const countSetBits = (bitmapHex: string): number => {
  const bitmap = hexToBytes(bitmapHex, "attested signer bitmap", 32);
  let count = 0;
  for (const byte of bitmap) {
    count += byte.toString(2).replaceAll("0", "").length;
  }
  return count;
};

export const normalizeBitmap = (bitmapHex: string): string =>
  normalizeHex(bitmapHex, {
    fieldName: "attested signer bitmap",
    byteLength: 32,
  });

const assertSignerIndex = (signerIndex: number): void => {
  if (
    !Number.isSafeInteger(signerIndex) ||
    signerIndex < 0 ||
    signerIndex > 255
  ) {
    throw new Error("signer index must fit in one byte");
  }
};
