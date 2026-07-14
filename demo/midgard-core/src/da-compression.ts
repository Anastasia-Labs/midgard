import * as zlib from "node:zlib";

export class DaCompressionUnavailableError extends Error {
  constructor() {
    super(
      "Native zstd requires Node.js >=22.15.0 (node:zlib zstdCompressSync is unavailable)",
    );
    this.name = "DaCompressionUnavailableError";
  }
}

export const assertDaZstdAvailable = (): void => {
  if (typeof zlib.zstdCompressSync !== "function") {
    throw new DaCompressionUnavailableError();
  }
};

export const compressDaPayloadZstd = (
  bytes: Uint8Array,
  level: number,
): Promise<Buffer> => {
  assertDaZstdAvailable();
  if (!Number.isSafeInteger(level) || level < 1 || level > 19) {
    throw new RangeError("DA zstd level must be an integer in [1, 19]");
  }
  return new Promise((resolve, reject) => {
    zlib.zstdCompress(
      Buffer.from(bytes),
      {
        params: {
          [zlib.constants.ZSTD_c_compressionLevel]: level,
        },
      },
      (error, result) => {
        if (error !== null) {
          reject(error);
          return;
        }
        resolve(result);
      },
    );
  });
};

export const decompressDaPayloadZstd = (
  bytes: Uint8Array,
  maxOutputLength: number,
): Promise<Buffer> => {
  assertDaZstdAvailable();
  if (!Number.isSafeInteger(maxOutputLength) || maxOutputLength <= 0) {
    throw new RangeError("DA zstd maxOutputLength must be a positive integer");
  }
  return new Promise((resolve, reject) => {
    zlib.zstdDecompress(
      Buffer.from(bytes),
      { maxOutputLength },
      (error, result) => {
        if (error !== null) {
          reject(error);
          return;
        }
        resolve(result);
      },
    );
  });
};

export const runDaZstdStartupSelfTest = async (): Promise<void> => {
  assertDaZstdAvailable();
  const expected = Buffer.from("midgard-da-zstd-startup-self-test", "utf8");
  const compressed = await compressDaPayloadZstd(expected, 3);
  const actual = await decompressDaPayloadZstd(compressed, expected.length);
  if (!actual.equals(expected)) {
    throw new Error("Native zstd startup self-test round-trip mismatch");
  }
};
