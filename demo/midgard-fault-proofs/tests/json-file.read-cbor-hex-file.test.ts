import { mkdtemp, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { readCborHexFile } from "../src/json-file.js";

describe("readCborHexFile", () => {
  it("reads exact lowercase CBOR files and rejects ambiguous wrappers", async () => {
    const directory = await mkdtemp(join(tmpdir(), "midgard-cbor-file-"));
    const rawPath = join(directory, "raw.cbor");
    const wrappedPath = join(directory, "wrapped.json");
    const ambiguousPath = join(directory, "ambiguous.json");
    const upperPath = join(directory, "upper.cbor");
    await Promise.all([
      writeFile(rawPath, "d87980\n"),
      writeFile(wrappedPath, '{"cborHex":"d87980"}\n'),
      writeFile(ambiguousPath, '{"cborHex":"d87980","unexpected":true}\n'),
      writeFile(upperPath, "D87980\n"),
    ]);
    await expect(readCborHexFile(rawPath, "fixture")).resolves.toBe("d87980");
    await expect(readCborHexFile(wrappedPath, "fixture")).resolves.toBe(
      "d87980",
    );
    await expect(readCborHexFile(ambiguousPath, "fixture")).rejects.toThrow(
      /exactly a cborHex field/u,
    );
    await expect(readCborHexFile(upperPath, "fixture")).rejects.toThrow(
      /raw CBOR hex or JSON/u,
    );
  });
});
