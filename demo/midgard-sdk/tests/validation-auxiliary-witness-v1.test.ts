import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { ValidationAuxiliaryWitnessV1Schema } from "../src/fraud-proof/validation-auxiliary-witness-v1.js";

const CANONICAL_AUXILIARY_CORPUS_CBOR =
  "9fd87980d87a9fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ffd8799f0100000100" +
  "41128080ffffd87b9fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111118080ffd8799f010000" +
  "010041128080ffd87980ffd87c9fd8799f010000010041128080ffd87a80d87980ffd87d9fd8799f41020100000000ffffd87e9f0041035820202020" +
  "2020202020202020202020202020202020202020202020202020202020410480d87980ffd87f9f014105582021212121212121212121212121212121" +
  "2121212121212121212121212121212180ffd905009f0041065820222222222222222222222222222222222222222222222222222222222222222241" +
  "07ffd905019f000058202323232323232323232323232323232323232323232323232323232323232323410980ffd905029f0000410a005820242424" +
  "242424242424242424242424242424242424242424242424242424242401582025252525252525252525252525252525252525252525252525252525" +
  "2525252580ffd905039f0001015820262626262626262626262626262626262626262626262626262626262626262680ffd905049f00000000582027" +
  "27272727272727272727272727272727272727272727272727272727272727410b800000410c01582030303030303030303030303030303030303030" +
  "30303030303030303030303030805820282828282828282828282828282828282828282828282828282828282828282880d8799f0100000100411280" +
  "80ffffd905059fd8799fd8799f0000582014141414141414141414141414141414141414141414141414141414141414145820151515151515151515" +
  "151515151515151515151515151515151515151515151558201616161616161616161616161616161616161616161616161616161616161616000000" +
  "ffd8799f0000582014141414141414141414141414141414141414141414141414141414141414145820151515151515151515151515151515151515" +
  "151515151515151515151515151558201616161616161616161616161616161616161616161616161616161616161616000000ffd87f80ffffd90506" +
  "9f0000410e410f80ffd905079f00411080ffd905089f80005820292929292929292929292929292929292929292929292929292929292929292980ff" +
  "d905099f00411141120180ffd9050a9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000" +
  "ff4040d8799f400000ffd8799f400000ffff00010158202a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a2a8000000058" +
  "202b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b2b411380ffd9050b9fd87a80d8799f01000000010158201313131313" +
  "1313131313131313131313131313131313131313131313131313130000000000000000d87a80ffd8799fd87c80d87a80d87a80ffffd9050c9fd8799f" +
  "00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff4040d8799f400000ffd8799f400000ffffff" +
  "d9050d9fd8799f00d8799f5820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000ff4040d8799f400000ffd879" +
  "9f400000ffff004114411580ffd9050e9fd8799fd8799f40000000ffd8799f400000ffd8799f400000ffffffd9050f9fd8799fd8799f40000000ffd8" +
  "799f400000ffd8799f400000ffffffd905109fd8799fd8799f400000ffd8799f400000ffd8799f400000ffffffd905119f00411658202c2c2c2c2c2c" +
  "2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c2c41170041184119018080d8799fd879800080ffffd905129f00411a00411b411c0180" +
  "80d8799fd879800080ffffd905139f00411d411e0180d8799fd879800080ffffd905149f01411f58202d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d" +
  "2d2d2d2d2d2d2d2d2d2d2d2d2d4120ffd905159f00412180ffd905169fd8799f01000100015820111111111111111111111111111111111111111111" +
  "11111111111111111111118080ffffd905179fd8799f0100010001582011111111111111111111111111111111111111111111111111111111111111" +
  "118080ff4122ffd905189f000158202e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e2e80ffd905199fd87980ffd9051a9f41" +
  "22d87980ffd9051b9fd8799f01000001d8799f0040ffff80ffd9051c9f0041234124d8799fd8799f01000080ff00800080ffffd9051d9fd8799f0100" +
  "00010041128080ffd87a80ffd9051e9f0000000058202f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f2f412580000041" +
  "260158203030303030303030303030303030303030303030303030303030303030303030805820313131313131313131313131313131313131313131" +
  "313131313131313131313180d87a8080ffd9051f9f00412780ffd905209fd8799f010000010041128080ffd87a80ffff";

const CANONICAL_AUXILIARY_CORPUS_SHA256 =
  "a23d464f93c22979d055c15e5705e29479b7e2086bd4f321ee595edcd612d5c2";

const EXPECTED_ARITIES = [
  0, 2, 3, 3, 1, 6, 4, 4, 5, 8, 5, 16, 1, 5, 3, 4, 5, 12, 3, 1, 5, 1, 1, 1, 11,
  9, 6, 4, 3, 1, 2, 4, 1, 2, 2, 4, 2, 17, 3, 2,
] as const;

const CorpusSchema = Data.Array(ValidationAuxiliaryWitnessV1Schema);

const canonicalRawWitnesses = (): Constr<unknown>[] => {
  const raw = Data.from(CANONICAL_AUXILIARY_CORPUS_CBOR);
  if (
    !Array.isArray(raw) ||
    !raw.every((witness) => witness instanceof Constr)
  ) {
    throw new Error("canonical auxiliary corpus is not a constructor list");
  }
  return raw as Constr<unknown>[];
};

const constructorBody = (source: string, name: string): string => {
  const body = source.match(
    new RegExp(`${name}: Data\\.Object\\(\\{([\\s\\S]*?)\\n    \\}\\),`, "u"),
  )?.[1];
  if (body === undefined) {
    throw new Error(`missing ${name} schema`);
  }
  return body;
};

const fieldNames = (body: string): string[] =>
  [...body.matchAll(/^      ([a-z_]+):/gmu)].map((match) => match[1]!);

describe("ValidationAuxiliaryWitnessV1Schema", () => {
  it("round-trips the canonical 40-constructor V1 corpus exactly", () => {
    expect(
      createHash("sha256")
        .update(Buffer.from(CANONICAL_AUXILIARY_CORPUS_CBOR, "hex"))
        .digest("hex"),
    ).toBe(CANONICAL_AUXILIARY_CORPUS_SHA256);

    const decoded = Data.from(CANONICAL_AUXILIARY_CORPUS_CBOR, CorpusSchema);
    expect(Data.to(decoded, CorpusSchema)).toBe(
      CANONICAL_AUXILIARY_CORPUS_CBOR,
    );

    const raw = canonicalRawWitnesses();
    expect(raw).toHaveLength(40);
    raw.forEach((witness, tag) => {
      expect(witness.index).toBe(tag);
      expect(witness.fields).toHaveLength(EXPECTED_ARITIES[tag]);
    });
  });

  it("preserves every individual canonical tag and encoding", () => {
    const raw = canonicalRawWitnesses();
    const decoded = raw.map((witness, tag) => {
      try {
        return Data.from(
          Data.to(witness as never),
          ValidationAuxiliaryWitnessV1Schema,
        );
      } catch (error) {
        const detail = error instanceof Error ? error.message : String(error);
        throw new Error(`canonical auxiliary tag ${tag} is invalid: ${detail}`);
      }
    });

    decoded.forEach((witness, tag) => {
      const encoded = Data.to(
        witness as never,
        ValidationAuxiliaryWitnessV1Schema,
      );
      expect(encoded).toBe(Data.to(raw[tag]! as never));
      const roundTrip = Data.from(encoded, ValidationAuxiliaryWitnessV1Schema);
      expect(Data.to(roundTrip, ValidationAuxiliaryWitnessV1Schema)).toBe(
        encoded,
      );
    });
  });

  it("rejects adjacent tags, wrong arities, and malformed nested shapes", () => {
    const rejects = (value: Constr<unknown>): void => {
      expect(() =>
        Data.from(Data.to(value as never), ValidationAuxiliaryWitnessV1Schema),
      ).toThrow();
    };

    rejects(new Constr(40, []));
    rejects(new Constr(1, ["01"]));

    const raw = canonicalRawWitnesses();
    rejects(new Constr(1, [0n, raw[1]!.fields[1]!]));
    rejects(
      new Constr(3, [
        raw[3]!.fields[0]!,
        new Constr(2, []),
        raw[3]!.fields[2]!,
      ]),
    );
    rejects(new Constr(11, raw[11]!.fields.slice(0, -1)));
    rejects(
      new Constr(11, [...raw[11]!.fields.slice(0, -1), new Constr(1, [])]),
    );
    rejects(new Constr(12, [new Constr(0, [])]));
    rejects(
      new Constr(34, [new Constr(0, [1n, 0n, 0n, 1n, new Constr(99, [])]), []]),
    );
  });

  it("contains only bounded descriptor-era production ABI fields", () => {
    const source = readFileSync(
      new URL(
        "../src/fraud-proof/validation-auxiliary-witness-v1.ts",
        import.meta.url,
      ),
      "utf8",
    );

    expect(source).not.toMatch(/\bTransactionFieldPreimageWitness\b/u);
    expect(source).not.toMatch(/\bOutputReplayWitness\b/u);
    expect(source).not.toMatch(/\bMidgardScriptWitnessSchema\b/u);
    expect(source).not.toMatch(/\boutput_cbor:/u);
    expect(source).not.toMatch(/\bsigner_hashes:/u);
    expect(source).not.toMatch(/\bData\s*\.\s*Any\s*\(/u);

    expect(
      fieldNames(constructorBody(source, "NativeExecutionScanWitness")),
    ).toEqual([
      "execution_index",
      "language_tag",
      "purpose_kind",
      "purpose_index",
      "script_hash",
      "subject",
      "purpose_siblings",
      "source_index",
      "origin_kind",
      "source_key",
      "script_total_length",
      "script_item_commitment",
      "source_siblings",
      "redeemer_leaf",
      "execution_siblings",
      "first_chunk_proof",
    ]);
    expect(
      fieldNames(constructorBody(source, "CekResolvedContextItemWitness")),
    ).toEqual([
      "source_kind",
      "item_index",
      "key",
      "descriptor_cbor",
      "siblings",
    ]);
    expect(
      fieldNames(constructorBody(source, "CekOutputContextItemWitness")),
    ).toEqual(["output_index", "descriptor_cbor", "siblings"]);
    expect(
      fieldNames(constructorBody(source, "CekContextFinalizeSpendWitness")),
    ).toEqual([
      "redeemer_control",
      "item_index",
      "key",
      "descriptor_cbor",
      "siblings",
    ]);
  });
});
