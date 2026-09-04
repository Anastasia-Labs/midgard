import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";

import { Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { ValidationAuxiliaryWitnessSchema } from "../src/fraud-proof/validation-auxiliary-witness.js";

type GeneratedAuxiliaryFixture = {
  readonly constructors: readonly {
    readonly tag: number;
    readonly arity: number;
    readonly cbor: string;
  }[];
  readonly corpusCbor: string;
  readonly corpusBlake2b256: string;
  readonly corpusSha256: string;
};

const auxiliaryFixture = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-validation/tests/fixtures/validation-auxiliary-witness-v1.generated.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as GeneratedAuxiliaryFixture;

const CorpusSchema = Data.Array(ValidationAuxiliaryWitnessSchema);

const canonicalRawWitnesses = (): Constr<unknown>[] => {
  const raw = Data.from(auxiliaryFixture.corpusCbor);
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
  [...body.matchAll(/^ {6}([a-z_]+):/gmu)].map((match) => match[1]!);

describe("ValidationAuxiliaryWitnessSchema", () => {
  it("round-trips the canonical 40-constructor V1 corpus exactly", () => {
    expect(
      createHash("sha256")
        .update(Buffer.from(auxiliaryFixture.corpusCbor, "hex"))
        .digest("hex"),
    ).toBe(auxiliaryFixture.corpusSha256);

    const decoded = Data.from(auxiliaryFixture.corpusCbor, CorpusSchema);
    expect(Data.to(decoded, CorpusSchema)).toBe(auxiliaryFixture.corpusCbor);

    const raw = canonicalRawWitnesses();
    expect(raw).toHaveLength(40);
    raw.forEach((witness, tag) => {
      expect(witness.index).toBe(tag);
      expect(witness.fields).toHaveLength(
        auxiliaryFixture.constructors[tag]!.arity,
      );
    });
  });

  it("preserves every individual canonical tag and encoding", () => {
    const raw = canonicalRawWitnesses();
    const decoded = raw.map((witness, tag) => {
      try {
        return Data.from(
          Data.to(witness as never),
          ValidationAuxiliaryWitnessSchema,
        );
      } catch (error) {
        const detail = error instanceof Error ? error.message : String(error);
        throw new Error(`canonical auxiliary tag ${tag} is invalid: ${detail}`);
      }
    });

    decoded.forEach((witness, tag) => {
      const encoded = Data.to(
        witness as never,
        ValidationAuxiliaryWitnessSchema,
      );
      expect(encoded).toBe(Data.to(raw[tag]! as never));
      const roundTrip = Data.from(encoded, ValidationAuxiliaryWitnessSchema);
      expect(Data.to(roundTrip, ValidationAuxiliaryWitnessSchema)).toBe(
        encoded,
      );
    });
  });

  it("rejects adjacent tags, wrong arities, and malformed nested shapes", () => {
    const rejects = (value: Constr<unknown>): void => {
      expect(() =>
        Data.from(Data.to(value as never), ValidationAuxiliaryWitnessSchema),
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
        "../src/fraud-proof/validation-auxiliary-witness.ts",
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
