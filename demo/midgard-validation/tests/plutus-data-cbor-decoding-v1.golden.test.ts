import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { encodeCborBytes } from "@al-ft/midgard-core/codec/cbor";
import { compactPlutusDataCarriageCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekPlutusData,
  midgardEnvelopeDataFromCborIterative,
  plutusDataFromCborIterative,
} from "../src/cek-constant.js";
import * as vectors from "./fixtures/plutus-data-cbor-decoding-v1.vectors.mjs";

/**
 * The TypeScript half of the Plutus Data CBOR decoding golden channel.
 *
 * The rows are rebuilt here through `src/` from the same definitions the
 * generator drives through `dist/`, and compared with the checked-in fixture.
 * The generated Aiken module
 * (`onchain/aiken/lib/midgard/cbor-deserialise-v1-golden.test.ak`) holds
 * `midgard/cbor_deserialise` to the same rows. Regenerate both with
 * `pnpm run fixtures:plutus-data-cbor-decoding-v1:sync`.
 */

const golden = JSON.parse(
  readFileSync(
    fileURLToPath(
      new URL(
        "./fixtures/plutus-data-cbor-decoding-v1.generated.json",
        import.meta.url,
      ),
    ),
    "utf8",
  ),
) as { readonly rows: readonly vectors.PlutusDataCborDecodingRowV1[] };

const rows = vectors.buildPlutusDataCborDecodingRows({
  decodeData: plutusDataFromCborIterative,
  decodeEnvelope: midgardEnvelopeDataFromCborIterative,
  encodeData: encodeMidgardCekPlutusData,
  encodeCborBytes,
  compactCarriageCbor: compactPlutusDataCarriageCbor,
});

describe("Plutus Data CBOR decoding golden vectors", () => {
  it("declares exactly the exports the fixture module has", () => {
    expect(Object.keys(vectors).sort()).toEqual([
      "ENVELOPE_ONLY_VECTORS",
      "ENVELOPE_REFUSED_VECTORS",
      "ENVELOPE_WRITER_LENGTHS",
      "PLUTUS_DATA_CARDANO_EDGE_VECTORS",
      "REFUSED",
      "buildPlutusDataCborDecodingRows",
      "plutusDataCborDecodingCarried",
      "plutusDataCborDecodingValues",
    ]);
  });

  it("match what src/ decodes today", () => {
    expect(rows).toEqual(golden.rows);
  });

  it("hold every envelope-writer output and written value as accepted by the envelope entry", () => {
    const written = rows.filter((row) => row.source === "written");
    expect(written.length).toBeGreaterThan(0);
    for (const row of written) {
      expect(row.envelope, row.input).not.toBeNull();
      if (row.data !== null) expect(row.envelope, row.input).toBe(row.data);
    }
  });

  it("give the envelope entry the Data entry's value wherever the Data entry accepts", () => {
    for (const row of rows) {
      if (row.data !== null) expect(row.envelope, row.input).toBe(row.data);
    }
  });

  it("refuse every mutated row in both entries", () => {
    const mutated = rows.filter((row) => row.source === "mutated");
    expect(mutated.length).toBeGreaterThan(0);
    for (const row of mutated) {
      expect([row.data, row.envelope], row.input).toEqual([null, null]);
    }
  });
});
