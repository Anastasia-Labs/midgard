import { Data } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS,
  DistinctAssetStep02DatumSchema,
} from "../src/distinct-asset-accumulation-limit/schemas.js";

// Canonical step-01 output c32c473be31804f32a13fa9ea14f854c84271f24b5b587af8a498d759ce801ef#0,
// created at slot 39121; this valid step-02 datum stopped production observation.
const step02Datum =
  "d87982581c3fa5ad81e439729263966b243f1384e544660adc86be882e67be2cced87981d87984d8798601010158209160a69307da89be3c3a1e26656dca0fa5825604089a30b7c87c5d0b303505b25827d8799f5820d20073ffc990ffe438f3f18c6f185de6ae56272f76453abb3ab9af29925888b202ffd87981d9052581005820075cd8aa2c255132663eab365b421aa7447e2246438dc062846b758ec9191c5101d87983020000";

it("decodes the retained step-02 output using its manifest-bound validator index", () => {
  expect(
    Data.from(
      step02Datum,
      DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS[1] as never,
    ),
  ).toMatchObject({
    fraud_prover: "3fa5ad81e439729263966b243f1384e544660adc86be882e67be2cce",
    data: { coordinate: { fold: 2n, primary_index: 0n, asset_index: 0n } },
  });
});

it("retained bytes match the actual step-02 builder schema", () => {
  expect(
    Data.from(step02Datum, DistinctAssetStep02DatumSchema as never),
  ).toMatchObject({
    data: { coordinate: { fold: 2n, primary_index: 0n, asset_index: 0n } },
  });
});
