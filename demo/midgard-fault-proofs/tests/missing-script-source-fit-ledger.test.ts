import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";

/**
 * Measured by `missing-script-source-publication-fit.test.ts` (publications)
 * and `missing-script-source-lifecycle.test.ts` under `MIDGARD_PRINT_FIT=1`
 * (lifecycle rows) against the testnet blueprint named below. Lifecycle rows
 * are taken at the maximum supported frontier — 2,520 inline plus 819
 * reference sources, the consensus field bounds pinned in
 * `tests/support/missing-script-source-shapes.ts` — walked in 167 batches of
 * 20 sources; the scan rows record the first batch, the batch with the
 * greatest memory (all-reference descriptors at the deepest sibling depth)
 * and the batch with the most bytes. Cancel rows come from the resumable
 * 30-source shape, whose 24-source batch is the frozen budget's own
 * measurement.
 */
export const missingScriptSourceFitMeasurements = [
  [
    "reference-step-01",
    "publication",
    "fully applied production script",
    15_117,
    0n,
    0n,
  ],
  [
    "reference-step-02",
    "publication",
    "fully applied production script",
    10_238,
    0n,
    0n,
  ],
  [
    "reference-step-03",
    "publication",
    "fully applied production script",
    10_772,
    0n,
    0n,
  ],
  [
    "reference-step-04",
    "publication",
    "fully applied production script",
    2_322,
    0n,
    0n,
  ],
  [
    "reference-step-05",
    "publication",
    "fully applied production script",
    5_522,
    0n,
    0n,
  ],
  [
    "reference-step-06",
    "publication",
    "fully applied production script",
    2_672,
    0n,
    0n,
  ],
  [
    "accepted-init",
    "lifecycle",
    "3339-source accepted absence",
    1_365,
    702_943n,
    240_851_334n,
  ],
  [
    "accepted-step-01",
    "lifecycle",
    "accepted transaction inclusion",
    2_064,
    1_340_723n,
    456_037_593n,
  ],
  [
    "accepted-step-02",
    "lifecycle",
    "trace and descriptor authentication",
    2_202,
    843_520n,
    334_135_303n,
  ],
  [
    "accepted-step-03",
    "lifecycle",
    "complete purpose and 3339-source frontiers",
    2_461,
    1_575_422n,
    720_437_663n,
  ],
  [
    "accepted-step-04",
    "lifecycle",
    "initialize universal source scan",
    1_149,
    154_123n,
    96_601_626n,
  ],
  [
    "accepted-step-05-first",
    "lifecycle",
    "first 20-source batch of the 3339-source scan",
    10_108,
    10_735_818n,
    3_892_108_825n,
  ],
  [
    "accepted-step-05-max-memory",
    "lifecycle",
    "deepest all-reference 20-source batch (batch 154 of 167)",
    8_870,
    11_395_818n,
    4_069_119_945n,
  ],
  [
    "accepted-step-05-max-bytes",
    "lifecycle",
    "widest 20-source batch (batch 126 of 167)",
    10_230,
    10_992_578n,
    3_955_392_885n,
  ],
  [
    "accepted-step-06",
    "lifecycle",
    "permanent proof mint",
    916,
    307_958n,
    117_127_941n,
  ],
  [
    "accepted-remove-leased",
    "lifecycle",
    "target plus successor removal under mutation lease",
    1_544,
    1_708_088n,
    576_044_325n,
  ],
  [
    "forced-init",
    "lifecycle",
    "3339-source wrongful rejection, last reference source matching",
    1_365,
    712_147n,
    243_624_075n,
  ],
  [
    "forced-step-01",
    "lifecycle",
    "forced transaction membership",
    1_798,
    1_110_802n,
    455_865_581n,
  ],
  [
    "forced-step-02",
    "lifecycle",
    "matching ScriptSourceScan authentication",
    2_252,
    915_702n,
    366_912_837n,
  ],
  [
    "forced-step-03",
    "lifecycle",
    "complete authenticated 3339-source prefix",
    2_507,
    1_575_898n,
    720_855_906n,
  ],
  [
    "forced-step-04",
    "lifecycle",
    "initialize presence scan",
    1_195,
    154_123n,
    97_092_648n,
  ],
  [
    "forced-step-05-first",
    "lifecycle",
    "first 20-source batch of the 3339-source prefix",
    10_154,
    10_735_818n,
    3_892_599_847n,
  ],
  [
    "forced-step-05-max-memory",
    "lifecycle",
    "deepest all-reference 20-source batch (batch 154 of 167)",
    8_916,
    11_395_818n,
    4_069_610_967n,
  ],
  [
    "forced-step-05-max-bytes",
    "lifecycle",
    "widest 20-source batch (batch 126 of 167)",
    10_276,
    10_992_578n,
    3_955_883_907n,
  ],
  [
    "forced-step-06",
    "lifecycle",
    "permanent proof mint",
    916,
    339_293n,
    127_497_709n,
  ],
  [
    "forced-remove-leased",
    "lifecycle",
    "target plus successor removal under mutation lease",
    1_544,
    1_712_235n,
    581_778_017n,
  ],
  [
    "resumable-step-05-full-budget",
    "lifecycle",
    "frozen 24-source batch over the 30-source resumable frontier",
    5_838,
    8_541_566n,
    3_319_920_447n,
  ],
  [
    "resumable-step-05-resumed",
    "lifecycle",
    "6-source closing batch resumed from the live checkpoint",
    2_013,
    2_248_794n,
    879_595_863n,
  ],
  [
    "cancel-step-01",
    "lifecycle",
    "cancel the bound thread before step 01",
    611,
    124_808n,
    42_452_566n,
  ],
  [
    "cancel-step-02",
    "lifecycle",
    "cancel after purpose binding",
    611,
    116_176n,
    41_056_424n,
  ],
  [
    "cancel-step-03",
    "lifecycle",
    "cancel after trace authentication",
    611,
    112_376n,
    40_448_424n,
  ],
  [
    "cancel-step-04",
    "lifecycle",
    "cancel after frontier authentication",
    611,
    110_676n,
    40_176_424n,
  ],
  [
    "cancel-step-05",
    "lifecycle",
    "cancel from the open or mid-walk scan",
    611,
    111_176n,
    40_256_424n,
  ],
  [
    "cancel-step-06",
    "lifecycle",
    "cancel the complete scan before the mint",
    611,
    111_876n,
    40_368_424n,
  ],
] as const;

export const buildMissingScriptSourceFitLedger = () =>
  buildVanRossemFitLedger({
    category: "missingScriptSource",
    blueprintSha256:
      "172c72d392706de52b5665dc0c1b208354a490298fba5ac0f411597f8454d10b",
    compilerVersion: "v1.1.23+5adf783",
    measurements: missingScriptSourceFitMeasurements.map(
      ([name, kind, maximumShape, signedBytes, memoryUnits, cpuUnits]) => ({
        name,
        kind,
        maximumShape,
        signedBytes,
        memoryUnits,
        cpuUnits,
      }),
    ),
  });

describe("missingScriptSource signed Van Rossem fit ledger", () => {
  it("reproduces publication and real accepted/forced lifecycle margins", async () => {
    const ledger = buildMissingScriptSourceFitLedger();
    const url = new URL(
      "../../../docs/fault-proofs/size-plans/missing-script-source-v1-fit-ledger.json",
      import.meta.url,
    );
    if (process.env.MIDGARD_UPDATE_MISSING_SCRIPT_SOURCE_LEDGER === "1")
      await writeVanRossemFitLedger(url.pathname, ledger);
    expect(ledger.entries).toHaveLength(
      missingScriptSourceFitMeasurements.length,
    );
    expect(
      ledger.entries.every(
        ({ signedByteMargin, memoryUnitMargin, cpuUnitMargin }) =>
          signedByteMargin > 0 &&
          BigInt(memoryUnitMargin) > 0n &&
          BigInt(cpuUnitMargin) > 0n,
      ),
    ).toBe(true);
    // Every lifecycle row also stays inside the 20% execution reserve the
    // lifecycle suite enforces (13,200,000 memory / 8,000,000,000 CPU).
    expect(
      ledger.entries
        .filter(({ kind }) => kind === "lifecycle")
        .every(
          ({ memoryUnitMargin, cpuUnitMargin }) =>
            BigInt(memoryUnitMargin) >= 3_300_000n &&
            BigInt(cpuUnitMargin) >= 2_000_000_000n,
        ),
    ).toBe(true);
    expect(
      ledger.entries
        .filter(({ kind }) => kind === "publication")
        .every(
          ({ publicationReserveMargin }) =>
            publicationReserveMargin !== null && publicationReserveMargin >= 0,
        ),
    ).toBe(true);
    expect(JSON.parse(await readFile(url, "utf8"))).toStrictEqual(ledger);
  });
});
