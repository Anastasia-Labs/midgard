import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedgerV1,
  writeVanRossemFitLedgerV1,
} from "../src/proof-fit/van-rossem-fit-ledger-v1.js";

export const missingScriptSourceFitMeasurementsV1 = [
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
    2_336,
    0n,
    0n,
  ],
  [
    "reference-step-05",
    "publication",
    "fully applied production script",
    5_534,
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
    "24-source accepted absence",
    1_365,
    712_147n,
    243_624_075n,
  ],
  [
    "accepted-step-01",
    "lifecycle",
    "accepted transaction inclusion",
    2_062,
    1_344_144n,
    456_804_191n,
  ],
  [
    "accepted-step-02",
    "lifecycle",
    "trace and descriptor authentication",
    1_990,
    743_967n,
    304_095_920n,
  ],
  [
    "accepted-step-03",
    "lifecycle",
    "complete purpose and source frontiers",
    1_991,
    1_258_879n,
    584_879_259n,
  ],
  [
    "accepted-step-04",
    "lifecycle",
    "initialize universal source scan",
    989,
    155_239n,
    80_422_420n,
  ],
  [
    "accepted-step-05-max-24",
    "lifecycle",
    "maximum 24-source universal absence batch",
    5_816,
    8_107_129n,
    3_170_683_719n,
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
    "reference-source wrongful rejection",
    1_365,
    712_147n,
    243_624_075n,
  ],
  [
    "forced-step-01",
    "lifecycle",
    "forced transaction membership",
    1_795,
    1_095_730n,
    450_772_709n,
  ],
  [
    "forced-step-02",
    "lifecycle",
    "matching ScriptSourceScan authentication",
    1_900,
    749_780n,
    316_838_785n,
  ],
  [
    "forced-step-03",
    "lifecycle",
    "complete authenticated frontiers",
    1_954,
    1_204_364n,
    562_978_554n,
  ],
  [
    "forced-step-04",
    "lifecycle",
    "initialize presence scan",
    995,
    155_191n,
    76_779_193n,
  ],
  [
    "forced-step-05",
    "lifecycle",
    "canonical resolved-reference match",
    1_251,
    727_026n,
    300_370_475n,
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
    1_719_026n,
    578_591_926n,
  ],
  [
    "cancel-after-step-02",
    "lifecycle",
    "restart-safe authenticated thread cancellation",
    611,
    112_376n,
    40_448_424n,
  ],
] as const;

export const buildMissingScriptSourceFitLedgerV1 = () =>
  buildVanRossemFitLedgerV1({
    category: "missingScriptSource",
    blueprintSha256:
      "ea6087b6e039af06feefde55d945addab8c804d4dfc0da0e9d20e96602ce10e4",
    compilerVersion: "v1.1.23+5adf783",
    measurements: missingScriptSourceFitMeasurementsV1.map(
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
    const ledger = buildMissingScriptSourceFitLedgerV1();
    const url = new URL(
      "../../../docs/fault-proofs/size-plans/missing-script-source-v1-fit-ledger.json",
      import.meta.url,
    );
    if (process.env.MIDGARD_UPDATE_MISSING_SCRIPT_SOURCE_LEDGER === "1")
      await writeVanRossemFitLedgerV1(url.pathname, ledger);
    expect(ledger.entries).toHaveLength(
      missingScriptSourceFitMeasurementsV1.length,
    );
    expect(
      ledger.entries.every(
        ({ signedByteMargin, memoryUnitMargin, cpuUnitMargin }) =>
          signedByteMargin > 0 &&
          BigInt(memoryUnitMargin) > 0n &&
          BigInt(cpuUnitMargin) > 0n,
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
