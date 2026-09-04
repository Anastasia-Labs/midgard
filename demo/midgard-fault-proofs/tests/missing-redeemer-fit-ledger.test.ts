import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import {
  MISSING_REDEEMER_MAXIMUM_FIELD_BYTES,
  MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT,
} from "./support/missing-redeemer-emulator.js";

/**
 * Measured by `missing-redeemer-lifecycle.test.ts` (`MIDGARD_PRINT_FIT=1`)
 * against the fresh locked testnet blueprint named below, on the shared Van
 * Rossem emulator parameters with local UPLC evaluation.
 *
 * The accepted rows are the maximum supported shape: the exact 32,768-byte
 * field 8 (17 items, tier-3 certified carriage: two full 15,148-byte chunks
 * plus a remainder), whose first walk batch scans the widest 16 items the
 * family can be asked to decode in one transaction. The forced rows are the
 * wrongful `RedeemerMissing` rejection of a mint purpose whose Plutus source
 * is a reference script; the cancel rows burn the thread from every
 * nonterminal physical validator, the grammar state and the mid-walk state
 * included.
 */
const MAXIMUM = `${MISSING_REDEEMER_MAXIMUM_SHAPE_ITEM_COUNT.toString()}-item ${MISSING_REDEEMER_MAXIMUM_FIELD_BYTES.toString()}-byte certified field 8`;
const FORCED = "wrongful mint-purpose rejection, reference-script source";
const CANCEL = "restart-safe authenticated thread cancellation";

export const missingRedeemerFitMeasurements = [
  [
    "reference-step-01",
    "publication",
    "fully applied production script",
    15_129,
    0n,
    0n,
  ],
  [
    "reference-step-02",
    "publication",
    "fully applied production script",
    7_616,
    0n,
    0n,
  ],
  [
    "reference-step-02a",
    "publication",
    "fully applied production script",
    12_192,
    0n,
    0n,
  ],
  [
    "reference-step-02b",
    "publication",
    "fully applied production script",
    5_291,
    0n,
    0n,
  ],
  [
    "reference-step-03",
    "publication",
    "fully applied production script",
    10_601,
    0n,
    0n,
  ],
  [
    "reference-step-04",
    "publication",
    "fully applied production script",
    9_755,
    0n,
    0n,
  ],
  [
    "reference-step-05",
    "publication",
    "fully applied production script",
    2_185,
    0n,
    0n,
  ],
  [
    "accepted-carriage-chunk-0",
    "lifecycle",
    `${MAXIMUM}, full 15,148-byte chunk`,
    15_872,
    0n,
    0n,
  ],
  [
    "accepted-carriage-chunk-1",
    "lifecycle",
    `${MAXIMUM}, full 15,148-byte chunk`,
    15_872,
    0n,
    0n,
  ],
  [
    "accepted-carriage-chunk-2",
    "lifecycle",
    `${MAXIMUM}, remainder chunk`,
    2_800,
    0n,
    0n,
  ],
  [
    "accepted-certify-field-8",
    "lifecycle",
    `${MAXIMUM}, tier-3 certificate mint`,
    1_318,
    515_270n,
    228_198_629n,
  ],
  [
    "accepted-init",
    "lifecycle",
    `${MAXIMUM}, wrongful acceptance`,
    1_641,
    757_757n,
    257_922_445n,
  ],
  [
    "accepted-step-01",
    "lifecycle",
    "accepted transaction inclusion",
    2_062,
    1_338_271n,
    455_095_123n,
  ],
  [
    "accepted-step-02",
    "lifecycle",
    "trace descriptor authentication",
    1_174,
    252_808n,
    115_962_096n,
  ],
  [
    "accepted-step-02a",
    "lifecycle",
    `${MAXIMUM}, stage-10 control authentication`,
    2_776,
    1_724_109n,
    768_270_438n,
  ],
  [
    "accepted-step-02b",
    "lifecycle",
    "purpose and reference-source selection",
    914,
    407_192n,
    158_145_788n,
  ],
  [
    "accepted-step-03-grammar-start",
    "lifecycle",
    `${MAXIMUM}, first 16-item grammar batch`,
    1_426,
    2_153_518n,
    975_110_784n,
  ],
  [
    "accepted-step-03-grammar-resume",
    "lifecycle",
    `${MAXIMUM}, resumed grammar batch`,
    1_518,
    1_232_585n,
    502_664_333n,
  ],
  [
    "accepted-step-03-grammar-finish",
    "lifecycle",
    `${MAXIMUM}, certified walk start`,
    1_522,
    1_032_134n,
    377_760_420n,
  ],
  [
    "accepted-step-04-batch-16",
    "lifecycle",
    `${MAXIMUM}, widest 16-item pointer batch`,
    1_486,
    5_019_938n,
    2_182_994_737n,
  ],
  [
    "accepted-step-04-batch-final",
    "lifecycle",
    `${MAXIMUM}, resumed final batch to absence`,
    1_378,
    1_419_675n,
    586_217_664n,
  ],
  [
    "accepted-step-05",
    "lifecycle",
    "permanent proof mint",
    916,
    261_411n,
    95_364_479n,
  ],
  [
    "accepted-remove-target",
    "lifecycle",
    "state-queue target removal under mutation lease",
    2_429,
    3_067_886n,
    1_046_152_083n,
  ],
  [
    "accepted-remove-descendant",
    "lifecycle",
    "descendant removal under mutation lease",
    1_544,
    1_714_224n,
    577_892_819n,
  ],
  [
    "forced-carriage-chunk",
    "lifecycle",
    `${FORCED}, raw carriage`,
    282,
    0n,
    0n,
  ],
  ["forced-init", "lifecycle", FORCED, 1_641, 766_961n, 260_695_186n],
  [
    "forced-step-01",
    "lifecycle",
    "forced transaction membership",
    1_797,
    1_109_640n,
    463_173_246n,
  ],
  [
    "forced-step-02",
    "lifecycle",
    "rejected trace descriptor authentication",
    1_222,
    321_539n,
    146_986_639n,
  ],
  [
    "forced-step-02a",
    "lifecycle",
    `${FORCED}, stage-10 control authentication`,
    2_659,
    1_608_986n,
    734_479_696n,
  ],
  [
    "forced-step-02b",
    "lifecycle",
    "mint purpose and reference-source selection",
    961,
    408_092n,
    158_892_134n,
  ],
  [
    "forced-step-03-direct",
    "lifecycle",
    "direct raw-carriage opening",
    1_364,
    611_715n,
    205_365_086n,
  ],
  [
    "forced-step-04",
    "lifecycle",
    "exact pointer match",
    1_312,
    1_075_529n,
    372_741_478n,
  ],
  [
    "forced-step-05",
    "lifecycle",
    "permanent proof mint",
    916,
    289_078n,
    104_714_000n,
  ],
  [
    "forced-remove-target",
    "lifecycle",
    "state-queue target removal under mutation lease",
    2_429,
    3_040_867n,
    1_043_189_678n,
  ],
  [
    "forced-remove-descendant",
    "lifecycle",
    "descendant removal under mutation lease",
    1_544,
    1_706_202n,
    580_030_975n,
  ],
  ["cancel-step-01", "lifecycle", CANCEL, 611, 124_808n, 42_452_566n],
  ["cancel-step-02", "lifecycle", CANCEL, 611, 111_876n, 40_368_424n],
  ["cancel-step-02a", "lifecycle", CANCEL, 611, 112_776n, 40_512_424n],
  ["cancel-step-02b", "lifecycle", CANCEL, 611, 111_276n, 40_272_424n],
  [
    "cancel-step-03-grammar",
    "lifecycle",
    `${CANCEL}, certified grammar state`,
    611,
    113_008n,
    40_564_566n,
  ],
  [
    "cancel-step-04-mid-walk",
    "lifecycle",
    `${CANCEL}, mid-walk checkpoint`,
    611,
    112_408n,
    40_468_566n,
  ],
  ["cancel-step-05", "lifecycle", CANCEL, 611, 111_876n, 40_368_424n],
] as const;

export const buildMissingRedeemerFitLedger = () =>
  buildVanRossemFitLedger({
    category: "missingRedeemer",
    blueprintSha256:
      "ae5d600efa7ac46b2e58286d125d6b94521e010493a4c0d93c4b2e97faf435ef",
    compilerVersion: "v1.1.23+5adf783",
    measurements: missingRedeemerFitMeasurements.map(
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

describe("missingRedeemer signed Van Rossem fit ledger", () => {
  it("reproduces publication and real accepted/forced lifecycle margins", async () => {
    const ledger = buildMissingRedeemerFitLedger();
    const url = new URL(
      "../../../docs/fault-proofs/size-plans/missing-redeemer-v1-fit-ledger.json",
      import.meta.url,
    );
    if (process.env.MIDGARD_UPDATE_MISSING_REDEEMER_LEDGER === "1")
      await writeVanRossemFitLedger(url.pathname, ledger);
    expect(ledger.entries).toHaveLength(missingRedeemerFitMeasurements.length);
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
    // The widest batch is the most expensive transaction of the family and
    // still leaves more than half of every Van Rossem budget.
    const widest = ledger.entries.find(
      ({ name }) => name === "accepted-step-04-batch-16",
    )!;
    expect(BigInt(widest.memoryUnitMargin)).toBeGreaterThan(8_250_000n);
    expect(BigInt(widest.cpuUnitMargin)).toBeGreaterThan(5_000_000_000n);
    expect(JSON.parse(await readFile(url, "utf8"))).toStrictEqual(ledger);
  });
});
