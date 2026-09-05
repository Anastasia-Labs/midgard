import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";

/**
 * Every row is transcribed from `mint-declared-asset-limit-lifecycle.test.ts`
 * and `submit-init-emulator-mint-declared-asset-limit-publication.test.ts`
 * run with `MIDGARD_PRINT_FIT=1` against the pinned blueprint below. A
 * regeneration that moves the blueprint digest re-measures both suites and
 * rewrites the JSON with `MIDGARD_UPDATE_MINT_DECLARED_ASSET_LIMIT_LEDGER=1`.
 */
const PINNED_BLUEPRINT_SHA256 =
  "d1ac61daef73a015ee617382b52bfa5cd0ca806e99ef10a0d1e99353c69f353c";

const ACCEPTED_MAXIMUM =
  "62 policies (1000-asset first policy, 60 singletons, header crossing at 16,385); exact 32768-byte certified field; 192-unit fold budget";

export const mintDeclaredAssetLimitFitMeasurements = [
  // Reference publications of the four applied scripts.
  [
    "reference-step-01",
    "publication",
    "fully applied production script",
    14712,
    0n,
    0n,
  ],
  [
    "reference-step-02",
    "publication",
    "fully applied production script",
    11664,
    0n,
    0n,
  ],
  [
    "reference-step-03",
    "publication",
    "fully applied production script",
    11231,
    0n,
    0n,
  ],
  [
    "reference-step-04",
    "publication",
    "fully applied production script",
    2214,
    0n,
    0n,
  ],
  // Carriage of every field the lifecycles open.
  [
    "certified-carriage-chunk-00",
    "publication",
    "first maximum certified field chunk",
    15872,
    0n,
    0n,
  ],
  [
    "certified-carriage-chunk-01",
    "publication",
    "second maximum certified field chunk",
    15872,
    0n,
    0n,
  ],
  [
    "certified-carriage-chunk-02",
    "publication",
    "terminal certified field chunk",
    2800,
    0n,
    0n,
  ],
  [
    "certified-carriage-certificate",
    "publication",
    "field-5 certificate over three chunks",
    1317,
    508943n,
    226720101n,
  ],
  [
    "raw-carriage-honest",
    "publication",
    "two-singleton honest field-5 preimage",
    325,
    0n,
    0n,
  ],
  [
    "raw-carriage-boundary",
    "publication",
    "singleton plus a header declaring exactly the bound in total",
    329,
    0n,
    0n,
  ],
  [
    "raw-carriage-forced",
    "publication",
    "300-asset forced field-5 preimage",
    1527,
    0n,
    0n,
  ],
  [
    "raw-carriage-honest-rejection",
    "publication",
    "forced field-5 preimage whose header crosses at policy 0",
    290,
    0n,
    0n,
  ],
  // The maximum accepted crossing through the production actuator.
  ["init", "lifecycle", ACCEPTED_MAXIMUM, 1497, 676000n, 231986817n],
  [
    "step-01-accepted",
    "lifecycle",
    ACCEPTED_MAXIMUM,
    2264,
    1352941n,
    458550430n,
  ],
  [
    "grammar-start",
    "lifecycle",
    "first 24 of 62 field items",
    1180,
    3792665n,
    1981578793n,
  ],
  [
    "grammar-resume-01",
    "lifecycle",
    "second 24 of 62 field items",
    1272,
    4071164n,
    2123487439n,
  ],
  [
    "grammar-resume-02",
    "lifecycle",
    "last 14 of 62 field items",
    1272,
    2829915n,
    1414795032n,
  ],
  [
    "grammar-finish",
    "lifecycle",
    "terminal grammar checkpoint and target header",
    1308,
    6772282n,
    4063315651n,
  ],
  [
    "fold-00",
    "lifecycle",
    "open the 1000-asset policy and consume 184 entries",
    1310,
    8903506n,
    2833524169n,
  ],
  [
    "fold-01",
    "lifecycle",
    "192 entries inside the open policy (full entry budget)",
    1312,
    9156183n,
    2907515829n,
  ],
  [
    "fold-02",
    "lifecycle",
    "192 entries inside the open policy (full entry budget)",
    1312,
    9156183n,
    2907515829n,
  ],
  [
    "fold-03",
    "lifecycle",
    "192 entries inside the open policy (full entry budget)",
    1311,
    9156183n,
    2907515829n,
  ],
  [
    "fold-04",
    "lifecycle",
    "192 entries inside the open policy (full entry budget)",
    1311,
    9156183n,
    2907515829n,
  ],
  [
    "fold-05",
    "lifecycle",
    "64 entries close the wide policy; 14 singleton policies follow",
    1304,
    9367944n,
    3917228044n,
  ],
  [
    "fold-06",
    "lifecycle",
    "21 singleton policies (full policy budget)",
    1304,
    9044832n,
    4059268248n,
  ],
  [
    "fold-07",
    "lifecycle",
    "21 singleton policies (full policy budget)",
    1304,
    9044832n,
    4059268248n,
  ],
  [
    "fold-08",
    "lifecycle",
    "four singleton policies and the target header crossing at 16,385",
    1201,
    2122381n,
    912566005n,
  ],
  [
    "permanent-proof-mint",
    "lifecycle",
    "terminal accepted contradiction",
    916,
    260039n,
    94942988n,
  ],
  [
    "mutation-leased-removal",
    "lifecycle",
    "target plus descendant removal with proof token by reference",
    2060,
    3007744n,
    1022837034n,
  ],
  // The honest accepted transaction reaches the terminal step and is refused.
  [
    "init-honest",
    "lifecycle",
    "honest two-singleton transaction",
    1497,
    676000n,
    231986817n,
  ],
  [
    "step-01-honest",
    "lifecycle",
    "honest two-singleton transaction",
    2260,
    1350426n,
    457612676n,
  ],
  [
    "honest-direct-field",
    "lifecycle",
    "complete two-singleton field opening",
    1101,
    728359n,
    234910198n,
  ],
  [
    "honest-complete-fold",
    "lifecycle",
    "complete non-crossing fold the terminal step refuses",
    1088,
    1294055n,
    448625033n,
  ],
  [
    "init-outside-coordinate",
    "lifecycle",
    "policy index past the field's item count",
    1497,
    676000n,
    231986817n,
  ],
  [
    "step-01-outside-coordinate",
    "lifecycle",
    "policy index past the field's item count, refused at step 02",
    2260,
    1350426n,
    457612676n,
  ],
  // The exact boundary opens the bound item instead of crossing.
  [
    "init-boundary",
    "lifecycle",
    "singleton plus a header declaring exactly the bound in total",
    1497,
    676000n,
    231986817n,
  ],
  [
    "step-01-boundary",
    "lifecycle",
    "singleton plus a header declaring exactly the bound in total",
    2260,
    1350426n,
    457612676n,
  ],
  [
    "boundary-direct-field",
    "lifecycle",
    "complete boundary field opening",
    1103,
    733179n,
    236376008n,
  ],
  [
    "boundary-open-target",
    "lifecycle",
    "16,384 declared in total opens the bound item as a non-crossing fold",
    1220,
    1243254n,
    441653318n,
  ],
  // Cancellation from every physical step.
  [
    "cancel-step-01",
    "lifecycle",
    "cancel bound Init output",
    611,
    124408n,
    42388566n,
  ],
  [
    "cancel-step-02",
    "lifecycle",
    "cancel authenticated policy coordinate",
    611,
    113008n,
    40564566n,
  ],
  [
    "cancel-step-03",
    "lifecycle",
    "cancel declared-count fold",
    611,
    112608n,
    40500566n,
  ],
  [
    "cancel-step-04",
    "lifecycle",
    "cancel finalized contradiction",
    611,
    111876n,
    40368424n,
  ],
  // The exact forced wrongful rejection, resumed inside the policy item.
  [
    "step-01-forced",
    "lifecycle",
    "exact MintDeclaredAssetLimit wrongful rejection",
    1794,
    802892n,
    349665285n,
  ],
  [
    "forced-direct-field",
    "lifecycle",
    "complete 300-asset forced field opening",
    1148,
    668635n,
    219064234n,
  ],
  [
    "forced-fold-00",
    "lifecycle",
    "open the 300-asset target and consume 184 entries",
    1241,
    8540986n,
    2630802738n,
  ],
  [
    "forced-fold-01",
    "lifecycle",
    "116 entries close the target: complete non-crossing result",
    1133,
    5631172n,
    1740063956n,
  ],
  [
    "forced-permanent-proof-mint",
    "lifecycle",
    "terminal forced contradiction",
    916,
    295698n,
    106633601n,
  ],
  // The honest forced rejection reaches its crossing and is refused.
  [
    "step-01-honest-rejection",
    "lifecycle",
    "rightly rejected crossing at policy 0",
    1793,
    800579n,
    348862192n,
  ],
  [
    "honest-rejection-direct-field",
    "lifecycle",
    "complete crossing-header field opening",
    1148,
    663815n,
    216379965n,
  ],
  [
    "honest-rejection-crossing",
    "lifecycle",
    "the crossing decision the terminal step refuses",
    1133,
    880156n,
    306089162n,
  ],
] as const;

export const buildMintDeclaredAssetLimitFitLedger = () =>
  buildVanRossemFitLedger({
    category: "mintDeclaredAssetLimit",
    blueprintSha256: PINNED_BLUEPRINT_SHA256,
    compilerVersion: "v1.1.23+5adf783",
    measurements: mintDeclaredAssetLimitFitMeasurements.map(
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

describe("mintDeclaredAssetLimit signed Van Rossem fit ledger", () => {
  it("reproduces every publication and maximum lifecycle row", async () => {
    const ledger = buildMintDeclaredAssetLimitFitLedger();
    const url = new URL(
      "../../../docs/fault-proofs/size-plans/mint-declared-asset-limit-v1-fit-ledger.json",
      import.meta.url,
    );
    if (process.env.MIDGARD_UPDATE_MINT_DECLARED_ASSET_LIMIT_LEDGER === "1")
      await writeVanRossemFitLedger(url.pathname, ledger);
    expect(ledger.entries).toHaveLength(
      mintDeclaredAssetLimitFitMeasurements.length,
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
    // The heaviest fold transaction and the full-entry-budget transactions
    // keep more than a third of the memory limit and half of the CPU limit.
    const folds = ledger.entries.filter(({ name }) =>
      /^(forced-)?fold-/u.test(name),
    );
    expect(folds.length).toBeGreaterThanOrEqual(11);
    for (const fold of folds) {
      expect(BigInt(fold.memoryUnitMargin), fold.name).toBeGreaterThan(
        5_500_000n,
      );
      expect(BigInt(fold.cpuUnitMargin), fold.name).toBeGreaterThan(
        5_000_000_000n,
      );
    }
    const stored: unknown = JSON.parse(await readFile(url, "utf8"));
    expect(stored).toStrictEqual(ledger);
  });
});
