import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedgerV1 } from "../src/proof-fit/van-rossem-fit-ledger-v1.js";

export const mintDeclaredAssetLimitFitMeasurementsV1 = [
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
    11443,
    0n,
    0n,
  ],
  [
    "reference-step-03",
    "publication",
    "fully applied production script",
    10219,
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
  [
    "raw-carriage-publication",
    "publication",
    "complete forced field-5 preimage",
    286,
    0n,
    0n,
  ],
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
    514679n,
    228504595n,
  ],
  [
    "init",
    "lifecycle",
    "49 policies; exact 32768-byte certified field; 24-policy batches",
    1497,
    676000n,
    231986817n,
  ],
  [
    "step-01-accepted",
    "lifecycle",
    "49 policies; exact 32768-byte certified field; 24-policy batches",
    1986,
    1312005n,
    445637538n,
  ],
  [
    "grammar-start",
    "lifecycle",
    "first 24 of 49 field items",
    1180,
    3781651n,
    1978182887n,
  ],
  [
    "grammar-resume-01",
    "lifecycle",
    "second 24 of 49 field items",
    1272,
    4062560n,
    2120810698n,
  ],
  [
    "grammar-resume-02",
    "lifecycle",
    "last of 49 field items",
    1272,
    1192806n,
    485002548n,
  ],
  [
    "grammar-finish",
    "lifecycle",
    "terminal grammar checkpoint and target header",
    1303,
    5587786n,
    3301442768n,
  ],
  [
    "fold-00",
    "lifecycle",
    "first 24 complete policies",
    1298,
    8502040n,
    3861450993n,
  ],
  [
    "fold-01",
    "lifecycle",
    "second 24 complete policies",
    1298,
    8516927n,
    3866869067n,
  ],
  [
    "fold-02-first-crossing",
    "lifecycle",
    "target policy declares 16385 before body completion",
    1200,
    1333411n,
    553904398n,
  ],
  [
    "permanent-proof-mint",
    "lifecycle",
    "terminal accepted contradiction",
    916,
    264663n,
    96311833n,
  ],
  [
    "mutation-leased-removal",
    "lifecycle",
    "target plus descendant removal with proof token by reference",
    2060,
    3051824n,
    1036600810n,
  ],
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
    112408n,
    40468566n,
  ],
  [
    "cancel-step-04",
    "lifecycle",
    "cancel finalized contradiction",
    611,
    111876n,
    40368424n,
  ],
  [
    "step-01-forced",
    "lifecycle",
    "exact MintDeclaredAssetLimit wrongful rejection",
    1721,
    721561n,
    324394866n,
  ],
  [
    "forced-direct-field",
    "lifecycle",
    "complete singleton forced field opening",
    1141,
    650371n,
    211513770n,
  ],
  [
    "forced-complete-fold",
    "lifecycle",
    "complete non-crossing singleton fold",
    1132,
    911225n,
    311648905n,
  ],
  [
    "forced-permanent-proof-mint",
    "lifecycle",
    "terminal forced contradiction",
    916,
    291074n,
    105264756n,
  ],
] as const;

export const buildMintDeclaredAssetLimitFitLedgerV1 = () =>
  buildVanRossemFitLedgerV1({
    category: "mintDeclaredAssetLimit",
    blueprintSha256:
      "179c65539806f39a85008527e3572eb31bcd792d0a701aaa6bc04c60938c021f",
    compilerVersion: "v1.1.23+5adf783",
    measurements: mintDeclaredAssetLimitFitMeasurementsV1.map(
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
    const ledger = buildMintDeclaredAssetLimitFitLedgerV1();
    expect(ledger.entries).toHaveLength(
      mintDeclaredAssetLimitFitMeasurementsV1.length,
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
    const stored: unknown = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/mint-declared-asset-limit-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    expect(stored).toStrictEqual(ledger);
  });
});
