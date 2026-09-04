import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";

export const redeemerCanonicityFitMeasurements = [
  [
    "reference-step-01",
    "publication",
    "fully applied production script",
    14833,
    0n,
    0n,
  ],
  [
    "reference-step-02",
    "publication",
    "fully applied production script",
    11763,
    0n,
    0n,
  ],
  [
    "reference-step-03",
    "publication",
    "fully applied production script",
    2220,
    0n,
    0n,
  ],
  [
    "init",
    "lifecycle",
    "224 redeemers; certified field-8 carriage",
    1641,
    758415n,
    258297722n,
  ],
  [
    "step-01-accepted",
    "lifecycle",
    "accepted source and exact coordinate binding",
    2015,
    1310893n,
    445594492n,
  ],
  [
    "authenticate-certified-maximum-field",
    "lifecycle",
    "exact malformed item in retained 224-item field",
    1248,
    1024902n,
    366367119n,
  ],
  [
    "permanent-proof-mint",
    "lifecycle",
    "terminal accepted contradiction",
    916,
    263468n,
    95965126n,
  ],
  [
    "mutation-leased-removal",
    "lifecycle",
    "target plus successor removal",
    1544,
    1678487n,
    571611300n,
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
    "cancel authenticated redeemer coordinate",
    611,
    112408n,
    40468566n,
  ],
  [
    "cancel-step-03",
    "lifecycle",
    "cancel finalized canonicality state",
    611,
    111876n,
    40368424n,
  ],
  [
    "step-01-forced",
    "lifecycle",
    "exact RedeemerMalformed wrongful rejection",
    1754,
    1054986n,
    442438557n,
  ],
  [
    "forced-direct-field",
    "lifecycle",
    "canonical redeemer item contradiction",
    1192,
    702672n,
    221143725n,
  ],
  [
    "forced-permanent-proof-mint",
    "lifecycle",
    "terminal forced contradiction",
    916,
    289579n,
    104870049n,
  ],
] as const;

export const buildRedeemerCanonicityFitLedger = () =>
  buildVanRossemFitLedger({
    category: "redeemerCanonicity",
    blueprintSha256:
      "e0ce6388662482aaa614e94a2eef0fa63c48f1729e1a9ed9507c22759e27e756",
    compilerVersion: "v1.1.23+5adf783",
    measurements: redeemerCanonicityFitMeasurements.map(
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

describe("redeemerCanonicity signed Van Rossem fit ledger", () => {
  it("reproduces every publication and maximum lifecycle row", async () => {
    const ledger = buildRedeemerCanonicityFitLedger();
    const url = new URL(
      "../../../docs/fault-proofs/size-plans/redeemer-canonicity-v1-fit-ledger.json",
      import.meta.url,
    );
    if (process.env.MIDGARD_UPDATE_REDEEMER_CANONICITY_LEDGER === "1")
      await writeVanRossemFitLedger(url.pathname, ledger);
    expect(ledger.entries).toHaveLength(
      redeemerCanonicityFitMeasurements.length,
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
