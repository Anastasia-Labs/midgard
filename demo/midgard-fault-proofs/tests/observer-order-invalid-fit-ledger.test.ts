import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";

export const observerOrderInvalidFitMeasurements = [
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
    7644,
    0n,
    0n,
  ],
  [
    "reference-step-03",
    "publication",
    "fully applied production script",
    9058,
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
    "certified-carriage-chunk-00",
    "publication",
    "first maximum 15153-byte field chunk",
    15872,
    0n,
    0n,
  ],
  [
    "certified-carriage-chunk-01",
    "publication",
    "terminal maximum field chunk",
    252,
    0n,
    0n,
  ],
  [
    "certified-carriage-certificate",
    "publication",
    "field-3 certificate over two chunks",
    1246,
    454768n,
    173519777n,
  ],
  [
    "init",
    "lifecycle",
    "505 observers; 15153-byte certified field",
    1641,
    772501n,
    262305899n,
  ],
  [
    "step-01-accepted",
    "lifecycle",
    "accepted source and exact index binding",
    1986,
    1306269n,
    443854497n,
  ],
  [
    "authenticate-certified-field",
    "lifecycle",
    "fixed-stride certified field-3 opening",
    1141,
    654486n,
    215794866n,
  ],
  [
    "scan-00",
    "lifecycle",
    "first maximum 24-observer batch",
    1228,
    5721036n,
    3010839985n,
  ],
  [
    "scan-01",
    "lifecycle",
    "second maximum 24-observer batch",
    1228,
    5728062n,
    3014260074n,
  ],
  [
    "scan-02-first-violation",
    "lifecycle",
    "duplicate at authenticated index 48",
    1163,
    1001734n,
    388810511n,
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
    "target plus descendant removal",
    2060,
    3044851n,
    1042105253n,
  ],
  [
    "raw-carriage-publication",
    "publication",
    "complete two-observer forced field",
    310,
    0n,
    0n,
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
    "cancel authenticated observer coordinate",
    611,
    112408n,
    40468566n,
  ],
  [
    "cancel-step-03",
    "lifecycle",
    "cancel observer scan",
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
    "exact ObserverOrderInvalid wrongful rejection",
    1721,
    706333n,
    320166971n,
  ],
  [
    "forced-direct-field",
    "lifecycle",
    "complete fixed-stride forced field opening",
    1110,
    492065n,
    165627638n,
  ],
  [
    "forced-complete-scan",
    "lifecycle",
    "ordered adjacent pair contradiction",
    1132,
    868053n,
    304029044n,
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

export const buildObserverOrderInvalidFitLedger = () =>
  buildVanRossemFitLedger({
    category: "observerOrderInvalid",
    blueprintSha256:
      "dd9cde6da423a5082a743e21020912fabc74848c32f52b496d9251d3dfa33b2a",
    compilerVersion: "v1.1.23+5adf783",
    measurements: observerOrderInvalidFitMeasurements.map(
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

describe("observerOrderInvalid signed Van Rossem fit ledger", () => {
  it("reproduces every publication and maximum lifecycle row", async () => {
    const ledger = buildObserverOrderInvalidFitLedger();
    const url = new URL(
      "../../../docs/fault-proofs/size-plans/observer-order-invalid-v1-fit-ledger.json",
      import.meta.url,
    );
    if (process.env.MIDGARD_UPDATE_OBSERVER_ORDER_LEDGER === "1")
      await writeVanRossemFitLedger(url.pathname, ledger);
    expect(ledger.entries).toHaveLength(
      observerOrderInvalidFitMeasurements.length,
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
