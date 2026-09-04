import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger.js";

const maximum = "32,768-byte certified field preimage";
// Emulator signing keys affect a few byte-comparison execution paths. For a
// stable release artifact, these are the conservative per-transaction maxima
// observed while reproducing each fixed shape; signed sizes are invariant.
const measurements = [
  [
    "accepted-cancel-step01",
    "lifecycle",
    "carriage-independent cancel",
    611,
    113440n,
    40648708n,
  ],
  [
    "accepted-cancel-step02",
    "lifecycle",
    "carriage-independent cancel",
    611,
    115208n,
    40916566n,
  ],
  [
    "accepted-cancel-step03",
    "lifecycle",
    "carriage-independent cancel",
    611,
    111876n,
    40368424n,
  ],
  [
    "forced-cancel-step02",
    "lifecycle",
    "carriage-independent cancel",
    611,
    115008n,
    40884566n,
  ],
  [
    "accepted-inline-authenticate",
    "lifecycle",
    "inline field carriage",
    713,
    885473n,
    292909632n,
  ],
  [
    "accepted-certified-certificate",
    "lifecycle",
    maximum,
    1317,
    507413n,
    226089381n,
  ],
  ["accepted-certified-init", "lifecycle", maximum, 1497, 679270n, 233044272n],
  [
    "accepted-certified-dispatch",
    "lifecycle",
    maximum,
    2497,
    1302918n,
    441083370n,
  ],
  [
    "accepted-certified-authenticate",
    "lifecycle",
    maximum,
    821,
    1115076n,
    396779323n,
  ],
  [
    "accepted-certified-final-mint",
    "lifecycle",
    maximum,
    916,
    273466n,
    98671803n,
  ],
  [
    "accepted-certified-remove",
    "lifecycle",
    maximum,
    2060,
    3043306n,
    1034465994n,
  ],
  [
    "forced-dispatch",
    "lifecycle",
    "forced exact-reason membership",
    616,
    127982n,
    44564932n,
  ],
  [
    "forced-authenticate",
    "lifecycle",
    "forced exact-reason membership",
    1724,
    1107407n,
    450760052n,
  ],
  [
    "forced-final-mint",
    "lifecycle",
    "forced exact-reason membership",
    916,
    304801n,
    109041571n,
  ],
  [
    "step01-reference-publication",
    "publication",
    "fully applied testnet validator",
    9764,
    0n,
    0n,
  ],
  [
    "step02-accepted-reference-publication",
    "publication",
    "fully applied testnet validator",
    10349,
    0n,
    0n,
  ],
  [
    "step02-forced-reference-publication",
    "publication",
    "fully applied testnet validator",
    12558,
    0n,
    0n,
  ],
  [
    "step03-reference-publication",
    "publication",
    "fully applied testnet validator",
    2308,
    0n,
    0n,
  ],
  [
    "raw-utxo-14337-publication",
    "publication",
    "14,337-byte RawUtxo boundary",
    15036,
    0n,
    0n,
  ],
  ["certified-32768-chunk01", "publication", maximum, 15872, 0n, 0n],
  ["certified-32768-chunk02", "publication", maximum, 15872, 0n, 0n],
  ["certified-32768-chunk03", "publication", maximum, 2800, 0n, 0n],
] as const;

describe("field-preimage-length-mismatch Van Rossem fit ledger", () => {
  it("reproduces the blueprint-bound machine-readable ledger with positive margins", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "fieldPreimageLengthMismatch:00000020:testnet",
      blueprintSha256:
        "99c8108c2fb404035c10aec076ab37493804b967fa347f6b31428c102feb5a7d",
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: measurements.map(
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
    expect(ledger.entries).toHaveLength(measurements.length);
    expect(
      ledger.entries.every(({ signedByteMargin }) => signedByteMargin > 0),
    ).toBe(true);
    expect(
      ledger.entries.every(
        ({ memoryUnitMargin, cpuUnitMargin }) =>
          BigInt(memoryUnitMargin) > 0n && BigInt(cpuUnitMargin) > 0n,
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
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(JSON.stringify(ledger, null, 2));
    const stored: unknown = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/field-preimage-length-mismatch-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    expect(stored).toStrictEqual(ledger);
  });
});
