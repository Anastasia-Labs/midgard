import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger.js";

const maximum = "32,768-byte certified field preimage";
// Emulator signing keys affect a few byte-comparison execution paths. For a
// stable release artifact, these are the conservative per-transaction maxima
// observed while reproducing each fixed shape; signed sizes are invariant.
const measurements = [
  [
    "accepted-cancel-step-01",
    "lifecycle",
    "carriage-independent cancel",
    611,
    113440n,
    40648708n,
  ],
  [
    "accepted-cancel-step-02-accepted",
    "lifecycle",
    "carriage-independent cancel",
    611,
    115208n,
    40916566n,
  ],
  [
    "accepted-cancel-step-03",
    "lifecycle",
    "carriage-independent cancel",
    611,
    111876n,
    40368424n,
  ],
  [
    "accepted-certified-authenticate",
    "lifecycle",
    maximum,
    821,
    1103604n,
    393210335n,
  ],
  [
    "accepted-certified-certificate",
    "lifecycle",
    maximum,
    1317,
    498809n,
    223412640n,
  ],
  [
    "accepted-certified-dispatch",
    "lifecycle",
    maximum,
    2497,
    1300050n,
    440191123n,
  ],
  [
    "accepted-certified-final-mint",
    "lifecycle",
    maximum,
    916,
    268842n,
    97302958n,
  ],
  ["accepted-certified-init", "lifecycle", maximum, 1497, 666998n, 229347284n],
  [
    "accepted-certified-remove",
    "lifecycle",
    maximum,
    2060,
    3072114n,
    1045313704n,
  ],
  [
    "accepted-dispatch",
    "lifecycle",
    "inline field carriage",
    2532,
    1294887n,
    439251379n,
  ],
  [
    "accepted-init",
    "lifecycle",
    "inline field carriage",
    1497,
    676596n,
    232224598n,
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
    "accepted-inline-final-mint",
    "lifecycle",
    "inline field carriage",
    916,
    273466n,
    98671803n,
  ],
  [
    "accepted-inline-remove",
    "lifecycle",
    "inline field carriage",
    1544,
    1711939n,
    581891518n,
  ],
  ["certified-32768-chunk01", "publication", maximum, 15872, 0n, 0n],
  ["certified-32768-chunk02", "publication", maximum, 15872, 0n, 0n],
  ["certified-32768-chunk03", "publication", maximum, 2800, 0n, 0n],
  [
    "forced-accepted-authenticate",
    "lifecycle",
    "forced accepted leaf, inline field carriage",
    1716,
    1070788n,
    435221767n,
  ],
  [
    "forced-accepted-dispatch",
    "lifecycle",
    "forced accepted leaf, inline field carriage",
    616,
    127781n,
    44480599n,
  ],
  [
    "forced-accepted-final-mint",
    "lifecycle",
    "forced accepted leaf, inline field carriage",
    916,
    274578n,
    99014039n,
  ],
  [
    "forced-accepted-init",
    "lifecycle",
    "forced accepted leaf, inline field carriage",
    1497,
    682338n,
    233968519n,
  ],
  [
    "forced-accepted-remove",
    "lifecycle",
    "forced accepted leaf, inline field carriage",
    2060,
    3092196n,
    1051590189n,
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
    "forced-cancel-step-02-forced",
    "lifecycle",
    "carriage-independent cancel",
    611,
    115008n,
    40884566n,
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
    "forced-final-mint",
    "lifecycle",
    "forced exact-reason membership",
    916,
    304801n,
    109041571n,
  ],
  [
    "forced-init",
    "lifecycle",
    "forced exact-reason membership",
    1497,
    685406n,
    234892766n,
  ],
  [
    "forced-remove",
    "lifecycle",
    "forced exact-reason membership",
    2060,
    3029332n,
    1035014482n,
  ],
  [
    "honest-accepted-cancel-step-03",
    "lifecycle",
    "carriage-independent cancel",
    611,
    111876n,
    40368424n,
  ],
  [
    "honest-forced-cancel-step-03",
    "lifecycle",
    "carriage-independent cancel",
    611,
    111876n,
    40368424n,
  ],
  [
    "raw-utxo-14337-publication",
    "publication",
    "14,337-byte RawUtxo boundary",
    15036,
    0n,
    0n,
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
] as const;

describe("field-preimage-length-mismatch Van Rossem fit ledger", () => {
  it("reproduces the blueprint-bound machine-readable ledger with positive margins", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "fieldPreimageLengthMismatch:00000020:testnet",
      blueprintSha256:
        "172c72d392706de52b5665dc0c1b208354a490298fba5ac0f411597f8454d10b",
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
