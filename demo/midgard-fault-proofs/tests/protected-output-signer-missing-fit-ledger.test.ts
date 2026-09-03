import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger-v1.js";

const lifecycle = "318 address witnesses; three-chunk Certified field 7";
const measurements = [
  ["accepted-cancel-step01", "lifecycle", 611, 124408n, 42388566n],
  ["accepted-carriage-certificate", "lifecycle", 1317, 511858n, 227246923n],
  ["accepted-init", "lifecycle", 1641, 776377n, 263764431n],
  [
    "accepted-remove-fraudulent-block",
    "lifecycle",
    2361,
    3001795n,
    1032228786n,
  ],
  ["accepted-step01", "lifecycle", 2020, 1318034n, 447947053n],
  ["accepted-step02", "lifecycle", 1135, 582522n, 181217778n],
  ["accepted-step03-witness-open", "lifecycle", 1383, 730835n, 241877968n],
  ["accepted-step04-resume-00", "lifecycle", 1438, 7908354n, 4008611708n],
  ["accepted-step04-resume-01", "lifecycle", 1438, 7908354n, 4008611708n],
  ["accepted-step04-resume-02", "lifecycle", 1438, 7908354n, 4008611708n],
  ["accepted-step04-resume-03", "lifecycle", 1438, 7908354n, 4008611708n],
  ["accepted-step04-resume-04", "lifecycle", 1438, 8154221n, 4100586641n],
  ["accepted-step04-resume-05", "lifecycle", 1438, 8459010n, 4179923132n],
  ["accepted-step04-resume-06", "lifecycle", 1438, 8459010n, 4179923132n],
  ["accepted-step04-resume-07", "lifecycle", 1438, 8459010n, 4179923132n],
  ["accepted-step04-resume-08", "lifecycle", 1438, 8459010n, 4179923132n],
  ["accepted-step04-terminal", "lifecycle", 1302, 8377185n, 3137903060n],
  ["accepted-step05-proof-mint", "lifecycle", 916, 258148n, 94250551n],
  ["field7-carriage-chunk01", "publication", 15872, 0n, 0n],
  ["field7-carriage-chunk02", "publication", 15872, 0n, 0n],
  ["field7-carriage-chunk03", "publication", 2789, 0n, 0n],
  ["step01-reference-publication", "publication", 14827, 0n, 0n],
  ["step02-reference-publication", "publication", 9239, 0n, 0n],
  ["step03-reference-publication", "publication", 7488, 0n, 0n],
  ["step04-reference-publication", "publication", 9119, 0n, 0n],
  ["step05-reference-publication", "publication", 2214, 0n, 0n],
] as const;

describe("protectedOutputSignerMissing signed Van Rossem fit ledger", () => {
  it("reproduces positive byte, memory, CPU and publication-reserve margins", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "protectedOutputSignerMissing:0000002b:testnet",
      blueprintSha256:
        "61ec67157434a1904ddac0a355337a1656d1ef62448744fa2856d0a1aa1602cb",
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: measurements.map(
        ([name, kind, signedBytes, memoryUnits, cpuUnits]) => ({
          name,
          kind,
          maximumShape:
            kind === "publication"
              ? "fully applied validator or exact 15,148-byte carriage chunk"
              : lifecycle,
          signedBytes,
          memoryUnits,
          cpuUnits,
        }),
      ),
    });
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(JSON.stringify(ledger, null, 2));
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
            (publicationReserveMargin ?? -1) >= 0,
        ),
    ).toBe(true);
    const stored: unknown = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/protected-output-signer-missing-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    expect(stored).toStrictEqual(ledger);
  });
});
