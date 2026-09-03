import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger-v1.js";

const measurements = [
  ["accepted-cancel-step01", "lifecycle", 611, 124408n, 42388566n],
  ["accepted-cancel-step02", "lifecycle", 611, 112408n, 40468566n],
  ["accepted-cancel-step03", "lifecycle", 611, 111676n, 40336424n],
  ["accepted-cancel-step04", "lifecycle", 611, 112876n, 40528424n],
  ["accepted-init", "lifecycle", 1497, 667200n, 229480492n],
  ["accepted-carriage-chunk01", "lifecycle", 15872, 0n, 0n],
  ["accepted-carriage-chunk02", "lifecycle", 15872, 0n, 0n],
  ["accepted-carriage-chunk03", "lifecycle", 2106, 0n, 0n],
  ["accepted-carriage-certificate", "lifecycle", 1246, 456130n, 177483230n],
  ["accepted-step01", "lifecycle", 1981, 1305605n, 443553415n],
  ["accepted-step02", "lifecycle", 1376, 873202n, 388451836n],
  ["accepted-step03-scan", "lifecycle", 9343, 637317n, 257251189n],
  ["accepted-step04-proof-mint", "lifecycle", 916, 515786n, 195058594n],
  [
    "accepted-remove-fraudulent-block",
    "lifecycle",
    2060,
    3050994n,
    1038487893n,
  ],
  ["step01-reference-publication", "publication", 14698, 0n, 0n],
  ["step02-reference-publication", "publication", 7422, 0n, 0n],
  ["step03-reference-publication", "publication", 11834, 0n, 0n],
  ["step04-reference-publication", "publication", 5177, 0n, 0n],
] as const;

describe("transactionOutputNonCanonical signed Van Rossem fit ledger", () => {
  it("reproduces positive byte, memory, CPU, and publication-reserve margins", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "transactionOutputNonCanonical:00000029:testnet",
      blueprintSha256:
        "179c65539806f39a85008527e3572eb31bcd792d0a701aaa6bc04c60938c021f",
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: measurements.map(
        ([name, kind, signedBytes, memoryUnits, cpuUnits]) => ({
          name,
          kind,
          maximumShape:
            kind === "publication"
              ? "fully applied testnet validator"
              : "16,384-byte selected output in 32,768-byte Certified field",
          signedBytes,
          memoryUnits,
          cpuUnits,
        }),
      ),
    });
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(JSON.stringify(ledger, null, 2));
    expect(ledger.entries).toHaveLength(measurements.length);
    expect(ledger.entries.every((entry) => entry.signedByteMargin > 0)).toBe(
      true,
    );
    expect(
      ledger.entries.every(
        (entry) =>
          BigInt(entry.memoryUnitMargin) > 0n &&
          BigInt(entry.cpuUnitMargin) > 0n,
      ),
    ).toBe(true);
    expect(
      ledger.entries
        .filter((entry) => entry.kind === "publication")
        .every((entry) => (entry.publicationReserveMargin ?? -1) >= 0),
    ).toBe(true);
    const stored: unknown = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/transaction-output-non-canonical-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    expect(stored).toStrictEqual(ledger);
  });
});
