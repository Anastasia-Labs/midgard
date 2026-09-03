import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger-v1.js";

const measurements = [
  ["accepted-cancel-step01", "lifecycle", 611, 124408n, 42388566n],
  ["accepted-carriage-certificate", "lifecycle", 1317, 504450n, 223593059n],
  ["accepted-carriage-chunk01", "lifecycle", 15872, 0n, 0n],
  ["accepted-carriage-chunk02", "lifecycle", 15872, 0n, 0n],
  ["accepted-carriage-chunk03", "lifecycle", 2011, 0n, 0n],
  ["accepted-init", "lifecycle", 1497, 679472n, 233177480n],
  [
    "accepted-remove-fraudulent-block",
    "lifecycle",
    2361,
    2994697n,
    1022824771n,
  ],
  ["accepted-step01", "lifecycle", 2017, 1310500n, 445350900n],
  ["accepted-step02", "lifecycle", 1211, 807668n, 299762389n],
  ["accepted-step03-prior-membership", "lifecycle", 2953, 1510406n, 509485430n],
  ["accepted-step04-resume-00", "lifecycle", 9725, 2699978n, 991617202n],
  ["accepted-step04-resume-01", "lifecycle", 9729, 2747755n, 1009519967n],
  ["accepted-step04-resume-02", "lifecycle", 9734, 2711221n, 996983181n],
  ["accepted-step04-resume-03", "lifecycle", 9735, 2645220n, 977493844n],
  ["accepted-step04-resume-04", "lifecycle", 9733, 2649164n, 979426115n],
  ["accepted-step04-resume-05", "lifecycle", 9741, 2792641n, 1023674857n],
  ["accepted-step04-resume-06", "lifecycle", 9741, 2670701n, 988685632n],
  ["accepted-step04-resume-07", "lifecycle", 9741, 2670701n, 988685632n],
  ["accepted-step04-resume-08", "lifecycle", 5451, 2654576n, 979400692n],
  ["accepted-step04-resume-09", "lifecycle", 5451, 2604483n, 960012291n],
  ["accepted-step04-terminal", "lifecycle", 5141, 2379651n, 865574346n],
  ["accepted-step05-proof-mint", "lifecycle", 916, 262271n, 95463347n],
  ["step01-reference-publication", "publication", 14846, 0n, 0n],
  ["step02-reference-publication", "publication", 7720, 0n, 0n],
  ["step03-reference-publication", "publication", 6475, 0n, 0n],
  ["step04-reference-publication", "publication", 15638, 0n, 0n],
  ["step05-reference-publication", "publication", 2207, 0n, 0n],
] as const;

describe("resolvedOutputNonCanonical signed Van Rossem fit ledger", () => {
  it("reproduces positive byte, memory, CPU, and publication-reserve margins", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "resolvedOutputNonCanonical:00000026:testnet",
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
              : "16,384-byte prior-ledger output and Certified maximum input field",
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
          "../../../docs/fault-proofs/size-plans/resolved-output-non-canonical-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    expect(stored).toStrictEqual(ledger);
  });
});
