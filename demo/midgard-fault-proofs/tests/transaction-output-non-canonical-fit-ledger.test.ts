import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";

// Measured by `transaction-output-non-canonical-lifecycle.test.ts`
// (`MIDGARD_FIT_OUT`) and `transaction-output-non-canonical-publication-fit.test.ts`
// (`MIDGARD_PRINT_FIT=1`) on the blueprint pinned below.
const MAXIMUM_SHAPE = {
  malformed:
    "16,384-byte malformed selected output in a 32,768-byte Certified field, rejected at the first window",
  canonical:
    "16,384-byte canonical selected output scanned to its exact terminal through 8,190-byte windows",
  publication: "fully applied testnet validator",
} as const;

const measurements = [
  ["accepted-carriage-chunk01", "malformed", 15872, 0n, 0n],
  ["accepted-carriage-chunk02", "malformed", 15872, 0n, 0n],
  ["accepted-carriage-chunk03", "malformed", 2800, 0n, 0n],
  ["accepted-carriage-certificate", "malformed", 1317, 507113n, 226017224n],
  ["accepted-init", "malformed", 1497, 667200n, 229480492n],
  ["accepted-step01", "malformed", 2071, 1383537n, 468784837n],
  ["accepted-step02", "malformed", 1376, 881806n, 391128577n],
  ["accepted-step03-scan", "malformed", 9343, 637317n, 257251189n],
  ["accepted-step04-proof-mint", "malformed", 916, 520610n, 196459439n],
  ["accepted-cancel-step01", "malformed", 611, 124408n, 42388566n],
  ["accepted-cancel-step02", "malformed", 611, 112408n, 40468566n],
  ["accepted-cancel-step03", "malformed", 611, 111676n, 40336424n],
  ["accepted-cancel-step04", "malformed", 611, 112876n, 40528424n],
  ["accepted-canonical-step02", "canonical", 1339, 839302n, 363399762n],
  [
    "accepted-canonical-step03-scan-first",
    "canonical",
    9374,
    1179406n,
    463900924n,
  ],
  [
    "accepted-canonical-step03-scan-final",
    "canonical",
    939,
    956030n,
    380040844n,
  ],
  [
    "accepted-remove-fraudulent-block",
    "malformed",
    2060,
    3016412n,
    1024573028n,
  ],
  ["forced-init", "canonical", 1497, 682540n, 234101727n],
  ["forced-step01", "canonical", 1721, 723521n, 325031336n],
  ["forced-step02-carriage-chunk01", "canonical", 15872, 0n, 0n],
  ["forced-step02-carriage-chunk02", "canonical", 1530, 0n, 0n],
  [
    "forced-step02-carriage-certificate",
    "canonical",
    1246,
    456060n,
    176301280n,
  ],
  ["forced-step02", "canonical", 1384, 842170n, 364646636n],
  ["forced-step03-scan-first", "canonical", 9419, 1184045n, 464893907n],
  ["forced-step03-scan-final", "canonical", 984, 960669n, 381033827n],
  ["forced-step04-proof-mint", "canonical", 916, 897184n, 345508520n],
  ["forced-remove-fraudulent-block", "canonical", 2060, 3074784n, 1050226065n],
  ["step01-reference-publication", "publication", 14698, 0n, 0n],
  ["step02-reference-publication", "publication", 7422, 0n, 0n],
  ["step03-reference-publication", "publication", 11834, 0n, 0n],
  ["step04-reference-publication", "publication", 5177, 0n, 0n],
] as const;

const ledgerPath = fileURLToPath(
  new URL(
    "../../../docs/fault-proofs/size-plans/transaction-output-non-canonical-v1-fit-ledger.json",
    import.meta.url,
  ),
);

describe("transactionOutputNonCanonical signed Van Rossem fit ledger", () => {
  it("reproduces positive byte, memory, CPU, and publication-reserve margins at both maximum shapes", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "transactionOutputNonCanonical:00000029:testnet",
      blueprintSha256:
        "6236b3f8d2bbf663d184018e3281934370745ecdf078edf4cbaa34e06a4f2a6b",
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: measurements.map(
        ([name, shape, signedBytes, memoryUnits, cpuUnits]) => ({
          name,
          kind: shape === "publication" ? "publication" : "lifecycle",
          maximumShape: MAXIMUM_SHAPE[shape],
          signedBytes,
          memoryUnits,
          cpuUnits,
        }),
      ),
    });
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1")
      await writeVanRossemFitLedger(ledgerPath, ledger);
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
    const stored: unknown = JSON.parse(await readFile(ledgerPath, "utf8"));
    expect(stored).toStrictEqual(ledger);
  });
});
