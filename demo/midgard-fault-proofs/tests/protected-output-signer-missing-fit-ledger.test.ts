import { readFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  buildVanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../src/proof-fit/van-rossem-fit-ledger.js";

/**
 * The stored ledger is regenerated from this table: set
 * `MIDGARD_WRITE_FIT_LEDGER=1` after transcribing a fresh lifecycle run
 * (`MIDGARD_PRINT_FIT=1`) and the test rewrites the artifact it then checks.
 */
const LEDGER_URL = new URL(
  "../../../docs/fault-proofs/size-plans/protected-output-signer-missing-v1-fit-ledger.json",
  import.meta.url,
);

const lifecycle = "318 address witnesses; three-chunk Certified field 7";
/**
 * The forced door binds the header, the counted forced-root membership and
 * the leaf's proof source; its cost does not grow with the witness field, so
 * the small forced lifecycle is its maximum shape.
 */
const forcedBinding =
  "forced leaf under the header's counted forced root; exact ProtectedOutputSignerMissing reason";
const measurements = [
  ["accepted-cancel-step01", "lifecycle", 611, 124408n, 42388566n],
  ["accepted-carriage-certificate", "lifecycle", 1317, 514726n, 228139170n],
  ["accepted-init", "lifecycle", 1641, 764105n, 260067443n],
  [
    "accepted-remove-fraudulent-block",
    "lifecycle",
    2361,
    2986041n,
    1020151393n,
  ],
  ["accepted-step01", "lifecycle", 2020, 1315166n, 447054806n],
  ["accepted-step02", "lifecycle", 1135, 582522n, 181217778n],
  ["accepted-step03-witness-open", "lifecycle", 1383, 727967n, 240985721n],
  ["accepted-step04-resume-00", "lifecycle", 1438, 7911222n, 4009503955n],
  ["accepted-step04-resume-01", "lifecycle", 1438, 7911222n, 4009503955n],
  ["accepted-step04-resume-02", "lifecycle", 1438, 7911222n, 4009503955n],
  ["accepted-step04-resume-03", "lifecycle", 1438, 7911222n, 4009503955n],
  ["accepted-step04-resume-04", "lifecycle", 1438, 8157089n, 4101478888n],
  ["accepted-step04-resume-05", "lifecycle", 1438, 8461878n, 4180815379n],
  ["accepted-step04-resume-06", "lifecycle", 1438, 8461878n, 4180815379n],
  ["accepted-step04-resume-07", "lifecycle", 1438, 8461878n, 4180815379n],
  ["accepted-step04-resume-08", "lifecycle", 1438, 8461878n, 4180815379n],
  ["accepted-step04-terminal", "lifecycle", 1302, 8380053n, 3138795307n],
  ["accepted-step05-proof-mint", "lifecycle", 916, 262772n, 95619396n],
  ["forced-step01", "lifecycle", 1757, 1058186n, 443761399n],
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
        "137e608cbbcccffb002b4ed638eb5cc4bdda212de8b40f18313d414f8803c461",
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: measurements.map(
        ([name, kind, signedBytes, memoryUnits, cpuUnits]) => ({
          name,
          kind,
          maximumShape:
            kind === "publication"
              ? "fully applied validator or exact 15,148-byte carriage chunk"
              : name === "forced-step01"
                ? forcedBinding
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
    if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1")
      await writeVanRossemFitLedger(fileURLToPath(LEDGER_URL), ledger);
    const stored: unknown = JSON.parse(await readFile(LEDGER_URL, "utf8"));
    expect(stored).toStrictEqual(ledger);
  });
});
