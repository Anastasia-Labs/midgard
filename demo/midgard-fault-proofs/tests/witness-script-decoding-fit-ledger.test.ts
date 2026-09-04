import { readFile, writeFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger.js";

const maximumShape =
  "32,768-byte Certified field 6; nine authenticated bounded-item chunks";
const measurements = [
  ["accepted-cancel-step01", "lifecycle", 611, 124408n, 42388566n],
  ["accepted-cancel-step02", "lifecycle", 611, 112608n, 40500566n],
  ["accepted-cancel-step03", "lifecycle", 611, 111876n, 40368424n],
  ["accepted-cancel-step04", "lifecycle", 611, 111876n, 40368424n],
  ["accepted-init", "lifecycle", 1497, 685406n, 234892766n],
  ["accepted-carriage-publication", "lifecycle", 1320, 523992n, 230923714n],
  [
    "accepted-carriage-certificate-and-step02",
    "lifecycle",
    1400,
    2254485n,
    923902969n,
  ],
  ["accepted-step01", "lifecycle", 2022, 1325835n, 450361020n],
  ["accepted-step03-resume", "lifecycle", 9706, 2592603n, 1022630443n],
  ["accepted-step03-close", "lifecycle", 9732, 1581604n, 616161225n],
  ["accepted-step04-proof-mint", "lifecycle", 916, 359083n, 143656913n],
  [
    "accepted-remove-fraudulent-block",
    "lifecycle",
    2060,
    3125045n,
    1066829534n,
  ],
  ["forced-init", "lifecycle", 1497, 685406n, 234892766n],
  ["forced-step01", "lifecycle", 1756, 1040278n, 432389761n],
  ["forced-step02", "lifecycle", 1339, 1000921n, 370675288n],
  ["forced-step03", "lifecycle", 973, 2139937n, 827644018n],
  ["forced-step04-proof-mint", "lifecycle", 916, 348823n, 141264833n],
  ["forced-remove-fraudulent-block", "lifecycle", 2060, 3063230n, 1046807425n],
  ["step01-reference-publication", "publication", 14922, 0n, 0n],
  ["step02-reference-publication", "publication", 10542, 0n, 0n],
  ["step03-reference-publication", "publication", 11693, 0n, 0n],
  ["step04-reference-publication", "publication", 2932, 0n, 0n],
] as const;

describe("witnessScriptDecoding signed Van Rossem fit ledger", () => {
  it("reproduces positive size, execution, and publication-reserve margins", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "witnessScriptDecoding:00000022:testnet",
      blueprintSha256:
        "5cc9a404fac1172e5569ec3f0ea59b031132ee706169280166edbc49ad4a647c",
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: measurements.map(
        ([name, kind, signedBytes, memoryUnits, cpuUnits]) => ({
          name,
          kind,
          maximumShape:
            kind === "publication"
              ? "fully applied testnet validator"
              : maximumShape,
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
    const ledgerUrl = new URL(
      "../../../docs/fault-proofs/size-plans/witness-script-decoding-v1-fit-ledger.json",
      import.meta.url,
    );
    if (process.env.MIDGARD_UPDATE_FIT === "1")
      await writeFile(
        ledgerUrl,
        `${JSON.stringify(ledger, null, 2)}\n`,
        "utf8",
      );
    const stored: unknown = JSON.parse(await readFile(ledgerUrl, "utf8"));
    expect(stored).toStrictEqual(ledger);
  });
});
