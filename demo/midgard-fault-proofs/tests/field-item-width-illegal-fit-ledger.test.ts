import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger.js";

const measurements = [
  [
    "accepted-cancel-step01",
    "lifecycle",
    "maximum certified output field",
    611,
    124408n,
    42388566n,
  ],
  [
    "accepted-cancel-step02",
    "lifecycle",
    "maximum certified output field",
    611,
    112408n,
    40468566n,
  ],
  [
    "accepted-cancel-step03",
    "lifecycle",
    "maximum certified output field",
    611,
    112076n,
    40400424n,
  ],
  [
    "accepted-init",
    "lifecycle",
    "maximum certified output field",
    1497,
    685406n,
    234892766n,
  ],
  [
    "accepted-carriage-chunk01",
    "lifecycle",
    "maximum certified output field",
    15872,
    0n,
    0n,
  ],
  [
    "accepted-carriage-chunk02",
    "lifecycle",
    "maximum certified output field",
    2106,
    0n,
    0n,
  ],
  [
    "accepted-carriage-certificate",
    "lifecycle",
    "maximum certified output field",
    1246,
    456130n,
    177483230n,
  ],
  [
    "accepted-step01",
    "lifecycle",
    "maximum certified output field",
    1983,
    1316200n,
    446873510n,
  ],
  [
    "accepted-step02",
    "lifecycle",
    "maximum certified output field",
    1107,
    792158n,
    310918682n,
  ],
  [
    "accepted-step03-proof-mint",
    "lifecycle",
    "maximum certified output field",
    916,
    274224n,
    99008550n,
  ],
  [
    "accepted-remove-fraudulent-block",
    "lifecycle",
    "maximum certified output field",
    2060,
    3041521n,
    1034725900n,
  ],
  [
    "forced-step01",
    "lifecycle",
    "forced output field adjacent legal item",
    1722,
    703009n,
    322578516n,
  ],
  [
    "forced-step02",
    "lifecycle",
    "forced output field adjacent legal item",
    1076,
    505918n,
    158524713n,
  ],
  [
    "forced-step03-proof-mint",
    "lifecycle",
    "forced output field adjacent legal item",
    916,
    300935n,
    108009473n,
  ],
  [
    "forced-remove-fraudulent-block",
    "lifecycle",
    "forced output field adjacent legal item",
    2060,
    2998981n,
    1020164720n,
  ],
  [
    "step01-reference-publication",
    "publication",
    "fully applied testnet validator",
    14810,
    0n,
    0n,
  ],
  [
    "step02-reference-publication",
    "publication",
    "fully applied testnet validator",
    7221,
    0n,
    0n,
  ],
  [
    "step03-reference-publication",
    "publication",
    "fully applied testnet validator",
    2321,
    0n,
    0n,
  ],
] as const;

describe("field-item-width-illegal signed Van Rossem fit ledger", () => {
  it("retains positive signed-byte, memory, and CPU margins", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "fieldItemWidthIllegal:00000021:testnet",
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
    expect(ledger.ledgerSha256).toBe(
      "d70d8b0abc0c19961645fb257a20e15b41acb6dda49e25196e16117166b24431",
    );
    const stored: unknown = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/field-item-width-illegal-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    expect(stored).toStrictEqual(ledger);
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(JSON.stringify(ledger, null, 2));
  });
});
