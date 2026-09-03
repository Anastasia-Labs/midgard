import { readFile, writeFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger.js";

const lifecycle = [
  ["accepted-init", 1497, 666796n, 229214076n],
  ["accepted-step01", 2051, 1311678n, 445862374n],
  ["accepted-step02", 1774, 1663638n, 547541374n],
  ["accepted-witness-chunk01", 15872, 0n, 0n],
  ["accepted-witness-chunk02", 15872, 0n, 0n],
  ["accepted-witness-chunk03", 2789, 0n, 0n],
  ["accepted-witness-certificate", 1317, 506122n, 225462429n],
  ["accepted-step03", 1379, 724166n, 239555552n],
  ["accepted-step04-00", 1434, 4459642n, 3036736158n],
  ["accepted-step04-01", 1434, 4456774n, 3035843911n],
  ["accepted-step04-02", 1434, 4459642n, 3036736158n],
  ["accepted-step04-03", 1434, 4462510n, 3037628405n],
  ["accepted-step04-04", 1434, 4459642n, 3036736158n],
  ["accepted-step04-05", 1434, 4459642n, 3036736158n],
  ["accepted-step04-06", 1434, 4462510n, 3037628405n],
  ["accepted-step04-07", 1434, 4462510n, 3037628405n],
  ["accepted-step04-08", 1434, 4459642n, 3036736158n],
  ["accepted-step04-09", 1434, 4705509n, 3128711091n],
  ["accepted-step04-10", 1434, 4732102n, 3121499623n],
  ["accepted-step04-11", 1434, 4732102n, 3121499623n],
  ["accepted-step04-12", 1434, 4734970n, 3122391870n],
  ["accepted-step04-13", 1434, 4734970n, 3122391870n],
  ["accepted-step04-14", 1434, 4734970n, 3122391870n],
  ["accepted-step04-15", 1434, 4734970n, 3122391870n],
  ["accepted-step04-16", 1434, 4734970n, 3122391870n],
  ["accepted-step04-17", 1434, 4734970n, 3122391870n],
  ["accepted-step04-18", 1434, 4934949n, 2828973195n],
  ["accepted-step04-19", 1302, 4447378n, 2265353389n],
  ["accepted-step05-proof-mint", 916, 257647n, 94094502n],
  ["accepted-remove", 2361, 3002431n, 1030047027n],
  ["forced-init", 1497, 685204n, 234759558n],
  ["forced-step01", 1787, 896210n, 374909418n],
  ["forced-step02", 1819, 1663638n, 547896001n],
  ["forced-step03", 1386, 506414n, 172189267n],
  ["forced-step04", 1308, 788947n, 328759938n],
  ["forced-step05-proof-mint", 916, 288382n, 104368270n],
  ["forced-remove", 2361, 3048997n, 1045744177n],
  ["cancel-step01", 611, 124908n, 42468566n],
  ["cancel-step02", 611, 112908n, 40548566n],
  ["cancel-step03", 611, 112408n, 40468566n],
  ["cancel-step04-initial", 611, 112408n, 40468566n],
  ["cancel-step04-resumed", 611, 112408n, 40468566n],
  ["cancel-step05", 611, 111876n, 40368424n],
] as const;

const publications = [
  ["step01-reference-publication", 14939],
  ["step02-reference-publication", 12116],
  ["step03-reference-publication", 7466],
  ["step04-reference-publication", 9075],
  ["step05-reference-publication", 2207],
] as const;

describe("spendInputSignerMissing signed Van Rossem fit ledger", () => {
  it("reproduces maximum-frontier lifecycle and publication margins", async () => {
    const maximumShape =
      "318 address witnesses; 32,757-byte Certified field; 16-witness scan batches";
    const ledger = buildVanRossemFitLedger({
      category: "spendInputSignerMissing:00000027:testnet",
      blueprintSha256:
        "61ec67157434a1904ddac0a355337a1656d1ef62448744fa2856d0a1aa1602cb",
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: [
        ...lifecycle.map(([name, signedBytes, memoryUnits, cpuUnits]) => ({
          name,
          kind: "lifecycle" as const,
          maximumShape,
          signedBytes,
          memoryUnits,
          cpuUnits,
        })),
        ...publications.map(([name, signedBytes]) => ({
          name,
          kind: "publication" as const,
          maximumShape: "fully applied testnet validator",
          signedBytes,
          memoryUnits: 0n,
          cpuUnits: 0n,
        })),
      ],
    });
    expect(ledger.entries).toHaveLength(48);
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
    if (process.env.MIDGARD_PRINT_FIT === "1")
      console.info(JSON.stringify(ledger, null, 2));
    const ledgerUrl = new URL(
      "../../../docs/fault-proofs/size-plans/spend-input-signer-missing-v1-fit-ledger.json",
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
