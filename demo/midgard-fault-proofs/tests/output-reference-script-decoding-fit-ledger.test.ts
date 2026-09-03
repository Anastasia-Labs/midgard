import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

import { buildVanRossemFitLedger } from "../src/proof-fit/van-rossem-fit-ledger-v1.js";

const publications = [
  ["step01-reference-publication", 14_800],
  ["step02-reference-publication", 7_417],
  ["step03-reference-publication", 11_523],
  ["step04-reference-publication", 12_836],
  ["step05-reference-publication", 11_710],
  ["step06-reference-publication", 2_930],
] as const;
const lifecycle = [
  ["init", 1_641, 766_971n, 260_858_482n],
  ["step01", 1_982, 1_306_769n, 443_953_471n],
  ["step02-certified", 1_309, 827_390n, 359_404_318n],
  ["step03-window-0", 9_344, 1_151_489n, 452_670_598n],
  ["step03-window-1", 9_348, 1_233_028n, 481_889_442n],
  ["step03-window-2", 9_354, 1_286_328n, 500_019_533n],
  ["step03-window-3", 5_132, 1_148_251n, 455_387_354n],
  ["step03-window-4", 5_132, 1_151_119n, 456_279_601n],
  ["step03-window-5", 5_132, 1_153_987n, 457_171_848n],
  ["step03-window-6", 5_117, 1_160_799n, 459_988_009n],
  ["step03-window-7", 5_117, 980_852n, 392_485_829n],
  ["step04-certified-reference-bind", 1_221, 1_827_632n, 772_234_699n],
  ["step05-native-scan", 9_483, 1_386_080n, 548_178_321n],
  ["step06-final-mint", 916, 359_428n, 144_748_250n],
  ["cancel-step01", 611, 125_208n, 42_516_566n],
  ["cancel-step02", 611, 112_408n, 40_468_566n],
  ["cancel-step03", 611, 111_676n, 40_336_424n],
  ["cancel-step04", 611, 112_908n, 40_548_566n],
  ["cancel-step05", 611, 111_876n, 40_368_424n],
  ["leased-removal", 2_060, 3_084_377n, 1_052_787_868n],
  ["forced-init", 1_641, 757_767n, 258_085_741n],
  ["forced-step01", 1_719, 717_851n, 317_942_948n],
  ["forced-step02", 1_224, 521_526n, 166_704_299n],
  ["forced-step03-window-0", 921, 1_120_635n, 433_983_288n],
  ["forced-step03-window-1", 925, 1_202_174n, 463_203_146n],
  ["forced-step03-window-2", 929, 1_244_554n, 478_034_251n],
  ["forced-step03-window-3", 928, 1_137_477n, 448_215_454n],
  ["forced-step03-window-4", 928, 966_134n, 387_576_872n],
  ["forced-step04", 1_238, 1_224_189n, 466_584_146n],
  ["forced-step05", 926, 1_456_227n, 565_249_422n],
  ["forced-step06", 916, 346_936n, 142_393_923n],
] as const;

describe("outputReferenceScriptDecoding signed Van Rossem fit ledger", () => {
  it("reproduces positive signed-publication margins", async () => {
    const ledger = buildVanRossemFitLedger({
      category: "outputReferenceScriptDecoding:0000002a:testnet",
      blueprintSha256:
        "b954780865e982a00285432059aeabb0ad05f4e48ff1a91a20c2eac041971b78",
      compilerVersion: "aiken v1.1.23+5adf783",
      measurements: [
        ...publications.map(([name, signedBytes]) => ({
          name,
          kind: "publication" as const,
          maximumShape:
            "fully applied testnet validator signed reference-script publication",
          signedBytes,
          memoryUnits: 0n,
          cpuUnits: 0n,
        })),
        ...lifecycle.map(([name, signedBytes, memoryUnits, cpuUnits]) => ({
          name,
          kind: "lifecycle" as const,
          maximumShape:
            "maximum 16300-byte accepted output with Certified field-2 carriage, resumable descriptor/native scans, cancellation, mint, and leased removal",
          signedBytes,
          memoryUnits,
          cpuUnits,
        })),
      ],
    });
    expect(
      ledger.entries.every(
        (entry) =>
          entry.signedByteMargin > 0 &&
          (entry.kind === "lifecycle" ||
            (entry.publicationReserveMargin ?? -1) >= 0),
      ),
    ).toBe(true);
    const stored: unknown = JSON.parse(
      await readFile(
        new URL(
          "../../../docs/fault-proofs/size-plans/output-reference-script-decoding-v1-fit-ledger.json",
          import.meta.url,
        ),
        "utf8",
      ),
    );
    expect(stored).toStrictEqual(ledger);
  });
});
