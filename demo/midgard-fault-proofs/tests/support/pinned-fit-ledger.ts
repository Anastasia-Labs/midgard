import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";

import { expect } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
  writeVanRossemFitLedger,
} from "../../src/proof-fit/van-rossem-fit-ledger.js";

/**
 * The blueprint digest and the compiler label (`aiken <version>`) recorded in
 * the blueprint's own preamble, so a pinned ledger names the build it was
 * measured against rather than a hand-maintained compiler literal.
 */
export const readBlueprintIdentity = async (
  blueprintPath: string,
): Promise<{ blueprintSha256: string; compilerVersion: string }> => {
  const bytes = await readFile(blueprintPath);
  const blueprint = JSON.parse(bytes.toString("utf8")) as {
    readonly preamble: { readonly compiler: { readonly version: string } };
  };
  return {
    blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
    compilerVersion: `aiken ${blueprint.preamble.compiler.version}`,
  };
};

/**
 * A fit ledger that a test holds to the current build. Under
 * `MIDGARD_WRITE_FIT_LEDGER=1` the fresh ledger replaces the checked-in one.
 * Otherwise the checked-in ledger must carry the fresh run's category,
 * blueprint and compiler identity and a digest that matches its body, and
 * list the same rows. Execution budgets vary between runs of one blueprint,
 * so the row values themselves are not compared.
 */
export const writeOrVerifyPinnedFitLedger = async (
  ledgerPath: string,
  ledger: VanRossemFitLedger,
): Promise<void> => {
  if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
    await writeVanRossemFitLedger(ledgerPath, ledger);
    return;
  }
  const stale = `${ledgerPath} does not match this build; regenerate it with MIDGARD_WRITE_FIT_LEDGER=1`;
  const pinned = JSON.parse(
    await readFile(ledgerPath, "utf8"),
  ) as VanRossemFitLedger;
  expect(pinned, stale).toEqual(
    buildVanRossemFitLedger({
      category: ledger.category,
      blueprintSha256: ledger.blueprintSha256,
      compilerVersion: ledger.compilerVersion,
      measurements: pinned.entries.map((entry) => ({
        ...entry,
        memoryUnits: BigInt(entry.memoryUnits),
        cpuUnits: BigInt(entry.cpuUnits),
      })),
    }),
  );
  const roster = (candidate: VanRossemFitLedger) =>
    candidate.entries.map(({ name, kind, maximumShape }) => ({
      name,
      kind,
      maximumShape,
    }));
  expect(roster(ledger), stale).toEqual(roster(pinned));
};
