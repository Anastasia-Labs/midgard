import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { afterAll, expect } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./emulator/blueprints.js";
import type { CompleteSignedTransactionMeasurement } from "./emulator/measurement.js";

const blueprintIdentity = async () => {
  const bytes = await readFile(realBlueprintPath);
  const blueprint = JSON.parse(bytes.toString()) as {
    preamble: { compiler: { version: string } };
  };
  return {
    blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
    compilerVersion: blueprint.preamble.compiler.version,
  };
};

const fragmentPath = (family: string, source: string): string => {
  const directory = process.env.MIDGARD_FIT_FRAGMENT_DIR;
  const run = process.env.MIDGARD_FIT_MEASUREMENT_RUN;
  if (!directory || !run || !/^[a-zA-Z0-9_-]+$/u.test(run))
    throw new Error(
      "Measured fit regeneration requires MIDGARD_FIT_FRAGMENT_DIR and a fresh MIDGARD_FIT_MEASUREMENT_RUN token",
    );
  return join(directory, run, family, `${source}.json`);
};

/** Capture evaluated, signed submissions; never populate rows from old ledgers. */
export const createMeasuredFitRecorder = (
  family: string,
  source: string,
  maximumShape: string,
) => {
  const measurements: VanRossemFitMeasurement[] = [];
  const initialIdentity = blueprintIdentity();
  const record = (
    name: string,
    measurement: CompleteSignedTransactionMeasurement,
    kind: VanRossemFitMeasurement["kind"] = "lifecycle",
    shape = maximumShape,
  ): void => {
    measurements.push({
      name: `${source}/${name}`,
      kind,
      maximumShape: shape,
      signedBytes: measurement.completeSignedBytes,
      memoryUnits: measurement.executionMemory,
      cpuUnits: measurement.executionSteps,
    });
  };
  afterAll(async () => {
    if (process.env.MIDGARD_WRITE_FIT_LEDGER !== "1") return;
    const identity = await initialIdentity;
    expect(await blueprintIdentity()).toEqual(identity);
    await writeVanRossemFitLedger(
      fragmentPath(family, source),
      buildVanRossemFitLedger({
        category: family,
        ...identity,
        measurements,
      }),
    );
  });
  return { record };
};

const measurementsOf = (ledger: VanRossemFitLedger) =>
  ledger.entries.map((row) => ({
    ...row,
    memoryUnits: BigInt(row.memoryUnits),
    cpuUnits: BigInt(row.cpuUnits),
  }));

/** Normal checks require checked-in current-build evidence; writing requires every fresh fragment. */
export const verifyMeasuredFitLedger = async ({
  family,
  ledgerName,
  sources,
}: {
  readonly family: string;
  readonly ledgerName: string;
  readonly sources: Readonly<Record<string, readonly string[]>>;
}): Promise<VanRossemFitLedger> => {
  const identity = await blueprintIdentity();
  const path = fileURLToPath(
    new URL(
      `../../../../docs/fault-proofs/size-plans/${ledgerName}`,
      import.meta.url,
    ),
  );
  let ledger: VanRossemFitLedger;
  if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1") {
    const fragments = await Promise.all(
      Object.keys(sources).map(async (source) => {
        const fragment = JSON.parse(
          await readFile(fragmentPath(family, source), "utf8"),
        ) as VanRossemFitLedger;
        expect(fragment.category).toBe(family);
        expect(fragment.blueprintSha256).toBe(identity.blueprintSha256);
        expect(fragment.compilerVersion).toBe(identity.compilerVersion);
        expect(fragment).toEqual(
          buildVanRossemFitLedger({
            category: family,
            ...identity,
            measurements: measurementsOf(fragment),
          }),
        );
        return fragment;
      }),
    );
    ledger = buildVanRossemFitLedger({
      category: family,
      ...identity,
      measurements: fragments.flatMap(measurementsOf),
    });
  } else {
    ledger = JSON.parse(await readFile(path, "utf8")) as VanRossemFitLedger;
  }
  expect(ledger.category).toBe(family);
  expect(ledger.blueprintSha256).toBe(identity.blueprintSha256);
  expect(ledger.compilerVersion).toBe(identity.compilerVersion);
  expect(ledger).toEqual(
    buildVanRossemFitLedger({
      category: family,
      ...identity,
      measurements: measurementsOf(ledger),
    }),
  );
  const expected = Object.entries(sources).flatMap(([source, names]) =>
    names.map((name) => `${source}/${name}`),
  );
  expect(ledger.entries.map(({ name }) => name).sort()).toEqual(
    expected.sort(),
  );
  if (process.env.MIDGARD_WRITE_FIT_LEDGER === "1")
    await writeVanRossemFitLedger(path, ledger);
  return ledger;
};
