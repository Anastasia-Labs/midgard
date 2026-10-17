import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { afterAll, expect } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
  writeVanRossemFitLedger,
} from "../../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./emulator/blueprints.js";
import type { CompleteSignedTransactionMeasurement } from "./emulator/measurement.js";

const blueprintIdentity = async () => {
  const bytes = await readFile(realBlueprintPath);
  return {
    blueprintSha256: createHash("sha256").update(bytes).digest("hex"),
  };
};

const fragmentPath = (family: string, source: string): string => {
  const directory = process.env.MIDGARD_FIT_FRAGMENT_DIR;
  const run = process.env.MIDGARD_FIT_MEASUREMENT_RUN;
  if (!directory || !run || !/^[a-zA-Z0-9_-]+$/u.test(run))
    throw new Error(
      "Measured fit fragments are written only inside a regeneration run: use `pnpm --dir demo/midgard-fault-proofs fit:regenerate <ledger>`, which sets MIDGARD_FIT_FRAGMENT_DIR and a fresh MIDGARD_FIT_MEASUREMENT_RUN for that run",
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
