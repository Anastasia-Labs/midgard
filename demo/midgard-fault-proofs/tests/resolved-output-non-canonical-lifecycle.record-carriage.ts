import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import { expect } from "vitest";

import {
  buildVanRossemFitLedger,
  type VanRossemFitMeasurement,
} from "../src/proof-fit/van-rossem-fit-ledger.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import type { CompleteSignedTransactionMeasurement } from "./support/emulator/measurement.js";
import { createLifecycleCoverageRecorder } from "./support/lifecycle-coverage.js";
import {
  MAXIMUM_INPUT_ITEM_COUNT,
  RESOLVED_OUTPUT_MAXIMUM_BYTES,
} from "./support/resolved-output-non-canonical-emulator.js";
import {
  createSplitFitLedgerPart,
  type SplitFitLedger,
} from "./support/split-fit-ledger.js";

/** The envelope suite's seams; the datum suite's are {@link DATUM_SEAMS}. */

export const AUTHENTICATION_SEAMS = [
  "tx_membership",
  "prior_root",
  "prior_output_descriptor",
  "prior_output_membership",
  "field_certificate",
  "forced_leaf_reason_coordinate",
  "forced_leaf_header",
  "forced_leaf_root",
  "forced_leaf_direction",
  "output_chunk",
  "scan_checkpoint",
  "wrong_successor",
  "finishable_advance",
  "premature_finalize",
] as const;

export const DATUM_SEAMS = [
  "datum_chunk",
  "datum_checkpoint",
  "datum_stack",
] as const;

/** The envelope suite's cancels; the datum suite cancels `step-04-datum`. */

export const CANCELLABLE_STEPS = [
  "step-01",
  "step-02",
  "step-03",
  "step-04",
  "step-05",
] as const;

/** The registered steps' publication rows, in chain order. */
const PUBLICATION_NAMES = [
  "publish-step01",
  "publish-step02",
  "publish-step03",
  "publish-step04",
  "publish-step04-datum",
  "publish-step05",
] as const;

const MAXIMUM_SHAPE = `${RESOLVED_OUTPUT_MAXIMUM_BYTES.toLocaleString("en-US")}-byte prior-ledger output at adversarial membership depth and a Certified ${MAXIMUM_INPUT_ITEM_COUNT.toString()}-item input field`;

/**
 * The cases that measure rows, in ledger order: the two maximum-shape
 * lifecycles of the envelope suite and the densest-datum read of the datum
 * suite. The suites' other cases measure nothing.
 */
export const MEASURED_CASES = {
  acceptedMaximum: 0,
  forcedMaximum: 1,
  densestDatum: 2,
} as const;

const FIT_LEDGER: SplitFitLedger = {
  path: fileURLToPath(
    new URL(
      "../../../docs/fault-proofs/size-plans/resolved-output-non-canonical-v1-fit-ledger.json",
      import.meta.url,
    ),
  ),
  category: "resolvedOutputNonCanonical:00000026:testnet",
  caseCount: Object.keys(MEASURED_CASES).length,
  parts: ["lifecycle", "datum"],
  source: fileURLToPath(import.meta.url),
  // Every row name is unique already.
  rowNames: "stem",
};

export const coverage = createLifecycleCoverageRecorder();

/** This file's rows, checked for margin at its teardown. */
const measurements: VanRossemFitMeasurement[] = [];

let publicationsRecorded = false;

let fit: ReturnType<typeof createSplitFitLedgerPart> | undefined;

let measuredCase: number | undefined;

/** Registers the calling test file as one part of the fit ledger. */
export const measureFitPart = (
  part: "lifecycle" | "datum",
  ordinals: readonly number[],
) => {
  fit = createSplitFitLedgerPart(FIT_LEDGER, part, ordinals);
};

/** Starts a measured case; every row until it passes belongs to it. */
export const beginMeasuredCase = (ordinal: number) => {
  measuredCase = ordinal;
  fit?.begin(ordinal);
};

/** Marks the measured case passed; the last statement of the case. */
export const passMeasuredCase = () => {
  if (measuredCase !== undefined) fit?.pass(measuredCase);
  measuredCase = undefined;
};

const push = (measurement: VanRossemFitMeasurement) => {
  if (fit === undefined || measuredCase === undefined)
    throw new Error(`${measurement.name} is measured outside a measured case`);
  measurements.push(measurement);
  const { name, ...row } = measurement;
  fit.record(measuredCase, { stem: name, ...row });
};

const expectFit = (
  name: string,
  measurement: CompleteSignedTransactionMeasurement,
  runsScripts: boolean,
) => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
  if (!runsScripts) return;
  expect(measurement.executionMemory, name).toBeGreaterThan(0n);
  expect(measurement.executionSteps, name).toBeGreaterThan(0n);
};

export const record = (
  name: string,
  measurement: CompleteSignedTransactionMeasurement,
  { runsScripts = true, maximumShape = MAXIMUM_SHAPE } = {},
) => {
  expectFit(name, measurement, runsScripts);
  push({
    name,
    kind: "lifecycle",
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};

export const recordPublication = (
  stepIndex: number,
  measurement: CompleteSignedTransactionMeasurement,
) => {
  const name = PUBLICATION_NAMES[stepIndex];
  if (name === undefined)
    throw new Error(`no step at index ${stepIndex.toString()}`);
  expect(measurement.completeSignedBytes, name).toBeLessThanOrEqual(15_872);
  if (publicationsRecorded) return;
  push({
    name,
    kind: "publication",
    maximumShape: "fully applied testnet validator",
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
  if (stepIndex === PUBLICATION_NAMES.length - 1) publicationsRecorded = true;
};

/** Step 02 submits its carriage chunks and certificate before the step itself. */
export const recordCarriage = (
  prefix: string,
  captured: {
    readonly measurements: readonly CompleteSignedTransactionMeasurement[];
  },
) => {
  const auxiliary = captured.measurements.slice(0, -1);
  expect(auxiliary.length).toBeGreaterThanOrEqual(2);
  auxiliary.forEach((measurement, index) => {
    const last = index === auxiliary.length - 1;
    // Chunk publications carry bytes only; the certificate runs its mint.
    record(
      last
        ? `${prefix}-carriage-certificate`
        : `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
      measurement,
      { runsScripts: last },
    );
  });
};

/** Every row this file measured fits with a margin; the rows are logged. */
export const expectMeasuredFit = () => {
  const ledger = buildVanRossemFitLedger({
    category: FIT_LEDGER.category,
    blueprintSha256: createHash("sha256")
      .update(readFileSync(realBlueprintPath))
      .digest("hex"),
    measurements,
  });
  for (const entry of ledger.entries) {
    expect(entry.signedByteMargin, entry.name).toBeGreaterThan(0);
    expect(BigInt(entry.memoryUnitMargin), entry.name).toBeGreaterThan(0n);
    expect(BigInt(entry.cpuUnitMargin), entry.name).toBeGreaterThan(0n);
    if (entry.kind === "publication")
      expect(entry.publicationReserveMargin, entry.name).toBeGreaterThanOrEqual(
        0,
      );
  }
  console.info(
    `[resolved-output-non-canonical-fit-ledger] ${JSON.stringify(ledger.entries)}`,
  );
};

export const progress = (message: string) =>
  console.info(`[resolved-output-non-canonical-progress] ${message}`);
