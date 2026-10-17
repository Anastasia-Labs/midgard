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
import {
  createSplitFitLedgerPart,
  type SplitFitLedger,
} from "./support/split-fit-ledger.js";
import {
  type StepIndex,
  stepLabel,
} from "./transaction-output-non-canonical-lifecycle.registered-contracts.js";

/**
 * The fit ledger's writer: the lifecycle and datum suites each record one
 * part of it. Kept apart from the registered contracts so that suites which
 * only drive those contracts do not own the ledger.
 */

/** The maximum-shape lifecycles' shapes; the datum suite names its own. */
const MALFORMED_MAXIMUM_SHAPE =
  "16,384-byte malformed selected output in a 32,768-byte Certified field, rejected at the first window";
const CANONICAL_SCAN_SHAPE =
  "16,384-byte canonical selected output scanned to its exact terminal through 8,190-byte windows";
/** The canonical output's scan rows and, past its envelope, datum rows. */
export const CANONICAL_SHAPES = {
  scan: CANONICAL_SCAN_SHAPE,
  datum:
    "16,384-byte canonical selected output whose inline datum is read to its verdict",
} as const;

/**
 * The cases that measure rows, in ledger order: the two maximum-shape
 * lifecycles of the envelope suite and the four verdicts of the datum suite.
 * The suites' other cases measure nothing.
 */
export const MEASURED_CASES = {
  acceptedMaximum: 0,
  forcedMaximum: 1,
  honestDatum: 2,
  falseRejectionDatum: 3,
  acceptedDatum: 4,
  densestDatum: 5,
} as const;

const FIT_LEDGER: SplitFitLedger = {
  path: fileURLToPath(
    new URL(
      "../../../docs/fault-proofs/size-plans/transaction-output-non-canonical-v1-fit-ledger.json",
      import.meta.url,
    ),
  ),
  category: "transactionOutputNonCanonical:00000029:testnet",
  caseCount: Object.keys(MEASURED_CASES).length,
  parts: ["lifecycle", "datum"],
  source: fileURLToPath(import.meta.url),
  // Every row name is unique already.
  rowNames: "stem",
};

/** This file's rows, checked for margin at its end. */
const measurements: VanRossemFitMeasurement[] = [];

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

export const record = (
  name: string,
  measurement: CompleteSignedTransactionMeasurement,
  maximumShape: string,
): void => {
  expect(measurement.l1ByteMargin, name).toBeGreaterThan(0);
  push({
    name,
    kind: "lifecycle",
    maximumShape,
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
  });
};

/** {@link record} with the maximum shape fixed. */
const recordShaped =
  (maximumShape: string) =>
  (name: string, measurement: CompleteSignedTransactionMeasurement): void =>
    record(name, measurement, maximumShape);

/** A row of the malformed maximum-shape lifecycle. */
export const recordMalformed = recordShaped(MALFORMED_MAXIMUM_SHAPE);

/** A row of a canonical maximum-shape lifecycle, before its datum. */
export const recordCanonical = recordShaped(CANONICAL_SCAN_SHAPE);

/** A carriage's chunk publications, then its certificate, named by `prefix`. */
export const recordCarriage = (
  prefix: string,
  submitted: readonly CompleteSignedTransactionMeasurement[],
  recordRow: (
    name: string,
    measurement: CompleteSignedTransactionMeasurement,
  ) => void,
): void =>
  submitted.forEach((measurement, index) =>
    recordRow(
      index === submitted.length - 1
        ? `${prefix}-carriage-certificate`
        : `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
      measurement,
    ),
  );

/** A registered step's reference-script publication, by physical step. */
export const recordPublication = (
  stepIndex: StepIndex,
  measurement: CompleteSignedTransactionMeasurement,
): void => {
  const name = `step${stepLabel(stepIndex)}-reference-publication`;
  expect(measurement.completeSignedBytes, name).toBeLessThanOrEqual(15_872);
  push({
    name,
    kind: "publication",
    maximumShape: "fully applied testnet validator",
    signedBytes: measurement.completeSignedBytes,
    memoryUnits: measurement.executionMemory,
    cpuUnits: measurement.executionSteps,
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
    `[transaction-output-non-canonical-fit-ledger] ${JSON.stringify(ledger.entries)}`,
  );
};

/** Labels the chunk, certificate and step transactions one builder call submitted. */
export const recordAll = (
  prefix: string,
  submitted: readonly CompleteSignedTransactionMeasurement[],
  certified: boolean,
  maximumShape: string,
): void => {
  const stepIndex = submitted.length - 1;
  const certificateIndex = certified ? stepIndex - 1 : -1;
  submitted.forEach((measurement, index) => {
    if (index === stepIndex) record(prefix, measurement, maximumShape);
    else if (index === certificateIndex)
      record(`${prefix}-carriage-certificate`, measurement, maximumShape);
    else
      record(
        `${prefix}-carriage-chunk${(index + 1).toString().padStart(2, "0")}`,
        measurement,
        maximumShape,
      );
  });
};
