import { describe, expect, it } from "vitest";

import {
  KIND_OUTSIDE_INPUTS_COORDINATE,
  PRESENT_MAX_INPUT_COUNT,
  PRESENT_MAX_SELECTED_INDEX,
  type PresentMaxClaim,
  type PresentMaxField,
  type PresentMaxRow,
  type PresentMaxShape,
  runPresentMaxJourney,
} from "./support/input-existence-present-max-journey.js";

/** Per-transaction fit limits for every input-existence step. */
const MAX_MEMORY = 13_200_000n;
const MAX_CPU = 8_000_000_000n;
const MAX_SIGNED_BYTES = 16_384;

const marginLine = ({ label, measurement }: PresentMaxRow) =>
  [
    label.padEnd(30),
    `bytes ${measurement.completeSignedBytes.toString().padStart(6)} (margin ${(MAX_SIGNED_BYTES - measurement.completeSignedBytes).toString()})`,
    `mem ${measurement.executionMemory.toString().padStart(9)} (margin ${(MAX_MEMORY - measurement.executionMemory).toString()})`,
    `cpu ${measurement.executionSteps.toString().padStart(13)} (margin ${(MAX_CPU - measurement.executionSteps).toString()})`,
  ].join("  ");

const cases: readonly (readonly [PresentMaxField, PresentMaxShape])[] = [
  [0, "branches"],
  [0, "forks_leaf"],
  [0, "forks_branch"],
  [1, "branches"],
  [1, "forks_leaf"],
  [1, "forks_branch"],
];

const claims: Readonly<Record<PresentMaxClaim, string>> = {
  present: `index ${PRESENT_MAX_SELECTED_INDEX.toString()}`,
  kindOutsideInputs: `source kind and index ${KIND_OUTSIDE_INPUTS_COORDINATE.toString()}`,
};

describe("input-existence forced wrongful-rejection maximum fit", () => {
  it.each(
    (Object.keys(claims) as PresentMaxClaim[]).flatMap((claim) =>
      cases.map(([field, shape]) => [claim, field, shape] as const),
    ),
  )(
    "fits every transaction of the deep 819-input wrongful-rejection proof (%s, field %i, %s)",
    async (claim, field, shape) => {
      const rows = await runPresentMaxJourney({ field, shape, claim });
      console.info(
        [
          `Present maximum (${claim}, field ${field.toString()}, ${shape}, ${PRESENT_MAX_INPUT_COUNT.toString()} inputs, ${claims[claim]}): limits ${MAX_SIGNED_BYTES.toString()} B / ${MAX_MEMORY.toString()} mem / ${MAX_CPU.toString()} cpu`,
          ...rows.map(marginLine),
        ].join("\n"),
      );
      // Every proof step and the removal ran scripts and were measured.
      for (const label of ["step-1", "step-2", "step-3", "step-4", "remove"])
        expect(
          rows.some(
            (row) =>
              row.label === label && row.measurement.executionMemory > 0n,
          ),
          label,
        ).toBe(true);
      for (const { label, measurement } of rows) {
        expect(measurement.completeSignedBytes, label).toBeLessThanOrEqual(
          MAX_SIGNED_BYTES,
        );
        expect(measurement.executionMemory, label).toBeLessThanOrEqual(
          MAX_MEMORY,
        );
        expect(measurement.executionSteps, label).toBeLessThanOrEqual(MAX_CPU);
      }
    },
    300_000,
  );
});
