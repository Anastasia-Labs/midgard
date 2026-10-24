/**
 * The pooled DA bond journey (ticket #692) run end to end over the emulator
 * adapter: all six spec steps, on the compiled validators, with the real
 * commit, Apply and `da-bond` operator commands. A fast dry run of what the
 * live devnet adapter proves; its report says so in its header.
 *
 * Set `MIDGARD_DA_BOND_JOURNEY_REPORT_PATH` to write the rendered report.
 */
import { writeFileSync } from "node:fs";

import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  createDaBondPoolEmulatorPort,
  type DaBondPoolEmulatorPort,
} from "./da-bond-pool-emulator-port.js";
import {
  DA_BOND_POOL_JOURNEY_CHRONOLOGY,
  type DaBondPoolJourneyRecord,
  renderDaBondPoolJourneyReport,
  tryRunDaBondPoolJourney,
} from "./da-bond-pool-journey.js";

const TIMEOUT_MS = 1_800_000;

describe("pooled DA bond journey on the emulator", () => {
  let port: DaBondPoolEmulatorPort | undefined;
  let record: DaBondPoolJourneyRecord;
  let error: unknown;

  beforeAll(async () => {
    port = await createDaBondPoolEmulatorPort();
    ({ record, error } = await tryRunDaBondPoolJourney(port));
    const reportPath = process.env.MIDGARD_DA_BOND_JOURNEY_REPORT_PATH;
    if (reportPath !== undefined && reportPath !== "")
      writeFileSync(
        reportPath,
        renderDaBondPoolJourneyReport(record, { adapter: "emulator" }),
      );
  }, TIMEOUT_MS);

  afterAll(() => port?.dispose());

  it("passes every step in the driver's chronology", () => {
    expect(
      error === undefined ? undefined : String(error),
      record.failure?.message,
    ).toBeUndefined();
    expect(record.status).toBe("passed");
    expect(record.stages.map((stage) => stage.step)).toEqual([
      ...DA_BOND_POOL_JOURNEY_CHRONOLOGY,
    ]);
    for (const stage of record.stages)
      expect(stage.status, `step ${stage.step}`).toBe("passed");
  });

  it("observes every assertion, and none fails", () => {
    // The emulator composes the committee view and the da-bond commands in
    // process, so only the P16/P18/P27 process evidence may be
    // not-observable here; the live devnet run requires it.
    const processOnly = (name: string) =>
      name.startsWith("P16: ") ||
      name.startsWith("P18: ") ||
      name.startsWith("P27: ");
    const unmet = record.stages.flatMap((stage) =>
      stage.assertions
        .filter(
          (assertion) =>
            assertion.ok === false ||
            (assertion.ok === "not-observable" && !processOnly(assertion.name)),
        )
        .map(
          (assertion) =>
            `step ${stage.step}: ${assertion.name} (${String(assertion.ok)}): ${assertion.detail}`,
        ),
    );
    expect(unmet).toEqual([]);
    expect(record.stages.every((stage) => stage.assertions.length > 0)).toBe(
      true,
    );
    // An emulator run never counts as P16/P18/P27 process evidence.
    const processRows = record.stages.flatMap((stage) =>
      stage.assertions.filter((assertion) => processOnly(assertion.name)),
    );
    expect(processRows.length).toBeGreaterThan(0);
    expect(
      processRows.every((assertion) => assertion.ok === "not-observable"),
    ).toBe(true);
  });

  it("refuses the short and the withdrawing Apply with the builder's reason", () => {
    const refusals = record.stages.flatMap((stage) =>
      stage.assertions
        .filter((assertion) => assertion.name.includes("is refused with"))
        .map((assertion) => `${stage.step}: ${assertion.detail}`),
    );
    expect(refusals).toHaveLength(2);
    expect(refusals[0]).toMatch(/^4: refused: pool-under-backed: /u);
    expect(refusals[1]).toMatch(/^6: refused: pool-withdrawing: /u);
  });
});
