import { expect, it } from "vitest";

import { journeyStartupProgress } from "./process.js";

const event = {
  packageName: "midgard-watcher",
  command: "start",
  state: "starting",
  productionReady: false,
  stage: "header_classification",
  outcome: "pending",
  elapsedMs: 30_000,
  observedAt: "2026-09-11T01:00:00.000Z",
};

it("preserves actual pending and failed startup events without claiming readiness", () => {
  expect(journeyStartupProgress(JSON.stringify(event))).toMatchObject({
    stage: "header_classification",
    outcome: "pending",
    elapsedMs: 30_000,
    productionReady: false,
  });
  expect(
    journeyStartupProgress(
      JSON.stringify({
        ...event,
        outcome: "failed",
        error: "public DA unavailable",
      }),
    ),
  ).toMatchObject({
    outcome: "failed",
    error: "public DA unavailable",
    productionReady: false,
  });
});

it("ignores unrelated, malformed and ready-state lines", () => {
  for (const line of [
    "plain stderr",
    "null",
    JSON.stringify({ ...event, productionReady: true }),
    JSON.stringify({ ...event, elapsedMs: -1 }),
    JSON.stringify({ ...event, outcome: "healthy" }),
  ])
    expect(journeyStartupProgress(line)).toBeUndefined();
});
