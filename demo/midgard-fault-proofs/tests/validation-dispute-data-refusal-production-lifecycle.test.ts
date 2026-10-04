import { expect, it } from "vitest";

import { buildDataRefusalDispute } from "./redeemer-data-refusal-deployed.build-dispute.js";
import { runForcedValidationDisputeScenario } from "./support/submit-init-emulator-shared.js";

it.each(["1801", "580100"])(
  "proves Data refusal %s through production submission, permanent mint and removal",
  async (data) => {
    const result = await runForcedValidationDisputeScenario(
      ({ operatorVkey, now }) =>
        buildDataRefusalDispute(data, operatorVkey, now),
      {
        onSubmittedTransaction: (measurement) => {
          console.log("Production Data refusal submitted", data, measurement);
        },
      },
    );
    expect(result.awardResult?.txHash).toHaveLength(64);
    expect(result.removal?.transactions.length).toBeGreaterThan(0);
    expect(result.semanticMeasurement?.completeSignedBytes).toBeLessThanOrEqual(
      15872,
    );
    expect(result.semanticMeasurement?.executionMemory).toBeLessThanOrEqual(
      13_200_000n,
    );
    expect(result.semanticMeasurement?.executionSteps).toBeLessThanOrEqual(
      8_000_000_000n,
    );
  },
  240_000,
);

it("resumes an authenticated Data refusal after the current-control checkpoint", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildDataRefusalDispute("d8799f011801ff", operatorVkey, now),
    { scriptSourcesItemCheckpoint: 3 },
  );
  expect(result.awardResult?.txHash).toHaveLength(64);
  expect(result.removal?.transactions.length).toBeGreaterThan(0);
}, 240_000);
it("cancels an authenticated Data refusal checkpoint and burns its thread token", async () => {
  const result = await runForcedValidationDisputeScenario(
    ({ operatorVkey, now }) =>
      buildDataRefusalDispute("1801", operatorVkey, now),
    { scriptSourcesItemCheckpoint: 3, cancelScriptSourcesItem: true },
  );
  expect(result.cancellation?.txHash).toHaveLength(64);
  expect(result.awardResult).toBeUndefined();
}, 240_000);
