import "./da-bond-pool-journey.faults.js";

import { type DaBondPoolJourneyRecord } from "./da-bond-pool-journey.js";

export const failedAssertions = (record: DaBondPoolJourneyRecord) =>
  record.stages.flatMap((stage) =>
    stage.assertions
      .filter((assertion) => assertion.ok === false)
      .map((assertion) => ({ step: stage.step, name: assertion.name })),
  );

export const stage = (record: DaBondPoolJourneyRecord, step: number) => {
  const found = record.stages.find((candidate) => candidate.step === step);
  if (found === undefined) throw new Error(`step ${step} not recorded`);
  return found;
};

export const META = {
  adapter: "emulator",
  runDir: "/tmp/run",
  deploymentManifestId: "manifest-1",
  networkMagic: 42,
  gitHead: "abc123",
} as const;
