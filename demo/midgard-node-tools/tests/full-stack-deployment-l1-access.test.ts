/**
 * The stack's deployment commands (the hub-oracle nonce, the reference
 * scripts and initialization) run on the local Cardano node's ledger alone,
 * whatever L1 access the stack's environment names, and read no wallet
 * through Kupo.
 */
import { afterEach, expect, it } from "vitest";

import { deploymentSteps } from "../src/full-stack/deployment.js";
import {
  RecordingProcesses,
  removeStackFixtures,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

afterEach(removeStackFixtures);

const DEPLOYMENT = {
  nonce: "nonce-create-or-resume",
  references: "references-publish",
  initialize: "initialize-submit",
} as const;

it("deploys over the local node's ledger, reading no wallet through Kupo", async () => {
  const { config } = await stackFixture();
  const processes = new RecordingProcesses(config, stackEnvironment(config));
  processes.env.L1_ACCESS = "kupmios";
  processes.responses["storage-identity"] = { id: "7" };
  processes.responses["host-database-identity"] = "7";
  for (const step of deploymentSteps(processes))
    if (step.id in DEPLOYMENT) await step.execute(undefined);

  for (const id of Object.values(DEPLOYMENT))
    expect(
      processes.calls.find((call) => call.id === id)?.args.slice(1, 3),
    ).toEqual(["--l1", "node"]);
  expect(processes.calls.some((call) => call.args.includes("l1-utxos"))).toBe(
    false,
  );
});
