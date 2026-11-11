import * as SDK from "@al-ft/midgard-sdk";
import {
  createPublishedWorkflowDeploymentAccounts,
  publishWorkflowDeployment,
} from "midgard-node/tests/helpers/published-workflow-deployment";
import { DEPOSIT_BLOCK_INTERVAL_MS } from "midgard-watcher/tests/support/published-deposit-trace";
import { afterEach, expect, it, vi } from "vitest";

import { stageAttachedTransitionTrace } from "./stack-trace-staging.js";

afterEach(() => {
  vi.restoreAllMocks();
});

/** An attached stack whose committee is never reached in these cases. */
const attachedStack = {
  attest: async () => {
    throw new Error("The trace reached attestation");
  },
  shiftWorkMs:
    Number(SDK.MAX_EVENT_VALIDITY_RANGE_LENGTH_MS) +
    SDK.EVENT_WAIT_DURATION_MS +
    DEPOSIT_BLOCK_INTERVAL_MS +
    61_000,
  beforeFaultCommit: async () => {},
};

// On an attached stack the trace reads the tail's ledger inside operator-2's
// shift. The replay must already stand at that tail before the trace
// onboards operator-2, so the read inside the shift only follows the blocks
// appended since.
it("reads the attached stack's tail from the replay before onboarding operator-2", async () => {
  const deployment = await publishWorkflowDeployment({
    accounts: createPublishedWorkflowDeploymentAccounts(),
  });
  vi.spyOn(Date, "now").mockImplementation(() => deployment.emulator.now());
  const events: string[] = [];
  const onStage = (name: string) => {
    events.push(name);
  };
  const onboarded = () => events.includes("operator registration");

  // A tail the replay has no ledger for is refused before any onboarding.
  await expect(
    stageAttachedTransitionTrace(deployment, {
      attachedStack,
      onStage,
      liveTail: {
        ledgerAt: async (headerHash) => {
          events.push(`replay read ${headerHash}`);
          return undefined;
        },
      },
    }),
  ).rejects.toThrow(/has no independently replayed ledger/u);
  expect(onboarded()).toBe(false);

  // The replay answers until operator-2 is onboarded, then stops, so
  // staging ends at its first read inside the shift.
  events.length = 0;
  await expect(
    stageAttachedTransitionTrace(deployment, {
      attachedStack,
      onStage,
      liveTail: {
        ledgerAt: async (headerHash) => {
          events.push(`replay read ${headerHash}`);
          return onboarded() ? undefined : [];
        },
      },
    }),
  ).rejects.toThrow(/has no independently replayed ledger/u);
  const reads = events.filter((event) => event.startsWith("replay read "));
  expect(events[0]).toMatch(/^replay read /u);
  expect(events.indexOf("operator registration")).toBeGreaterThan(0);
  // The read inside the shift is of the same tail the replay was brought to.
  expect(reads).toHaveLength(2);
  expect(reads[1]).toBe(reads[0]);
}, 1_800_000);
