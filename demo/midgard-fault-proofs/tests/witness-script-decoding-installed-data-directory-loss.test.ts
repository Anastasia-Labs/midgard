import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
  recovery: undefined as unknown,
}));
vi.mock("../src/workflow/deployment-manifest-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/workflow/deployment-manifest-binding.js")
  >()),
  bindFraudProofWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  const { withFraudProofL1Recovery } = await import(
    "../src/workflow/l1-source.js"
  );
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      withFraudProofL1Recovery(
        actual.createFraudProofFamilyRawL1ObservationPort({
          ...input,
          authority: hooks.authority as Parameters<
            typeof actual.createFraudProofFamilyRawL1ObservationPort
          >[0]["authority"],
        }),
        hooks.recovery as Parameters<typeof withFraudProofL1Recovery>[1],
      ),
  };
});

import {
  kindOf,
  scriptDecodeRoles,
  watchLedger,
} from "./support/own-outputs-installed.js";
import {
  INSTALLED_WITNESS,
  runInstalled,
} from "./support/script-decode-installed-run.js";
import {
  delayChainScript,
  plutusItem,
  witnessSubject,
} from "./support/script-decode-plutus-max-shape.js";

/** An undecodable witness whose record stages, binds, walks and finishes. */
const ITEM = plutusItem(delayChainScript(600));
const SCENARIO = {
  ...INSTALLED_WITNESS,
  nativeTx: () => witnessSubject([ITEM]).nativeTx,
};
/** Confirmation polls one process may take before it counts as spinning. */
const POLL_CAP = 200;
/** Turns the run may take: a recovered run convicts well inside this. */
const TURN_CAP = 12;

/**
 * Stops the watcher process once its first transaction of `kind` is on
 * chain, lets one block pass, and starts another over an emptied data
 * directory: the journals and the funding store are gone, the watcher
 * detects the fault again, and only the chain keeps the objective's
 * progress. Every later turn must make progress: a turn that throws again
 * fails the run.
 */
const runWithLostDataDirectory = async (kind: "stage" | "step") => {
  let stops = 0;
  let stoppedAfter: readonly string[] = [];
  const run = await runInstalled(SCENARIO, hooks, {
    maxPolls: POLL_CAP,
    maxTurns: TURN_CAP,
    beforeTurns: ({ harness, clock }) => {
      const watch = watchLedger(harness.emulator);
      const roles = scriptDecodeRoles({
        emulator: harness.emulator,
        proverAddress: harness.proverSigner.address,
        item: ITEM,
      });
      clock.state.onPoll = async () => {
        if (stoppedAfter.length !== 0) return;
        const kinds = watch
          .ours()
          .map((transaction) => kindOf(transaction, roles.staging, roles.run));
        if (!kinds.includes(kind)) return;
        stoppedAfter = kinds;
        clock.state.crashAfterPolls = clock.state.polls;
      };
      return () => {
        clock.state.onPoll = undefined;
        watch.restore();
      };
    },
    onStopped: async (error, { application, clock, harness }) => {
      expect(String(error)).toContain("installed application process stopped");
      stops += 1;
      expect(stops, "only the stop the row engineered").toBe(1);
      harness.emulator.awaitBlock();
      await application.loseDataDirectory();
      clock.state.crashAfterPolls = clock.state.polls + POLL_CAP;
    },
  });
  expect(stoppedAfter.filter((value) => value === kind)).toHaveLength(1);
  expect(stops).toBe(1);
  return run;
};

describe("script-decode data directory loss in the installed watcher application", () => {
  it("convicts after the data directory is lost once a field publication is on chain", async () => {
    const run = await runWithLostDataDirectory("stage");
    expect(run.convicted).toBe(true);
  }, 1_800_000);

  it("convicts after the data directory is lost once the thread has stepped on chain, spending its own thread output", async () => {
    const run = await runWithLostDataDirectory("step");
    expect(run.convicted).toBe(true);
  }, 1_800_000);
});
