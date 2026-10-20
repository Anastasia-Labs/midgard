/**
 * A proof action the prover wallet cannot pay for, through the watcher's
 * installed application, for a fault that takes no script-decode record: the
 * family's own proof transactions, funded by the shared reserved wallet.
 *
 * A wallet that cannot pay for the next action pauses the objective with the
 * funding-shortfall reason, which says it funds from plain-ADA coins only, and
 * releases its reservation; a top-up resumes it to conviction. A coin holding
 * a token is never spent, whether or not it alone could have paid.
 */
import type { OutRef } from "@lucid-evolution/lucid";
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

import { WORKFLOW_FUNDING_SHORTFALL } from "../src/workflow/funding-reservation-permit.js";
import {
  drainProver,
  sendTokenCoin,
  topUpProver,
} from "./support/installed-prover-wallet.js";
import {
  INSTALLED_WITNESS,
  type InstalledTurnContext,
  runInstalled,
} from "./support/script-decode-installed-run.js";
import { headerSmallShape } from "./witness-script-decoding-lifecycle.scan-honest-accepted-to-close.js";

/** A witness whose wrapper does not decode: no Plutus item, so no record. */
const SCENARIO = {
  ...INSTALLED_WITNESS,
  nativeTx: () => headerSmallShape().nativeTx,
  violationId: "witness-script-header-malformed",
};
const ADA = 1_000_000n;
/** Confirmation polls a run may take before it counts as spinning. */
const POLL_CAP = 5_000;
const PLAIN_ADA_RULE =
  "from its plain-ADA coins (a coin holding a token is never spent)";

type Harness = InstalledTurnContext["harness"];

describe("a proof action the prover wallet cannot fund, in the installed watcher application", () => {
  it("pauses on a wallet whose ada sits in a coin holding a token, and convicts after a top-up without spending that coin", async () => {
    let tokenCoin: OutRef | undefined;
    let harness: Harness | undefined;
    let toppedUp = false;
    const pausedReasons: string[] = [];
    const run = await runInstalled(SCENARIO, hooks, {
      beforeTurns: async (context) => {
        harness = context.harness;
        context.clock.state.crashAfterPolls = POLL_CAP;
        // Collateral and a coin too small to fund anything; the wallet's
        // ada sits in a coin that also holds a token.
        await drainProver(context.harness, 1_500_000n);
        tokenCoin = await sendTokenCoin(context.harness, 5_000n * ADA);
      },
      afterTurn: async ({ harness, result }) => {
        if (toppedUp) return;
        if (result.reason?.startsWith(WORKFLOW_FUNDING_SHORTFALL) !== true)
          return;
        pausedReasons.push(result.reason);
        await topUpProver(harness, 5_000n * ADA);
        toppedUp = true;
      },
    });

    expect(pausedReasons).toHaveLength(1);
    expect(pausedReasons[0]).toContain(PLAIN_ADA_RULE);
    expect(run.convicted).toBe(true);
    expect(await harness!.emulator.getUtxosByOutRef([tokenCoin!])).toHaveLength(
      1,
    );
  }, 1_800_000);

  it("pauses on an underfunded plain-ADA wallet on each observation, and convicts after a top-up", async () => {
    let toppedUp = false;
    const pausedReasons: string[] = [];
    const run = await runInstalled(SCENARIO, hooks, {
      beforeTurns: async ({ harness, clock }) => {
        clock.state.crashAfterPolls = POLL_CAP;
        await drainProver(harness, 1_500_000n);
      },
      afterTurn: async ({ harness, result }) => {
        if (toppedUp) return;
        if (result.reason?.startsWith(WORKFLOW_FUNDING_SHORTFALL) !== true)
          return;
        // Paused, not wedged: it pauses again on each observation until the
        // wallet is funded.
        pausedReasons.push(result.reason);
        if (pausedReasons.length < 2) return;
        await topUpProver(harness, 5_000n * ADA);
        toppedUp = true;
      },
    });

    expect(pausedReasons).toHaveLength(2);
    expect(toppedUp).toBe(true);
    expect(run.convicted).toBe(true);
  }, 1_800_000);
});
