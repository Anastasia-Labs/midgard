/**
 * The script-decode record through the watcher's installed application when
 * the process stops or the prover wallet runs dry in the middle of it.
 *
 * A watcher process that stops between two journaled record actions starts
 * again over its journal and reservation store and finishes the record it
 * began: it leaves exactly the outputs an uninterrupted run leaves, so no
 * record action is taken twice. A wallet that cannot pay for the next record
 * action pauses the objective with the typed underfunded reason, which says
 * it funds from plain-ADA coins only, and a top-up resumes it to conviction,
 * a coin holding a token left unspent. A Walk the ledger rejects on every
 * one of its bounded attempts holds the objective instead of refitting it
 * forever.
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

import { SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS } from "../src/script-decode/record.js";
import {
  SCRIPT_DECODE_RECORD_HELD,
  SCRIPT_DECODE_RECORD_UNDERFUNDED,
} from "../src/workflow/script-decode-prerequisite.port.js";
import {
  drainProver,
  sendTokenCoin,
  topUpProver,
} from "./support/installed-prover-wallet.js";
import { scriptDecodeDeploymentOf } from "./support/script-decode-emulator.js";
import { interceptSubmissions } from "./support/script-decode-installed-host.js";
import {
  INSTALLED_WITNESS,
  type InstalledTurnContext,
  runInstalled,
  stagingOutputsOf,
} from "./support/script-decode-installed-run.js";
import {
  delayChainScript,
  plutusItem,
  witnessSubject,
} from "./support/script-decode-plutus-max-shape.js";

/** An undecodable witness whose record takes a Bind, a few Walks, a Finish. */
const SCENARIO = {
  ...INSTALLED_WITNESS,
  nativeTx: () => witnessSubject([plutusItem(delayChainScript(600))]).nativeTx,
};
const ADA = 1_000_000n;
/** Confirmation polls a run may take before it counts as spinning. */
const POLL_CAP = 5_000;
/**
 * Confirmation polls one process of the restart row may take, and turns
 * each of its runs may take: each run convicts well inside both, so a run
 * that spins within a turn or across turns fails fast.
 */
const RECOVERY_POLL_CAP = 100;
const RECOVERY_TURN_CAP = 10;

type Harness = InstalledTurnContext["harness"];

/** The address every Bind, Walk and Finish pays its run or record to. */
const recordAddress = (harness: Harness) =>
  scriptDecodeDeploymentOf(harness.emulator).validator.spendingScriptAddress;

describe("script-decode record recovery in the installed watcher application", () => {
  it("resumes a record its stopped process began from the journal, taking no record action twice", async () => {
    const uninterrupted = await runInstalled(SCENARIO, hooks, {
      maxPolls: RECOVERY_POLL_CAP,
      maxTurns: RECOVERY_TURN_CAP,
    });
    expect(uninterrupted.convicted).toBe(true);

    let stops = 0;
    let engineered = false;
    const restarted = await runInstalled(SCENARIO, hooks, {
      maxPolls: RECOVERY_POLL_CAP,
      maxTurns: RECOVERY_TURN_CAP,
      beforeTurns: ({ harness, clock }) => {
        const address = recordAddress(harness);
        const seen = new Set<string>();
        let bound = false;
        let walked = false;
        // The process stops while it waits for the Bind, and again for the
        // first Walk: both submitted, neither yet journaled as confirmed.
        return interceptSubmissions(harness.emulator, (submission) => {
          if (seen.has(submission.txHash) || !submission.paysTo(address))
            return "accept";
          seen.add(submission.txHash);
          if (!bound || (submission.withdraws && !walked)) {
            if (bound) walked = true;
            bound = true;
            engineered = true;
            clock.state.crashAfterPolls = clock.state.polls;
          }
          return "accept";
        }).restore;
      },
      onStopped: async (error, { application, clock }) => {
        expect(String(error)).toContain(
          "installed application process stopped",
        );
        expect(engineered, "a stop the row engineered, not the poll cap").toBe(
          true,
        );
        engineered = false;
        stops += 1;
        await application.restart();
        clock.state.crashAfterPolls = clock.state.polls + RECOVERY_POLL_CAP;
      },
    });

    expect(restarted.convicted).toBe(true);
    expect(stops).toBe(2);
    expect(restarted.footprint).toEqual(uninterrupted.footprint);
  }, 1_800_000);

  it("pauses a record the prover wallet cannot fund with the typed reason, and resumes it after a top-up", async () => {
    let drained = false;
    let toppedUp = false;
    const pausedReasons: string[] = [];
    const run = await runInstalled(SCENARIO, hooks, {
      beforeTurns: ({ harness, clock }) => {
        clock.state.crashAfterPolls = POLL_CAP;
        // Once the record has begun, the wallet runs low while the runner
        // waits for a confirmation.
        clock.state.onPoll = async () => {
          if (drained || (await stagingOutputsOf(harness)).length === 0) return;
          drained = true;
          await drainProver(harness, 1_500_000n);
        };
      },
      afterTurn: async ({ harness, result }) => {
        if (!drained || toppedUp) return;
        const { reason } = result;
        if (reason?.startsWith(SCRIPT_DECODE_RECORD_UNDERFUNDED) !== true)
          return;
        // Paused, not wedged: it pauses again on each observation until
        // the wallet is funded.
        pausedReasons.push(reason);
        if (pausedReasons.length < 2) return;
        await topUpProver(harness, 5_000n * ADA);
        toppedUp = true;
      },
    });

    expect(drained).toBe(true);
    expect(pausedReasons).toHaveLength(2);
    expect(toppedUp).toBe(true);
    expect(run.convicted).toBe(true);
  }, 1_800_000);

  it("pauses a record the wallet's plain-ADA coins cannot fund beside a coin holding a token, and convicts after a top-up without spending that coin", async () => {
    let drained = false;
    let toppedUp = false;
    let tokenCoin: OutRef | undefined;
    let harness: Harness | undefined;
    const pausedReasons: string[] = [];
    const run = await runInstalled(SCENARIO, hooks, {
      beforeTurns: (context) => {
        harness = context.harness;
        context.clock.state.crashAfterPolls = POLL_CAP;
        context.clock.state.onPoll = async () => {
          if (drained || (await stagingOutputsOf(context.harness)).length === 0)
            return;
          drained = true;
          await drainProver(context.harness, 1_500_000n);
          // The wallet's ada now sits in a coin that also holds a token.
          tokenCoin = await sendTokenCoin(context.harness, 5_000n * ADA);
        };
      },
      afterTurn: async ({ harness, result }) => {
        if (!drained || toppedUp) return;
        if (
          result.reason?.startsWith(SCRIPT_DECODE_RECORD_UNDERFUNDED) !== true
        )
          return;
        pausedReasons.push(result.reason);
        await topUpProver(harness, 5_000n * ADA);
        toppedUp = true;
      },
    });

    expect(pausedReasons).toHaveLength(1);
    expect(pausedReasons[0]).toContain(
      "from its plain-ADA coins (a coin holding a token is never spent)",
    );
    expect(run.convicted).toBe(true);
    expect(await harness!.emulator.getUtxosByOutRef([tokenCoin!])).toHaveLength(
      1,
    );
  }, 1_800_000);

  it("holds a Walk the ledger rejects on every bounded attempt, submitting it no more", async () => {
    const rejected = new Set<string>();
    const held: number[] = [];
    const run = await runInstalled(SCENARIO, hooks, {
      beforeTurns: ({ harness, clock }) => {
        clock.state.crashAfterPolls = POLL_CAP;
        const address = recordAddress(harness);
        return interceptSubmissions(harness.emulator, (submission) => {
          if (!submission.withdraws || !submission.paysTo(address))
            return "accept";
          rejected.add(submission.txHash);
          // A Walk past the bound stops the process: the refit is unbounded.
          if (rejected.size > SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS)
            clock.state.crashAfterPolls = clock.state.polls;
          return "reject";
        }).restore;
      },
      afterTurn: ({ result }) => {
        if (result.reason?.startsWith(SCRIPT_DECODE_RECORD_HELD) !== true)
          return;
        held.push(rejected.size);
        return held.length < 3 ? undefined : "stop";
      },
    });

    expect(run.convicted).toBe(false);
    // Each attempt is a refit Walk the ledger rejected; once held, the
    // objective pauses on each observation and submits no further one.
    expect(rejected.size).toBe(SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS);
    expect(held).toEqual([
      SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS,
      SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS,
      SCRIPT_DECODE_WALK_SUBMIT_ATTEMPTS,
    ]);
  }, 1_800_000);
});
