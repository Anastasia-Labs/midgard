/** The validation-trace dispute's reveal across a watcher process stop, under
 * the watcher's prover funding, on the emulator. A defender opens a thread
 * on our case and reveals; the watcher signs our reveal and stops. The next
 * process holds nothing of the stopped one. Stopped after broadcasting, the
 * stopped bytes land and the next process sends nothing again. Stopped
 * before broadcasting, the next process builds the reveal again from the
 * chain, and the stopped bytes can no longer land.
 * Either way exactly one reveal moves the thread.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import { beforeEach, describe, expect, it } from "vitest";

import { VALIDATION_TRACE_THREAD_AWAITING } from "../src/validation-trace-dispute/threads.js";
import { workflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import {
  confirmedMoves,
  type FundedMoves,
  liveFundedThread,
  moveSends,
  openDefendedFundedCase,
} from "./support/validation-trace-dispute-funded-moves.js";
import {
  type AdapterWrap,
  turnUntil,
} from "./support/validation-trace-dispute-funded-runner.js";
import { TIMEOUT } from "./support/validation-trace-dispute-funded-verify-source.js";
import {
  isTurn,
  revealAsDefender,
  threadOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

let journey: FundedMoves;

const isReveal = (action: { readonly input: unknown }) =>
  (action.input as { readonly stage?: unknown }).stage === "reveal";

/**
 * Stops the watcher process at our reveal's submit, after (`broadcast`) or
 * before its bytes reach the ledger. The stopped turn never returns.
 */
const stopAtRevealSubmit = async ({
  broadcast,
}: {
  readonly broadcast: boolean;
}) => {
  let signedHex: string | undefined;
  let signedHash: string | undefined;
  let stopped!: () => void;
  const stop = new Promise<void>((resolve) => {
    stopped = resolve;
  });
  const wrap: AdapterWrap = (adapter) => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      if (isReveal(context.action)) {
        const signed = workflowPreflightTransaction(preflight)!;
        signedHex = signed.toTransaction().to_cbor_hex();
        signedHash = signed.toHash();
      }
      return preflight;
    },
    submit: async (input) => {
      if (!isReveal(input.action)) return await adapter.submit(input);
      if (broadcast) await adapter.submit(input);
      stopped();
      return await new Promise<never>(() => undefined);
    },
  });
  const turns = turnUntil(journey.dispute, VALIDATION_TRACE_THREAD_AWAITING, {
    wrap,
  });
  await Promise.race([
    stop,
    turns.then(({ history }) => {
      throw new Error(`the process ended before submit: ${history.join("\n")}`);
    }),
  ]);
  if (signedHash === undefined) throw new Error("the reveal was not signed");
  return { signedHex: signedHex!, signedHash };
};

/** The process restarted, holding nothing of the stopped one, run until
 * our reveal has put the thread on the defender's turn; returns the hash of
 * the reveal that moved the thread. */
const resume = async () => {
  journey.dispute.restart();
  const { result, history } = await turnUntil(
    journey.dispute,
    VALIDATION_TRACE_THREAD_AWAITING,
  );
  expect(result.kind, history.join("\n")).toBe("pending");
  const reveals = (await confirmedMoves(journey.dispute)).filter(
    ({ stage }) => stage === "reveal",
  );
  // Exactly one reveal moved the thread.
  expect(reveals).toHaveLength(1);
  const live = await liveFundedThread(journey);
  expect(isTurn(threadOf(live), "AwaitingOperator")).toBe(true);
  expect(reveals[0]!.outputs).toStrictEqual([`${live.txHash}#0`]);
  return { landed: live.txHash };
};

describe("validation-trace dispute reveal across a watcher process stop", () => {
  beforeEach(async () => {
    journey = await openDefendedFundedCase();
    await revealAsDefender(
      journey.scene,
      journey.defender,
      await liveFundedThread(journey),
      journey.trace,
    );
  }, TIMEOUT);

  it(
    "lands the same signed reveal after a stop between its broadcast and its landing, sending nothing again",
    async () => {
      const stop = await stopAtRevealSubmit({ broadcast: true });
      const { landed } = await resume();
      expect(landed).toBe(stop.signedHash);
      expect(journey.dispute.rebroadcasts).toStrictEqual([]);
      expect(
        moveSends(journey.dispute, "reveal").map(({ txHash }) => txHash),
      ).toStrictEqual([stop.signedHash]);
    },
    TIMEOUT,
  );

  it(
    "builds the reveal again after a stop before broadcast: the next process's own reveal lands, once, and the stopped bytes cannot land beside it",
    async () => {
      const stop = await stopAtRevealSubmit({ broadcast: false });
      const { landed } = await resume();
      // The next process knows nothing of the stopped bytes: it rebroadcasts
      // nothing and sends the reveal it builds from the chain, which may be
      // the very same transaction.
      expect(journey.dispute.rebroadcasts).not.toContain(stop.signedHex);
      const sends = moveSends(journey.dispute, "reveal");
      expect(sends).toHaveLength(2);
      expect(sends[0]!.txHash).toBe(stop.signedHash);
      expect(landed).toBe(sends[1]!.txHash);
      const { emulator } = journey.scene.h;
      await expect(emulator.submitTx(stop.signedHex)).rejects.toThrow();
    },
    TIMEOUT,
  );
});
