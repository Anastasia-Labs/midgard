/** The validation-trace dispute's wallet-paid thread moves on two threads of
 * one case under the watcher's prover funding, its terms following the
 * emulator's live fees, which are raised before the watcher's first turn
 * until every thread move is priced over the per-move cap: our plain-Ada
 * wallet pays each of them whole. Two operators defend our case, each on its
 * own thread, and reveal, the second's clock on us starting first.
 *
 * The funding permit checks the wallet-move reserve at each such move, not
 * once for the case: a wallet funding both of our reveals lands both, the
 * one whose clock ends first first. Both defenders then go silent. When other
 * work drains the wallet between the two timeouts, the second is held under
 * the operator's alarm, naming the case, the thread and the deadline, before
 * any byte reaches the ledger; it is retried at each observation and lands
 * once the wallet is topped up, and both threads go on to close.
 *
 * The category is not installed until its defender exists; this file
 * stands in for that install so the production classifier mints the
 * objective's actuation permit. */
import * as SDK from "@al-ft/midgard-sdk";
import { Data, type UTxO } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE } from "../src/validation-trace-dispute/move-fee.js";
import {
  VALIDATION_TRACE_THREAD_AWAITING,
  validationTraceChallengerDeadlineMs,
  validationTraceDefenderDeadlineMs,
} from "../src/validation-trace-dispute/threads.js";
import { validationTraceRunState } from "../src/validation-trace-dispute/waiting.js";
import { validationTraceCaseExpiryMs } from "../src/validation-trace-dispute/wallet-move-unfunded.js";
import { VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED } from "../src/workflow/funding-reservation-permit.validation-dispute-wallet-move-reserve.js";
import { drainProver, topUpProver } from "./support/installed-prover-wallet.js";
import { watchLedger } from "./support/own-outputs-installed.js";
import {
  type FundedMoves,
  openTwoDefendedFundedCase,
  recordedOutput,
  type SecondDefense,
} from "./support/validation-trace-dispute-funded-moves.js";
import { turnUntil } from "./support/validation-trace-dispute-funded-runner.js";
import {
  ADA,
  TIMEOUT,
} from "./support/validation-trace-dispute-funded-verify-source.js";
import { landedMoveFee } from "./support/validation-trace-dispute-move-fees.js";
import {
  liveThread,
  revealAsDefender,
  threadOf,
} from "./support/validation-trace-dispute-scripted-defender.js";

// Stands in for the family's install, which waits on its defender.
vi.mock("../src/workflow/installed-categories.js", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("../src/workflow/installed-categories.js")
    >();
  return {
    ...actual,
    isInstalledFraudProofCategory: (
      category: Parameters<typeof actual.isInstalledFraudProofCategory>[0],
    ) =>
      category === "validationTraceDispute" ||
      actual.isInstalledFraudProofCategory(category),
  };
});

let journey: FundedMoves & Readonly<{ second: SecondDefense }>;
let restoreFees: (() => void) | undefined;

const outRefOf = (utxo: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

/** Both threads' tokens: the scene operator's, then the successor's. */
const units = () => [journey.unit, journey.second.unit];

/** The signed moves of `kind` the watcher built, in order. */
const signedOf = (kind: string) =>
  journey.moves.filter(({ authority }) => authority.kind === kind);

/** The thread token a captured move spends. */
const unitOf = ({ thread }: { readonly thread: UTxO }) =>
  units().find((unit) => thread.assets[unit] === 1n);

const landed = async (txHash: string) =>
  (await journey.scene.h.emulator.getTransactionStatus(txHash)).status ===
  "confirmed";

/** Each landed move of `kind`, by its hash. */
const landedOf = async (kind: string) => {
  const hashes = signedOf(kind).map(({ signed }) => signed.toHash());
  const confirmed = await Promise.all(hashes.map(landed));
  return hashes.filter((_, index) => confirmed[index]);
};

/** Turns until `done`, or `limit` turns. */
const turnWhile = async (done: () => Promise<boolean>, limit: number) => {
  for (let turn = 0; turn < limit && !(await done()); turn += 1)
    await journey.dispute.turn(journey.capture);
};

/** Expects our wallet paid the whole fee of each of `hashes`, within
 * `reserve`, and its successor kept all of its thread's lovelace. */
const expectWalletPaid = async (
  hashes: readonly string[],
  spent: ReadonlyMap<string, UTxO>,
  reserve: bigint,
) => {
  for (const hash of hashes) {
    const fee = await landedMoveFee(journey.scene, hash);
    expect(fee.paid).toBeGreaterThan(VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE);
    expect(fee.paid).toBeLessThanOrEqual(reserve);
    const move = journey.moves.find(({ signed }) => signed.toHash() === hash)!;
    expect(recordedOutput(journey.scene, `${hash}#0`).assets.lovelace).toBe(
      spent.get(unitOf(move)!)!.assets.lovelace,
    );
  }
};

/** Each live thread, by token. */
const liveThreads = async () =>
  new Map(
    await Promise.all(
      units().map(
        async (unit) =>
          [unit, (await liveThread(journey.scene, unit))!] as const,
      ),
    ),
  );

/**
 * Turns twice into the operator's alarm over the `stage` move of `thread`,
 * which must land by `deadline`: a pending run its next observation retries,
 * nothing reaching the ledger, every input released and the thread unmoved.
 * Returns each alarm's reason.
 */
const expectUnfundedAlarms = async ({
  stage,
  thread,
  deadline,
}: {
  readonly stage: string;
  readonly thread: UTxO;
  readonly deadline: number;
}): Promise<readonly string[]> => {
  const { scene, dispute, capture, caseOrigin } = journey;
  const caseUnit = Object.keys(caseOrigin.assets).find((unit) =>
    unit.startsWith(scene.chain.case.policyId),
  )!;
  const reasons: string[] = [];
  const watch = watchLedger(scene.h.emulator);
  try {
    for (let alarm = 0; alarm < 2; alarm += 1) {
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
        { wrap: capture },
      );
      expect(validationTraceRunState(result), history.join("\n")).toMatchObject(
        {
          kind: "pending",
          resumeOnObservation: true,
          namedState: {
            name: VALIDATION_DISPUTE_WALLET_MOVE_UNFUNDED,
            alarm: true,
          },
        },
      );
      if (result.kind !== "pending") throw new Error("no alarm");
      expect(result.reason).toContain(
        `cannot fund validation-trace-${stage}:${outRefOf(thread)} on thread ${outRefOf(thread)} of case ${caseUnit},`,
      );
      expect(result.reason).toContain(
        `before its deadline ${deadline.toString()};`,
      );
      // The refused attempt released every input it held.
      for (const reservation of await dispute.readReservations())
        expect(reservation).toMatchObject({ activeInputs: [] });
      reasons.push(result.reason);
    }
    expect(watch.ours()).toHaveLength(0);
  } finally {
    watch.restore();
  }
  const unit = units().find((held) => thread.assets[held] === 1n)!;
  expect(outRefOf((await liveThread(scene, unit))!)).toBe(outRefOf(thread));
  return reasons;
};

/** Our case's expiry. */
const caseExpiry = () =>
  validationTraceCaseExpiryMs(Data.from(journey.caseOrigin.datum!, SDK.Case));

describe("validation-trace dispute wallet-paid moves on two threads of one case under the prover funding permit", () => {
  beforeAll(async () => {
    journey = await openTwoDefendedFundedCase({
      liveParameters: true,
      // Fees rise before the watcher's first turn until every thread move's
      // minimum passes the per-move cap.
      beforeOpen: ({ h: { emulator } }) => {
        const parameters = emulator.protocolParameters;
        emulator.protocolParameters = { ...parameters, minFeeA: 1_500 };
        restoreFees = () => {
          emulator.protocolParameters = parameters;
        };
      },
    });
    const { scene, second } = journey;
    // The successor reveals first, so our clock on its thread ends first.
    for (const { defender, trace, unit } of [
      second,
      { defender: journey.defender, trace: journey.trace, unit: journey.unit },
    ]) {
      await revealAsDefender(
        scene,
        defender,
        (await liveThread(scene, unit))!,
        trace,
      );
      scene.h.emulator.awaitSlot(30);
    }
  }, TIMEOUT);

  afterAll(async () => {
    restoreFees?.();
    await journey?.dispute.close();
  });

  it(
    "holds our first reveal under the operator's alarm, naming the end of our clock on its thread, while the wallet holds less than the reserve a move it pays must find",
    async () => {
      const reserve = await journey.dispute.walletMoveReserve();
      await drainProver(journey.scene.h as never, reserve / 2n);
      const thread = (await liveThreads()).get(journey.second.unit)!;
      const reasons = await expectUnfundedAlarms({
        stage: "reveal",
        thread,
        // A reveal is on our clock, which ends before the case expires.
        deadline: Math.min(
          caseExpiry(),
          validationTraceChallengerDeadlineMs(threadOf(thread)),
        ),
      });
      for (const reason of reasons)
        expect(reason).toContain(
          `held=${(reserve / 2n).toString()} reserve=${reserve.toString()}`,
        );
    },
    TIMEOUT,
  );

  it(
    "lands both of our reveals from a wallet funding both, each paid whole by our wallet, the one whose clock ends first first",
    async () => {
      const { scene, dispute, capture } = journey;
      const reserve = await dispute.walletMoveReserve();
      const before = await liveThreads();
      await topUpProver(scene.h as never, 100n * ADA);
      await drainProver(scene.h as never, 2n * reserve + 5n * ADA);
      const { result, history } = await turnUntil(
        dispute,
        VALIDATION_TRACE_THREAD_AWAITING,
        { wrap: capture },
      );
      expect(result.kind, history.join("\n")).toBe("pending");
      const reveals = await landedOf("reveal");
      expect(reveals).toHaveLength(2);
      await expectWalletPaid(reveals, before, reserve);
      const order = reveals.map(
        (hash) =>
          unitOf(
            journey.moves.find(({ signed }) => signed.toHash() === hash)!,
          )!,
      );
      expect(new Set(order)).toStrictEqual(new Set(units()));
      const ends = (unit: string) =>
        validationTraceChallengerDeadlineMs(threadOf(before.get(unit)!));
      expect(ends(order[0]!)).toBeLessThan(ends(order[1]!));
      expect(order[0]).toBe(journey.second.unit);
    },
    TIMEOUT,
  );

  it(
    "holds the second timeout under the operator's alarm when other work drains the wallet after the first, before any byte reaches the ledger, and lands it once the wallet is topped up; both threads then close",
    async () => {
      const { scene, dispute } = journey;
      const silent = await liveThreads();
      // Both defenders' clocks run out.
      const deadline = Math.max(
        ...[...silent.values()].map((thread) =>
          validationTraceDefenderDeadlineMs(threadOf(thread)),
        ),
      );
      const lucid = scene.h.proverLucid;
      scene.h.emulator.awaitSlot(
        lucid.unixTimeToSlot(deadline) - lucid.currentSlot() + 2,
      );
      const reserve = await dispute.walletMoveReserve();
      await drainProver(scene.h as never, reserve);
      await turnWhile(async () => signedOf("timeout").length >= 1, 4);
      const [first] = signedOf("timeout");
      expect(first).toBeDefined();
      const firstHash = first!.signed.toHash();
      expect(await landed(firstHash)).toBe(true);
      await expectWalletPaid([firstHash], silent, reserve);
      // Other work leaves the wallet more than a timeout's fee and its
      // change, and less than the reserve a move our wallet pays must find.
      const fee = (await landedMoveFee(scene, firstHash)).paid;
      const left = fee + (reserve - fee) / 2n;
      await drainProver(scene.h as never, left);
      const remaining = units().find((unit) => unit !== unitOf(first!))!;
      // A timeout is not on our clock: it must land before the case expires.
      const reasons = await expectUnfundedAlarms({
        stage: "timeout",
        thread: silent.get(remaining)!,
        deadline: caseExpiry(),
      });
      // The permit's own check at this move, not the builder's.
      for (const reason of reasons)
        expect(reason).toContain(
          `held=${left.toString()} reserve=${reserve.toString()}`,
        );
      await topUpProver(scene.h as never, 100n * ADA);
      await turnWhile(async () => (await landedOf("timeout")).length >= 2, 4);
      const timeouts = await landedOf("timeout");
      expect(timeouts).toHaveLength(2);
      await expectWalletPaid(timeouts, silent, reserve);
      // Nothing wedged: both threads close into our case.
      await turnWhile(
        async () => (await landedOf("close_thread")).length >= 2,
        8,
      );
      for (const unit of units())
        expect(await liveThread(scene, unit)).toBeUndefined();
    },
    TIMEOUT,
  );
});
