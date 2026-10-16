import { type MidgardValidationDispute } from "@al-ft/midgard-core";
import { type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { expectOnchainRefusal } from "./support/submit-init-emulator-shared.js";
import {
  gameMove,
  prepareResolutionMove,
} from "./support/validation-game-moves.js";
import {
  enterResolution,
  nextReveal,
  openThread,
  options,
  playToOneStep,
  reveal,
} from "./support/validation-game-play.js";
import {
  buildMove,
  cap,
  createdAt,
  type Funding,
  maxRangeMs,
  type Move,
  moveWindow,
  seededIdentity,
  seedThreads,
  setUpGameScene,
  sideClockMs,
  submitMove,
  threadDatum,
  threadLovelace,
  walletPays,
} from "./support/validation-game-scene.js";
import { divergentTraces, openGame } from "./support/validation-game-traces.js";

const lovelaceOf = (utxo: UTxO) => BigInt(utxo.assets.lovelace ?? 0n);

describe("validation game on the emulator", () => {
  it("lets a third party make both reveals, each paying a capped fee from the thread", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const opened = await openThread(scene, traces);
    const revealed = await reveal(scene, traces, opened);
    expect(revealed.dispute.turn.type).toBe("awaitingChallenger");
    expect(revealed.utxo.address).toBe(scene.game.address);
    expect(revealed.utxo.datum).toBe(
      threadDatum(scene, revealed.identity, revealed.dispute),
    );
    expect(lovelaceOf(revealed.utxo)).toBe(threadLovelace - cap);
    const ready = await reveal(scene, traces, revealed);
    expect(ready.dispute.turn.type).toBe("readyForOneStep");
    expect(lovelaceOf(ready.utxo)).toBe(threadLovelace - 2n * cap);
  });

  it("refuses a lone thread input paying more than the cap, and a second input taking thread value", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const opened = await openThread(scene, traces);
    const at = (funding: Funding) => {
      const from = scene.emulator.now();
      const { action, next } = nextReveal(traces, opened.dispute, from);
      return gameMove(scene, opened, action, next, options(from, { funding }));
    };
    await expectOnchainRefusal(() =>
      buildMove(scene, at({ kind: "thread", fee: cap + 1n })),
    );
    await expectOnchainRefusal(() =>
      buildMove(scene, at({ kind: "wallet", skim: 1n })),
    );
    const hash = await submitMove(scene, at(walletPays));
    expect(lovelaceOf(await createdAt(scene, hash, 0))).toBe(threadLovelace);
  });

  it("enters resolution and prepares it only with the fraud prover's signature", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const ready = await playToOneStep(
      scene,
      traces,
      await openThread(scene, traces),
    );
    await expectOnchainRefusal(() =>
      buildMove(scene, enterResolution(scene, ready, { signed: false }).move),
    );
    const entered = enterResolution(scene, ready, { signed: true });
    const enteredHash = await submitMove(scene, entered.move);
    const boundary = await createdAt(scene, enteredHash, 0);
    expect(boundary.address).toBe(scene.boundary.address);
    expect(boundary.datum).toBe(
      threadDatum(scene, ready.identity, entered.next),
    );

    const prepare = (signed: boolean) =>
      prepareResolutionMove(
        scene,
        { ...ready, utxo: boundary, dispute: entered.next },
        traces.agreedLowState,
        options(scene.emulator.now(), { signed }),
      );
    await expectOnchainRefusal(() => buildMove(scene, prepare(false)));
    const preparedHash = await submitMove(scene, prepare(true));
    const resolver = await createdAt(scene, preparedHash, 0);
    expect(resolver.address).toBe(prepare(true).outputs[0]!.address);
    expect(resolver.datum).toBe(prepare(true).outputs[0]!.datum);
  });

  it("refuses a challenger timeout until the operator's clock has run out, then lets anyone hand the thread to the timeout", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const opened = await openThread(scene, traces);
    const toTimeout = (from: number) =>
      gameMove(
        scene,
        opened,
        { kind: "EnterChallengerTimeout" },
        opened.dispute,
        options(from),
      );
    // A lower bound exactly on the operator's deadline is too early.
    const deadline = opened.dispute.turnStartedAt + sideClockMs;
    scene.emulator.awaitSlot((deadline - scene.emulator.now()) / 1000);
    expect(scene.emulator.now()).toBe(deadline);
    await expectOnchainRefusal(() =>
      buildMove(scene, toTimeout(scene.emulator.now())),
    );
    scene.emulator.awaitSlot(1);
    const timedOut = await createdAt(
      scene,
      await submitMove(scene, toTimeout(scene.emulator.now())),
      0,
    );
    expect(timedOut.address).toBe(scene.timeout.address);
    expect(timedOut.datum).toBe(
      threadDatum(scene, opened.identity, opened.dispute),
    );
  });

  it("charges a long gap inside the clock to the mover, exactly, and convicts no one", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const opened = await openThread(scene, traces);
    const deadline = opened.dispute.turnStartedAt + sideClockMs;
    scene.emulator.awaitSlot((deadline - scene.emulator.now()) / 1000 - 30);
    const from = scene.emulator.now();
    const { action, next } = nextReveal(traces, opened.dispute, from);
    expect(next.operatorClockMs).toBe(30_000);
    const move = (dispute: MidgardValidationDispute) =>
      gameMove(scene, opened, action, dispute, options(from));
    await expectOnchainRefusal(() =>
      buildMove(scene, move({ ...next, operatorClockMs: 29_999 })),
    );
    await expectOnchainRefusal(() =>
      buildMove(scene, move({ ...next, operatorClockMs: 30_001 })),
    );
    await submitMove(scene, move(next));
  });

  it("charges nothing for a lower bound before the turn start, and charges from the previous upper bound", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const opened = await openThread(scene, traces);
    const operatorFrom = scene.emulator.now();
    const revealed = await reveal(scene, traces, opened);
    const previousUpper = moveWindow(operatorFrom).currentTimeUpper;
    expect(revealed.dispute.turnStartedAt).toBe(previousUpper);

    // The challenger's window opens before the operator's upper bound.
    const early = operatorFrom + 1000;
    expect(early).toBeLessThan(previousUpper);
    expect(scene.emulator.now()).toBeLessThan(early + maxRangeMs);
    const { action, next } = nextReveal(traces, revealed.dispute, early);
    expect(next.challengerClockMs).toBe(sideClockMs);
    await expectOnchainRefusal(() =>
      buildMove(
        scene,
        gameMove(
          scene,
          revealed,
          action,
          { ...next, challengerClockMs: sideClockMs - 1 },
          options(early),
        ),
      ),
    );
    await submitMove(
      scene,
      gameMove(scene, revealed, action, next, options(early)),
    );
  });

  it("charges a move made after a maximal upper bound only the time since that bound", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const opened = await openThread(scene, traces);
    const revealed = await reveal(scene, traces, opened);
    scene.emulator.awaitSlot(5);
    const from = scene.emulator.now();
    const { action, next } = nextReveal(traces, revealed.dispute, from);
    expect(next.challengerClockMs).toBe(
      sideClockMs - (from - revealed.dispute.turnStartedAt),
    );
    await submitMove(
      scene,
      gameMove(scene, revealed, action, next, options(from)),
    );
  });

  it("refuses a validity range longer than the dispute maximum", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const opened = await openThread(scene, traces);
    const from = scene.emulator.now();
    const { action, next } = nextReveal(traces, opened.dispute, from);
    await expectOnchainRefusal(() =>
      buildMove(
        scene,
        gameMove(
          scene,
          opened,
          action,
          next,
          options(from, { width: maxRangeMs + 1000 }),
        ),
      ),
    );
    await submitMove(
      scene,
      gameMove(scene, opened, action, next, options(from)),
    );
  });

  it("moves two threads in one transaction only when each redeemer names its own successor", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const now = scene.emulator.now();
    const threads = ([0, 1] as const).map((index) => ({
      identity: seededIdentity(now, index),
      dispute: openGame(traces, now),
    }));
    const utxos = await seedThreads(scene, scene.game.address, threads);
    const from = scene.emulator.now();
    const reveals = threads.map(({ dispute }) =>
      nextReveal(traces, dispute, from),
    );
    const successor = (index: 0 | 1) =>
      threadDatum(scene, threads[index]!.identity, reveals[index]!.next);
    expect(successor(0)).not.toBe(successor(1));
    const both = (outputIndices: readonly [bigint, bigint]): Move => {
      const moves = ([0, 1] as const).map((index) =>
        gameMove(
          scene,
          { utxo: utxos[index]!, identity: threads[index]!.identity },
          reveals[index]!.action,
          reveals[index]!.next,
          {
            ...options(from, { funding: walletPays }),
            outputIndex: outputIndices[index],
          },
        ),
      );
      return {
        ...moves[0]!,
        spends: moves.flatMap((move) => move.spends),
        outputs: moves.flatMap((move) => move.outputs),
      };
    };
    await expectOnchainRefusal(() => buildMove(scene, both([1n, 0n])));
    const hash = await submitMove(scene, both([0n, 1n]));
    expect((await createdAt(scene, hash, 0)).datum).toBe(successor(0));
    expect((await createdAt(scene, hash, 1)).datum).toBe(successor(1));
  });
});
