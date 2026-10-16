import { type MidgardValidationDispute } from "@al-ft/midgard-core";
import { type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { expectOnchainRefusal } from "./support/submit-init-emulator-shared.js";
import {
  awardFinalizeMove,
  awardTerminalPaddingMove,
  gameMove,
  prepareResolutionMove,
  timeoutFinalizeMove,
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
  fraudProofDatum,
  fraudProofOutputLovelace,
  type Funding,
  gameDatum,
  type GameScene,
  maxRangeMs,
  type Move,
  moveWindow,
  seedThreads,
  setUpGameScene,
  sideClockMs,
  submitMove,
  tagOf,
  threadLovelace,
  threadPays,
  walletPays,
} from "./support/validation-game-scene.js";
import {
  divergentTraces,
  neitherTerminalPaddingTraces,
  openGame,
  operatorTerminalPaddingTraces,
  terminalPaddingTraces,
} from "./support/validation-game-traces.js";

const lovelaceOf = (utxo: UTxO) => BigInt(utxo.assets.lovelace ?? 0n);

/** The finalize outputs: the remainder to the prover, then the fraud proof. */
const expectConviction = async (
  scene: GameScene,
  hash: string,
  convicted: UTxO,
  fee: bigint,
) => {
  const remainder = await createdAt(scene, hash, 0);
  const fraudProof = await createdAt(scene, hash, 1);
  expect(remainder.address).toBe(scene.proverAddress);
  expect(remainder.datum).toBe(tagOf(convicted));
  expect(lovelaceOf(remainder)).toBe(
    lovelaceOf(convicted) - fraudProofOutputLovelace - fee,
  );
  expect(fraudProof.address).toBe(scene.fraudProofAddress);
  expect(fraudProof.datum).toBe(fraudProofDatum(scene));
  expect(fraudProof.assets[scene.fraudProofUnit]).toBe(1n);
  expect(lovelaceOf(fraudProof)).toBe(fraudProofOutputLovelace);
};

describe("validation game on the emulator", () => {
  it("lets a third party make both reveals, each paying a capped fee from the thread", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const opened = await openThread(scene, traces);
    const revealed = await reveal(scene, traces, opened);
    expect(revealed.dispute.turn.type).toBe("awaitingChallenger");
    expect(revealed.utxo.address).toBe(scene.game.address);
    expect(revealed.utxo.datum).toBe(gameDatum(scene, revealed.dispute));
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
      return gameMove(
        scene,
        opened.utxo,
        action,
        next,
        options(from, { funding }),
      );
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
    expect(boundary.datum).toBe(gameDatum(scene, entered.next));

    const prepare = (signed: boolean) =>
      prepareResolutionMove(
        scene,
        boundary,
        entered.next,
        traces.agreedLowState,
        options(scene.emulator.now(), { signed }),
      );
    await expectOnchainRefusal(() => buildMove(scene, prepare(false)));
    const preparedHash = await submitMove(scene, prepare(true));
    const resolver = await createdAt(scene, preparedHash, 0);
    expect(resolver.address).toBe(prepare(true).outputs[0]!.address);
    expect(resolver.datum).toBe(prepare(true).outputs[0]!.datum);
  });

  it("awards terminal padding only with the fraud prover's signature, and anyone finalizes the award in full", async () => {
    const scene = await setUpGameScene();
    const traces = terminalPaddingTraces();
    const ready = await playToOneStep(
      scene,
      traces,
      await openThread(scene, traces),
    );
    const entered = enterResolution(scene, ready, { signed: true });
    const boundary = await createdAt(
      scene,
      await submitMove(scene, entered.move),
      0,
    );
    const award = (signed: boolean) =>
      awardTerminalPaddingMove(
        scene,
        boundary,
        traces.agreedLowState,
        options(scene.emulator.now(), { signed }),
      );
    await expectOnchainRefusal(() => buildMove(scene, award(false)));
    const awarded = await createdAt(
      scene,
      await submitMove(scene, award(true)),
      0,
    );
    expect(awarded.address).toBe(scene.award.address);

    // Beside a wallet input, the prover's remainder may not come up short.
    await expectOnchainRefusal(() =>
      buildMove(
        scene,
        awardFinalizeMove(
          scene,
          awarded,
          { kind: "wallet", skim: 1n },
          scene.emulator.now(),
        ),
      ),
    );
    const finalizeHash = await submitMove(
      scene,
      awardFinalizeMove(scene, awarded, threadPays, scene.emulator.now()),
    );
    await expectConviction(scene, finalizeHash, awarded, cap);
  });

  it("refuses the padding award when the challenger's step after the agreed Terminal state is not that state", async () => {
    for (const traces of [
      operatorTerminalPaddingTraces(),
      neitherTerminalPaddingTraces(),
    ]) {
      const scene = await setUpGameScene();
      const ready = await playToOneStep(
        scene,
        traces,
        await openThread(scene, traces),
      );
      const entered = enterResolution(scene, ready, { signed: true });
      const boundary = await createdAt(
        scene,
        await submitMove(scene, entered.move),
        0,
      );
      await expectOnchainRefusal(() =>
        buildMove(
          scene,
          awardTerminalPaddingMove(
            scene,
            boundary,
            traces.agreedLowState,
            options(scene.emulator.now(), { signed: true }),
          ),
        ),
      );
    }
  });

  it("refuses a challenger timeout until the operator's clock has run out, then anyone finalizes it for at most the cap", async () => {
    const scene = await setUpGameScene();
    const traces = divergentTraces();
    const opened = await openThread(scene, traces);
    const toTimeout = (from: number) =>
      gameMove(
        scene,
        opened.utxo,
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
    expect(timedOut.datum).toBe(gameDatum(scene, opened.dispute));

    await expectOnchainRefusal(() =>
      buildMove(
        scene,
        timeoutFinalizeMove(
          scene,
          timedOut,
          { kind: "thread", fee: cap + 1n },
          scene.emulator.now(),
        ),
      ),
    );
    const finalizeHash = await submitMove(
      scene,
      timeoutFinalizeMove(scene, timedOut, threadPays, scene.emulator.now()),
    );
    await expectConviction(scene, finalizeHash, timedOut, cap);
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
      gameMove(scene, opened.utxo, action, dispute, options(from));
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
          revealed.utxo,
          action,
          { ...next, challengerClockMs: sideClockMs - 1 },
          options(early),
        ),
      ),
    );
    await submitMove(
      scene,
      gameMove(scene, revealed.utxo, action, next, options(early)),
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
      gameMove(scene, revealed.utxo, action, next, options(from)),
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
          opened.utxo,
          action,
          next,
          options(from, { width: maxRangeMs + 1000 }),
        ),
      ),
    );
    await submitMove(
      scene,
      gameMove(scene, opened.utxo, action, next, options(from)),
    );
  });

  it("moves two threads under one CT name only when each redeemer names its own successor", async () => {
    const scene = await setUpGameScene();
    const first = divergentTraces();
    const second = terminalPaddingTraces();
    const now = scene.emulator.now();
    const disputes = [openGame(first, now), openGame(second, now)] as const;
    const utxos = await seedThreads(
      scene,
      scene.game.address,
      disputes.map((dispute) => gameDatum(scene, dispute)),
    );
    const from = scene.emulator.now();
    const reveals = [
      nextReveal(first, disputes[0], from),
      nextReveal(second, disputes[1], from),
    ];
    expect(gameDatum(scene, reveals[0]!.next)).not.toBe(
      gameDatum(scene, reveals[1]!.next),
    );
    // Both successors carry the same CT unit, so only the redeemer's output
    // index tells them apart.
    const both = (outputIndices: readonly [bigint, bigint]): Move => {
      const moves = ([0, 1] as const).map((index) =>
        gameMove(
          scene,
          utxos[index]!,
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
    expect((await createdAt(scene, hash, 0)).datum).toBe(
      gameDatum(scene, reveals[0]!.next),
    );
    expect((await createdAt(scene, hash, 1)).datum).toBe(
      gameDatum(scene, reveals[1]!.next),
    );
  });
});
