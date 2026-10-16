/**
 * The validation case at the consensus maximum: a case opened over a deep
 * trace, one defense, and the helpers that play and measure the thread's
 * moves.
 */
import { type MidgardValidationDispute } from "@al-ft/midgard-core";
import { type UTxO } from "@lucid-evolution/lucid";

import { type measureCompleteSignedTransaction } from "./submit-init-emulator-shared.js";
import {
  defend,
  type Defender,
  type OpenCase,
  openCase,
  registerDefender,
} from "./validation-case-moves.js";
import {
  type CaseWorld,
  setUpCaseWorld,
  submitSigned,
} from "./validation-case-scene.js";
import { type DeepGameTraces } from "./validation-game-deep-traces.js";
import { gameMove } from "./validation-game-moves.js";
import { nextReveal, options } from "./validation-game-play.js";
import {
  buildMove,
  createdAt,
  type GameScene,
  type Thread,
} from "./validation-game-scene.js";

export const lovelaceOf = (utxo: UTxO) => BigInt(utxo.assets.lovelace ?? 0n);

export type Budget = ReturnType<typeof measureCompleteSignedTransaction>;

export type MaxShape = {
  readonly world: CaseWorld;
  readonly opened: OpenCase;
  readonly thread: Thread;
  readonly defender: Defender;
  readonly budgets: { readonly open: Budget; readonly defend: Budget };
};

/** A case over `traces` with one defense, made right after the open. */
export const setUpMaxShape = async (
  traces: DeepGameTraces,
): Promise<MaxShape> => {
  const world = await setUpCaseWorld();
  const { budget: open, ...opened } = await openCase(world, traces);
  const registered = await registerDefender(world);
  const defended = await defend(world, opened, registered);
  return {
    world,
    opened: defended.opened,
    thread: defended.thread,
    defender: defended.defender,
    budgets: { open, defend: defended.budget },
  };
};

/** The move with the larger CPU budget. */
const heavier = (left: Budget | undefined, right: Budget) =>
  left === undefined || right.executionSteps > left.executionSteps
    ? right
    : left;

/**
 * Plays honest reveals by a third party, each paying the capped fee from
 * the thread, until `stop` holds for the thread and its next honest
 * successor. `wait` runs before each move and may advance the clock.
 */
export const playReveals = async (
  scene: GameScene,
  traces: DeepGameTraces,
  opened: Thread,
  stop: (
    current: MidgardValidationDispute,
    next: MidgardValidationDispute,
  ) => boolean,
  wait: (dispute: MidgardValidationDispute) => void = () => undefined,
): Promise<{ readonly thread: Thread; readonly heaviest?: Budget }> => {
  let thread = opened;
  let heaviest: Budget | undefined;
  for (;;) {
    wait(thread.dispute);
    const from = scene.emulator.now();
    const { action, next } = nextReveal(traces, thread.dispute, from);
    if (stop(thread.dispute, next)) return { thread, heaviest };
    const built = await buildMove(
      scene,
      gameMove(scene, thread, action, next, options(from)),
    );
    const { hash, budget } = await submitSigned(scene, built);
    heaviest = heavier(heaviest, budget);
    thread = {
      ...thread,
      utxo: await createdAt(scene, hash, 0),
      dispute: next,
    };
    if (next.turn.type === "readyForOneStep") return { thread, heaviest };
  }
};

/** Stops before the defender's `count`-th reveal. */
export const beforeDefenderReveal = (count: number) => {
  let seen = 0;
  return (current: MidgardValidationDispute) =>
    current.turn.type === "awaitingOperator" && (seen += 1) === count;
};

/**
 * Makes each side's first move as late as its clock allows while keeping one
 * slot for the rest: a prompt move lands one slot-step after the previous
 * move's upper bound and is charged that millisecond.
 */
export const spendWholeClock =
  (scene: GameScene) => (dispute: MidgardValidationDispute) => {
    if (dispute.round !== 0) return;
    const clock =
      dispute.turn.type === "awaitingOperator"
        ? dispute.operatorClockMs
        : dispute.challengerClockMs;
    const slots =
      Math.floor(
        (dispute.turnStartedAt + clock - scene.emulator.now()) / 1000,
      ) - 1;
    if (slots > 0) scene.emulator.awaitSlot(slots);
  };

export const report = (
  label: string,
  depth: number,
  budget: Budget | undefined,
) =>
  console.log(
    `max-shape ${label} depth=${depth} bytes=${budget?.completeSignedBytes} ` +
      `mem=${budget?.executionMemory} steps=${budget?.executionSteps}`,
  );
