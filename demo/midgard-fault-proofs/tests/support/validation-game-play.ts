import {
  enterMidgardValidationDisputeResolution,
  type MidgardValidationDispute,
} from "@al-ft/midgard-core";

import {
  type GameAction,
  gameMove,
  type MoveOptions,
} from "./validation-game-moves.js";
import {
  createdAt,
  type Funding,
  type GameScene,
  maxRangeMs,
  moveWindow,
  type SeededGameScene,
  seededIdentity,
  seedThreads,
  submitMove,
  type Thread,
  threadPays,
} from "./validation-game-scene.js";
import {
  challengerProof,
  divergentTraces,
  type GameTraces,
  openGame,
  operatorProof,
  revealChallenger,
  revealOperator,
} from "./validation-game-traces.js";

/** A game thread opened at the scene's current time. */
export const openThread = async (
  scene: SeededGameScene,
  traces: GameTraces,
): Promise<Thread> => {
  const now = scene.emulator.now();
  const dispute = openGame(traces, now);
  const identity = seededIdentity(now);
  const [utxo] = await seedThreads(scene, scene.game.address, [
    { identity, dispute },
  ]);
  return { utxo: utxo!, dispute, identity };
};

export const options = (
  from: number,
  {
    funding = threadPays,
    signed = false,
    width = maxRangeMs,
  }: {
    readonly funding?: Funding;
    readonly signed?: boolean;
    readonly width?: number;
  } = {},
): MoveOptions => {
  const window = moveWindow(from, width);
  return {
    funding,
    signed,
    validFrom: window.validFrom,
    validTo: window.validTo,
  };
};

/** The honest next state and redeemer action for the side to move. */
export const nextReveal = (
  traces: GameTraces,
  dispute: MidgardValidationDispute,
  from: number,
): { readonly action: GameAction; readonly next: MidgardValidationDispute } => {
  const time = moveWindow(from);
  return dispute.turn.type === "awaitingOperator"
    ? {
        action: {
          kind: "RevealOperator",
          proof: operatorProof(traces, dispute),
        },
        next: revealOperator(traces, dispute, time),
      }
    : {
        action: {
          kind: "RevealChallenger",
          proof: challengerProof(traces, dispute),
        },
        next: revealChallenger(traces, dispute, time),
      };
};

/** One honest reveal, submitted by the third-party wallet. */
export const reveal = async (
  scene: GameScene,
  traces: GameTraces,
  thread: Thread,
): Promise<Thread> => {
  const from = scene.emulator.now();
  const { action, next } = nextReveal(traces, thread.dispute, from);
  const hash = await submitMove(
    scene,
    gameMove(scene, thread, action, next, options(from)),
  );
  return { ...thread, utxo: await createdAt(scene, hash, 0), dispute: next };
};

export const playToOneStep = async (
  scene: GameScene,
  traces: GameTraces,
  opened: Thread,
): Promise<Thread> => {
  let thread = opened;
  while (thread.dispute.turn.type !== "readyForOneStep") {
    thread = await reveal(scene, traces, thread);
  }
  return thread;
};

/** EnterResolution from a thread at ReadyForOneStep, made now. */
export const enterResolution = (
  scene: GameScene,
  thread: Thread,
  shape: { readonly funding?: Funding; readonly signed: boolean },
) => {
  const from = scene.emulator.now();
  const next = enterMidgardValidationDisputeResolution({
    dispute: thread.dispute,
    ...moveWindow(from),
  });
  const move = gameMove(
    scene,
    thread,
    { kind: "EnterResolution" },
    next,
    options(from, shape),
  );
  return { move, next };
};

/** A thread at the boundary validator, after a signed EnterResolution. */
export const boundaryThread = async (
  scene: SeededGameScene,
  traces: GameTraces,
): Promise<Thread> => {
  const ready = await playToOneStep(
    scene,
    traces,
    await openThread(scene, traces),
  );
  const entered = enterResolution(scene, ready, { signed: true });
  const hash = await submitMove(scene, entered.move);
  return {
    ...ready,
    utxo: await createdAt(scene, hash, 0),
    dispute: entered.next,
  };
};

/** Waits whole slots until `deadline` has passed. */
export const passDeadline = (scene: GameScene, deadline: number) => {
  scene.emulator.awaitSlot(
    Math.max(0, Math.ceil((deadline - scene.emulator.now()) / 1000)) + 1,
  );
};

/** Waits until the operator's clock has run out on `thread`. */
export const passOperatorDeadline = (scene: GameScene, thread: Thread) =>
  passDeadline(
    scene,
    thread.dispute.turnStartedAt + thread.dispute.operatorClockMs,
  );

/** The move that hands a thread whose side to move ran out to the timeout. */
export const timeoutMove = (scene: GameScene, thread: Thread) =>
  gameMove(
    scene,
    thread,
    { kind: "EnterChallengerTimeout" },
    thread.dispute,
    options(scene.emulator.now()),
  );

/** A thread at the timeout validator, after the operator's clock ran out. */
export const timedOutThread = async (scene: SeededGameScene) => {
  const opened = await openThread(scene, divergentTraces());
  passOperatorDeadline(scene, opened);
  const hash = await submitMove(scene, timeoutMove(scene, opened));
  return { ...opened, utxo: await createdAt(scene, hash, 0) };
};
