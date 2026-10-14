import {
  enterMidgardValidationDisputeResolution,
  type MidgardValidationDispute,
} from "@al-ft/midgard-core";
import { type UTxO } from "@lucid-evolution/lucid";

import {
  awardTerminalPaddingMove,
  type GameAction,
  gameMove,
  type MoveOptions,
} from "./validation-game-moves.js";
import {
  createdAt,
  type Funding,
  gameDatum,
  type GameScene,
  maxRangeMs,
  moveWindow,
  seedThreads,
  submitMove,
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
  terminalPaddingTraces,
} from "./validation-game-traces.js";

export type Thread = {
  readonly utxo: UTxO;
  readonly dispute: MidgardValidationDispute;
};

/** A game thread opened at the scene's current time. */
export const openThread = async (
  scene: GameScene,
  traces: GameTraces,
): Promise<Thread> => {
  const dispute = openGame(traces, scene.emulator.now());
  const [utxo] = await seedThreads(scene, scene.game.address, [
    gameDatum(scene, dispute),
  ]);
  return { utxo: utxo!, dispute };
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
    gameMove(scene, thread.utxo, action, next, options(from)),
  );
  return { utxo: await createdAt(scene, hash, 0), dispute: next };
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
    thread.utxo,
    { kind: "EnterResolution" },
    next,
    options(from, shape),
  );
  return { move, next };
};

/** A thread at the boundary validator, after a signed EnterResolution. */
export const boundaryThread = async (
  scene: GameScene,
  traces: GameTraces,
): Promise<Thread> => {
  const ready = await playToOneStep(
    scene,
    traces,
    await openThread(scene, traces),
  );
  const entered = enterResolution(scene, ready, { signed: true });
  const hash = await submitMove(scene, entered.move);
  return { utxo: await createdAt(scene, hash, 0), dispute: entered.next };
};

/** Waits until the operator's clock has run out on `thread`. */
export const passOperatorDeadline = (scene: GameScene, thread: Thread) => {
  const deadline =
    thread.dispute.turnStartedAt + thread.dispute.operatorClockMs;
  scene.emulator.awaitSlot((deadline - scene.emulator.now()) / 1000 + 1);
};

/** A thread at the timeout validator, after the operator's clock ran out. */
export const timedOutThread = async (scene: GameScene): Promise<UTxO> => {
  const traces = divergentTraces();
  const opened = await openThread(scene, traces);
  passOperatorDeadline(scene, opened);
  const from = scene.emulator.now();
  const hash = await submitMove(
    scene,
    gameMove(
      scene,
      opened.utxo,
      { kind: "EnterChallengerTimeout" },
      opened.dispute,
      options(from),
    ),
  );
  return createdAt(scene, hash, 0);
};

/** A thread at the award validator, after a signed AwardTerminalPadding. */
export const awardThread = async (scene: GameScene): Promise<UTxO> => {
  const traces = terminalPaddingTraces();
  const boundary = await boundaryThread(scene, traces);
  const hash = await submitMove(
    scene,
    awardTerminalPaddingMove(
      scene,
      boundary.utxo,
      traces.agreedLowState,
      options(scene.emulator.now(), { signed: true }),
    ),
  );
  return createdAt(scene, hash, 0);
};
