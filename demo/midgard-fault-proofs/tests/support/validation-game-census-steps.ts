import {
  awardFinalizeMove,
  awardTerminalPaddingMove,
  gameMove,
  prepareResolutionMove,
  timeoutFinalizeMove,
} from "./validation-game-moves.js";
import {
  awardThread,
  boundaryThread,
  enterResolution,
  nextReveal,
  openThread,
  options,
  passOperatorDeadline,
  playToOneStep,
  reveal,
  timedOutThread,
} from "./validation-game-play.js";
import { setUpGameScene, TRACE_TITLES } from "./validation-game-scene.js";
import {
  divergentTraces,
  terminalPaddingTraces,
} from "./validation-game-traces.js";
import { type CensusStep } from "./validation-trace-census.js";

/** The game, boundary, award and timeout steps, each with an honest move. */
export const GAME_CENSUS_STEPS: readonly CensusStep[] = [
  {
    name: "game RevealOperator",
    title: TRACE_TITLES.game,
    mover: "anyone",
    prepare: async () => {
      const scene = await setUpGameScene();
      const traces = divergentTraces();
      const opened = await openThread(scene, traces);
      return {
        scene,
        move: (shape) => {
          const from = scene.emulator.now();
          const { action, next } = nextReveal(traces, opened.dispute, from);
          return gameMove(
            scene,
            opened.utxo,
            action,
            next,
            options(from, shape),
          );
        },
      };
    },
  },
  {
    name: "game RevealChallenger",
    title: TRACE_TITLES.game,
    mover: "anyone",
    prepare: async () => {
      const scene = await setUpGameScene();
      const traces = divergentTraces();
      const revealed = await reveal(
        scene,
        traces,
        await openThread(scene, traces),
      );
      return {
        scene,
        move: (shape) => {
          const from = scene.emulator.now();
          const { action, next } = nextReveal(traces, revealed.dispute, from);
          return gameMove(
            scene,
            revealed.utxo,
            action,
            next,
            options(from, shape),
          );
        },
      };
    },
  },
  {
    name: "game EnterResolution",
    title: TRACE_TITLES.game,
    mover: "prover",
    prepare: async () => {
      const scene = await setUpGameScene();
      const traces = divergentTraces();
      const ready = await playToOneStep(
        scene,
        traces,
        await openThread(scene, traces),
      );
      return {
        scene,
        move: (shape) => enterResolution(scene, ready, shape).move,
      };
    },
  },
  {
    name: "game EnterChallengerTimeout",
    title: TRACE_TITLES.game,
    mover: "anyone",
    prepare: async () => {
      const scene = await setUpGameScene();
      const opened = await openThread(scene, divergentTraces());
      passOperatorDeadline(scene, opened);
      return {
        scene,
        move: (shape) =>
          gameMove(
            scene,
            opened.utxo,
            { kind: "EnterChallengerTimeout" },
            opened.dispute,
            options(scene.emulator.now(), shape),
          ),
      };
    },
  },
  {
    name: "boundary PrepareResolution",
    title: TRACE_TITLES.boundary,
    mover: "prover",
    prepare: async () => {
      const scene = await setUpGameScene();
      const traces = divergentTraces();
      const boundary = await boundaryThread(scene, traces);
      return {
        scene,
        move: (shape) =>
          prepareResolutionMove(
            scene,
            boundary.utxo,
            boundary.dispute,
            traces.agreedLowState,
            options(scene.emulator.now(), shape),
          ),
      };
    },
  },
  {
    name: "boundary AwardTerminalPadding",
    title: TRACE_TITLES.boundary,
    mover: "prover",
    prepare: async () => {
      const scene = await setUpGameScene();
      const traces = terminalPaddingTraces();
      const boundary = await boundaryThread(scene, traces);
      return {
        scene,
        move: (shape) =>
          awardTerminalPaddingMove(
            scene,
            boundary.utxo,
            traces.agreedLowState,
            options(scene.emulator.now(), shape),
          ),
      };
    },
  },
  {
    name: "timeout ChallengerTimeout",
    title: TRACE_TITLES.timeout,
    mover: "anyone",
    prepare: async () => {
      const scene = await setUpGameScene();
      const timedOut = await timedOutThread(scene);
      return {
        scene,
        move: ({ funding }) =>
          timeoutFinalizeMove(scene, timedOut, funding, scene.emulator.now()),
      };
    },
  },
  {
    name: "award finalize",
    title: TRACE_TITLES.award,
    mover: "anyone",
    prepare: async () => {
      const scene = await setUpGameScene();
      const awarded = await awardThread(scene);
      return {
        scene,
        move: ({ funding }) =>
          awardFinalizeMove(scene, awarded, funding, scene.emulator.now()),
      };
    },
  },
];
