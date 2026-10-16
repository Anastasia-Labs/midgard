import { gameMove, prepareResolutionMove } from "./validation-game-moves.js";
import {
  boundaryThread,
  enterResolution,
  nextReveal,
  openThread,
  options,
  passOperatorDeadline,
  playToOneStep,
  reveal,
} from "./validation-game-play.js";
import { setUpGameScene, TRACE_TITLES } from "./validation-game-scene.js";
import { divergentTraces } from "./validation-game-traces.js";
import { type CensusStep } from "./validation-trace-census.js";

/**
 * The game and boundary steps, each with an honest move. The timeout and
 * award spends close a thread into its case, so the case tests cover them.
 */
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
          return gameMove(scene, opened, action, next, options(from, shape));
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
          return gameMove(scene, revealed, action, next, options(from, shape));
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
            opened,
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
            boundary,
            traces.agreedLowState,
            options(scene.emulator.now(), shape),
          ),
      };
    },
  },
];
