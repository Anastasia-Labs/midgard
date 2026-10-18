import { expectOnchainRefusal } from "./submit-init-emulator-shared.js";
import {
  buildMove,
  cap,
  type Funding,
  type GameScene,
  type Move,
  submitMove,
  threadPays,
  walletPays,
} from "./validation-game-scene.js";

/** Who may make a validation-trace move. */
export type Mover = "anyone" | "prover";

export type MoveShape = {
  readonly funding: Funding;
  /** Whether the transaction carries the fraud prover's signature. */
  readonly signed: boolean;
};

/**
 * One validation-trace step: a validator, a move through it and who may make
 * it. `prepare` sets up a fresh scene with the step's input thread in place
 * and returns the honest move in any funding and signing shape.
 */
export type CensusStep = {
  readonly name: string;
  /** The blueprint title of the spend handler the move runs. */
  readonly title: string;
  readonly mover: Mover;
  readonly prepare: () => Promise<{
    readonly scene: GameScene;
    readonly move: (shape: MoveShape) => Move;
  }>;
};

export type CensusVariant = {
  readonly name: string;
  readonly run: () => Promise<void>;
};

const refused = (scene: GameScene, move: Move) =>
  expectOnchainRefusal(() => buildMove(scene, move));

/**
 * The census of one step, each variant on a fresh thread:
 * (a) a lone thread input pays a fee up to the cap, and no more;
 * (b) beside another input, the thread may not lose value, and may keep it;
 * (c) a prover-only move needs the prover's signature; any other move is
 *     open to a third party.
 */
export const censusVariants = (step: CensusStep): readonly CensusVariant[] => [
  {
    name: `${step.name}: a lone thread input pays the fee, up to the cap`,
    run: async () => {
      const { scene, move } = await step.prepare();
      await refused(
        scene,
        move({ funding: { kind: "thread", fee: cap + 1n }, signed: true }),
      );
      await submitMove(scene, move({ funding: threadPays, signed: true }));
    },
  },
  {
    name: `${step.name}: beside another input the thread keeps its value`,
    run: async () => {
      const { scene, move } = await step.prepare();
      await refused(
        scene,
        move({ funding: { kind: "wallet", skim: 1n }, signed: true }),
      );
      await submitMove(scene, move({ funding: walletPays, signed: true }));
    },
  },
  step.mover === "prover"
    ? {
        name: `${step.name}: only the fraud prover may move`,
        run: async () => {
          const { scene, move } = await step.prepare();
          await refused(scene, move({ funding: threadPays, signed: false }));
          await submitMove(scene, move({ funding: threadPays, signed: true }));
        },
      }
    : {
        name: `${step.name}: a third party may move`,
        run: async () => {
          const { scene, move } = await step.prepare();
          await submitMove(scene, move({ funding: threadPays, signed: false }));
        },
      },
];

const TRACE_PREFIX = "fraud_proofs/validation_trace/";

/** Every validation-trace spend handler in `blueprint`, sorted. */
export const validationTraceSpends = (blueprint: {
  readonly validators: readonly { readonly title: string }[];
}): readonly string[] =>
  blueprint.validators
    .map((validator) => validator.title)
    .filter(
      (title) => title.startsWith(TRACE_PREFIX) && title.endsWith(".spend"),
    )
    .sort();

/**
 * The validation-trace spend handlers in `blueprint` that no census step
 * covers. The release gate holds this to empty; this lane only guarantees
 * that every step it built is covered.
 */
export const uncoveredValidationTraceSpends = (
  blueprint: { readonly validators: readonly { readonly title: string }[] },
  steps: readonly CensusStep[],
): readonly string[] => {
  const covered = new Set(steps.map((step) => step.title));
  return validationTraceSpends(blueprint).filter(
    (title) => !covered.has(title),
  );
};
