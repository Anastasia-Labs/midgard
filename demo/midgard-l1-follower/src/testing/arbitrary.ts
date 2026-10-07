import fc from "fast-check";

import {
  FORK_SHAPES,
  type ForkEpisode,
  type ForkScenario,
  type ForkShape,
} from "./episodes.js";

/** A random fork episode whose rollback stays within `k`. */
export const forkEpisodeArbitrary = (k: number): fc.Arbitrary<ForkEpisode> =>
  fc.record({
    shape: fc.constantFrom(...FORK_SHAPES),
    depth: fc.integer({ min: 1, max: k }),
    extra: fc.integer({ min: 0, max: 2 }),
    landAt: fc.nat({ max: 2 * k }),
    variant: fc.nat({ max: 5 }),
    lead: fc.integer({ min: 0, max: 2 }),
  });

/** Random scenarios: several episodes back to back over seeded filler traffic. */
export const forkScenarioArbitrary = (
  k: number,
  maxEpisodes = 4,
): fc.Arbitrary<ForkScenario> =>
  fc.record({
    seed: fc.nat(),
    episodes: fc.array(forkEpisodeArbitrary(k), {
      minLength: 1,
      maxLength: maxEpisodes,
    }),
  });

const VARIANTS: Readonly<Record<ForkShape, readonly number[]>> = {
  reland: [0],
  never_reland: [0, 1],
  changed_valid_to: [0],
  new_fork_only: [0],
  phase2_failed: [0, 1, 2],
};

export type NamedScenario = Readonly<{ name: string; scenario: ForkScenario }>;

/**
 * The deterministic corpus: every shape and variant at depth 1, about k/2
 * and k, landing first and last on the new branch, plus one scenario that
 * chains every shape. Projection tickets run their cases over it.
 */
export const forkCorpus = (k: number): NamedScenario[] => {
  const depths = [...new Set([1, Math.ceil(k / 2), k])];
  const corpus: NamedScenario[] = [];
  let seed = 1;
  for (const shape of FORK_SHAPES)
    for (const variant of VARIANTS[shape])
      for (const depth of depths) {
        const landAt = depth % 2 === 0 ? 0 : depth;
        corpus.push({
          name: `${shape} v${variant} depth ${depth}`,
          scenario: {
            seed: seed++,
            episodes: [{ shape, depth, extra: 1, landAt, variant, lead: 1 }],
          },
        });
      }
  corpus.push({
    name: "every shape in sequence",
    scenario: {
      seed: seed++,
      episodes: FORK_SHAPES.flatMap((shape, index) =>
        VARIANTS[shape].map((variant) => ({
          shape,
          depth: 1 + ((index + variant) % k),
          extra: variant % 2,
          landAt: index,
          variant,
          lead: 0,
        })),
      ),
    },
  });
  return corpus;
};
