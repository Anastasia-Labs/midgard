import fc from "fast-check";

import {
  FORK_SHAPES,
  type ForkEpisode,
  type ForkScenario,
  type ForkShape,
} from "./episodes.js";

/** Whether generated episodes may prune (`ForkEpisode.prune`); default true. */
export type ForkPruneOption = Readonly<{ prune?: boolean }>;

/** A random fork episode whose rollback stays within `k`. */
export const forkEpisodeArbitrary = (
  k: number,
  options: ForkPruneOption = {},
): fc.Arbitrary<ForkEpisode> =>
  fc.record({
    shape: fc.constantFrom(...FORK_SHAPES),
    depth: fc.integer({ min: 1, max: k }),
    extra: fc.integer({ min: 0, max: 2 }),
    landAt: fc.nat({ max: 2 * k }),
    variant: fc.nat({ max: 5 }),
    lead: fc.integer({ min: 0, max: 2 }),
    prune: options.prune === false ? fc.constant(false) : fc.boolean(),
  });

/** Random scenarios: several episodes back to back over seeded filler traffic. */
export const forkScenarioArbitrary = (
  k: number,
  maxEpisodes = 4,
  options: ForkPruneOption = {},
): fc.Arbitrary<ForkScenario> =>
  fc.record({
    seed: fc.nat(),
    episodes: fc.array(forkEpisodeArbitrary(k, options), {
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
 * chains every shape. Then the prune cases: every shape with a prune before
 * and after a depth-k rollback over a 2k-block lead (rows pruned, then the
 * rewind drops the boundary below them), and the chain of every shape again
 * with every episode pruned (left out with `prune: false`). Projection
 * tickets run their cases over it.
 */
export const forkCorpus = (
  k: number,
  options: ForkPruneOption = {},
): NamedScenario[] => {
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
  if (options.prune === false) return corpus;
  for (const shape of FORK_SHAPES)
    corpus.push({
      name: `${shape} pruned around a depth-${k} rollback`,
      scenario: {
        seed: seed++,
        episodes: [
          { shape, depth: k, extra: 1, landAt: 0, variant: 0, lead: 2 * k },
          {
            shape,
            depth: k,
            extra: 1,
            landAt: k,
            variant: 1,
            lead: 1,
            prune: true,
          },
        ],
      },
    });
  corpus.push({
    name: "every shape in sequence, pruned",
    scenario: {
      seed: seed++,
      episodes: FORK_SHAPES.flatMap((shape, index) =>
        VARIANTS[shape].map((variant) => ({
          shape,
          depth: k - ((index + variant) % 2),
          extra: variant % 2,
          landAt: index,
          variant,
          lead: 2,
          prune: true,
        })),
      ),
    },
  });
  return corpus;
};
