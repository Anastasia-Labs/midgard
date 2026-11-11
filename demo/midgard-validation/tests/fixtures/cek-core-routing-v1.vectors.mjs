/**
 * The CEK core routing the TypeScript twins give, as rows the Aiken routing
 * (`cek_semantic_chain.core_group` and `semantic_group`) must give too:
 *
 * - each machine mode from compute to one past the semantic builtin mode,
 *   with each witness constructor from 0 to one past the last arm, and for
 *   arms 30 and 38 in builtin mode each builtin tag from 0 to one past the
 *   last; a pair the twin routes is a routed row, and a pair it refuses is a
 *   refused row when it is in builtin mode or borders a routed arm of its
 *   mode, or is the first arm of a mode that routes none;
 * - each builtin tag from -1 to one past the last, routed by its semantics
 *   group, and refused where it borders a routed tag.
 *
 * `scripts/generate-cek-core-routing-v1-goldens.mjs` renders the rows as an
 * Aiken module, and `tests/cek-core-routing-v1.golden.test.ts` checks the
 * twins against them. This module holds data only and takes the twins as
 * arguments, so that each caller passes the build it runs.
 */

/** The modes swept: compute (0) to the semantic builtin mode (8), and one
 * past it. */
const MODES = Array.from({ length: 10 }, (_, mode) => mode);

/** The builtin mode, in which every arm is swept. */
const BUILTIN_MODE = 3;

/** The witness constructors swept: the arms 0 to 41, and one past them. */
const ARMS = Array.from({ length: 43 }, (_, arm) => arm);

/** The arms whose witness carries the builtin tag the mode routes by. */
const TAGGED_ARMS = new Set([30, 38]);

/** The builtin tags swept: 0 to 100, and one past them. */
const TAGS = Array.from({ length: 102 }, (_, tag) => tag);

/** The tags `semantic_group` is swept over: one below the first, then
 * `TAGS`. */
const SEMANTIC_TAGS = [-1, ...TAGS];

const borders = (routes, at) => routes(at - 1) || routes(at + 1);

/**
 * The rows, given `coreGroup(mode, arm, tag)` (`cekCoreGroup`) and
 * `semanticGroup(tag)` (`midgardCekSemanticGroup`), each taking and
 * returning bigints or numbers as its twin does and giving undefined or null
 * for a refusal.
 */
export const routingRows = ({ coreGroup, semanticGroup }) => {
  const groupOf = (mode, arm, tag) => {
    const group = coreGroup(BigInt(mode), arm, BigInt(tag));
    return group === undefined ? null : Number(group);
  };
  const routedCore = [];
  const refusedCore = [];
  for (const mode of MODES) {
    const routesArm = (arm) => groupOf(mode, arm, 0) !== null;
    const routesNone = !ARMS.some(routesArm);
    for (const arm of ARMS) {
      const tags = mode === BUILTIN_MODE && TAGGED_ARMS.has(arm) ? TAGS : [0];
      for (const tag of tags) {
        const group = groupOf(mode, arm, tag);
        if (group !== null) routedCore.push({ mode, arm, tag, group });
        else if (
          mode === BUILTIN_MODE ||
          borders(routesArm, arm) ||
          (routesNone && arm === 0)
        )
          refusedCore.push({ mode, arm, tag });
      }
    }
  }
  const semanticOf = (tag) => {
    const group = semanticGroup(BigInt(tag));
    return group === null ? null : Number(group);
  };
  const routesTag = (tag) => semanticOf(tag) !== null;
  return {
    coreGroup: { routed: routedCore, refused: refusedCore },
    semanticGroup: {
      routed: SEMANTIC_TAGS.filter(routesTag).map((tag) => ({
        tag,
        group: semanticOf(tag),
      })),
      refused: SEMANTIC_TAGS.filter(
        (tag) => !routesTag(tag) && borders(routesTag, tag),
      ).map((tag) => ({ tag })),
    },
  };
};
