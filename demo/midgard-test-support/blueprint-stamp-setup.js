import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { checkBlueprintStamp } from "../scripts/lib/blueprint-stamp.mjs";
import { aikenBinary } from "../../scripts/lib/aiken.mjs";
import { withBuildLock } from "./build-lock.js";

const repositoryRoot = resolve(
  dirname(fileURLToPath(import.meta.url)),
  "../..",
);

/**
 * Vitest global setup for every package whose suites read the compiled
 * blueprint. A blueprint built from other sources turns every result that
 * touches a script hash or a budget into noise (the 2026-09 stale-blueprint
 * incident surfaced as 864 unrelated reds), so the setup makes the default
 * blueprint describe this tree before any file starts.
 *
 * A missing or stale default blueprint is rebuilt the way `contrib test`
 * prepares one (`scripts/contrib/blueprint.mjs`): copied from a checkout whose
 * build record names identical inputs, else built with `deployment:build` for
 * the selected profile. The run is refused only when it cannot be rebuilt:
 * the pinned compiler is unavailable, or the stamp cannot be judged.
 *
 * `MIDGARD_REAL_BLUEPRINT_PATH` names a blueprint explicitly, so it must exist
 * and be fresh: its build record must name this tree's sources (the
 * interactive-emulator build and the traced refusal overlays both write one).
 * A named blueprint is never rebuilt; it is refused.
 *
 * @returns {{ action: "run" } | { action: "rebuild", reason: string } | { action: "refuse", message: string }}
 */
export const blueprintStampDecision = ({ env = process.env, root } = {}) => {
  const override = env.MIDGARD_REAL_BLUEPRINT_PATH;
  const verdict = checkBlueprintStamp({
    ...(root === undefined ? {} : { root }),
    ...(override === undefined ? {} : { blueprintPath: resolve(override) }),
  });
  if (verdict.status === "fresh") return { action: "run" };
  if (override === undefined && verdict.status !== "unknown")
    return { action: "rebuild", reason: verdict.detail };
  return {
    action: "refuse",
    message: [
      `[blueprint-stamp] ${verdict.detail}`,
      ...(override === undefined
        ? []
        : [`(named by MIDGARD_REAL_BLUEPRINT_PATH=${override})`]),
      ...(verdict.fix === null ? [] : [`Rebuild it: ${verdict.fix}`]),
    ].join("\n"),
  };
};

/** The default build: `contrib`'s, loaded only when a rebuild is due. */
const contribEnsureBlueprint = async (root, { build }) => {
  const { deploymentBuild, ensureBlueprint } = await import(
    "../../scripts/contrib/blueprint.mjs"
  );
  return ensureBlueprint(root, {
    build: (...args) => build(deploymentBuild, ...args),
  });
};

/**
 * Makes the default blueprint of `root` fresh: one process rebuilds it under
 * a lock in `onchain/aiken/build/`, and every other run that needs it waits
 * for that build and then reads the result. Copying needs no compiler; a
 * build is refused, with the reason, when the pinned compiler is unavailable.
 */
export const rebuildBlueprint = async ({
  root = repositoryRoot,
  reason,
  ensure = contribEnsureBlueprint,
  compiler = aikenBinary,
  log = (line) => process.stderr.write(`${line}\n`),
  timeoutMs = 1_800_000,
}) => {
  const fresh = () => checkBlueprintStamp({ root }).status === "fresh";
  await withBuildLock(
    {
      lock: resolve(root, "onchain/aiken/build/default-blueprint.lock"),
      ready: fresh,
      timeoutMs,
    },
    async () => {
      log(`[blueprint-stamp] ${reason}; rebuilding it before the run`);
      const result = await ensure(root, {
        build: (deploymentBuild, ...args) => {
          try {
            compiler();
          } catch (cause) {
            throw new Error(
              `[blueprint-stamp] ${reason}\nIt cannot be rebuilt: the pinned compiler is unavailable (${cause instanceof Error ? cause.message : String(cause)})`,
              { cause },
            );
          }
          return deploymentBuild(...args);
        },
      });
      log(
        result.action === "copied"
          ? `[blueprint-stamp] copied a fresh blueprint from ${result.from}`
          : `[blueprint-stamp] rebuilt the ${String(result.profile)} blueprint (log: ${String(result.logPath)})`,
      );
    },
  );
  if (!fresh())
    throw new Error(
      `[blueprint-stamp] the rebuild left a stale blueprint: ${checkBlueprintStamp({ root }).detail}`,
    );
};

export default async function setup() {
  const decision = blueprintStampDecision();
  if (decision.action === "refuse") throw new Error(decision.message);
  if (decision.action === "rebuild")
    await rebuildBlueprint({ reason: decision.reason });
}
