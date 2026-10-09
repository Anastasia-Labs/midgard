import { resolve } from "node:path";

import { checkBlueprintStamp } from "../scripts/lib/blueprint-stamp.mjs";

/**
 * Vitest global setup for every package whose suites read the compiled
 * blueprint. A blueprint built from other sources, or by another compiler,
 * turns every result that touches a script hash or a budget into noise — the
 * 2026-09 stale-blueprint incident surfaced as 864 unrelated reds — so the run
 * is refused before any file starts, with the command that rebuilds it.
 *
 * An absent default blueprint is left to the suites that read it: they fail
 * on the missing file by name, and packages whose selected files never read
 * it still run. `MIDGARD_REAL_BLUEPRINT_PATH` names a blueprint explicitly, so
 * it must exist and be fresh: its build record must name this tree's sources
 * and the pinned compiler (the interactive-emulator build and the traced
 * refusal overlays both write one). `MIDGARD_BLUEPRINT_STAMP=warn` is the one
 * deliberate downgrade, from a refusal to a warning, for a local run against
 * a stale build; CI and `contrib test` never set it.
 *
 * @returns {{ action: "run" | "warn" | "refuse", message?: string }}
 */
export const blueprintStampDecision = ({ env = process.env, root } = {}) => {
  const override = env.MIDGARD_REAL_BLUEPRINT_PATH;
  const verdict = checkBlueprintStamp({
    ...(root === undefined ? {} : { root }),
    ...(override === undefined ? {} : { blueprintPath: resolve(override) }),
  });
  if (verdict.status === "fresh") return { action: "run" };
  if (verdict.blueprintAbsent && override === undefined)
    return { action: "run" };
  const message = [
    `[blueprint-stamp] ${verdict.detail}`,
    ...(override === undefined
      ? []
      : [`(named by MIDGARD_REAL_BLUEPRINT_PATH=${override})`]),
    ...(verdict.fix === null ? [] : [`Rebuild it: ${verdict.fix}`]),
  ].join("\n");
  if (env.MIDGARD_BLUEPRINT_STAMP === "warn")
    return {
      action: "warn",
      message: `${message}\n(continuing: MIDGARD_BLUEPRINT_STAMP=warn)`,
    };
  return {
    action: "refuse",
    message: `${message}\nSet MIDGARD_BLUEPRINT_STAMP=warn only for a deliberate run against this build.`,
  };
};

export default function setup() {
  const decision = blueprintStampDecision();
  if (decision.action === "warn") console.warn(decision.message);
  if (decision.action === "refuse") throw new Error(decision.message);
}
