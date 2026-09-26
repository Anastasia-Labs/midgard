import { resolve } from "node:path";

import { checkBlueprintStamp } from "../scripts/lib/blueprint-stamp.mjs";

/**
 * Vitest global setup for every package whose suites read the compiled
 * blueprint. A blueprint built from other sources, or by another compiler,
 * turns every result that touches a script hash or a budget into noise — the
 * 2026-09 stale-blueprint incident surfaced as 864 unrelated reds — so the run
 * is refused before any file starts, with the command that rebuilds it.
 *
 * An absent blueprint is left to the suites that read it: they fail on the
 * missing file by name, and packages whose selected files never read it still
 * run. `MIDGARD_REAL_BLUEPRINT_PATH` names an explicitly prepared artifact,
 * which may lack a build record; that case warns instead of refusing.
 * `MIDGARD_BLUEPRINT_STAMP=warn` downgrades a refusal to a warning for a
 * deliberate local run against a stale build; CI never sets it.
 */
export default function setup() {
  const override = process.env.MIDGARD_REAL_BLUEPRINT_PATH;
  const verdict = checkBlueprintStamp(
    override === undefined ? {} : { blueprintPath: resolve(override) },
  );
  if (verdict.status === "fresh") {
    return;
  }
  if (verdict.blueprintAbsent && override === undefined) {
    return;
  }
  const message = [
    `[blueprint-stamp] ${verdict.detail}`,
    ...(verdict.fix === null ? [] : [`Rebuild it: ${verdict.fix}`]),
  ].join("\n");
  if (
    override !== undefined ||
    process.env.MIDGARD_BLUEPRINT_STAMP === "warn"
  ) {
    console.warn(`${message}\n(continuing: warning only)`);
    return;
  }
  throw new Error(
    `${message}\nSet MIDGARD_BLUEPRINT_STAMP=warn only for a deliberate run against this build.`,
  );
}
