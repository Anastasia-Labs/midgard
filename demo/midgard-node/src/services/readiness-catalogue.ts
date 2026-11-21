/**
 * The readiness catalogue: every code the node's `/readyz` can name, where
 * it appears and the format of each parameter after it, held as data. A
 * row reads the code from the constant its producer declares where there is
 * one; the producers stay as they are.
 *
 * A public profile shows a line through `publicReadinessLine`, and the
 * public log redactor keeps a catalogued code. A row whose last parameter is
 * free text shows as its code alone, so no producer's message reaches a
 * public answer.
 */
import { COMMIT_READINESS_ROWS } from "./readiness-catalogue.commit.js";
import { FOLLOWER_READINESS_ROWS } from "./readiness-catalogue.follower.js";
import { NODE_READINESS_ROWS } from "./readiness-catalogue.node.js";
import type { ReadinessRow } from "./readiness-catalogue.row.js";
import { STARTUP_READINESS_ROWS } from "./readiness-catalogue.startup.js";

export const READINESS_CATALOGUE: readonly ReadinessRow[] = [
  ...STARTUP_READINESS_ROWS,
  ...FOLLOWER_READINESS_ROWS,
  ...NODE_READINESS_ROWS,
  ...COMMIT_READINESS_ROWS,
];

const rows: ReadonlyMap<string, ReadinessRow> = new Map(
  READINESS_CATALOGUE.map((row) => [row.code, row]),
);

/** Every catalogued code. */
export const READINESS_CODES: ReadonlySet<string> = new Set(rows.keys());

/** The code of `line` and its parameters, `:`-separated after it. */
const split = (line: string): readonly [string, readonly string[]] => {
  const at = line.indexOf(":");
  return at < 0
    ? [line, []]
    : [line.slice(0, at), line.slice(at + 1).split(":")];
};

/**
 * The row `line` is a well-formed line of: its code is catalogued and its
 * parameters are the row's, each in its format (free text taking the rest).
 * Undefined otherwise.
 */
export const parseReadinessLine = (line: string): ReadinessRow | undefined => {
  const [code, values] = split(line);
  const row = rows.get(code);
  if (row === undefined) return undefined;
  for (const [at, param] of row.params.entries()) {
    if (param.kind === "text") return at < values.length ? row : undefined;
    const value = values[at];
    if (value === undefined)
      return row.params
        .slice(at)
        .every((rest) => rest.kind === "bounded" && rest.optional)
        ? row
        : undefined;
    if (!param.format.test(value)) return undefined;
  }
  return values.length === row.params.length ? row : undefined;
};

/**
 * `line` as a public profile shows it: undefined for a code the catalogue
 * does not hold; the code alone for a row with free text, and for a line
 * whose parameters are not the row's; otherwise the line, each value out of
 * its format replaced by its parameter's fixed `otherwise` value.
 */
export const publicReadinessLine = (line: string): string | undefined => {
  const [code, values] = split(line);
  const row = rows.get(code);
  if (row === undefined) return undefined;
  if (values.length === 0 || values.length > row.params.length) return code;
  const shown: string[] = [code];
  for (const [at, param] of row.params.entries()) {
    if (param.kind === "text") return code;
    const value = values[at];
    if (value === undefined) {
      if (!param.optional) return code;
      continue;
    }
    const safe = param.format.test(value) ? value : param.otherwise;
    if (safe === undefined) return code;
    shown.push(safe);
  }
  return shown.join(":");
};
