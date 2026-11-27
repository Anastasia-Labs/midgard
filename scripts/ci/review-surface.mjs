#!/usr/bin/env node

// Renders a pull request's on-chain review surface from two blueprints, the
// merge base's and the head's, as Markdown for the job summary. Nothing here is
// committed: ABI movement is review information read off fresh builds, not a
// snapshot to keep in sync.
//
// The ABI of a blueprint is every validator's title, parameters, datum and
// redeemer schemas, plus the definitions they reach. Compiled code and hashes
// change with any source edit and are not ABI; the size section reports code
// size movement separately.
//
// usage: node scripts/ci/review-surface.mjs <base plutus.json> <head plutus.json>
// Exit 0 with the Markdown on stdout, 2 on usage errors. A blueprint that
// cannot be read or has no validators fails, so a broken build never reads as
// "no ABI change".

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

const DEFINITION_PREFIX = "#/definitions/";
const SIZE_ROWS = 15;

// A `$ref` JSON pointer to its key in `definitions`.
const refName = (ref) =>
  ref
    .slice(DEFINITION_PREFIX.length)
    .replaceAll("~1", "/")
    .replaceAll("~0", "~");

// Every definition a schema reaches, followed through `$ref`s.
const reachedDefinitions = (schemas, definitions) => {
  const reached = new Set();
  const pending = [...schemas];
  while (pending.length > 0) {
    const node = pending.pop();
    if (Array.isArray(node)) pending.push(...node);
    else if (node !== null && typeof node === "object") {
      for (const [key, value] of Object.entries(node)) {
        if (key === "$ref" && typeof value === "string") {
          const name = refName(value);
          if (!reached.has(name)) {
            reached.add(name);
            pending.push(definitions[name]);
          }
        } else pending.push(value);
      }
    }
  }
  return reached;
};

const stable = (value) => JSON.stringify(value);

/** The ABI and code sizes of one parsed blueprint. */
export const blueprintAbi = (blueprint) => {
  const validators = blueprint?.validators;
  if (!Array.isArray(validators) || validators.length === 0)
    throw new Error("blueprint has no validators");
  const definitions = blueprint.definitions ?? {};
  const abi = new Map();
  const sizes = new Map();
  const schemas = [];
  for (const validator of validators) {
    const interfaceOf = {
      parameters: (validator.parameters ?? []).map(({ title, schema }) => ({
        title,
        schema,
      })),
      datum: validator.datum?.schema ?? null,
      redeemer: validator.redeemer?.schema ?? null,
    };
    abi.set(validator.title, interfaceOf);
    sizes.set(validator.title, (validator.compiledCode ?? "").length / 2);
    schemas.push(interfaceOf);
  }
  const reached = new Map(
    [...reachedDefinitions(schemas, definitions)].map((name) => [
      name,
      definitions[name] ?? null,
    ]),
  );
  return { validators: abi, definitions: reached, sizes };
};

const keyedDiff = (base, head) => {
  const added = [...head.keys()].filter((key) => !base.has(key)).sort();
  const removed = [...base.keys()].filter((key) => !head.has(key)).sort();
  const changed = [...head.keys()]
    .filter(
      (key) => base.has(key) && stable(base.get(key)) !== stable(head.get(key)),
    )
    .sort();
  return { added, removed, changed };
};

const list = (heading, items, describe = (item) => `\`${item}\``) =>
  items.length === 0
    ? []
    : [`**${heading}**`, "", ...items.map((item) => `- ${describe(item)}`), ""];

/** Markdown for the ABI and code-size movement from `base` to `head`. */
export const renderReviewSurface = (base, head) => {
  const validators = keyedDiff(base.validators, head.validators);
  const definitions = keyedDiff(base.definitions, head.definitions);
  const changedParts = (title) =>
    ["parameters", "datum", "redeemer"]
      .filter(
        (part) =>
          stable(base.validators.get(title)[part]) !==
          stable(head.validators.get(title)[part]),
      )
      .join(", ");
  const abiChanged =
    Object.values(validators).some((items) => items.length > 0) ||
    Object.values(definitions).some((items) => items.length > 0);
  const lines = ["## On-chain ABI", ""];
  if (!abiChanged) lines.push("No on-chain ABI change.", "");
  else
    lines.push(
      ...list("Validators added", validators.added),
      ...list("Validators removed", validators.removed),
      ...list(
        "Validator interfaces changed",
        validators.changed,
        (title) => `\`${title}\` (${changedParts(title)})`,
      ),
      ...list("Definitions added", definitions.added),
      ...list("Definitions removed", definitions.removed),
      ...list("Definitions changed", definitions.changed),
    );
  const moved = [...head.sizes.entries()]
    .filter(([title]) => base.sizes.has(title))
    .map(([title, bytes]) => ({
      title,
      before: base.sizes.get(title),
      after: bytes,
    }))
    .filter(({ before, after }) => before !== after)
    .sort(
      (a, b) =>
        Math.abs(b.after - b.before) - Math.abs(a.after - a.before) ||
        a.title.localeCompare(b.title, "en"),
    );
  lines.push("## Compiled size", "");
  if (moved.length === 0) lines.push("No compiled-size change.", "");
  else {
    lines.push(
      `${moved.length} validator(s) changed size; the largest movements:`,
      "",
      "| Validator | Base bytes | Head bytes | Change |",
      "|---|---:|---:|---:|",
      ...moved
        .slice(0, SIZE_ROWS)
        .map(
          ({ title, before, after }) =>
            `| \`${title}\` | ${before} | ${after} | ${after > before ? "+" : ""}${after - before} |`,
        ),
      "",
    );
  }
  return lines.join("\n");
};

const readBlueprint = (path) =>
  blueprintAbi(JSON.parse(readFileSync(path, "utf8")));

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const [basePath, headPath] = process.argv.slice(2);
  if (basePath === undefined || headPath === undefined) {
    process.stderr.write(
      "usage: review-surface.mjs <base plutus.json> <head plutus.json>\n",
    );
    process.exit(2);
  }
  process.stdout.write(
    `${renderReviewSurface(readBlueprint(basePath), readBlueprint(headPath))}\n`,
  );
}
