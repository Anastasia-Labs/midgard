import { readdirSync } from "node:fs";
import { join } from "node:path";

import { readSourceFacets } from "../../../../scripts/lib/source-facets.mjs";

export const EXIT_COMPLETE = 0;

export const EXIT_GAPS = 1;

export const EXIT_CANNOT_READ = 2;

export const EXIT_USAGE = 64;

export const SOURCES = Object.freeze({
  sdkCatalogue: "demo/midgard-sdk/src/fraud-proof/catalogue.ts",
  sdkBuild: "demo/midgard-sdk/src/fraud-proof/contracts/build.ts",
  coreCatalogue:
    "demo/midgard-core/src/deployment-manifest-identity/catalogue-roles.ts",
  coreReferenceContracts:
    "demo/midgard-core/src/deployment-manifest-identity/reference-script-contracts.ts",
  coreReferenceTokens:
    "demo/midgard-core/src/deployment-manifest-identity/reference-script-tokens.ts",
  registry:
    "demo/midgard-fault-proofs/src/workflow/family-application-registry.ts",
  linearSpec: "demo/midgard-fault-proofs/src/workflow/linear-family-spec.ts",
  definitions: "demo/midgard-fault-proofs/src/workflow/family-definitions.ts",
  reasons: "demo/midgard-fault-proofs/src/workflow/reason-disposition.ts",
  classification: "demo/midgard-fault-proofs/src/workflow/classification.ts",
  adapters: "demo/midgard-fault-proofs/src/workflow/adapters.ts",
  catalogueStatus: "docs/fault-proofs/catalogue-status.md",
  journeys: "demo/midgard-node-tools/devnet/watcher-journeys/catalogue.ts",
  faultProofTests: "demo/midgard-fault-proofs/tests",
  validators: "onchain/aiken/validators",
});

/** Thrown when a source cannot be read or parsed: exit code 2, not a gap. */
export class CannotLook extends Error {}

export const readSource = (root, key) => {
  const path = join(root, SOURCES[key]);
  try {
    return readSourceFacets(path);
  } catch (error) {
    throw new CannotLook(`cannot read ${SOURCES[key]}: ${error.message}`);
  }
};

// Returns the text of the bracketed literal that follows `const <name>`,
// skipping strings and comments so brackets inside them do not count.
export const extractLiteral = (text, name, file) => {
  const start = new RegExp(`\\bconst ${name}\\b`, "u").exec(text);
  if (start === null) {
    throw new CannotLook(`${file}: no \`const ${name}\` found`);
  }
  let index = text.slice(start.index).search(/[[{]/u);
  if (index < 0) throw new CannotLook(`${file}: ${name} has no literal`);
  index += start.index;
  const open = index;
  let depth = 0;
  while (index < text.length) {
    const char = text[index];
    const next = text[index + 1];
    if (char === "/" && next === "/") {
      index = text.indexOf("\n", index);
      if (index < 0) break;
      continue;
    }
    if (char === "/" && next === "*") {
      index = text.indexOf("*/", index + 2);
      if (index < 0) break;
      index += 2;
      continue;
    }
    if (char === '"' || char === "'" || char === "`") {
      index += 1;
      while (index < text.length && text[index] !== char) {
        index += text[index] === "\\" ? 2 : 1;
      }
      index += 1;
      continue;
    }
    if (char === "[" || char === "{") depth += 1;
    if (char === "]" || char === "}") {
      depth -= 1;
      if (depth === 0) return text.slice(open, index + 1);
    }
    index += 1;
  }
  throw new CannotLook(`${file}: ${name} literal is not closed`);
};

// Splits an array literal into its top-level element texts, skipping strings
// and comments.
export const topLevelElements = (literal) => {
  const elements = [];
  let depth = 0;
  let start = 1;
  let index = 0;
  while (index < literal.length) {
    const char = literal[index];
    const next = literal[index + 1];
    if (char === "/" && next === "/") {
      const end = literal.indexOf("\n", index);
      index = end < 0 ? literal.length : end;
      continue;
    }
    if (char === "/" && next === "*") {
      const end = literal.indexOf("*/", index + 2);
      index = end < 0 ? literal.length : end + 2;
      continue;
    }
    if (char === '"' || char === "'" || char === "`") {
      index += 1;
      while (index < literal.length && literal[index] !== char) {
        index += literal[index] === "\\" ? 2 : 1;
      }
      index += 1;
      continue;
    }
    if (char === "[" || char === "{" || char === "(") depth += 1;
    if (char === "]" || char === "}" || char === ")") {
      depth -= 1;
      if (depth === 0) {
        elements.push(literal.slice(start, index));
        break;
      }
    }
    if (char === "," && depth === 1) {
      elements.push(literal.slice(start, index));
      start = index + 1;
    }
    index += 1;
  }
  return elements.map((element) => element.trim()).filter((e) => e !== "");
};

export const stripComments = (text) =>
  text.replace(/\/\*[\s\S]*?\*\//gu, "").replace(/(^|[^:"])\/\/.*$/gmu, "$1");

/** Quoted strings of an array literal, in order. */
export const arrayStrings = (literal) =>
  [...stripComments(literal).matchAll(/"([^"]+)"/gu)].map((match) => match[1]);

/** `key: "value"` or `"key": "value"` pairs of an object literal. */
export const objectStringPairs = (literal) =>
  new Map(
    [
      ...stripComments(literal).matchAll(
        /(?:"([^"]+)"|\b([A-Za-z_$][\w$]*))\s*:\s*"([^"]*)"/gu,
      ),
    ].map((match) => [match[1] ?? match[2], match[3]]),
  );

/** Top-level identifier keys of an object literal (`key:` or `...spread`). */
export const objectKeys = (literal) =>
  new Set(
    [
      ...stripComments(literal).matchAll(
        /(?:^|[{,])\s*(?:\.\.\.)?([A-Za-z_$][\w$]*)\s*(?=[:,}])/gmu,
      ),
    ].map((match) => match[1]),
  );

export const kebab = (camel) =>
  camel.replace(/([a-z0-9])([A-Z])/gu, "$1-$2").toLowerCase();

export const listFiles = (root, relativeDirectory) => {
  try {
    return readdirSync(join(root, relativeDirectory));
  } catch (error) {
    throw new CannotLook(`cannot list ${relativeDirectory}: ${error.message}`);
  }
};

// Blueprint title `fraud_proofs/a_b/step_01.main.spend` names the Aiken module
// `fraud_proofs/a_b/step_01`, whose file is validators/fraud-proofs/a-b/step-01.ak
// (Aiken maps dashes in file names to underscores in module names).
export const validatorFileCandidates = (title) => {
  const module = title.split(".")[0];
  return [
    ...new Set([`${module.replaceAll("_", "-")}.ak`, `${module}.ak`]),
  ].map((path) => `${SOURCES.validators}/${path}`);
};
