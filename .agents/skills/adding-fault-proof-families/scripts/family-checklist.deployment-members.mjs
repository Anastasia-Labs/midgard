import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";

import { sourceFacetPaths } from "../../../../scripts/lib/source-facets.mjs";
import {
  CannotLook,
  extractLiteral,
  SOURCES,
  stripComments,
  topLevelElements,
} from "./family-checklist.top-level-elements.mjs";

const readExactSource = (root, path) => {
  try {
    return stripComments(readFileSync(join(root, path), "utf8"));
  } catch (error) {
    throw new CannotLook(`cannot read ${path}: ${error.message}`);
  }
};

// SDK family facades explicitly import/re-export their sibling facets. Only
// those reached files supply declarations; an unused naming sibling does not.
export const readSdkFamilySources = (root, path) => {
  const allowed = new Set(
    sourceFacetPaths(join(root, path)).map((file) => resolve(file)),
  );
  const pending = [resolve(root, path)];
  const modules = new Map();
  while (pending.length > 0) {
    const file = pending.pop();
    if (modules.has(file)) continue;
    const source = readExactSource("", file);
    modules.set(file, source);
    for (const [, specifier] of source.matchAll(
      /\b(?:from\s*|import\s*)"(\.\/[^"\n]+)\.js"/gu,
    )) {
      const target = resolve(dirname(file), `${specifier}.ts`);
      if (allowed.has(target)) pending.push(target);
    }
  }
  return {
    entry: modules.get(resolve(root, path)),
    modules: [...modules.values()],
  };
};

// Inspect only the declarative source shape owned by members.data.ts. Follow
// its actual spreads, not every file in members/: an unregistered file must
// not supply evidence. This never loads package output or evaluates TypeScript.
export const readDeploymentMembers = (root) => {
  const aggregate = readExactSource(root, SOURCES.coreMembers);
  if (
    !/export\s+const\s+DEPLOYMENT_MEMBERS\s*=\s*Object\.freeze\(\s*\{/u.test(
      aggregate,
    )
  ) {
    throw new CannotLook(
      `${SOURCES.coreMembers}: expected DEPLOYMENT_MEMBERS = Object.freeze({...})`,
    );
  }
  const imports = new Map(
    [
      ...aggregate.matchAll(
        /import\s*\{\s*(\w+)\s*\}\s*from\s*"(\.\/members\/[\w-]+)\.js"\s*;/gu,
      ),
    ].map((match) => [match[1], `${match[2]}.ts`]),
  );
  const members = new Map();
  const literal = extractLiteral(
    aggregate,
    "DEPLOYMENT_MEMBERS",
    SOURCES.coreMembers,
  );
  const after = aggregate.slice(aggregate.indexOf(literal) + literal.length);
  if (
    !/^\s*\)\s*(?:satisfies\s+Readonly\s*<\s*Record\s*<\s*DeploymentContract\s*,\s*DeploymentMember\s*>\s*>\s*)?;/u.test(
      after,
    )
  ) {
    throw new CannotLook(
      `${SOURCES.coreMembers}: expected a plain Object.freeze member aggregate`,
    );
  }
  const spreads = topLevelElements(literal);
  if (spreads.length === 0) {
    throw new CannotLook(`${SOURCES.coreMembers}: DEPLOYMENT_MEMBERS is empty`);
  }
  for (const spread of spreads) {
    const name = /^\.\.\.(\w+)$/u.exec(spread)?.[1];
    if (name === undefined || !imports.has(name)) {
      throw new CannotLook(
        `${SOURCES.coreMembers}: expected an imported member spread, got ${spread}`,
      );
    }
    const path = join(dirname(SOURCES.coreMembers), imports.get(name));
    const source = readExactSource(root, path);
    const declaration = new RegExp(
      `export\\s+const\\s+${name}\\s*=\\s*defineDeploymentFamily\\(\\s*"(\\w+)"\\s*,\\s*\\{`,
      "u",
    ).exec(source);
    if (declaration === null) {
      throw new CannotLook(
        `${path}: expected ${name} = defineDeploymentFamily("family", {...})`,
      );
    }
    const family = declaration[1];
    const literal = extractLiteral(source.slice(declaration.index), name, path);
    const literalEnd =
      source.indexOf(literal, declaration.index) + literal.length;
    if (!/^\s*,?\s*\);/u.test(source.slice(literalEnd))) {
      throw new CannotLook(
        `${path}: expected a plain defineDeploymentFamily declaration`,
      );
    }
    const entries = topLevelElements(literal);
    if (entries.length === 0) throw new CannotLook(`${path}: ${name} is empty`);
    for (const entry of entries) {
      const match = /^(\w+)\s*:\s*(\{[\s\S]*\})$/u.exec(entry);
      if (match === null)
        throw new CannotLook(`${path}: expected a literal member record`);
      const [, contract, body] = match;
      if (members.has(contract))
        throw new CannotLook(`${path}: duplicate member ${contract}`);
      const fields = new Map();
      for (const field of topLevelElements(body)) {
        const pair = /^(\w+)\s*:\s*([\s\S]+)$/u.exec(field);
        if (pair === null || fields.has(pair[1])) {
          throw new CannotLook(
            `${path}: malformed or duplicate field in ${contract}`,
          );
        }
        fields.set(pair[1], pair[2].trim());
      }
      const textOrNull = (key) => {
        const value = fields.get(key);
        if (value === "null") return null;
        if (!/^"(?:[^"\\]|\\.)*"$/u.test(value ?? "")) {
          throw new CannotLook(
            `${path}: ${contract}.${key} must be a string literal or null`,
          );
        }
        return JSON.parse(value);
      };
      let familyPaths;
      try {
        const paths = fields.get("familyPaths") ?? "";
        if (!paths.startsWith("[") || !paths.endsWith("]")) throw new Error();
        if (extractLiteral(`const paths = ${paths}`, "paths", path) !== paths)
          throw new Error();
        familyPaths = topLevelElements(paths).map((parts) => {
          if (!parts.startsWith("[") || !parts.endsWith("]")) throw new Error();
          if (extractLiteral(`const parts = ${parts}`, "parts", path) !== parts)
            throw new Error();
          return topLevelElements(parts).map((part) => {
            if (!/^"(?:[^"\\]|\\.)*"$/u.test(part)) throw new Error();
            return JSON.parse(part);
          });
        });
      } catch {
        throw new CannotLook(
          `${path}: ${contract}.familyPaths must be literal string arrays`,
        );
      }
      if (
        !Array.isArray(familyPaths) ||
        !familyPaths.every(
          (parts) =>
            Array.isArray(parts) &&
            parts.length > 0 &&
            parts.every((part) => typeof part === "string" && part.length > 0),
        )
      )
        throw new CannotLook(
          `${path}: ${contract}.familyPaths must be literal string arrays`,
        );
      members.set(contract, {
        family,
        referenceRole: textOrNull("referenceRole"),
        validatorTitle: textOrNull("validatorTitle"),
        firstStep: familyPaths.some(
          (parts) => parts.length === 1 && parts[0] === "firstStep",
        ),
      });
    }
  }
  return members;
};
