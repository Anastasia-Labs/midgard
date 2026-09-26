/**
 * The fraud-proof catalogue is append-only. A category's four-byte id is
 * written into the computation-thread asset name of every proof of that
 * family, so renumbering or reusing an id silently redirects (or collides
 * with) threads of an already-deployed family.
 *
 * The oracle is a second, independently maintained table: `midgard-core`'s
 * deployment-manifest identity registry carries its own category order and id
 * map, and the deployment manifest is verified against it. Comparing the two
 * makes each side a check on the other, so a renumber applied in one place
 * fails here instead of transcribing the SDK's table back into this file
 * (where every new family would mean a hand edit and no added signal).
 */
import { readdirSync, readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";

import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
} from "../src/index.js";

const REPOSITORY_ROOT = fileURLToPath(new URL("../../../", import.meta.url));

/**
 * The reserved non-production category ids: every literal category id in
 * the repository that is deliberately not in the catalogue. The 2026-08-26
 * fan-out reserved test ids 0000000e-0000001b; all of them have since been
 * registered as production ids, so no test-only range remains. A new
 * test-only id is added here with its reason; an entry whose use is gone
 * fails the scan, so the list only shrinks unless someone argues an addition.
 */
const UNREGISTERED_CATEGORY_ID_USES: readonly {
  readonly path: string;
  readonly categoryId: string;
  readonly reason: string;
}[] = [
  {
    path: "demo/midgard-node/tests/inspect-contracts.test.ts",
    categoryId: "ffffffff",
    reason:
      "deliberately non-canonical; the test proves contract inspection refuses a catalogue that carries it",
  },
];

type CategoryIdUse = {
  readonly path: string;
  readonly line: number;
  readonly name: string;
  readonly categoryId: string;
  /** The family the identifier or the defining source directory names. */
  readonly family: string | undefined;
};

const SKIPPED_DIRECTORIES = new Set([
  "node_modules",
  "dist",
  "build",
  "coverage",
  "vendor",
  "patches",
]);

const sourceFiles = (directory: string): string[] =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    if (entry.name.startsWith(".") || SKIPPED_DIRECTORIES.has(entry.name)) {
      return [];
    }
    const path = join(directory, entry.name);
    if (entry.isDirectory()) return sourceFiles(path);
    return /\.(?:ts|mts|cts|js|mjs|cjs|ak)$/u.test(entry.name) ? [path] : [];
  });

// A TypeScript identifier ending in `categoryId`/`CATEGORY_ID`, or any
// `..._ID`, then within one expression a quoted eight-hex literal. An Aiken
// identifier containing `category_id` then a `#"........"` literal. Blind
// spot: an id literal bound to an unrelated name (`const x = #"00000014"`) or
// passed positionally is not seen.
const TYPESCRIPT_USE =
  /\b(\w*(?:categoryId|CategoryId|CATEGORY_ID|_ID))\b[^"'`;,]{0,80}["'`]([0-9a-f]{8})["'`]/gu;
const AIKEN_USE = /\b(\w*category_id\w*)\b[^#;\n]{0,60}#"([0-9a-f]{8})"/gu;

const familyKey = (name: string): string =>
  name.replace(/[-_]/gu, "").toLowerCase();

const FAMILY_BY_KEY = new Map<string, string>(
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name) => [familyKey(name), name]),
);

const familyByKeyOrUniquePrefix = (key: string): string | undefined => {
  if (key === "") return undefined;
  const exact = FAMILY_BY_KEY.get(key);
  if (exact !== undefined) return exact;
  const prefixed = [...FAMILY_BY_KEY].filter(([candidate]) =>
    candidate.startsWith(key),
  );
  return prefixed.length === 1 ? prefixed[0][1] : undefined;
};

/**
 * `<family>[_fraud|_test|_proposed]*[_category]_id[_v1]` names its family;
 * a bare `categoryId`/`fraud_category_id` names none.
 */
const familyNamedBy = (identifier: string): string | undefined => {
  const tokens = identifier
    .replace(/([a-z0-9])([A-Z])/gu, "$1_$2")
    .toLowerCase()
    .split("_")
    .filter((token) => token !== "");
  if (tokens.at(-1) === "v1") tokens.pop();
  if (tokens.at(-1) !== "id") return undefined;
  tokens.pop();
  if (tokens.at(-1) === "category") tokens.pop();
  while (["fraud", "test", "proposed"].includes(tokens.at(-1) ?? "")) {
    tokens.pop();
  }
  return familyByKeyOrUniquePrefix(tokens.join(""));
};

/**
 * Non-test source inside one family's directory defines that family, so a
 * generic `categoryId` there is that family's id. Tests are excluded: they
 * reuse other families' ids on purpose.
 */
const familyOwningSource = (path: string): string | undefined => {
  const directory =
    /^demo\/midgard-fault-proofs\/src\/([a-z0-9-]+)\//u.exec(path)?.[1] ??
    /^demo\/midgard-sdk\/src\/fraud-proof\/([a-z0-9-]+)\.ts$/u.exec(
      path,
    )?.[1] ??
    (path.endsWith(".test.ak")
      ? undefined
      : /^onchain\/aiken\/(?:lib\/midgard|validators)\/fraud-proofs\/([a-z0-9-]+)\//u.exec(
          path,
        )?.[1]);
  return directory === undefined
    ? undefined
    : FAMILY_BY_KEY.get(familyKey(directory));
};

const scanCategoryIdUses = (): CategoryIdUse[] => {
  const roots = [
    ...readdirSync(join(REPOSITORY_ROOT, "demo"), { withFileTypes: true })
      .filter(
        (entry) =>
          entry.isDirectory() &&
          !entry.name.startsWith(".") &&
          !SKIPPED_DIRECTORIES.has(entry.name),
      )
      .map((entry) => join(REPOSITORY_ROOT, "demo", entry.name)),
    join(REPOSITORY_ROOT, "onchain/aiken/lib"),
    join(REPOSITORY_ROOT, "onchain/aiken/validators"),
  ];
  // This file names the reserved ids itself, as data.
  const self = fileURLToPath(import.meta.url);
  return roots
    .flatMap(sourceFiles)
    .filter((file) => file !== self)
    .flatMap((file) => {
      const path = relative(REPOSITORY_ROOT, file).split("\\").join("/");
      const text = readFileSync(file, "utf8");
      const pattern = file.endsWith(".ak") ? AIKEN_USE : TYPESCRIPT_USE;
      return [...text.matchAll(pattern)].flatMap((match): CategoryIdUse[] => {
        const [, name, categoryId] = match;
        const namedFamily = familyNamedBy(name);
        // A `..._ID` that neither says "category" nor names a family is some
        // other identifier (a policy id, a key id), not a category id.
        if (namedFamily === undefined && !/category/iu.test(name)) return [];
        return [
          {
            path,
            line: text.slice(0, match.index).split("\n").length,
            name,
            categoryId,
            family: namedFamily ?? familyOwningSource(path),
          },
        ];
      });
    });
};

const describeUse = (use: CategoryIdUse): string =>
  `${use.path}:${use.line.toString()} ${use.name} = ${use.categoryId}`;

describe("production fraud-proof catalogue registration", () => {
  it("agrees with the deployment-manifest registry on every category id", () => {
    expect(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER).toEqual(
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    );
    expect({ ...FRAUD_PROOF_CATALOGUE_CATEGORY_IDS }).toEqual({
      ...DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
    });
  });

  it("keeps the id map total, sparse-safe and collision-free", () => {
    // Derived invariants, independent of either table's contents: identity is
    // carried by the map, never by array position, so the map must cover the
    // order exactly, ids must be the declared width, and no two families may
    // share one.
    expect(Object.keys(FRAUD_PROOF_CATALOGUE_CATEGORY_IDS)).toEqual(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    );
    const ids = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
      (name) => FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name],
    );
    const width = FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT * 2;
    const malformed = ids.filter(
      (id) => !new RegExp(`^[0-9a-f]{${width.toString()}}$`, "u").test(id),
    );
    expect(malformed).toEqual([]);
    const seen = new Map<string, string[]>();
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.forEach((name) => {
      const id = FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name];
      seen.set(id, [...(seen.get(id) ?? []), name]);
    });
    expect([...seen.entries()].filter(([, names]) => names.length > 1)).toEqual(
      [],
    );
  });

  describe("every category id written in the repository", () => {
    const uses = scanCategoryIdUses();
    const registeredFamilyById = new Map<string, string>(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((name) => [
        FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name],
        name,
      ]),
    );

    it("sees the on-chain and off-chain id constants it is meant to check", () => {
      // Guards the scan itself: if the patterns or roots stop matching, the
      // two checks below would pass on nothing.
      expect(
        uses.filter((use) => use.family !== undefined).length,
      ).toBeGreaterThan(40);
      expect(
        uses.find(
          (use) =>
            use.path ===
            "onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-01.ak",
        ),
      ).toMatchObject({
        categoryId: FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.fabricatedDeposit,
        family: "fabricatedDeposit",
      });
      expect(
        uses.find((use) =>
          use.path.startsWith(
            "demo/midgard-fault-proofs/src/withdrawal-mistag/",
          ),
        ),
      ).toMatchObject({ family: "withdrawalMistag" });
    });

    it("is registered in the catalogue or reserved with a reason", () => {
      const unregistered = uses
        .filter((use) => !registeredFamilyById.has(use.categoryId))
        .map(({ path, categoryId }) => ({ path, categoryId }));
      expect(unregistered).toEqual(
        UNREGISTERED_CATEGORY_ID_USES.map(({ path, categoryId }) => ({
          path,
          categoryId,
        })),
      );
    });

    it("is the id of the family its name or defining directory names", () => {
      // Deliberately no "registered id no longer used" check: ids are frozen
      // wire identities inside computation-thread asset names and are never
      // reused, so a family whose code is deleted keeps its id reserved in
      // the catalogue. Most families also reach their id only through
      // FRAUD_PROOF_CATALOGUE_CATEGORY_IDS, which no literal scan sees, and
      // the typed FAMILY_APPLICATION_REGISTRY already refuses an omitted
      // category.
      const misattributed = uses
        .filter(
          (use) =>
            use.family !== undefined &&
            registeredFamilyById.get(use.categoryId) !== use.family,
        )
        .map(
          (use) =>
            `${describeUse(use)}, but ${use.family ?? ""} is ${
              FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[
                use.family as keyof typeof FRAUD_PROOF_CATALOGUE_CATEGORY_IDS
              ]
            }`,
        );
      expect(misattributed).toEqual([]);
    });
  });
});
