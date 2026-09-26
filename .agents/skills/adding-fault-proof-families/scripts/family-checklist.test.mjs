import assert from "node:assert/strict";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { afterEach, describe, it } from "node:test";
import { fileURLToPath } from "node:url";

import {
  EXIT_CANNOT_READ,
  EXIT_COMPLETE,
  EXIT_GAPS,
  EXIT_USAGE,
  SOURCES,
  main,
} from "./family-checklist.mjs";

// A minimal tree with two families: fooBar (complete) and bazFooBar, whose
// kebab stem contains fooBar's, so name matching must not give its tests to
// fooBar.
const completeTree = () => ({
  [SOURCES.sdkCatalogue]: `
export const FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = [
  "fooBar",
  "bazFooBar",
] as const satisfies readonly (keyof FraudProofs)[];
export const FRAUD_PROOF_CATALOGUE_CATEGORY_IDS = {
  fooBar: "00000040",
  bazFooBar: "00000041",
} as const;
`,
  [SOURCES.coreIdentity]: `
export const DEPLOYMENT_MANIFEST_CONTRACT_NAMES = Object.freeze([
  "fraudProofFooBar",
  "fraudProofFooBarStep02",
  "fraudProofBazFooBar",
] as const);
export const DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER =
  Object.freeze(["fooBar", "bazFooBar"] as const);
export const DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY =
  Object.freeze({
    fooBar: "fraudProofFooBar",
    bazFooBar: "fraudProofBazFooBar",
  } as const);
export const DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS =
  Object.freeze({ fooBar: "00000040", bazFooBar: "00000041" } as const);
export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE =
  Object.freeze({
    "V1 fraud-proof foo-bar step-01": "fraudProofFooBar",
    "V1 fraud-proof foo-bar step-02":
      "fraudProofFooBarStep02",
    "V1 fraud-proof baz-foo-bar step-01": "fraudProofBazFooBar",
  } as const);
export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES = Object.freeze({
  "V1 fraud-proof foo-bar step-01": "V1FpFooBarS01",
  "V1 fraud-proof foo-bar step-02": "V1FpFooBarS02",
  "V1 fraud-proof baz-foo-bar step-01": "V1FpBazFooBarS01",
} as const);
`,
  [SOURCES.sdkBuild]: `
import { buildFooBarChain } from "./families/foo-bar.js";
import { buildBazFooBarChain } from "./families/baz-foo-bar.js";
export const build = () =>
  Effect.gen(function* () {
    const fooBar = yield* buildFooBarChain({ ...params, ...shared });
    const bazFooBar = yield* buildBazFooBarChain({ ...params, ...shared });
  });
`,
  "demo/midgard-sdk/src/fraud-proof/contracts/families/foo-bar.ts": `
export const FOO_BAR_TITLES = {
  step01: "fraud_proofs/foo_bar/step_01.main.spend",
  step02: "fraud_proofs/foo_bar/step_02.main.spend",
} as const;
`,
  "demo/midgard-sdk/src/fraud-proof/contracts/families/baz-foo-bar.ts": `
export const BAZ_FOO_BAR_TITLES = {
  step01: "fraud_proofs/baz_foo_bar/step_01.main.spend",
} as const;
`,
  "onchain/aiken/validators/fraud-proofs/foo-bar/step-01.ak": "",
  "onchain/aiken/validators/fraud-proofs/foo-bar/step-02.ak": "",
  "onchain/aiken/validators/fraud-proofs/baz-foo-bar/step-01.ak": "",
  [SOURCES.registry]: `
export const FAMILY_APPLICATION_REGISTRY: {
  readonly [Category in FraudProofCatalogueCategoryName]: FamilyApplicationRegistryEntry<Category>;
} = Object.freeze({
  ...LINEAR_FAMILY_APPLICATION_RECORDS,
  fooBar: FOO_BAR_FAMILY_APPLICATION_RECORD,
});
`,
  [SOURCES.linearSpec]: `
export const LINEAR_FAMILY_CATEGORIES = Object.freeze([
  "bazFooBar",
] as const);
`,
  [SOURCES.definitions]: `
export const CURSOR_FAMILY_DEFINITIONS: CursorFamilyDefinitions = Object.freeze(
  { fooBar: FOO_BAR_FAMILY_DEFINITION },
);
`,
  [SOURCES.reasons]: `
export const TYPED_REASON_DISPOSITIONS = Object.freeze({
  // Comment with a { bracket.
  FooBarBroken: direct("fooBar"),
  SomethingElse: direct("bazFooBar"),
}) satisfies Readonly<Record<string, TypedReasonDisposition>>;
`,
  [SOURCES.classification]: `
export const FRAUD_PROOF_CLASSIFICATION_RULES = Object.freeze([
  { category: "fooBar", violationIds: ["foo-bar"] },
  { category: "bazFooBar", violationIds: [BAZ_VIOLATION_ID] },
] as const);
`,
  [SOURCES.adapters]: `
const workflowAdapterRegistrationRows = [
  { category: "fooBar", status: "ready", runner: fooBarRunner },
  // A helper-call row, as in the real table (manual("transitionTrace", ...)).
  manual("bazFooBar", ["baz-foo-bar/workflow.ts, with a comma"]),
] as const satisfies readonly WorkflowAdapterRegistration[];
`,
  [SOURCES.catalogueStatus]: `
| ID         | Category    | Watcher installed |
| ---------- | ----------- | ----------------- |
| \`00000040\` | \`fooBar\`    | Yes               |
| \`00000041\` | \`bazFooBar\` | Yes               |
`,
  [SOURCES.journeys]: `
export const JOURNEY_FIXTURE_OWNERS = {
  fooBar: "transaction",
  bazFooBar: "history",
} as const;
`,
  [`${SOURCES.faultProofTests}/foo-bar-lifecycle.test.ts`]:
    "await expectOnchainRefusal(() => build());\n",
  [`${SOURCES.faultProofTests}/baz-foo-bar.test.ts`]: "",
});

const roots = [];
afterEach(() => {
  while (roots.length > 0) {
    rmSync(roots.pop(), { recursive: true, force: true });
  }
});

const writeTree = (files) => {
  const root = mkdtempSync(join(tmpdir(), "family-checklist-"));
  roots.push(root);
  for (const [path, text] of Object.entries(files)) {
    if (text === null) continue;
    mkdirSync(dirname(join(root, path)), { recursive: true });
    writeFileSync(join(root, path), text);
  }
  return root;
};

const run = (root, category) => {
  const out = [];
  const err = [];
  const code = main(
    [category, "--root", root],
    (line) => out.push(line),
    (line) => err.push(line),
  );
  return { code, out: out.join("\n"), err: err.join("\n") };
};

const edit = (tree, path, from, to) => {
  assert.ok(tree[path].includes(from), `fixture ${path} lacks ${from}`);
  return { ...tree, [path]: tree[path].replace(from, to) };
};

describe("family-checklist", () => {
  it("passes a complete family and reports its evidence", () => {
    const result = run(writeTree(completeTree()), "fooBar");
    assert.equal(result.code, EXIT_COMPLETE, result.out + result.err);
    assert.match(result.out, /OK\s+catalogue-id\s+ID 00000040/u);
    assert.match(
      result.out,
      /OK\s+reference-scripts\s+2 contract\(s\) named fraudProofFooBar\*/u,
    );
    assert.match(result.out, /typed-reasons\s+.*FooBarBroken/u);
    assert.match(result.out, /cursor FamilyDefinition/u);
    assert.match(result.out, /foo-bar-lifecycle\.test\.ts/u);
  });

  it("does not credit a family with tests named after a longer stem", () => {
    const tree = completeTree();
    delete tree[`${SOURCES.faultProofTests}/foo-bar-lifecycle.test.ts`];
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS, result.out);
    assert.match(result.out, /GAP\s+tests\s/u);
  });

  it("derives a linear family's record from LINEAR_FAMILY_CATEGORIES", () => {
    const result = run(writeTree(completeTree()), "bazFooBar");
    assert.equal(result.code, EXIT_COMPLETE, result.out + result.err);
    assert.match(result.out, /OK\s+adapter-registration\s+.*row 1,/u);
    assert.match(result.out, /derived record via LINEAR_FAMILY_CATEGORIES/u);
  });

  it("fails on a family missing from the application registry", () => {
    const tree = edit(
      completeTree(),
      SOURCES.registry,
      "  fooBar: FOO_BAR_FAMILY_APPLICATION_RECORD,\n",
      "",
    );
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(result.out, /GAP\s+application-record/u);
  });

  it("fails when core and SDK disagree on the ID", () => {
    const tree = edit(
      completeTree(),
      SOURCES.coreIdentity,
      'Object.freeze({ fooBar: "00000040"',
      'Object.freeze({ fooBar: "00000042"',
    );
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(result.out, /GAP\s+core-identity.*differs from SDK ID/u);
  });

  it("fails when two categories share an ID", () => {
    const tree = edit(
      completeTree(),
      SOURCES.sdkCatalogue,
      'bazFooBar: "00000041"',
      'bazFooBar: "00000040"',
    );
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(result.out, /GAP\s+catalogue-id.*also used by bazFooBar/u);
  });

  it("fails when a step contract has no reference-script token name", () => {
    const tree = edit(
      completeTree(),
      SOURCES.coreIdentity,
      '  "V1 fraud-proof foo-bar step-02": "V1FpFooBarS02",\n',
      "",
    );
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(result.out, /GAP\s+reference-scripts.*step-02.*token name/u);
  });

  it("fails when a non-step contract of the family has no role", () => {
    const tree = edit(
      completeTree(),
      SOURCES.coreIdentity,
      '  "fraudProofBazFooBar",\n] as const);',
      '  "fraudProofBazFooBar",\n  "fraudProofFooBarStep02TxYield",\n] as const);',
    );
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(
      result.out,
      /GAP\s+reference-scripts.*fraudProofFooBarStep02TxYield has no reference-script role/u,
    );
  });

  it("fails when classification rules are out of catalogue order", () => {
    const tree = edit(
      completeTree(),
      SOURCES.classification,
      '  { category: "fooBar", violationIds: ["foo-bar"] },\n  { category: "bazFooBar", violationIds: [BAZ_VIOLATION_ID] },',
      '  { category: "bazFooBar", violationIds: [BAZ_VIOLATION_ID] },\n  { category: "fooBar", violationIds: ["foo-bar"] },',
    );
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(
      result.out,
      /GAP\s+classification-rule\s+FRAUD_PROOF_CLASSIFICATION_RULES row 1 but catalogue position 0/u,
    );
  });

  it("fails when the adapter registration row is missing", () => {
    const tree = edit(
      completeTree(),
      SOURCES.adapters,
      '  { category: "fooBar", status: "ready", runner: fooBarRunner },\n',
      "",
    );
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(
      result.out,
      /GAP\s+adapter-registration\s+no workflowAdapterRegistrationRows row/u,
    );
  });

  it("fails when a blueprint title has no validator file", () => {
    const tree = completeTree();
    tree["onchain/aiken/validators/fraud-proofs/foo-bar/step-02.ak"] = null;
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(result.out, /GAP\s+sdk-chain.*step_02/u);
  });

  it("fails on a missing catalogue-status row and journey owner", () => {
    let tree = edit(
      completeTree(),
      SOURCES.catalogueStatus,
      "| `00000040` | `fooBar`    | Yes               |\n",
      "",
    );
    tree = edit(tree, SOURCES.journeys, '  fooBar: "transaction",\n', "");
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(result.out, /GAP\s+catalogue-status-row/u);
    assert.match(result.out, /GAP\s+journey-owner/u);
  });

  it("reports every gap for a category that is not registered at all", () => {
    const result = run(writeTree(completeTree()), "quxQuux");
    assert.equal(result.code, EXIT_GAPS);
    assert.match(result.out, /GAP\s+catalogue-order/u);
    assert.match(result.out, /GAP\s+application-record/u);
  });

  it("exits 2, not 1, when the registry cannot be read", () => {
    const tree = completeTree();
    tree[SOURCES.registry] = null;
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_CANNOT_READ);
    assert.match(result.err, /could not look: cannot read .*registry/u);
    assert.equal(result.out, "");
  });

  it("exits 2 when the registry no longer has the parsed shape", () => {
    const tree = edit(
      completeTree(),
      SOURCES.registry,
      "FAMILY_APPLICATION_REGISTRY",
      "FAMILY_REGISTRY_RENAMED",
    );
    const result = run(writeTree(tree), "fooBar");
    assert.equal(result.code, EXIT_CANNOT_READ);
    assert.match(result.err, /FAMILY_APPLICATION_REGISTRY/u);
  });

  it("exits 64 on bad usage", () => {
    assert.equal(
      main(
        [],
        () => {},
        () => {},
      ),
      EXIT_USAGE,
    );
    assert.equal(
      main(
        ["Foo-Bar"],
        () => {},
        () => {},
      ),
      EXIT_USAGE,
    );
    assert.equal(
      main(
        ["fooBar", "--root"],
        () => {},
        () => {},
      ),
      EXIT_USAGE,
    );
  });

  // Parser-rot guard: the real tree must stay readable. This asserts only
  // that the script can look (exit 0 or 1), not that every family is complete.
  it("can read every category in the repository tree", () => {
    const repository = resolve(
      dirname(fileURLToPath(import.meta.url)),
      "../../../..",
    );
    const catalogue = readFileSync(
      join(repository, SOURCES.sdkCatalogue),
      "utf8",
    );
    const order = catalogue.slice(
      catalogue.indexOf("FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER = ["),
      catalogue.indexOf("] as const satisfies"),
    );
    const categories = [...order.matchAll(/"(\w+)"/gu)].map((m) => m[1]);
    assert.ok(categories.length > 0, "found no categories in the real tree");
    for (const category of categories) {
      const result = run(repository, category);
      assert.notEqual(
        result.code,
        EXIT_CANNOT_READ,
        `${category}: ${result.err}`,
      );
      assert.match(result.out, /catalogue-order/u);
    }
  });
});
