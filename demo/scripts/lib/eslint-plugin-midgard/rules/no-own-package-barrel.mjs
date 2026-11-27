// A package's own code imports the module that declares a name, not the
// package's barrel (`src/index.ts`).
//
// `contrib test --related` and `--cached` pick the tests an edit reaches from
// the import graph (scripts/contrib/reached.mjs). A barrel re-exports the whole
// package, so a test or source module importing it depends on every module of
// the package and reruns on every edit. The barrel stays the published entry
// point for other packages; inside the package nothing imports it. The rule
// applies to the packages listed below, whose imports have been narrowed; a
// package joins the list once it has none left. Flagged: a static import or
// re-export, or a literal dynamic `import()`, whose specifier is the package's
// own name or a relative path to its `src`, `src/index` or `src/index.<ext>`.
// Blind spot: it reads specifiers, so a namespace import of a narrower module
// that itself re-exports the package (a sub-barrel) passes, and so does a
// `require` or a computed path.
//
// The baselined sites assert the names the root publishes or sit at their
// module-size cap. A name a test stubs through a re-exporting module
// (`vi.spyOn` on its namespace, or `vi.mock` of it) stays imported from that
// module: the stub replaces only that module's binding.

import { existsSync, readFileSync } from "node:fs";
import { join, posix } from "node:path";

import { defineRule } from "../baseline.mjs";

export const NARROWED_PACKAGES = [
  "midgard-fault-proofs",
  "midgard-node",
  "midgard-watcher",
];

const INDEX = /^src(?:\/index(?:\.[cm]?[jt]sx?)?)?$/u;

const packageNames = new Map();
const packageName = (root, directory) => {
  if (!packageNames.has(directory)) {
    const manifest = join(root, directory, "package.json");
    packageNames.set(
      directory,
      existsSync(manifest)
        ? JSON.parse(readFileSync(manifest, "utf8")).name
        : undefined,
    );
  }
  return packageNames.get(directory);
};

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Inside a narrowed package, import the declaring module rather than the package's own barrel.",
    },
    schema: [],
    messages: {
      ownBarrel:
        "`{{specifier}}` is this package's own barrel, which re-exports every module: whatever imports it reruns on every edit to the package under `contrib test --related` and `--cached`. Fix: import each name from the module that declares it (for example `../src/<module>.js`); the barrel is the entry point for other packages only.",
    },
  },
  create: (context, report, file) => {
    const directory = file.split("/")[0];
    if (!NARROWED_PACKAGES.includes(directory)) return {};
    if (INDEX.test(posix.relative(directory, file).replace(/\.ts$/u, ""))) {
      // The barrel itself.
      return {};
    }
    const root = context.settings?.midgard?.root ?? context.cwd;
    const name = packageName(root, directory);
    const check = (source) => {
      if (source?.type !== "Literal" || typeof source.value !== "string")
        return;
      const specifier = source.value;
      const own =
        specifier === name ||
        (specifier.startsWith(".") &&
          INDEX.test(
            posix.relative(
              directory,
              posix.join(posix.dirname(file), specifier),
            ),
          ));
      if (own)
        report({ node: source, messageId: "ownBarrel", data: { specifier } });
    };
    return {
      ImportDeclaration: (node) => check(node.source),
      ExportNamedDeclaration: (node) => check(node.source),
      ExportAllDeclaration: (node) => check(node.source),
      ImportExpression: (node) => check(node.source),
    };
  },
});
