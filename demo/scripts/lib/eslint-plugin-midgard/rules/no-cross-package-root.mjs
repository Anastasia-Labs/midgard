// Another package's code imports a subpath of a narrowed package, not its
// root entry.
//
// `contrib test --related` and `--cached` pick the tests an edit reaches from
// the import graph (scripts/contrib/reached.mjs). A package root re-exports
// the whole package, so a file of another package importing it depends on
// every module of that package and reruns on every edit to it. The packages
// listed below publish a subpath for each cohesive part of their `src`
// (their `exports` map); a consumer imports the subpath that declares the
// names it uses. A package joins the list once its consumers have been
// narrowed. Its own files are held to `no-own-package-barrel` instead.
// Flagged: a static import or re-export, or a literal dynamic `import()`,
// whose specifier is exactly a listed package's name, in a file outside that
// package. Blind spot: it reads specifiers, so it does not check that a
// subpath declares every imported name (a name published only by the root is
// baselined), and a `require`, a computed path or a `vi.mock` of the root
// passes.
//
// A name with no cohesive subpath yet gets one added to the `exports` map.
// The baselined sites are imports whose names a test stubs on the root (the
// stub replaces only the root's binding), the evaluation-order preambles of
// modules that load one of them, the settlement worker's route to the
// fault-proofs root that `settlement-worker-module-graph.test.ts` asserts, a
// dynamic import inside a test's mock body, files at their module-size cap,
// and a module whose compiled dist a test loads by URL together with that
// test (the workspace bundle serves both one instance only through the root).

import { defineRule } from "../baseline.mjs";

export const NARROWED_ROOTS = new Map([
  ["@al-ft/midgard-fault-proofs", "midgard-fault-proofs"],
  ["@al-ft/midgard-validation", "midgard-validation"],
]);

export default defineRule({
  meta: {
    type: "problem",
    docs: {
      description:
        "Outside a narrowed package, import its subpaths rather than its root entry.",
    },
    schema: [],
    messages: {
      crossPackageRoot:
        "`{{specifier}}` is the root entry of a narrowed package and re-exports every module of it: whatever imports it reruns on every edit to that package under `contrib test --related` and `--cached`. Fix: import each name from the subpath that declares it (the `exports` map of demo/{{directory}}/package.json, for example `{{specifier}}/<module>`).",
    },
  },
  create: (context, report, file) => {
    const owner = file.split("/")[0];
    const check = (source) => {
      if (source?.type !== "Literal" || typeof source.value !== "string")
        return;
      const specifier = source.value;
      const directory = NARROWED_ROOTS.get(specifier);
      if (directory === undefined || directory === owner) return;
      report({
        node: source,
        messageId: "crossPackageRoot",
        data: { specifier, directory },
      });
    };
    return {
      ImportDeclaration: (node) => check(node.source),
      ExportNamedDeclaration: (node) => check(node.source),
      ExportAllDeclaration: (node) => check(node.source),
      ImportExpression: (node) => check(node.source),
    };
  },
});
