/** Only the shared runner and the funding permit write a workflow journal.
 * A family that needs its own journal writer has grown its own runner. */
import { readdirSync, readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { fileURLToPath } from "node:url";

import * as ts from "typescript";
import { expect, it } from "vitest";

const DEMO_ROOT = fileURLToPath(new URL("../../", import.meta.url));
const SCANNED_ROOTS = ["midgard-fault-proofs/src", "midgard-watcher/src"];
const WRITER_VERBS = new Set([
  "appendWorkflowJournalEvents",
  "commitWithJournal",
  "createStoredWorkflowJournalWriter",
]);
const WRITERS =
  /^midgard-fault-proofs\/src\/workflow\/(?:orchestrator|funding-reservation-permit)(?:\.[^/]+)?\.ts$/u;
const DEFINITION =
  "midgard-fault-proofs/src/workflow/funding-reservation-permit.workflow-journal-writer.ts";

const sourcesBelow = (directory: string): string[] =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = join(directory, entry.name);
    if (entry.isDirectory()) return sourcesBelow(path);
    return entry.name.endsWith(".ts") ? [path] : [];
  });

/** Every name of a writer verb a module mentions: an import, a re-export, or
 * a property read through a namespace import. */
const writerVerbsIn = (path: string): string[] => {
  const source = ts.createSourceFile(
    path,
    readFileSync(path, "utf8"),
    ts.ScriptTarget.Latest,
    false,
    ts.ScriptKind.TS,
  );
  const found = new Set<string>();
  const visit = (node: ts.Node): void => {
    if (ts.isIdentifier(node) && WRITER_VERBS.has(node.text))
      found.add(node.text);
    ts.forEachChild(node, visit);
  };
  visit(source);
  return [...found].sort();
};

it("keeps the workflow journal writer verbs inside the shared runner and the funding permit", () => {
  // The scan sees the verbs where they are defined.
  expect(writerVerbsIn(join(DEMO_ROOT, DEFINITION))).toEqual(
    [...WRITER_VERBS].sort(),
  );
  const offenders = SCANNED_ROOTS.flatMap((root) =>
    sourcesBelow(join(DEMO_ROOT, root)),
  ).flatMap((path) => {
    const module = relative(DEMO_ROOT, path).split("\\").join("/");
    if (WRITERS.test(module)) return [];
    const verbs = writerVerbsIn(path);
    return verbs.length === 0 ? [] : [`${module}: ${verbs.join(", ")}`];
  });
  expect(offenders).toEqual([]);
});
