/**
 * The trace producers see the transaction and the walk, never the replay
 * verdict. The driver alone compares a producer's outcome with replay, so a
 * producer that imports replay's verdict types could steer its own stops.
 */
import { existsSync, readdirSync, readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import * as ts from "typescript";
import { describe, expect, it } from "vitest";

const SOURCE_ROOT = fileURLToPath(new URL("../src/", import.meta.url));
const MACHINE_ROOT = join(SOURCE_ROOT, "validation-machine");

/** Every module the walk hands a phase to. */
const PRODUCER_MODULE =
  /^trace-builder-(?:prepare|complete)(?:\.[\w-]+)?\.ts$/u;

/** Replay and its verdict: the producers may name none of these. */
const REPLAY_MODULES = [
  "phase-a.ts",
  "phase-b.ts",
  "validation-machine/trace-replay.ts",
  "validation-machine/trace-builder-stop.ts",
].map((path) => join(SOURCE_ROOT, path));
const REPLAY_NAMES = new Set([
  "RejectedTx",
  "RejectCode",
  "RejectCodes",
  "ValidationMachineReplayInput",
  "ValidationReplay",
  "ValidationTraceStopped",
]);

type ProducerImport = {
  readonly module: string;
  readonly names: readonly string[];
};

const moduleSpecifiers = (source: ts.SourceFile): ProducerImport[] => {
  const found: ProducerImport[] = [];
  const visit = (node: ts.Node): void => {
    if (
      (ts.isImportDeclaration(node) || ts.isExportDeclaration(node)) &&
      node.moduleSpecifier !== undefined &&
      ts.isStringLiteral(node.moduleSpecifier)
    ) {
      const names: string[] = [];
      if (ts.isImportDeclaration(node)) {
        const bindings = node.importClause?.namedBindings;
        if (bindings !== undefined && ts.isNamedImports(bindings)) {
          for (const element of bindings.elements) {
            names.push((element.propertyName ?? element.name).text);
          }
        }
      } else if (
        node.exportClause !== undefined &&
        ts.isNamedExports(node.exportClause)
      ) {
        for (const element of node.exportClause.elements) {
          names.push((element.propertyName ?? element.name).text);
        }
      }
      found.push({ module: node.moduleSpecifier.text, names });
    } else if (
      ts.isImportTypeNode(node) &&
      ts.isLiteralTypeNode(node.argument) &&
      ts.isStringLiteral(node.argument.literal)
    ) {
      found.push({
        module: node.argument.literal.text,
        names:
          node.qualifier === undefined
            ? []
            : [
                ts.isIdentifier(node.qualifier)
                  ? node.qualifier.text
                  : node.qualifier.right.text,
              ],
      });
    } else if (
      ts.isCallExpression(node) &&
      node.expression.kind === ts.SyntaxKind.ImportKeyword &&
      node.arguments[0] !== undefined &&
      ts.isStringLiteral(node.arguments[0])
    ) {
      found.push({ module: node.arguments[0].text, names: [] });
    }
    ts.forEachChild(node, visit);
  };
  visit(source);
  return found;
};

const resolvedSourcePath = (from: string, specifier: string): string =>
  specifier.startsWith(".")
    ? resolve(dirname(from), specifier).replace(/\.js$/u, ".ts")
    : specifier;

describe("trace walk producers", () => {
  const producers = readdirSync(MACHINE_ROOT)
    .filter((name) => PRODUCER_MODULE.test(name))
    .map((name) => join(MACHINE_ROOT, name));

  it("are found, and the replay modules they must not name exist", () => {
    expect(producers.length).toBeGreaterThanOrEqual(2);
    expect(REPLAY_MODULES.filter((path) => !existsSync(path))).toEqual([]);
  });

  it("import nothing from replay or its verdict", () => {
    const violations = producers.flatMap((path) => {
      const source = ts.createSourceFile(
        path,
        readFileSync(path, "utf8"),
        ts.ScriptTarget.Latest,
        true,
      );
      return moduleSpecifiers(source).flatMap(({ module, names }) => {
        const target = resolvedSourcePath(path, module);
        const local = path.slice(MACHINE_ROOT.length + 1);
        return [
          ...(REPLAY_MODULES.includes(target)
            ? [`${local} imports ${module}`]
            : []),
          ...names
            .filter((name) => REPLAY_NAMES.has(name))
            .map((name) => `${local} imports ${name} from ${module}`),
        ];
      });
    });
    expect(violations).toEqual([]);
  });
});
