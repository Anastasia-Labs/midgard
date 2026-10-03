import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
import { resolve, relative } from "node:path";

import {
  filesUnder,
  packageByName,
  inside,
  inputIdentity,
  atomicJson,
  runtimeBuildClosure,
  outputIdentity,
} from "./files.mjs";
import { realpathSync } from "node:fs";
import { withResource } from "./resources.mjs";
import { sourceFacetPaths } from "../lib/source-facets.mjs";
import { checkBuild, buildPackage, runDirectory } from "./build.mjs";
import { runProcess } from "./process.mjs";
import { writeReceipt } from "./receipts.mjs";

export const symbolOwners = (root, query) => {
  const ts = createRequire(resolve(root, "demo/midgard-core/package.json"))(
    "typescript",
  );
  const found = [];
  for (const file of filesUnder(resolve(root, "demo")).filter((path) =>
    /\.[cm]?tsx?$/u.test(path),
  )) {
    const source = ts.createSourceFile(
      file,
      readFileSync(file, "utf8"),
      ts.ScriptTarget.Latest,
      true,
    );
    const visit = (node) => {
      if (
        node.name &&
        ts.isIdentifier(node.name) &&
        node.name.text.toLowerCase().includes(query.toLowerCase())
      ) {
        const owner = [
          ts.SyntaxKind.FunctionDeclaration,
          ts.SyntaxKind.ClassDeclaration,
          ts.SyntaxKind.InterfaceDeclaration,
          ts.SyntaxKind.TypeAliasDeclaration,
          ts.SyntaxKind.VariableDeclaration,
        ].includes(node.kind);
        if (owner)
          found.push({
            path: relative(root, file),
            symbol: node.name.text,
            line:
              source.getLineAndCharacterOfPosition(node.getStart()).line + 1,
            kind: ts.SyntaxKind[node.kind],
            facets: sourceFacetPaths(file).map((path) => relative(root, path)),
          });
      }
      ts.forEachChild(node, visit);
    };
    visit(source);
  }
  return found;
};

export const checkBoundaries = async (
  root,
  name,
  { signal, env = process.env } = {},
) =>
  withResource(
    `workspace:${realpathSync(root)}`,
    async (ownedEnv) => {
      const pkg = packageByName(root, name);
      if (checkBuild(root, name).status !== "fresh") {
        const built = await buildPackage(root, name, { signal, env: ownedEnv });
        if (built.exitCode !== 0) return built;
      }
      const directory = runDirectory();
      const before = inputIdentity(root, name);
      const artifacts = runtimeBuildClosure(root, name).map((entry) => ({
        name: entry.name,
        outputs: checkBuild(root, entry.name).stamp.outputs,
      }));
      const bins =
        typeof pkg.bin === "string" ? [pkg.bin] : Object.values(pkg.bin ?? {});
      const targets = Object.entries(pkg.exports ?? {}).flatMap(
        ([key, value]) => {
          if (key.includes("*") || typeof value !== "object" || !value.import)
            return [];
          return [
            {
              role: key,
              path: inside(root, resolve(root, pkg.directory, value.import)),
            },
          ];
        },
      );
      if (
        pkg.main &&
        !bins.some(
          (bin) =>
            resolve(root, pkg.directory, bin) ===
            resolve(root, pkg.directory, pkg.main),
        ) &&
        !targets.some(
          (entry) => entry.path === resolve(root, pkg.directory, pkg.main),
        )
      )
        targets.push({
          role: "main",
          path: inside(root, resolve(root, pkg.directory, pkg.main)),
        });
      if (!targets.length && !pkg.bin)
        throw new Error(`no declared runtime exports or CLI for ${name}`);
      const steps = [];
      // Sequential imports expose actual ESM initialization order/cycles. Child
      // processes load import/default dist conditions, never Vitest's src alias.
      if (targets.length)
        steps.push(
          await runProcess({
            argv: [
              process.execPath,
              "--input-type=module",
              "-e",
              "for (const path of JSON.parse(process.argv[1])) await import(path);",
              JSON.stringify(targets.map((entry) => entry.path)),
            ],
            cwd: resolve(root, pkg.directory),
            env: ownedEnv,
            signal,
            logPath: resolve(directory, "exports.log"),
            echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
          }),
        );
      for (const bin of bins)
        steps.push(
          await runProcess({
            argv: [
              process.execPath,
              inside(root, resolve(root, pkg.directory, bin)),
              "--help",
            ],
            cwd: resolve(root, pkg.directory),
            env: ownedEnv,
            signal,
            logPath: resolve(directory, `cli-${steps.length}.log`),
            echo: process.env.MIDGARD_CONTRIB_VERBOSE === "1",
          }),
        );
      const receipt = writeReceipt({
        root,
        pkg,
        directory,
        kind: "boundary",
        before,
        after: inputIdentity(root, name),
        steps,
      });
      receipt.artifacts = artifacts;
      if (
        artifacts.some(
          ({ name, outputs }) =>
            checkBuild(root, name).status !== "fresh" ||
            outputs.sha256 !==
              outputIdentity(
                root,
                `${packageByName(root, name).directory}/dist`,
              ).sha256,
        )
      ) {
        receipt.status = "failed";
        receipt.exitCode = 1;
        receipt.reason = "compiled artifacts changed during boundary execution";
      }
      atomicJson(receipt.path, receipt);
      return receipt;
    },
    { signal, env },
  );
