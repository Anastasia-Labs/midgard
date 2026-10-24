import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
import { resolve, relative } from "node:path";

import {
  filesUnder,
  packageByName,
  inputIdentity,
  atomicJson,
  runtimeBuildClosure,
  outputIdentity,
} from "./files.mjs";
import { realpathSync } from "node:fs";
import { withResource } from "./resources.mjs";
import { sourceFacetPaths } from "../lib/source-facets.mjs";
import { checkBuild, buildPackage, runDirectory } from "./build.mjs";
import { writeReceipt } from "./receipts.mjs";
import { probeInstalledPackage } from "./installed-boundary.mjs";

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
      const installed = await probeInstalledPackage({
        root,
        pkg,
        directory,
        env: ownedEnv,
        signal,
      });
      const receipt = writeReceipt({
        root,
        pkg,
        directory,
        kind: "boundary",
        before,
        after: inputIdentity(root, name),
        steps: installed.steps,
      });
      receipt.artifacts = artifacts;
      receipt.evidenceFiles = installed.evidenceFiles;
      receipt.surfaces = installed.surfaces;
      if (installed.reason) {
        receipt.status = "failed";
        receipt.exitCode = 1;
        receipt.reason = installed.reason;
      }
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
