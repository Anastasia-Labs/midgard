#!/usr/bin/env node
import {
  cpSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { runDirectory } from "./build.mjs";
import { atomicJson } from "./files.mjs";
import { runProcess } from "./process.mjs";

const root = fileURLToPath(new URL("../..", import.meta.url));
const directory = runDirectory();
const controls = [
  {
    name: "receipt-wall-clock-order",
    file: "receipts.mjs",
    change: (text) =>
      text.replace(
        "  step.exitCode === 0 &&",
        "  Date.parse(step.endedAt) >= Date.parse(step.startedAt) && step.exitCode === 0 &&",
      ),
    match: "receipt timing uses monotonic duration",
    failure: /incomplete\/failed step|not a completed pass/u,
    testFile: "receipts.test.mjs",
  },
  {
    name: "receipt-monotonic-duration",
    file: "receipts.mjs",
    change: (text) =>
      text.replace(
        /Number\.isFinite\(step\.durationMs\) &&\s*step\.durationMs >= 0/u,
        "true",
      ),
    match: "receipt timing uses monotonic duration",
    failure: /invalid execution timing cannot earn a pass/u,
    testFile: "receipts.test.mjs",
  },
  {
    name: "native-output-partition",
    file: "files.mjs",
    change: (text) =>
      text.replace(/!nativeOutputs\.some\([\s\S]*?\) &&/u, "true &&"),
    match: "native outputs have separate ownership",
    failure: /adding the separately guarded Go binary/u,
    testFile: "identity.test.mjs",
  },
  {
    name: "reproduction-final-native",
    file: "reproduction.mjs",
    change: (text) =>
      text.replace(
        /      for \(const name of plan\.nativePackages\) \{\n        const verdict = checkNative[\s\S]*?\n      \}/u,
        "",
      ),
    match: "reproduction refuses a native output deleted",
    failure: /deleted native output cannot earn a reproduction pass/u,
    testFile: "operations.test.mjs",
  },
  {
    name: "workspace-missing-registration",
    file: "workspace.mjs",
    change: (text) =>
      text.replace(
        /      \} catch \(error\) \{[\s\S]*?\n      \}/u,
        "      } catch (error) { throw error; }",
      ),
    match: "workspace inventory reports missing registered worktrees",
    failure: /spawnSync git ENOENT/u,
    testFile: "workspace.test.mjs",
  },
  {
    name: "scratch-staged-deletions",
    file: "artifacts.mjs",
    change: (text) =>
      text.replace(
        '      ...headEntries.map((entry) => entry.slice(entry.indexOf("\\t") + 1)),\n',
        "",
      ),
    match: "scratch candidates preserve staged deletions and rename removals",
    failure: /AssertionError/u,
    testFile: "artifacts.test.mjs",
  },
  {
    name: "devnet-run-directory",
    file: "operations.mjs",
    change: (text) =>
      text.replace(
        '          MIDGARD_PHASE4_RUN_DIR: resolve(directory, "devnet"),\n',
        "",
      ),
    match: "devnet generation supplies a private fresh run directory",
    failure: /AssertionError/u,
    testFile: "operations.test.mjs",
  },
  {
    name: "dispatcher-inputs",
    file: "files.mjs",
    change: (text) =>
      text
        .replace('  "scripts/bin",\n', "")
        .replace('  "scripts/pnpm.mjs",\n', "")
        .replace('  "scripts/bin/pnpm",\n', ""),
    match: "lockfile, exports, native sources and dependency facets invalidate",
    failure: /AssertionError/u,
    testFile: "identity.test.mjs",
  },
  {
    name: "scratch-complete-inputs",
    file: "artifacts.mjs",
    change: (text) =>
      text.replace(
        /            if \(inputIdentity\(scratch, pkg.name\).sha256 !== before.sha256\)\s+throw new Error\(\s*"scratch build inputs differ from destination inputs",?\s*\);/u,
        "",
      ),
    match: "artifact sync refuses destination input drift",
    failure: /unexecuted package inputs cannot earn a pass/u,
    testFile: "artifacts.test.mjs",
  },
  {
    name: "scratch-output-basename",
    file: "artifacts.mjs",
    change: (text) =>
      text.replace(
        "!sourceInput(root, path) ||",
        '!sourceInput(root, path) || path.split("/").includes("dist") ||',
      ),
    match: "scratch overlays candidate sources inside output-named directories",
    failure: /AssertionError/u,
    testFile: "artifacts.test.mjs",
  },
  {
    name: "packet-output-basename",
    file: "workspace.mjs",
    change: (text) =>
      text.replace(
        "!sourceInput(root, path) ||",
        '!sourceInput(root, path) || path.split("/").includes("dist") ||',
      ),
    match: "packets permit candidate sources inside output-named directories",
    failure: /packet path is protected/u,
    testFile: "workspace.test.mjs",
  },
  {
    name: "source-output-basename",
    file: "files.mjs",
    change: (text) =>
      text.replace(
        "if (!outputs && excludedInput(directory, entry.name, source)) continue;",
        'if (!outputs && (packageOutputs.has(entry.name) || ["build", "target"].includes(entry.name) || excludedInput(directory, entry.name, source))) continue;',
      ),
    match: "source directories named like outputs remain bound",
    failure: /AssertionError/u,
    testFile: "identity.test.mjs",
  },
  {
    name: "repository-generated-outputs",
    file: "files.mjs",
    change: (text) =>
      text.replace(
        "const identity = hashFiles(root, repositorySources);",
        "const identity = hashFiles(root, filesUnder(root));",
      ),
    match: "repository receipts ignore generated site and spec outputs",
    failure: /AssertionError/u,
    testFile: "identity.test.mjs",
  },
  {
    name: "sync-input-race",
    file: "artifacts.mjs",
    change: (text) =>
      text.replace(
        /      const assertSyncDestination = [\s\S]*?\n      \};/u,
        "      const assertSyncDestination = () => {};",
      ),
    match: "artifact sync refuses destination input drift",
    failure: /AssertionError/u,
    testFile: "artifacts.test.mjs",
  },
  {
    name: "resource-namespace",
    file: "resources.mjs",
    change: (text) =>
      text.replace(
        /  if \(owner.namespace && owner.namespace !== self.namespace\) return "unknown";/u,
        "",
      ),
    match: "another PID namespace and an unfinished launch remain unknown",
    failure: /AssertionError/u,
    testFile: "lifecycle.test.mjs",
  },
  {
    name: "resource-children",
    file: "resources.mjs",
    change: (text) =>
      text.replace(
        "return groupsState(directory, record.token);",
        'return "abandoned";',
      ),
    match:
      "a killed lease owner cannot be reclaimed while its detached managed child still runs",
    failure: /AssertionError/u,
    testFile: "lifecycle.test.mjs",
  },
  {
    name: "channel-inputs",
    file: "receipts.mjs",
    change: (text) =>
      text.replace(/  if \(receipt.channel\) \{[\s\S]*?\n  \}/u, ""),
    match:
      "artifact receipts invalidate changed or newly added explicit channel inputs",
    failure: /AssertionError/u,
    testFile: "artifacts.test.mjs",
  },
  {
    name: "boundary-restamp",
    file: "discovery.mjs",
    change: (text) =>
      text.replace(/outputs\.sha256 !==[\s\S]*?\.sha256/u, "false"),
    match:
      "boundary compares its execution snapshot even when a child restamps",
    failure: /AssertionError/u,
  },
  {
    name: "artifact-consumer",
    file: "artifacts.mjs",
    change: (text) =>
      text.replace(
        /          if \(!unchangedArtifacts\(root, artifacts\)\)\s*throw new Error\(\s*"compiled artifacts changed during artifact check"\s*,?\s*\);/u,
        "",
      ),
    match: "artifact checks reject self-mutating compiled consumers",
    failure: /AssertionError/u,
    testFile: "artifacts.test.mjs",
  },
  {
    name: "boundary-cli-main",
    file: "discovery.mjs",
    change: (text) =>
      text.replace(/        !bins\.some\([\s\S]*?        \) &&\n/u, ""),
    match: "a CLI main shared with its bin is probed with help",
    failure: /AssertionError/u,
  },
  {
    name: "boundary-path",
    file: "discovery.mjs",
    change: (text) =>
      text.replaceAll(
        "resolve(root, pkg.directory, value.import)",
        "resolve(pkg.directory, value.import)",
      ),
    match: "boundary imports resolve against the selected root",
    failure: /expected a file inside/u,
  },
  {
    name: "compiled-dependency",
    file: "build.mjs",
    change: (text) =>
      text.replace(
        /    const dependencies = compiledDependencies[\s\S]*?    return \{ status: "fresh", stamp \};/u,
        '    return { status: "fresh", stamp };',
      ),
    match: "compiled dependency substitution invalidates",
    failure: /AssertionError/u,
  },
  {
    name: "boundary-artifact",
    file: "discovery.mjs",
    change: (text) =>
      text.replace(
        /    if \(\s*artifacts\.some[\s\S]*?    atomicJson\(receipt.path, receipt\);/u,
        "    atomicJson(receipt.path, receipt);",
      ),
    match: "unguarded dist mutation during import fails",
    failure: /AssertionError/u,
  },
];
const results = [];
try {
  for (const control of [{ name: "fixed" }, ...controls]) {
    const scratch = mkdtempSync(resolve(tmpdir(), "midgard-contrib-redcheck-"));
    try {
      cpSync(resolve(root, "scripts"), resolve(scratch, "scripts"), {
        recursive: true,
      });
      for (const directory of ["onchain/aiken/scripts", "demo/scripts/lib"])
        cpSync(resolve(root, directory), resolve(scratch, directory), {
          recursive: true,
        });
      if (control.file) {
        const path = resolve(scratch, "scripts/contrib", control.file);
        const original = readFileSync(path, "utf8");
        const mutant = control.change(original);
        if (mutant === original)
          throw new Error(
            `control anchor moved: ${control.name}; update the causal mutant`,
          );
        writeFileSync(path, mutant);
      }
      const step = await runProcess({
        argv: [
          process.execPath,
          "--test",
          ...(control.match ? ["--test-name-pattern", control.match] : []),
          ...(!control.file
            ? [
                "scripts/contrib/artifacts.test.mjs",
                "scripts/contrib/boundary.test.mjs",
                "scripts/contrib/operations.test.mjs",
                "scripts/contrib/identity.test.mjs",
                "scripts/contrib/receipts.test.mjs",
              ]
            : [`scripts/contrib/${control.testFile ?? "boundary.test.mjs"}`]),
        ],
        cwd: scratch,
        logPath: resolve(directory, `${control.name}.log`),
      });
      const output = readFileSync(step.logPath, "utf8");
      const expected = control.file
        ? step.exitCode !== 0 &&
          control.failure.test(output) &&
          !/SyntaxError|ERR_MODULE_NOT_FOUND/u.test(output)
        : step.exitCode === 0;
      results.push({ control: control.name, expected, step });
      if (!expected)
        throw new Error(
          `causal control did not reach its expected assertion: ${step.logPath}`,
        );
    } finally {
      rmSync(scratch, { recursive: true, force: true });
    }
  }
  atomicJson(resolve(directory, "controls.json"), results);
  console.log(
    JSON.stringify(
      {
        status: "passed",
        controls: results.map(({ control, step }) => ({
          control,
          exitCode: step.exitCode,
        })),
        evidence: resolve(directory, "controls.json"),
      },
      null,
      2,
    ),
  );
} catch (error) {
  console.error(error.message);
  process.exitCode = 1;
}
