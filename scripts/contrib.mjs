#!/usr/bin/env node
import { realpathSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { artifactChannels, runArtifact } from "./contrib/artifacts.mjs";
import { reproduce } from "./contrib/reproduction.mjs";
import { buildPackage, checkBuild } from "./contrib/build.mjs";
import { captureHttp, diagnostics } from "./contrib/diagnostics.mjs";
import { checkBoundaries, symbolOwners } from "./contrib/discovery.mjs";
import { atomicJson, workspacePackages } from "./contrib/files.mjs";
import { gatePlan, GATES, runGate } from "./contrib/gates.mjs";
import { measureFiles } from "./contrib/measure.mjs";
import { buildNative } from "./contrib/native.mjs";
import {
  acceptancePlan,
  devnetPlan,
  generateDevnet,
  runAcceptance,
} from "./contrib/operations.mjs";
import { renderProgram, validateProgram } from "./contrib/program.mjs";
import { verifyReceipt } from "./contrib/receipts.mjs";
import { checkPnpm } from "./doctor.check-pnpm.mjs";
import { pinnedPnpmVersion } from "./contrib/pnpm.mjs";
import { listResources, reclaimResource } from "./contrib/resources.mjs";
import { preparationPlan, prepare, runTests } from "./contrib/tests.mjs";
import { summaryLines } from "./contrib/vitest-command.mjs";
import {
  createWorktree,
  removeWorktree,
  setupWorktree,
} from "./contrib/worktree.mjs";
import {
  applyPacket,
  createPacket,
  inspectWorkspace,
  locate,
  verifyPacket,
} from "./contrib/workspace.mjs";

export const HELP = `Midgard deterministic contributor tools (run from any cwd)
  node scripts/contrib.mjs worktree create --branch NAME [--base REF] [--package NAME]
  node scripts/contrib.mjs worktree setup [--package NAME]
  node scripts/contrib.mjs worktree remove [--root WORKTREE] [--force]
  node scripts/contrib.mjs prepare --package NAME [--plan | --execute] [--source-only]
  node scripts/contrib.mjs build --package NAME [--force]
  node scripts/contrib.mjs native --package NAME
  node scripts/contrib.mjs test --package NAME [--file PATH]... [--name REGEX] [--seed N] [--source-only]
      [--maxWorkers N] [--exclude GLOB]... [--disableConsoleIntercept]
  node scripts/contrib.mjs gate NAME [--plan] [--seed N]
  node scripts/contrib.mjs artifacts list | check --channel ID | sync --channel ID
  node scripts/contrib.mjs artifacts builds
  node scripts/contrib.mjs resources list | reclaim --resource ID
  node scripts/contrib.mjs workspace inspect
  node scripts/contrib.mjs packet create --base REF --file PATH --output FILE
  node scripts/contrib.mjs packet verify | apply --input FILE
  node scripts/contrib.mjs receipts verify | render --input FILE
  node scripts/contrib.mjs program validate | render --input FILE
  node scripts/contrib.mjs locate [--query TEXT]
  node scripts/contrib.mjs locate --symbol TEXT
  node scripts/contrib.mjs boundary --package NAME
  node scripts/contrib.mjs measure --file FILE [--file FILE] [--maximum-bytes N]
  node scripts/contrib.mjs diagnose --snapshot FILE [--package NAME]
  node scripts/contrib.mjs diagnose --url LOCAL_URL [--output FILE]
  node scripts/contrib.mjs reproduce [--plan | --execute]
  node scripts/contrib.mjs devnet plan | generate --run-id ID
  node scripts/contrib.mjs acceptance --input STACK_CONFIG [--plan]
Global: --root CHECKOUT, --output JSON_FILE. Exit 0 passed/planned, 1 failed,
2 invalid usage, 3 incomplete/skipped. Plans never execute prerequisites.
Receipts/logs live in the owner-only temporary run registry. Preserve their
directories when using them as handoff evidence. See docs/agents/contrib.md.`;

// Detailed digests belong in receipts, not thousands of terminal/context
// lines. --output preserves the complete machine-readable result.
export const compact = (value) => {
  if (value?.schema === "midgard-workspace/v1")
    return {
      schema: value.schema,
      root: value.root,
      complete: value.complete,
      worktrees: {
        total: value.worktrees.length,
        available: value.worktrees.filter(
          (tree) => tree.inspection === "available",
        ).length,
        unavailable: value.worktrees.filter(
          (tree) => tree.inspection === "unavailable",
        ).length,
      },
      current: compact(
        value.worktrees.find((tree) => tree.root === value.root),
      ),
      overlapCount: value.overlaps.length,
      overlapExamples: value.overlaps.slice(0, 10),
      resources: compact(value.resources),
      details: "Use --output FILE for all worktrees, changes and overlaps",
    };
  if (Array.isArray(value)) return value.map(compact);
  if (!value || typeof value !== "object") return value;
  return Object.fromEntries(
    Object.entries(value).map(([key, item]) =>
      key === "changes" && Array.isArray(item)
        ? ["changeCount", item.length]
        : key === "files" &&
            item &&
            !Array.isArray(item) &&
            typeof item === "object"
          ? ["fileCount", Object.keys(item).length]
          : [key, compact(item)],
    ),
  );
};

// A test receipt is long (digests of every input); the terminal gets what
// a reader acts on, and --output keeps the whole receipt.
const testView = (result) =>
  result?.schema === "midgard-contrib-receipt/v1" && result.kind === "test"
    ? {
        status: result.status,
        package: result.package,
        counts: result.counts,
        files: result.selectedFiles?.length,
        seed: result.seed,
        flags: result.flags,
        reason: result.reason,
        reportError: result.reportError,
        failures: result.failures?.slice(0, 10),
        blueprintAction: result.blueprintAction,
        databaseCleanup: result.databaseCleanup,
        logs: result.steps.map((step) => step.logPath),
        receipt: result.path,
        exitCode: result.exitCode,
      }
    : result;

const valueOptions = new Set([
  "root",
  "package",
  "file",
  "name",
  "seed",
  "channel",
  "resource",
  "base",
  "output",
  "input",
  "query",
  "symbol",
  "maximum-bytes",
  "snapshot",
  "url",
  "proof-kind",
  "run-id",
  "branch",
  "maxWorkers",
  "exclude",
]);
const flagOptions = new Set([
  "plan",
  "execute",
  "source-only",
  "force",
  "help",
  "disableConsoleIntercept",
]);
// Vitest's own flags, passed through `contrib test` under Vitest's spelling.
const vitestOptions = ["maxWorkers", "exclude", "disableConsoleIntercept"];
export const parse = (argv) => {
  const options = { files: [], excludes: [], words: [] };
  for (let index = 0; index < argv.length; index += 1) {
    const argument = argv[index];
    if (!argument.startsWith("--")) {
      options.words.push(argument);
      continue;
    }
    const key = argument.slice(2);
    if (!valueOptions.has(key) && !flagOptions.has(key))
      throw new Error(`unknown option ${argument}`);
    if (flagOptions.has(key)) {
      options[key] = true;
      continue;
    }
    const value = argv[++index];
    if (!value || value.startsWith("--"))
      throw new Error(`${argument} requires a value`);
    if (key === "file") options.files.push(value);
    else if (key === "exclude") options.excludes.push(value);
    else if (options[key] !== undefined)
      throw new Error(`duplicate ${argument}`);
    else options[key] = value;
  }
  if (options.plan && options.execute)
    throw new Error("--plan and --execute are exclusive");
  return options;
};

export const main = async (argv) => {
  let options;
  try {
    options = parse(argv);
  } catch (error) {
    console.error(`${error.message}\n${HELP}`);
    return 2;
  }
  if (options.help || options.words.length === 0) {
    console.log(HELP);
    return 0;
  }
  const root = realpathSync(
    options.root ?? fileURLToPath(new URL("..", import.meta.url)),
  );
  const [command, action, ...extra] = options.words;
  if (extra.length) {
    console.error("unexpected positional arguments");
    return 2;
  }
  const controller = new AbortController();
  const abort = () => controller.abort();
  process.on("SIGINT", abort);
  process.on("SIGTERM", abort);
  const required = (key) => {
    if (!options[key]) throw new Error(`--${key} is required`);
    return options[key];
  };
  const execution = {
    signal: controller.signal,
    sourceOnly: options["source-only"] ?? false,
    seed: Number(options.seed ?? 1),
    proofKind: options["proof-kind"] ?? "candidate-pass",
  };
  let result;
  try {
    if (
      ![
        "candidate-pass",
        "baseline-reproduction",
        "causal-guard-mutant",
        "model-inspection",
      ].includes(execution.proofKind)
    )
      throw new Error("unknown or non-synthetic proof kind");
    if (
      options.plan &&
      !["prepare", "gate", "reproduce", "acceptance"].includes(command)
    )
      throw new Error(
        `--plan is not supported for ${command}; no command was executed`,
      );
    if (options.execute && !["prepare", "reproduce"].includes(command))
      throw new Error(`--execute is not supported for ${command}`);
    if (
      options["source-only"] &&
      !["prepare", "test", "gate"].includes(command)
    )
      throw new Error(`--source-only is not supported for ${command}`);
    const vitestOption = vitestOptions.find((key) =>
      key === "exclude" ? options.excludes.length : options[key] !== undefined,
    );
    if (vitestOption && command !== "test")
      throw new Error(`--${vitestOption} is supported only for test`);
    if (
      options.force &&
      command !== "build" &&
      !(command === "worktree" && action === "remove")
    )
      throw new Error(`--force is not supported for ${command}`);
    if (
      [
        "build",
        "native",
        "prepare",
        "test",
        "boundary",
        "acceptance",
        "gate",
        "artifacts",
        "reproduce",
        "worktree",
      ].includes(command) &&
      action !== "remove" &&
      !options.plan &&
      !(command === "prepare" && !options.execute) &&
      !(command === "artifacts" && ["list", "builds"].includes(action)) &&
      !(command === "reproduce" && !options.execute) &&
      action !== "list"
    ) {
      const pnpm = checkPnpm({ root, run: pinnedPnpmVersion });
      if (pnpm.status !== "ok") throw new Error(`${pnpm.detail}; ${pnpm.fix}`);
    }
    if (command === "build" && !action) {
      result = await buildPackage(root, required("package"), {
        ...execution,
        force: options.force ?? false,
      });
      if (result.status === "fresh")
        console.error(
          `contrib build ${result.package}: fresh: skipped (dist matches its digest stamp; --force rebuilds)`,
        );
      else if (result.unstamped)
        console.error(
          `contrib build ${result.package}: built, but its ${result.reason}`,
        );
    } else if (command === "native" && !action)
      result = await buildNative(root, required("package"), execution);
    else if (command === "worktree" && action === "create")
      result = await createWorktree(root, {
        ...execution,
        branch: required("branch"),
        base: options.base,
        packageName: options.package,
      });
    else if (command === "worktree" && action === "setup")
      result = await setupWorktree(root, {
        ...execution,
        packageName: options.package,
      });
    else if (command === "worktree" && action === "remove")
      result = await removeWorktree(root, { force: options.force ?? false });
    else if (command === "prepare" && !action)
      result = options.execute
        ? await prepare(root, required("package"), execution)
        : preparationPlan(root, required("package"), execution);
    else if (command === "test" && !action)
      result = await runTests(root, required("package"), {
        ...execution,
        files: options.files,
        testName: options.name,
        flags: {
          maxWorkers: options.maxWorkers,
          exclude: options.excludes,
          disableConsoleIntercept: options.disableConsoleIntercept,
        },
      });
    else if (command === "gate")
      result =
        action === "list"
          ? Object.keys(GATES)
          : options.plan
            ? gatePlan(root, action)
            : await runGate(root, action, execution);
    else if (command === "artifacts" && action === "list")
      result = artifactChannels(root).channels.map(
        ({ id, summary, check, sync }) => ({ id, summary, check, sync }),
      );
    else if (command === "artifacts" && action === "builds")
      result = workspacePackages(root)
        .filter((pkg) => pkg.scripts?.build)
        .map((pkg) => ({ package: pkg.name, ...checkBuild(root, pkg.name) }));
    else if (command === "artifacts" && ["check", "sync"].includes(action))
      result = await runArtifact(root, required("channel"), {
        ...execution,
        sync: action === "sync",
      });
    else if (command === "resources" && action === "list")
      result = listResources();
    else if (command === "resources" && action === "reclaim") {
      reclaimResource(required("resource"));
      result = { reclaimed: options.resource };
    } else if (command === "workspace" && action === "inspect")
      result = inspectWorkspace(root);
    else if (command === "packet" && action === "create")
      result = createPacket(root, {
        base: required("base"),
        files: options.files,
        output: resolve(required("output")),
      });
    else if (command === "packet" && action === "verify")
      result = verifyPacket(root, required("input"));
    else if (command === "packet" && action === "apply")
      result = await applyPacket(root, required("input"), execution);
    else if (command === "receipts" && ["verify", "render"].includes(action)) {
      result = verifyReceipt(root, required("input"));
      if (action === "render")
        result = result.steps.map((step) => ({
          argv: step.argv,
          cwd: step.cwd,
          exitCode: step.exitCode,
          date: step.endedAt,
          counts: result.counts,
          proofKind: result.proofKind,
        }));
    } else if (
      command === "program" &&
      ["validate", "render"].includes(action)
    ) {
      result = validateProgram(root, required("input"));
      if (action === "render") result = renderProgram(result);
    } else if (command === "locate" && !action)
      result = options.symbol
        ? symbolOwners(root, options.symbol)
        : locate(root, options.query);
    else if (command === "boundary" && !action)
      result = await checkBoundaries(root, required("package"), execution);
    else if (command === "measure" && !action)
      result = await measureFiles(options.files, {
        ...execution,
        maximumBytes: Number(
          options["maximum-bytes"] ?? Number.MAX_SAFE_INTEGER,
        ),
      });
    else if (command === "diagnose" && !action)
      result = options.url
        ? await captureHttp(options.url, execution)
        : diagnostics(root, {
            snapshot: required("snapshot"),
            name: options.package,
          });
    else if (command === "reproduce" && !action)
      result = await reproduce(root, {
        ...execution,
        execute: options.execute,
      });
    else if (command === "devnet" && ["plan", "generate"].includes(action))
      result =
        action === "plan"
          ? devnetPlan(root, required("run-id"))
          : await generateDevnet(root, required("run-id"), execution);
    else if (command === "acceptance" && !action)
      result = options.plan
        ? acceptancePlan(root, required("input"))
        : await runAcceptance(root, required("input"), execution);
    else
      throw new Error(`unknown command ${options.words.join(" ")}; use --help`);
    if (options.output && !(command === "packet" && action === "create"))
      atomicJson(resolve(options.output), result);
    if (
      result?.schema === "midgard-contrib-receipt/v1" &&
      result.kind === "test"
    )
      console.error(
        summaryLines(result, {
          limit: result.status === "passed" ? 0 : 10,
        }).join("\n"),
      );
    console.log(
      typeof result === "string"
        ? result
        : JSON.stringify(compact(testView(result)), null, 2),
    );
    return result?.exitCode ?? 0;
  } catch (error) {
    console.error(`contrib: ${error.message}`);
    return 1;
  } finally {
    process.off("SIGINT", abort);
    process.off("SIGTERM", abort);
  }
};

if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
)
  process.exitCode = await main(process.argv.slice(2));
