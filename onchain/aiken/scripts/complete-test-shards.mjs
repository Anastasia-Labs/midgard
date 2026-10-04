import { spawnSync } from "node:child_process";
import { createHash, randomInt } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  realpathSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { delimiter, dirname, join, relative, resolve } from "node:path";
import { isDeepStrictEqual } from "node:util";

import { assertPinnedAiken, pinnedAikenFork } from "./pinned-compiler.mjs";

const digest = (value) => createHash("sha256").update(value).digest("hex");
const integer = (value, maximum) =>
  Number.isSafeInteger(value) && value >= 0 && value <= maximum;
const environmentName = /^[a-z0-9_-]+$/u;
const sourcePath =
  /^(?:[a-z][a-z0-9_-]*\/)*[a-z][a-z0-9_-]*(?:\.[a-z0-9_-]*)*\.ak$/u;
const polarities = new Set([
  "fail_immediately",
  "succeed_immediately",
  "succeed_eventually",
]);
const refuse = (message) => {
  throw new Error(`Aiken shards: ${message}`);
};

// Match the pinned compiler's source roots and path normalization, not test
// declarations. Every definition in every matching module is selected.
export const readShardSources = (projectDirectory) => {
  const sources = [];
  const visit = (directory, root, ancestors = new Set()) => {
    if (!existsSync(directory)) return;
    const real = realpathSync(directory);
    if (ancestors.has(real)) refuse("source directory symlink cycle");
    const next = new Set([...ancestors, real]);
    for (const name of readdirSync(directory).sort()) {
      const path = join(directory, name);
      const physical = relative(
        realpathSync(projectDirectory),
        realpathSync(path),
      ).replaceAll("\\", "/");
      if (physical === ".." || physical.startsWith("../"))
        refuse("source symlink escapes project");
      if (statSync(path).isDirectory()) visit(path, root, next);
      else if (name.endsWith(".ak")) {
        const modulePath = relative(root, path).replaceAll("\\", "/");
        if (!sourcePath.test(modulePath))
          refuse(`invalid module path ${modulePath}`);
        const content = readFileSync(path);
        sources.push({
          path: relative(projectDirectory, path).replaceAll("\\", "/"),
          module: modulePath.slice(0, -3).replaceAll("-", "_"),
          bytes: content.length,
          sha256: digest(content),
        });
      }
    }
  };
  for (const root of ["lib", "validators", "env"])
    visit(join(projectDirectory, root), join(projectDirectory, root));
  for (const path of ["aiken.toml", "aiken.lock"]) {
    const content = readFileSync(join(projectDirectory, path));
    sources.push({ path, bytes: content.length, sha256: digest(content) });
  }
  return sources.sort((a, b) =>
    a.path < b.path ? -1 : a.path > b.path ? 1 : 0,
  );
};

export const createShardPlan = (
  projectDirectory,
  count,
  seed,
  environment = null,
) => {
  if (!integer(count, 16) || count < 1 || !integer(seed, 0xffffffff))
    refuse("invalid shard count or uint32 seed");
  if (environment !== null && !environmentName.test(environment))
    refuse("invalid environment");
  const sources = readShardSources(projectDirectory);
  const moduleSources = sources.filter((source) => source.module !== undefined);
  const modules = [
    ...new Set([
      ...moduleSources.map((source) => source.module),
      "env",
      "config",
    ]),
  ].sort();
  // Config is generated from TOML constants; env may be used as an alias.
  // Include both conservatively, even when neither has test definitions.
  const prefixes = [
    ...new Set(modules.map((module) => module.split(".")[0])),
  ].sort();
  const parents = prefixes.map((_, index) => index);
  const find = (index) => {
    while (parents[index] !== index) {
      parents[index] = parents[parents[index]];
      index = parents[index];
    }
    return index;
  };
  for (const module of modules) {
    const matches = prefixes
      .map((prefix, index) => (module.includes(prefix) ? index : -1))
      .filter((index) => index !== -1);
    for (const index of matches.slice(1))
      parents[find(index)] = find(matches[0]);
  }
  const components = new Map();
  for (const [index, prefix] of prefixes.entries()) {
    const key = find(index);
    if (!components.has(key)) components.set(key, []);
    components.get(key).push(prefix);
  }
  const groups = [...components.values()]
    .map((selectors) => {
      const selected = modules.filter((module) =>
        selectors.some((selector) => module.includes(selector)),
      );
      const bytes = moduleSources
        .filter((source) => selected.includes(source.module))
        .reduce((sum, source) => sum + source.bytes, 0);
      return { selectors, modules: selected, bytes };
    })
    .sort(
      (a, b) => b.bytes - a.bytes || (a.selectors[0] < b.selectors[0] ? -1 : 1),
    );
  const shards = Array.from({ length: count }, (_, index) => ({
    index: index + 1,
    modules: [],
    selectors: [],
    bytes: 0,
  }));
  for (const group of groups) {
    const shard = [...shards].sort(
      (a, b) => a.bytes - b.bytes || a.index - b.index,
    )[0];
    shard.modules.push(...group.modules);
    shard.selectors.push(...group.selectors);
    shard.bytes += group.bytes;
  }
  for (const shard of shards) {
    shard.modules.sort();
    // Remove redundant filters only when their complete module coverage is
    // retained. No test-name filter or test count participates in planning.
    const remaining = new Set(shard.modules);
    const selectors = [];
    while (remaining.size > 0) {
      const selector = [...shard.selectors].sort(
        (a, b) =>
          [...remaining].filter((module) => module.includes(b)).length -
            [...remaining].filter((module) => module.includes(a)).length ||
          a.length - b.length ||
          (a < b ? -1 : 1),
      )[0];
      selectors.push(`${selector}.`);
      for (const module of remaining)
        if (module.includes(selector)) remaining.delete(module);
    }
    shard.selectors = selectors;
  }
  const compiler = pinnedAikenFork();
  const plan = {
    schema: "midgard-aiken-shards-v1",
    sourceHash: digest(JSON.stringify({ sources, compiler })),
    compiler,
    environment,
    seed,
    maxSuccess: 100,
    count,
    shards,
  };
  return { ...plan, planHash: digest(JSON.stringify(plan)) };
};

export const validateShardPlan = (plan, projectDirectory) => {
  const current = createShardPlan(
    projectDirectory,
    plan?.count,
    plan?.seed,
    plan?.environment,
  );
  if (!isDeepStrictEqual(plan, current))
    refuse("plan differs from current complete source/module assignments");
  const owners = new Map();
  for (const shard of plan.shards) {
    for (const module of shard.modules) {
      if (owners.has(module)) refuse(`duplicate module assignment ${module}`);
      owners.set(module, shard.index);
    }
  }
  for (const [module, owner] of owners) {
    const matching = plan.shards.filter((shard) =>
      shard.selectors.some((selector) =>
        module.includes(selector.slice(0, -1)),
      ),
    );
    if (matching.length !== 1 || matching[0].index !== owner)
      refuse(`module ${module} is not selected exactly once`);
  }
  return plan;
};

export const shardArguments = (plan, index) => {
  const shard = plan.shards.find((entry) => entry.index === index);
  if (shard === undefined || shard.selectors.length === 0)
    refuse("unknown or empty shard");
  const args = [
    "check",
    "--plain-numbers",
    "--seed",
    String(plan.seed),
    "--max-success",
    "100",
  ];
  for (const selector of shard.selectors) args.push("-m", selector);
  if (plan.environment !== null) args.push("--env", plan.environment);
  return args;
};

export const validateShardArtifact = (artifact, plan) => {
  const metadata = artifact?.metadata;
  if (
    artifact?.schema !== "midgard-aiken-shard-report-v1" ||
    metadata?.planHash !== plan.planHash ||
    metadata?.sourceHash !== plan.sourceHash ||
    metadata?.environment !== plan.environment ||
    metadata?.seed !== plan.seed ||
    metadata?.maxSuccess !== 100 ||
    metadata?.count !== plan.count ||
    !isDeepStrictEqual(metadata?.compiler?.pin, plan.compiler) ||
    metadata?.compiler?.version !== plan.compiler.version ||
    !/^[0-9a-f]{64}$/u.test(metadata?.compiler?.sha256 ?? "") ||
    !isDeepStrictEqual(metadata?.args, shardArguments(plan, metadata?.index))
  )
    refuse("artifact source/compiler/environment/seed/shard metadata differs");
  if (
    metadata.exitStatus !== 0 ||
    metadata.signal !== null ||
    metadata.error !== null
  )
    refuse("compiler did not exit successfully");
  if (
    typeof artifact.rawStdout !== "string" ||
    metadata.rawStdoutSha256 !== digest(artifact.rawStdout)
  )
    refuse("raw compiler report identity differs");
  let report;
  try {
    report = JSON.parse(artifact.rawStdout);
  } catch {
    refuse("compiler report is not valid JSON");
  }
  if (report?.seed !== plan.seed || !Array.isArray(report?.modules))
    refuse("report seed or modules differ");
  const shard = plan.shards.find((entry) => entry.index === metadata.index);
  const ids = new Map();
  const seenModules = new Set();
  const kinds = { unit: 0, property: 0 };
  for (const module of report.modules) {
    if (
      !shard.modules.includes(module?.name) ||
      seenModules.has(module.name) ||
      !Array.isArray(module.tests) ||
      module.tests.length === 0
    )
      refuse("report has an unexpected, duplicated or empty module");
    seenModules.add(module.name);
    const moduleKinds = { unit: 0, property: 0 };
    for (const test of module.tests) {
      if (
        typeof test?.title !== "string" ||
        test.title.length === 0 ||
        test.status !== "pass" ||
        !polarities.has(test.on_failure)
      )
        refuse("test identity, status or polarity is invalid");
      const key = JSON.stringify([module.name, test.title]);
      if (ids.has(key)) refuse("duplicate test ID");
      const property = Object.hasOwn(test, "iterations");
      if (property) {
        if (
          !integer(test.iterations, 100) ||
          test.iterations < 1 ||
          !(
            test.counterexample === null ||
            typeof test.counterexample === "string"
          ) ||
          Object.hasOwn(test, "execution_units")
        )
          refuse("invalid property test");
        if (
          test.on_failure !== "succeed_immediately" &&
          (test.iterations !== 100 || test.counterexample !== null)
        )
          refuse("successful property test did not preserve all100iterations");
        if (
          test.on_failure === "succeed_immediately" &&
          test.counterexample === null
        )
          refuse("expected-failure property lacks its counterexample");
      } else if (
        !integer(test.execution_units?.mem, Number.MAX_SAFE_INTEGER) ||
        !integer(test.execution_units?.cpu, Number.MAX_SAFE_INTEGER)
      )
        refuse("invalid unit execution units");
      moduleKinds[property ? "property" : "unit"] += 1;
      kinds[property ? "property" : "unit"] += 1;
      ids.set(key, test);
    }
    const expected = {
      total: module.tests.length,
      passed: module.tests.length,
      failed: 0,
      kind: moduleKinds,
    };
    if (!isDeepStrictEqual(module.summary, expected))
      refuse("module summary/kinds differ from actual tests");
  }
  const summary = report.summary;
  if (
    ids.size === 0 ||
    !integer(summary?.total, Number.MAX_SAFE_INTEGER) ||
    summary.total !== ids.size ||
    summary.passed !== ids.size ||
    summary.failed !== 0 ||
    !isDeepStrictEqual(summary.kind, kinds)
  )
    refuse("nonzero all-pass summary/kinds differ from actual tests");
  return { ids, summary, report };
};

const writeExclusive = (path, value) => {
  mkdirSync(dirname(resolve(path)), { recursive: true });
  writeFileSync(path, JSON.stringify(value, null, 2) + "\n", { flag: "wx" });
};
const binaryPath = (binary) => {
  const paths = binary.includes("/")
    ? [resolve(binary)]
    : (process.env.PATH ?? "")
        .split(delimiter)
        .map((directory) => join(directory, binary));
  const found = paths.find(
    (path) => existsSync(path) && statSync(path).isFile(),
  );
  if (found === undefined) refuse("compiler binary cannot be resolved");
  return realpathSync(found);
};

export const runShardCheck = ({
  plan,
  index,
  output,
  binary,
  projectDirectory,
}) => {
  validateShardPlan(plan, projectDirectory);
  if ((process.env.MIDGARD_AIKEN_ENV ?? null) !== plan.environment)
    refuse("current environment differs from plan");
  if (existsSync(output)) refuse("report destination already exists");
  const args = shardArguments(plan, index);
  const version = assertPinnedAiken(binary);
  const compiler = {
    pin: plan.compiler,
    version,
    sha256: digest(readFileSync(binaryPath(binary))),
  };
  const result = spawnSync(binary, args, {
    cwd: projectDirectory,
    encoding: "utf8",
    maxBuffer: 64 * 1024 * 1024,
  });
  const rawStdout = result.stdout ?? "";
  const artifact = {
    schema: "midgard-aiken-shard-report-v1",
    metadata: {
      planHash: plan.planHash,
      sourceHash: plan.sourceHash,
      compiler,
      environment: plan.environment,
      seed: plan.seed,
      maxSuccess: 100,
      index,
      count: plan.count,
      args,
      exitStatus: result.status,
      signal: result.signal ?? null,
      error: result.error?.message ?? null,
      rawStdoutSha256: digest(rawStdout),
    },
    rawStdout,
    rawStderr: result.stderr ?? "",
  };
  // Keep complete raw output even when JSON parsing or process status fails.
  writeExclusive(output, artifact);
  if (result.stderr) process.stderr.write(result.stderr);
  validateShardPlan(plan, projectDirectory);
  const { summary } = validateShardArtifact(artifact, plan);
  return { index, ...summary };
};

export const collectShardReports = ({
  plan,
  projectDirectory,
  reportsDirectory,
  baseline,
}) => {
  validateShardPlan(plan, projectDirectory);
  const files = readdirSync(reportsDirectory)
    .filter((name) => name.endsWith(".json"))
    .sort();
  const expected = plan.shards
    .map((shard) => `shard-${shard.index}.json`)
    .sort();
  if (!isDeepStrictEqual(files, expected))
    refuse("missing or unexpected shard report files");
  const ids = new Map();
  const summary = {
    total: 0,
    passed: 0,
    failed: 0,
    kind: { unit: 0, property: 0 },
  };
  for (const shard of plan.shards) {
    const artifact = JSON.parse(
      readFileSync(join(reportsDirectory, `shard-${shard.index}.json`), "utf8"),
    );
    if (artifact?.metadata?.index !== shard.index)
      refuse("shard file has another shard identity");
    const validated = validateShardArtifact(artifact, plan);
    for (const [id, test] of validated.ids) {
      if (ids.has(id)) refuse("duplicate test ID across shards");
      ids.set(id, test);
    }
    for (const kind of ["unit", "property"])
      summary.kind[kind] += validated.summary.kind[kind];
    summary.total += validated.summary.total;
    summary.passed += validated.summary.passed;
  }
  if (baseline !== undefined) {
    const report = JSON.parse(readFileSync(baseline, "utf8"));
    const original = new Map();
    for (const module of report.modules)
      for (const test of module.tests) {
        const id = JSON.stringify([module.name, test.title]);
        if (original.has(id)) refuse("duplicate baseline test ID");
        original.set(id, test);
      }
    if (
      report.seed !== plan.seed ||
      !isDeepStrictEqual(report.summary, summary) ||
      original.size !== ids.size ||
      [...original].some(([id, test]) => !isDeepStrictEqual(ids.get(id), test))
    )
      refuse("all-shard union differs from complete baseline records");
  }
  return {
    planHash: plan.planHash,
    sourceHash: plan.sourceHash,
    seed: plan.seed,
    shards: plan.count,
    ...summary,
  };
};

export const runShardInvocation = (args, context) => {
  const mode = args[0];
  const offset = mode === "--collect-shards" ? 1 : 2;
  if ((args.length - offset) % 2 !== 0) refuse("invalid shard command options");
  const options = new Map();
  for (let index = offset; index < args.length; index += 2) {
    if (options.has(args[index]) || args[index + 1] === undefined)
      refuse("duplicate or missing shard option");
    options.set(args[index], args[index + 1]);
  }
  const allowed =
    mode === "--plan-shards"
      ? ["--seed", "--output"]
      : mode === "--shard"
        ? ["--plan", "--output"]
        : ["--plan", "--reports", "--baseline"];
  if ([...options.keys()].some((key) => !allowed.includes(key)))
    refuse("unknown shard option");
  if (mode === "--plan-shards") {
    if (!options.has("--output")) refuse("plan needs --output");
    const seed = options.has("--seed")
      ? Number(options.get("--seed"))
      : randomInt(0, 0x100000000);
    const plan = createShardPlan(
      context.projectDirectory,
      Number(args[1]),
      seed,
      context.environment ?? null,
    );
    writeExclusive(options.get("--output"), plan);
    return { planHash: plan.planHash, seed: plan.seed, shards: plan.count };
  }
  if (!options.has("--plan")) refuse("command needs --plan");
  const plan = JSON.parse(readFileSync(options.get("--plan"), "utf8"));
  if ((context.environment ?? null) !== plan.environment)
    refuse("current environment differs from plan");
  if (mode === "--shard") {
    if (!options.has("--output")) refuse("shard needs --output");
    return runShardCheck({
      ...context,
      plan,
      index: Number(args[1]),
      output: options.get("--output"),
    });
  }
  if (mode !== "--collect-shards" || !options.has("--reports"))
    refuse("collector needs --reports");
  return collectShardReports({
    ...context,
    plan,
    reportsDirectory: options.get("--reports"),
    baseline: options.get("--baseline"),
  });
};
