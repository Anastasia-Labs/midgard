import { spawn } from "node:child_process";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { constants } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

// Package manifests own task argv, environment and prerequisites. Both the
// repository guard and isolated Docker builds execute these same steps.
const object = (value) =>
  value && typeof value === "object" && !Array.isArray(value);
const fail = (pkg, message) => {
  throw new Error(`${pkg.name}: midgardTasks ${message}`);
};
const keys = (pkg, value, allowed, label) => {
  if (!object(value)) fail(pkg, `${label} must be an object`);
  for (const key of Object.keys(value))
    if (!allowed.includes(key)) fail(pkg, `${label}.${key} is unknown`);
};
const environment = (pkg, value, label) => {
  if (value === undefined) return;
  if (
    !object(value) ||
    Object.entries(value).some(
      ([key, entry]) =>
        !/^[A-Za-z_][A-Za-z0-9_]*$/u.test(key) ||
        typeof entry !== "string" ||
        entry.includes("\0"),
    )
  )
    fail(pkg, `${label} must map environment names to literal strings`);
};
const command = (pkg, step, label) => {
  keys(pkg, step, ["command", "environment", "appendEnvironment"], label);
  if (
    !Array.isArray(step.command) ||
    step.command.length === 0 ||
    step.command.some(
      (word) => typeof word !== "string" || word.includes("\0"),
    ) ||
    !step.command[0]
  )
    fail(pkg, `${label}.command must be a nonempty argv array`);
  environment(pkg, step.environment, `${label}.environment`);
  environment(pkg, step.appendEnvironment, `${label}.appendEnvironment`);
  for (const name of Object.keys(step.appendEnvironment ?? {}))
    if (Object.hasOwn(step.environment ?? {}, name))
      fail(pkg, `${label} both replaces and appends ${name}`);
};
const prerequisites = (pkg, entries = [], kind) => {
  if (!Array.isArray(entries))
    fail(pkg, `${kind}.prerequisites must be an array`);
  const seen = new Set();
  for (const entry of entries) {
    keys(pkg, entry, ["kind", "package"], `${kind}.prerequisite`);
    if (
      !(
        kind === "build"
          ? ["build"]
          : ["build", "blueprint", "postgres", "native"]
      ).includes(entry.kind)
    )
      fail(pkg, `${kind} prerequisite kind ${entry.kind} is unknown`);
    if (["build", "native"].includes(entry.kind)) {
      if (typeof entry.package !== "string" || !entry.package)
        fail(pkg, `${entry.kind} prerequisite needs a package`);
      if (
        entry.package !== pkg.name &&
        !Object.hasOwn(
          { ...pkg.dependencies, ...pkg.devDependencies },
          entry.package,
        )
      )
        fail(
          pkg,
          `${entry.package} prerequisite must be a declared workspace dependency`,
        );
      if (kind === "build" && entry.package === pkg.name)
        fail(pkg, "a build cannot depend on itself");
    } else if (entry.package !== undefined)
      fail(pkg, `${entry.kind} prerequisite has no package`);
    const identity = `${entry.kind}:${entry.package ?? ""}`;
    if (seen.has(identity)) fail(pkg, `duplicate ${identity} prerequisite`);
    seen.add(identity);
  }
};
export const packageTasks = (pkg) => {
  const tasks = pkg.midgardTasks ?? {};
  keys(pkg, tasks, ["build", "test"], "declaration");
  if (tasks.build !== undefined) {
    const task = tasks.build;
    keys(pkg, task, ["steps", "prerequisites", "outputs"], "build");
    if (!Array.isArray(task.steps) || !task.steps.length)
      fail(pkg, "build.steps must be nonempty");
    task.steps.forEach((step, index) =>
      command(pkg, step, `build.steps[${index}]`),
    );
    // dist holds the build stamp; any other output is a further top-level
    // directory the build writes and owns (validation's vendor/).
    if (
      !Array.isArray(task.outputs) ||
      task.outputs[0] !== "dist" ||
      new Set(task.outputs).size !== task.outputs.length ||
      task.outputs.some(
        (output) =>
          typeof output !== "string" || !/^[a-z][a-z0-9-]*$/u.test(output),
      )
    )
      fail(
        pkg,
        'build.outputs must be "dist" then any other top-level directories the build writes',
      );
    prerequisites(pkg, task.prerequisites, "build");
  }
  if (tasks.test !== undefined) {
    const task = tasks.test;
    keys(
      pkg,
      task,
      ["command", "environment", "prerequisites", "before"],
      "test",
    );
    command(
      pkg,
      { command: task.command, environment: task.environment },
      "test",
    );
    if (task.command[0] !== "vitest" || task.command[1] !== "run")
      fail(pkg, "test.command must start with vitest run");
    prerequisites(pkg, task.prerequisites, "test");
    if (task.before !== undefined && !Array.isArray(task.before))
      fail(pkg, "test.before must be an array");
    for (const [index, step] of (task.before ?? []).entries()) {
      command(pkg, step, `test.before[${index}]`);
      if (
        step.command.length !== 3 ||
        step.command[0] !== "pnpm" ||
        step.command[1] !== "run" ||
        !pkg.scripts?.[step.command[2]]
      )
        fail(pkg, "test.before must name an existing pnpm run script");
    }
  }
  return tasks;
};
export const packageBuildTask = (pkg) => packageTasks(pkg).build;
// The repository paths of what a package's build writes: dist, and any
// further outputs its build declares.
export const packageOutputDirectories = (pkg) =>
  (packageBuildTask(pkg)?.outputs ?? ["dist"]).map(
    (output) => `${pkg.directory}/${output}`,
  );
export const packageTestTask = (pkg) => packageTasks(pkg).test;
export const taskEnvironment = (step, env = process.env) => ({
  ...env,
  ...step.environment,
  ...Object.fromEntries(
    Object.entries(step.appendEnvironment ?? {}).map(([name, suffix]) => [
      name,
      `${env[name] ?? ""}${suffix}`,
    ]),
  ),
});
export const taskProcess = (step, cwd, env = process.env) => {
  const [program, ...args] = step.command;
  if (program === "node")
    return {
      argv: [process.execPath, ...args],
      cwd,
      env: taskEnvironment(step, env),
    };
  if (program === "tsup" || program === "vitest") {
    const require = createRequire(resolve(cwd, "package.json"));
    const manifestPath = require.resolve(`${program}/package.json`);
    const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
    const bin =
      typeof manifest.bin === "string" ? manifest.bin : manifest.bin[program];
    return {
      argv: [process.execPath, resolve(dirname(manifestPath), bin), ...args],
      cwd,
      env: taskEnvironment(step, env),
    };
  }
  return { argv: [program, ...args], cwd, env: taskEnvironment(step, env) };
};
export const runTaskProcess = ({ argv: [executable, ...args], ...options }) =>
  new Promise((done, reject) => {
    const child = spawn(executable, args, {
      ...options,
      stdio: "inherit",
      shell: false,
    });
    child.once("error", reject);
    child.once("exit", (status, signal) =>
      done(status ?? (signal ? 128 + constants.signals[signal] : 1)),
    );
  });
export const rawBuild = async (cwd) => {
  const pkg = JSON.parse(readFileSync(resolve(cwd, "package.json"), "utf8"));
  const task = packageBuildTask(pkg);
  if (!task) fail(pkg, "has no build declaration");
  for (const step of task.steps) {
    const code = await runTaskProcess(taskProcess(step, cwd));
    if (code !== 0) return code;
  }
  return 0;
};
if (
  process.argv[1] &&
  resolve(process.argv[1]) === fileURLToPath(import.meta.url)
) {
  if (process.argv[2] !== "build" || process.argv.length !== 3)
    throw new Error("usage: package-tasks.mjs build (in package directory)");
  process.exitCode = await rawBuild(process.cwd());
}
