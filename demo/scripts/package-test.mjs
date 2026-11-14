import { readFileSync, realpathSync } from "node:fs";
import { resolve } from "node:path";
import { pinnedPnpm } from "../../scripts/contrib/pnpm.mjs";
import { withResource } from "../../scripts/contrib/resources.mjs";
import {
  prepareArtifacts,
  prepareNativeArtifacts,
} from "../../scripts/contrib/tests.mjs";
import {
  packageTestTask,
  runTaskProcess,
  taskEnvironment,
  taskProcess,
} from "./package-tasks.mjs";

const cwd = process.cwd();
const root = realpathSync(resolve(cwd, "../.."));
const pkg = JSON.parse(readFileSync(resolve(cwd, "package.json"), "utf8"));
const task = packageTestTask(pkg);
if (!task) throw new Error(`${pkg.name}: no midgardTasks.test declaration`);
await withResource(`workspace:${root}`, async (ownedEnv) => {
  await prepareArtifacts(root, [pkg.name], { env: ownedEnv });
  await prepareNativeArtifacts(root, pkg.name, { env: ownedEnv });
});
// Ordinary package scripts preserve Vitest's argv, environment and exit status.
// Contrib separately owns selected-file evidence and disposable database policy.
for (const step of task.before ?? []) {
  const code = await runTaskProcess({
    ...pinnedPnpm(cwd, step.command.slice(1)),
    env: taskEnvironment(step),
  });
  if (code !== 0) process.exit(code);
}
process.exitCode = await runTaskProcess(
  taskProcess(
    { ...task, command: [...task.command, ...process.argv.slice(2)] },
    cwd,
  ),
);
