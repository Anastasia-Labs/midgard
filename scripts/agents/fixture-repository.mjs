// Throwaway git repositories for the agent-check tests. Each one has its own
// git configuration and excludes, never the developer's, and the calling
// process drops any GIT_* variables it inherited (a hook running the tests
// sets GIT_DIR and GIT_INDEX_FILE, which would point the checks at the outer
// repository). Every directory made here is removed when the process exits.

import { execFileSync, spawnSync } from "node:child_process";
import {
  mkdirSync,
  mkdtempSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";

const created = [];
process.on("exit", () => {
  for (const directory of created)
    rmSync(directory, { recursive: true, force: true });
});

export const temporaryDirectory = (prefix) => {
  const directory = realpathSync(mkdtempSync(join(tmpdir(), prefix)));
  created.push(directory);
  return directory;
};

export const isolateGit = () => {
  for (const key of Object.keys(process.env))
    if (key.startsWith("GIT_")) delete process.env[key];
  const base = temporaryDirectory("midgard-agent-git-");
  const globalConfig = join(base, "gitconfig");
  writeFileSync(
    globalConfig,
    "[user]\n\tname = Fixture\n\temail = fixture@example.invalid\n[init]\n\tdefaultBranch = main\n",
  );
  // The default global excludes file lives under HOME or XDG_CONFIG_HOME;
  // point both at the sandbox so personal ignore patterns cannot leak in.
  process.env.HOME = base;
  process.env.XDG_CONFIG_HOME = join(base, "config");
  process.env.GIT_CONFIG_GLOBAL = globalConfig;
  process.env.GIT_CONFIG_NOSYSTEM = "1";
  process.env.GIT_CEILING_DIRECTORIES = tmpdir();
};

// Writes `files` ({ path: content }) into a new repository and stages them;
// `untracked` files are written but not staged.
export const fixtureRepository = (files, untracked = {}) => {
  const root = temporaryDirectory("midgard-agent-fixture-");
  execFileSync("git", ["init", "--quiet"], { cwd: root });
  const write = (entries) => {
    for (const [path, content] of Object.entries(entries)) {
      mkdirSync(dirname(join(root, path)), { recursive: true });
      writeFileSync(join(root, path), content);
    }
  };
  write(files);
  write(untracked);
  const paths = Object.keys(files);
  if (paths.length > 0)
    execFileSync("git", ["add", "--", ...paths], { cwd: root });
  return root;
};

export const runScript = (script, args) =>
  spawnSync(process.execPath, [script, ...args], {
    encoding: "utf8",
    env: process.env,
  });
