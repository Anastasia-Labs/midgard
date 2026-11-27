#!/usr/bin/env node
import { spawn } from "node:child_process";
import { resolve } from "node:path";

// Select the project before Corepack resolves packageManager. This also works
// for bare pnpm calls inside registered generator and nested build recipes.
const args = process.argv.slice(2);
let cwd = process.cwd();
let selected = false;
while (args.length && args[0].startsWith("-")) {
  const option = args[0];
  if (["--dir", "-C"].includes(option) || /^(?:--dir|-C)=/u.test(option)) {
    if (selected) throw new Error("pnpm directory selected more than once");
    const value = option.includes("=")
      ? args.shift().split("=").slice(1).join("=")
      : (args.shift(), args.shift());
    if (!value || value.startsWith("-"))
      throw new Error("pnpm directory needs a path");
    cwd = resolve(cwd, value);
    selected = true;
  } else break;
}
const child = spawn("corepack", ["pnpm", ...args], { cwd, stdio: "inherit" });
child.on("error", (error) => {
  console.error(error.message);
  process.exitCode = 1;
});
child.on("exit", (code, signal) => {
  process.exitCode = code ?? (signal ? 1 : 0);
});
