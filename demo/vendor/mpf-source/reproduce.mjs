import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import { mkdtempSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = dirname(fileURLToPath(import.meta.url));
const receipt = JSON.parse(readFileSync(join(root, "package-receipt.json")));
const provenance = JSON.parse(readFileSync(join(root, "provenance.json")));
const digest = (file) =>
  createHash("sha256").update(readFileSync(file)).digest("hex");
const check = (file, expected) => {
  if (digest(file) !== expected) throw new Error(`SHA256 mismatch: ${file}`);
};
check(join(root, "../mpf-1.3.1-midgard.1.tgz"), receipt.tarballSha256);
check(join(root, "off-chain-source.tgz"), provenance.sourceArchiveSha256);
const scratch = mkdtempSync(join(tmpdir(), "midgard-mpf-rebuild-"));
const cwd = join(scratch, "off-chain");
const run = (command, args, options = {}) =>
  execFileSync(command, args, { cwd, stdio: "inherit", ...options });
try {
  execFileSync("tar", [
    "xzf",
    join(root, "off-chain-source.tgz"),
    "-C",
    scratch,
  ]);
  for (const [file, expected] of Object.entries(provenance.sourceFiles)) {
    check(join(scratch, file), expected);
  }
  run("npm", ["ci", "--ignore-scripts", "--cache", join(scratch, "cache")]);
  run("npm", ["run", "build"]);
  run("npm", [
    "pack",
    "--ignore-scripts",
    "--cache",
    join(scratch, "cache"),
    "--pack-destination",
    scratch,
  ]);
  const archive = join(
    scratch,
    "aiken-lang-merkle-patricia-forestry-1.3.1-midgard.1.tgz",
  );
  const members = execFileSync("tar", ["tzf", archive], { encoding: "utf8" })
    .trim()
    .split("\n")
    .sort();
  if (
    JSON.stringify(members) !==
    JSON.stringify(Object.keys(receipt.members).sort())
  ) {
    throw new Error(
      "Rebuilt archive member list differs from the package receipt",
    );
  }
  run("tar", ["xzf", archive, "-C", scratch]);
  for (const [file, expected] of Object.entries(receipt.members))
    check(join(scratch, file), expected);
  console.log(
    `Reproduced all ${members.length} MPF package members; archive bytes may differ across npm versions.`,
  );
} finally {
  rmSync(scratch, { recursive: true, force: true });
}
