// Post-build stamp: records a digest of the exact sources the dist was built
// from so gate preflights can refuse to run against a stale dist. Consumers
// resolve @al-ft/midgard-core from the gitignored dist/, so on a fresh
// checkout (or after pulling a src change) a stale dist silently serves
// retired constants — the 8,273 -> 12,810 rebind (#597 ruling b, b4b6c488)
// is the motivating incident. The digest is verified by
// demo/scripts/assert-midgard-core-dist-current.mjs.
import { createHash } from "node:crypto";
import { readdir, readFile, writeFile } from "node:fs/promises";
import { relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const packageRoot = resolve(fileURLToPath(new URL("..", import.meta.url)));

export const computeMidgardCoreSourceDigest = async () => {
  const sourceRoot = resolve(packageRoot, "src");
  const files = (
    await readdir(sourceRoot, { recursive: true, withFileTypes: true })
  )
    .filter((entry) => entry.isFile())
    .map((entry) => resolve(entry.parentPath, entry.name))
    .sort((left, right) => (left < right ? -1 : left > right ? 1 : 0));
  const hash = createHash("sha256");
  // The build script string participates: changing the tsup entry list or
  // flags invalidates the stamp exactly like a source edit does.
  const packageJson = JSON.parse(
    await readFile(resolve(packageRoot, "package.json"), "utf8"),
  );
  hash.update("build\0");
  hash.update(packageJson.scripts.build);
  for (const file of files) {
    hash.update("\0file\0");
    hash.update(relative(packageRoot, file));
    hash.update("\0");
    hash.update(await readFile(file));
  }
  return hash.digest("hex");
};

const isMain = process.argv[1] === fileURLToPath(import.meta.url);
if (isMain) {
  const digest = await computeMidgardCoreSourceDigest();
  const stampPath = resolve(packageRoot, "dist/.source-digest-v1.json");
  await writeFile(
    stampPath,
    `${JSON.stringify({ schema: "midgard-core-dist-source-digest/v1", sha256: digest }, null, 2)}\n`,
    "utf8",
  );
  process.stdout.write(`dist source digest stamped: ${digest}\n`);
}
