// Post-build step: writes the CommonJS declarations of the two entries a
// CommonJS consumer can require, `dist/index.d.cts` and
// `dist/blueprint-abi/index.d.cts`, from the ES module declarations tsup has
// just written. Building them with tsup would type-check the SDK a second
// time. Each `.d.ts` file an entry reaches gets a `.d.cts` twin with the same
// text, except that its relative specifiers name `.cjs` instead of `.js`, so
// a CommonJS consumer resolves only `.d.cts` files, exactly the declarations
// an ES module consumer resolves. The layer subpaths are ESM only (see
// tsup.config.ts); their twins exist only because the root re-exports them.
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const dist = resolve(fileURLToPath(new URL("..", import.meta.url)), "dist");
const ENTRIES = ["index.d.ts", "blueprint-abi/index.d.ts"];

// A relative specifier in an import, export, `import()` or side-effect import.
const SPECIFIER =
  /(\bfrom\s*|\bimport\s*\(\s*|^\s*import\s*)(["'])(\.\.?\/[^"']*)\2/gmu;

const pending = ENTRIES.map((entry) => resolve(dist, entry));
const written = new Set();
for (let file = pending.pop(); file !== undefined; file = pending.pop()) {
  if (written.has(file)) continue;
  written.add(file);
  const text = readFileSync(file, "utf8").replace(
    SPECIFIER,
    (_, lead, quote, specifier) => {
      if (!specifier.endsWith(".js")) {
        throw new Error(
          `${relative(dist, file)} names ${specifier}, which is not a .js specifier`,
        );
      }
      const target = resolve(
        dirname(file),
        `${specifier.slice(0, -".js".length)}.d.ts`,
      );
      if (!existsSync(target)) {
        throw new Error(
          `${relative(dist, file)} names ${specifier}, which has no declarations`,
        );
      }
      pending.push(target);
      return `${lead}${quote}${specifier.slice(0, -".js".length)}.cjs${quote}`;
    },
  );
  writeFileSync(`${file.slice(0, -".d.ts".length)}.d.cts`, text);
}
process.stdout.write(
  `wrote ${written.size} CommonJS declaration files under dist\n`,
);
