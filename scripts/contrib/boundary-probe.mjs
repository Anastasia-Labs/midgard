// Copied into an isolated consumer by installed-boundary.mjs. No repository
// imports or custom source conditions may participate in package resolution.
import { realpathSync, writeFileSync } from "node:fs";
import { createRequire, registerHooks } from "node:module";
import { relative, isAbsolute } from "node:path";
import { fileURLToPath } from "node:url";

const root = realpathSync(process.env.MIDGARD_BOUNDARY_CONSUMER);
registerHooks({
  resolve(specifier, context, nextResolve) {
    const result = nextResolve(specifier, context);
    if (result.url.startsWith("file:")) {
      const path = realpathSync(fileURLToPath(result.url));
      const local = relative(root, path);
      if (local === ".." || local.startsWith("../") || isAbsolute(local))
        throw new Error(
          `package resolved outside isolated consumer: ${specifier} -> ${path}`,
        );
    }
    return result;
  },
});

// --import also uses this module as the isolation guard for installed CLIs.
if (process.env.MIDGARD_BOUNDARY_MODE) {
  const mode = process.env.MIDGARD_BOUNDARY_MODE;
  const require = createRequire(import.meta.url);
  const loaded = [];
  for (const specifier of JSON.parse(process.env.MIDGARD_BOUNDARY_EXPORTS)) {
    const resolved =
      mode === "import"
        ? import.meta.resolve(specifier)
        : require.resolve(specifier);
    if (mode === "import") await import(specifier);
    else require(specifier);
    loaded.push({ specifier, resolved });
    console.log(JSON.stringify({ mode, specifier, resolved }));
  }
  // A package can terminate with exit 0 while initializing. Only this terminal
  // record proves that every requested import/require actually returned.
  writeFileSync(
    process.env.MIDGARD_BOUNDARY_COMPLETION,
    JSON.stringify({ mode, loaded }),
    { flag: "wx", mode: 0o600 },
  );
}
