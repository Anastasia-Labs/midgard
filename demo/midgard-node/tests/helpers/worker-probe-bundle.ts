import { mkdtemp, rm, symlink } from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, join } from "node:path";

import { build as bundleWithTsup } from "tsup";

const packageRoot = join(import.meta.dirname, "../..");

/**
 * Bundles a worker-thread probe under tests/helpers from source, outside the
 * package tree. A worker thread resolves workspace packages through their
 * dist, so they are bundled through the `midgard-source` condition; the
 * remaining imports resolve through the package's node_modules, linked into
 * the bundle's directory. Returns the bundle's path and its removal.
 */
export const bundleWorkerProbe = async (
  probe: string,
): Promise<{ readonly file: string; readonly remove: () => Promise<void> }> => {
  const directory = await mkdtemp(join(tmpdir(), "midgard-worker-probe-"));
  const remove = () => rm(directory, { recursive: true, force: true });
  try {
    await symlink(
      join(packageRoot, "node_modules"),
      join(directory, "node_modules"),
      "dir",
    );
    await bundleWithTsup({
      entry: [join(packageRoot, "tests/helpers", probe)],
      format: ["esm"],
      platform: "node",
      target: "node22",
      outDir: directory,
      config: false,
      splitting: false,
      silent: true,
      noExternal: [/^@al-ft\//],
      loader: { ".sql": "text" },
      banner: {
        js: 'import { createRequire as __createRequire } from "node:module"; const require = __createRequire(import.meta.url);',
      },
      esbuildOptions(options) {
        options.conditions = ["midgard-source", ...(options.conditions ?? [])];
      },
    });
  } catch (error) {
    await remove();
    throw error;
  }
  return {
    file: join(directory, basename(probe).replace(/\.ts$/u, ".js")),
    remove,
  };
};
