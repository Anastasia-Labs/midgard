import { dirname, resolve, sep } from "node:path";
import { fileURLToPath } from "node:url";

import { vi } from "vitest";

/**
 * Setup file of every workspace-bundle project (see `workspaceBundleProjects`
 * in `vitest.js`). In such a project only the test files and the package's
 * `tests/` directory load from source; everything else is loaded natively
 * from the bundle, so a module mock of anything outside `tests/`, or a
 * module-registry reset, would silently stop reaching code it reaches in a
 * source-mode run. The static routing in `workspace-bundle-analysis.js` sends
 * those files to the source project; this guard makes a file it misses fail
 * by name instead of passing for the wrong reason.
 *
 * The check sits on the worker's mocker rather than on `vi.mock` itself:
 * `vi.mock` finds the calling file from its own stack frame, which a wrapper
 * would displace.
 */
const sourceRoot = process.env.MIDGARD_WORKSPACE_BUNDLE_SOURCE_ROOT;
const refuse = (what) => {
  throw new Error(
    `[workspace-bundle] ${what} in a workspace-bundle project: bundled workspace code would not see it. Route this file to the source project (midgard-test-support/workspace-bundle-analysis.js).`,
  );
};
const reachesBundle = (specifier, importer) => {
  if (!specifier.startsWith(".") && !specifier.startsWith("/")) return true;
  if (typeof importer !== "string") return true;
  const from = importer.startsWith("file:")
    ? fileURLToPath(importer)
    : importer;
  return !resolve(dirname(from), specifier).startsWith(sourceRoot + sep);
};

// Installed by Vitest's worker runner.
const mocker = globalThis.__vitest_mocker__;
if (mocker === undefined || sourceRoot === undefined)
  throw new Error(
    "[workspace-bundle] guard could not find the Vitest mocker or the source region",
  );
for (const name of ["queueMock", "queueUnmock"]) {
  const original = mocker[name];
  mocker[name] = function (id, importer, ...rest) {
    if (typeof id === "string" && reachesBundle(id, importer))
      refuse(`vi.mock/unmock(${JSON.stringify(id)})`);
    return original.call(this, id, importer, ...rest);
  };
}
vi.resetModules = () => refuse("vi.resetModules()");
