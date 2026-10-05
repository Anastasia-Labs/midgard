import { vi } from "vitest";

/**
 * Setup file of every workspace-bundle project (see `workspaceBundleProjects`
 * in `vitest.js`). In such a project the workspace packages a test reaches are
 * loaded natively from the bundle, so a module mock of anything outside the
 * package under test, or a module-registry reset, would silently stop
 * reaching code it reaches in a source-mode run. The static routing in
 * `workspace-bundle-analysis.js` sends those files to the source project; this guard
 * makes a file it misses fail by name instead of passing for the wrong reason.
 *
 * The check sits on the worker's mocker rather than on `vi.mock` itself:
 * `vi.mock` finds the calling file from its own stack frame, which a wrapper
 * would displace.
 */
const self = process.env.MIDGARD_WORKSPACE_BUNDLE_SELF;
const refuse = (what) => {
  throw new Error(
    `[workspace-bundle] ${what} in a workspace-bundle project: bundled workspace code would not see it. Route this file to the source project (midgard-test-support/workspace-bundle-analysis.js).`,
  );
};
const outsideSelf = (specifier) =>
  !specifier.startsWith(".") &&
  !specifier.startsWith("/") &&
  specifier !== self &&
  !specifier.startsWith(`${self}/`);

// Installed by Vitest's worker runner.
const mocker = globalThis.__vitest_mocker__;
if (mocker === undefined || self === undefined)
  throw new Error(
    "[workspace-bundle] guard could not find the Vitest mocker or the package under test",
  );
for (const name of ["queueMock", "queueUnmock"]) {
  const original = mocker[name];
  mocker[name] = function (id, ...rest) {
    if (typeof id === "string" && outsideSelf(id))
      refuse(`vi.mock/unmock(${JSON.stringify(id)})`);
    return original.call(this, id, ...rest);
  };
}
vi.resetModules = () => refuse("vi.resetModules()");
