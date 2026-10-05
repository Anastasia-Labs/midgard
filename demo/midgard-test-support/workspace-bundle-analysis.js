import { existsSync } from "node:fs";
import { dirname, join, relative, resolve } from "node:path";

import {
  bundleablePackages,
  demoDirectory,
  isInside,
  isNodeBuiltin,
  packageOfFile,
  resolveRelative,
  scan,
  splitSpecifier,
  workspacePackages,
  workspaceSourceTarget,
} from "./workspace-bundle-files.js";

/**
 * Static analysis for the workspace bundle (`workspace-bundle.js`): which
 * workspace packages a suite may load from the bundle, which specifiers are
 * bundle entries, and which test files must run module-by-module from source
 * instead. `vitest.js` (`workspaceBundleProjects`) states the guarantee and
 * the hazards this routing protects.
 *
 * Plain JavaScript for the same reason as `vitest.js`: it runs at config time.
 */

/**
 * Static analysis of one suite: which workspace entry points its files reach,
 * which of those can be bundled, and which test files must keep loading
 * workspace code from source (with the reasons).
 *
 * Two import graphs, each walked once and propagated backwards (so the cost is
 * linear in the graph, not in files x graph):
 * - the source side: everything Vite still loads from source in the forks —
 *   the suite's own files, source-only packages, and any file reached by a
 *   relative path — stopping at bare imports of a bundleable package, which
 *   become candidate entry points;
 * - the bundle side: everything a candidate entry reaches inside the
 *   bundleable packages. An entry that reaches an unbundleable workspace file,
 *   or a file that mocks modules, is not bundled.
 * A test file is routed to source when anything on its source side mocks a
 * module outside the package or resets the registry, reaches an entry that is
 * not bundled, or loads from source a module that an entry it imports also
 * carries (a second instance).
 *
 * `setupRoots` are the project's `globalSetup` files. They run in the main
 * process, never beside a test, so they are never routed; they are walked so
 * that a workspace package they import is served from the bundle as well,
 * which keeps every load of a bundled module in that project on one side.
 */
export const analyzeSuite = ({
  packageDirectory,
  testFiles,
  setupRoots = [],
}) => {
  const packages = workspacePackages();
  const self = packageOfFile(packages, join(packageDirectory, "package.json"));
  if (self === undefined)
    throw new Error(
      `[workspace-bundle] ${packageDirectory} is not a workspace package`,
    );
  const bundleable = bundleablePackages(packages, self.name);
  const label = (file) => relative(demoDirectory, file);

  const ownerOf = new Map();
  const owner = (file) => {
    if (!ownerOf.has(file))
      ownerOf.set(file, packageOfFile(packages, file) ?? null);
    return ownerOf.get(file);
  };
  const resolvedRelative = new Map();
  const relativeTarget = (from, specifier) => {
    const key = `${dirname(from)}\0${specifier}`;
    if (!resolvedRelative.has(key))
      resolvedRelative.set(key, resolveRelative(from, specifier));
    return resolvedRelative.get(key);
  };
  const bareTarget = (specifier) => {
    const parts = splitSpecifier(specifier);
    const pkg = parts === undefined ? undefined : packages.get(parts.name);
    if (pkg === undefined) return undefined;
    const target = workspaceSourceTarget(pkg, parts.subpath);
    return {
      pkg,
      target:
        target !== undefined &&
        existsSync(target) &&
        /\.(?:m?[jt]s|tsx)$/u.test(target)
          ? target
          : undefined,
    };
  };

  // Bundle side.
  const bundleEdges = new Map(); // file -> files
  const bundleDirect = new Map(); // file -> escape reason
  const walkBundle = (start) => {
    const stack = [start];
    while (stack.length > 0) {
      const current = stack.pop();
      if (bundleEdges.has(current)) continue;
      const edges = [];
      bundleEdges.set(current, edges);
      const scanned = scan(current);
      if (
        scanned.mocks.length > 0 ||
        scanned.resetsModules ||
        scanned.spiedNamespaces.length > 0
      )
        bundleDirect.set(current, `${label(current)} mocks modules`);
      if (scanned.readsImportMetaEnv)
        bundleDirect.set(current, `${label(current)} reads import.meta.env`);
      for (const specifier of scanned.imports) {
        let target;
        if (specifier.startsWith("."))
          target = relativeTarget(current, specifier);
        else if (!isNodeBuiltin(specifier)) {
          const bare = bareTarget(specifier);
          if (bare === undefined) continue;
          if (!bundleable.has(bare.pkg.name)) {
            bundleDirect.set(current, `${label(current)} imports ${specifier}`);
            continue;
          }
          target = bare.target;
        }
        if (target === undefined) continue;
        const targetOwner = owner(target);
        if (targetOwner === null) continue;
        if (!bundleable.has(targetOwner.name)) {
          bundleDirect.set(
            current,
            `${label(current)} reaches ${label(target)}`,
          );
          continue;
        }
        edges.push(target);
        stack.push(target);
      }
    }
  };

  // Source side.
  const sourceEdges = new Map(); // file -> files
  const sourceDirect = new Map(); // file -> reasons
  const boundary = new Map(); // file -> [{ specifier, target }]
  const addReason = (file, reason) => {
    if (!sourceDirect.has(file)) sourceDirect.set(file, []);
    sourceDirect.get(file).push(reason);
  };
  const stack = [...testFiles, ...setupRoots];
  while (stack.length > 0) {
    const current = stack.pop();
    if (sourceEdges.has(current)) continue;
    const edges = [];
    const crossings = [];
    sourceEdges.set(current, edges);
    boundary.set(current, crossings);
    const scanned = scan(current);
    if (scanned.resetsModules)
      addReason(current, `${label(current)}: vi.resetModules`);
    for (const specifier of scanned.mocks) {
      if (specifier.startsWith(".")) {
        const target =
          relativeTarget(current, specifier) ??
          resolve(dirname(current), specifier);
        if (!isInside(self.directory, target))
          addReason(
            current,
            `${label(current)}: vi.mock(${specifier}) outside the package`,
          );
        continue;
      }
      if (splitSpecifier(specifier)?.name === self.name) continue;
      addReason(current, `${label(current)}: vi.mock(${specifier})`);
    }
    for (const specifier of scanned.spiedNamespaces) {
      const spiedOwner = specifier.startsWith(".")
        ? owner(relativeTarget(current, specifier) ?? "")
        : isNodeBuiltin(specifier)
          ? undefined
          : bareTarget(specifier)?.pkg;
      if (bundleable.has(spiedOwner?.name))
        addReason(
          current,
          `${label(current)}: vi.spyOn(${specifier} namespace)`,
        );
    }
    for (const specifier of scanned.imports) {
      let target;
      if (specifier.startsWith(".")) {
        target = relativeTarget(current, specifier);
        // A relative path into a bundleable package names the same module
        // its package specifier does, so it is served from the bundle too
        // (keyed by the absolute file).
        if (target !== undefined && bundleable.has(owner(target)?.name)) {
          crossings.push({ specifier: target, target });
          continue;
        }
      } else if (!isNodeBuiltin(specifier)) {
        const bare = bareTarget(specifier);
        if (bare === undefined) continue;
        if (bundleable.has(bare.pkg.name)) {
          crossings.push({ specifier, target: bare.target });
          continue;
        }
        target = bare.target;
      }
      if (target === undefined || owner(target) === null) continue;
      edges.push(target);
      stack.push(target);
    }
  }

  // Candidate entries, their bundle-side closure, and backward propagation of
  // escapes.
  const candidates = new Map();
  for (const crossings of boundary.values())
    for (const { specifier, target } of crossings)
      candidates.set(specifier, target);
  for (const target of candidates.values())
    if (target !== undefined) walkBundle(target);
  const escapeOf = propagate(
    bundleEdges,
    new Map([...bundleDirect].map(([f, r]) => [f, [r]])),
  );
  const entries = new Map();
  const unbundled = new Map();
  for (const [specifier, target] of candidates) {
    if (target === undefined) unbundled.set(specifier, "no source entry point");
    else if (escapeOf.has(target))
      unbundled.set(specifier, escapeOf.get(target)[0]);
    else entries.set(specifier, target);
  }
  const bundled = new Set();
  const closure = [...entries.values()];
  while (closure.length > 0) {
    const current = closure.pop();
    if (bundled.has(current)) continue;
    bundled.add(current);
    closure.push(...bundleEdges.get(current));
  }

  for (const [file, crossings] of boundary)
    for (const { specifier } of crossings)
      if (unbundled.has(specifier))
        addReason(
          file,
          `${specifier} is not bundleable (${unbundled.get(specifier)})`,
        );
  const reasonsOf = propagate(sourceEdges, sourceDirect);
  const sourceRouted = new Map();
  for (const file of testFiles)
    if (reasonsOf.has(file)) sourceRouted.set(file, reasonsOf.get(file));

  // Second instances: a module a test loads from source (say through a
  // relative import into another package) that is also inside a bundled entry
  // the same test imports. Only the few files on both sides need checking.
  const sourceReverse = reverseOf(sourceEdges);
  const bundleReverse = reverseOf(bundleEdges);
  for (const shared of [...sourceEdges.keys()].filter((file) =>
    bundled.has(file),
  )) {
    const containing = reachable(bundleReverse, [shared]);
    const viaEntries = [...entries]
      .filter(([, target]) => containing.has(target))
      .map(([specifier]) => specifier);
    const importers = [...boundary]
      .filter(([, crossings]) =>
        crossings.some(({ specifier }) => viaEntries.includes(specifier)),
      )
      .map(([file]) => file);
    const reachEntry = reachable(sourceReverse, importers);
    const reachShared = reachable(sourceReverse, [shared]);
    for (const file of testFiles)
      if (reachEntry.has(file) && reachShared.has(file)) {
        const reasons = sourceRouted.get(file) ?? [];
        if (reasons.length < 3)
          reasons.push(
            `${label(shared)} would load from source and from the bundle`,
          );
        sourceRouted.set(file, reasons);
      }
  }
  return {
    self: self.name,
    bundleable: [...bundleable].sort(),
    entries: new Map(
      [...entries].sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0)),
    ),
    unbundled,
    sourceRouted,
  };
};

const reverseOf = (edges) => {
  const reverse = new Map();
  for (const [from, targets] of edges)
    for (const target of targets) {
      if (!reverse.has(target)) reverse.set(target, []);
      reverse.get(target).push(from);
    }
  return reverse;
};

const reachable = (graph, starts) => {
  const found = new Set();
  const stack = [...starts];
  while (stack.length > 0) {
    const current = stack.pop();
    if (found.has(current)) continue;
    found.add(current);
    stack.push(...(graph.get(current) ?? []));
  }
  return found;
};

/**
 * Backward propagation over an import graph: a file carries the reasons of
 * every file it reaches (a few representative ones, which is all a report
 * needs).
 */
const propagate = (edges, direct) => {
  const reverse = reverseOf(edges);
  const result = new Map();
  const queue = [];
  for (const [file, reasons] of direct) {
    result.set(file, [...reasons]);
    queue.push(file);
  }
  while (queue.length > 0) {
    const current = queue.shift();
    const reasons = result.get(current);
    for (const importer of reverse.get(current) ?? []) {
      const existing = result.get(importer);
      if (existing === undefined) {
        result.set(importer, reasons.slice(0, 3));
        queue.push(importer);
      } else if (existing.length < 3) {
        for (const reason of reasons)
          if (existing.length < 3 && !existing.includes(reason))
            existing.push(reason);
      }
    }
  }
  return result;
};
