import { fileURLToPath } from "node:url";

import {
  midgardSourceEnvironments,
  rawSqlLoaderPlugin,
} from "@al-ft/midgard-test-support/vitest";
import { afterAll, beforeAll, describe, expect, it } from "vitest";
import { createViteServer } from "vitest/node";

// The package's exported values must not depend on which of its modules is
// imported first. A module on an import cycle can be evaluated before a cycle
// partner it reads at load time; under the module runner that read yields
// `undefined`, which surfaces later as a throw or, worse, as a frozen
// definition that is silently missing data.
//
// This file builds its own Vite server and module runner over `src`, with the
// source project's resolve conditions, so the package's modules are evaluated
// one by one whatever the workspace bundle does with this file.

const packageRoot = fileURLToPath(new URL("..", import.meta.url));
const src = fileURLToPath(new URL("../src/", import.meta.url));
const indexModule = `${src}index.ts`;
const registryModule = `${src}workflow/family-application-registry.ts`;

let server: Awaited<ReturnType<typeof createViteServer>>;

beforeAll(async () => {
  server = await createViteServer({
    root: packageRoot,
    configFile: false,
    logLevel: "error",
    appType: "custom",
    plugins: [rawSqlLoaderPlugin()],
    server: { middlewareMode: true, hmr: false, ws: false, watch: null },
    optimizeDeps: { noDiscovery: true, include: [] },
    environments: { ssr: midgardSourceEnvironments().ssr },
  });
});

afterAll(async () => {
  await server?.close();
});

interface ModuleRunner {
  clearCache(): void;
  import(url: string): Promise<unknown>;
}

const runner = (): ModuleRunner => {
  const environment = server.environments.ssr;
  if (!("runner" in environment)) {
    throw new Error("the ssr environment has no module runner");
  }
  return environment.runner as ModuleRunner;
};

/** Imports `modules` in order with every module evaluated afresh. */
const importFresh = async (
  modules: readonly string[],
): Promise<Record<string, unknown>> => {
  const moduleRunner = runner();
  moduleRunner.clearCache();
  let last: Record<string, unknown> = {};
  for (const module of modules) {
    last = (await moduleRunner.import(module)) as Record<string, unknown>;
  }
  return last;
};

/** A structural, depth-limited picture of a value; functions are markers. */
const shape = (
  value: unknown,
  depth = 0,
  seen = new WeakSet<object>(),
): unknown => {
  if (value === undefined) return "undefined";
  if (typeof value === "bigint") return `bigint:${value}`;
  if (typeof value === "function") return "function";
  if (typeof value === "symbol") return "symbol";
  if (value === null || typeof value !== "object") return value;
  if (seen.has(value)) return "cycle";
  seen.add(value);
  if (depth >= 5) return "object";
  if (ArrayBuffer.isView(value)) return `bytes:${value.byteLength}`;
  if (value instanceof Map || value instanceof Set)
    return `collection:${value.size}`;
  if (Array.isArray(value))
    return value.map((item) => shape(item, depth + 1, seen));
  return Object.fromEntries(
    Object.keys(value)
      .slice(0, 200)
      .map((key) => [
        key,
        shape((value as Record<string, unknown>)[key], depth + 1, seen),
      ]),
  );
};

const exportsSnapshot = (
  namespace: Record<string, unknown>,
): Record<string, string> =>
  Object.fromEntries(
    Object.keys(namespace)
      .sort()
      .map((name) => [name, JSON.stringify(shape(namespace[name]))]),
  );

/** `src` modules on an import cycle in the runner's resolved module graph. */
const cyclicSourceModules = (): string[] => {
  const nodes = [...server.environments.ssr.moduleGraph.idToModuleMap.values()]
    .filter((node) => node.file?.startsWith(src) === true)
    .map((node) => ({
      file: node.file as string,
      next: [...node.importedModules]
        .map((imported) => imported.file)
        .filter((file): file is string => file?.startsWith(src) === true),
    }));
  const edges = new Map(nodes.map(({ file, next }) => [file, next]));
  // Tarjan's strongly connected components.
  const index = new Map<string, number>();
  const low = new Map<string, number>();
  const stack: string[] = [];
  const onStack = new Set<string>();
  const cyclic: string[] = [];
  const visit = (file: string): void => {
    index.set(file, index.size);
    low.set(file, index.get(file)!);
    stack.push(file);
    onStack.add(file);
    for (const next of edges.get(file) ?? []) {
      if (!index.has(next)) {
        visit(next);
        low.set(file, Math.min(low.get(file)!, low.get(next)!));
      } else if (onStack.has(next)) {
        low.set(file, Math.min(low.get(file)!, index.get(next)!));
      }
    }
    if (low.get(file) === index.get(file)) {
      const component: string[] = [];
      let member: string;
      do {
        member = stack.pop()!;
        onStack.delete(member);
        component.push(member);
      } while (member !== file);
      if (component.length > 1 || (edges.get(file) ?? []).includes(file)) {
        cyclic.push(...component);
      }
    }
  };
  for (const { file } of nodes) if (!index.has(file)) visit(file);
  return cyclic.sort();
};

describe("module evaluation order", () => {
  it("exports the same values whichever cyclic module is imported first", async () => {
    const reference = exportsSnapshot(await importFresh([indexModule]));
    const candidates = cyclicSourceModules();
    expect(Object.keys(reference).length).toBeGreaterThan(0);
    expect(candidates.length).toBeGreaterThan(0);

    const broken: string[] = [];
    for (const candidate of candidates) {
      const label = candidate.slice(src.length);
      try {
        const snapshot = exportsSnapshot(
          await importFresh([candidate, registryModule, indexModule]),
        );
        const differing = Object.keys(reference).filter(
          (name) => snapshot[name] !== reference[name],
        );
        if (differing.length > 0) {
          broken.push(`${label}: changes ${differing.join(", ")}`);
        }
      } catch (error) {
        broken.push(
          `${label}: throws ${error instanceof Error ? error.message : String(error)}`,
        );
      }
    }
    expect(broken).toEqual([]);
  }, 600_000);
});
