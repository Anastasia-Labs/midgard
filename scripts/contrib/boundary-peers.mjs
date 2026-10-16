import { cpSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, relative, resolve } from "node:path";

// deploy --prod treats the project as an application and omits peers also
// listed in devDependencies. A library's consumer supplies those peers. Reuse
// the workspace's exact locked resolutions, patches and overrides, with only
// these peers as consumer dependencies; no development importer is installed.
export const preparePeerConsumer = (root, pkg, consumer) => {
  const workspace = JSON.parse(
    readFileSync(resolve(root, "demo/package.json")),
  );
  const external = [];
  const local = [];
  for (const [name, specifier] of Object.entries(pkg.peerDependencies ?? {})) {
    if (pkg.peerDependenciesMeta?.[name]?.optional) continue;
    (specifier.startsWith("workspace:") ? local : external).push(name);
  }
  const manifest = {
    private: true,
    type: "module",
    packageManager: workspace.packageManager,
  };
  if (external.length) {
    const yaml = createRequire(resolve(root, "demo/package.json"))("yaml");
    const lock = yaml.parse(
      readFileSync(resolve(root, "demo/pnpm-lock.yaml"), "utf8"),
    );
    const importer =
      lock.importers[
        relative(resolve(root, "demo"), resolve(root, pkg.directory))
      ];
    const locked = { ...importer?.devDependencies, ...importer?.dependencies };
    const dependencies = Object.fromEntries(
      external.map((name) => {
        if (!locked[name] || locked[name].version.startsWith("link:"))
          throw new Error(
            `declared peer ${name} has no installed external lock resolution for ${pkg.name}`,
          );
        return [name, locked[name]];
      }),
    );
    manifest.dependencies = Object.fromEntries(
      Object.entries(dependencies).map(([name, entry]) => [
        name,
        entry.specifier,
      ]),
    );
    manifest.pnpm = workspace.pnpm;
    for (const patch of Object.values(
      workspace.pnpm?.patchedDependencies ?? {},
    )) {
      const destination = resolve(consumer, patch);
      if (relative(consumer, destination).startsWith(".."))
        throw new Error(`patch leaves peer consumer: ${patch}`);
      mkdirSync(dirname(destination), { recursive: true });
      cpSync(resolve(root, "demo", patch), destination);
    }
    writeFileSync(
      resolve(consumer, "pnpm-lock.yaml"),
      yaml.stringify({
        ...lock,
        importers: { ".": { dependencies } },
      }),
    );
  }
  writeFileSync(resolve(consumer, "package.json"), JSON.stringify(manifest));
  return { external, local };
};
