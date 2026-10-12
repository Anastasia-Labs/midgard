import assert from "node:assert/strict";
import {
  appendFileSync,
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";
import test from "node:test";

import { buildPackage, checkBuild } from "./build.mjs";
import { buildRefusals } from "./build-inputs.mjs";
import { outputIdentity } from "./files.mjs";

// Real tsup builds of midgard-core (and midgard-validation over it) in a
// scratch checkout, not mocks: the skip is only worth having if a real dist
// that the verdict calls fresh is the dist a rebuild would emit.
const checkout = fileURLToPath(new URL("../..", import.meta.url));
const installed = existsSync(
  resolve(checkout, "demo/midgard-core/node_modules/tsup"),
);
const required = process.env.MIDGARD_REQUIRE_REAL_BUILDS === "1";

// Workspace links are relative, so verbatim copies resolve inside the scratch
// tree; the shared store is linked entry by entry so its installed lock record
// stays a real scratch file that the input closure can read.
const mirrorInstall = (scratch) => {
  const store = resolve(checkout, "demo/node_modules");
  const copy = resolve(scratch, "demo/node_modules");
  mkdirSync(resolve(copy, ".pnpm"), { recursive: true });
  for (const entry of readdirSync(store))
    if (![".pnpm", ".modules.yaml"].includes(entry))
      symlinkSync(resolve(store, entry), resolve(copy, entry));
  for (const entry of readdirSync(resolve(store, ".pnpm")))
    if (entry === "lock.yaml")
      cpSync(resolve(store, ".pnpm", entry), resolve(copy, ".pnpm", entry));
    else
      symlinkSync(
        resolve(store, ".pnpm", entry),
        resolve(copy, ".pnpm", entry),
      );
};

const scratchCheckout = (t) => {
  const scratch = mkdtempSync(resolve(tmpdir(), "midgard-build-fresh-"));
  t.after(() => rmSync(scratch, { recursive: true, force: true }));
  for (const path of [
    "demo/package.json",
    "demo/pnpm-lock.yaml",
    "demo/pnpm-workspace.yaml",
  ])
    cpSync(resolve(checkout, path), resolve(scratch, path));
  // midgard-core inlines l1-node-transport's sources through the
  // `midgard-source` condition, so its build needs that package too.
  for (const name of [
    "l1-node-transport",
    "midgard-core",
    "midgard-validation",
  ])
    cpSync(resolve(checkout, "demo", name), resolve(scratch, "demo", name), {
      recursive: true,
      verbatimSymlinks: true,
      filter: (source) =>
        !source.startsWith(resolve(checkout, "demo", name, "dist")),
    });
  mirrorInstall(scratch);
  return scratch;
};

test(
  "a guarded build is a verified no-op exactly while its dist is provably fresh",
  {
    skip:
      !installed && !required
        ? "midgard-core dependencies are not installed (pnpm --dir demo install); set MIDGARD_REQUIRE_REAL_BUILDS=1 to make this a failure"
        : false,
  },
  async (t) => {
    assert.ok(
      installed,
      "MIDGARD_REQUIRE_REAL_BUILDS=1 needs an installed demo workspace",
    );
    const root = scratchCheckout(t);
    const core = "@al-ft/midgard-core";
    const coreDist = "demo/midgard-core/dist";
    const build = (name, { env = {}, force = false } = {}) =>
      buildPackage(root, name, { env: { ...process.env, ...env }, force });
    const rebuilt = (receipt) => {
      assert.equal(receipt.status, "passed", receipt.reason ?? receipt.path);
      assert.equal(receipt.steps.length, 1);
      assert.equal(receipt.fresh, undefined);
    };
    const skipped = (receipt) => {
      assert.equal(receipt.status, "fresh");
      assert.equal(receipt.fresh, "skipped");
      assert.equal(receipt.steps.length, 0);
      assert.equal(receipt.exitCode, 0);
    };

    rebuilt(await build(core));
    const emitted = outputIdentity(root, coreDist).sha256;

    await t.test("a fresh dist skips without touching it", async () => {
      const stamp = readFileSync(
        resolve(root, coreDist, ".contrib-build-v1.json"),
      );
      skipped(await build(core));
      assert.deepEqual(
        readFileSync(resolve(root, coreDist, ".contrib-build-v1.json")),
        stamp,
      );
      assert.equal(outputIdentity(root, coreDist).sha256, emitted);
    });

    await t.test(
      "environment the recipe never names leaves the emitted bytes unchanged",
      async () => {
        const ambient = {
          MIDGARD_DEPLOYMENT_PROFILE: "preprod-testing",
          NODE_ENV: "production",
        };
        skipped(await build(core, { env: ambient }));
        rebuilt(await build(core, { env: ambient, force: true }));
        assert.equal(outputIdentity(root, coreDist).sha256, emitted);
        skipped(await build(core));
      },
    );

    // `--force` is exercised above; `pnpm run build` reaches the guard with
    // only the environment to carry the request.
    await t.test(
      "MIDGARD_CONTRIB_FORCE_BUILD rebuilds a fresh dist",
      async () => {
        rebuilt(
          await build(core, { env: { MIDGARD_CONTRIB_FORCE_BUILD: "1" } }),
        );
        assert.equal(outputIdentity(root, coreDist).sha256, emitted);
      },
    );

    await t.test("a source edit rebuilds", async () => {
      const source = resolve(root, "demo/midgard-core/src/hex.ts");
      appendFileSync(source, "\nexport const freshnessProbe = 1;\n");
      assert.match(checkBuild(root, core).reason, /input closure changed/u);
      rebuilt(await build(core));
      assert.match(
        readFileSync(resolve(root, coreDist, "hex.js"), "utf8"),
        /freshnessProbe/u,
      );
      skipped(await build(core));
    });

    await t.test(
      "a lockfile or installed-package change makes the dist stale",
      () => {
        for (const path of [
          "demo/pnpm-lock.yaml",
          "demo/node_modules/.pnpm/lock.yaml",
        ]) {
          const original = readFileSync(resolve(root, path));
          appendFileSync(resolve(root, path), "\n# probe\n");
          assert.match(
            checkBuild(root, core).reason,
            /input closure changed/u,
            path,
          );
          writeFileSync(resolve(root, path), original);
          assert.equal(checkBuild(root, core).status, "fresh", path);
        }
      },
    );

    await t.test("a tampered or missing dist output rebuilds", async () => {
      const before = outputIdentity(root, coreDist).sha256;
      const index = resolve(root, coreDist, "index.js");
      const original = readFileSync(index);
      appendFileSync(index, "\n// tampered\n");
      assert.match(checkBuild(root, core).reason, /contents changed/u);
      writeFileSync(index, original);
      assert.equal(checkBuild(root, core).status, "fresh");
      rmSync(resolve(root, coreDist, "hex.cjs"));
      assert.match(checkBuild(root, core).reason, /contents changed/u);
      appendFileSync(index, "\n// tampered\n");
      rebuilt(await build(core));
      assert.ok(existsSync(resolve(root, coreDist, "hex.cjs")));
      assert.equal(outputIdentity(root, coreDist).sha256, before);
    });

    await t.test("a dependency dist change rebuilds its consumer", async () => {
      const validation = "@al-ft/midgard-validation";
      rebuilt(await build(validation));
      skipped(await build(validation));
      appendFileSync(resolve(root, coreDist, "index.js"), "\n// substituted\n");
      assert.match(
        checkBuild(root, validation).reason,
        /compiled dependency contents changed/u,
      );
      // Rebuilding the dependency restores its bytes, so the consumer it was
      // compiled against is fresh again and is not rebuilt.
      skipped(await build(validation));
      assert.equal(checkBuild(root, core).status, "fresh");
      // A real change of the dependency's emitted bytes rebuilds both.
      appendFileSync(
        resolve(root, "demo/midgard-core/src/hex.ts"),
        "\nexport const secondProbe = 2;\n",
      );
      rebuilt(await build(validation));
      assert.equal(checkBuild(root, core).status, "fresh");
    });

    await t.test("a variable the recipe names rebinds the dist", async () => {
      const path = resolve(root, "demo/midgard-core/package.json");
      const pkg = JSON.parse(readFileSync(path, "utf8"));
      const recipe = pkg.scripts["build:contrib-raw"];
      // Any other command the recipe could run is outside the allow-list.
      pkg.scripts["build:contrib-raw"] = `${recipe} && test -n x`;
      writeFileSync(path, JSON.stringify(pkg, null, 2));
      assert.match(
        checkBuild(root, core).reason,
        /runs test, which the guard does not scan/u,
      );
      pkg.scripts["build:contrib-raw"] =
        `MIDGARD_FRESHNESS_COPY="\${MIDGARD_FRESHNESS_PROBE:-unset}" ${recipe}`;
      writeFileSync(path, JSON.stringify(pkg, null, 2));
      rebuilt(await build(core, { env: { MIDGARD_FRESHNESS_PROBE: "one" } }));
      skipped(await build(core, { env: { MIDGARD_FRESHNESS_PROBE: "one" } }));
      assert.match(
        checkBuild(root, core, {
          env: { ...process.env, MIDGARD_FRESHNESS_PROBE: "two" },
        }).reason,
        /build environment changed/u,
      );
      assert.equal(
        checkBuild(root, core, {
          env: { ...process.env, MIDGARD_FRESHNESS_PROBE: "one" },
        }).status,
        "fresh",
      );
    });

    // The static checks cannot see a file a config reads through node:fs;
    // the read trace does, so the dist builds but is never stamped.
    await t.test(
      "a build that reads an unbound file stays unstamped",
      async () => {
        const config = resolve(root, "demo/midgard-core/tsup.config.ts");
        const original = readFileSync(config, "utf8");
        mkdirSync(resolve(root, "outside"));
        writeFileSync(resolve(root, "outside/banner.txt"), "/* outside */");
        writeFileSync(
          config,
          original.replace(
            "export default defineConfig({",
            'import { readFileSync } from "node:fs";\n\nexport default defineConfig({\n  banner: { js: readFileSync(new URL("../../outside/banner.txt", import.meta.url), "utf8") },',
          ),
        );
        assert.notEqual(checkBuild(root, core).reason, undefined);
        assert.doesNotMatch(
          checkBuild(root, core).reason,
          /never provably fresh/u,
        );
        const receipt = await build(core);
        assert.equal(receipt.status, "passed", receipt.reason);
        assert.match(
          receipt.reason,
          /dist left unstamped: build read outside\/banner\.txt, which its input closure does not bind/u,
        );
        assert.match(
          readFileSync(resolve(root, coreDist, "index.js"), "utf8"),
          /outside/u,
        );
        // The stamp path holds the reasons instead of a stamp.
        const record = JSON.parse(
          readFileSync(resolve(root, coreDist, ".contrib-build-v1.json")),
        );
        assert.equal(record.reads, undefined);
        assert.equal(record.outputs, undefined);
        assert.match(record.unstamped.join(), /outside\/banner\.txt/u);
        const verdict = checkBuild(root, core);
        assert.equal(verdict.status, "missing");
        assert.match(
          verdict.reason,
          /dist left unstamped: build read outside/u,
        );
        writeFileSync(config, original);
        rebuilt(await build(core));
        skipped(await build(core));
      },
    );

    // Each attack below reaches a file outside the closure by a route the
    // static scan does not see (esbuild --inject, an fs copy) or refuses
    // (a public directory, a child process); the trace must still leave the
    // dist unstamped and name the file.
    const packagePath = resolve(root, "demo/midgard-core/package.json");
    const originalPackage = readFileSync(packagePath, "utf8");
    const withRecipe = (edit) => {
      const pkg = JSON.parse(originalPackage);
      pkg.scripts["build:contrib-raw"] = edit(pkg.scripts["build:contrib-raw"]);
      writeFileSync(packagePath, JSON.stringify(pkg, null, 2));
    };
    const configPath = resolve(root, "demo/midgard-core/tsup.config.ts");
    const originalConfig = readFileSync(configPath, "utf8");
    const withConfig = (imports, statement) =>
      writeFileSync(
        configPath,
        `${imports}\n${originalConfig.replace(
          "export default defineConfig({",
          `${statement}\n\nexport default defineConfig({`,
        )}`,
      );
    const unstamped = async (reason) => {
      const receipt = await build(core);
      assert.equal(receipt.status, "passed", receipt.reason);
      assert.match(receipt.reason, reason);
      const verdict = checkBuild(root, core);
      assert.equal(verdict.status, "missing");
      assert.match(verdict.reason, reason);
    };

    await t.test(
      "a public directory outside the closure is refused and its copy traced",
      async () => {
        mkdirSync(resolve(root, "outside-public"));
        writeFileSync(resolve(root, "outside-public/asset.txt"), "asset");
        withRecipe((recipe) =>
          recipe.replace(
            " --clean &&",
            " --clean --publicDir ../../outside-public &&",
          ),
        );
        // A fresh stamp from the restored build above: the refusal is
        // reported before any digest comparison.
        assert.match(
          checkBuild(root, core).reason,
          /copies public directory \.\.\/\.\.\/outside-public, which is outside the input closure/u,
        );
        await unstamped(
          /build read outside-public\/asset\.txt, which its input closure does not bind/u,
        );
        writeFileSync(packagePath, originalPackage);
      },
    );

    await t.test(
      "a file esbuild injects from outside the closure stays unstamped",
      async () => {
        writeFileSync(
          resolve(root, "outside.js"),
          "export const injected = 1;\n",
        );
        withRecipe((recipe) =>
          recipe.replace(
            " --clean &&",
            " --clean --inject ../../outside.js &&",
          ),
        );
        assert.deepEqual(buildRefusals(root, core), []);
        await unstamped(
          /build read outside\.js, which its input closure does not bind/u,
        );
        writeFileSync(packagePath, originalPackage);
      },
    );

    await t.test(
      "a file a config copies from outside the closure stays unstamped",
      async () => {
        mkdirSync(resolve(root, "outside-copy"));
        writeFileSync(resolve(root, "outside-copy/asset.txt"), "asset");
        withConfig(
          'import { cpSync } from "node:fs";',
          'cpSync(new URL("../../outside-copy", import.meta.url), new URL("../../outside-copied", import.meta.url), { recursive: true });',
        );
        assert.deepEqual(buildRefusals(root, core), []);
        await unstamped(
          /build read outside-copy\/asset\.txt, which its input closure does not bind/u,
        );
        writeFileSync(configPath, originalConfig);
      },
    );

    await t.test(
      "a post-build lifecycle script is refused and never run",
      async () => {
        writeFileSync(resolve(root, "outside.js"), "export const o = 1;\n");
        const pkg = JSON.parse(originalPackage);
        pkg.scripts["postbuild:contrib-raw"] =
          "cp ../../outside.js dist/outside.js";
        writeFileSync(packagePath, JSON.stringify(pkg, null, 2));
        assert.match(
          buildRefusals(root, core).join(),
          /package\.json defines postbuild:contrib-raw, a lifecycle script the trace does not follow/u,
        );
        await unstamped(/defines postbuild:contrib-raw/u);
        // The guarded build also tells pnpm not to run it.
        assert.ok(
          !existsSync(resolve(root, "demo/midgard-core/dist/outside.js")),
        );
        writeFileSync(packagePath, originalPackage);
      },
    );

    await t.test("a config that fetches is refused and traced", async () => {
      withConfig("", 'fetch("http://127.0.0.1:1/").catch(() => undefined);');
      assert.match(
        buildRefusals(root, core).join(),
        /tsup\.config\.ts uses fetch, a network API whose responses no stamp binds/u,
      );
      await unstamped(
        /build ran network fetch, which the trace cannot follow/u,
      );
      writeFileSync(configPath, originalConfig);
    });

    await t.test(
      "a config that runs a child process is refused and traced",
      async () => {
        withConfig(
          'import { execFileSync } from "node:child_process";',
          'execFileSync("cat", [new URL("../../outside.js", import.meta.url).pathname]);',
        );
        assert.match(
          buildRefusals(root, core).join(),
          /tsup\.config\.ts imports node:child_process, which build code may not import/u,
        );
        await unstamped(
          /build ran child process execFileSync cat \S*outside\.js, which the trace cannot follow/u,
        );
        writeFileSync(configPath, originalConfig);
        rebuilt(await build(core));
        skipped(await build(core));
      },
    );
  },
);
