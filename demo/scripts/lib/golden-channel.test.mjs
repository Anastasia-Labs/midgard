import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { copyFileSync, mkdirSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import test from "node:test";

import { rebindAikenConstants } from "../../midgard-core/scripts/golden-channel.mjs";

test("rebinding fixture literals preserves public and private visibility", () => {
  const source = 'pub const amount = 1_000\nconst payload = #"aa"\n';
  assert.equal(
    rebindAikenConstants({
      source,
      constants: { amount: 42, payload: Buffer.from("bb", "hex") },
    }),
    'pub const amount = 42\nconst payload = #"bb"\n',
  );
});

test("public fixtures retain the missing and unsupported-literal diagnostics", () => {
  assert.throws(
    () =>
      rebindAikenConstants({
        source: "pub const amount = other\n",
        constants: { amount: 42 },
      }),
    /unrecognized literal form for Aiken constant amount at line 1/,
  );
  assert.throws(
    () => rebindAikenConstants({ source: "", constants: { amount: 42 } }),
    /missing Aiken constant amount/,
  );
});

const isolatedGoldenConsumer = (t) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-golden-consumer-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  const core = join(directory, "node_modules/@al-ft/midgard-core");
  mkdirSync(join(core, "scripts"), { recursive: true });
  for (const file of ["package.json", "scripts/golden-channel.mjs"]) {
    copyFileSync(new URL(`../../midgard-core/${file}`, import.meta.url), join(core, file));
  }
  return directory;
};

test("the public golden-channel import works without repository-only helpers", (t) => {
  const directory = isolatedGoldenConsumer(t);
  const result = spawnSync(
    process.execPath,
    [
      "--input-type=module",
      "-e",
      `import assert from "node:assert/strict";
       import { hex, bytes } from "@al-ft/midgard-core/scripts/golden-channel.mjs";
       assert.equal(hex(bytes("00abcdef")), "00abcdef");`,
    ],
    { cwd: directory, encoding: "utf8" },
  );
  assert.equal(result.status, 0, result.stderr);
});

test("formatting uses the supplied repository resolver and cleans up on failure", (t) => {
  const directory = isolatedGoldenConsumer(t);
  const repositoryRoot = join(directory, "repository");
  mkdirSync(join(repositoryRoot, "scripts/lib"), { recursive: true });
  writeFileSync(
    join(repositoryRoot, "scripts/lib/aiken.mjs"),
    'export const aikenBinary = () => { throw new Error("explicit repository resolver"); };\n',
  );
  const temporary = join(directory, "formatter-temporary");
  mkdirSync(temporary);
  const result = spawnSync(
    process.execPath,
    [
      "--input-type=module",
      "-e",
      `import assert from "node:assert/strict";
       import { readdirSync } from "node:fs";
       import { join } from "node:path";
       import { formatAikenSource } from "@al-ft/midgard-core/scripts/golden-channel.mjs";
       const [repositoryRoot, temporary] = process.argv.slice(1);
       const options = {
         source: "const value = 1\\n",
         fileName: "fixture.ak",
         repositoryRoot,
         tmpPrefix: "golden-format-",
       };
       assert.throws(() => formatAikenSource(options), {
         message: "explicit repository resolver",
       });
       assert.deepEqual(readdirSync(temporary), []);
       const missingRoot = join(repositoryRoot, "missing");
       assert.throws(
         () => formatAikenSource({ ...options, repositoryRoot: missingRoot }),
         (error) => error.code === "MODULE_NOT_FOUND" &&
           error.message.includes(join(missingRoot, "scripts/lib/aiken.mjs")),
       );
       assert.deepEqual(readdirSync(temporary), []);`,
      repositoryRoot,
      temporary,
    ],
    {
      cwd: directory,
      encoding: "utf8",
      env: { ...process.env, TMPDIR: temporary, TMP: temporary, TEMP: temporary },
    },
  );
  assert.equal(result.status, 0, result.stderr);
});
