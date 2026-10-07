import assert from "node:assert/strict";
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  writeFileSync,
  rmSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { resolve } from "node:path";
import test from "node:test";

import { atomicJson } from "./files.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { generateDevnet } from "./operations.mjs";
import { verifyReceipt } from "./receipts.mjs";
import { reproduce } from "./reproduction.mjs";
import { runTests } from "./tests.mjs";

test("focused tests preserve the ordinary test runtime despite an ambient emulator mode", async (t) => {
  const root = fixture(t);
  const runner = resolve(root, "demo/node_modules/vitest");
  mkdirSync(runner, { recursive: true });
  atomicJson(resolve(runner, "package.json"), { name: "vitest" });
  mkdirSync(resolve(root, "demo/example/tests"));
  writeFileSync(
    resolve(root, "demo/example/tests/runtime.test.ts"),
    "// runtime fixture\n",
  );
  // Drive the child environment, not an invented production assertion count.
  writeFileSync(
    resolve(runner, "vitest.mjs"),
    `
import assert from 'node:assert/strict';
import {writeFileSync} from 'node:fs';
import {resolve} from 'node:path';
assert.equal(process.env.NODE_ENV, 'test', 'ordinary tests require NODE_ENV=test');
const output = process.argv.find(arg => arg.startsWith('--outputFile=')).slice('--outputFile='.length);
writeFileSync(output, JSON.stringify({success:true,numPassedTests:1,numFailedTests:0,testResults:[{name:resolve('tests/runtime.test.ts'),assertionResults:[{status:'passed',fullName:'runtime fixture'}]}]}));
`,
  );
  const receipt = await runTests(root, "example", {
    files: ["tests/runtime.test.ts"],
    sourceOnly: true,
    env: { ...process.env, NODE_ENV: "emulator" },
  });
  assert.equal(
    receipt.status,
    "passed",
    readFileSync(receipt.steps[0].logPath, "utf8"),
  );
  assert.equal(receipt.counts.executed, 1);
});

test("devnet generation supplies a private fresh run directory and verifies produced assets", async (t) => {
  const root = fixture(t);
  const path = "demo/example/package.json";
  const pkg = JSON.parse(readFileSync(resolve(root, path)));
  pkg.name = "midgard-node-tools";
  atomicJson(resolve(root, path), pkg);
  const script = resolve(
    root,
    "demo/midgard-node-tools/devnet/phase4-process/scripts/generate.sh",
  );
  mkdirSync(resolve(script, ".."), { recursive: true });
  // Exercise the generator's required environment and output contract without
  // launching a Cardano container or claiming live chain acceptance.
  writeFileSync(
    script,
    '#!/bin/sh\nset -eu\n: "${MIDGARD_PHASE4_RUN_DIR:?MIDGARD_PHASE4_RUN_DIR is required}"\ntest ! -e "$MIDGARD_PHASE4_RUN_DIR"\nmkdir "$MIDGARD_PHASE4_RUN_DIR"\nprintf config > "$MIDGARD_PHASE4_RUN_DIR/config.json"\n',
  );
  // The run id fixes the allocation's ports, which another process on this
  // machine may hold; the generator refuses those, so try another run id.
  let runId;
  let receipt;
  for (let attempt = 0; receipt === undefined; attempt++) {
    runId = `directory-check-${attempt.toString()}`;
    try {
      receipt = await generateDevnet(root, runId, {
        env: { ...process.env, MIDGARD_PHASE4_RUN_DIR: root },
      });
    } catch (error) {
      if (attempt >= 20 || !/is unavailable: EADDRINUSE/u.test(error.message))
        throw error;
    }
  }
  assert.equal(receipt.status, "passed", receipt.reason);
  const directory = receipt.allocation.env.MIDGARD_PHASE4_RUN_DIR;
  assert.notEqual(
    directory,
    root,
    "ambient run directories must not select an existing deployment",
  );
  assert.equal(verifyReceipt(root, receipt.path).status, "passed");
  const again = await generateDevnet(root, runId);
  assert.equal(again.status, "passed", again.reason);
  assert.notEqual(again.allocation.env.MIDGARD_PHASE4_RUN_DIR, directory);
  writeFileSync(resolve(directory, "config.json"), "tampered");
  assert.throws(
    () => verifyReceipt(root, receipt.path),
    /retained generator artifact changed/u,
  );
});

test("reproduction refuses a native output deleted after its successful build", async (t) => {
  const root = fixture(t);
  const registry = mkdtempSync(resolve(tmpdir(), "midgard-reproduce-tests-"));
  const tools = resolve(registry, "tools");
  mkdirSync(tools);
  const previousPath = process.env.PATH;
  const previousTmpdir = process.env.TMPDIR;
  t.after(() => {
    process.env.PATH = previousPath;
    if (previousTmpdir === undefined) delete process.env.TMPDIR;
    else process.env.TMPDIR = previousTmpdir;
    rmSync(registry, { recursive: true, force: true });
  });
  const put = (path, contents) => {
    mkdirSync(resolve(root, path, ".."), { recursive: true });
    writeFileSync(resolve(root, path), contents);
  };
  put("demo/package.json", JSON.stringify({ packageManager: "pnpm@9.15.4" }));
  // The fake corepack below runs the fixture compiler for every recipe, so
  // the recipe must say so: the trace binds each command the recipe names.
  put(
    "demo/example/package.json",
    JSON.stringify({
      name: "example",
      type: "module",
      scripts: {
        build: "guarded",
        "build:contrib-raw": "node ../scripts/fixture-compiler.mjs",
      },
    }),
  );
  for (const [directory, name] of [
    ["midgard-node", "midgard-node"],
    ["l1-node-transport", "@al-ft/l1-node-transport"],
  ]) {
    put(
      `demo/${directory}/package.json`,
      JSON.stringify({
        name,
        scripts: {
          build: "guarded",
          "build:contrib-raw": "node ../scripts/fixture-compiler.mjs",
          "native:mpf-owner:build:contrib-raw": "fixture compiler",
          "native:build:contrib-raw": "fixture compiler",
        },
      }),
    );
  }
  put("demo/midgard-node/native/mpf-event-flat-wasm/Cargo.toml", "[package]\n");
  put("demo/l1-node-transport/native/go.mod", "module fixture\n");
  put("demo/midgard-node/scripts/generate.mjs", "// fixture generator\n");
  put(
    ".agents/skills/regenerating-goldens-and-ledgers/scripts/channels.json",
    JSON.stringify({
      inputSets: {},
      channels: [
        {
          id: "native-composition",
          inputs: ["demo/midgard-node/scripts/generate.mjs"],
          generators: ["demo/midgard-node/scripts/generate.mjs"],
          outputs: ["demo/midgard-node/generated.json"],
          check: { run: "node check-native.mjs" },
        },
      ],
    }),
  );
  put(
    "check-native.mjs",
    "import {rmSync} from 'node:fs';if(process.env.CONTRIB_FIXTURE_DROP_NATIVE==='1')rmSync('demo/l1-node-transport/dist/native',{recursive:true});\n",
  );
  // A shared build input, so the traced compiler reads only bound files.
  put(
    "demo/scripts/fixture-compiler.mjs",
    "import {mkdirSync,writeFileSync} from 'node:fs';const recipe=process.argv[2];const output=recipe==='build:contrib-raw'?'dist/index.js':recipe==='native:build:contrib-raw'?'dist/native/midgard-l1-node-transport':'native/mpf-event-flat-wasm/target/release/architecture-g-owner';mkdirSync(output.slice(0,output.lastIndexOf('/')),{recursive:true});writeFileSync(output,'fixture output');\n",
  );
  put(
    "guarded-build.mjs",
    `import {resolve} from 'node:path';import {buildPackage} from ${JSON.stringify(new URL("./build.mjs", import.meta.url).href)};for(const name of ['example','midgard-node','@al-ft/l1-node-transport']){const result=await buildPackage(resolve(process.cwd(),'..'),name);if(result.exitCode)process.exit(result.exitCode);}\n`,
  );
  writeFileSync(
    resolve(tools, "corepack"),
    '#!/bin/sh\nset -eu\nshift\nwhile [ "${1#--config.}" != "$1" ]; do shift; done\nif [ "$1" = run ] && [ "$2" = build ]; then exec node ../guarded-build.mjs; fi\nif [ "$1" = run ]; then exec node ../scripts/fixture-compiler.mjs "$2"; fi\n',
    { mode: 0o755 },
  );
  for (const name of ["go", "cargo", "rustc"]) {
    writeFileSync(
      resolve(tools, name),
      '#!/bin/sh\nprintf "fixture compiler v1\\n"\n',
      { mode: 0o755 },
    );
  }
  process.env.PATH = `${tools}:${previousPath}`;
  process.env.TMPDIR = registry;
  // These tiny compilers exercise orchestration, not a production build claim.
  const intact = await reproduce(root, { execute: true });
  assert.equal(intact.status, "passed", intact.reason);
  const deleted = await reproduce(root, {
    execute: true,
    env: { ...process.env, CONTRIB_FIXTURE_DROP_NATIVE: "1" },
  });
  assert.equal(
    deleted.status,
    "failed",
    "deleted native output cannot earn a reproduction pass",
  );
  assert.match(
    deleted.reason,
    /final native output.*@al-ft\/l1-node-transport.*missing/u,
  );
});
