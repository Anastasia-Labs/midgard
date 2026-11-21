import assert from "node:assert/strict";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";
import test from "node:test";

import { atomicJson } from "./files.mjs";
import { fixture } from "./fixture.test-support.mjs";
import { generateDevnet } from "./operations.mjs";
import { verifyReceipt } from "./receipts.mjs";

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
