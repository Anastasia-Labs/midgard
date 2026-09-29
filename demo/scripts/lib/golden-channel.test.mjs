import assert from "node:assert/strict";
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
