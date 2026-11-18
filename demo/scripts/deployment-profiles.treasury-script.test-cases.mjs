import assert from "node:assert/strict";
import { test } from "node:test";

import { encodeNativeScript } from "./deployment-profile-aiken.mjs";

test("the treasury native script renders as its ledger CBOR", () => {
  // The vector `midgard/dispute_bond_v1.test` and the SDK pin too, checked
  // there against the ledger's own encoder and hash.
  const key = (n) => `706c616365686f6c6465723a74726573${"00".repeat(11)}0${n}`;
  const sig = (n) => ({ type: "sig", keyHash: key(n) });
  const sigCbor = (n) => `8200581c${key(n)}`;
  assert.equal(
    encodeNativeScript({
      type: "atLeast",
      required: 2,
      scripts: [1, 2, 3].map(sig),
    }),
    `83030283${sigCbor(1)}${sigCbor(2)}${sigCbor(3)}`,
  );
  assert.equal(
    encodeNativeScript({ type: "all", scripts: [sig(1)] }),
    `820181${sigCbor(1)}`,
  );
  assert.equal(encodeNativeScript({ type: "any", scripts: [] }), "820280");
  assert.equal(encodeNativeScript({ type: "after", slot: 1000 }), "82041903e8");
  assert.equal(encodeNativeScript({ type: "before", slot: 23 }), "820517");
  assert.throws(
    () => encodeNativeScript({ type: "sig", keyHash: "00" }),
    /28-byte/u,
  );
  assert.throws(
    () => encodeNativeScript({ type: "multisig" }),
    /Unknown native/u,
  );
});
