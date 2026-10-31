import assert from "node:assert/strict";
import { test } from "node:test";

import { blueprintAbi, renderReviewSurface } from "./review-surface.mjs";

const redeemer = (index) => ({
  title: "Redeemer",
  anyOf: [
    { title: "Spend", dataType: "constructor", index: 0, fields: [] },
    { title: "Close", dataType: "constructor", index, fields: [] },
  ],
});

const blueprint = ({ closeIndex = 1, code = "abcd", extra = [] } = {}) => ({
  preamble: { title: "fixture" },
  validators: [
    {
      title: "hub.hub.spend",
      parameters: [
        { title: "policy", schema: { $ref: "#/definitions/PolicyId" } },
      ],
      redeemer: {
        title: "redeemer",
        schema: { $ref: "#/definitions/hub~1Redeemer" },
      },
      compiledCode: code,
      hash: `hash-${code}`,
    },
    ...extra,
  ],
  definitions: {
    PolicyId: { dataType: "bytes" },
    "hub/Redeemer": redeemer(closeIndex),
    "unused/Thing": { dataType: "integer" },
  },
});

const surface = (base, head) =>
  renderReviewSurface(blueprintAbi(base), blueprintAbi(head));

test("an identical build reports no change", () => {
  const text = surface(blueprint(), blueprint());
  assert.match(text, /No on-chain ABI change\./u);
  assert.match(text, /No compiled-size change\./u);
});

test("a redeemer constructor index change is an ABI change of the definition it reaches", () => {
  const text = surface(blueprint(), blueprint({ closeIndex: 2 }));
  assert.match(text, /\*\*Definitions changed\*\*\n\n- `hub\/Redeemer`/u);
  assert.doesNotMatch(text, /No on-chain ABI change/u);
});

test("a code-only change moves size but not the ABI", () => {
  const text = surface(blueprint(), blueprint({ code: "abcdef" }));
  assert.match(text, /No on-chain ABI change\./u);
  assert.match(text, /\| `hub\.hub\.spend` \| 2 \| 3 \| \+1 \|/u);
});

test("added validators and unreached definitions", () => {
  const added = {
    title: "hub.hub.mint",
    redeemer: { title: "r", schema: { $ref: "#/definitions/PolicyId" } },
    compiledCode: "00",
  };
  const text = surface(blueprint(), blueprint({ extra: [added] }));
  assert.match(text, /\*\*Validators added\*\*\n\n- `hub\.hub\.mint`/u);
  // A definition no validator reaches is not part of the ABI.
  assert.doesNotMatch(text, /unused\/Thing/u);
});

test("a blueprint without validators is refused, never read as unchanged", () => {
  assert.throws(() => blueprintAbi({ validators: [] }), /no validators/u);
});
