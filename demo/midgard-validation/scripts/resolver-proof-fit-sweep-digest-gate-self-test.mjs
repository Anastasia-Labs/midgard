#!/usr/bin/env node

/**
 * Falsifiability proof for the resolver-sweep digest gate
 * (`resolver-proof-fit-sweep-inputs-digest-v1.mjs`): a staleness gate that
 * skips a 9-minute regeneration is only safe if every declared closure
 * member demonstrably participates in the digest — otherwise it is a gate
 * that cannot fail, silently blessing a stale artifact. This mutates one
 * representative member per closure entry through the compute function's
 * `transform` injection point (never the working tree) and asserts the
 * digest moves; then asserts a transform aimed at a label outside the
 * closure moves nothing.
 *
 * usage: node scripts/resolver-proof-fit-sweep-digest-gate-self-test.mjs
 */

import {
  RESOLVER_SWEEP_INPUT_CLOSURE_V1,
  computeResolverSweepInputsDigestV1,
} from "./resolver-proof-fit-sweep-inputs-digest-v1.mjs";

let failures = 0;
const check = (label, ok) => {
  if (ok) {
    process.stdout.write(`  PASS ${label}\n`);
  } else {
    failures += 1;
    process.stderr.write(`  FAIL ${label}\n`);
  }
};

const baseline = computeResolverSweepInputsDigestV1();
const flipFirstByte = (buffer) => {
  const copy = Buffer.from(buffer);
  if (copy.length === 0) {
    return Buffer.from([1]);
  }
  copy[0] = copy[0] ^ 0xff;
  return copy;
};

// One mutation per declared closure entry. For `tree` entries the transform
// targets every label under the tree's prefix, so the assertion holds even
// if the tree's first file changes over time; what is proven is that the
// TREE participates, which is the declaration being tested.
for (const member of RESOLVER_SWEEP_INPUT_CLOSURE_V1) {
  const prefix = member.kind === "lockSelection" ? `lock:${member.path}` : member.path;
  const mutated = computeResolverSweepInputsDigestV1({
    transform: (label, buffer) =>
      label === prefix || label.startsWith(`${prefix}/`)
        ? flipFirstByte(buffer)
        : buffer,
  });
  check(
    `${member.kind} ${member.path} participates in the digest`,
    mutated !== baseline,
  );
}

// A transform that matches nothing must change nothing: the closure is
// exactly its declared members, and the identity transform reproduces the
// baseline (determinism).
const untouched = computeResolverSweepInputsDigestV1({
  transform: (label, buffer) =>
    label === "not/a/declared/member" ? flipFirstByte(buffer) : buffer,
});
check("a label outside the closure moves nothing", untouched === baseline);
check(
  "the digest is deterministic",
  computeResolverSweepInputsDigestV1() === baseline,
);

if (failures > 0) {
  process.stderr.write(
    `resolver sweep digest gate self-test FAILED (${failures})\n`,
  );
  process.exit(1);
}
process.stdout.write("resolver sweep digest gate self-test PASS\n");
