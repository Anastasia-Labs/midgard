// Tests for compare-blueprint-hashes.mjs. Each test writes small synthetic
// blueprints to a temporary directory and runs the script on them as a child
// process, so exit codes and output are checked exactly as a caller sees them.
//
//   node --test .agents/skills/splitting-oversized-modules/scripts/compare-blueprint-hashes.test.mjs

import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { after, test } from "node:test";
import { fileURLToPath } from "node:url";

const SCRIPT = fileURLToPath(
  new URL("./compare-blueprint-hashes.mjs", import.meta.url),
);

const dir = mkdtempSync(join(tmpdir(), "compare-blueprint-hashes-"));
after(() => rmSync(dir, { recursive: true, force: true }));

const validator = (title, hash, compiledCode) => ({
  title,
  redeemer: { schema: { $ref: "#/definitions/Data" } },
  compiledCode,
  hash,
});

const blueprint = ({
  validators = [
    validator("correction_lock.spend.spend", "aa01", "5901aa"),
    validator("correction_lock.spend.else", "aa01", "5901aa"),
    validator("hub_oracle.mint.mint", "bb02", "5902bb"),
  ],
  definitions = { Data: {}, "midgard/correction-lock/Datum": {} },
  compiler = "v1.1.22+fork",
  plutusVersion = "v3",
} = {}) => ({
  preamble: {
    title: "midgard/validators",
    compiler: { name: "Aiken", version: compiler },
    plutusVersion,
  },
  validators,
  definitions,
});

let counter = 0;
const write = (content) => {
  counter += 1;
  const path = join(dir, `blueprint-${counter}.json`);
  writeFileSync(
    path,
    typeof content === "string" ? content : JSON.stringify(content),
  );
  return path;
};

const run = (...args) => {
  const result = spawnSync(process.execPath, [SCRIPT, ...args], {
    encoding: "utf8",
  });
  return {
    code: result.status,
    stdout: result.stdout,
    stderr: result.stderr,
  };
};

test("identical blueprints exit 0", () => {
  const result = run(write(blueprint()), write(blueprint()));
  assert.equal(result.code, 0, result.stderr);
  assert.match(
    result.stdout,
    /^Blueprint validators identical: 3 validators before, 3 after/u,
  );
  assert.doesNotMatch(result.stdout, /definitions keys moved/u);
});

test("validator order does not matter", () => {
  const reordered = blueprint();
  reordered.validators.reverse();
  const result = run(write(blueprint()), write(reordered));
  assert.equal(result.code, 0, result.stdout);
});

test("a changed hash exits 1 and names the validator", () => {
  const changed = blueprint();
  changed.validators[0] = validator(
    "correction_lock.spend.spend",
    "ff09",
    "5901aa",
  );
  const result = run(write(blueprint()), write(changed));
  assert.equal(result.code, 1);
  assert.match(result.stdout, /^Blueprint validators DIFFER: 1 finding/u);
  assert.match(
    result.stdout,
    /changed {5}correction_lock\.spend\.spend: hash aa01 -> ff09/u,
  );
});

test("changed compiled code under the same hash still exits 1", () => {
  const changed = blueprint();
  changed.validators[2] = validator("hub_oracle.mint.mint", "bb02", "5902bc");
  const result = run(write(blueprint()), write(changed));
  assert.equal(result.code, 1);
  assert.match(result.stdout, /changed {5}hub_oracle\.mint\.mint/u);
});

test("a moved validator file shows as missing and unexpected", () => {
  const moved = blueprint();
  moved.validators[2] = validator("hub_oracle_v2.mint.mint", "bb02", "5902bb");
  const result = run(write(blueprint()), write(moved));
  assert.equal(result.code, 1);
  assert.match(result.stdout, /DIFFER: 2 finding/u);
  assert.match(result.stdout, /missing {5}hub_oracle\.mint\.mint/u);
  assert.match(result.stdout, /unexpected {2}hub_oracle_v2\.mint\.mint/u);
});

test("an added validator exits 1", () => {
  const added = blueprint();
  added.validators.push(validator("extra.spend.spend", "cc03", "5903cc"));
  const result = run(write(blueprint()), write(added));
  assert.equal(result.code, 1);
  assert.match(result.stdout, /unexpected {2}extra\.spend\.spend/u);
});

test("moved definitions keys are reported but still exit 0", () => {
  const moved = blueprint({
    definitions: { Data: {}, "midgard/correction-lock-types/Datum": {} },
  });
  const result = run(write(blueprint()), write(moved));
  assert.equal(result.code, 0, result.stdout);
  assert.match(result.stdout, /^Blueprint validators identical/u);
  assert.match(
    result.stdout,
    /Note: definitions keys moved \(1 only before, 1 only after\)/u,
  );
  assert.match(
    result.stdout,
    /only before {2}midgard\/correction-lock\/Datum/u,
  );
  assert.match(
    result.stdout,
    /only after {3}midgard\/correction-lock-types\/Datum/u,
  );
});

test("different compilers cannot be compared (exit 3)", () => {
  const result = run(
    write(blueprint()),
    write(blueprint({ compiler: "v1.1.22+stock" })),
  );
  assert.equal(result.code, 3);
  assert.match(result.stderr, /could not compare: .*different compilers/u);
  assert.equal(result.stdout, "");
});

test("different Plutus versions cannot be compared (exit 3)", () => {
  const result = run(
    write(blueprint()),
    write(blueprint({ plutusVersion: "v2" })),
  );
  assert.equal(result.code, 3);
  assert.match(result.stderr, /different Plutus versions/u);
});

test("a title named twice cannot be compared (exit 3)", () => {
  const duplicated = blueprint();
  duplicated.validators.push(
    validator("hub_oracle.mint.mint", "bb02", "5902bb"),
  );
  const result = run(write(blueprint()), write(duplicated));
  assert.equal(result.code, 3);
  assert.match(result.stderr, /names validator hub_oracle\.mint\.mint twice/u);
});

test("malformed, unreadable or non-blueprint input exits 3, not 0", () => {
  const good = write(blueprint());
  for (const [label, bad, pattern] of [
    ["not JSON", write("{ not json"), /could not read/u],
    ["missing file", join(dir, "does-not-exist.json"), /could not read/u],
    ["no validators", write({ preamble: {} }), /no validators array/u],
    ["JSON null", write("null"), /no validators array/u],
    [
      "validator without hash",
      write({ validators: [{ title: "x.spend.spend", compiledCode: "59" }] }),
      /without a string title, hash and compiledCode/u,
    ],
  ]) {
    for (const args of [
      [good, bad],
      [bad, good],
    ]) {
      const result = run(...args);
      assert.equal(result.code, 3, `${label}: ${result.stdout}`);
      assert.match(result.stderr, pattern, label);
    }
  }
});

test("wrong argument count or an option exits 2", () => {
  const good = write(blueprint());
  for (const args of [[], [good], [good, good, good], ["--help", good]]) {
    const result = run(...args);
    assert.equal(result.code, 2, args.join(" "));
    assert.match(result.stderr, /^Usage: /u);
  }
});
