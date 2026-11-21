import assert from "node:assert/strict";
import { test } from "node:test";
import { createHash } from "node:crypto";

import {
  compareBlueprintRebuilds,
  parseBlueprintRebuildArguments,
} from "./blueprint-rebuild.mjs";

const blueprint = Buffer.from(
  JSON.stringify({
    validators: [{ title: "known.spend.spend", compiledCode: "00" }],
  }),
);
const stamp = {
  blueprintHash: createHash("sha256").update(blueprint).digest("hex"),
  sourceHash: "source",
  profileDigest: "profile",
};
const builds = () =>
  [1, 2].map((index) => ({
    root: `fresh-${index}`,
    bytes: blueprint,
    stamp,
    compilerExitCode: 0,
  }));

test("requires two actual independent successful compiler executions", () => {
  assert.equal(compareBlueprintRebuilds(builds(), blueprint).validatorCount, 1);
  assert.throws(
    () => compareBlueprintRebuilds(builds().slice(0, 1), blueprint),
    /Two successful/,
  );
  assert.throws(
    () =>
      compareBlueprintRebuilds(
        [{ ...builds()[0], compilerExitCode: 1 }, builds()[1]],
        blueprint,
      ),
    /Two successful/,
  );
  assert.throws(
    () =>
      compareBlueprintRebuilds(
        [builds()[0], { ...builds()[1], root: "fresh-1" }],
        blueprint,
      ),
    /roots must differ/,
  );
});
test("refuses changed compiled bytes even when metadata claims the release hash", () => {
  const changed = Buffer.from(
    JSON.stringify({
      validators: [{ title: "known.spend.spend", compiledCode: "01" }],
    }),
  );
  assert.throws(
    () =>
      compareBlueprintRebuilds(
        [builds()[0], { ...builds()[1], bytes: changed }],
        blueprint,
      ),
    /differs/,
  );
});
test("refuses changed source/profile identities", () => {
  for (const key of ["sourceHash", "profileDigest", "blueprintHash"]) {
    assert.throws(
      () =>
        compareBlueprintRebuilds(
          [
            builds()[0],
            { ...builds()[1], stamp: { ...stamp, [key]: "other" } },
          ],
          blueprint,
        ),
      /differs/,
    );
  }
});
test("does not accept zero or duplicate collected validators", () => {
  for (const validators of [
    [],
    [{ title: "duplicate" }, { title: "duplicate" }],
  ]) {
    const bytes = Buffer.from(JSON.stringify({ validators }));
    const hash = createHash("sha256").update(bytes).digest("hex");
    const inputs = builds().map((build) => ({
      ...build,
      bytes,
      stamp: { ...stamp, blueprintHash: hash },
    }));
    assert.throws(
      () => compareBlueprintRebuilds(inputs, bytes),
      /validators|duplicated/,
    );
  }
});
test("argument gate refuses moving revisions and nonpublic profiles before writing", () => {
  assert.throws(
    () => parseBlueprintRebuildArguments(["--revision", "HEAD"]),
    /frozen|Usage/,
  );
  assert.throws(
    () => parseBlueprintRebuildArguments(["--profile", "preprod-testing"]),
    /frozen|Usage/,
  );
  assert.throws(
    () => parseBlueprintRebuildArguments(["--root", "/tmp", "--root", "/tmp"]),
    /Usage/,
  );
});
