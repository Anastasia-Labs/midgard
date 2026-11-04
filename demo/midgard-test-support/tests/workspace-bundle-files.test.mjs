import assert from "node:assert/strict";
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { test } from "node:test";

import { scan } from "../workspace-bundle-files.js";

/** `scan` of a fresh file holding `text`. */
const scanText = (t, text) => {
  const directory = mkdtempSync(join(tmpdir(), "midgard-bundle-scan-test-"));
  t.after(() => rmSync(directory, { recursive: true, force: true }));
  const file = join(directory, "module.ts");
  writeFileSync(file, text);
  return scan(file);
};

test("a dynamic import of a run-time specifier is a computed import", (t) => {
  assert.equal(
    scanText(
      t,
      "export const load = async (url: string) => (await import(url)).default;\n",
    ).importsComputedSpecifier,
    true,
  );
});

test("a dynamic import of a literal, on one line or across lines, is not", (t) => {
  assert.equal(
    scanText(
      t,
      [
        'export const a = () => import("./a.js");',
        "export const b = () =>",
        "  import(",
        "    // the second module",
        '    "./b.js"',
        "  );",
        "",
      ].join("\n"),
    ).importsComputedSpecifier,
    false,
  );
});

test("prose about an import in a comment is not a computed import", (t) => {
  assert.equal(
    scanText(
      t,
      [
        "/**",
        " * A local fault of a foreign block's import (within the runner's",
        " * bound) is retried.",
        " */",
        "// a failed import (any kind) is held",
        'export const HELD = "held";',
        "",
      ].join("\n"),
    ).importsComputedSpecifier,
    false,
  );
});
