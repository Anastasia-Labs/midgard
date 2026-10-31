import { describe, expect, it } from "vitest";

import {
  canonicalJson,
  compareCanonicalJsonKeys,
} from "../src/canonical-json.js";

describe("canonical JSON key order", () => {
  it("sorts keys by the pinned en collation, not the host locale", () => {
    // `et` collation orders "z" before "t"; the pinned order must not.
    expect(["sz", "st"].sort(compareCanonicalJsonKeys)).toEqual(["st", "sz"]);
    expect(canonicalJson({ sz: 1, st: 2 }, "test record")).toBe(
      '{"st":2,"sz":1}',
    );
  });

  it("keeps the collation order existing digests were produced with", () => {
    // UTF-16 code-unit order would put maxOutputValueCborBytes first ("V" <
    // "s"); the V1 consensus-profile digest depends on this order.
    expect(
      ["maxOutputValueCborBytes", "maxOutputsPreimageBytes"].sort(
        compareCanonicalJsonKeys,
      ),
    ).toEqual(["maxOutputsPreimageBytes", "maxOutputValueCborBytes"]);
  });
});
