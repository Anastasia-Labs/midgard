import { describe, expect, it } from "vitest";

import { vectors } from "../../midgard-core/tests/fixtures/canonical-cbor-head-v1.vectors.mjs";
import { head } from "../src/transition-trace/data-summary-walk.cbor.js";

// The data summary walk's `head` is the TypeScript twin of
// `data_summary_walk_v1`'s, which reads Plutus Data and so also opens an
// indefinite head, as `[major, -1n, next]`. On every other vector it is a
// canonical head reader. Its callers pass offsets inside `[0, end)` and it does
// not guard a negative one, so the negative-offset vectors are left out.
const inDomain = vectors.filter((vector) => vector.offset >= 0);

describe("canonical CBOR head vectors: data summary walk head", () => {
  it("leaves out only the negative-offset vectors", () => {
    expect(vectors.length - inDomain.length).toBe(2);
  });

  it.each(inDomain.map((vector) => [vector.name, vector] as const))(
    "%s",
    (_name, { hex, offset, canonical }) => {
      const bytes = Buffer.from(hex, "hex");
      const read = head(bytes, offset, bytes.length);
      if (canonical.verdict === "accept") {
        expect(read).toEqual([
          canonical.major,
          canonical.value,
          canonical.next,
        ]);
      } else if (canonical.reason === "indefinite") {
        expect(read).toEqual([bytes[offset]! >> 5, -1n, offset + 1]);
      } else {
        expect(read).toBeNull();
      }
    },
  );
});
