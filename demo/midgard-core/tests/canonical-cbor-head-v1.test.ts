import { describe, expect, it } from "vitest";

import { readArgument } from "../src/codec/cbor.read-argument.js";
import {
  type CanonicalCborHeadRefusalReason,
  vectors,
} from "./fixtures/canonical-cbor-head-v1.vectors.mjs";

// `readArgument` is the TypeScript twin of the canonical head readers that
// `canonical-cbor-head-v1-golden.test.ak` holds to the same vectors. It names
// each refusal, so this suite also pins the reason class.
const refusalMessages: Record<CanonicalCborHeadRefusalReason, RegExp> = {
  "out-of-range": /^Unexpected end of CBOR$/u,
  truncated: /^Unexpected end of CBOR while reading argument$/u,
  "non-minimal": /^Non-minimal CBOR integer or length encoding$/u,
  reserved: /^Reserved CBOR additional-info value$/u,
  indefinite: /^Indefinite-length CBOR is not valid$/u,
};

// Every caller walks forward from offset 0, so `readArgument` does not guard a
// negative offset; only the on-chain readers are held to those vectors.
const inDomain = vectors.filter((vector) => vector.offset >= 0);

describe("canonical CBOR head vectors: readArgument", () => {
  it("leaves out only the negative-offset vectors", () => {
    expect(vectors.length - inDomain.length).toBe(2);
  });

  it.each(inDomain.map((vector) => [vector.name, vector] as const))(
    "%s",
    (_name, { hex, offset, canonical }) => {
      const bytes = Buffer.from(hex, "hex");
      if (canonical.verdict === "accept") {
        expect(readArgument(bytes, offset)).toMatchObject({
          major: canonical.major,
          value: canonical.value,
          nextOffset: canonical.next,
        });
      } else {
        expect(() => readArgument(bytes, offset)).toThrow(
          refusalMessages[canonical.reason],
        );
      }
    },
  );
});
