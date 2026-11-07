import { describe, expect, it } from "vitest";

import {
  encodeWorkflowArtifact,
  requireWorkflowArtifactMatches,
} from "../src/workflow/artifact-codec.js";

describe("typed proof artifact persistence", () => {
  it("encodes every node without type collisions and returns only freshly rebuilt material", () => {
    const fresh = {
      integer: 12345678901234567890n,
      bytes: Buffer.from("abcd", "hex"),
      nested: [null, true, "bytes", ["bigint", "1"]],
    };
    const recorded = encodeWorkflowArtifact(fresh);
    expect(
      encodeWorkflowArtifact({
        nested: fresh.nested,
        bytes: fresh.bytes,
        integer: fresh.integer,
      }),
    ).toEqual(recorded);
    expect(requireWorkflowArtifactMatches(recorded, fresh)).toBe(fresh);
    expect(encodeWorkflowArtifact(1n)).not.toEqual(encodeWorkflowArtifact("1"));
    expect(encodeWorkflowArtifact(Buffer.from("ab", "hex"))).not.toEqual(
      encodeWorkflowArtifact(["bytes", "ab"]),
    );
    expect(() =>
      requireWorkflowArtifactMatches(recorded, { ...fresh, integer: 1n }),
    ).toThrow("differs");
  });
  it("rejects values which cannot be reconstructed deterministically without executing code", () => {
    const cyclic: Record<string, unknown> = {};
    cyclic.self = cyclic;
    const accessor = Object.defineProperty({}, "secret", {
      enumerable: true,
      get: () => {
        throw new Error("getter executed");
      },
    });
    const accessorArray = Object.defineProperty([1], "0", {
      get: () => {
        throw new Error("getter executed");
      },
    });
    const sparse: unknown[] = [];
    sparse.length = 2;
    for (const value of [
      undefined,
      NaN,
      -0,
      1.5,
      new Date(),
      new Map(),
      cyclic,
      accessor,
      accessorArray,
      sparse,
      Object.defineProperty({}, "hidden", { value: 1 }),
    ])
      expect(() => encodeWorkflowArtifact(value)).toThrow(/workflow artifact/);
  });
});
