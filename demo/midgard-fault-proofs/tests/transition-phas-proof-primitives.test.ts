import { describe, expect, it } from "vitest";

import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";

describe("PHAS proof primitives", () => {
  it("keeps successive absence openings and later membership on the same committed root", async () => {
    const key = Buffer.from("one");
    const value = Buffer.from("committed value");
    const root = await keyValuePhasRootWithCount([{ key, value }]);
    const expectedMembership = await keyValuePhasProof(root, key, value);
    await keyValuePhasNonMembershipProof(root, Buffer.from("missing one"));
    await keyValuePhasNonMembershipProof(root, Buffer.from("missing two"));
    expect(await keyValuePhasProof(root, key, value)).toEqual(
      expectedMembership,
    );
  });
});
