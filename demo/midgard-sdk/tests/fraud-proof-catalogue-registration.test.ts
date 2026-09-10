/**
 * The fraud-proof catalogue is append-only. A category's four-byte id is
 * written into the computation-thread asset name of every proof of that
 * family, so renumbering or reusing an id silently redirects (or collides
 * with) threads of an already-deployed family.
 *
 * The oracle is a second, independently maintained table: `midgard-core`'s
 * deployment-manifest identity registry carries its own category order and id
 * map, and the deployment manifest is verified against it. Comparing the two
 * makes each side a check on the other, so a renumber applied in one place
 * fails here instead of transcribing the SDK's table back into this file
 * (where every new family would mean a hand edit and no added signal).
 */
import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { describe, expect, it } from "vitest";

import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT,
} from "../src/index.js";

describe("production fraud-proof catalogue registration", () => {
  it("agrees with the deployment-manifest registry on every category id", () => {
    expect(FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER).toEqual(
      DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    );
    expect({ ...FRAUD_PROOF_CATALOGUE_CATEGORY_IDS }).toEqual({
      ...DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
    });
  });

  it("keeps the id map total, sparse-safe and collision-free", () => {
    // Derived invariants, independent of either table's contents: identity is
    // carried by the map, never by array position, so the map must cover the
    // order exactly, ids must be the declared width, and no two families may
    // share one.
    expect(Object.keys(FRAUD_PROOF_CATALOGUE_CATEGORY_IDS)).toEqual(
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    );
    const ids = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map(
      (name) => FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name],
    );
    const width = FRAUD_PROOF_CATALOGUE_ID_BYTE_COUNT * 2;
    const malformed = ids.filter(
      (id) => !new RegExp(`^[0-9a-f]{${width.toString()}}$`, "u").test(id),
    );
    expect(malformed).toEqual([]);
    const seen = new Map<string, string[]>();
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.forEach((name) => {
      const id = FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[name];
      seen.set(id, [...(seen.get(id) ?? []), name]);
    });
    expect([...seen.entries()].filter(([, names]) => names.length > 1)).toEqual(
      [],
    );
  });
});
