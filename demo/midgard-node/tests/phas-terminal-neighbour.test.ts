import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import blake2b from "blake2b";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import {
  keyValuePhasNonMembershipProof,
  keyValuePhasProof,
  keyValuePhasRoot,
  verifyKeyValuePhasMembershipProof,
  verifyKeyValuePhasNonMembershipProof,
} from "../src/mpf/index.js";

describe("PHAS terminal neighbour", () => {
  // path(04) = 6 4 2 2 ..., path(0106) = 6 a 9 5 ..., path(05) = f b 3 d ...
  it.effect(
    "refuses a PHAS non-membership proof whose terminal leaf skip passes the true divergence",
    () =>
      Effect.gen(function* () {
        const absent = Buffer.from("04", "hex");
        const keys = [Buffer.from("0106", "hex")];
        const values = [Buffer.from("0b", "hex")];
        const root = yield* keyValuePhasRoot(keys, values);
        const proof = yield* keyValuePhasNonMembershipProof(
          keys,
          values,
          absent,
        );
        expect(proof).toHaveLength(1);
        const [terminal] = proof;
        if (terminal === undefined || !("Leaf" in terminal)) {
          throw new Error("expected a terminal leaf step");
        }
        expect(terminal.Leaf.skip).toBe(1n);
        yield* verifyKeyValuePhasNonMembershipProof({
          root,
          key: absent,
          proof,
        });

        const inflated = yield* verifyKeyValuePhasNonMembershipProof({
          root,
          key: absent,
          proof: [{ Leaf: { ...terminal.Leaf, skip: 2n } }],
        }).pipe(Effect.either);

        expect(inflated._tag).toBe("Left");
      }),
  );

  it.effect(
    "refuses a PHAS non-membership proof that re-reads a present leaf as a terminal fork",
    () =>
      Effect.gen(function* () {
        const present = Buffer.from("04", "hex");
        const presentValue = Buffer.from("0a", "hex");
        const keys = [present, Buffer.from("05", "hex")];
        const values = [presentValue, Buffer.from("0c", "hex")];
        const root = yield* keyValuePhasRoot(keys, values);
        const membership = yield* keyValuePhasProof(keys, values, present);
        expect(membership).toHaveLength(1);
        yield* verifyKeyValuePhasMembershipProof({
          root,
          key: present,
          value: presentValue,
          proof: membership,
        });
        // The present leaf sits at odd cursor 1. Re-read as a branch at nibble
        // 0 whose prefix is its suffix without the leaf marker, it collapses to
        // a branch hash, and leaf and branch node preimages are disjoint.
        const path = Buffer.from(blake2b(32).update(present).digest());
        const masquerade = {
          Fork: {
            skip: 0n,
            neighbor: {
              nibble: 0n,
              prefix: Buffer.concat([
                Buffer.from([path[0]! % 16]),
                path.subarray(1),
              ]).toString("hex"),
              root: Buffer.from(
                blake2b(32).update(presentValue).digest(),
              ).toString("hex"),
            },
          },
        } as SDK.ProofStep;

        const absent = yield* verifyKeyValuePhasNonMembershipProof({
          root,
          key: present,
          proof: [...membership, masquerade],
        }).pipe(Effect.either);

        expect(absent._tag).toBe("Left");
      }),
  );
});
