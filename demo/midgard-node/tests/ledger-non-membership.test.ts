import * as SDK from "@al-ft/midgard-sdk";
import { it } from "@effect/vitest";
import { Effect } from "effect";
import { afterAll, beforeAll, describe, expect } from "vitest";

import { deleteMpfStore, MidgardMpf } from "../src/workers/utils/mpf.js";
import { buildLedgerNonMembershipProof } from "../src/workers/utils/mpf/ledger-non-membership.js";

const LEDGER_DB = "test-ledger-non-membership-db";

// Stand-ins for Cardano `TransactionInput` CBOR ledger keys.
const removedKey = Buffer.from("a0", "hex"); // present at R1, removed by R2
const stableKey = Buffer.from("b1", "hex"); // present at both R1 and R2
const addedKey = Buffer.from("d3", "hex"); // added by R2
const neverKey = Buffer.from("c2", "hex"); // never in the ledger

const value = (byte: string) => Buffer.from(byte, "hex");

// Historical ledger roots captured while building the store.
let r1 = "";
let r2 = "";

beforeAll(async () => {
  await Effect.runPromise(deleteMpfStore(LEDGER_DB, "test-ledger-nm"));
  await Effect.runPromise(
    Effect.gen(function* () {
      const mpf = yield* MidgardMpf.create("test-ledger-nm", LEDGER_DB);
      // R1: the prev-utxos state the "bad block" sits on.
      yield* mpf.insert(removedKey, value("01"));
      yield* mpf.insert(stableKey, value("02"));
      r1 = yield* mpf.rootHex();
      // Advance the ledger to R2 (subsequent blocks): drop removedKey, add addedKey.
      yield* mpf.delete(removedKey);
      yield* mpf.insert(addedKey, value("03"));
      r2 = yield* mpf.rootHex();
      yield* mpf.close();
    }),
  );
});

afterAll(async () => {
  await Effect.runPromise(deleteMpfStore(LEDGER_DB, "test-ledger-nm"));
});

// Independently reconstructs the historical trie at `root`, inserts the absent
// key, and returns the root the resulting proof reconstructs in exclusion mode
// (`verify(false)`) — exactly what the on-chain `pexcludes` validator checks.
const exclusionVerifiedRoot = (root: string, key: Buffer) =>
  Effect.gen(function* () {
    const mpf = yield* MidgardMpf.loadReadOnlyOverlay(
      "verify",
      LEDGER_DB,
      Buffer.from(root, "hex"),
    );
    yield* mpf.insert(key, Buffer.from(""));
    const proof = yield* mpf.prove(key);
    yield* mpf.close();
    return Buffer.from(proof.proof.verify(false)).toString("hex");
  });

describe("ledger non-membership over a non-empty ledger", () => {
  it.effect(
    "builds a proof against a historical prev-utxos root that reconstructs it",
    () =>
      Effect.gen(function* () {
        expect(r1).not.toBe(r2);
        expect(r1).not.toBe(SDK.EMPTY_MERKLE_TREE_ROOT);

        const result = yield* buildLedgerNonMembershipProof({
          ledgerDbPath: LEDGER_DB,
          prevUtxosRoot: r1,
          inputCbor: neverKey.toString("hex"),
        });

        expect(result.prevUtxosRoot).toBe(r1);
        expect(result.input).toBe(neverKey.toString("hex"));
        expect(result.proofCbor.length).toBeGreaterThan(0);

        // The proof must reconstruct the historical root R1, not the live R2.
        const verified = yield* exclusionVerifiedRoot(r1, neverKey);
        expect(verified).toBe(r1);
      }),
  );

  it.effect(
    "reads the historical root, not the live one: a later-removed key is present at R1 but absent at R2",
    () =>
      Effect.gen(function* () {
        // removedKey is gone from the current ledger (R2), so a proof there succeeds...
        const atR2 = yield* buildLedgerNonMembershipProof({
          ledgerDbPath: LEDGER_DB,
          prevUtxosRoot: r2,
          inputCbor: removedKey.toString("hex"),
        });
        expect(atR2.proofCbor.length).toBeGreaterThan(0);

        // ...but at R1 it was present, so there is no non-existent-input fraud.
        const atR1 = yield* buildLedgerNonMembershipProof({
          ledgerDbPath: LEDGER_DB,
          prevUtxosRoot: r1,
          inputCbor: removedKey.toString("hex"),
        }).pipe(Effect.either);

        expect(atR1._tag).toBe("Left");
        if (atR1._tag === "Left") {
          expect(atR1.left._tag).toBe("LedgerInputPresentError");
        }
      }),
  );

  it.effect("serves the empty root without opening the ledger store", () =>
    Effect.gen(function* () {
      const result = yield* buildLedgerNonMembershipProof({
        // A bogus path proves the empty-root path never touches LevelDB.
        ledgerDbPath: "this-ledger-db-path-does-not-exist",
        prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        inputCbor: neverKey.toString("hex"),
      });

      expect(result.prevUtxosRoot).toBe(SDK.EMPTY_MERKLE_TREE_ROOT);
      expect(result.proofCbor.length).toBeGreaterThan(0);
    }),
  );
});
