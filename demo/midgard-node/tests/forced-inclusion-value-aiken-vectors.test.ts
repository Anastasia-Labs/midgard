/**
 * The forced-inclusion value the node hashes is the one the chain hashes:
 * `encodeForcedInclusionValueV1` gives, byte for byte, Aiken's
 * `cbor.serialise` of the same value (v1.1.24), with the transaction's
 * compact bytes in 64-byte chunks and the verdict's integers at their edges:
 * a purpose kind past 64 bits (a tag-2 bignum) and a policy index of
 * 2^64 - 1.
 */
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { ForcedTransactionsDB } from "../src/database/index.js";
import { encodedTransaction } from "./forced-transactions.make-signed-effectful-transaction.js";

/** The value's tx id and source for `encodedTransaction()`, as Aiken
 * serialises them; the verdict and the closing break follow. */
const TX_AND_SOURCE =
  "d8799f582075c4ea27a41fe204572bdf9c2bcc0b21b76ca74eeca5337a5f7260" +
  "65e27d334ad8799f5f584083018c582045b0cfc220ceec5b7c1c62c4d4193d38" +
  "e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d419" +
  "3d38e4eba48e8815729ce758405f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c" +
  "62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0002020582045b0cfc220" +
  "ceec5b7c1c62c4d4193d38e4eb5840a48e8815729ce75f9c0ab0e4c1c0582045" +
  "b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c058" +
  "2045b0cfc220ceec5b7c1c62c4d41958403d38e4eba48e8815729ce75f9c0ab0" +
  "e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60" +
  "e095e3ab53582001f4b788593d4f70de2a583a45c2e1e87088bfbdfa29577ae1" +
  "b62aba60e095e3ab5318ff582052408b440567f475c3eee21c9a8be91ed05a31" +
  "e81e175c69fcaec5ccd187258cff5f584083582045b0cfc220ceec5b7c1c62c4" +
  "d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c" +
  "62c4d4193d38e4eba48e8815729ce75f9c58270ab0e4c1c0582045b0cfc220ce" +
  "ec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0ff4a89010101" +
  "010101010101ff";

const VERDICTS: readonly {
  name: string;
  verdict: ForcedTransactionsDB.ForcedInclusionValueV1Input["verdict"];
  aiken: string;
}[] = [
  { name: "a valid verdict", verdict: "ForcedTxValid", aiken: "d87980" },
  {
    name: "a fee below the minimum",
    verdict: { ForcedTxInvalid: { reason: "FeeBelowMinimum" } },
    aiken: "d87a9fd87f80ff",
  },
  {
    name: "a script failure with a purpose kind past 64 bits",
    verdict: {
      ForcedTxInvalid: {
        reason: {
          PlutusExecutionFailed: { purpose_kind: 2n ** 70n, purpose_index: 3n },
        },
      },
    },
    aiken: "d87a9fd905229fc24940000000000000000003ffff",
  },
  {
    name: "a malformed mint item at policy index 2^64 - 1",
    verdict: {
      ForcedTxInvalid: {
        reason: { MintItemMalformed: { policy_index: 2n ** 64n - 1n } },
      },
    },
    aiken: "d87a9fd905289f1bffffffffffffffffffff",
  },
];

describe("the forced-inclusion value the node hashes", () => {
  it.each(VERDICTS)(
    "is Aiken's serialisation of the value with $name",
    async ({ verdict, aiken }) => {
      const { value } = await Effect.runPromise(
        ForcedTransactionsDB.encodeForcedInclusionValueV1({
          nativeTxCbor: encodedTransaction(),
          verdict,
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
        }),
      );
      expect(value.toString("hex")).toBe(`${TX_AND_SOURCE}${aiken}ff`);
    },
  );
});
