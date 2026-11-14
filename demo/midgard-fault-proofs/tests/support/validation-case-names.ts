/**
 * The token names of a validation case (`midgard/validation_case_v1`): the
 * case NFT, the bond marker, and the thread token a defense mints (whose name
 * the lost-defense receipt shares).
 */
import { hashHexWithBlake2b } from "@al-ft/midgard-sdk";
import { Constr, Data, fromText } from "@lucid-evolution/lucid";
import { Effect } from "effect";

const blake2b256 = (hex: string): string =>
  Effect.runSync(hashHexWithBlake2b(hex, 32));

/** hash(header hash ‖ cbor(event key) ‖ cbor(opener out-ref)). */
export const caseIdOf = (
  headerHash: string,
  eventKeyCbor: string,
  opener: { readonly txHash: string; readonly outputIndex: number },
): string =>
  blake2b256(
    headerHash +
      eventKeyCbor +
      Data.to(new Constr(0, [opener.txHash, BigInt(opener.outputIndex)])),
  );

/** hash("bond" ‖ case id). */
export const bondMarkerName = (caseId: string): string =>
  blake2b256(fromText("bond") + caseId);

/** hash(case id ‖ defender key hash): the thread token and its receipt. */
export const threadTokenName = (caseId: string, defenderVkh: string): string =>
  blake2b256(caseId + defenderVkh);
