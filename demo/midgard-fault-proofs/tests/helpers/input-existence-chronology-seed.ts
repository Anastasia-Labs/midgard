/** Actual deposit-derived seed outputs and signed key-owned transactions for
 * input-existence chronology. The same entries feed the committed prior
 * deposit block and the checked predecessor ledger; no held root is invented. */
import { computeHash28 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { deriveCanonicalOriginalDepositTransitionEffect } from "@al-ft/midgard-validation";
import { CML } from "@lucid-evolution/lucid";

import type { LedgerEntry } from "../../src/ledger-at.js";
import {
  buildFixtureTransaction,
  type FixtureTransactionInput,
} from "./canonical-block-evidence-fixture.js";

const ownerVerificationKey = () => {
  const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 71));
  try {
    return Buffer.from(key.to_public().to_raw_bytes());
  } finally {
    key.free();
  }
};
export const CHRONOLOGY_OWNER_KEY_HASH = computeHash28(
  ownerVerificationKey(),
).toString("hex");
export const CHRONOLOGY_SEED_VALUE = 2_000_000n;
/** A deposit of the chronology seed value to the owner, identified by
 * `byte` repeated as its L1 transaction id. */
export const chronologyDeposit = (byte: number) => ({
  event: {
    id: {
      transactionId: byte.toString(16).padStart(2, "0").repeat(32),
      outputIndex: 0n,
    },
    info: {
      l2_address: {
        paymentCredential: {
          PublicKeyCredential: [CHRONOLOGY_OWNER_KEY_HASH],
        },
        stakeCredential: null,
      },
      l2_network_id: 0n,
      l2_datum: null,
    },
  } satisfies SDK.DepositEvent,
  originalAssets: { lovelace: CHRONOLOGY_SEED_VALUE },
  honest: true,
});
export const chronologySeedDeposits = () => [81, 82].map(chronologyDeposit);
/** The ledger entry the canonical deposit transition inserts. */
export const chronologyDepositEntry = ({
  event,
  originalAssets,
}: ReturnType<typeof chronologyDeposit>): LedgerEntry => {
  const effect = deriveCanonicalOriginalDepositTransitionEffect({
    configuredNetwork: "Custom",
    eventId: event.id,
    l2Address: event.info.l2_address,
    l2NetworkId: event.info.l2_network_id,
    l2DatumCbor: null,
    originalAssets,
  });
  const operation = effect.operations[0];
  if (operation?.type !== "insert")
    throw new Error("chronology deposit has no canonical insertion");
  return { outRef: operation.outRefCbor, output: operation.outputCbor };
};
export const chronologySeedEntries = (): readonly LedgerEntry[] =>
  chronologySeedDeposits().map(chronologyDepositEntry);
export const signedChronologyTransaction = (input: FixtureTransactionInput) => {
  const unsigned = buildFixtureTransaction(input);
  const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 71));
  try {
    return buildFixtureTransaction({
      ...input,
      addressWitnesses: [
        {
          verification_key: ownerVerificationKey().toString("hex"),
          signature: Buffer.from(
            key.sign(Buffer.from(unsigned.txId, "hex")).to_raw_bytes(),
          ).toString("hex"),
        },
      ],
    });
  } finally {
    key.free();
  }
};
