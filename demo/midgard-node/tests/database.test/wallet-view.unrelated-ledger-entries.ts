import { createHash } from "node:crypto";

import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";

import { MempoolLedgerDB } from "../../src/database/index.js";
import { ledgerEntry2 } from "./fixtures.js";

/** Real address2 output rows make the address1 page meaningfully selective. */
export const populateUnrelatedWalletViewEntries = (count: number) =>
  MempoolLedgerDB.insert(
    Array.from({ length: count }, (_, index) => {
      const txId = createHash("sha256")
        .update(`unrelated-wallet-${index.toString()}`)
        .digest();
      return {
        ...ledgerEntry2,
        tx_id: txId,
        outref: encodeMidgardSpendInputItem({ txId, outputIndex: 0 }),
      };
    }),
  );
