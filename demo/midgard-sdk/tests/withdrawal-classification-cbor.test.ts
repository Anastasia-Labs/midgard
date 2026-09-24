import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { addressDataFromBech32, Value } from "../src/common.js";
import {
  WithdrawalBody,
  WithdrawalInfo,
  WithdrawalValidity,
} from "../src/ledger-state.js";
import {
  signWithdrawalBodyCbor,
  verifyWithdrawalSignature,
} from "../src/withdrawal-signature.js";
import { classifyWithdrawalFromLedger } from "../src/withdrawal-validation.js";

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
const owner = key.to_public().hash().to_hex();

const fixture = async () => {
  const value: Value = new Map([["", new Map([["", 7_000_000n]])]]);
  const body: WithdrawalBody = {
    l2_outref: { transactionId: "ab".repeat(32), outputIndex: 0n },
    l2_owner: owner,
    l2_value: value,
    l1_address: await Effect.runPromise(addressDataFromBech32(wallet.address)),
    l1_datum: { InlineDatum: { data: 0n } },
  };
  const bodyCbor = replacePlutusConstrFieldCbor(
    Data.to(body, WithdrawalBody),
    [4, 0],
    "a302c2410101d86682008002c2420001",
  );
  const signature = signWithdrawalBodyCbor(key, bodyCbor);
  const infoCbor = replacePlutusConstrFieldCbor(
    Data.to(
      { body, signature, validity: "IncorrectWithdrawalOwner" },
      WithdrawalInfo,
    ),
    [0],
    bodyCbor,
  );
  const ledgerOutput = encodeMidgardTxOutput({
    address: midgardAddressFromText(wallet.address),
    value: { lovelace: 7_000_000n, assets: new Map() },
  });
  return {
    signature,
    bodyCbor,
    input: {
      l2Owner: owner,
      l2ValueCbor: Data.to(value, Value),
      eventInfoCbor: infoCbor,
      ledgerOutRef: encodeMidgardSpendInputItem({
        txId: Buffer.from(body.l2_outref.transactionId, "hex"),
        outputIndex: 0,
      }),
      ledgerOutput,
    },
  };
};

describe("raw withdrawal classification", () => {
  it.each([false, true])(
    "preserves signed raw content when output is missing: %s",
    async (missing) => {
      const { input, bodyCbor, signature } = await fixture();
      // A decoded JS Map drops a pair and therefore cannot reproduce the signature.
      expect(
        verifyWithdrawalSignature(
          Data.from(bodyCbor, WithdrawalBody),
          signature,
          owner,
        ).valid,
      ).toBe(false);
      const result = await Effect.runPromise(
        classifyWithdrawalFromLedger({
          ...input,
          ledgerOutput: missing ? null : input.ledgerOutput,
        }),
      );
      const validity = missing
        ? "NonExistentWithdrawalUtxo"
        : "WithdrawalIsValid";
      expect(result.validity).toBe(validity);
      expect(result.shouldDeleteLedgerUtxo).toBe(!missing);
      const settlement = result.settlementEventInfo.toString("hex");
      expect(plutusConstrFieldCbor(settlement, [0])).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(bodyCbor),
      );
      expect(plutusConstrFieldCbor(settlement, [0, 4, 0])).toBe(
        "a3020101d879800201",
      );
      expect(plutusConstrFieldCbor(settlement, [1])).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(
          plutusConstrFieldCbor(input.eventInfoCbor, [1]),
        ),
      );
      expect(
        Data.from(plutusConstrFieldCbor(settlement, [2]), WithdrawalValidity),
      ).toBe(validity);
    },
  );

  it("refuses a changed raw map while retaining the attempted event content", async () => {
    const { input } = await fixture();
    const mutated = replacePlutusConstrFieldCbor(
      input.eventInfoCbor,
      [0, 4, 0],
      "a301d8798002010201",
    );
    const result = await Effect.runPromise(
      classifyWithdrawalFromLedger({ ...input, eventInfoCbor: mutated }),
    );
    expect(result.validity).toBe("IncorrectWithdrawalSignature");
    expect(result.shouldDeleteLedgerUtxo).toBe(false);
    expect(
      plutusConstrFieldCbor(
        result.settlementEventInfo.toString("hex"),
        [0, 4, 0],
      ),
    ).toBe("a301d8798002010201");
  });
});
