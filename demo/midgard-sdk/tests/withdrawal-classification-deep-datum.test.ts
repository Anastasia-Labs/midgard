/**
 * Withdrawal classification decodes an event whose L1 datum is deeply
 * nested, or nested to the admission bound, and still refuses event info of
 * the wrong shape.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import {
  ADMISSION_PAYLOAD_NODES,
  DEEP_PLUTUS_DATA_CASES,
  deepPlutusDataCbor,
  depthAtAdmissionBound,
  plutusDataNodeCount,
  replaceCborItemOnce,
} from "@al-ft/midgard-test-support/deep-plutus-data";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { addressDataFromBech32, Value } from "../src/common.js";
import { WithdrawalBody, WithdrawalInfo } from "../src/ledger-state.js";
import { EventHistoryPayload } from "../src/user-events/history.js";
import { signWithdrawalBodyCbor } from "../src/withdrawal-signature.js";
import {
  classifyWithdrawalFromLedger,
  WithdrawalValidationError,
} from "../src/withdrawal-validation.js";

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
const owner = key.to_public().hash().to_hex();
// Enough for the min-ADA of a payout carrying the largest datum here.
const LOVELACE = 500_000_000n;

/**
 * A withdrawal whose inline L1 datum is `datumCbor`, signed by its owner
 * unless `signed` is false (a placeholder pair then).
 */
const withdrawal = async (datumCbor: string, signed = true) => {
  const value: Value = new Map([["", new Map([["", LOVELACE]])]]);
  const body: WithdrawalBody = {
    l2_outref: { transactionId: "ab".repeat(32), outputIndex: 0n },
    l2_owner: owner,
    l2_value: value,
    l1_address: await Effect.runPromise(addressDataFromBech32(wallet.address)),
    l1_datum: { InlineDatum: { data: 0n } },
  };
  const shallowBody = Data.to(body, WithdrawalBody);
  const bodyCbor = replaceCborItemOnce(
    shallowBody,
    plutusConstrFieldCbor(shallowBody, [4]),
    `d87b81${datumCbor}`,
  );
  const signature: [string, string] = signed
    ? signWithdrawalBodyCbor(key, bodyCbor)
    : ["44".repeat(32), "55".repeat(64)];
  const info = { body, signature, validity: "WithdrawalIsValid" } as const;
  const eventInfoCbor = replaceCborItemOnce(
    Data.to(info, WithdrawalInfo),
    shallowBody,
    bodyCbor,
  );
  // The event's history payload, for its node count.
  const payloadCbor = replaceCborItemOnce(
    Data.to(
      {
        WithdrawalPayload: {
          event: { id: body.l2_outref, info },
          refund_address: body.l1_address,
          refund_datum: "NoDatum",
        },
      },
      EventHistoryPayload,
    ),
    shallowBody,
    bodyCbor,
  );
  return {
    payloadCbor,
    l2Owner: owner,
    l2ValueCbor: Data.to(value, Value),
    eventInfoCbor,
    ledgerOutRef: encodeMidgardSpendInputItem({
      txId: Buffer.from(body.l2_outref.transactionId, "hex"),
      outputIndex: 0,
    }),
    ledgerOutput: encodeMidgardTxOutput({
      address: midgardAddressFromText(wallet.address),
      value: { lovelace: LOVELACE, assets: new Map() },
    }),
  };
};

const classify = (input: Parameters<typeof classifyWithdrawalFromLedger>[0]) =>
  Effect.runPromise(Effect.either(classifyWithdrawalFromLedger(input)));

/** A `shape` datum that makes its withdrawal's payload exactly the bound. */
const datumAtAdmissionBound = async (shape: "list" | "constr") => {
  const { payloadCbor } = await withdrawal("00", false);
  const datum = deepPlutusDataCbor({
    shape,
    depth: depthAtAdmissionBound(payloadCbor),
  });
  const deep = await withdrawal(datum, false);
  expect(plutusDataNodeCount(deep.payloadCbor)).toBe(ADMISSION_PAYLOAD_NODES);
  return datum;
};

/** The settlement's inline datum, as Aiken serialises it. */
const settledDatum = (settlement: Buffer) =>
  aikenSerialisedPlutusDataCborPreservingMapOrder(
    plutusConstrFieldCbor(settlement.toString("hex"), [0, 4, 0]),
  );

// Field 1 (the signature pair) as an integer: not a WithdrawalInfo.
const misshapen = (eventInfoCbor: string) =>
  replaceCborItemOnce(
    eventInfoCbor,
    plutusConstrFieldCbor(eventInfoCbor, [1]),
    "00",
  );

// Building, signing and classifying a withdrawal around a 30,000-deep datum
// takes about 2 s on a developer machine and over 5 s on a loaded CI runner.
const DEEP_ROW_TIMEOUT_MS = 60_000;

describe("withdrawal classification of deeply nested datums", () => {
  it.each(DEEP_PLUTUS_DATA_CASES)(
    "classifies a signed payable withdrawal with a $shape datum $depth deep ($bound) as valid",
    { timeout: DEEP_ROW_TIMEOUT_MS },
    async (deep) => {
      const datum = deepPlutusDataCbor(deep);
      const result = await classify(await withdrawal(datum));
      if (result._tag === "Left") throw result.left;
      expect(result.right.validity).toBe("WithdrawalIsValid");
      expect(result.right.shouldDeleteLedgerUtxo).toBe(true);
      expect(settledDatum(result.right.settlementEventInfo)).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(datum),
      );
    },
  );

  it.each(DEEP_PLUTUS_DATA_CASES)(
    "classifies a withdrawal with a $shape datum $depth deep ($bound) and no ledger output as non-existent",
    { timeout: DEEP_ROW_TIMEOUT_MS },
    async (deep) => {
      const datum = deepPlutusDataCbor(deep);
      const input = await withdrawal(datum, false);
      const result = await classify({ ...input, ledgerOutput: null });
      if (result._tag === "Left") throw result.left;
      expect(result.right.validity).toBe("NonExistentWithdrawalUtxo");
      expect(settledDatum(result.right.settlementEventInfo)).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(datum),
      );
    },
  );

  // Guard rows: these hold with a recursive decoder as well.
  it.each(["list", "constr"] as const)(
    "classifies a withdrawal whose %s datum is nested to the admission bound",
    async (shape) => {
      const datum = await datumAtAdmissionBound(shape);
      const result = await classify({
        ...(await withdrawal(datum, false)),
        ledgerOutput: null,
      });
      if (result._tag === "Left") throw result.left;
      expect(result.right.validity).toBe("NonExistentWithdrawalUtxo");
      expect(settledDatum(result.right.settlementEventInfo)).toBe(
        aikenSerialisedPlutusDataCborPreservingMapOrder(datum),
      );
    },
  );

  it.each([
    { name: "a shallow datum", datum: "00" },
    {
      name: "a list datum 15000 deep",
      datum: deepPlutusDataCbor({ shape: "list", depth: 15_000 }),
    },
  ])("refuses event info of the wrong shape with $name", async ({ datum }) => {
    const input = await withdrawal(datum, false);
    const result = await classify({
      ...input,
      ledgerOutput: null,
      eventInfoCbor: misshapen(input.eventInfoCbor),
    });
    if (result._tag === "Right")
      throw new Error(`classified ${result.right.validity}`);
    expect(result.left).toBeInstanceOf(WithdrawalValidationError);
    expect(result.left.message).toBe("Failed to decode withdrawal event info");
  });
});
