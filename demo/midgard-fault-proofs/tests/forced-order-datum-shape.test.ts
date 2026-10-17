import {
  encodeMidgardForcedTxCanonical,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core";
import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  canonicalPlutusDataCbor,
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import {
  Constr,
  Data,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it, vi } from "vitest";

import {
  type OrderDatumRewrite,
  withOrderDatumRewrite,
} from "./support/forced-order-refund.js";
import { publishForcedOrderCarriage } from "./support/forced-order-settlement.js";
import {
  alignUnixTimeToEmulatorSlotBoundary,
  expectOnchainRefusal,
  makeFaultProofEmulatorHarness,
  makeHeader,
  makeNativeTx,
  publishPlainReferenceScriptUtxo,
  submitSetupTx,
  withPlaceholderDeposit,
} from "./support/submit-init-emulator-shared.js";

/** An order the SDK admits: one spend input, every other field empty. */
const submitted = materializeMidgardForcedTxFromCanonical(
  makeNativeTx({
    spendInputCbors: [
      encodeMidgardSpendInputItem({
        txId: Buffer.alloc(32, 0x31),
        outputIndex: 0,
      }),
    ],
    fee: 0n,
  }),
);
const submittedBytes = encodeMidgardForcedTxCanonical(submitted);

const REFUND_ADDRESS_FIELD = 3;
const REFUND_DATUM_FIELD = 4;

const hash = (length: number, byte: number) =>
  Buffer.alloc(length, byte).toString("hex");
const key = (length: number, byte: number) =>
  new Constr(0, [hash(length, byte)]);
const script = (length: number, byte: number) =>
  new Constr(1, [hash(length, byte)]);
const inline = (credential: Data) =>
  new Constr(0, [new Constr(0, [credential])]);
const pointer = new Constr(0, [new Constr(1, [1n, 2n, 3n])]);
const noStake = new Constr(1, []);
const address = (payment: Data, stake: Data) =>
  Data.to(new Constr(0, [payment, stake]));
const datumHash = (hex: string) => Data.to(new Constr(1, [hex]));
const inlineDatum = (data: Data) => Data.to(new Constr(2, [data]));

/** Replace the order datum's refund address and, when given, its datum. */
const refunding =
  (addressCbor: string, refundDatumCbor?: string): OrderDatumRewrite =>
  (datumCbor) => {
    const withAddress = replacePlutusConstrFieldCbor(
      datumCbor,
      [REFUND_ADDRESS_FIELD],
      addressCbor,
    );
    return refundDatumCbor === undefined
      ? withAddress
      : replacePlutusConstrFieldCbor(
          withAddress,
          [REFUND_DATUM_FIELD],
          refundDatumCbor,
        );
  };

/** The SDK writes the datum with indefinite-length lists. */
const lastField = (datumCbor: string) =>
  plutusConstrFieldCbor(datumCbor, [REFUND_DATUM_FIELD]);
const outerConstr1: OrderDatumRewrite = (datumCbor) => {
  expect(datumCbor.startsWith("d8799f")).toBe(true);
  return `d87a9f${datumCbor.slice(6)}`;
};
const extraField: OrderDatumRewrite = (datumCbor) => {
  expect(datumCbor.endsWith(`${lastField(datumCbor)}ff`)).toBe(true);
  return `${datumCbor.slice(0, -2)}00ff`;
};
const missingField: OrderDatumRewrite = (datumCbor) => {
  const last = lastField(datumCbor);
  expect(datumCbor.endsWith(`${last}ff`)).toBe(true);
  return `${datumCbor.slice(0, -(last.length + 2))}ff`;
};

/** The same datum with every list definite-length, as other encoders write. */
const definiteLength: OrderDatumRewrite = (datumCbor) => {
  const definite = canonicalPlutusDataCbor(datumCbor);
  expect(definite).not.toBe(datumCbor);
  return definite;
};

const escape = (text: string) => text.replace(/[.*+?^${}()|[\]\\]/gu, "\\$&");
const at = (check: string) => new RegExp(escape(check), "u");

const REFUSED: readonly (readonly [string, OrderDatumRewrite, string])[] = [
  ["an outer constructor 1", outerConstr1, "expect order: Datum = order_data"],
  ["an extra field", extraField, "expect order: Datum = order_data"],
  ["a missing field", missingField, "expect order: Datum = order_data"],
  [
    "a 27-byte payment key hash",
    refunding(address(key(27, 0x11), noStake)),
    "expect credential_hash_is_28_bytes(payment_credential)",
  ],
  [
    "a 29-byte payment script hash",
    refunding(address(script(29, 0x12), noStake)),
    "expect credential_hash_is_28_bytes(payment_credential)",
  ],
  [
    "a 27-byte stake key hash",
    refunding(address(key(28, 0x13), inline(key(27, 0x14)))),
    "expect credential_hash_is_28_bytes(stake_credential)",
  ],
  [
    "a 29-byte stake script hash",
    refunding(address(key(28, 0x15), inline(script(29, 0x16)))),
    "expect credential_hash_is_28_bytes(stake_credential)",
  ],
  [
    "a pointer stake part",
    refunding(address(key(28, 0x17), pointer)),
    "expect Inline(stake_credential) = stake_part",
  ],
  [
    "a 31-byte refund datum hash",
    refunding(address(key(28, 0x18), noStake), datumHash(hash(31, 0x19))),
    "expect refund_datum_hash_is_32_bytes(refund_datum)",
  ],
];

const ADMITTED: readonly (readonly [string, OrderDatumRewrite | undefined])[] =
  [
    ["the SDK's own order", undefined],
    ["a definite-length spelling", definiteLength],
    [
      "a 28-byte key with a 28-byte inline key stake part",
      refunding(address(key(28, 0x21), inline(key(28, 0x22)))),
    ],
    [
      "a 28-byte script with no stake part",
      refunding(address(script(28, 0x23), noStake)),
    ],
    [
      "a 32-byte refund datum hash",
      refunding(address(key(28, 0x24), noStake), datumHash(hash(32, 0x25))),
    ],
    [
      "an inline refund datum",
      refunding(
        address(key(28, 0x26), noStake),
        inlineDatum(new Constr(0, [1n])),
      ),
    ],
  ];

describe("order datum shape at the real tx-order mint", () => {
  let h: Awaited<ReturnType<typeof makeFaultProofEmulatorHarness>>;
  let lucid: LucidEvolution;
  let txOrderMinting: UTxO;

  beforeAll(async () => {
    h = await makeFaultProofEmulatorHarness({
      contractOptions: { realTxOrder: true },
    });
    lucid = await Lucid(h.emulator, SELECTED_DEPLOYMENT_PROFILE.network);
    h.proverSigner.selectWallet(lucid);
    const operator = getAddressDetails(
      await h.funderLucid.wallet().address(),
    ).paymentCredential!;
    await submitSetupTx({
      lucid: h.funderLucid,
      contracts: h.contracts,
      nonceUtxo: h.nonceUtxo,
      catalogue: h.catalogue,
      header: withPlaceholderDeposit(
        makeHeader(
          operator.hash,
          alignUnixTimeToEmulatorSlotBoundary(
            h.funderLucid,
            h.emulator.now() + 120_000,
          ) - 1,
        ),
      ),
    });
    // A reference script, as in production: the traced mint is too large to
    // attach inline.
    txOrderMinting = (
      await publishPlainReferenceScriptUtxo({
        lucid: h.funderLucid,
        script: h.contracts.txOrder.mintingScript,
        label: "order mint",
      })
    ).utxo;
  }, 300_000);

  /** Build, sign and submit one order; the build evaluates the mint. */
  const mintOrder = async (rewrite?: OrderDatumRewrite) => {
    const carriage = await publishForcedOrderCarriage(h, lucid, submittedBytes);
    const [nonce] = (await lucid.wallet().getUtxos())
      .filter(
        (utxo) => utxo.datum === undefined && utxo.datumHash === undefined,
      )
      .sort((left, right) =>
        Number(right.assets.lovelace - left.assets.lovelace),
      );
    if (nonce === undefined) throw new Error("no nonce UTxO in the wallet");
    const refundAddress = await Effect.runPromise(
      SDK.addressDataFromBech32(await lucid.wallet().address()),
    );
    const clock = vi
      .spyOn(Date, "now")
      .mockImplementation(() => h.emulator.now());
    const built = await Effect.runPromise(
      SDK.buildUnsignedTxOrderTxWithMetadataProgram(
        rewrite === undefined
          ? lucid
          : withOrderDatumRewrite(
              lucid,
              h.contracts.txOrder.spendingScriptAddress,
              rewrite,
            ),
        h.contracts,
        {
          nonceInput: nonce,
          submittedTxCbor: submittedBytes.toString("hex"),
          carriageReferenceInputs: carriage.referenceInputs,
          fieldPreimageCertificatePolicyId:
            h.contracts.fieldPreimageCertificate.policyId,
          refundAddress,
          lovelace: 3_000_000n,
          referenceScripts: { txOrderMinting },
        },
      ),
    ).finally(() => clock.mockRestore());
    const signed = await built.tx.sign.withWallet().complete();
    await lucid.awaitTx(await signed.submit());
  };

  it.each(ADMITTED)(
    "%s mints",
    async (_name, rewrite) => {
      const before = (
        await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)
      ).length;
      await mintOrder(rewrite);
      expect(
        (await lucid.utxosAt(h.contracts.txOrder.spendingScriptAddress)).length,
      ).toBe(before + 1);
    },
    300_000,
  );

  it.each(REFUSED)(
    "an order datum with %s is refused at the mint's datum check",
    async (_name, rewrite, check) => {
      await expectOnchainRefusal(() => mintOrder(rewrite), {
        refusedBy: "user_events/tx_order_v1",
        check: at(check),
      });
    },
    300_000,
  );
});
