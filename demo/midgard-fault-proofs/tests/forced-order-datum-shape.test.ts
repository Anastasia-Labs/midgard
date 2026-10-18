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
  orderRefundAddress,
  withOrderDatumRewrite,
} from "./support/forced-order-refund.js";
import {
  makeForcedOrderSettlementScenario,
  publishForcedOrderCarriage,
} from "./support/forced-order-settlement.js";
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
const noDatum = Data.to(new Constr(0, []));
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

/** A refund shape the property draws: payment, stake part and refund datum. */
type RefundShape = {
  readonly payment: { readonly script: boolean; readonly length: number };
  readonly stake:
    | null
    | "pointer"
    | { readonly script: boolean; readonly length: number };
  readonly datum: null | "inline" | { readonly hashLength: number };
};

/** Mulberry32: a fixed seed gives the same shapes on every run. */
const seeded = (seed: number) => {
  let state = seed >>> 0;
  return (bound: number) => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) % bound >>> 0;
  };
};

const HASH_LENGTHS = [28, 28, 28, 28, 28, 27, 29, 0, 32] as const;
const draw = (seed: number, count: number): readonly RefundShape[] => {
  const next = seeded(seed);
  const length = () => HASH_LENGTHS[next(HASH_LENGTHS.length)]!;
  return Array.from({ length: count }, () => {
    const stake = next(4);
    const datum = next(4);
    return {
      payment: { script: next(2) === 1, length: length() },
      stake:
        stake === 0
          ? null
          : stake === 1
            ? "pointer"
            : { script: stake === 3, length: length() },
      datum:
        datum === 0
          ? null
          : datum === 1
            ? "inline"
            : { hashLength: datum === 2 ? 32 : 31 + 2 * next(2) },
    };
  });
};

/** The shapes a ledger output can carry, which the mint is to admit. */
const ledgerPayable = (shape: RefundShape) =>
  shape.payment.length === 28 &&
  (shape.stake === null ||
    (shape.stake !== "pointer" && shape.stake.length === 28)) &&
  (shape.datum === null ||
    shape.datum === "inline" ||
    shape.datum.hashLength === 32);

/** The order rewrite and refund for one shape; `index` keeps hashes apart. */
const refundOf = (shape: RefundShape, index: number) => {
  const credential = (
    part: { readonly script: boolean; readonly length: number },
    byte: number,
  ) => (part.script ? script : key)(part.length, byte);
  const stake =
    shape.stake === null
      ? noStake
      : shape.stake === "pointer"
        ? pointer
        : inline(credential(shape.stake, 0x80 + index));
  const refundDatum =
    shape.datum === null
      ? noDatum
      : shape.datum === "inline"
        ? inlineDatum(new Constr(0, [BigInt(index)]))
        : datumHash(hash(shape.datum.hashLength, 0xc0 + index));
  return {
    rewrite: refunding(
      address(credential(shape.payment, 0x40 + index), stake),
      refundDatum,
    ),
  };
};

const SETTLED = [
  ["a 28-byte key with a 28-byte inline key stake part", 0],
  ["a 28-byte key with no stake part", 1],
  ["a 28-byte script with a 28-byte inline script stake part", 2],
  ["a definite-length spelling", 3],
] as const;
const SETTLED_SHAPES: readonly RefundShape[] = [
  {
    payment: { script: false, length: 28 },
    stake: { script: false, length: 28 },
    datum: null,
  },
  { payment: { script: false, length: 28 }, stake: null, datum: null },
  {
    payment: { script: true, length: 28 },
    stake: { script: true, length: 28 },
    datum: { hashLength: 32 },
  },
];
/** Draws 4 shapes the ledger can pay and 10 others, which between them hold a
 * payment hash, a stake hash, a pointer and a datum hash each on its own. */
const PROPERTY_SEED = 0x5eed_000a;
const PROPERTY_SHAPES = draw(PROPERTY_SEED, 14);

describe("refunds the tx-order mint admits settle", () => {
  let s: Awaited<ReturnType<typeof makeForcedOrderSettlementScenario>>;
  const shapes = [...SETTLED_SHAPES, null, ...PROPERTY_SHAPES];

  beforeAll(async () => {
    s = await makeForcedOrderSettlementScenario({
      submitted,
      refunds: shapes.map((shape, index) =>
        shape === null ? { rewrite: definiteLength } : refundOf(shape, index),
      ),
    });
  }, 900_000);

  /** Settle order `index` and read back the refund output it paid. */
  const settleAndReadRefund = async (index: number) => {
    const order = s.minted[index]!;
    await s.settleOrder(index);
    const network = SELECTED_DEPLOYMENT_PROFILE.network;
    const paid = (
      await s.h.proverLucid.utxosAt(orderRefundAddress(network, order))
    ).filter((utxo) => utxo.assets.lovelace === order.assets.lovelace);
    expect(paid).toHaveLength(1);
    const refundDatum = Data.from(
      plutusConstrFieldCbor(order.datum!, [REFUND_DATUM_FIELD]),
    ) as Constr<string>;
    if (refundDatum.index === 0) {
      expect(paid[0]!.datum ?? paid[0]!.datumHash).toBeUndefined();
    } else if (refundDatum.index === 1) {
      expect(paid[0]!.datumHash).toBe(refundDatum.fields[0]);
    } else {
      expect(paid[0]!.datum).toBe(
        plutusConstrFieldCbor(order.datum!, [REFUND_DATUM_FIELD, 0]),
      );
    }
  };

  it.each(SETTLED)(
    "%s mints and settles",
    async (_name, index) => {
      expect(s.refusals[index]).toBeUndefined();
      await settleAndReadRefund(index);
    },
    300_000,
  );

  it("across drawn refund shapes, every order the mint admits settles", async () => {
    const offset = SETTLED_SHAPES.length + 1;
    const admitted = PROPERTY_SHAPES.map((shape) => ledgerPayable(shape));
    // Both polarities are drawn, so neither half of the property is vacuous.
    expect(admitted.filter(Boolean)).toHaveLength(4);
    expect(admitted.filter((payable) => !payable)).toHaveLength(10);
    for (const [draw, payable] of admitted.entries()) {
      const index = offset + draw;
      expect(
        { shape: PROPERTY_SHAPES[draw], minted: s.minted[index] !== undefined },
        `seed ${PROPERTY_SEED.toString(16)} draw ${draw.toString()}`,
      ).toEqual({ shape: PROPERTY_SHAPES[draw], minted: payable });
      if (payable) await settleAndReadRefund(index);
      else expect(s.refusals[index]).toMatch(/failed script execution/u);
    }
  }, 900_000);
});
