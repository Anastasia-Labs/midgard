import {
  CML,
  Data,
  Emulator,
  generateEmulatorAccount,
  getAddressDetails,
  Lucid,
  type LucidEvolution,
  scriptFromNative,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { buildDaAvailabilityFundingPreparationTx } from "../src/availability-challenge-operation.build-da-availability-funding-preparation-tx.js";
import { DaAvailabilityTransactionError } from "../src/availability-challenge-transactions.at.js";

const OUTPUT_LOVELACE = 30_000_000n;
const FEE_LOVELACE = 1_000_000n;
// Larger than the ordinary coin, so largest-first selection reaches it first.
const HELD_LOVELACE = 150_000_000n;

type HeldShape = "an inline datum" | "a datum hash" | "a reference script";

const refOf = (u: Pick<UTxO, "txHash" | "outputIndex">) =>
  `${u.txHash}#${u.outputIndex}`;

/** Pays `to` one coin of `HELD_LOVELACE` carrying `shape`, from `lucid`. */
const payHeld = async (
  lucid: LucidEvolution,
  to: string,
  signer: string,
  shape: HeldShape,
  count = 1,
): Promise<void> => {
  const script = scriptFromNative({
    type: "sig",
    keyHash: getAddressDetails(signer).paymentCredential!.hash,
  });
  let tx = lucid.newTx();
  for (let i = 0; i < count; i++)
    tx =
      shape === "a reference script"
        ? tx.pay.ToAddressWithData(
            to,
            undefined,
            { lovelace: HELD_LOVELACE },
            script,
          )
        : tx.pay.ToAddressWithData(
            to,
            {
              kind: shape === "an inline datum" ? "inline" : "asHash",
              value: Data.to(BigInt(i)),
            },
            { lovelace: HELD_LOVELACE },
          );
  const signed = await (await tx.complete()).sign.withWallet().complete();
  await lucid.awaitTx(await signed.submit());
};

const carries = (u: UTxO): boolean =>
  u.datum != null || u.datumHash != null || u.scriptRef != null;

const prepare = (
  lucid: LucidEvolution,
  emulator: Emulator,
  walletInputs?: UTxO[],
) =>
  buildDaAvailabilityFundingPreparationTx(lucid, {
    outputLovelace: OUTPUT_LOVELACE,
    feeLovelace: FEE_LOVELACE,
    ...(walletInputs === undefined ? {} : { walletInputs }),
    validFrom: BigInt(emulator.now()),
    validTo: BigInt(emulator.now() + 60_000),
  });

const SHAPES: readonly HeldShape[] = [
  "an inline datum",
  "a datum hash",
  "a reference script",
];

describe("availability funding preparation wallet coins", () => {
  it.each(
    SHAPES.flatMap((shape) =>
      (["the wallet's coins", "explicit wallet inputs"] as const).map(
        (source) => ({ shape, source }),
      ),
    ),
  )(
    "spends only the coin without a datum or reference script beside a coin carrying $shape, from $source",
    async ({ shape, source }) => {
      const account = generateEmulatorAccount({ lovelace: 200_000_000n });
      const emulator = new Emulator([account]);
      const lucid = await Lucid(emulator, "Custom");
      lucid.selectWallet.fromSeed(account.seedPhrase);
      await payHeld(lucid, account.address, account.address, shape);
      const coins = await lucid.wallet().getUtxos();
      const held = coins.filter(carries);
      const ordinary = coins.filter((u) => !carries(u));
      expect(held).toHaveLength(1);
      expect(ordinary).toHaveLength(1);
      expect(held[0]!.assets.lovelace).toBeGreaterThan(
        ordinary[0]!.assets.lovelace!,
      );

      const signed = await (
        await prepare(
          lucid,
          emulator,
          source === "explicit wallet inputs" ? coins : undefined,
        )
      ).sign
        .withWallet()
        .complete();
      const body = CML.Transaction.from_cbor_hex(signed.toCBOR()).body();
      const inputs = Array.from({ length: body.inputs().len() }, (_, i) => {
        const input = body.inputs().get(i);
        return `${input.transaction_id().to_hex()}#${input.index()}`;
      });
      expect(inputs).toEqual([refOf(ordinary[0]!)]);
      const txHash = await signed.submit();
      await lucid.awaitTx(txHash);

      expect(await lucid.utxosByOutRef([held[0]!])).toEqual([held[0]]);
      expect(await lucid.utxosByOutRef([ordinary[0]!])).toEqual([]);
      const [opening] = await lucid.utxosByOutRef([{ txHash, outputIndex: 0 }]);
      expect(opening!.assets).toEqual({ lovelace: OUTPUT_LOVELACE });
    },
  );

  it.each(SHAPES)(
    "refuses a wallet whose every coin carries %s, spending none of them",
    async (shape) => {
      const funder = generateEmulatorAccount({ lovelace: 1_000_000_000n });
      const holder = generateEmulatorAccount({ lovelace: 0n });
      const emulator = new Emulator([funder]);
      const lucid = await Lucid(emulator, "Custom");
      lucid.selectWallet.fromSeed(funder.seedPhrase);
      await payHeld(lucid, holder.address, funder.address, shape, 2);
      lucid.selectWallet.fromSeed(holder.seedPhrase);
      const coins = await lucid.wallet().getUtxos();
      expect(coins).toHaveLength(2);
      expect(coins.every(carries)).toBe(true);

      for (const walletInputs of [undefined, coins]) {
        const error = await prepare(lucid, emulator, walletInputs).then(
          () => undefined,
          (cause: unknown) => cause,
        );
        expect(error).toBeInstanceOf(DaAvailabilityTransactionError);
        expect((error as DaAvailabilityTransactionError).reason).toBe(
          "preparation-no-wallet-inputs",
        );
      }
      expect(await lucid.utxosByOutRef(coins)).toEqual(coins);
    },
  );
});
