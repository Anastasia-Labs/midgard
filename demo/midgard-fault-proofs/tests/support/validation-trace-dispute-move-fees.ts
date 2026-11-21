/** What a landed move of the challenger paid for itself, against the ledger
 * minimum of its signed bytes. */
import { CML, type UTxO, utxoToCore } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
  validationTraceMoveMinimumFee,
} from "../../src/validation-trace-dispute/move-fee.js";
import { VALIDATION_DISPUTE_WALLET_MOVE } from "../../src/workflow/runtime-funding-fee.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

/** A landed move's fee: what it paid, the minimum the builder priced it at,
 * and the ledger's own minimum of its signed bytes. */
export type LandedMoveFee = Readonly<{
  paid: bigint;
  minimum: bigint;
  ledger: bigint;
}>;

const outRefsOf = (inputs: CML.TransactionInputList | undefined) => {
  const outRefs = [];
  for (let index = 0; index < (inputs?.len() ?? 0); index += 1) {
    const input = inputs!.get(index);
    outRefs.push({
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    });
  }
  return outRefs;
};

/** A protocol price, written as a decimal, as the exact rational it names. */
const subCoin = (value: number): CML.SubCoin => {
  const [digits = "", exponent = "0"] = value.toString().split("e");
  const [whole = "", fraction = ""] = digits.split(".");
  const places = fraction.length - Number(exponent);
  const numerator = BigInt(whole + fraction);
  return places >= 0
    ? CML.SubCoin.new(numerator, 10n ** BigInt(places))
    : CML.SubCoin.new(numerator * 10n ** BigInt(-places), 1n);
};

/** The bytes the ledger prices of each reference script `utxos` carry. */
const referenceScriptSize = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((sum, utxo) => {
    const script = utxoToCore(utxo).output().script_ref();
    if (script === undefined) return sum;
    const native = script.as_native();
    const bytes =
      native?.to_cbor_bytes() ??
      script.as_plutus_v3()?.to_raw_bytes() ??
      script.as_plutus_v2()?.to_raw_bytes() ??
      script.as_plutus_v1()!.to_raw_bytes();
    return sum + BigInt(bytes.length);
  }, 0n);

/**
 * The fee move `txHash` paid, the minimum the move builder priced its signed
 * bytes at, and the ledger's minimum fee of those landed bytes: the
 * multiplatform library's own fee rule at the emulator's protocol
 * parameters, over the reference scripts of every input and reference input
 * it reads.
 */
export const landedMoveFee = async (
  scene: Scene,
  txHash: string,
): Promise<LandedMoveFee> => {
  const cbor = scene.l1.signedCbors.get(txHash);
  if (cbor === undefined) throw new Error(`${txHash} did not land`);
  const transaction = CML.Transaction.from_cbor_hex(cbor);
  const body = transaction.body();
  const referenced = await scene.h.emulator.getUtxosByOutRef(
    outRefsOf(body.reference_inputs()),
  );
  const spent = await scene.h.emulator.getUtxosByOutRef(
    outRefsOf(body.inputs()),
  );
  const parameters = await scene.h.emulator.getProtocolParameters();
  return {
    paid: body.fee(),
    minimum: validationTraceMoveMinimumFee({
      lucid: scene.h.proverLucid,
      transactionCborHex: cbor,
      referenced,
    }),
    ledger: CML.min_fee(
      transaction,
      CML.LinearFee.new(
        BigInt(parameters.minFeeA),
        BigInt(parameters.minFeeB),
        BigInt(parameters.minFeeRefScriptCostPerByte),
      ),
      CML.ExUnitPrices.new(
        subCoin(parameters.priceMem),
        subCoin(parameters.priceStep),
      ),
      referenceScriptSize([...spent, ...referenced]),
    ),
  };
};

/** The measured shape of a landed move: its signed bytes and the
 * reference-script bytes of every input and reference input it reads. */
export const landedMoveShape = async (scene: Scene, txHash: string) => {
  const cbor = scene.l1.signedCbors.get(txHash);
  if (cbor === undefined) throw new Error(`${txHash} did not land`);
  const body = CML.Transaction.from_cbor_hex(cbor).body();
  const read = await scene.h.emulator.getUtxosByOutRef([
    ...outRefsOf(body.reference_inputs()),
    ...outRefsOf(body.inputs()),
  ]);
  return {
    transactionBytes: BigInt(cbor.length / 2),
    referenceScriptBytes: referenceScriptSize(read),
  };
};

/** Each landed move of `hashes` reads no more reference-script bytes than
 * the move the wallet-move reserve is priced at; the reserve budgets the
 * signed size and execution at the protocol's limits. */
export const expectWithinWalletMoveShape = async (
  scene: Scene,
  hashes: readonly string[],
): Promise<void> => {
  for (const hash of hashes)
    expect(
      (await landedMoveShape(scene, hash)).referenceScriptBytes,
      `${hash} referenceScriptBytes`,
    ).toBeLessThanOrEqual(VALIDATION_DISPUTE_WALLET_MOVE.referenceScriptBytes);
};

/**
 * How far above its ledger minimum the transaction builder may price a move:
 * a few dozen bytes of its size estimate, far below the cap.
 */
const BUILDER_MARGIN_LOVELACE = 4_096n;

/** A move paid its real fee: at least the ledger's minimum of its landed
 * bytes, which its builder's minimum never undercounts, its builder's
 * minimum within the builder's margin, and below the per-move cap. */
export const expectRealMoveFee = ({
  paid,
  minimum,
  ledger,
}: LandedMoveFee): void => {
  expect(paid).toBeGreaterThanOrEqual(ledger);
  expect(minimum).toBeGreaterThanOrEqual(ledger);
  expect(paid).toBeGreaterThanOrEqual(minimum);
  expect(paid - minimum).toBeLessThanOrEqual(BUILDER_MARGIN_LOVELACE);
  expect(paid).toBeLessThan(VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE);
};
