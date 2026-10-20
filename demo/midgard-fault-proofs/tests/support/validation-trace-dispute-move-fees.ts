/** What a landed move of the challenger paid for itself, against the ledger
 * minimum of its signed bytes. */
import { CML } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import {
  VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE,
  validationTraceMoveMinimumFee,
} from "../../src/validation-trace-dispute/move-fee.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";

/** The fee move `txHash` paid and the minimum its signed bytes need. */
export const landedMoveFee = async (
  scene: Scene,
  txHash: string,
): Promise<Readonly<{ paid: bigint; minimum: bigint }>> => {
  const cbor = scene.l1.signedCbors.get(txHash);
  if (cbor === undefined) throw new Error(`${txHash} did not land`);
  const body = CML.Transaction.from_cbor_hex(cbor).body();
  const inputs = body.reference_inputs();
  const outRefs = [];
  for (let index = 0; index < (inputs?.len() ?? 0); index += 1) {
    const input = inputs!.get(index);
    outRefs.push({
      txHash: input.transaction_id().to_hex(),
      outputIndex: Number(input.index()),
    });
  }
  return {
    paid: body.fee(),
    minimum: validationTraceMoveMinimumFee({
      lucid: scene.h.proverLucid,
      transactionCborHex: cbor,
      referenced: await scene.h.emulator.getUtxosByOutRef(outRefs),
    }),
  };
};

/**
 * How far above its ledger minimum the transaction builder may price a move:
 * a few dozen bytes of its size estimate, far below the cap.
 */
const BUILDER_MARGIN_LOVELACE = 4_096n;

/** A move paid its real fee: its ledger minimum, within the builder's margin,
 * and below the per-move cap. */
export const expectRealMoveFee = ({
  paid,
  minimum,
}: Readonly<{ paid: bigint; minimum: bigint }>): void => {
  expect(paid).toBeGreaterThanOrEqual(minimum);
  expect(paid - minimum).toBeLessThanOrEqual(BUILDER_MARGIN_LOVELACE);
  expect(paid).toBeLessThan(VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE);
};
