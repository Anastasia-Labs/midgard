/**
 * Signed-intent recovery over the emulator, as the watcher's follower gives
 * it to the fault-proof families: where one exact signed transaction stands
 * at the emulator's tip.
 *
 * - included: the emulator confirmed it;
 * - pending: it waits in the mempool;
 * - expired_at_tip: its validity ended before the tip's slot;
 * - invalidated_at_tip: one of its inputs is no longer unspent;
 * - rebroadcast: none of these, so it can still land and is resubmitted.
 *
 * The emulator has no rollbacks, so the release-final point is the tip, or
 * `releaseDepth` blocks below it when a test models a release horizon.
 */
import { CML, type Emulator, utxoToCore } from "@lucid-evolution/lucid";

import type { FraudProofSignedTransactionRecovery } from "../../src/workflow/l1-source.js";
import { inspectSignedWorkflowTransaction } from "../../src/workflow/signed-transaction-reconciliation.js";
import { syntheticEmulatorPoint } from "./cross-block-raw-emulator.js";

export const emulatorSignedTransactionRecovery = (
  emulator: Emulator,
  releaseDepth = 0,
): FraudProofSignedTransactionRecovery => ({
  observeSignedTransaction: async (input) => {
    const { body, inputOutRefs } = inspectSignedWorkflowTransaction(input);
    const tip = syntheticEmulatorPoint(emulator.blockHeight, emulator.slot);
    const releaseFinal =
      releaseDepth === 0
        ? tip
        : syntheticEmulatorPoint(
            Math.max(0, emulator.blockHeight - releaseDepth),
            Math.max(0, emulator.slot - releaseDepth * 20),
          );
    const inputs = inputOutRefs.flatMap((outRef) => {
      const [hash, index] = outRef.split("#");
      const key = `${hash!}${index!}`;
      // A mempool spender only reserves a ledger entry; it is still unspent.
      const entry = emulator.ledger[key] ?? emulator.mempool[key];
      return entry === undefined
        ? []
        : [
            {
              outRef,
              outputCbor: utxoToCore(entry.utxo).output().to_cbor_hex(),
            },
          ];
    });
    const observed = (
      status: Exclude<
        Awaited<
          ReturnType<
            FraudProofSignedTransactionRecovery["observeSignedTransaction"]
          >
        >["status"],
        "included"
      >,
      reason: string,
    ) => ({
      ...input,
      status,
      canonicalPoint: tip,
      releaseFinalPoint: releaseFinal,
      inputs,
      reason,
    });
    const status = await emulator.getTransactionStatus(input.transactionHash);
    if (status.status === "confirmed") {
      const { blockHeight, slot } = status.confirmation;
      if (blockHeight === undefined || slot === undefined)
        throw new Error("emulator confirmation lacks its inclusion point");
      return {
        ...observed("pending", "included"),
        status: "included",
        inclusionPoint: syntheticEmulatorPoint(blockHeight, slot),
      };
    }
    if (status.status === "pending")
      return observed("pending", "waiting in the mempool");
    const ttl = body.ttl();
    if (ttl !== undefined && ttl < BigInt(emulator.slot))
      return observed("expired_at_tip", `validity ended at slot ${ttl}`);
    if (inputs.length !== inputOutRefs.length)
      return observed("invalidated_at_tip", "an input is no longer unspent");
    return observed("rebroadcast", "absent, and can still land");
  },
  rebroadcastSignedTransaction: async ({
    authorizeResubmission,
    ...signed
  }) => {
    inspectSignedWorkflowTransaction(signed);
    await authorizeResubmission(signed);
    const submitted = await emulator.submitTx(signed.signedTransactionCborHex);
    if (
      submitted !==
      CML.hash_transaction(
        CML.Transaction.from_cbor_hex(signed.signedTransactionCborHex).body(),
      ).to_hex()
    )
      throw new Error("the emulator changed the rebroadcast transaction");
    return submitted;
  },
});
