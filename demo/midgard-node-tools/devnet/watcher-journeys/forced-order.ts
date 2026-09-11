import { existsSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyCategory } from "./fixture.js";
import {
  createPreparedJourneyFixture,
  type JourneyFaultBuildInput,
  type JourneyFaultPreparationInput,
  type JourneyPreparedFault,
} from "./staging.js";

type ForcedOrderCheckpoint = {
  deploymentFingerprint: string;
  nativeTxCbor: string;
  signedCbor: string;
  txHash: string;
  metadata: SDK.TxOrderBuildMetadata;
  confirmed: boolean;
};

/** Stage the actual order consumed by a forced-verdict fixture and its control. */
const publishJourneyForcedOrder = async (
  input: JourneyFaultPreparationInput,
  nativeTxCbor: Buffer,
): Promise<SDK.OutputReference> => {
  const { context, directory, onStage } = input;
  const { deployment, provider } = context;
  const { operatorLucid: lucid, contracts } = deployment;
  const checkpointPath = join(directory, "forced-order.json");
  let checkpoint: ForcedOrderCheckpoint;
  if (existsSync(checkpointPath)) {
    checkpoint =
      await readJourneyArtifact<ForcedOrderCheckpoint>(checkpointPath);
    if (
      checkpoint.deploymentFingerprint !== deployment.manifest.manifestId ||
      checkpoint.nativeTxCbor !== nativeTxCbor.toString("hex")
    )
      throw new Error(
        "Forced order checkpoint changed deployment or exact native transaction bytes",
      );
  } else {
    lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
    const nonceInput = (await lucid.wallet().getUtxos()).find(
      (utxo) =>
        utxo.datum == null &&
        utxo.datumHash == null &&
        utxo.scriptRef == null &&
        Object.keys(utxo.assets).every((unit) => unit === "lovelace"),
    );
    if (nonceInput === undefined)
      throw new Error("No ordinary funding input for forced order");
    const mintingReference = deployment.references.get("txOrderMint");
    if (mintingReference === undefined)
      throw new Error("Missing published tx-order minting reference");
    const refund = await Effect.runPromise(
      SDK.addressDataFromBech32(await lucid.wallet().address()),
    );
    const refundAddress: SDK.TxOrderRefundAddress = {
      paymentCredential: refund.paymentCredential,
      stakeCredential:
        refund.stakeCredential !== null && "Pointer" in refund.stakeCredential
          ? { Pointer: refund.stakeCredential.Pointer[0] }
          : refund.stakeCredential,
    };
    const order = await Effect.runPromise(
      SDK.buildUnsignedTxOrderTxWithMetadataProgram(lucid, contracts, {
        nativeTxCbor: nativeTxCbor.toString("hex"),
        nonceInput,
        refundAddress,
        referenceScripts: { txOrderMinting: mintingReference },
      }),
    );
    const signed = await order.tx.sign.withWallet().complete();
    checkpoint = {
      deploymentFingerprint: deployment.manifest.manifestId,
      nativeTxCbor: nativeTxCbor.toString("hex"),
      signedCbor: signed.toCBOR(),
      txHash: signed.toHash(),
      metadata: order.metadata,
      confirmed: false,
    };
    // Preserve the exact submission before sending it. A resumed attempt only
    // ever submits these bytes; it cannot mint a new event from a fresh nonce.
    await writeJourneyArtifact(checkpointPath, checkpoint);
  }
  if (
    CML.hash_transaction(
      CML.Transaction.from_cbor_hex(checkpoint.signedCbor).body(),
    ).to_hex() !== checkpoint.txHash
  )
    throw new Error(
      "Recorded forced-order bytes changed their transaction hash",
    );
  if (!checkpoint.confirmed) {
    const outputs = await provider.getUtxosWithUnit(
      contracts.txOrder.spendingScriptAddress,
      checkpoint.metadata.txOrderAuthUnit,
    );
    if (outputs.length > 1)
      throw new Error("Duplicate forced-order role token");
    if (outputs.length === 0) {
      onStage("forced order publication");
      try {
        const txHash = await provider.submitTx(checkpoint.signedCbor);
        if (txHash !== checkpoint.txHash)
          throw new Error(
            "Provider changed the signed forced-order transaction hash",
          );
      } catch (cause) {
        // The submission can have reached the node. Preserve the unresolved
        // checkpoint so a caller reconciles it instead of creating a new order.
        throw new Error(
          `Forced order ${checkpoint.txHash} submission needs reconciliation`,
          { cause },
        );
      }
    }
    await provider.awaitTx(checkpoint.txHash, 500);
    checkpoint.confirmed = true;
    await writeJourneyArtifact(checkpointPath, checkpoint);
    lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
  }
  onStage("forced order inclusion interval");
  await deployment.chain.awaitSlot(
    Math.max(
      0,
      Math.ceil(
        (checkpoint.metadata.inclusionTime - deployment.chain.now()) /
          context.customNetwork.slotConfig.slotLength,
      ),
    ),
  );
  if (
    input.predecessor.header.endTime >=
    BigInt(checkpoint.metadata.inclusionTime)
  )
    throw new Error("Forced order is not due after the retained predecessor");
  return checkpoint.metadata.txOrderId;
};

export const createForcedTransactionJourneyFixture = (
  category: JourneyCategory,
  prepareMaterial: (input: JourneyFaultPreparationInput) => {
    transaction: { canonicalCbor: Buffer };
  },
  builders: {
    buildFault(
      input: JourneyFaultBuildInput & { orderKey: SDK.OutputReference },
    ): ReturnType<JourneyPreparedFault["buildFault"]>;
    buildSuccessor(
      input: JourneyFaultBuildInput & { orderKey: SDK.OutputReference },
    ): ReturnType<JourneyPreparedFault["buildFault"]>;
  },
) =>
  createPreparedJourneyFixture(category, async (input) => {
    const material = prepareMaterial(input);
    const orderKey = await publishJourneyForcedOrder(
      input,
      material.transaction.canonicalCbor,
    );
    return {
      buildFault: (timed) => builders.buildFault({ ...timed, orderKey }),
      buildSuccessor: (timed) =>
        builders.buildSuccessor({ ...timed, orderKey }),
    };
  });
