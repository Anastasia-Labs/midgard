import { existsSync } from "node:fs";
import { join } from "node:path";

import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardForcedTxCanonical,
  submittedForcedTransactionFromNative,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { readJourneyArtifact, writeJourneyArtifact } from "./artifacts.js";
import type { JourneyCategory } from "./fixture.js";
import { reconcileSignedCommit } from "./signed-commit-reconciliation.js";
import {
  createPreparedJourneyFixture,
  type JourneyFaultBuildInput,
  type JourneyFaultPreparationInput,
  type JourneyPreparedFault,
} from "./staging.js";

type ForcedOrderCheckpoint = {
  deploymentFingerprint: string;
  submittedTxCbor: string;
  signedCbor: string;
  txHash: string;
  metadata: SDK.TxOrderBuildMetadata;
  confirmed: boolean;
};

/** Stage the actual order consumed by a forced-verdict fixture and its control. */
export const publishJourneyForcedOrder = async (
  input: JourneyFaultPreparationInput,
  submittedTxCbor: Buffer,
): Promise<SDK.OutputReference> => {
  const { context, directory, onStage } = input;
  const { deployment, provider } = context;
  const { operatorLucid: lucid, contracts } = deployment;
  const checkpointPath = join(directory, "forced-order.json");
  const buildAttempt = async (): Promise<ForcedOrderCheckpoint> => {
    // A fault's signed header already binds this order's identity. Only an
    // unsigned preparation can choose a new nonce after authenticated retirement.
    const stagedPath = join(directory, "staged.json");
    if (existsSync(stagedPath)) {
      const staged = await readJourneyArtifact<{
        deploymentFingerprint: string;
        signedCommit?: unknown;
        commitTxHash?: unknown;
      }>(stagedPath);
      if (
        staged.deploymentFingerprint !== deployment.manifest.manifestId ||
        staged.signedCommit !== undefined ||
        staged.commitTxHash !== undefined
      )
        throw new Error(
          "Cannot replace a forced order bound by a signed or published fault commitment",
        );
    }
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
        submittedTxCbor: submittedTxCbor.toString("hex"),
        nonceInput,
        refundAddress,
        // Tx-order references are optional in the deployment catalogue. The
        // SDK otherwise attaches this deployment's exact minting policy.
        ...(mintingReference === undefined
          ? {}
          : { referenceScripts: { txOrderMinting: mintingReference } }),
      }),
    );
    const signed = await order.tx.sign.withWallet().complete();
    const checkpoint: ForcedOrderCheckpoint = {
      deploymentFingerprint: deployment.manifest.manifestId,
      submittedTxCbor: submittedTxCbor.toString("hex"),
      signedCbor: signed.toCBOR(),
      txHash: signed.toHash(),
      metadata: order.metadata,
      confirmed: false,
    };
    const transaction = CML.Transaction.from_cbor_hex(checkpoint.signedCbor);
    try {
      if (
        !transaction.is_valid() ||
        CML.hash_transaction(transaction.body()).to_hex() !==
          checkpoint.txHash ||
        transaction.to_cbor_hex() !== checkpoint.signedCbor
      )
        throw new Error(
          "Built forced-order bytes changed their transaction identity",
        );
    } finally {
      transaction.free();
    }
    // Submit freshly built bytes immediately: their validity interval is short.
    // Every retry/restart first reconciles the durable exact attempt below.
    await writeJourneyArtifact(checkpointPath, checkpoint);
    onStage("forced order publication");
    let accepted: string | undefined;
    try {
      accepted = await provider.submitTx(checkpoint.signedCbor);
    } catch (cause) {
      onStage(
        `Forced order ${checkpoint.txHash} submission unresolved: ${String(cause)}`,
      );
    }
    if (accepted !== undefined && accepted !== checkpoint.txHash)
      throw new Error(
        "Provider changed the signed forced-order transaction hash",
      );
    return checkpoint;
  };
  let checkpoint = existsSync(checkpointPath)
    ? await readJourneyArtifact<ForcedOrderCheckpoint>(checkpointPath)
    : await buildAttempt();
  if (
    checkpoint.deploymentFingerprint !== deployment.manifest.manifestId ||
    checkpoint.submittedTxCbor !== submittedTxCbor.toString("hex")
  )
    throw new Error(
      "Forced order checkpoint changed deployment or exact submitted transaction bytes",
    );
  for (;;) {
    // Even a prior confirmed checkpoint must survive current canonical
    // reobservation. Pending or unknown attempts retain their exact signed bytes.
    const disposition = await reconcileSignedCommit({
      attempt: { txHash: checkpoint.txHash, signedCbor: checkpoint.signedCbor },
      readRecovery: input.readSignedCommitRecovery,
      pollDelay: async () => {
        await deployment.chain.delaySlots(1);
      },
      resubmit: async (signedCbor) => {
        onStage("forced order publication");
        return provider.submitTx(signedCbor);
      },
      onStage,
    });
    if (disposition.kind === "included") {
      checkpoint.confirmed = true;
      await writeJourneyArtifact(checkpointPath, checkpoint);
      lucid.overrideUTxOs(await lucid.utxosAt(await lucid.wallet().address()));
      break;
    }
    onStage(
      `Forced order ${checkpoint.txHash} retired: ${disposition.reason}; rebuilding from current wallet state`,
    );
    checkpoint = await buildAttempt();
  }
  onStage("forced order inclusion interval");
  await deployment.chain.awaitLedgerTime(checkpoint.metadata.inclusionTime);
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
      encodeMidgardForcedTxCanonical(
        submittedForcedTransactionFromNative(
          decodeMidgardNativeTxFullFromCanonicalCbor(
            material.transaction.canonicalCbor,
          ),
        ),
      ),
    );
    return {
      buildFault: (timed) => builders.buildFault({ ...timed, orderKey }),
      buildSuccessor: (timed) =>
        builders.buildSuccessor({ ...timed, orderKey }),
    };
  });
