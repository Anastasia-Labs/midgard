/** Init a validation-trace thread on one of the Open scene's blocks and
 * Open it with a claim (validation-trace-dispute-open-scene.ts). */
import * as SDK from "@al-ft/midgard-sdk";
import { toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { submitResolvedInit } from "../../src/submit-init.submit-resolved-init.js";
import { network } from "./submit-init-emulator-shared.js";
import type { Block, Scene } from "./validation-trace-dispute-open-scene.js";
import { measureWithinTransactionLimits } from "./validation-trace-dispute-transaction-limits.js";

const liveBlock = async (scene: Scene, block: Block): Promise<UTxO> => {
  const { h } = scene;
  const [node] = await h.proverLucid.utxosAtWithUnit(
    h.contracts.stateQueue.spendingScriptAddress,
    toUnit(
      h.contracts.stateQueue.policyId,
      SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.evidence.headerHash,
    ),
  );
  if (node === undefined) throw new Error("the challenged block is absent");
  return node;
};

/** Init a validation-trace thread on `block`, then Open it with `claim`
 * within the transaction limits; returns the pending claim at source-v1. */
export const initAndOpen = async (
  scene: Scene,
  block: Block,
  claim: SDK.ValidationDisputeClaim,
): Promise<{ readonly opener: UTxO }> => {
  const { h, chain } = scene;
  const category = h.catalogue.categories.validationTraceDispute;
  if (category === undefined)
    throw new Error("catalogue has no validation-trace category");
  const blockUtxo = await liveBlock(scene, block);
  const init = await submitResolvedInit({
    lucid: h.proverLucid,
    blueprint: h.realBlueprint,
    network,
    label: "validation-trace open",
    contracts: {
      steps: [h.contracts.fraudProofs.validationTraceDispute],
      computationThread: h.contracts.computationThread,
      hubOraclePolicyId: h.contracts.hubOracle.policyId,
      stateQueuePolicyId: h.contracts.stateQueue.policyId,
    },
    category,
    catalogue: {
      policyId: h.contracts.fraudProofCatalogue.policyId,
      spendingScriptAddress:
        h.contracts.fraudProofCatalogue.spendingScriptAddress,
      root: h.catalogue.root,
    },
    signer: h.proverSigner,
    fraudulentBlockOutRef: `${blockUtxo.txHash}#${blockUtxo.outputIndex.toString()}`,
    witnessReferenceScripts: h.witnessReferenceScripts,
    awaitConfirmation: true,
  });
  const thread = (
    await h.proverLucid.utxosAtWithUnit(
      init.firstStepAddress,
      init.computationThreadUnit,
    )
  ).find((utxo) => utxo.txHash === init.txHash);
  if (thread === undefined) throw new Error("the Init thread is absent");
  const challenged = await Effect.runPromise(
    SDK.validationDisputeChallengedBlock(
      blockUtxo,
      h.contracts.stateQueue.policyId,
    ),
  );
  const built = await SDK.appendValidationDisputeOpen(h.proverLucid.newTx(), {
    contracts: { opener: chain.opener, source: chain.source },
    computationThreadPolicyId: h.contracts.computationThread.policyId,
    openerReferenceScript: scene.references.opener,
    thread,
    fraudProver: init.fraudProver,
    hubOracle: scene.hubOracle,
    challenged,
    claim,
  }).complete({ localUPLCEval: true });
  const signed = await built.sign.withWallet().complete();
  measureWithinTransactionLimits(signed.toCBOR());
  const txHash = await signed.submit();
  await h.proverLucid.awaitTx(txHash);
  const opener = (
    await h.proverLucid.utxosAtWithUnit(
      chain.source.spendingScriptAddress,
      init.computationThreadUnit,
    )
  ).find((utxo) => utxo.txHash === txHash);
  if (opener === undefined) throw new Error("the pending claim is absent");
  return { opener };
};
