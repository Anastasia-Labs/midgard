import { CML } from "@lucid-evolution/lucid";

import { type WatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { type WatcherDeploymentIdentityPolicy } from "../../runtime/deployment-identity.js";
import { type WatcherUserEventReferenceEvidence } from ".././user-event-reference-authority.js";
import { consumeHistoryOrder } from "./decode.consume-history-order.js";
import {
  canonicalBody,
  decodeMintRedeemer,
  decodeWitnessRedeemer,
  matchingRedeemer,
  mintPolicyIndex,
  outputReference,
  redeemerAtGlobalIndex,
  registeredScriptHashAt,
} from "./decode.forced-order-material-field-count.js";
import { decodeTerminalSpend } from "./decode.scan-created-transaction-events.js";
import { verifyTerminalSemantics } from "./decode.verify-terminal-semantics.js";
import { isNatural, watcherForcedOperatorVerdict } from "./policy.js";
import {
  WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION,
  type WatcherIndexedUserEvent,
  type WatcherTerminalUserEvent,
} from "./types.js";

export const scanConsumedTransactionEvents = (
  block: WatcherNormalizedL1Block,
  transaction: WatcherNormalizedL1Block["transactions"][number],
  referenceEvidence: WatcherUserEventReferenceEvidence,
  active: Map<string, WatcherIndexedUserEvent>,
  terminal: WatcherTerminalUserEvent[],
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
): true | null => {
  if (!transaction.isValid) {
    return true;
  }
  const body = canonicalBody(transaction.body.bytesHex);
  if (body === null) {
    return null;
  }
  const inputs = body.inputs();
  const mint = body.mint();
  for (let inputIndex = 0; inputIndex < inputs.len(); inputIndex += 1) {
    const event = active.get(outputReference(inputs.get(inputIndex)));
    if (event === undefined) {
      continue;
    }
    if (event.kind !== "forced_order") {
      const disposition = consumeHistoryOrder(
        event,
        transaction,
        referenceEvidence,
        deployment,
        inputIndex,
      );
      if (disposition === null) return null;
      active.delete(event.outRef);
      if ("continuation" in disposition)
        active.set(disposition.continuation.outRef, disposition.continuation);
      else
        terminal.push(
          Object.freeze({
            ...event,
            terminalStatus: disposition.terminalStatus,
            terminalTransactionHash: transaction.txHash,
            terminalPointDigest: block.chainPoint.pointDigest,
            terminalBlockHash: block.chainPoint.blockHash,
            terminalSlot: block.chainPoint.slot,
            terminalBlockNo: block.chainPoint.blockNo,
            terminalFinalityStatus: "pending",
          }),
        );
      continue;
    }
    const spendRedeemer = matchingRedeemer(transaction, "spend", inputIndex);
    if (spendRedeemer === null || mint === undefined) {
      return null;
    }
    const policyIndex = mintPolicyIndex(mint, event.policyId);
    const terminalSpend =
      policyIndex < 0
        ? null
        : decodeTerminalSpend(
            event.kind,
            spendRedeemer.bytes.bytesHex,
            inputIndex,
          );
    const mintRedeemer =
      terminalSpend === null
        ? null
        : redeemerAtGlobalIndex(transaction, terminalSpend.mintRedeemerIndex);
    const decodedMint =
      mintRedeemer === null
        ? null
        : decodeMintRedeemer(mintRedeemer.bytes.bytesHex, event.kind);
    if (terminalSpend === null) {
      return null;
    }
    if (
      !verifyTerminalSemantics(
        event,
        referenceEvidence,
        transaction,
        body,
        terminalSpend,
        deployment,
      )
    ) {
      return null;
    }
    if (
      policyIndex < 0 ||
      mint.get(
        CML.ScriptHash.from_hex(event.policyId),
        CML.AssetName.from_hex(event.assetNameHex),
      ) !== -1n ||
      mint.get_assets(CML.ScriptHash.from_hex(event.policyId))?.len() !== 1 ||
      decodedMint === null ||
      !("BurnEventNFT" in decodedMint.event) ||
      decodedMint.event.BurnEventNFT.nonce_asset_name !== event.assetNameHex ||
      decodedMint.event.BurnEventNFT.witness_unregistration_redeemer_index <
        0n ||
      // #594: the tx-order policy requires a burn's carriage vector to be
      // empty, because a burn reads no material and an unread wire field is a
      // second spelling of the same transaction (§8.11, §6.1). `null` here is
      // the three unwrapped policies, which have no vector to constrain.
      (decodedMint.materialCarriage !== null &&
        decodedMint.materialCarriage.length !== 0)
    ) {
      return null;
    }
    const certificateRedeemer = redeemerAtGlobalIndex(
      transaction,
      decodedMint.event.BurnEventNFT.witness_unregistration_redeemer_index,
    );
    const certificateIndex =
      certificateRedeemer?.purpose === "certificate" &&
      isNatural(certificateRedeemer.index) &&
      BigInt(certificateRedeemer.index) <= BigInt(Number.MAX_SAFE_INTEGER)
        ? Number(certificateRedeemer.index)
        : -1;
    const witnessRedeemer =
      certificateRedeemer === null
        ? null
        : decodeWitnessRedeemer(certificateRedeemer.bytes.bytesHex);
    if (
      registeredScriptHashAt(body, certificateIndex, false) !==
        event.witnessScriptHash ||
      witnessRedeemer === null ||
      !("MintOrBurn" in witnessRedeemer) ||
      witnessRedeemer.MintOrBurn.targetPolicy !== event.policyId
    ) {
      return null;
    }
    const forcedOperatorValidity =
      event.kind === "forced_order"
        ? watcherForcedOperatorVerdict(terminalSpend.purpose)
        : null;
    if (event.kind === "forced_order" && forcedOperatorValidity === null) {
      return null;
    }
    active.delete(event.outRef);
    terminal.push(
      Object.freeze({
        ...event,
        terminalStatus: terminalSpend.terminalStatus,
        terminalTransactionHash: transaction.txHash,
        terminalPointDigest: block.chainPoint.pointDigest,
        terminalBlockHash: block.chainPoint.blockHash,
        terminalSlot: block.chainPoint.slot,
        terminalBlockNo: block.chainPoint.blockNo,
        terminalFinalityStatus: "pending",
        ...(forcedOperatorValidity === null
          ? {}
          : {
              terminalClassification: Object.freeze({
                schemaVersion:
                  WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION,
                operatorValidity: forcedOperatorValidity,
                terminalTransactionHash: transaction.txHash,
                terminalPointDigest: block.chainPoint.pointDigest,
              }),
            }),
      }),
    );
  }
  return true;
};
