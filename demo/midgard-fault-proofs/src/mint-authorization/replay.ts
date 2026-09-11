import {
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { retainedOutputReferenceScript } from "../evidence/retained-ledger-output.js";
import { nativeScriptDecodingPriorLedger } from "../native-script-decoding/replay.js";
import { decodeTransactionMaterial } from "../prepare-double-spend.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import {
  buildEventToStepMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";
import { collectReplayFindingBatches } from "../workflow/replay-prerequisite.js";
import { scanMintAuthorization } from "./prover.js";

export type MintAuthorizationCoordinate = Readonly<{
  sourceIndex: number;
  policyIndex: string;
}>;
export const mintAuthorizationDetectionId = (c: MintAuthorizationCoordinate) =>
  `${SDK.MINT_AUTHORIZATION_VIOLATION_ID}:${c.sourceIndex}:${c.policyIndex}`;

/** Reconstruct every claim input from committed source and retained output bytes. */
export const prepareMintAuthorizationReplay = async ({
  current,
  predecessor,
  sourceIndex,
}: {
  current: TransitionTraceReconstruction;
  predecessor?: TransitionTraceReconstruction;
  sourceIndex: number;
}) => {
  const source = current.transactions[sourceIndex];
  if (source === undefined)
    throw new Error("mint authorization: source absent");
  if (source.validity !== "TxIsValid") return [];
  const material = await decodeTransactionMaterial({
    nodeTxId: source.txId,
    txCbor: source.fullTransactionCbor.toString("hex"),
    l2TransactionSourceCbor: source.valueBytes.toString("hex"),
  });
  const tx = material.nativeTx;
  if (decodeMidgardFieldPreimage(tx.body.mintPreimageCbor).length === 0)
    return [];
  const event = await buildEventToStepMembershipProof({
    reconstruction: current,
    eventKey: { L2TransactionEventKey: { tx_id: source.txId } },
  });
  const transition = await buildIndexedTraceProof({
    reconstruction: current,
    stepIndex: event.value.step_index,
  });
  const ledger = await nativeScriptDecodingPriorLedger(
    current,
    event.value.step_index,
    predecessor,
  );
  if (ledger.root.root !== transition.value.pre_utxos_root)
    throw new Error("mint authorization: selected event prior root changed");
  const referenceItems = decodeMidgardFieldPreimage(
    tx.body.referenceInputsPreimageCbor,
  );
  // A missing reference belongs to the reference-input absence family.
  if (referenceItems.some((key) => !ledger.outputs.has(key.toString("hex"))))
    return [];
  const nativeScriptBytesByHashHex: Record<string, string> = {};
  const references = referenceItems.map((key) => {
    const output = ledger.outputs.get(key.toString("hex"))!;
    const entry = ledger.root.entries.find((entry) => entry.key.equals(key));
    if (entry === undefined)
      throw new Error("mint authorization: descriptor absent");
    const descriptor = decodeMidgardLedgerOutputCommitment(entry.value);
    const scriptBytes = retainedOutputReferenceScript(output.value);
    if (descriptor.referenceScriptLanguage === 0) {
      if (scriptBytes === null)
        throw new Error("mint authorization: retained native payload absent");
      const script = decodeMidgardVersionedScript(scriptBytes);
      if (
        script.language !== "NativeCardano" ||
        hashMidgardVersionedScript(script) !==
          descriptor.referenceScriptHash.toString("hex")
      )
        throw new Error(
          "mint authorization: retained native payload hash changed",
        );
      nativeScriptBytesByHashHex[hashMidgardVersionedScript(script)] =
        script.scriptBytes.toString("hex");
    }
    return {
      keyHex: key.toString("hex"),
      descriptorCbor: entry.value.toString("hex"),
      referenceScriptLanguage: BigInt(descriptor.referenceScriptLanguage),
      referenceScriptHashHex: descriptor.referenceScriptHash.toString("hex"),
    };
  });
  const findings = scanMintAuthorization({
    mintPreimageCbor: tx.body.mintPreimageCbor,
    scriptTxWitsPreimageCbor: tx.witnessSet.scriptTxWitsPreimageCbor,
    addrTxWitsPreimageCbor: tx.witnessSet.addrTxWitsPreimageCbor,
    validityIntervalStart: tx.body.validityIntervalStart,
    validityIntervalEnd: tx.body.validityIntervalEnd,
    resolvedReferenceScripts: references,
    nativeScriptBytesByHashHex,
  });
  const proof = await keyValuePhasProof(
    {
      ...current.rootData.transactions,
      root: current.rootData.transactions.phasRoot,
    },
    source.keyBytes,
    source.valueBytes,
  );
  const items = (bytes: Uint8Array) =>
    decodeMidgardFieldPreimage(bytes).map((item) => item.toString("hex"));
  return findings.map((finding) => ({
    coordinate: { sourceIndex, policyIndex: finding.policyIndex.toString() },
    finding,
    current,
    txInclusion: {
      nativeTxId: source.txId,
      nativeTx: material.nativeTxCompact,
      nativeTxCompactCbor: material.nativeCompactCbor,
      l2TransactionSourceCbor: source.valueBytes.toString("hex"),
      transactionsPhasRoot: current.rootData.transactions.phasRoot,
      txMembershipProofCbor: Data.to(proof, SDK.Proof),
    },
    nativeTxCanonicalCbor: material.txCbor,
    nativeTxCompactCbor: material.nativeCompactCbor,
    witnessSet: material.nativeTxCompact.witness_set_hash,
    mintItemCbors: items(tx.body.mintPreimageCbor),
    scriptWitnessItemCbors: items(tx.witnessSet.scriptTxWitsPreimageCbor),
    addrWitnessItemCbors: items(tx.witnessSet.addrTxWitsPreimageCbor),
    referenceInputItemCbors: items(tx.body.referenceInputsPreimageCbor),
    references,
    referenceLedger: ledger.root,
  }));
};

export const detectMintAuthorizationReplay = async ({
  block,
  predecessor,
}: {
  block: CanonicalBlockEvidence;
  predecessor?: CanonicalBlockEvidence;
}) => {
  return collectReplayFindingBatches(
    block.reconstruction.transactions.map(async (_, sourceIndex) => {
      const prepared = await prepareMintAuthorizationReplay({
        current: block.reconstruction,
        predecessor: predecessor?.reconstruction,
        sourceIndex,
      });
      return prepared.map((item) => ({
        detectionId: mintAuthorizationDetectionId(item.coordinate),
        headerHash: block.headerHash,
        violationId: SDK.MINT_AUTHORIZATION_VIOLATION_ID,
        position: BigInt(sourceIndex),
        diagnostic: `accepted transaction ${item.txInclusion.nativeTxId} has unauthorized mint policy ${item.finding.policyIdHex}`,
        prepared: item,
      }));
    }),
  );
};
