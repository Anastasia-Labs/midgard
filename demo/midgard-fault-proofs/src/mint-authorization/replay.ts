import {
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardVersionedScript,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKFraudProof from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import { checkedLedgerAsLedger } from "../checked-ledger-store.js";
import {
  ledgerBeforeTraceStep,
  readingLedger,
} from "../event-ledger.ledger.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { retainedOutputReferenceScript } from "../evidence/retained-ledger-output.js";
import { type CheckedLedger, ledgerEntryLeaf } from "../ledger-at.js";
import { type Ledger, ledgerProof } from "../ledger-store/index.js";
import { transactionHasNonCanonicalMintItem } from "../mint-item-non-canonical/replay.js";
import { decodeTransactionMaterial } from "../prepare-double-spend.resolve-double-spend-pair.js";
import { keyValuePhasProof } from "../transition-trace/phas.js";
import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { buildEventToStepMembershipProof } from "../transition-trace/witnesses.build-source-membership-proof.js";
import { acceptedTransactionSubject } from "../workflow/detection-subject.js";
import {
  collectReplayFindingBatches,
  replayPrerequisiteFailure,
} from "../workflow/replay-prerequisite.js";
import { scanMintAuthorization } from "./prover.js";

export type MintAuthorizationCoordinate = Readonly<{
  sourceIndex: number;
  policyIndex: string;
}>;
export const mintAuthorizationDetectionId = (c: MintAuthorizationCoordinate) =>
  `${SDKFraudProof.MINT_AUTHORIZATION_VIOLATION_ID}:${c.sourceIndex}:${c.policyIndex}`;

/** `prepareMintAuthorizationReplay`, asking for the predecessor only once
 * the transaction mints. */
const replayTransaction = async ({
  current,
  predecessor,
  sourceIndex,
}: {
  current: TransitionTraceReconstruction;
  predecessor: () => Promise<Ledger | undefined>;
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
  const ledger = await ledgerBeforeTraceStep(
    current,
    event.value.step_index,
    await predecessor(),
  );
  const referenceItems = decodeMidgardFieldPreimage(
    tx.body.referenceInputsPreimageCbor,
  );
  const spendItems = decodeMidgardFieldPreimage(
    tx.body.spendInputsPreimageCbor,
  );
  const outputs = new Map<string, Buffer>();
  await readingLedger("mint authorization resolved inputs", async () => {
    for (const [keyHex, { output }] of await ledger.read([
      ...referenceItems,
      ...spendItems,
    ]))
      if (output !== undefined) outputs.set(keyHex, output);
  });
  // A missing reference belongs to the reference-input absence family, and a
  // missing spent input to the non-existent-input family.
  if (
    [...referenceItems, ...spendItems].some(
      (key) => !outputs.has(key.toString("hex")),
    )
  )
    return [];
  const nativeScriptBytesByHashHex: Record<string, string> = {};
  const resolve = (key: Buffer) => {
    const output = outputs.get(key.toString("hex"))!;
    // The output's trie leaf: the descriptor the ledger commits for it.
    const { leaf } = ledgerEntryLeaf(
      { outRef: key, output },
      "mint authorization resolved input",
    );
    const descriptor = decodeMidgardLedgerOutputCommitment(leaf);
    const scriptBytes = retainedOutputReferenceScript(output);
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
      descriptorCbor: leaf.toString("hex"),
      referenceScriptLanguage: BigInt(descriptor.referenceScriptLanguage),
      referenceScriptHashHex: descriptor.referenceScriptHash.toString("hex"),
    };
  };
  const references = referenceItems.map(resolve);
  const spends = spendItems.map(resolve);
  const findings = scanMintAuthorization({
    mintPreimageCbor: tx.body.mintPreimageCbor,
    scriptTxWitsPreimageCbor: tx.witnessSet.scriptTxWitsPreimageCbor,
    addrTxWitsPreimageCbor: tx.witnessSet.addrTxWitsPreimageCbor,
    validityIntervalStart: tx.body.validityIntervalStart,
    validityIntervalEnd: tx.body.validityIntervalEnd,
    resolvedOutputScripts: [...references, ...spends],
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
    spendInputItemCbors: items(tx.body.spendInputsPreimageCbor),
    references,
    spends,
    referenceLedger: ledger,
  }));
};

/** Reconstruct every claim input from committed source and retained output bytes. */
export const prepareMintAuthorizationReplay = async ({
  current,
  predecessor,
  sourceIndex,
}: {
  current: TransitionTraceReconstruction;
  /** The ledger the block starts from; none when it starts from genesis. */
  predecessor?: Ledger;
  sourceIndex: number;
}) =>
  await replayTransaction({
    current,
    predecessor: async () => predecessor,
    sourceIndex,
  });

type MintAuthorizationReplayItem = Awaited<
  ReturnType<typeof prepareMintAuthorizationReplay>
>[number];

/** The membership proof of `input`, one of `prepared`'s resolved inputs, in
 * the ledger before its transaction: what step 04 carries for it. */
export const mintAuthorizationInputProof = async (
  prepared: Pick<MintAuthorizationReplayItem, "referenceLedger">,
  input: Readonly<{ keyHex: string }>,
): Promise<SDK.Proof> =>
  await readingLedger(
    "mint authorization input proof",
    async () =>
      await ledgerProof(
        prepared.referenceLedger,
        Buffer.from(input.keyHex, "hex"),
      ),
  );

export const detectMintAuthorizationReplay = async ({
  block,
  predecessor,
}: {
  block: CanonicalBlockEvidence;
  predecessor?: CheckedLedger;
}) => {
  // The predecessor as a store ledger, converted once, for the first
  // transaction that mints: a block with none converts nothing.
  let ledger: Promise<Ledger> | undefined;
  const predecessorLedger = async () =>
    predecessor === undefined
      ? undefined
      : await (ledger ??= checkedLedgerAsLedger(predecessor));
  return collectReplayFindingBatches(
    block.reconstruction.transactions,
    async (source, sourceIndex) => {
      // A mint item outside the field-5 grammar has no authorization
      // meaning; the direct mint-item proof owns that transaction.
      if (transactionHasNonCanonicalMintItem(source.fullTransactionCbor))
        throw replayPrerequisiteFailure(
          block.headerHash,
          { L2TransactionEventKey: { tx_id: source.txId } },
          "representable_field_shape",
        );
      const prepared = await replayTransaction({
        current: block.reconstruction,
        predecessor: predecessorLedger,
        sourceIndex,
      });
      return prepared.map((item) => ({
        ...acceptedTransactionSubject(source.txId),
        detectionId: mintAuthorizationDetectionId(item.coordinate),
        headerHash: block.headerHash,
        violationId: SDKFraudProof.MINT_AUTHORIZATION_VIOLATION_ID,
        position: BigInt(sourceIndex),
        diagnostic: `accepted transaction ${item.txInclusion.nativeTxId} has unauthorized mint policy ${item.finding.policyIdHex}`,
        prepared: item,
      }));
    },
  );
};
