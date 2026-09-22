import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardCekProgramEnvelope,
  decodeMidgardCekProgramMaterialSidecar,
  midgardCekProgramMaterialKindTag,
  verifyMidgardCekProgramMaterial,
} from "@al-ft/midgard-core/cek-proof";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
  validatorToRewardAddress,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";

const normalizeRoot = (root: Uint8Array | null | undefined) =>
  root == null || Buffer.from(root).equals(Buffer.alloc(32))
    ? SDK.EMPTY_MERKLE_TREE_ROOT
    : Buffer.from(root).toString("hex");
type Stack = {
  readonly task: SDK.CekMaterialTask;
  readonly root: string;
  readonly next?: Stack;
};
const push = (task: SDK.CekMaterialTask, next?: Stack): Stack => ({
  task,
  root: SDK.pushCekMaterialTask(
    task,
    next?.root ?? SDK.CEK_MATERIAL_EMPTY_STACK,
  ),
  ...(next === undefined ? {} : { next }),
});

export const initialCekMaterialTraversal = (material: {
  readonly envelopeCbor: Uint8Array;
  readonly programMaterialSidecarCbor: Uint8Array;
}): SDK.CekMaterialTraversalState => {
  const envelope = decodeMidgardCekProgramEnvelope(material.envelopeCbor);
  return {
    pending_root: push({
      kind: 0n,
      root: Buffer.from(envelope.termRoot).toString("hex"),
      expected_length: -1n,
    }).root,
    visited_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    node_count: 0n,
    byte_length: 0n,
    expected_node_count: envelope.nodeCount,
    expected_byte_length: envelope.materialByteLength,
  };
};

/** Replays from retained canonical material to the exact on-chain checkpoint before resuming. */
export const submitCekMaterialTraversal = async ({
  lucid,
  network,
  signer,
  contracts,
  threadUtxo: initialThread,
  threadUnit,
  material,
  traversalReference,
  taskReferences,
  awardDatum,
  getValidityRange,
  maxTransactions,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly contracts: SDK.ValidationTraceDisputeFaultProofContracts["validationTraceDispute"];
  readonly threadUtxo: UTxO;
  readonly threadUnit: string;
  readonly material: {
    readonly envelopeCbor: Uint8Array;
    readonly programMaterialSidecarCbor: Uint8Array;
  };
  readonly traversalReference: UTxO;
  readonly taskReferences: readonly [UTxO, UTxO];
  readonly awardDatum: string;
  readonly maxTransactions?: number;
  readonly getValidityRange: () => {
    readonly validFrom: number;
    readonly validTo: number;
  };
}) => {
  if (
    maxTransactions !== undefined &&
    (!Number.isSafeInteger(maxTransactions) || maxTransactions < 1)
  )
    throw new Error("CEK traversal transaction limit must be positive");
  const envelope = decodeMidgardCekProgramEnvelope(material.envelopeCbor);
  const entries = decodeMidgardCekProgramMaterialSidecar(
    material.programMaterialSidecarCbor,
  );
  verifyMidgardCekProgramMaterial(envelope, entries);
  const byRoot = new Map(
    entries.map((entry) => [Buffer.from(entry.root).toString("hex"), entry]),
  );
  const visited = new Set<string>();
  const trie = await Trie.fromList([]);
  let stack: Stack | undefined = push({
    kind: 0n,
    root: Buffer.from(envelope.termRoot).toString("hex"),
    expected_length: -1n,
  });
  let state = initialCekMaterialTraversal(material);
  let threadUtxo = initialThread;
  if (threadUtxo.datum == null)
    throw new Error("CEK material traversal checkpoint has no datum");
  const checkpoint = Data.from(threadUtxo.datum, SDK.CekMaterialTraversalDatum);
  if (
    checkpoint.data === null ||
    checkpoint.fraud_prover !== signer.paymentKeyHash
  )
    throw new Error(
      "CEK material traversal checkpoint has wrong owner or no state",
    );
  const checkpointCbor = Data.to(
    checkpoint.data,
    SDK.CekMaterialTraversalState,
  );
  let foundCheckpoint = false;
  const transactions: {
    kind: "traversal";
    txHash: string;
    nextThreadOutRef: string;
    completeSignedBytes: number;
    inputIndex: number;
    outputIndex: number;
  }[] = [];
  while (stack !== undefined) {
    if (
      !foundCheckpoint &&
      Data.to(state, SDK.CekMaterialTraversalState) === checkpointCbor
    )
      foundCheckpoint = true;
    const head: Stack = stack;
    const entry = byRoot.get(head.task.root);
    if (entry === undefined)
      throw new Error("CEK material traversal missing authenticated node");
    const alreadySeen = visited.has(head.task.root);
    if (!alreadySeen)
      await trie.insert(Buffer.from(head.task.root, "hex"), Buffer.from([1]));
    const proof = await trie.prove(Buffer.from(head.task.root, "hex"));
    if (normalizeRoot(proof.verify(alreadySeen)) !== state.visited_root)
      throw new Error("CEK visited proof does not open the current root");
    const visitedProof = Data.from(
      Buffer.from(proof.toCBOR()).toString("hex"),
      SDK.Proof,
    );
    stack = head.next;
    if (!alreadySeen) {
      visited.add(head.task.root);
      const children = SDK.cekMaterialChildren(entry);
      for (let i = children.length - 1; i >= 0; i--)
        stack = push(children[i]!, stack);
    }
    state = {
      ...state,
      pending_root: stack?.root ?? SDK.CEK_MATERIAL_EMPTY_STACK,
      visited_root: normalizeRoot(trie.hash),
      node_count: state.node_count + (alreadySeen ? 0n : 1n),
      byte_length:
        state.byte_length + (alreadySeen ? 0n : BigInt(entry.preimage.length)),
    };
    if (!foundCheckpoint) continue;
    const terminal = stack === undefined;
    if (
      terminal &&
      (state.node_count !== state.expected_node_count ||
        state.byte_length !== state.expected_byte_length)
    )
      throw new Error("CEK material graph totals differ from envelope");
    const outputContract = terminal
      ? contracts.award
      : contracts.cekMaterialTraversal;
    const outputDatum = terminal
      ? awardDatum
      : Data.to(
          { fraud_prover: checkpoint.fraud_prover, data: state },
          SDK.CekMaterialTraversalDatum,
        );
    const roleIndex = head.task.kind <= 3n ? 0 : 1;
    const role = SDK.CEK_MATERIAL_TASK_YIELD_ROLES[roleIndex];
    const taskContract = contracts.yields[role.contract];
    const taskReference = taskReferences[roleIndex];
    let inputIndex: bigint | undefined;
    let outputIndex: bigint | undefined;
    const currentInput = threadUtxo;
    const redeemer = ((ctx) => {
      SDK.requireOwnSpendPurpose(ctx, currentInput, "CEK material traversal");
      inputIndex = SDK.requireInputIndex(
        ctx,
        currentInput,
        "CEK material traversal",
      );
      outputIndex = SDK.requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputContract.spendingScriptAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "CEK material traversal",
      );
      return Data.to(
        {
          Continue: [
            {
              input_index: inputIndex,
              output_index: outputIndex,
              task: head.task,
              tail_root: head.next?.root ?? SDK.CEK_MATERIAL_EMPTY_STACK,
              entry: {
                kind: midgardCekProgramMaterialKindTag(entry.kind),
                root: head.task.root,
                preimage: entry.preimage.toString("hex"),
              },
              already_seen: alreadySeen,
              visited_proof: visitedProof,
              yield_reference_input_index: SDK.requireReferenceInputIndex(
                ctx,
                taskReference,
                role.role,
              ),
              next_pending_root: state.pending_root,
            },
          ],
        },
        SDK.CekMaterialTraversalRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    signer.selectWallet(lucid);
    const { validFrom, validTo } = getValidityRange();
    const unsigned = await lucid
      .newTx()
      .collectFrom([selectFeeInput(await lucid.wallet().getUtxos())])
      .collectFrom([currentInput], redeemer)
      .readFrom([traversalReference, taskReference])
      .withdraw(
        validatorToRewardAddress(network, taskContract.withdrawalScript),
        0n,
        Data.void(),
      )
      .pay.ToContract(
        outputContract.spendingScriptAddress,
        { kind: "inline", value: outputDatum },
        { lovelace: currentInput.assets.lovelace ?? 0n, [threadUnit]: 1n },
      )
      .validFrom(validFrom)
      .validTo(validTo)
      .addSignerKey(signer.paymentKeyHash)
      .complete({ localUPLCEval: true });
    const signed = await unsigned.sign.withWallet().complete();
    const completeSignedBytes = signed.toCBOR().length / 2;
    if (completeSignedBytes > 16_384)
      throw new Error("CEK material traversal exceeds L1 transaction limit");
    if (inputIndex === undefined || outputIndex === undefined)
      throw new Error("CEK material traversal layout missing");
    const txHash = await signed.submit();
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    threadUtxo = await fetchUtxoByOutRef({
      lucid,
      outRef: { txHash, outputIndex: Number(outputIndex) },
      label: "CEK material traversal continuation",
    });
    transactions.push({
      kind: "traversal",
      txHash,
      nextThreadOutRef: outRefLabel(threadUtxo),
      completeSignedBytes,
      inputIndex: Number(inputIndex),
      outputIndex: Number(outputIndex),
    });
    if (maxTransactions !== undefined && transactions.length >= maxTransactions)
      break;
  }
  if (!foundCheckpoint || transactions.length === 0)
    throw new Error(
      "CEK material checkpoint is not reachable from the retained graph",
    );
  return { transactions, threadUtxo, completed: stack === undefined };
};
