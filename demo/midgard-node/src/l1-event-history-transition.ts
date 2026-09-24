import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { Data, datumToHash } from "@lucid-evolution/lucid";

import type { EventHistorySourceBinding } from "./l1-event-history-source.js";
import {
  type HistoryChainTransaction,
  historyZeroWithdrawal,
} from "./l1-event-history-transaction.js";
import type { LedgerSnapshotOutput } from "./l1-ledger-snapshot.js";

type Kind = "deposit" | "withdrawal";
type Node = SDK.AuthenticatedHistoryNode;
/** Stable, pointer-independent facts for durable provenance. Output location
 * is separate so a continuation cannot replace the admission's identity. */
export type HistoryTransitionEvent = Readonly<{
  key: string;
  idCbor: string;
  inclusionTime: bigint;
  factsCbor: string;
  payloadCbor: string;
  originalAssetsCbor: string;
  outRef: OutRefLike;
}>;
export type HistoryTransition = Readonly<{
  kind: Kind;
  operation:
    | "Initialize"
    | "InsertOrder"
    | "InsertFiller"
    | "PromoteFiller"
    | "ReclaimFiller"
    | "RetireOrder";
  transactionHash: string;
  consumed: readonly OutRefLike[];
  produced: readonly LedgerSnapshotOutput[];
  admission?: HistoryTransitionEvent;
  continuations: readonly Readonly<{
    key: string;
    before: OutRefLike;
    after: OutRefLike;
  }>[];
  retirement?: Readonly<{
    event: HistoryTransitionEvent;
    reason: "absorbed" | "payout_initialized" | "refunded";
    observerRedeemerIndex: number;
    witnessCbor: string;
  }>;
}>;

const fail = (message: string): never => {
  throw new Error(`Invalid history transition: ${message}`);
};
const label = (ref: OutRefLike) => `${ref.txHash}#${ref.outputIndex}`;
const refOf = (output: OutRefLike): OutRefLike =>
  Object.freeze({ txHash: output.txHash, outputIndex: output.outputIndex });
const at = <T>(items: readonly T[], index: bigint, description: string): T => {
  if (index < 0n || index >= BigInt(items.length))
    return fail(`${description} index is outside the transaction roster`);
  return items[Number(index)]!;
};
const order = (node: Node) => {
  if (node.node.payload === "RootContent" || !("Order" in node.node.payload))
    return fail("expected an Order node");
  return node.node.payload.Order.facts;
};
const sameAssets = (
  left: LedgerSnapshotOutput["assets"],
  right: LedgerSnapshotOutput["assets"],
) =>
  Object.keys(left).length === Object.keys(right).length &&
  Object.entries(left).every(([unit, amount]) => right[unit] === amount);
const authenticate = (
  outputs: readonly LedgerSnapshotOutput[],
  deployment: SDK.EventHistoryDeployment,
) => {
  const selected = outputs.filter((output) =>
    Object.keys(output.assets).some((unit) =>
      unit.startsWith(deployment.policyId),
    ),
  );
  if (selected.some((output) => output.hasReferenceScript))
    return fail("authenticated output carries a reference script");
  return SDK.authenticateHistoryNodes(
    selected.map((output) => ({ ...output, assets: { ...output.assets } })),
    deployment,
  );
};

/** Interpret one valid, canonically admitted L1 transaction. The caller supplies
 * the complete pre-transaction list and exact historical reference outputs from
 * that same branch, using contracts from the source binding's admitted manifest.
 * This is not a second ledger validator or a source of canonical authority.
 * Missing provenance fails; absence alone never creates a retirement record.
 */
export const decodeEventHistoryTransition = ({
  transaction,
  kind,
  history,
  binding,
  currentNodes,
  resolveReference,
  slotToUnixTime,
}: {
  readonly transaction: HistoryChainTransaction;
  readonly kind: Kind;
  readonly history: SDK.EventHistoryContracts;
  readonly binding: Pick<
    EventHistorySourceBinding,
    "deployments" | "hubAddress" | "hubUnit" | "hubDatumCbor" | "network"
  >;
  readonly currentNodes: readonly LedgerSnapshotOutput[];
  /** Scoped by the owner to this observing transaction and canonical branch. */
  readonly resolveReference: (
    ref: OutRefLike,
  ) => LedgerSnapshotOutput | undefined;
  /** Derived from the approved source's slot configuration, never wall time. */
  readonly slotToUnixTime: (slot: number) => number;
}): HistoryTransition | null => {
  // Failed transaction body intents have no list effects, even if they contain
  // the expected observer, mint and ordinary outputs.
  if (transaction.spends !== "inputs") return null;
  const deployment = SDK.eventHistoryDeploymentFromContracts(history);
  const bound = binding.deployments[kind];
  if (
    deployment.policyId !== bound.policyId ||
    deployment.address !== bound.address ||
    deployment.retentionAddress !== bound.retentionAddress ||
    deployment.inlineLimitBytes !== bound.inlineLimitBytes ||
    history.list.withdrawalScriptHash !== deployment.policyId ||
    history.recipe.kind !== (kind === "deposit" ? "Deposit" : "Withdrawal") ||
    history.recipe.hubPolicyId !== binding.hubUnit.slice(0, 56)
  )
    return fail("contract views differ from the admitted source binding");

  const policy = deployment.policyId;
  const before = authenticate(currentNodes, deployment);
  const inputs = before.filter((node) =>
    transaction.inputs.some((ref) => label(ref) === label(node.utxo)),
  );
  const outputs = authenticate(transaction.outputs, deployment);
  const mint = Object.entries(transaction.mint).filter(([unit]) =>
    unit.startsWith(policy),
  );
  const networkId = binding.network === "Mainnet" ? 1 : 0;
  const observed = historyZeroWithdrawal(transaction, policy, networkId);
  if (inputs.length === 0 && outputs.length === 0 && mint.length === 0) {
    if (observed !== null) return fail("list observer has no list effects");
    return null;
  }
  if (observed === null) return fail("list effects lack their exact observer");
  const observe = Data.from(observed.redeemer.cbor, SDK.EventHistoryObserve);
  const claimedInputs = new Set<string>();
  const claimedOutputs = new Set<string>();
  const continuations: HistoryTransition["continuations"][number][] = [];
  const input = (index: bigint): Node => {
    const ref = at(transaction.inputs, index, "input");
    const node = inputs.find((entry) => label(entry.utxo) === label(ref));
    if (node === undefined) return fail("history input provenance is missing");
    if (claimedInputs.has(label(ref))) return fail("history input is reused");
    const redeemer = transaction.redeemers.find(
      (entry) => entry.purpose === "spend" && entry.index === Number(index),
    );
    if (redeemer === undefined || Data.from(redeemer.cbor) !== index)
      return fail("history Spend does not name its own input index");
    claimedInputs.add(label(ref));
    return node;
  };
  const output = (index: bigint): Node => {
    const ref = at(transaction.outputs, index, "output");
    const node = outputs.find((entry) => label(entry.utxo) === label(ref));
    if (node === undefined)
      return fail("operation output is not authenticated");
    if (claimedOutputs.has(label(ref))) return fail("history output is reused");
    claimedOutputs.add(label(ref));
    return node;
  };
  const reference = (index: bigint): LedgerSnapshotOutput => {
    const ref = at(transaction.references, index, "reference");
    const resolved = resolveReference(ref);
    if (resolved === undefined || label(resolved) !== label(ref))
      return fail("exact historical reference output is unavailable");
    return resolved;
  };
  const validityUpperTime = (): number => {
    if (transaction.invalidAfter === undefined)
      return fail("operation requires a finite validity limit");
    const time = slotToUnixTime(transaction.invalidAfter);
    if (!Number.isSafeInteger(time) || time < 0)
      return fail("source slot clock returned an invalid timestamp");
    return time;
  };
  const requireMint = (key: string, amount: bigint) => {
    if (
      amount === 0n
        ? mint.length !== 0
        : mint.length !== 1 ||
          mint[0]![0] !== policy + key ||
          mint[0]![1] !== amount
    )
      return fail("list mint does not match the operation");
    if (amount !== 0n) {
      const policies = [
        ...new Set(
          Object.keys(transaction.mint).map((unit) => unit.slice(0, 56)),
        ),
      ].sort();
      if (
        !transaction.redeemers.some(
          (entry) =>
            entry.purpose === "mint" &&
            entry.index === policies.indexOf(policy),
        )
      )
        return fail("list mint has no redeemer");
    }
  };
  const continueNode = (old: Node, next: Node) => {
    if (
      old.key !== next.key ||
      old.utxo.address !== next.utxo.address ||
      !sameAssets(old.utxo.assets, next.utxo.assets) ||
      [0, 3].some(
        (field) =>
          aikenSerialisedPlutusDataCborPreservingMapOrder(
            plutusConstrFieldCbor(old.utxo.datum!, [field]),
          ) !==
          aikenSerialisedPlutusDataCborPreservingMapOrder(
            plutusConstrFieldCbor(next.utxo.datum!, [field]),
          ),
      ) ||
      next.node.protected_until < old.node.protected_until
    )
      return fail("continuation changes immutable facts, address or Value");
    if (old.node.payload !== "RootContent" && "Order" in old.node.payload)
      continuations.push(
        Object.freeze({
          key: old.key!,
          before: refOf(old.utxo),
          after: refOf(next.utxo),
        }),
      );
  };
  const openOrder = (
    node: Node,
    externalIndex: bigint | null,
  ): HistoryTransitionEvent => {
    const facts = order(node);
    let rawPayload: string;
    let retainedDataUtxo: LedgerSnapshotOutput | undefined;
    if ("Inline" in facts.location) {
      if (externalIndex !== null)
        return fail("inline order claims external data");
      rawPayload = plutusConstrFieldCbor(node.utxo.datum!, [3, 0, 2, 0]);
    } else {
      if (externalIndex === null)
        return fail("external order omits its reference");
      retainedDataUtxo = reference(externalIndex);
      if (
        retainedDataUtxo.address !== deployment.retentionAddress ||
        retainedDataUtxo.hasReferenceScript ||
        retainedDataUtxo.datum === undefined ||
        retainedDataUtxo.datumHash !== undefined ||
        datumToHash(
          aikenSerialisedPlutusDataCborPreservingMapOrder(
            retainedDataUtxo.datum,
          ),
        ) !== facts.location.External.storage_datum_hash
      )
        return fail("external opening is not the referenced retention datum");
      const retained = Data.from(retainedDataUtxo.datum, SDK.EventHistoryData);
      if (retained.event_key !== node.key)
        return fail("retained data has another key");
      rawPayload = plutusConstrFieldCbor(retainedDataUtxo.datum, [1]);
    }
    const payloadCbor =
      aikenSerialisedPlutusDataCborPreservingMapOrder(rawPayload);
    const payload = Data.from(payloadCbor, SDK.EventHistoryPayload);
    if (
      "Inline" in facts.location &&
      BigInt(payloadCbor.length / 2) > deployment.inlineLimitBytes
    )
      return fail("inline payload exceeds the deployment bound");
    const captured = SDK.captureEventHistoryWitness(
      {
        kind: "Present",
        anchor: node,
        payload,
        payloadCbor,
        ...(retainedDataUtxo === undefined
          ? {}
          : {
              retainedDataUtxo: {
                ...retainedDataUtxo,
                assets: { ...retainedDataUtxo.assets },
              },
            }),
      },
      policy,
      kind === "deposit" ? "Deposit" : "Withdrawal",
    );
    return Object.freeze({
      key: node.key!,
      idCbor: Data.to(captured.commitment.event_id, SDK.OutputReference),
      inclusionTime: captured.commitment.inclusion_time,
      factsCbor: captured.factsCbor,
      payloadCbor,
      originalAssetsCbor: Data.to(captured.originalAssets, SDK.Value),
      outRef: refOf(node.utxo),
    });
  };
  let admission: HistoryTransitionEvent | undefined;
  let retirement: HistoryTransition["retirement"];
  let operation: HistoryTransition["operation"];
  if ("Initialize" in observe) {
    operation = "Initialize";
    const nonce = at(
      transaction.inputs,
      observe.Initialize.nonce_input_index,
      "nonce",
    );
    if (
      nonce.txHash !== history.recipe.initializationNonce.transactionId ||
      BigInt(nonce.outputIndex) !==
        history.recipe.initializationNonce.outputIndex ||
      before.length !== 0
    )
      return fail("initialization does not bind its unused deployment nonce");
    const root = output(observe.Initialize.root_output_index);
    if (
      root.key !== null ||
      root.node.next !== null ||
      root.node.protected_until !==
        BigInt(validityUpperTime()) - 1n + history.recipe.protectionDurationMs
    )
      return fail("initialization does not produce an empty root");
    requireMint("", 1n);
  } else {
    const hub = reference(observe.Apply.hub_reference_index);
    if (
      hub.address !== binding.hubAddress ||
      hub.assets[binding.hubUnit] !== 1n ||
      (hub.assets.lovelace ?? 0n) <= 0n ||
      Object.keys(hub.assets).some(
        (unit) => unit !== "lovelace" && unit !== binding.hubUnit,
      ) ||
      hub.hasReferenceScript ||
      hub.datumHash !== undefined ||
      hub.datum === undefined ||
      Data.to(Data.from(hub.datum, SDK.HubOracleDatum), SDK.HubOracleDatum) !==
        binding.hubDatumCbor
    )
      return fail("referenced hub differs from the admitted deployment");
    const op = observe.Apply.operation;
    if ("InsertOrder" in op || "InsertFiller" in op) {
      const args = "InsertOrder" in op ? op.InsertOrder : op.InsertFiller;
      const predecessor = input(args.predecessor_input_index);
      const next = output(args.predecessor_output_index);
      const inserted = output(
        "InsertOrder" in op
          ? op.InsertOrder.order_output_index
          : op.InsertFiller.filler_output_index,
      );
      continueNode(predecessor, next);
      if (
        inserted.key === null ||
        (predecessor.key !== null && predecessor.key >= inserted.key) ||
        (predecessor.node.next !== null &&
          inserted.key >= predecessor.node.next) ||
        next.node.next !== inserted.key ||
        inserted.node.next !== predecessor.node.next
      )
        return fail("insertion does not split the claimed predecessor gap");
      requireMint(inserted.key, 1n);
      if ("InsertOrder" in op) {
        operation = "InsertOrder";
        admission = openOrder(
          inserted,
          op.InsertOrder.external_reference_index,
        );
      } else {
        operation = "InsertFiller";
        if (
          inserted.node.payload === "RootContent" ||
          !("Filler" in inserted.node.payload)
        )
          return fail("filler insertion produced an Order");
      }
    } else if ("PromoteFiller" in op) {
      operation = "PromoteFiller";
      const args = op.PromoteFiller;
      const filler = input(args.filler_input_index);
      const promoted = output(args.order_output_index);
      if (
        filler.node.payload === "RootContent" ||
        !("Filler" in filler.node.payload) ||
        filler.key !== promoted.key ||
        filler.node.next !== promoted.node.next ||
        promoted.node.protected_until < filler.node.protected_until
      )
        return fail("promotion does not replace the exact filler");
      at(transaction.outputs, args.refund_output_index, "filler refund");
      requireMint(promoted.key!, 0n);
      admission = openOrder(promoted, args.external_reference_index);
    } else {
      let args:
        | SDK.EventHistoryRetirementWitness
        | Extract<
            SDK.EventHistoryOperation,
            { ReclaimFiller: unknown }
          >["ReclaimFiller"];
      if ("RetireOrder" in op) {
        operation = "RetireOrder";
        const observedRetirement = historyZeroWithdrawal(
          transaction,
          history.retirement.withdrawalScriptHash,
          networkId,
        );
        if (observedRetirement === null)
          return fail("retirement has no exact deployment observer");
        const decoded = Data.from(
          observedRetirement.redeemer.cbor,
          SDK.EventHistoryRetirementArgs,
        );
        if (
          decoded.hub_reference_index !== observe.Apply.hub_reference_index ||
          Data.to(
            SDK.eventHistoryRetirementOperation(decoded.witness),
            SDK.EventHistoryOperation,
          ) !== Data.to(op, SDK.EventHistoryOperation)
        )
          return fail("retirement and list observers disagree");
        args = decoded.witness;
        // Resolve the exact historical references now. Their finality/membership
        // predicates were enforced by the admitted transaction's validators.
        reference(args.confirmed_reference_index);
        reference(args.settlement_reference_index);
      } else {
        operation = "ReclaimFiller";
        args = op.ReclaimFiller;
      }
      const predecessor = input(args.predecessor_input_index);
      const removed = input(
        "order_input_index" in args
          ? args.order_input_index
          : args.filler_input_index,
      );
      const next = output(args.predecessor_output_index);
      continueNode(predecessor, next);
      if (
        removed.key === null ||
        predecessor.node.next !== removed.key ||
        next.node.next !== removed.node.next
      )
        return fail("removal does not join the claimed predecessor gap");
      requireMint(removed.key, -1n);
      if ("order_input_index" in args) {
        const event = openOrder(removed, args.external_reference_index);
        const reason =
          args.purpose === "AbsorbDeposit"
            ? "absorbed"
            : args.purpose === "InitializeWithdrawalPayout"
              ? "payout_initialized"
              : "refunded";
        if ((kind === "deposit") !== (reason === "absorbed"))
          return fail("retirement purpose belongs to the other list");
        at(transaction.outputs, args.funds_output_index, "retirement funds");
        if (args.structural_refund_output_index !== null)
          at(
            transaction.outputs,
            args.structural_refund_output_index,
            "structural refund",
          );
        const observer = historyZeroWithdrawal(
          transaction,
          history.retirement.withdrawalScriptHash,
          networkId,
        )!;
        retirement = Object.freeze({
          event,
          reason,
          observerRedeemerIndex: transaction.redeemers.indexOf(
            observer.redeemer,
          ),
          witnessCbor: Data.to(args, SDK.EventHistoryRetirementWitness),
        });
      } else {
        if (
          removed.node.payload === "RootContent" ||
          !("Filler" in removed.node.payload)
        )
          return fail("filler reclamation consumed an Order");
        at(transaction.outputs, args.refund_output_index, "filler refund");
      }
    }
    if (admission !== undefined) {
      const args =
        "InsertOrder" in op
          ? op.InsertOrder
          : "PromoteFiller" in op
            ? op.PromoteFiller
            : fail("admission without an admission operation");
      const nonce = at(transaction.inputs, args.nonce_input_index, "nonce");
      if (
        admission.idCbor !==
          Data.to(
            {
              transactionId: nonce.txHash,
              outputIndex: BigInt(nonce.outputIndex),
            },
            SDK.OutputReference,
          ) ||
        transaction.invalidAfter === undefined
      )
        return fail(
          "admission does not bind its consumed nonce and validity limit",
        );
      const inclusionTime = SDK.resolveEventInclusionTime(
        validityUpperTime(),
        binding.network,
      );
      if (
        !Number.isSafeInteger(inclusionTime) ||
        admission.inclusionTime !== BigInt(inclusionTime)
      )
        return fail(
          "admission inclusion time differs from its ledger validity limit",
        );
    }
  }
  if (
    claimedInputs.size !== inputs.length ||
    claimedOutputs.size !== outputs.length
  )
    return fail("operation leaves unclassified authenticated list effects");
  return Object.freeze({
    kind,
    operation,
    transactionHash: transaction.txHash,
    consumed: Object.freeze(inputs.map((node) => refOf(node.utxo))),
    produced: Object.freeze(
      transaction.outputs.filter((entry) => claimedOutputs.has(label(entry))),
    ),
    continuations: Object.freeze(continuations),
    ...(admission === undefined ? {} : { admission }),
    ...(retirement === undefined ? {} : { retirement }),
  });
};
