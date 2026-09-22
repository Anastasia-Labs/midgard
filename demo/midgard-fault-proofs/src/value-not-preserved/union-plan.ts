import { Store, Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardLedgerOutputAssetFrontier,
  buildMidgardValidationMerkleMembershipIndex,
  computeHash32,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardMintPolicyItem,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeCbor,
  encodeMidgardDefiniteBytes,
  type MidgardLedgerOutputAsset,
} from "@al-ft/midgard-core";
import {
  advanceMidgardLedgerOutputScan,
  finishMidgardLedgerOutputScan,
  initialMidgardLedgerOutputScanControl,
  isExactMidgardLedgerOutputScanTerminal,
} from "@al-ft/midgard-core";
import {
  type EventToStepMembershipProof,
  fieldOpeningForField,
  type IndexedTraceProof,
  Proof,
} from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

import { transactionOutputScanControlData } from "../transaction-output-non-canonical/transaction-output-non-canonical.js";
import type { ValueNotPreservedContracts } from "./contracts.js";
import { flattenMidgardValueAssets } from "./evidence.js";
import { planConservationField } from "./field-plan.js";
import * as S from "./union-schemas.js";

export type ConservationPosition = Exclude<
  keyof ValueNotPreservedContracts,
  | "steps"
  | "computationThread"
  | "fraudProof"
  | "hubOraclePolicyId"
  | "stateQueuePolicyId"
  | "fieldPreimageCertificatePolicyId"
>;
export type ConservationAction = Readonly<{
  position: ConservationPosition;
  inputState: string;
  nextPosition: ConservationPosition | null;
  outputState: string | null;
  /** Constr 0 family args with neutral layout indexes, rewritten after balancing. */
  args: string;
  fieldIndex?: 0 | 2 | 5;
}>;
const raw = <T>(value: T, schema: T): Data =>
  Data.from(Data.to(value as never, schema as never));
const encoded = <T>(value: T, schema: T): string =>
  Data.to(value as never, schema as never);
const EMPTY_DELTA_ROOT = "00".repeat(32);

/** Deterministic complete-domain replay. Every action commits its exact predecessor. */
export const planConservationFold = async ({
  contracts,
  source,
  eventMembership,
  traceMembership,
  nativeTxCompactCbor,
  fields,
  spentInputs,
}: {
  readonly contracts: ValueNotPreservedContracts;
  readonly source: S.ConservationSource;
  readonly eventMembership: EventToStepMembershipProof;
  readonly traceMembership: IndexedTraceProof;
  readonly nativeTxCompactCbor: string;
  readonly fields: Readonly<Record<0 | 2 | 5, string>>;
  readonly spentInputs: readonly {
    readonly descriptorCbor: string;
    readonly proof: Proof;
    readonly assets: readonly MidgardLedgerOutputAsset[];
  }[];
}) => {
  const actions: ConservationAction[] = [];
  let position: ConservationPosition = "unionEvent";
  let state = encoded(source, S.ConservationSource);
  const append = (
    nextPosition: ConservationPosition | null,
    outputState: string | null,
    args: string,
    fieldIndex?: 0 | 2 | 5,
  ) => {
    actions.push({
      position,
      inputState: state,
      nextPosition,
      outputState,
      args,
      ...(fieldIndex === undefined ? {} : { fieldIndex }),
    });
    if (nextPosition !== null && outputState !== null) {
      position = nextPosition;
      state = outputState;
    }
  };
  const layout = { input_index: 0n, output_index: 0n };
  const event: S.ConservationEvent = {
    transaction_id: source.transaction_id,
    claim: source.claim,
    fee: source.fee,
    event_key: source.event_key,
    trace_root: source.trace_root,
    trace_count: source.trace_count,
    step_index: eventMembership.value.step_index,
  };
  append(
    "unionPreState",
    encoded(event, S.ConservationEvent),
    encoded(
      { ...layout, membership: eventMembership },
      S.ConservationEventArgs,
    ),
  );
  let balance: S.ConservationBalance = {
    transaction_id: source.transaction_id,
    claim: source.claim,
    pre_utxos_root: traceMembership.value.pre_utxos_root,
    lovelace_delta: -source.fee,
    asset_delta_root: EMPTY_DELTA_ROOT,
  };
  const fold = (continuation: Data): string =>
    encoded({ balance, continuation }, S.ConservationFold);
  append(
    "unionInputs",
    fold(raw({ cursor: 0n }, S.ConservationInputs)),
    encoded(
      { ...layout, membership: traceMembership },
      S.ConservationPreStateArgs,
    ),
  );
  const opening = (fieldIndex: 0 | 2 | 5) =>
    fieldOpeningForField({
      fieldIndex,
      nativeTxCompactCbor,
      carriage: { Inline: { preimage: fields[fieldIndex] } },
    });
  const store = new Store(undefined);
  await store.ready();
  const trie = new Trie(store);
  const deltas = new Map<string, bigint>();
  const contribute = async (
    unit: string,
    quantity: bigint,
    returnPosition: ConservationPosition,
    continuation: Data,
  ) => {
    const oldDelta = deltas.get(unit) ?? 0n;
    const nextDelta = oldDelta + quantity;
    const key = Buffer.from(unit, "hex");
    let proof: Proof;
    if (oldDelta === 0n) {
      await trie.insert(key, encodeCbor(nextDelta));
      proof = Data.from(
        (await trie.prove(key)).toCBOR().toString("hex"),
        Proof,
      );
    } else {
      proof = Data.from(
        (await trie.prove(key)).toCBOR().toString("hex"),
        Proof,
      );
      await trie.delete(key);
      if (nextDelta !== 0n) await trie.insert(key, encodeCbor(nextDelta));
    }
    if (nextDelta === 0n) deltas.delete(unit);
    else deltas.set(unit, nextDelta);
    balance = {
      ...balance,
      asset_delta_root:
        trie.hash == null
          ? EMPTY_DELTA_ROOT
          : Buffer.from(trie.hash).toString("hex"),
    };
    append(
      returnPosition,
      fold(continuation),
      encoded(
        { ...layout, old_delta: oldDelta, proof },
        S.ConservationUpdateArgs,
      ),
    );
  };
  const assets = async (
    leaves: readonly MidgardLedgerOutputAsset[],
    sign: bigint,
    nextPosition: ConservationPosition,
    next: Data,
  ) => {
    const material = buildMidgardLedgerOutputAssetFrontier(leaves);
    const membershipIndex = buildMidgardValidationMerkleMembershipIndex(
      material.leaves,
    );
    const cursor = (index: number): S.ConservationAssetCursor => ({
      count: BigInt(leaves.length),
      frontier_commitment: Buffer.from(material.commitment).toString("hex"),
      cursor: BigInt(index),
      quantity_sign: sign,
      next_script_hash: contracts[nextPosition].spendingScriptHash,
      next_continuation: next,
    });
    for (const [index, leaf] of leaves.entries()) {
      const continuation = raw(cursor(index + 1), S.ConservationAssetCursor);
      const unit = Buffer.concat([leaf.policyId, leaf.assetName]).toString(
        "hex",
      );
      const quantity = sign * leaf.quantity;
      const pending = raw(
        {
          unit,
          quantity,
          return_script_hash: contracts.unionAssets.spendingScriptHash,
          next_continuation: continuation,
        },
        S.ConservationPending,
      );
      append(
        "unionUpdate",
        fold(pending),
        encoded(
          {
            ...layout,
            action: {
              Select: {
                policy_id: Buffer.from(leaf.policyId).toString("hex"),
                asset_name: Buffer.from(leaf.assetName).toString("hex"),
                quantity: leaf.quantity,
                peaks: material.frontier.peaks.map((p) => ({
                  height: BigInt(p.height),
                  hash: Buffer.from(p.hash).toString("hex"),
                })),
                siblings: membershipIndex
                  .membershipAt(index)
                  .siblings.map((h) => Buffer.from(h).toString("hex")),
              },
            },
          },
          S.ConservationAssetsArgs,
        ),
      );
      await contribute(unit, quantity, "unionAssets", continuation);
    }
    append(
      nextPosition,
      fold(next),
      encoded({ ...layout, action: "Finish" }, S.ConservationAssetsArgs),
    );
  };
  const inputItems = decodeMidgardFieldPreimage(Buffer.from(fields[0], "hex"));
  if (inputItems.length !== spentInputs.length)
    throw new Error("value conservation: incomplete spent-input evidence");
  for (const [index, item] of inputItems.entries()) {
    const decoded = decodeMidgardSpendInputItem(item);
    const input = {
      tx_id: Buffer.from(decoded.txId).toString("hex"),
      output_index: BigInt(decoded.outputIndex),
    };
    append(
      "unionInputValue",
      fold(
        raw(
          {
            input,
            cursor: BigInt(index),
            selector_hash: contracts.unionInputs.spendingScriptHash,
          },
          S.ConservationSelectedInput,
        ),
      ),
      encoded({ ...layout, opening: opening(0) }, S.ConservationInputsArgs),
      0,
    );
    const spent = spentInputs[index]!;
    const descriptor = decodeMidgardLedgerOutputCommitment(
      Buffer.from(spent.descriptorCbor, "hex"),
    );
    const material = buildMidgardLedgerOutputAssetFrontier(spent.assets);
    if (
      descriptor.assetCount !== material.count ||
      !Buffer.from(descriptor.assetFrontierCommitment).equals(
        material.commitment,
      )
    )
      throw new Error(
        "value conservation: spent asset frontier differs from descriptor",
      );
    balance = {
      ...balance,
      lovelace_delta: balance.lovelace_delta + descriptor.lovelace,
    };
    const next = raw({ cursor: BigInt(index + 1) }, S.ConservationInputs);
    const cursor = raw(
      {
        count: BigInt(descriptor.assetCount),
        frontier_commitment: Buffer.from(
          descriptor.assetFrontierCommitment,
        ).toString("hex"),
        cursor: 0n,
        quantity_sign: 1n,
        next_script_hash: contracts.unionInputs.spendingScriptHash,
        next_continuation: next,
      },
      S.ConservationAssetCursor,
    );
    append(
      "unionAssets",
      fold(cursor),
      encoded(
        {
          ...layout,
          descriptor_cbor: spent.descriptorCbor,
          proof: spent.proof,
        },
        S.ConservationInputValueArgs,
      ),
    );
    await assets(spent.assets, 1n, "unionInputs", next);
  }
  append(
    "unionFieldGrammar",
    fold(
      raw(
        { field_index: 2n, checkpoint_hash: null },
        S.ConservationFieldGrammar,
      ),
    ),
    encoded({ ...layout, opening: opening(0) }, S.ConservationInputsArgs),
    0,
  );
  const grammar = (field: 2 | 5) => {
    const plan = planConservationField(
      source.transaction_id,
      field,
      Buffer.from(fields[field], "hex"),
    );
    const fieldCursor: S.ConservationFieldCursor = {
      field_index: BigInt(field),
      checkpoint_hash: plan.initialWalkHash,
      grammar_script_hash: contracts.unionFieldGrammar.spendingScriptHash,
    };
    for (const [index, step] of plan.grammarSteps.entries()) {
      const next = step.complete
        ? field === 2
          ? raw(fieldCursor, S.ConservationFieldCursor)
          : raw(
              { field: fieldCursor, previous_policy: null, active: null },
              S.ConservationMintCursor,
            )
        : raw(
            { field_index: BigInt(field), checkpoint_hash: step.afterHash },
            S.ConservationFieldGrammar,
          );
      append(
        step.complete
          ? field === 2
            ? "unionOutputs"
            : "unionMint"
          : "unionFieldGrammar",
        fold(next),
        encoded(
          {
            ...layout,
            opening: opening(field),
            checkpoint_bytes: index === 0 ? "" : step.before,
          },
          S.ConservationFieldGrammarArgs,
        ),
        field,
      );
    }
    return plan;
  };
  const outputPlan = grammar(2);
  const fieldBytes = Buffer.from(fields[2], "hex");
  const chunks = Array.from(
    { length: Math.ceil(fieldBytes.length / 15148) },
    (_, i) => fieldBytes.subarray(i * 15148, (i + 1) * 15148).toString("hex"),
  );
  for (const [index, extent] of outputPlan.extents.entries()) {
    const leaves = flattenMidgardValueAssets(
      decodeMidgardTxOutput(extent.item).value,
    );
    const material = buildMidgardLedgerOutputAssetFrontier(leaves);
    const item: S.ConservationOutputItem = {
      field_total_length: BigInt(fieldBytes.length),
      field_chunk_hashes: chunks.map((c) =>
        computeHash32(Buffer.from(c, "hex")).toString("hex"),
      ),
      index: BigInt(index),
      offset: BigInt(extent.offset),
      length: BigInt(extent.length),
      checkpoint_hash: extent.beforeHash,
      next_checkpoint_hash: extent.afterHash,
      selector_hash: contracts.unionOutputs.spendingScriptHash,
      grammar_script_hash: contracts.unionFieldGrammar.spendingScriptHash,
    };
    let control = initialMidgardLedgerOutputScanControl();
    append(
      "unionOutputScan",
      fold(
        raw(
          { item, control: transactionOutputScanControlData(control) },
          S.ConservationOutputScan,
        ),
      ),
      encoded(
        { ...layout, opening: opening(2), checkpoint_bytes: extent.before },
        S.ConservationOutputsArgs,
      ),
      2,
    );
    while (
      !isExactMidgardLedgerOutputScanTerminal({
        control,
        totalLength: extent.length,
      })
    ) {
      for (
        let i = 0;
        i < 4 &&
        !isExactMidgardLedgerOutputScanTerminal({
          control,
          totalLength: extent.length,
        });
        i++
      ) {
        const next =
          finishMidgardLedgerOutputScan({
            control,
            totalLength: extent.length,
          }) ??
          advanceMidgardLedgerOutputScan({
            control,
            totalLength: extent.length,
            window: extent.item.subarray(control.cursor, control.cursor + 132),
            windowOffset: 0,
          });
        if (next === null)
          throw new Error("value conservation: noncanonical output");
        control = next;
      }
      const complete = isExactMidgardLedgerOutputScanTerminal({
        control,
        totalLength: extent.length,
      });
      const next = raw(
        {
          field_index: 2n,
          checkpoint_hash: extent.afterHash,
          grammar_script_hash: contracts.unionFieldGrammar.spendingScriptHash,
        },
        S.ConservationFieldCursor,
      );
      if (complete)
        balance = {
          ...balance,
          lovelace_delta: balance.lovelace_delta - control.lovelace,
        };
      const continuation = complete
        ? raw(
            {
              count: BigInt(leaves.length),
              frontier_commitment: Buffer.from(material.commitment).toString(
                "hex",
              ),
              cursor: 0n,
              quantity_sign: -1n,
              next_script_hash: contracts.unionOutputs.spendingScriptHash,
              next_continuation: next,
            },
            S.ConservationAssetCursor,
          )
        : raw(
            { item, control: transactionOutputScanControlData(control) },
            S.ConservationOutputScan,
          );
      append(
        complete ? "unionAssets" : "unionOutputScan",
        fold(continuation),
        encoded(
          { ...layout, carriage: { InlineChunks: { chunks } }, budget: 4n },
          S.ConservationOutputScanArgs,
        ),
      );
      if (complete) await assets(leaves, -1n, "unionOutputs", next);
    }
  }
  append(
    "unionFieldGrammar",
    fold(
      raw(
        { field_index: 5n, checkpoint_hash: null },
        S.ConservationFieldGrammar,
      ),
    ),
    encoded(
      {
        ...layout,
        opening: opening(2),
        checkpoint_bytes: outputPlan.terminalWalk,
      },
      S.ConservationOutputsArgs,
    ),
    2,
  );
  const mintPlan = grammar(5);
  let previousPolicy: string | null = null;
  for (const extent of mintPlan.extents) {
    const policy = decodeMidgardMintPolicyItem(extent.item);
    const policyId = Buffer.from(policy.policyId).toString("hex");
    const field = {
      field_index: 5n,
      checkpoint_hash: extent.beforeHash,
      grammar_script_hash: contracts.unionFieldGrammar.spendingScriptHash,
    };
    // The canonical array and policy bytes precede a canonical map header.
    let cursor =
      1 +
      encodeMidgardDefiniteBytes(policy.policyId).length +
      (policy.assets.length < 24 ? 1 : policy.assets.length <= 255 ? 2 : 3);
    let active: S.ConservationMintPolicy = {
      offset: BigInt(extent.offset),
      length: BigInt(extent.length),
      next_checkpoint_hash: extent.afterHash,
      policy_id: policyId,
      cursor: BigInt(cursor),
      remaining: BigInt(policy.assets.length),
      previous_asset: null,
    };
    append(
      "unionMint",
      fold(
        raw(
          { field, previous_policy: previousPolicy, active },
          S.ConservationMintCursor,
        ),
      ),
      encoded(
        { ...layout, opening: opening(5), checkpoint_bytes: extent.before },
        S.ConservationMintArgs,
      ),
      5,
    );
    for (const [index, asset] of policy.assets.entries()) {
      const assetName = Buffer.from(asset.assetName).toString("hex");
      cursor +=
        encodeMidgardDefiniteBytes(asset.assetName).length +
        encodeCbor(asset.quantity).length;
      active = {
        ...active,
        cursor: BigInt(cursor),
        remaining: BigInt(policy.assets.length - index - 1),
        previous_asset: assetName,
      };
      const continuation = raw(
        { field, previous_policy: previousPolicy, active },
        S.ConservationMintCursor,
      );
      const unit = policyId + assetName;
      append(
        "unionUpdate",
        fold(
          raw(
            {
              unit,
              quantity: asset.quantity,
              return_script_hash: contracts.unionMint.spendingScriptHash,
              next_continuation: continuation,
            },
            S.ConservationPending,
          ),
        ),
        encoded(
          { ...layout, opening: opening(5), checkpoint_bytes: extent.before },
          S.ConservationMintArgs,
        ),
        5,
      );
      await contribute(unit, asset.quantity, "unionMint", continuation);
    }
    previousPolicy = policyId;
    append(
      "unionMint",
      fold(
        raw(
          {
            field: { ...field, checkpoint_hash: extent.afterHash },
            previous_policy: previousPolicy,
            active: null,
          },
          S.ConservationMintCursor,
        ),
      ),
      encoded(
        { ...layout, opening: opening(5), checkpoint_bytes: extent.before },
        S.ConservationMintArgs,
      ),
      5,
    );
  }
  append(
    "unionTerminal",
    fold(new Constr(0, [])),
    encoded(
      {
        ...layout,
        opening: opening(5),
        checkpoint_bytes: mintPlan.terminalWalk,
      },
      S.ConservationMintArgs,
    ),
    5,
  );
  let witness: S.ConservationDeltaWitness | null = null;
  if (source.claim === "ForcedConservation") {
    if (balance.lovelace_delta !== 0n || deltas.size !== 0)
      throw new Error("value conservation: honest rejection");
  } else {
    const { asset, direction } = source.claim.AcceptedImbalance;
    let delta = balance.lovelace_delta;
    if (asset !== "AdaAsset") {
      const unit = asset.TokenAsset.policy_id + asset.TokenAsset.asset_name;
      delta = deltas.get(unit) ?? 0n;
      if (delta !== 0n)
        witness = {
          delta,
          proof: Data.from(
            (await trie.prove(Buffer.from(unit, "hex")))
              .toCBOR()
              .toString("hex"),
            Proof,
          ),
        };
    }
    if (direction === "ClaimedAssetInflated" ? delta >= 0n : delta <= 0n)
      throw new Error("value conservation: claimed imbalance is absent");
  }
  append(
    null,
    null,
    encoded(
      { ...layout, fraud_proof_mint_redeemer_index: 0n, witness },
      S.ConservationTerminalArgs,
    ),
  );
  return { actions, finalBalance: balance };
};
