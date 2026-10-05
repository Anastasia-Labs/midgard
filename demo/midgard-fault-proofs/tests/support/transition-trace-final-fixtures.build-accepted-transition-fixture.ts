import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  commitCountedRoot,
} from "../../src/transition-trace/phas.js";
import { makeHeader } from "./emulator/header-fixtures.js";
import { makeNativeTx } from "./emulator/native-tx.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

/**
 * One L2 transaction over an otherwise empty ledger. By default it spends
 * nothing and produces `outputCbors`. With `drained`, the ledger instead holds
 * one entry (the first of `outputCbors`) that the transaction spends while
 * producing nothing, so the honest post root is the committed empty-ledger
 * root and the dishonest one is the MPF library's null root.
 */
export const buildAcceptedTransitionFixture = async ({
  operatorVkey,
  now,
  honest = false,
  depth = 0,
  drained = false,
  outputCbors = [
    Buffer.from("a200581d60" + "aa".repeat(28) + "01821a001e8480a0", "hex"),
  ],
}: {
  operatorVkey: string;
  now: number;
  honest?: boolean;
  depth?: number;
  drained?: boolean;
  outputCbors?: Buffer[];
}) => {
  const drainedEntry = drained
    ? {
        outRef: encodeMidgardSpendInputItem({
          txId: Buffer.alloc(32, 0xee),
          outputIndex: 0,
        }),
        output: outputCbors[0]!,
      }
    : null;
  const drainedValue =
    drainedEntry === null
      ? null
      : Buffer.from(
          buildCanonicalMidgardLedgerOutputMaterial({
            outputIndex: 0,
            outputCbor: drainedEntry.output,
          }).descriptorCbor,
        );
  const txOutputCbors = drained ? [] : outputCbors;
  const native = makeNativeTx({
    spendInputCbors: drainedEntry === null ? [] : [drainedEntry.outRef],
    fee: drained ? 2_000_000n : 0n,
    outputCbors: txOutputCbors,
  });
  const txId = computeMidgardNativeTxId(native).toString("hex");
  const source = deriveMidgardNativeTxProofSource(native);
  const encodedSource = Data.to(
    {
      tx_id: txId,
      source: {
        compact_cbor: source.compactCbor.toString("hex"),
        witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          source.fieldPreimageLengthsCbor.toString("hex"),
      },
    },
    SDK.L2TransactionSource,
  );
  const ledger = await Trie.fromList(
    drainedEntry === null
      ? []
      : [{ key: drainedEntry.outRef, value: drainedValue! }],
  );
  const preUtxosRoot =
    drainedEntry === null
      ? SDK.EMPTY_MERKLE_TREE_ROOT
      : ledger.hash.toString("hex");
  const spent: SDK.LedgerDeleteWitness[] = [];
  if (drainedEntry !== null) {
    spent.push({
      key: drainedEntry.outRef.toString("hex"),
      value: drainedValue!.toString("hex"),
      opening: "",
      delete_proof: Data.from(
        (await ledger.prove(drainedEntry.outRef)).toCBOR().toString("hex"),
        SDK.Proof,
      ),
    });
    await ledger.delete(drainedEntry.outRef);
  }
  const produced: SDK.LedgerInsertWitness[] = [];
  for (const [index, bytes] of txOutputCbors.entries()) {
    const key = encodeMidgardSpendInputItem({
      txId: Buffer.from(txId, "hex"),
      outputIndex: index,
    });
    const value = Buffer.from(
      buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: index,
        outputCbor: bytes,
      }).descriptorCbor,
    );
    await ledger.insert(key, value);
    const proof = Data.from(
      (await ledger.prove(key)).toCBOR().toString("hex"),
      SDK.Proof,
    );
    produced.push({
      key: key.toString("hex"),
      value: value.toString("hex"),
      non_membership_proof: proof,
      insert_proof: proof,
    });
  }
  const dishonest = drained ? "00".repeat(32) : "bb".repeat(32);
  const step: SDK.TransitionStep = {
    schema_version: 1n,
    step_index: 0n,
    event_key: { L2TransactionEventKey: { tx_id: txId } },
    phase: "L2Transaction",
    pre_utxos_root: preUtxosRoot,
    post_utxos_root: !honest
      ? dishonest
      : ledger.hash === null
        ? SDK.EMPTY_MERKLE_TREE_ROOT
        : ledger.hash.toString("hex"),
  };
  const event = { step_index: 0n, phase: "L2Transaction" as const };
  const roots = await Promise.all([
    buildCountedRoot(SDK.ROOT_DOMAINS.transactionsV1, [
      {
        key: Buffer.from(txId, "hex"),
        value: Buffer.from(encodedSource, "hex"),
      },
    ]),
    buildCountedRoot(SDK.ROOT_DOMAINS.transitionTrace, [
      {
        key: Buffer.from(Data.to(0n), "hex"),
        value: Buffer.from(Data.to(step, SDK.TransitionStep), "hex"),
      },
    ]),
    buildCountedRoot(SDK.ROOT_DOMAINS.eventToStep, [
      {
        key: Buffer.from(Data.to(step.event_key, SDK.EventKey), "hex"),
        value: Buffer.from(Data.to(event, SDK.EventToStepValue), "hex"),
      },
    ]),
  ]);
  let transactions = roots[0];
  const [, trace, mapping] = roots;
  const deep =
    depth === 0
      ? null
      : syntheticDeepMembershipProof({
          key: Buffer.from(txId, "hex"),
          value: Buffer.from(encodedSource, "hex"),
          branchLevels: depth,
        });
  if (deep !== null)
    transactions = {
      ...transactions!,
      phasRoot: deep.transactionsPhasRoot,
      root: await commitCountedRoot({
        domain: transactions!.domain,
        count: transactions!.count,
        phasRoot: deep.transactionsPhasRoot,
      }),
    };
  const header: SDK.Header = {
    ...makeHeader(operatorVkey, now),
    utxosRoot: step.post_utxos_root,
    transactionsRoot: transactions!.root,
    transitionTraceRoot: trace!.root,
    eventToStepRoot: mapping!.root,
    validationTracesRoot: "cc".repeat(32),
    l2TransactionCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const rootFields = (root: typeof transactions) => ({
    domain: root!.domain,
    root: root!.root,
    phas_root: root!.phasRoot,
    count: root!.count,
    proof: [],
  });
  const proof: SDK.TransitionFaultProof = {
    challenged_header_hash: headerHash,
    header,
    fault: {
      InvalidOneStepTransition: {
        witness: {
          L2TransactionTransition: {
            trace_proof: { ...rootFields(trace), key: 0n, value: step },
            event_to_step: {
              ...rootFields(mapping),
              key: step.event_key,
              value: event,
            },
            source_membership: {
              ...rootFields(transactions),
              key: txId,
              value: encodedSource,
              proof: deep === null ? [] : Data.from(deep.proofCbor, SDK.Proof),
            },
            spend_inputs_preimage: encodeMidgardFieldPreimage(
              drainedEntry === null ? [] : [drainedEntry.outRef],
            ).toString("hex"),
            outputs_preimage:
              encodeMidgardFieldPreimage(txOutputCbors).toString("hex"),
            spent_utxos: spent,
            produced_utxos: produced,
          },
        },
      },
    },
  };
  return { header, headerHash, proof, drainedEntry };
};

export const buildDepositTransitionFixture = async ({
  operatorVkey,
  now,
  id,
  info,
  outputCbor,
  honest = false,
  depth = 0,
}: {
  operatorVkey: string;
  now: number;
  id: SDK.OutputReference;
  info: SDK.DepositInfo;
  outputCbor: Buffer;
  honest?: boolean;
  depth?: number;
}) => {
  const key = encodeMidgardSpendInputItem({
    txId: Buffer.from(id.transactionId, "hex"),
    outputIndex: Number(id.outputIndex),
  });
  const value = Buffer.from(
    buildCanonicalMidgardLedgerOutputMaterial({
      outputIndex: Number(id.outputIndex),
      outputCbor,
    }).descriptorCbor,
  );
  const ledger = await Trie.fromList([{ key, value }]);
  const insert = Data.from(
    (await ledger.prove(key)).toCBOR().toString("hex"),
    SDK.Proof,
  );
  const step: SDK.TransitionStep = {
    schema_version: 1n,
    step_index: 0n,
    event_key: { DepositEventKey: { deposit_id: id } },
    phase: "Deposit",
    pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    post_utxos_root: honest ? ledger.hash.toString("hex") : "bb".repeat(32),
  };
  const event = { step_index: 0n, phase: "Deposit" as const };
  const [originalSource, trace, mapping] = await Promise.all([
    buildCountedRoot(SDK.ROOT_DOMAINS.deposits, [
      {
        key: Buffer.from(Data.to(id, SDK.OutputReference), "hex"),
        value: Buffer.from(Data.to(info, SDK.DepositInfo), "hex"),
      },
    ]),
    buildCountedRoot(SDK.ROOT_DOMAINS.transitionTrace, [
      {
        key: Buffer.from(Data.to(0n), "hex"),
        value: Buffer.from(Data.to(step, SDK.TransitionStep), "hex"),
      },
    ]),
    buildCountedRoot(SDK.ROOT_DOMAINS.eventToStep, [
      {
        key: Buffer.from(Data.to(step.event_key, SDK.EventKey), "hex"),
        value: Buffer.from(Data.to(event, SDK.EventToStepValue), "hex"),
      },
    ]),
  ]);
  const deep =
    depth === 0
      ? null
      : syntheticDeepMembershipProof({
          key: Buffer.from(Data.to(id, SDK.OutputReference), "hex"),
          value: Buffer.from(Data.to(info, SDK.DepositInfo), "hex"),
          branchLevels: depth,
        });
  const source =
    deep === null
      ? originalSource
      : {
          ...originalSource,
          phasRoot: deep.transactionsPhasRoot,
          root: await commitCountedRoot({
            domain: originalSource.domain,
            count: originalSource.count,
            phasRoot: deep.transactionsPhasRoot,
          }),
        };
  const header: SDK.Header = {
    ...makeHeader(operatorVkey, now),
    utxosRoot: step.post_utxos_root,
    depositsRoot: source.root,
    transitionTraceRoot: trace.root,
    eventToStepRoot: mapping.root,
    depositCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const fields = (root: typeof source) => ({
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    proof: [],
  });
  const proof: SDK.TransitionFaultProof = {
    challenged_header_hash: headerHash,
    header,
    fault: {
      InvalidOneStepTransition: {
        witness: {
          ValidDepositTransition: {
            trace_proof: { ...fields(trace), key: 0n, value: step },
            event_to_step: {
              ...fields(mapping),
              key: step.event_key,
              value: event,
            },
            source_membership: {
              ...fields(source),
              key: id,
              value: info,
              proof: deep === null ? [] : Data.from(deep.proofCbor, SDK.Proof),
            },
            projected_utxo: {
              key: key.toString("hex"),
              value: value.toString("hex"),
              non_membership_proof: insert,
              insert_proof: insert,
            },
          },
        },
      },
    },
  };
  return { header, headerHash, proof };
};
