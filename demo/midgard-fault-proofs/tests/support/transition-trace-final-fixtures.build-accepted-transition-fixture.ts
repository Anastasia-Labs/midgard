import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardMpfDeletionOpening,
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardSpendInputItem,
  parseMidgardMpfProofJson,
} from "@al-ft/midgard-core";
import {
  deriveMidgardForcedTxProofSource,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  commitCountedRoot,
} from "../../src/transition-trace/phas.js";
import { type TransitionProofMaterial } from "../../src/transition-trace/proof-material.js";
import { makeHeader } from "./emulator/header-fixtures.js";
import { makeNativeTx } from "./emulator/native-tx.js";
import {
  syntheticDeepInsertProof,
  syntheticDeepMembershipProof,
} from "./synthetic-deep-proof.js";

/**
 * One L2 transaction over an otherwise empty ledger. By default it spends
 * nothing and produces `outputCbors`. With `drained`, the ledger instead holds
 * one entry (the first of `outputCbors`) that the transaction spends while
 * producing nothing, so the honest post root is the committed empty-ledger
 * root and the dishonest one is the MPF library's null root. With `forced`, the
 * same transaction is committed instead as a forced leaf carrying that
 * verdict, and the witness is the accepted forced transition.
 */
export const buildAcceptedTransitionFixture = async ({
  operatorVkey,
  now,
  honest = false,
  depth = 0,
  drained = false,
  spentCount = 0,
  ledgerFill = 0,
  ledgerDepth = 0,
  forced,
  outputCbors = [
    Buffer.from("a200581d60" + "aa".repeat(28) + "01821a001e8480a0", "hex"),
  ],
}: {
  operatorVkey: string;
  now: number;
  honest?: boolean;
  depth?: number;
  drained?: boolean;
  /** Spend this many ledger entries (each holding the first output) as well. */
  spentCount?: number;
  /** Unrelated ledger entries, so MPF proofs have a realistic depth. */
  ledgerFill?: number;
  /**
   * Insert the single output at this MPF depth (64 is the deepest a path
   * allows) into a synthesised ledger, in place of an otherwise empty one.
   */
  ledgerDepth?: number;
  /** Commit the transaction as a forced leaf with this verdict. */
  forced?: "valid" | "invalid";
  outputCbors?: Buffer[];
}) => {
  if (
    ledgerDepth > 0 &&
    (drained || spentCount > 0 || ledgerFill > 0 || outputCbors.length !== 1)
  )
    throw new Error("ledgerDepth needs exactly one output and nothing spent.");
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
  const extraSpent = Array.from({ length: spentCount }, (_, outputIndex) => ({
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0xed),
      outputIndex,
    }),
    value: Buffer.from(
      buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex,
        outputCbor: outputCbors[0]!,
      }).descriptorCbor,
    ),
  }));
  const fill = Array.from({ length: ledgerFill }, (_, index) => ({
    key: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0xec),
      outputIndex: index,
    }),
    value: Buffer.alloc(32, 0x01),
  }));
  const spendInputCbors = [
    ...(drainedEntry === null ? [] : [drainedEntry.outRef]),
    ...extraSpent.map(({ outRef }) => outRef),
  ];
  const native = makeNativeTx({
    spendInputCbors,
    fee: drained ? 2_000_000n : 0n,
    outputCbors: txOutputCbors,
  });
  const txId = computeMidgardNativeTxId(native).toString("hex");
  const source =
    forced === undefined
      ? deriveMidgardNativeTxProofSource(native)
      : deriveMidgardForcedTxProofSource(
          materializeMidgardForcedTxFromCanonical(native),
        );
  const proofSource = {
    compact_cbor: source.compactCbor.toString("hex"),
    witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
    field_preimage_lengths_cbor:
      source.fieldPreimageLengthsCbor.toString("hex"),
  };
  const txOrderId: SDK.OutputReference = {
    transactionId: "f1".repeat(32),
    outputIndex: 0n,
  };
  const sourceKey =
    forced === undefined ? txId : Data.to(txOrderId, SDK.OutputReference);
  const encodedSource =
    forced === undefined
      ? Data.to({ tx_id: txId, source: proofSource }, SDK.L2TransactionSource)
      : Data.to(
          {
            tx_id: txId,
            submitted_source: proofSource,
            verdict:
              forced === "valid"
                ? ("ForcedTxValid" as const)
                : {
                    ForcedTxInvalid: {
                      reason: {
                        PlutusExecutionFailed: {
                          purpose_kind: 0n,
                          purpose_index: 0n,
                        },
                      },
                    },
                  },
          },
          SDK.ForcedInclusionTxV1,
        );
  const ledger = await Trie.fromList([
    ...(drainedEntry === null
      ? []
      : [{ key: drainedEntry.outRef, value: drainedValue! }]),
    ...extraSpent.map(({ outRef, value }) => ({ key: outRef, value })),
    ...fill,
  ]);
  let preUtxosRoot =
    ledger.hash === null
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
  for (const { outRef, value } of extraSpent) {
    const proved = await ledger.prove(outRef);
    const opening = await buildMidgardMpfDeletionOpening(
      ledger,
      outRef,
      parseMidgardMpfProofJson(proved.toJSON()),
    );
    spent.push({
      key: outRef.toString("hex"),
      value: value.toString("hex"),
      opening: opening.toString("hex"),
      delete_proof: Data.from(proved.toCBOR().toString("hex"), SDK.Proof),
    });
    await ledger.delete(outRef);
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
    const deepInsert =
      ledgerDepth === 0
        ? null
        : syntheticDeepInsertProof({ key, value, branchLevels: ledgerDepth });
    if (deepInsert === null) await ledger.insert(key, value);
    else preUtxosRoot = deepInsert.preRoot;
    const proof = Data.from(
      deepInsert?.proofCbor ??
        (await ledger.prove(key)).toCBOR().toString("hex"),
      SDK.Proof,
    );
    produced.push({
      key: key.toString("hex"),
      value: value.toString("hex"),
      insert_proof: proof,
    });
  }
  const dishonest = drained ? "00".repeat(32) : "bb".repeat(32);
  const step: SDK.TransitionStep = {
    schema_version: 1n,
    step_index: 0n,
    event_key:
      forced === undefined
        ? { L2TransactionEventKey: { tx_id: txId } }
        : { ForcedTransactionEventKey: { tx_order_id: txOrderId } },
    phase: forced === undefined ? "L2Transaction" : "ForcedTransaction",
    pre_utxos_root: preUtxosRoot,
    post_utxos_root: !honest
      ? dishonest
      : ledgerDepth > 0
        ? syntheticDeepInsertProof({
            key: Buffer.from(produced[0]!.key, "hex"),
            value: Buffer.from(produced[0]!.value, "hex"),
            branchLevels: ledgerDepth,
          }).postRoot
        : ledger.hash === null
          ? SDK.EMPTY_MERKLE_TREE_ROOT
          : ledger.hash.toString("hex"),
  };
  const event = { step_index: 0n, phase: step.phase };
  const roots = await Promise.all([
    buildCountedRoot(
      forced === undefined
        ? SDK.ROOT_DOMAINS.transactionsV1
        : SDK.ROOT_DOMAINS.forcedTransactionsV1,
      [
        {
          key: Buffer.from(sourceKey, "hex"),
          value: Buffer.from(encodedSource, "hex"),
        },
      ],
    ),
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
          key: Buffer.from(sourceKey, "hex"),
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
    ...(forced === undefined
      ? { transactionsRoot: transactions!.root, l2TransactionCount: 1n }
      : {
          forcedTransactionsRoot: transactions!.root,
          forcedTransactionCount: 1n,
        }),
    transitionTraceRoot: trace!.root,
    eventToStepRoot: mapping!.root,
    totalEventCount: 1n,
    transitionStepCount: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const rootFields = (root: typeof transactions) => ({
    domain: root!.domain,
    root: root!.root,
    phas_root: root!.phasRoot,
    count: root!.count,
    proof: [],
  });
  const transition = {
    trace_proof: { ...rootFields(trace), key: 0n, value: step },
    event_to_step: {
      ...rootFields(mapping),
      key: step.event_key,
      value: event,
    },
    source_membership: {
      ...rootFields(transactions),
      key: sourceKey,
      value: encodedSource,
      proof: deep === null ? [] : Data.from(deep.proofCbor, SDK.Proof),
    },
  };
  const proof: SDK.TransitionFaultProof = {
    challenged_header_hash: headerHash,
    header,
    fault: {
      InvalidOneStepTransition: {
        witness:
          forced === undefined
            ? { L2TransactionTransition: transition }
            : { ValidForcedTransactionTransition: transition },
      },
    },
  };
  const material: TransitionProofMaterial = {
    proofCbor: Data.to(proof, SDK.TransitionFaultProof),
    l2Replay: {
      spendInputCbors: spendInputCbors.map((item) => item.toString("hex")),
      outputCbors: txOutputCbors.map((item) => item.toString("hex")),
      spentUtxos: spent,
      producedUtxos: produced,
    },
  };
  return { header, headerHash, proof: material, drainedEntry };
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
              value: Data.to(info, SDK.DepositInfo),
              proof: deep === null ? [] : Data.from(deep.proofCbor, SDK.Proof),
            },
            projected_utxo: {
              key: key.toString("hex"),
              value: value.toString("hex"),
              insert_proof: insert,
            },
          },
        },
      },
    },
  };
  return { header, headerHash, proof };
};
