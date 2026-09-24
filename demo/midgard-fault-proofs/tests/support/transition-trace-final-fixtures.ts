import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxProofCommitment,
  hashMidgardValidationContext,
  hashMidgardValidationEventKey,
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  type MidgardValidationMachineState,
} from "@al-ft/midgard-core";
import {
  computeMidgardNativeTxId,
  deriveMidgardNativeTxProofSource,
  encodeMidgardFieldPreimage,
  encodeMidgardSpendInputItem,
} from "@al-ft/midgard-core";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import { ensureHash32 } from "@al-ft/midgard-core/codec/hash";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  encodeValidationTerminalWitnessCbor,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildCountedRoot,
  commitCountedRoot,
} from "../../src/transition-trace/phas.js";
import { makeHeader } from "./emulator/header-fixtures.js";
import { makeNativeTx } from "./emulator/native-tx.js";
import { syntheticDeepMembershipProof } from "./synthetic-deep-proof.js";

export const buildAcceptedTransitionFixture = async ({
  operatorVkey,
  now,
  honest = false,
  depth = 0,
  outputCbors = [
    Buffer.from("a200581d60" + "aa".repeat(28) + "01821a001e8480a0", "hex"),
  ],
}: {
  operatorVkey: string;
  now: number;
  honest?: boolean;
  depth?: number;
  outputCbors?: Buffer[];
}) => {
  const native = makeNativeTx({ spendInputCbors: [], fee: 0n, outputCbors });
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
  const ledger = await Trie.fromList([]);
  const produced: SDK.LedgerInsertWitness[] = [];
  for (const [index, bytes] of outputCbors.entries()) {
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
  const actual = ledger.hash.toString("hex");
  const step: SDK.TransitionStep = {
    schema_version: 1n,
    step_index: 0n,
    event_key: { L2TransactionEventKey: { tx_id: txId } },
    phase: "L2Transaction",
    pre_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
    post_utxos_root: honest ? actual : "bb".repeat(32),
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
            spend_inputs_preimage: encodeMidgardFieldPreimage([]).toString(
              "hex",
            ),
            outputs_preimage:
              encodeMidgardFieldPreimage(outputCbors).toString("hex"),
            spent_utxos: [],
            produced_utxos: produced,
          },
        },
      },
    },
  };
  return { header, headerHash, proof };
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

export const buildAcceptedClaimTransitionFixture = async ({
  operatorVkey,
  now,
  honest = false,
  endTime,
  blockSlot,
}: {
  operatorVkey: string;
  now: number;
  honest?: boolean;
  endTime?: bigint;
  blockSlot?: bigint;
}) => {
  const base = await buildAcceptedTransitionFixture({
    operatorVkey,
    now,
    honest,
  });
  if (
    !("InvalidOneStepTransition" in base.proof.fault) ||
    !(
      "L2TransactionTransition" in
      base.proof.fault.InvalidOneStepTransition.witness
    )
  )
    throw new Error("Expected accepted fixture");
  const opening =
    base.proof.fault.InvalidOneStepTransition.witness.L2TransactionTransition;
  const nativeSource = Data.from(
    opening.source_membership.value,
    SDK.L2TransactionSource,
  );
  const source = nativeSource.source;
  const header = {
    ...base.header,
    ...(endTime === undefined ? {} : { endTime }),
    ...(blockSlot === undefined ? {} : { blockSlot }),
  };
  const context = Buffer.from(
    Data.to([
      1n,
      Buffer.from("midgard-consensus-v1").toString("hex"),
      header.endTime,
      header.expectedNetworkId,
      header.minFeeA,
      header.minFeeB,
      header.blockSlot,
    ]),
    "hex",
  );
  const initialWitness = encodeCbor([
    Buffer.from(source.compact_cbor, "hex"),
    Buffer.from(source.witness_set_compact_cbor, "hex"),
    Buffer.from(source.field_preimage_lengths_cbor, "hex"),
    context,
    0n,
    0n,
    0n,
    -1n,
    0n,
  ]);
  const root = Buffer.from(
    honest ? opening.trace_proof.value.post_utxos_root : "cc".repeat(32),
    "hex",
  );
  const terminalWitness = encodeValidationTerminalWitnessCbor({
    verdict: "accepted",
    postLedgerRoot: root,
    ledgerDeltaFrontier: { count: 0, peaks: [] },
  });
  const initial: MidgardValidationMachineState = {
    machineVersion: 1,
    eventKeyHash: hashMidgardValidationEventKey(
      Buffer.from(
        Data.to(opening.trace_proof.value.event_key, SDK.EventKey),
        "hex",
      ),
    ),
    transactionId: ensureHash32(Buffer.from(nativeSource.tx_id, "hex"), "tx"),
    transactionCommitment: ensureHash32(
      computeMidgardNativeTxProofCommitment({
        compactCbor: Buffer.from(source.compact_cbor, "hex"),
        witnessSetCompactCbor: Buffer.from(
          source.witness_set_compact_cbor,
          "hex",
        ),
        fieldPreimageLengthsCbor: Buffer.from(
          source.field_preimage_lengths_cbor,
          "hex",
        ),
      }),
      "source",
    ),
    validationContextHash: hashMidgardValidationContext(context),
    sourceKind: "normal",
    priorLedgerRoot: ensureHash32(
      Buffer.from(opening.trace_proof.value.pre_utxos_root, "hex"),
      "prior",
    ),
    phase: "canonicalDecode",
    programCounter: 0,
    workRoot: hashMidgardValidationWorkWitness({
      phase: "canonicalDecode",
      programCounter: 0,
      witnessCbor: initialWitness,
    }),
    executionCpu: 0n,
    executionMemory: 0n,
    verdict: "pending",
    rejectionCodeHash: ensureHash32(Buffer.alloc(32), "reject"),
    ledgerDeltaRoot: ensureHash32(Buffer.alloc(32), "delta"),
  };
  const terminal: MidgardValidationMachineState = {
    ...initial,
    phase: "terminal",
    programCounter: 1,
    workRoot: hashMidgardValidationWorkWitness({
      phase: "terminal",
      programCounter: 1,
      witnessCbor: terminalWitness,
    }),
    verdict: "accepted",
  };
  const tree = buildMidgardValidationTraceTree(
    [
      hashMidgardValidationMachineState(initial),
      hashMidgardValidationMachineState(terminal),
    ],
    "accepted",
  );
  const hex = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");
  const state = (
    value: MidgardValidationMachineState,
  ): SDK.ValidationMachineState => ({
    machine_version: 1n,
    event_key_hash: hex(value.eventKeyHash),
    transaction_id: hex(value.transactionId),
    transaction_commitment: hex(value.transactionCommitment),
    validation_context_hash: hex(value.validationContextHash),
    source_kind: "Normal",
    prior_ledger_root: hex(value.priorLedgerRoot),
    phase: value.phase === "terminal" ? "Terminal" : "CanonicalDecode",
    program_counter: BigInt(value.programCounter),
    work_root: hex(value.workRoot),
    execution_cpu: 0n,
    execution_memory: 0n,
    verdict: value.verdict === "accepted" ? "Accepted" : "Pending",
    rejection_code_hash: hex(value.rejectionCodeHash),
    ledger_delta_root: hex(value.ledgerDeltaRoot),
  });
  const descriptor: SDK.ValidationTraceDescriptor = {
    schema_version: 1n,
    machine_version: 1n,
    trace_root: hex(tree.descriptor.traceRoot),
    step_count: 1n,
    initial_state_hash: hex(tree.stateHashes[0]!),
    terminal_state_hash: hex(tree.stateHashes[1]!),
    verdict: "Accepted",
    rejection_code_hash: "00".repeat(32),
  };
  const descriptors = await buildCountedRoot(
    SDK.ROOT_DOMAINS.validationTraces,
    [
      {
        key: Buffer.from(
          Data.to(opening.trace_proof.value.event_key, SDK.EventKey),
          "hex",
        ),
        value: Buffer.from(
          Data.to(descriptor, SDK.ValidationTraceDescriptor),
          "hex",
        ),
      },
    ],
  );
  const claim: SDK.ValidationClaimWitness = {
    version: 1n,
    descriptor_membership: {
      domain: descriptors.domain,
      root: descriptors.root,
      phas_root: descriptors.phasRoot,
      count: descriptors.count,
      proof: [],
      key: opening.trace_proof.value.event_key,
      value: descriptor,
    },
    transition_step_membership: opening.trace_proof,
    event_to_step_membership: opening.event_to_step,
    source_membership: {
      NormalValidationSource: {
        membership: { ...opening.source_membership, value: nativeSource },
      },
    },
    validation_context_cbor: hex(context),
    initial_state: state(initial),
    terminal_state: state(terminal),
    initial_state_proof: {
      state_index: 0n,
      state_hash: hex(tree.stateHashes[0]!),
      siblings: tree.proofs[0]!.siblings.map(hex),
    },
    terminal_state_proof: {
      state_index: 1n,
      state_hash: hex(tree.stateHashes[1]!),
      siblings: tree.proofs[1]!.siblings.map(hex),
    },
  };
  const resultHeader = {
    ...header,
    validationTracesRoot: descriptors.root,
    validationTraceCount: 1n,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(resultHeader));
  const proof: SDK.TransitionFaultProof = {
    challenged_header_hash: headerHash,
    header: resultHeader,
    fault: {
      AcceptedTransactionTransitionMismatch: {
        witness: {
          claim,
          terminal_acceptance_witness_cbor: hex(terminalWitness),
        },
      },
    },
  };
  return { header: resultHeader, headerHash, proof };
};
