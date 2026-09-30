import {
  buildMidgardValidationTraceTree,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardValidationLedgerDelta,
  hashMidgardValidationMachineState,
} from "@al-ft/midgard-core";
import {
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  Header,
  OutputReference,
  ROOT_DOMAINS,
  TransitionStepSchema,
  type ValidationClaimWitness,
  validationMachineStateDataFromCore,
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorSchema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildValidationMachineLedgerInsertOp,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
} from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import { buildCountedRoot, keyValuePhasProof } from "../../../src/index.js";
import { makeHeader, transitionTraceDaEntry } from "./header-fixtures.js";

export type ForcedValidationSourceEntry = NonNullable<
  ValidationClaimWitness["source_membership"] extends infer Source
    ? Source extends {
        ForcedValidationSource: { membership: { value: infer V } };
      }
      ? V
      : never
    : never
>;

/**
 * Builds the block-owned roots, forced-source membership, committed claim, and
 * header for a single-forced-transaction validation-trace block from an
 * operator trace. Shared by every forced validation-dispute fixture so the
 * committed-claim shape stays identical across scenarios and only the traces
 * and transition roots vary.
 */
export const buildForcedValidationDisputeCommitments = async ({
  operatorVkey,
  now,
  txOrderId,
  eventKey,
  forcedTransaction,
  operatorTrace,
  preUtxosRoot,
  postUtxosRoot,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly txOrderId: OutputReference;
  readonly eventKey: {
    readonly ForcedTransactionEventKey: {
      readonly tx_order_id: OutputReference;
    };
  };
  readonly forcedTransaction: ForcedValidationSourceEntry;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly preUtxosRoot: string;
  readonly postUtxosRoot: string;
}): Promise<{
  readonly header: Header;
  readonly claim: ValidationClaimWitness;
}> => {
  const step = {
    schema_version: 1n,
    step_index: 0n,
    event_key: eventKey,
    phase: "ForcedTransaction" as const,
    pre_utxos_root: preUtxosRoot,
    post_utxos_root: postUtxosRoot,
  };
  const eventToStepValue = {
    step_index: 0n,
    phase: "ForcedTransaction" as const,
  };
  const operatorDescriptor = validationTraceDescriptorDataFromCore(
    operatorTrace.tree.descriptor,
  );
  const forcedEntry = transitionTraceDaEntry({
    key: txOrderId,
    keySchema: OutputReference as never,
    value: forcedTransaction,
    valueSchema: ForcedInclusionTxV1Schema,
  });
  const transitionEntry = transitionTraceDaEntry({
    key: step.step_index,
    keySchema: Data.Integer() as never,
    value: step,
    valueSchema: TransitionStepSchema,
  });
  const eventToStepEntry = transitionTraceDaEntry({
    key: eventKey,
    keySchema: EventKeySchema,
    value: eventToStepValue,
    valueSchema: EventToStepValueSchema,
  });
  const descriptorEntry = transitionTraceDaEntry({
    key: eventKey,
    keySchema: EventKeySchema,
    value: operatorDescriptor,
    valueSchema: ValidationTraceDescriptorSchema,
  });
  const forcedRoot = await buildCountedRoot(ROOT_DOMAINS.forcedTransactionsV1, [
    {
      key: Buffer.from(forcedEntry[0], "hex"),
      value: Buffer.from(forcedEntry[1], "hex"),
    },
  ]);
  const transitionRoot = await buildCountedRoot(ROOT_DOMAINS.transitionTrace, [
    {
      key: Buffer.from(transitionEntry[0], "hex"),
      value: Buffer.from(transitionEntry[1], "hex"),
    },
  ]);
  const eventToStepRoot = await buildCountedRoot(ROOT_DOMAINS.eventToStep, [
    {
      key: Buffer.from(eventToStepEntry[0], "hex"),
      value: Buffer.from(eventToStepEntry[1], "hex"),
    },
  ]);
  const descriptorRoot = await buildCountedRoot(ROOT_DOMAINS.validationTraces, [
    {
      key: Buffer.from(descriptorEntry[0], "hex"),
      value: Buffer.from(descriptorEntry[1], "hex"),
    },
  ]);
  const membership = async (
    root: typeof forcedRoot,
    entry: readonly [string, string],
  ) => ({
    domain: root.domain,
    root: root.root,
    phas_root: root.phasRoot,
    count: root.count,
    proof: await keyValuePhasProof(
      {
        root: root.phasRoot,
        count: root.count,
        entries: root.entries,
      },
      Buffer.from(entry[0], "hex"),
      Buffer.from(entry[1], "hex"),
    ),
  });
  const claim: ValidationClaimWitness = {
    version: 1n,
    descriptor_membership: {
      ...(await membership(descriptorRoot, descriptorEntry)),
      key: eventKey,
      value: operatorDescriptor,
    },
    transition_step_membership: {
      ...(await membership(transitionRoot, transitionEntry)),
      key: 0n,
      value: step,
    },
    event_to_step_membership: {
      ...(await membership(eventToStepRoot, eventToStepEntry)),
      key: eventKey,
      value: eventToStepValue,
    },
    source_membership: {
      ForcedValidationSource: {
        membership: {
          ...(await membership(forcedRoot, forcedEntry)),
          key: txOrderId,
          value: forcedTransaction,
        },
      },
    },
    validation_context_cbor:
      operatorTrace.validationContextCbor.toString("hex"),
    initial_state: validationMachineStateDataFromCore(operatorTrace.states[0]!),
    terminal_state: validationMachineStateDataFromCore(
      operatorTrace.states.at(-1)!,
    ),
    initial_state_proof: validationTraceProofDataFromCore(
      operatorTrace.tree.proofs[0]!,
    ),
    terminal_state_proof: validationTraceProofDataFromCore(
      operatorTrace.tree.proofs.at(-1)!,
    ),
  };
  const header: Header = {
    ...makeHeader(operatorVkey, now),
    forcedTransactionsRoot: forcedRoot.root,
    transitionTraceRoot: transitionRoot.root,
    eventToStepRoot: eventToStepRoot.root,
    validationTracesRoot: descriptorRoot.root,
    forcedTransactionCount: 1n,
    totalEventCount: 1n,
    transitionStepCount: 1n,
    validationTraceCount: 1n,
  };
  return { header, claim };
};

/**
 * The empty claimed-delta commitment, i.e. `frontier_commitment(0, [])` on the
 * Aiken side. Every pre-VM-DEFECT-2 rejection fixture pinned the machine
 * state's `ledger_delta_root` to exactly this value, which is the one
 * pre-state in which the deleted `rejected_successor_is_exact` clause was
 * satisfiable.
 */
export const EMPTY_CLAIMED_LEDGER_DELTA_ROOT = hashMidgardValidationLedgerDelta(
  [],
);

export const outRefCbor = (byte: number, index = 0n): Buffer =>
  encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, byte),
    outputIndex: Number(index),
  });

export const plainOutputCbor = (lovelace: bigint): Buffer =>
  encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x55)]),
    value: { lovelace, assets: new Map() },
  });

/**
 * Produces a genuine non-empty claimed ledger-delta commitment through exactly
 * the reference-builder pipeline an accepted transaction uses
 * (`hashMidgardValidationLedgerDelta` over authenticated delete/insert
 * operations carrying real MPF proof descriptors). The value is deliberately
 * *not* synthesised: it is the commitment a real one-input/one-output L2
 * transaction claims.
 */
export const buildNonEmptyClaimedLedgerDeltaRoot =
  async (): Promise<Buffer> => {
    const spent = outRefCbor(0x9c);
    const produced = outRefCbor(0x9d);
    const spentOutput = plainOutputCbor(10_000_000n);
    const producedOutput = plainOutputCbor(9_000_000n);
    const mutationSteps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: [
        { type: "delete", key: spent },
        buildValidationMachineLedgerInsertOp({
          key: produced,
          outputCbor: producedOutput,
        }),
      ],
    });
    return hashMidgardValidationLedgerDelta(
      mutationSteps.map(({ operation, proofFoldTrace }) => ({
        ...operation,
        proofDescriptor: proofFoldTrace.descriptor,
      })),
    );
  };

export const restampTraceLedgerDeltaRoot = (
  trace: DeterministicValidationMachineTrace,
  ledgerDeltaRoot: Buffer,
): DeterministicValidationMachineTrace => {
  const states = trace.states.map((state) => ({ ...state, ledgerDeltaRoot }));
  return {
    ...trace,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      trace.verdict,
      states.at(-1)!.rejectionCodeHash,
    ),
  };
};

export const replaceTerminalState = (
  trace: DeterministicValidationMachineTrace,
  {
    terminal,
    verdict,
    rejectionCode,
    rejectionCodeHash,
  }: {
    readonly terminal: DeterministicValidationMachineTrace["states"][number];
    readonly verdict: "accepted" | "rejected";
    readonly rejectionCode: DeterministicValidationMachineTrace["rejectionCode"];
    readonly rejectionCodeHash: Buffer;
  },
): DeterministicValidationMachineTrace => {
  const states = trace.states.map((state, index) =>
    index === trace.states.length - 1 ? terminal : state,
  );
  return {
    ...trace,
    states,
    tree: buildMidgardValidationTraceTree(
      states.map(hashMidgardValidationMachineState),
      verdict,
      rejectionCodeHash,
    ),
    verdict,
    rejectionCode,
  };
};
