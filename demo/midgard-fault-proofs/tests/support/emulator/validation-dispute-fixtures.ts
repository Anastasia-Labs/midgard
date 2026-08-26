import {
  buildMidgardValidationTraceTree,
  computeMidgardNativeTxIdV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardSpendInputItemV1,
  encodeMidgardTxOutput,
  hashMidgardValidationLedgerDeltaV1,
  hashMidgardValidationMachineStateV1,
  hashMidgardValidationRejectionCodeV1,
  hashMidgardValidationWorkWitnessV1,
  MIDGARD_CONSENSUS_PROFILE_V1,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import {
  EMPTY_MERKLE_TREE_ROOT,
  EventKeySchema,
  EventToStepValueSchema,
  ForcedInclusionTxV1Schema,
  HeaderV1,
  OutputReference,
  ROOT_DOMAINS,
  TransitionStepSchema,
  type ValidationClaimWitnessV1,
  validationMachineStateDataFromCore,
  validationTraceDescriptorDataFromCore,
  ValidationTraceDescriptorV1Schema,
  validationTraceProofDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationDisputeEvidenceBundleV1,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  type DeterministicValidationMachineTrace,
  outputCborMeetsMinAdaV1,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  buildCountedRoot,
  encodeData,
  keyValuePhasProof,
} from "../../../src/index.js";
import {
  makeHeader,
  transitionTraceDaEntry,
  transitionTraceOutRef,
} from "./header-fixtures.js";
import { makeNativeTx } from "./native-tx.js";

export type ForcedValidationSourceEntryV1 = NonNullable<
  ValidationClaimWitnessV1["source_membership"] extends infer Source
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
  readonly forcedTransaction: ForcedValidationSourceEntryV1;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly preUtxosRoot: string;
  readonly postUtxosRoot: string;
}): Promise<{
  readonly header: HeaderV1;
  readonly claim: ValidationClaimWitnessV1;
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
    valueSchema: ValidationTraceDescriptorV1Schema,
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
  const claim: ValidationClaimWitnessV1 = {
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
  const header: HeaderV1 = {
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
export const EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1 =
  hashMidgardValidationLedgerDeltaV1([]);

export const outRefCbor = (byte: number, index = 0n): Buffer =>
  encodeMidgardSpendInputItemV1({
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
 * (`hashMidgardValidationLedgerDeltaV1` over authenticated delete/insert
 * operations carrying real MPF proof descriptors). The value is deliberately
 * *not* synthesised: it is the commitment a real one-input/one-output L2
 * transaction claims.
 */
export const buildNonEmptyClaimedLedgerDeltaRootV1 =
  async (): Promise<Buffer> => {
    const spent = outRefCbor(0x9c);
    const produced = outRefCbor(0x9d);
    const spentOutput = plainOutputCbor(10_000_000n);
    const producedOutput = plainOutputCbor(9_000_000n);
    const mutationSteps = await buildValidationMachineLedgerMutationSteps({
      initialEntries: [{ outRef: spent, output: spentOutput }],
      operations: [
        { type: "delete", key: spent },
        buildValidationMachineLedgerInsertOpV1({
          key: produced,
          outputCbor: producedOutput,
        }),
      ],
    });
    return hashMidgardValidationLedgerDeltaV1(
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
      states.map(hashMidgardValidationMachineStateV1),
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
      states.map(hashMidgardValidationMachineStateV1),
      verdict,
      rejectionCodeHash,
    ),
    verdict,
    rejectionCode,
  };
};

/**
 * Work root of the exact rejecting-terminal witness the canonical V1 machine
 * requires of a rejection successor, i.e. the Aiken
 * `hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(rejection_code, pre.prior_ledger_root))`.
 */
export const rejectingTerminalWorkRootV1 = ({
  programCounter,
  rejectionCode,
  priorLedgerRoot,
}: {
  readonly programCounter: number;
  readonly rejectionCode: string;
  readonly priorLedgerRoot: Buffer;
}): Buffer =>
  Buffer.from(
    hashMidgardValidationWorkWitnessV1({
      phase: "terminal",
      programCounter,
      witnessCbor: encodeCbor([
        2n,
        Buffer.from(rejectionCode, "ascii"),
        priorLedgerRoot,
        Buffer.from("80", "hex"),
      ]),
    }),
  );

export type ForcedValidationDisputeFixture = {
  readonly header: HeaderV1;
  readonly claim: ValidationClaimWitnessV1;
  readonly operatorTrace: DeterministicValidationMachineTrace;
  readonly challengerTrace: DeterministicValidationMachineTrace;
  readonly challengerDescriptor: ReturnType<
    typeof validationTraceDescriptorDataFromCore
  >;
  readonly evidence: ReturnType<typeof buildValidationDisputeEvidenceBundleV1>;
  readonly claimedLedgerDeltaRoot: Buffer;
};

/**
 * VM-DEFECT-2 regression fixture — the adversarial case the pre-fix rejection
 * surface could not express.
 *
 * The forced source carries `verdict: ForcedTxValid`, which
 * `validation-claim-v1.ak` (`forced_verdict_matches`, and the exactly
 * analogous `descriptor.verdict == Accepted` clause every *normal* L2 source
 * is held to) forces into an `Accepted` committed descriptor. The transaction
 * itself has no spend inputs, so the deterministic machine rejects it with
 * `E_EMPTY_INPUTS` at the `inputSets` instruction. The claimed ledger delta is
 * a real non-empty commitment, exactly as a real transaction's would be.
 *
 * The reference TypeScript builder refuses to emit a rejected trace with a
 * non-empty delta (`validation-machine.ts`: "a rejected transaction must
 * commit an exact ledger no-op"), so the operator's chosen claimed-delta
 * commitment is re-stamped onto every state afterwards. That is faithful:
 * `ledger_delta_root` is immutable context chosen by the operator and pinned
 * pre == post by `immutable_context_matches`, and nothing before the
 * `ledgerDelta` phase reads it.
 */
export const buildAcceptedClaimOverRejectingTransactionFixture = async ({
  operatorVkey,
  now,
  claimedLedgerDeltaRoot,
  clearChallengerTerminalDelta = false,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly claimedLedgerDeltaRoot: Buffer;
  readonly clearChallengerTerminalDelta?: boolean;
}): Promise<ForcedValidationDisputeFixture> => {
  const txOrderId = transitionTraceOutRef("e2");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [],
    fee: 0n,
    outputCbor: plainOutputCbor(100_000_000n),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const transactionId = computeMidgardNativeTxIdV1(forcedNativeTx);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid" as const,
  };
  const honestTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
      ledgerWitnessEntries: [],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.EmptyInputs,
      // The challenger replays the operator's ACCEPTED leaf to a rejection;
      // its states must still bind the committed (ForcedTxValid) source.
      committedForcedVerdict: "accepted",
    }),
  );
  const restamped = restampTraceLedgerDeltaRoot(
    honestTrace,
    claimedLedgerDeltaRoot,
  );
  // Counterfactual variant: the successor shape the deleted
  // `post.ledger_delta_root == frontier_commitment(0, [])` clause demanded.
  const challengerTrace = clearChallengerTerminalDelta
    ? replaceTerminalState(restamped, {
        terminal: {
          ...restamped.states.at(-1)!,
          ledgerDeltaRoot: EMPTY_CLAIMED_LEDGER_DELTA_ROOT_V1,
        },
        verdict: "rejected",
        rejectionCode: restamped.rejectionCode,
        rejectionCodeHash: restamped.states.at(-1)!.rejectionCodeHash,
      })
    : restamped;
  const operatorTrace = replaceTerminalState(restamped, {
    terminal: {
      ...restamped.states.at(-1)!,
      verdict: "accepted",
      rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      workRoot: Buffer.alloc(32, 0x7e),
    },
    verdict: "accepted",
    rejectionCode: null,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  });
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
    postUtxosRoot: EMPTY_MERKLE_TREE_ROOT,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot,
  };
};

/**
 * Lovelace carried by both sides of the min-Ada journey transaction below.
 * `coins_per_utxo_byte * (160 + |canonical output|)` for an output of that
 * shape is on the order of a million lovelace, so this is a decisive miss,
 * and the fixture asserts the miss rather than trusting the arithmetic.
 */
const MIN_ADA_JOURNEY_OUTPUT_LOVELACE_V1 = 100_000n;

/**
 * R8 of decision 0005 (#618) / the #627 ruling: the end-to-end journey for the
 * `E_MIN_ADA` wiring in the ValueAndMint output ladder.
 *
 * The forced source carries `verdict: ForcedTxValid`, which
 * `validation-claim-v1.ak` forces into an `Accepted` committed descriptor. The
 * transaction is otherwise impeccable -- one resolved spend input, a real
 * key witness, zero fee, and the produced output carries exactly the lovelace
 * the input did, so value is preserved and nothing before stage 3 of
 * ValueAndMint has anything to say about it. The one rule it breaks is the
 * produced output's minimum-Ada floor, which the machine convicts on at the
 * output-descriptor step of stage 3 (`E_MIN_ADA`).
 *
 * The operator commits the honest trace with only its terminal replaced by an
 * `Accepted` one, so the bisection lands on the last step -- the ValueAndMint
 * output-descriptor instruction whose successor is the rejecting terminal --
 * and the challenger proves it through `value_and_mint_v1` and
 * `value_and_mint_output_descriptor_semantic_v1`. That is the only route on
 * which the new `rejected_successor_is_exact(pre, post, reject_min_ada)`
 * conjunct executes on L1.
 *
 * A rejected transaction commits an exact ledger no-op, so the block's prior
 * and post UTxO roots are both the root of the honest pre-state ledger and
 * there are no mutation steps.
 */
export const buildAcceptedClaimOverMinAdaRejectingTransactionFixture = async ({
  operatorVkey,
  now,
}: {
  readonly operatorVkey: string;
  readonly now: number;
}): Promise<
  ForcedValidationDisputeFixture & { readonly disputedLowIndex: number }
> => {
  const txOrderId = transitionTraceOutRef("e6");
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const spendingKey = CML.PrivateKey.generate_ed25519();
  const spendingAddress = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKey.to_public().hash()),
    )
      .to_address()
      .to_raw_bytes(),
  );
  const spentOutRef = outRefCbor(0x8b);
  const spentOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: MIN_ADA_JOURNEY_OUTPUT_LOVELACE_V1, assets: new Map() },
  });
  const producedOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: MIN_ADA_JOURNEY_OUTPUT_LOVELACE_V1, assets: new Map() },
  });
  // Measured, not assumed: this fixture only means anything if the produced
  // output really is below the floor the wiring convicts on.
  expect(
    outputCborMeetsMinAdaV1(producedOutput, MIN_ADA_JOURNEY_OUTPUT_LOVELACE_V1),
  ).toBe(false);
  const unsignedTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
  });
  const transactionId = computeMidgardNativeTxIdV1(unsignedTx);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
    addrTxWitsPreimageCbor: encodeCbor([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(transactionId),
          spendingKey,
        ).to_cbor_bytes(),
      ),
    ]),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid" as const,
  };
  // The probe deletion is only a way to read the root of the honest pre-state
  // ledger trie; none of its steps reach the machine, which is given an exact
  // no-op as a rejected transaction requires.
  const ledgerRootProbe = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spentOutRef, output: spentOutput }],
    operations: [{ type: "delete", key: spentOutRef }],
  });
  const utxosRoot = ledgerRootProbe[0]!.preRoot.toString("hex");
  const challengerTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: utxosRoot,
      postUtxosRoot: utxosRoot,
      ledgerWitnessEntries: [{ outRef: spentOutRef, output: spentOutput }],
      expectedLedgerOps: [],
      ledgerMutationSteps: [],
      expectedVerdict: "rejected",
      expectedRejectionCode: RejectCodes.MinAda,
      // The challenger replays the operator's ACCEPTED leaf to a rejection;
      // its states must still bind the committed (ForcedTxValid) source.
      committedForcedVerdict: "accepted",
    }),
  );
  const operatorTrace = replaceTerminalState(challengerTrace, {
    terminal: {
      ...challengerTrace.states.at(-1)!,
      verdict: "accepted",
      rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      workRoot: Buffer.alloc(32, 0x7e),
    },
    verdict: "accepted",
    rejectionCode: null,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  });
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot: utxosRoot,
    postUtxosRoot: utxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: challengerTrace.states[0]!.ledgerDeltaRoot,
    disputedLowIndex: challengerTrace.states.length - 2,
  };
};

/**
 * Mirror control for VM-DEFECT-2 (GOAL_SPEC §3 invariant 9 -- soundness is
 * symmetric). Same block layout, same disputed instruction, same rejection
 * code and same *genuinely non-empty* claimed ledger delta as the
 * challenger-wins fixture; the only difference is that the transaction is
 * actually valid and the operator's committed `Accepted` verdict is honest.
 *
 * The dishonest challenger commits the strongest forgery available: a
 * rejecting terminal whose immutable context, program counter, execution
 * budget and work root are all exactly what `rejected_successor_is_exact`
 * demands (`hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(code, pre.prior_ledger_root))`). The one
 * thing it cannot supply is a genuine rejection at the `inputSets`
 * instruction, so the challenger must lose. Removing the delta-clearing clause
 * must not have made honest blocks challengeable.
 */
/**
 * One honest, valid, signed native transaction (one spend, one output) and
 * its accepted deterministic trace, shared by the honest-operator mirror
 * fixture below and the forged-operator-successor fixtures that dispute one
 * of its steps. `txOrderSeed` keeps the forced-event keys of the fixtures
 * distinct.
 */
const buildHonestAcceptedNativeTransactionTraceV1 = async ({
  now,
  txOrderSeed,
}: {
  readonly now: number;
  readonly txOrderSeed: string;
}) => {
  const txOrderId = transitionTraceOutRef(txOrderSeed);
  const eventKey = { ForcedTransactionEventKey: { tx_order_id: txOrderId } };
  const spendingKey = CML.PrivateKey.generate_ed25519();
  const spendingAddress = Buffer.from(
    CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKey.to_public().hash()),
    )
      .to_address()
      .to_raw_bytes(),
  );
  const spentOutRef = outRefCbor(0x8a);
  const spentOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: 10_000_000n, assets: new Map() },
  });
  const producedOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: 10_000_000n, assets: new Map() },
  });
  const unsignedTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
  });
  const transactionId = computeMidgardNativeTxIdV1(unsignedTx);
  const forcedNativeTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
    addrTxWitsPreimageCbor: encodeCbor([
      Buffer.from(
        CML.make_vkey_witness(
          CML.TransactionHash.from_raw_bytes(transactionId),
          spendingKey,
        ).to_cbor_bytes(),
      ),
    ]),
  });
  const forcedCanonicalCbor = encodeMidgardNativeTxCanonicalV1(forcedNativeTx);
  const forcedSource =
    deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(forcedCanonicalCbor);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    source: {
      compact_cbor: forcedSource.compactCbor.toString("hex"),
      witness_set_compact_cbor:
        forcedSource.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        forcedSource.fieldPreimageLengthsCbor.toString("hex"),
    },
    verdict: "ForcedTxValid" as const,
  };
  const producedOutRef = encodeMidgardSpendInputItemV1({
    txId: transactionId,
    outputIndex: 0,
  });
  const expectedLedgerOps = [
    { type: "delete" as const, key: spentOutRef },
    buildValidationMachineLedgerInsertOpV1({
      key: producedOutRef,
      outputCbor: producedOutput,
    }),
  ];
  const ledgerMutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: [{ outRef: spentOutRef, output: spentOutput }],
    operations: expectedLedgerOps,
  });
  const preUtxosRoot = ledgerMutationSteps[0]!.preRoot.toString("hex");
  const postUtxosRoot = ledgerMutationSteps.at(-1)!.postRoot.toString("hex");
  const honestTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace({
      consensusProfile: MIDGARD_CONSENSUS_PROFILE_V1,
      eventKeyCbor: encodeData(eventKey, EventKeySchema),
      sourceKind: "forced",
      blockEndTimeMs: now + 1_000,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      blockSlot: 0n,
      transactionId,
      canonicalTransactionCbor: forcedCanonicalCbor,
      priorUtxosRoot: preUtxosRoot,
      postUtxosRoot,
      ledgerWitnessEntries: [{ outRef: spentOutRef, output: spentOutput }],
      expectedLedgerOps,
      ledgerMutationSteps,
      expectedVerdict: "accepted",
      expectedRejectionCode: null,
    }),
  );
  return {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace,
    preUtxosRoot,
    postUtxosRoot,
  };
};

/**
 * Mirror control for VM-DEFECT-2 (GOAL_SPEC §3 invariant 9 -- soundness is
 * symmetric). Same block layout, same disputed instruction, same rejection
 * code and same *genuinely non-empty* claimed ledger delta as the
 * challenger-wins fixture; the only difference is that the transaction is
 * actually valid and the operator's committed `Accepted` verdict is honest.
 *
 * The dishonest challenger commits the strongest forgery available: a
 * rejecting terminal whose immutable context, program counter, execution
 * budget and work root are all exactly what `rejected_successor_is_exact`
 * demands (`hash_work_witness(Terminal, pre.program_counter + 1,
 * encode_terminal_rejection_witness(code, pre.prior_ledger_root))`). The one
 * thing it cannot supply is a genuine rejection at the `inputSets`
 * instruction, so the challenger must lose. Removing the delta-clearing clause
 * must not have made honest blocks challengeable.
 */
export const buildHonestAcceptedValidationDisputeFixture = async ({
  operatorVkey,
  now,
}: {
  readonly operatorVkey: string;
  readonly now: number;
}): Promise<
  ForcedValidationDisputeFixture & { readonly disputedPhase: string }
> => {
  const {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace: operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  } = await buildHonestAcceptedNativeTransactionTraceV1({
    now,
    txOrderSeed: "e3",
  });
  const disputedLowIndex = operatorTrace.states.findIndex(
    (state) => state.phase === "inputSets",
  );
  if (disputedLowIndex < 0) {
    throw new Error(
      "honest accepted validation trace is missing its inputSets instruction",
    );
  }
  const preState = operatorTrace.states[disputedLowIndex]!;
  const forgedRejectionCode = RejectCodes.EmptyInputs;
  const forgedTerminal = {
    ...preState,
    phase: "terminal" as const,
    programCounter: preState.programCounter + 1,
    workRoot: rejectingTerminalWorkRootV1({
      programCounter: preState.programCounter + 1,
      rejectionCode: forgedRejectionCode,
      priorLedgerRoot: preState.priorLedgerRoot,
    }),
    verdict: "rejected" as const,
    rejectionCodeHash: Buffer.from(
      hashMidgardValidationRejectionCodeV1(forgedRejectionCode),
    ),
  };
  const challengerStates = operatorTrace.states.map((state, index) =>
    index <= disputedLowIndex ? state : forgedTerminal,
  );
  const challengerTrace: DeterministicValidationMachineTrace = {
    ...operatorTrace,
    states: challengerStates,
    tree: buildMidgardValidationTraceTree(
      challengerStates.map(hashMidgardValidationMachineStateV1),
      "rejected",
      forgedTerminal.rejectionCodeHash,
    ),
    verdict: "rejected",
    rejectionCode: forgedRejectionCode,
  };
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: operatorTrace.states[0]!.ledgerDeltaRoot,
    disputedPhase: preState.phase,
  };
};

/**
 * R5 item 1 (#617) journey fixture for the cek and ValueAndMint prepare +
 * semantic decomposition. The transaction is the honest, valid, signed
 * native transaction above and the challenger's trace is its honest
 * accepted trace; the operator commits the same trace up to the first state
 * of `disputedPhase` and a forged successor from there on (the honest
 * `Accepted` terminal with a fabricated work root, repeated to the honest
 * length so the descriptor and every midpoint after the boundary disagree,
 * which pins the bisection at exactly that boundary). The one-step the challenger then
 * proves on L1 is the honest trace's first step of that phase:
 *
 * - `cek`: this pure key-witness spend has `execution_count == 0` (no script
 *   execution of any language), so the cek phase is the single stand-alone
 *   ValueAndMint hand-off (`cek_v1` prepare,
 *   then `cek_finish_semantic_v1`, resolver 11 / semantic 0);
 * - `valueAndMint`: the stage-0 `begin` step (`value_and_mint_v1` prepare,
 *   then `value_and_mint_begin_semantic_v1`, resolver 12 / semantic 0).
 */
export const buildForgedOperatorSuccessorValidationDisputeFixture = async ({
  operatorVkey,
  now,
  disputedPhase,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  readonly disputedPhase: "cek" | "valueAndMint";
}): Promise<
  ForcedValidationDisputeFixture & {
    readonly disputedPhase: "cek" | "valueAndMint";
    readonly disputedLowIndex: number;
  }
> => {
  const {
    txOrderId,
    eventKey,
    forcedTransaction,
    honestTrace: challengerTrace,
    preUtxosRoot,
    postUtxosRoot,
  } = await buildHonestAcceptedNativeTransactionTraceV1({
    now,
    txOrderSeed: disputedPhase === "cek" ? "e4" : "e5",
  });
  const disputedLowIndex = challengerTrace.states.findIndex(
    (state) => state.phase === disputedPhase,
  );
  if (disputedLowIndex < 0) {
    throw new Error(
      `honest accepted validation trace is missing its ${disputedPhase} phase`,
    );
  }
  const honestTerminal = challengerTrace.states.at(-1)!;
  if (honestTerminal.phase !== "terminal") {
    throw new Error(
      "honest accepted validation trace does not end in a terminal state",
    );
  }
  // The honest terminal with only its work root fabricated: every endpoint
  // check the source validator applies to the operator's claim (terminal
  // phase, program counter == step count, verdict, rejection code, immutable
  // context, ledger delta root) still holds, so the dispute opens and the
  // bisection -- not the source stage -- is what exposes the forgery.
  const forgedTerminal = {
    ...honestTerminal,
    workRoot: Buffer.alloc(32, 0x7e),
  };
  const operatorStates = challengerTrace.states.map((state, index) =>
    index <= disputedLowIndex ? state : forgedTerminal,
  );
  const operatorTrace: DeterministicValidationMachineTrace = {
    ...challengerTrace,
    states: operatorStates,
    tree: buildMidgardValidationTraceTree(
      operatorStates.map(hashMidgardValidationMachineStateV1),
      "accepted",
      MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
    ),
  };
  const evidence = buildValidationDisputeEvidenceBundleV1({
    operatorTrace,
    challengerTrace,
    currentTime: now + 2_000,
  });
  const { header, claim } = await buildForcedValidationDisputeCommitments({
    operatorVkey,
    now,
    txOrderId,
    eventKey,
    forcedTransaction,
    operatorTrace,
    preUtxosRoot,
    postUtxosRoot,
  });
  return {
    header,
    claim,
    operatorTrace,
    challengerTrace,
    challengerDescriptor: validationTraceDescriptorDataFromCore(
      challengerTrace.tree.descriptor,
    ),
    evidence,
    claimedLedgerDeltaRoot: operatorTrace.states[0]!.ledgerDeltaRoot,
    disputedPhase,
    disputedLowIndex,
  };
};
