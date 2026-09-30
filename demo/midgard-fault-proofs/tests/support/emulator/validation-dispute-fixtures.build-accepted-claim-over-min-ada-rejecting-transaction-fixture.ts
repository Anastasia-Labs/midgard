import {
  computeMidgardNativeTxId,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
  encodeCbor,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
} from "@al-ft/midgard-core";
import {
  EventKeySchema,
  validationTraceDescriptorDataFromCore,
} from "@al-ft/midgard-sdk";
import {
  buildDeterministicValidationMachineTrace,
  buildValidationDisputeEvidenceBundle,
  buildValidationMachineLedgerMutationSteps,
  outputCborMeetsMinAda,
  RejectCodes,
} from "@al-ft/midgard-validation";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import { encodeData } from "../../../src/index.js";
import { transitionTraceOutRef } from "./header-fixtures.js";
import { makeNativeTx } from "./native-tx.js";
import {
  type ForcedValidationDisputeFixture,
  MIN_ADA_JOURNEY_OUTPUT_LOVELACE,
} from "./validation-dispute-fixtures.build-accepted-claim-over-rejecting-transaction-fixture.js";
import {
  buildForcedValidationDisputeCommitments,
  outRefCbor,
  replaceTerminalState,
} from "./validation-dispute-fixtures.build-forced-validation-dispute-commitments.js";

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
  terminalCounterMismatch = false,
}: {
  readonly operatorVkey: string;
  readonly now: number;
  /** Commits an ordinary off-by-one terminal counter for source routing. */
  readonly terminalCounterMismatch?: boolean;
}): Promise<
  ForcedValidationDisputeFixture & {
    readonly disputedLowIndex: number;
    /**
     * The exact deterministic-machine replay input the challenger trace was
     * built from, for the production challenge authority
     * (`admitValidationTraceChallenge`) to reproduce independently.
     */
    readonly challengerReplayInput: Parameters<
      typeof buildDeterministicValidationMachineTrace
    >[0];
  }
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
    value: { lovelace: MIN_ADA_JOURNEY_OUTPUT_LOVELACE, assets: new Map() },
  });
  const producedOutput = encodeMidgardTxOutput({
    address: spendingAddress,
    value: { lovelace: MIN_ADA_JOURNEY_OUTPUT_LOVELACE, assets: new Map() },
  });
  // Measured, not assumed: this fixture only means anything if the produced
  // output really is below the floor the wiring convicts on.
  expect(
    outputCborMeetsMinAda(producedOutput, MIN_ADA_JOURNEY_OUTPUT_LOVELACE),
  ).toBe(false);
  const unsignedTx = makeNativeTx({
    spendInputCbors: [spentOutRef],
    fee: 0n,
    outputCbor: producedOutput,
  });
  const transactionId = computeMidgardNativeTxId(unsignedTx);
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
  const forcedCanonicalCbor = encodeMidgardForcedTxCanonical(forcedNativeTx);
  const forcedSource =
    deriveMidgardForcedTxProofSourceFromCanonicalCbor(forcedCanonicalCbor);
  const forcedTransaction = {
    tx_id: transactionId.toString("hex"),
    submitted_source: {
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
  // Exposed to journey tests so the production challenge authority can rebuild
  // the identical trace from the exact W25 replay input
  // (`admitValidationTraceChallenge` refuses caller-authored trace objects).
  const challengerReplayInput = {
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
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
  } as const;
  const challengerTrace = await Effect.runPromise(
    buildDeterministicValidationMachineTrace(challengerReplayInput),
  );
  const operatorTrace = replaceTerminalState(challengerTrace, {
    terminal: {
      ...challengerTrace.states.at(-1)!,
      programCounter:
        challengerTrace.states.at(-1)!.programCounter -
        (terminalCounterMismatch ? 1 : 0),
      verdict: "accepted",
      rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
      workRoot: Buffer.alloc(32, 0x7e),
    },
    verdict: "accepted",
    rejectionCode: null,
    rejectionCodeHash: MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  });
  const evidence = buildValidationDisputeEvidenceBundle({
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
    challengerReplayInput,
  };
};
