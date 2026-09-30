import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type AbiFixtureValue,
  address,
  daPayloadBodyFixture,
  eventKeys,
  eventToStepValueFixture,
  forcedInclusionTxFixture,
  h28,
  h32,
  h64,
  headerFixture,
  l2TransactionSourceFixture,
  outputReference,
  proof,
  rootCountProof,
  secondTransitionStepFixture,
  transitionStepFixture,
  value,
} from "./sdk-abi-fixtures.header-fixture.js";

export const buildTransitionTraceAbiFixtures = (): Record<
  string,
  AbiFixtureValue
> => {
  const withdrawalInfo: SDK.WithdrawalInfo = {
    body: {
      l2_outref: { transactionId: h32, outputIndex: 1n },
      l2_owner: h28,
      l2_value: value,
      l1_address: address,
      l1_datum: "NoDatum",
    },
    signature: [h32, h64],
    validity: "IncorrectWithdrawalSignature",
  };
  const validWithdrawalInfo: SDK.WithdrawalInfo = {
    ...withdrawalInfo,
    validity: "WithdrawalIsValid",
  };
  const depositInfo: SDK.DepositInfo = {
    l2_address: address,
    l2_network_id: 0n,
    l2_datum: null,
  };
  const ledgerDeleteWitness: SDK.LedgerDeleteWitness = {
    key: "aa",
    value: "bb",
    membership_proof: proof,
    delete_proof: proof,
  };
  const ledgerInsertWitness: SDK.LedgerInsertWitness = {
    key: "cc",
    value: "dd",
    non_membership_proof: proof,
    insert_proof: proof,
  };
  const traceProof: SDK.IndexedTraceProof = {
    domain: SDK.ROOT_DOMAINS.transitionTrace,
    root: headerFixture.transitionTraceRoot,
    phas_root: "66".repeat(32),
    count: 2n,
    key: 0n,
    value: transitionStepFixture,
    proof,
  };
  const secondTraceProof: SDK.IndexedTraceProof = {
    ...traceProof,
    key: 1n,
    value: secondTransitionStepFixture,
  };
  const eventToStepMembership: SDK.EventToStepMembershipProof = {
    domain: SDK.ROOT_DOMAINS.eventToStep,
    root: headerFixture.eventToStepRoot,
    phas_root: "67".repeat(32),
    count: 1n,
    key: eventKeys[0]!,
    value: eventToStepValueFixture,
    proof,
  };
  const eventToStepNonMembership: SDK.EventToStepNonMembershipProof = {
    domain: SDK.ROOT_DOMAINS.eventToStep,
    root: headerFixture.eventToStepRoot,
    phas_root: "67".repeat(32),
    count: 1n,
    key: eventKeys[3]!,
    proof,
  };
  const withdrawalSourceMembership: SDK.WithdrawalSourceMembershipProof = {
    domain: SDK.ROOT_DOMAINS.withdrawals,
    root: headerFixture.withdrawalsRoot,
    phas_root: "68".repeat(32),
    count: 1n,
    key: outputReference,
    value: withdrawalInfo,
    proof,
  };
  const validWithdrawalSourceMembership: SDK.WithdrawalSourceMembershipProof = {
    ...withdrawalSourceMembership,
    value: validWithdrawalInfo,
  };
  const forcedSourceMembership: SDK.ForcedTransactionSourceMembershipProof = {
    domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
    root: headerFixture.forcedTransactionsRoot,
    phas_root: "69".repeat(32),
    count: 1n,
    key: outputReference,
    value: forcedInclusionTxFixture,
    proof,
  };
  const l2SourceMembership: SDK.L2TransactionSourceMembershipProof = {
    domain: SDK.ROOT_DOMAINS.transactionsV1,
    root: headerFixture.transactionsRoot,
    phas_root: "71".repeat(32),
    count: 1n,
    key: h32,
    value: Data.to(l2TransactionSourceFixture, SDK.L2TransactionSource),
    proof,
  };
  const depositSourceMembership: SDK.DepositSourceMembershipProof = {
    domain: SDK.ROOT_DOMAINS.deposits,
    root: headerFixture.depositsRoot,
    phas_root: "70".repeat(32),
    count: 1n,
    key: outputReference,
    value: depositInfo,
    proof,
  };
  const withdrawalSourceNonMembership: SDK.WithdrawalSourceNonMembershipProof =
    {
      domain: SDK.ROOT_DOMAINS.withdrawals,
      root: headerFixture.withdrawalsRoot,
      phas_root: "68".repeat(32),
      count: 1n,
      key: outputReference,
      proof,
    };
  const forcedSourceNonMembership: SDK.ForcedTransactionSourceNonMembershipProof =
    {
      domain: SDK.ROOT_DOMAINS.forcedTransactionsV1,
      root: headerFixture.forcedTransactionsRoot,
      phas_root: "69".repeat(32),
      count: 1n,
      key: outputReference,
      proof,
    };
  const depositSourceNonMembership: SDK.DepositSourceNonMembershipProof = {
    domain: SDK.ROOT_DOMAINS.deposits,
    root: headerFixture.depositsRoot,
    phas_root: "70".repeat(32),
    count: 1n,
    key: outputReference,
    proof,
  };
  const sourceMemberships = {
    withdrawal: {
      WithdrawalSourceMembership: { membership: withdrawalSourceMembership },
    },
    deposit: {
      DepositSourceMembership: { membership: depositSourceMembership },
    },
  } satisfies Record<string, SDK.TransitionSourceMembershipProof>;
  const sourceNonMemberships = {
    withdrawal: {
      WithdrawalSourceNonMembership: {
        non_membership: withdrawalSourceNonMembership,
      },
    },
  } satisfies Record<string, SDK.TransitionSourceNonMembershipProof>;
  const transitionFaults: Record<string, SDK.TransitionFault> = {
    "transition-fault.trace-boundary": SDK.traceBoundaryFault({
      side: "TraceStart",
      traceProof,
    }),
    "transition-fault.trace-link": SDK.traceLinkFault({
      lower: traceProof,
      upper: secondTraceProof,
    }),
    "transition-fault.event-to-step-mismatch-membership":
      SDK.eventToStepMismatchFault({
        traceProof,
        eventToStep: {
          EventToStepMembership: { membership: eventToStepMembership },
        },
      }),
    "transition-fault.event-to-step-mismatch-non-membership":
      SDK.eventToStepMismatchFault({
        traceProof,
        eventToStep: {
          EventToStepNonMembership: {
            non_membership: eventToStepNonMembership,
          },
        },
      }),
    "transition-fault.source-mapped-event-missing-from-source":
      SDK.sourceMembershipMismatchFault({
        MappedEventMissingFromSource: {
          trace_proof: traceProof,
          event_to_step: eventToStepMembership,
          source_non_membership: sourceNonMemberships.withdrawal,
        },
      }),
    "transition-fault.source-event-missing-trace":
      SDK.sourceMembershipMismatchFault({
        SourceEventMissingTrace: {
          source_membership: sourceMemberships.withdrawal,
          event_to_step_non_membership: eventToStepNonMembership,
        },
      }),
    "transition-fault.source-phase-mismatch": SDK.sourceMembershipMismatchFault(
      {
        SourcePhaseMismatch: {
          trace_proof: traceProof,
          source_membership: sourceMemberships.deposit,
        },
      },
    ),
    "transition-fault.valid-withdrawal-transition":
      SDK.invalidOneStepTransitionFault({
        ValidWithdrawalTransition: {
          trace_proof: traceProof,
          event_to_step: eventToStepMembership,
          source_membership: validWithdrawalSourceMembership,
          spent_utxo: ledgerDeleteWitness,
        },
      }),
    "transition-fault.invalid-withdrawal-no-op":
      SDK.invalidOneStepTransitionFault({
        InvalidWithdrawalNoOpTransition: {
          trace_proof: traceProof,
          event_to_step: eventToStepMembership,
          source_membership: withdrawalSourceMembership,
        },
      }),
    "transition-fault.invalid-forced-no-op": SDK.invalidOneStepTransitionFault({
      InvalidForcedTransactionNoOpTransition: {
        trace_proof: traceProof,
        event_to_step: eventToStepMembership,
        source_membership: forcedSourceMembership,
      },
    }),
    "transition-fault.valid-deposit-transition":
      SDK.invalidOneStepTransitionFault({
        ValidDepositTransition: {
          trace_proof: traceProof,
          event_to_step: eventToStepMembership,
          source_membership: depositSourceMembership,
          projected_utxo: ledgerInsertWitness,
        },
      }),
    "transition-fault.l2-transaction-transition":
      SDK.invalidOneStepTransitionFault({
        L2TransactionTransition: {
          trace_proof: traceProof,
          event_to_step: eventToStepMembership,
          source_membership: l2SourceMembership,
          spend_inputs_preimage: "80",
          outputs_preimage: "80",
          spent_utxos: [ledgerDeleteWitness],
          produced_utxos: [ledgerInsertWitness],
        },
      }),
    "transition-fault.omitted-deposit": SDK.omittedDueL1EventFault({
      OmittedDueDeposit: {
        source_non_membership: depositSourceNonMembership,
      },
    }),
    "transition-fault.omitted-withdrawal": SDK.omittedDueL1EventFault({
      OmittedDueWithdrawal: {
        source_non_membership: withdrawalSourceNonMembership,
      },
    }),
    "transition-fault.omitted-forced": SDK.omittedDueL1EventFault({
      OmittedDueForcedTransaction: {
        event_ref_input_index: 2n,
        event_asset_name: "cc",
        validity_override: {
          ForcedTxInvalid: {
            reason: { PlutusExecutionFailed: { execution_index: 0n } },
          },
        },
        source_non_membership: forcedSourceNonMembership,
      },
    }),
    "transition-fault.duplicate-trace-event": SDK.duplicateTraceEventFault({
      leftTrace: traceProof,
      rightTrace: secondTraceProof,
    }),
    "transition-fault.out-of-window-deposit": SDK.outOfWindowSourceEventFault({
      OutOfWindowDeposit: {
        source_membership: depositSourceMembership,
      },
    }),
    "transition-fault.out-of-window-withdrawal":
      SDK.outOfWindowSourceEventFault({
        OutOfWindowWithdrawal: {
          source_membership: withdrawalSourceMembership,
        },
      }),
    "transition-fault.out-of-window-forced": SDK.outOfWindowSourceEventFault({
      OutOfWindowForcedTransaction: {
        event_ref_input_index: 2n,
        event_asset_name: "cc",
        validity_override: {
          ForcedTxInvalid: {
            reason: { PlutusExecutionFailed: { execution_index: 0n } },
          },
        },
        source_membership: forcedSourceMembership,
      },
    }),
    "transition-fault.count-header-total": SDK.countFault(
      "HeaderTotalCountMismatch",
    ),
    "transition-fault.count-header-transition-step": SDK.countFault(
      "HeaderTransitionStepCountMismatch",
    ),
    "transition-fault.count-source-root": SDK.countFault({
      SourceRootCountMismatch: {
        proof: rootCountProof(
          SDK.ROOT_DOMAINS.withdrawals,
          headerFixture.withdrawalsRoot,
          "68".repeat(32),
          1n,
        ),
      },
    }),
    "transition-fault.count-event-to-step-root": SDK.countFault({
      EventToStepRootCountMismatch: {
        proof: rootCountProof(
          SDK.ROOT_DOMAINS.eventToStep,
          headerFixture.eventToStepRoot,
          "67".repeat(32),
          1n,
        ),
      },
    }),
    "transition-fault.count-transition-trace-root": SDK.countFault({
      TransitionTraceRootCountMismatch: {
        proof: rootCountProof(
          SDK.ROOT_DOMAINS.transitionTrace,
          headerFixture.transitionTraceRoot,
          "66".repeat(32),
          2n,
        ),
      },
    }),
  };

  const proofFor = (fault: SDK.TransitionFault): SDK.TransitionFaultProof =>
    SDK.makeTransitionFaultProof({
      challengedHeaderHash: h28,
      header: headerFixture,
      fault,
    });
  const routeArgsFor = (
    fault: SDK.TransitionFault,
  ): SDK.TransitionTraceRouteArgs => ({
    input_index: 0n,
    output_index: 1n,
    proof: proofFor(fault),
    proof_ref_indices: [],
  });
  const validationState: SDK.ValidationMachineState = {
    machine_version: 1n,
    event_key_hash: "81".repeat(32),
    transaction_id: h32,
    transaction_commitment: "35".repeat(32),
    validation_context_hash: "82".repeat(32),
    source_kind: "Normal",
    prior_ledger_root: headerFixture.prevUtxosRoot,
    phase: "Terminal",
    program_counter: 1n,
    work_root: "83".repeat(32),
    execution_cpu: 1n,
    execution_memory: 1n,
    verdict: "Accepted",
    rejection_code_hash: "00".repeat(32),
    ledger_delta_root: "84".repeat(32),
  };
  const validationDescriptor: SDK.ValidationTraceDescriptor = {
    schema_version: 1n,
    machine_version: 1n,
    trace_root: "85".repeat(32),
    step_count: 1n,
    initial_state_hash: "86".repeat(32),
    terminal_state_hash: "87".repeat(32),
    verdict: "Accepted",
    rejection_code_hash: "00".repeat(32),
  };
  const validationProof: SDK.ValidationTraceProof = {
    state_index: 0n,
    state_hash: validationDescriptor.terminal_state_hash,
    siblings: [],
  };
  const acceptedTransactionClaim: SDK.ValidationClaimWitness = {
    version: 1n,
    descriptor_membership: {
      domain: SDK.ROOT_DOMAINS.validationTraces,
      root: headerFixture.validationTracesRoot,
      phas_root: "72".repeat(32),
      count: 1n,
      key: eventKeys[2]!,
      value: validationDescriptor,
      proof,
    },
    transition_step_membership: traceProof,
    event_to_step_membership: eventToStepMembership,
    source_membership: {
      NormalValidationSource: {
        membership: {
          ...l2SourceMembership,
          value: l2TransactionSourceFixture,
        },
      },
    },
    validation_context_cbor: "80",
    initial_state: validationState,
    terminal_state: validationState,
    initial_state_proof: validationProof,
    terminal_state_proof: validationProof,
  };
  transitionFaults["transition-fault.accepted-transaction-transition"] =
    SDK.acceptedTransactionTransitionMismatchFault({
      claim: acceptedTransactionClaim,
      terminalAcceptanceWitnessCbor: "80",
    });
  const fixtures: Record<string, AbiFixtureValue> = {
    HeaderV1: {
      schemaName: "HeaderV1",
      value: headerFixture,
      schema: SDK.Header,
    },
    ForcedInclusionTxV1: {
      schemaName: "ForcedInclusionTxV1",
      value: forcedInclusionTxFixture,
      schema: SDK.ForcedInclusionTxV1,
    },
    TransitionStep: {
      schemaName: "TransitionStep",
      value: transitionStepFixture,
      schema: SDK.TransitionStep,
    },
    "EventKey.withdrawal": {
      schemaName: "EventKey",
      value: eventKeys[0]!,
      schema: SDK.EventKey,
    },
    "EventKey.forced": {
      schemaName: "EventKey",
      value: eventKeys[1]!,
      schema: SDK.EventKey,
    },
    "EventKey.l2": {
      schemaName: "EventKey",
      value: eventKeys[2]!,
      schema: SDK.EventKey,
    },
    "EventKey.deposit": {
      schemaName: "EventKey",
      value: eventKeys[3]!,
      schema: SDK.EventKey,
    },
    EventToStepValue: {
      schemaName: "EventToStepValue",
      value: eventToStepValueFixture,
      schema: SDK.EventToStepValue,
    },
    DaPayloadBodyV1: {
      schemaName: "DaPayloadBody",
      value: daPayloadBodyFixture,
      schema: SDK.DaPayloadBody,
    },
    TransitionTraceRouteSpendRedeemerCancel: {
      schemaName: "TransitionTraceRouteSpendRedeemer",
      value: {
        Cancel: {
          input_index: 0n,
          computation_thread_mint_redeemer_index: 1n,
        },
      } satisfies SDK.TransitionTraceRouteSpendRedeemer,
      schema: SDK.TransitionTraceRouteSpendRedeemer,
    },
    TransitionTraceFinalSpendRedeemerContinue: {
      schemaName: "TransitionTraceFinalSpendRedeemer",
      value: {
        Continue: [
          {
            input_index: 0n,
            output_index: 1n,
            hub_ref_input_index: 2n,
            fraud_proof_mint_redeemer_index: 3n,
          },
        ],
      } satisfies SDK.TransitionTraceFinalSpendRedeemer,
      schema: SDK.TransitionTraceFinalSpendRedeemer,
    },
  };

  for (const [name, fault] of Object.entries(transitionFaults)) {
    fixtures[`${name}.fault`] = {
      schemaName: "TransitionFault",
      value: fault,
      schema: SDK.TransitionFault,
    };
    fixtures[`${name}.proof`] = {
      schemaName: "TransitionFaultProof",
      value: proofFor(fault),
      schema: SDK.TransitionFaultProof,
    };
    fixtures[`${name}.continue-redeemer`] = {
      schemaName: "TransitionTraceRouteSpendRedeemer",
      value: {
        Continue: [routeArgsFor(fault)],
      } satisfies SDK.TransitionTraceRouteSpendRedeemer,
      schema: SDK.TransitionTraceRouteSpendRedeemer,
    };
  }

  return fixtures;
};
