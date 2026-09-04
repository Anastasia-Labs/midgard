import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import * as SDK from "@al-ft/midgard-sdk";
import { Data, validatorToScriptHash } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

type BlueprintConstructor = {
  readonly title: string;
  readonly index: number;
  readonly fields?: readonly {
    readonly title?: string;
    readonly $ref?: string;
  }[];
};

type BlueprintDefinition = {
  readonly anyOf?: readonly BlueprintConstructor[];
};

type Blueprint = {
  readonly definitions: Record<string, BlueprintDefinition>;
};

type GoldenAbiFixture = {
  readonly schema: string;
  readonly cborHex: string;
  readonly byteLength: number;
  readonly sha256: string;
};

type GoldenAbiFixtureFile = {
  readonly version: number;
  readonly encoding: "lucid-plutus-data-cbor-hex";
  readonly fixtures: Record<string, GoldenAbiFixture>;
};

const testDir = path.dirname(fileURLToPath(import.meta.url));
const repoRoot = path.resolve(testDir, "../../..");
const transitionTraceAbiGoldenPath = path.join(
  testDir,
  "fixtures/transition-trace-abi.json",
);
const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  path.join(repoRoot, "onchain/aiken/plutus.json");
const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as Blueprint;
const testnetEnv = readFileSync(
  path.join(repoRoot, "onchain/aiken/env/testnet.ak"),
  "utf8",
);
const ledgerStateSource = readFileSync(
  path.join(repoRoot, "onchain/aiken/lib/midgard/ledger-state.ak"),
  "utf8",
);
const transitionTraceAbiGolden = JSON.parse(
  readFileSync(transitionTraceAbiGoldenPath, "utf8"),
) as GoldenAbiFixtureFile;

const definition = (name: string): BlueprintDefinition => {
  const found = blueprint.definitions[name];
  expect(found, `missing blueprint definition ${name}`).toBeDefined();
  return found;
};

const constructor = (
  definitionName: string,
  constructorName: string,
): BlueprintConstructor => {
  const found = definition(definitionName).anyOf?.find(
    (candidate) => candidate.title === constructorName,
  );
  expect(
    found,
    `missing ${constructorName} constructor in ${definitionName}`,
  ).toBeDefined();
  return found!;
};

const fields = (ctor: BlueprintConstructor): readonly string[] =>
  (ctor.fields ?? []).map((field) => field.title ?? "");

const aikenIntegerConst = (
  source: string,
  sourceLabel: string,
  name: string,
): bigint => {
  const match = source.match(
    new RegExp(`pub const ${name}: [^=]+=([\\d_\\s*]+)`, "m"),
  );
  expect(match, `missing ${sourceLabel} Aiken const ${name}`).toBeDefined();
  const expression = match![1]!.trim().replace(/\s+/g, " ");
  expect(expression, `unsupported expression for ${name}`).toMatch(
    /^[\d_]+(?: \* [\d_]+)*$/,
  );
  return expression
    .split(" * ")
    .map((term) => BigInt(term.replaceAll("_", "")))
    .reduce((acc, term) => acc * term, 1n);
};

const testnetIntegerConst = (name: string): bigint =>
  aikenIntegerConst(testnetEnv, "testnet", name);

const ledgerStateIntegerConst = (name: string): bigint =>
  aikenIntegerConst(ledgerStateSource, "ledger-state", name);

const h28 = "11".repeat(28);
const h32 = "22".repeat(32);
const h64 = "33".repeat(64);
const outputReference: SDK.OutputReference = {
  transactionId: h32,
  outputIndex: 0n,
};
const address: SDK.AddressData = {
  paymentCredential: { PublicKeyCredential: [h28] },
  stakeCredential: null,
};
const value: SDK.Value = new Map([["", new Map([["", 1n]])]]);
const proof: SDK.Proof = [];
const transitionPhases: readonly SDK.TransitionPhase[] = [
  "Withdrawal",
  "ForcedTransaction",
  "L2Transaction",
  "Deposit",
];
const eventKeys: readonly SDK.EventKey[] = [
  { WithdrawalEventKey: { withdrawal_id: outputReference } },
  { ForcedTransactionEventKey: { tx_order_id: outputReference } },
  { L2TransactionEventKey: { tx_id: h32 } },
  { DepositEventKey: { deposit_id: outputReference } },
];

const headerFixture: SDK.Header = {
  prevUtxosRoot: h32,
  utxosRoot: "44".repeat(32),
  withdrawalsRoot: "77".repeat(32),
  forcedTransactionsRoot: "78".repeat(32),
  transactionsRoot: "79".repeat(32),
  depositsRoot: "80".repeat(32),
  transitionTraceRoot: "55".repeat(32),
  eventToStepRoot: "88".repeat(32),
  validationTracesRoot: "89".repeat(32),
  withdrawalCount: 1n,
  forcedTransactionCount: 1n,
  l2TransactionCount: 1n,
  depositCount: 1n,
  totalEventCount: 4n,
  transitionStepCount: 4n,
  validationTraceCount: 4n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: h28,
  operatorVkey: h28,
  protocolVersion: 1n,
};

const forcedInclusionTxFixture: SDK.ForcedInclusionTxV1 = {
  tx_id: h32,
  source: {
    compact_cbor: "80",
    witness_set_compact_cbor: "81",
    field_preimage_lengths_cbor: "82",
  },
  verdict: {
    ForcedTxInvalid: {
      reason: { PlutusExecutionFailed: { execution_index: 0n } },
    },
  },
};

const l2TransactionSourceFixture: SDK.L2TransactionSource = {
  tx_id: h32,
  source: {
    compact_cbor: "80",
    witness_set_compact_cbor: "81",
    field_preimage_lengths_cbor: "82",
  },
};

const transitionStepFixture: SDK.TransitionStep = {
  schema_version: 1n,
  step_index: 0n,
  event_key: eventKeys[0]!,
  phase: "Withdrawal",
  pre_utxos_root: h32,
  post_utxos_root: "44".repeat(32),
};

const secondTransitionStepFixture: SDK.TransitionStep = {
  schema_version: 1n,
  step_index: 1n,
  event_key: eventKeys[1]!,
  phase: "ForcedTransaction",
  pre_utxos_root: "44".repeat(32),
  post_utxos_root: "55".repeat(32),
};

const eventToStepValueFixture: SDK.EventToStepValue = {
  step_index: 0n,
  phase: "Withdrawal",
};

const daPayloadBodyFixture: SDK.DaPayloadBody = {
  header_hash: h28,
  header: headerFixture,
  utxos: [["01", "02"]],
  withdrawals: [["03", "04"]],
  forced_transactions: [["05", "06"]],
  transactions: [["07", "08"]],
  deposits: [["09", "0a"]],
  transition_trace: [["0b", "0c"]],
  event_to_step: [["0d", "0e"]],
  transaction_preimages: [["0f", "10"]],
  forced_transaction_preimages: [["11", "12"]],
  cek_program_material: [["13", "14"]],
  validation_traces: [["15", "16"]],
  validation_trace_witnesses: [],
  counts: {
    withdrawalCount: 1n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 1n,
    depositCount: 1n,
    totalEventCount: 4n,
    transitionStepCount: 4n,
    validationTraceCount: 1n,
  },
};

const roundTrip = <T>(value: T, schema: unknown): T =>
  Data.from(Data.to(value as any, schema as any), schema as any) as T;

const expectRoundTrip = <T>(value: T, schema: unknown): void =>
  expect(roundTrip(value, schema)).toEqual(value);

const encodedFixture = (
  value: unknown,
  schema: unknown,
): Omit<GoldenAbiFixture, "schema"> => {
  const cborHex = Data.to(value as never, schema as never);
  return {
    cborHex,
    byteLength: Buffer.byteLength(cborHex, "hex"),
    sha256: createHash("sha256")
      .update(Buffer.from(cborHex, "hex"))
      .digest("hex"),
  };
};

const expectGoldenFixture = ({
  name,
  schemaName,
  value,
  schema,
}: {
  readonly name: string;
  readonly schemaName: string;
  readonly value: unknown;
  readonly schema: unknown;
}): void => {
  const expected = transitionTraceAbiGolden.fixtures[name];
  expect(
    expected,
    `missing transition trace ABI fixture ${name}`,
  ).toBeDefined();
  expect(expected).toEqual({
    schema: schemaName,
    ...encodedFixture(value, schema),
  });
  expect(Data.from(expected!.cborHex, schema as never)).toEqual(value);
};

type AbiFixtureValue = {
  readonly schemaName: string;
  readonly value: unknown;
  readonly schema: unknown;
};

const rootCountProof = (
  domain: SDK.RootDomain,
  root: string,
  phasRoot: string,
  count: bigint,
): SDK.RootCountProof => ({
  domain,
  root,
  phas_root: phasRoot,
  count,
});

const buildTransitionTraceAbiFixtures = (): Record<string, AbiFixtureValue> => {
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
          event_ref_input_index: 0n,
          event_asset_name: "aa",
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
        event_ref_input_index: 0n,
        event_asset_name: "aa",
        source_non_membership: depositSourceNonMembership,
      },
    }),
    "transition-fault.omitted-withdrawal": SDK.omittedDueL1EventFault({
      OmittedDueWithdrawal: {
        event_ref_input_index: 1n,
        event_asset_name: "bb",
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
        event_ref_input_index: 0n,
        event_asset_name: "aa",
        source_membership: depositSourceMembership,
      },
    }),
    "transition-fault.out-of-window-withdrawal":
      SDK.outOfWindowSourceEventFault({
        OutOfWindowWithdrawal: {
          event_ref_input_index: 1n,
          event_asset_name: "bb",
          validity_override: "IncorrectWithdrawalSignature",
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

describe("SDK canonical ABI fixtures", () => {
  it("keeps SDK protocol timing constants aligned with canonical Aiken values", () => {
    expect(SDK.SHIFT_DURATION_MS).toBe(testnetIntegerConst("shift_duration"));
    expect(SDK.REGISTRATION_DURATION_MS).toBe(
      testnetIntegerConst("registration_duration"),
    );
    expect(SDK.MATURITY_DURATION_MS).toBe(
      ledgerStateIntegerConst("block_maturity_duration_v1"),
    );
    expect(SDK.USER_EVENTS_NEGLIGENCE_TIMEOUT_MS).toBe(
      testnetIntegerConst("user_events_negligence_timeout"),
    );
    expect(SDK.MAX_INACTIVITY_BETWEEN_BLOCK_COMMITMENTS_MS).toBe(
      testnetIntegerConst("max_inactivity_between_block_commitments"),
    );
    expect(SDK.NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS).toBe(
      testnetIntegerConst("new_shift_inactivity_grace_period"),
    );
    expect(SDK.MAX_VALIDITY_RANGE_LENGTH_MS).toBe(
      testnetIntegerConst("max_validity_range_length"),
    );
    expect(SDK.MAX_INACTIVITY_STRIKES).toBe(
      testnetIntegerConst("max_inactivity_strikes"),
    );
    expect(BigInt(SDK.EVENT_WAIT_DURATION_MS)).toBe(
      testnetIntegerConst("event_wait_duration"),
    );
  });

  it("tracks canonical Aiken datum and redeemer field names", () => {
    expect(
      fields(constructor("midgard/scheduler/SchedDatum", "ActiveOperator")),
    ).toEqual(["operator", "start_time"]);
    expect(
      constructor("midgard/scheduler/SchedDatum", "NoActiveOperators").index,
    ).toBe(0);
    expect(
      fields(constructor("midgard/ledger_state/DepositInfo", "DepositInfo")),
    ).toEqual(["l2_address", "l2_network_id", "l2_datum"]);
    expect(
      fields(
        constructor(
          "midgard/state_queue/MintRedeemer",
          "MergeToConfirmedStateV1",
        ),
      ),
    ).toEqual([
      "yield_to_ref_input_index",
      "header_node_key",
      "confirmed_state_input_outref",
      "confirmed_state_output_index",
      "m_settlement_redeemer_index",
      "merged_block_withdrawals_root",
      "merged_block_forced_transactions_root",
      "merged_block_transactions_root",
      "merged_block_deposits_root",
      "merged_block_transition_trace_root",
      "merged_block_event_to_step_root",
      "merged_block_validation_traces_root",
      "merged_block_withdrawal_count",
      "merged_block_forced_transaction_count",
      "merged_block_l2_transaction_count",
      "merged_block_deposit_count",
      "merged_block_total_event_count",
      "merged_block_transition_step_count",
      "merged_block_validation_trace_count",
    ]);
    expect(
      fields(constructor("midgard/settlement/MintRedeemer", "Spawn")),
    ).toEqual([
      "settlement_id",
      "output_index",
      "state_queue_merge_redeemer_index",
      "hub_ref_input_index",
    ]);
    expect(fields(constructor("midgard/settlement/Datum", "Datum"))).toEqual([
      "deposits_root",
      "withdrawals_root",
      "forced_transactions_root",
      "transactions_root",
      "resolution_claim",
    ]);
    expect(
      fields(
        constructor("midgard/user_events/deposit/DepositDatum", "DepositDatum"),
      ),
    ).toEqual(["event", "inclusion_time", "witness"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/proof/TransitionFaultProof",
          "TransitionFaultProof",
        ),
      ),
    ).toEqual(["challenged_header_hash", "header", "fault"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/proof/TransitionFault",
          "TraceBoundaryFault",
        ),
      ),
    ).toEqual(["side", "trace_proof"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/proof/SourceMembershipMismatchWitness",
          "MappedEventMissingFromSource",
        ),
      ),
    ).toEqual(["trace_proof", "event_to_step", "source_non_membership"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/proof/OmittedDueL1EventWitness",
          "OmittedDueDeposit",
        ),
      ),
    ).toEqual([
      "event_ref_input_index",
      "event_asset_name",
      "source_non_membership",
    ]);
    expect(
      fields(
        constructor("fraud_proofs/transition_trace/route_v1/Args", "Args"),
      ),
    ).toEqual(["input_index", "output_index", "proof"]);
    expect(
      fields(
        constructor(
          "midgard/fraud_proofs/transition_trace/final_v1/Args",
          "Args",
        ),
      ),
    ).toEqual([
      "input_index",
      "output_index",
      "hub_ref_input_index",
      "fraud_proof_mint_redeemer_index",
    ]);
    expect(
      fields(
        constructor(
          "midgard/user_events/withdrawal/SpendRedeemer",
          "SpendRedeemer",
        ),
      ),
    ).toContain("purpose");
    expect(
      constructor(
        "midgard/ledger_state/WithdrawalValidity",
        "UnpayableWithdrawalValue",
      ).index,
    ).toBe(7);

    expect(
      fields(constructor("midgard/payout/MintRedeemer", "MintPayout")),
    ).toEqual([
      "withdrawal_utxo_out_ref",
      "withdrawal_input_index",
      "withdrawal_spend_redeemer_index",
      "hub_ref_input_index",
    ]);
    expect(
      fields(constructor("midgard/payout/MintRedeemer", "BurnPayout")),
    ).toEqual([
      "payout_input_index",
      "payout_asset_name",
      "payout_spend_redeemer_index",
      "hub_ref_input_index",
    ]);
    const addFundsFields = fields(
      constructor("midgard/payout/SpendRedeemer", "AddFunds"),
    );
    expect(addFundsFields).toEqual([
      "payout_input_index",
      "payout_output_index",
      "reserve_input_index",
      "reserve_change_output_index",
      "reserve_spend_redeemer_index",
      "payout_spend_redeemer_index",
      "hub_ref_input_index",
    ]);
    expect(addFundsFields).not.toContain("settlement_ref_input_index");
    expect(addFundsFields).not.toContain("membership_proof");
    const concludeFields = fields(
      constructor("midgard/payout/SpendRedeemer", "ConcludeWithdrawal"),
    );
    expect(concludeFields).toEqual([
      "payout_input_index",
      "l1_output_index",
      "burn_redeemer_index",
      "hub_ref_input_index",
    ]);
    expect(concludeFields).not.toContain("settlement_ref_input_index");
    expect(concludeFields).not.toContain("membership_proof");
    expect(
      fields(constructor("midgard/reserve/SpendRedeemer", "Spend")),
    ).toEqual([
      "reserve_input_index",
      "payout_input_index",
      "payout_spend_redeemer_index",
      "hub_ref_input_index",
    ]);
  });

  it("matches transition trace golden ABI fixture files", () => {
    expect(transitionTraceAbiGolden.version).toBe(1);
    expect(transitionTraceAbiGolden.encoding).toBe(
      "lucid-plutus-data-cbor-hex",
    );

    const fixtures = buildTransitionTraceAbiFixtures();
    if (process.env.UPDATE_TRANSITION_TRACE_ABI_FIXTURE === "1") {
      const regenerated: GoldenAbiFixtureFile = {
        version: 1,
        encoding: "lucid-plutus-data-cbor-hex",
        fixtures: Object.fromEntries(
          Object.entries(fixtures).map(([name, fixture]) => {
            try {
              return [
                name,
                {
                  schema: fixture.schemaName,
                  ...encodedFixture(fixture.value, fixture.schema),
                },
              ];
            } catch (error) {
              throw new Error(
                `failed to encode transition trace ABI fixture ${name}`,
                { cause: error },
              );
            }
          }),
        ),
      };
      writeFileSync(
        transitionTraceAbiGoldenPath,
        `${JSON.stringify(regenerated, null, 2)}\n`,
      );
      Object.assign(transitionTraceAbiGolden, regenerated);
    }
    expect(Object.keys(transitionTraceAbiGolden.fixtures).sort()).toEqual(
      Object.keys(fixtures).sort(),
    );
    for (const [name, fixture] of Object.entries(fixtures)) {
      expectGoldenFixture({
        name,
        schemaName: fixture.schemaName,
        value: fixture.value,
        schema: fixture.schema,
      });
    }
  });

  it("encodes scheduler, hub-oracle, state-queue, and operator redeemers", () => {
    expectRoundTrip("NoActiveOperators", SDK.SchedulerDatum);
    expectRoundTrip(
      { ActiveOperator: { operator: h28, start_time: 10n } },
      SDK.SchedulerDatum,
    );

    const hubOracleDatum: SDK.HubOracleDatum = {
      registered_operators: h28,
      active_operators: h28,
      retired_operators: h28,
      scheduler: h28,
      state_queue: h28,
      fraud_proof_catalogue: h28,
      fraud_proof: h28,
      deposit: h28,
      withdrawal: h28,
      tx_order: h28,
      settlement: h28,
      payout: h28,
      registered_operators_addr: address,
      active_operators_addr: address,
      retired_operators_addr: address,
      scheduler_addr: address,
      state_queue_addr: address,
      fraud_proof_catalogue_addr: address,
      fraud_proof_addr: address,
      deposit_addr: address,
      withdrawal_addr: address,
      tx_order_addr: address,
      settlement_addr: address,
      reserve_addr: address,
      payout_addr: address,
      reserve_observer: h28,
    };
    expect(Object.keys(roundTrip(hubOracleDatum, SDK.HubOracleDatum))).toEqual([
      "registered_operators",
      "active_operators",
      "retired_operators",
      "scheduler",
      "state_queue",
      "fraud_proof_catalogue",
      "fraud_proof",
      "deposit",
      "withdrawal",
      "tx_order",
      "settlement",
      "payout",
      "registered_operators_addr",
      "active_operators_addr",
      "retired_operators_addr",
      "scheduler_addr",
      "state_queue_addr",
      "fraud_proof_catalogue_addr",
      "fraud_proof_addr",
      "deposit_addr",
      "withdrawal_addr",
      "tx_order_addr",
      "settlement_addr",
      "reserve_addr",
      "payout_addr",
      "reserve_observer",
    ]);

    expectRoundTrip({ InitV1: { output_index: 2n } }, SDK.StateQueueRedeemer);
    expectRoundTrip("LinkedListMutation", SDK.StateQueueSpendRedeemer);
    expect(
      roundTrip(
        {
          CommitBlockHeader: {
            yield_to_ref_input_index: 0n,
            new_block_output_index: 1n,
            continued_latest_block_output_index: 2n,
            operator: h28,
            scheduler_ref_input_index: 0n,
            active_operators_input_index: 1n,
            active_operators_redeemer_index: 1n,
            m_confirmed_state_ref_input_index: null,
            m_head_state_queue_node_ref_input_index: null,
          },
        },
        SDK.StateQueueRedeemer,
      ),
    ).toMatchObject({ CommitBlockHeader: { operator: h28 } });
    expect(
      roundTrip(
        {
          MergeToConfirmedStateV1: {
            yield_to_ref_input_index: 0n,
            header_node_key: h28,
            confirmed_state_input_outref: outputReference,
            confirmed_state_output_index: 0n,
            m_settlement_redeemer_index: 2n,
            merged_block_withdrawals_root: h32,
            merged_block_forced_transactions_root: h32,
            merged_block_transactions_root: h32,
            merged_block_deposits_root: h32,
            merged_block_transition_trace_root: h32,
            merged_block_event_to_step_root: h32,
            merged_block_validation_traces_root: h32,
            merged_block_withdrawal_count: 1n,
            merged_block_forced_transaction_count: 2n,
            merged_block_l2_transaction_count: 3n,
            merged_block_deposit_count: 4n,
            merged_block_total_event_count: 10n,
            merged_block_transition_step_count: 10n,
            merged_block_validation_trace_count: 10n,
          },
        },
        SDK.StateQueueRedeemer,
      ),
    ).toMatchObject({ MergeToConfirmedStateV1: { header_node_key: h28 } });

    expect(
      roundTrip(
        {
          RegisterOperator: {
            registering_operator: h28,
            root_output_index: 0n,
            registered_node_output_index: 1n,
            hub_oracle_ref_input_index: 0n,
            active_operators_element_ref_input_index: 1n,
            retired_operators_element_ref_input_index: 2n,
          },
        },
        SDK.RegisteredOperatorMintRedeemer,
      ),
    ).toMatchObject({ RegisterOperator: { registering_operator: h28 } });
    expect(
      roundTrip(
        {
          ActivateOperator: {
            new_active_operator_key: h28,
            active_operator_anchor_element_output_index: 0n,
            active_operator_inserted_node_output_index: 1n,
            registered_operators_redeemer_index: 2n,
            active_operators_set_was_empty: true,
          },
        },
        SDK.ActiveOperatorMintRedeemer,
      ),
    ).toMatchObject({ ActivateOperator: { new_active_operator_key: h28 } });
  });

  it("encodes user-event witness, user-event spend, settlement, and fraud-proof fixtures", () => {
    const aikenWitnessPrefix = readFileSync(
      path.join(repoRoot, "onchain/aiken/env/testnet.ak"),
      "utf8",
    ).match(
      /pub const user_events_witness_script_prefix: ByteArray =\n {2}#"([^"]+)"/,
    )?.[1];
    expect(SDK.USER_EVENT_WITNESS_SCRIPT_PREFIX).toBe(aikenWitnessPrefix);

    const witnessValidator = SDK.buildUserEventWitnessCertificateValidator(h32);
    expect(SDK.userEventWitnessScriptHash(h32)).toBe(
      validatorToScriptHash(witnessValidator),
    );
    expect(
      Data.from(
        SDK.encodeUserEventWitnessMintOrBurnRedeemer(h28),
        SDK.UserEventWitnessPublishRedeemer,
      ),
    ).toEqual({ MintOrBurn: { targetPolicy: h28 } });
    const depositDatum: SDK.DepositDatum = {
      event: {
        id: { transactionId: h32, outputIndex: 0n },
        info: { l2_address: address, l2_network_id: 0n, l2_datum: null },
      },
      inclusion_time: 123n,
      witness: h28,
    };
    expectRoundTrip(depositDatum, SDK.DepositDatum);
    const depositMembershipProof: SDK.RawRootMembershipProof = {
      domain: SDK.ROOT_DOMAINS.deposits,
      root: "44".repeat(32),
      phas_root: "55".repeat(32),
      count: 1n,
      key: Data.to(depositDatum.event.id, SDK.OutputReference),
      value: Data.to(depositDatum.event.info, SDK.DepositInfo),
      proof,
    };
    expectRoundTrip(
      {
        domain: depositMembershipProof.domain,
        root: depositMembershipProof.root,
        phas_root: depositMembershipProof.phas_root,
        count: depositMembershipProof.count,
      },
      SDK.RootCountProof,
    );
    expectRoundTrip(depositMembershipProof, SDK.RawRootMembershipProof);
    expect(
      roundTrip(
        {
          input_index: 0n,
          output_index: 0n,
          hub_ref_input_index: 1n,
          settlement_ref_input_index: 2n,
          mint_redeemer_index: 3n,
          membership_proof: depositMembershipProof,
          inclusion_proof_script_withdraw_redeemer_index: 4n,
        },
        SDK.DepositSpendRedeemer,
      ),
    ).toMatchObject({ input_index: 0n });

    const withdrawalDatum: SDK.WithdrawalOrderDatum = {
      event: {
        id: { transactionId: h32, outputIndex: 0n },
        info: {
          body: {
            l2_outref: { transactionId: h32, outputIndex: 1n },
            l2_owner: h28,
            l2_value: value,
            l1_address: address,
            l1_datum: "NoDatum",
          },
          signature: [h32, h64],
          validity: "WithdrawalIsValid",
        },
      },
      inclusion_time: 123n,
      witness: h28,
      refund_address: address,
      refund_datum: "NoDatum",
    };
    expectRoundTrip(withdrawalDatum, SDK.WithdrawalOrderDatum);
    const withdrawalMembershipValue: SDK.WithdrawalInfo = {
      ...withdrawalDatum.event.info,
      validity: {
        SpentWithdrawalUtxo: { l2_tx_id: h32 },
      },
    };
    const withdrawalMembershipProof: SDK.RawRootMembershipProof = {
      domain: SDK.ROOT_DOMAINS.withdrawals,
      root: "88".repeat(32),
      phas_root: "99".repeat(32),
      count: 1n,
      key: Data.to(withdrawalDatum.event.id, SDK.OutputReference),
      value: Data.to(withdrawalMembershipValue, SDK.WithdrawalInfo),
      proof,
    };
    expect(
      roundTrip(
        {
          input_index: 0n,
          output_index: 0n,
          hub_ref_input_index: 1n,
          settlement_ref_input_index: 2n,
          burn_redeemer_index: 3n,
          payout_mint_redeemer_index: 4n,
          membership_proof: withdrawalMembershipProof,
          inclusion_proof_script_withdraw_redeemer_index: 5n,
          purpose: {
            Refund: {
              validity_override: {
                SpentWithdrawalUtxo: { l2_tx_id: h32 },
              },
            },
          },
        },
        SDK.WithdrawalSpendRedeemer,
      ),
    ).toMatchObject({ purpose: { Refund: expect.any(Object) } });
    expectRoundTrip("UnpayableWithdrawalValue", SDK.WithdrawalValidity);

    expect(
      roundTrip(
        {
          deposits_root: h32,
          withdrawals_root: h32,
          forced_transactions_root: h32,
          transactions_root: h32,
          resolution_claim: null,
        },
        SDK.SettlementDatum,
      ),
    ).toMatchObject({ resolution_claim: null });
    expectRoundTrip(
      {
        ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
        prevUtxosRoot: h32,
        utxosRoot: h32,
        withdrawalsRoot: h32,
        transactionsRoot: h32,
        depositsRoot: h32,
        startTime: 1n,
        endTime: 2n,
        blockSlot: 0n,
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        prevHeaderHash: h28,
        operatorVkey: h28,
        protocolVersion: 1n,
      },
      SDK.Header,
    );
    const forcedInclusionTx: SDK.ForcedInclusionTxV1 = {
      ...forcedInclusionTxFixture,
      verdict: "ForcedTxValid",
    };
    expectRoundTrip(forcedInclusionTx, SDK.ForcedInclusionTxV1);
    expect(
      Object.keys(roundTrip(forcedInclusionTx, SDK.ForcedInclusionTxV1)),
    ).toEqual(["tx_id", "source", "verdict"]);
    for (const phase of transitionPhases) {
      expectRoundTrip(phase, SDK.TransitionPhase);
      expectRoundTrip({ step_index: 1n, phase }, SDK.EventToStepValue);
    }
    for (const [index, eventKey] of eventKeys.entries()) {
      const phase = transitionPhases[index]!;
      expectRoundTrip(eventKey, SDK.EventKey);
      expectRoundTrip(
        {
          schema_version: 1n,
          step_index: BigInt(index),
          event_key: eventKey,
          phase,
          pre_utxos_root: h32,
          post_utxos_root: "44".repeat(32),
        },
        SDK.TransitionStep,
      );
    }
    const transitionStep: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKeys[0]!,
      phase: "Withdrawal",
      pre_utxos_root: h32,
      post_utxos_root: "44".repeat(32),
    };
    const traceProof: SDK.IndexedTraceProof = {
      domain: SDK.ROOT_DOMAINS.transitionTrace,
      root: "55".repeat(32),
      phas_root: "66".repeat(32),
      count: 1n,
      key: 0n,
      value: transitionStep,
      proof,
    };
    const transitionHeader: SDK.Header = {
      ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
      prevUtxosRoot: h32,
      utxosRoot: "44".repeat(32),
      withdrawalsRoot: "77".repeat(32),
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transitionTraceRoot: traceProof.root,
      eventToStepRoot: "88".repeat(32),
      withdrawalCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
      startTime: 1n,
      endTime: 2n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: h28,
      operatorVkey: h28,
      protocolVersion: 1n,
    };
    const transitionFaultProof = SDK.makeTransitionFaultProof({
      challengedHeaderHash: h28,
      header: transitionHeader,
      fault: SDK.traceBoundaryFault({
        side: "TraceStart",
        traceProof,
      }),
    });
    expectRoundTrip(transitionFaultProof, SDK.TransitionFaultProof);
    expectRoundTrip(
      {
        Continue: [
          {
            input_index: 0n,
            output_index: 1n,
            proof: transitionFaultProof,
          },
        ],
      },
      SDK.TransitionTraceRouteSpendRedeemer,
    );
    expect(
      SDK.transitionTraceThreadAssetName({
        fraudCategoryId: "00000004",
        challengedHeaderHash: h28,
      }),
    ).toBe(`00000004${h28}`);
    expect(
      roundTrip(
        {
          Spawn: {
            settlement_id: h28,
            output_index: 0n,
            state_queue_merge_redeemer_index: 1n,
            hub_ref_input_index: 2n,
          },
        },
        SDK.SettlementMintRedeemer,
      ),
    ).toMatchObject({ Spawn: { settlement_id: h28 } });
    expectRoundTrip("Init", SDK.FraudProofCatalogueMintRedeemer);
  });

  it("commits every transition field into the block header hash", async () => {
    const header: SDK.Header = {
      prevUtxosRoot: h32,
      utxosRoot: h32,
      withdrawalsRoot: h32,
      forcedTransactionsRoot: h32,
      transactionsRoot: h32,
      depositsRoot: h32,
      transitionTraceRoot: h32,
      eventToStepRoot: h32,
      validationTracesRoot: h32,
      withdrawalCount: 1n,
      forcedTransactionCount: 2n,
      l2TransactionCount: 3n,
      depositCount: 4n,
      totalEventCount: 10n,
      transitionStepCount: 10n,
      validationTraceCount: 10n,
      startTime: 1n,
      endTime: 2n,
      blockSlot: 0n,
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      prevHeaderHash: h28,
      operatorVkey: h28,
      protocolVersion: 1n,
    };
    const baselineHash = await Effect.runPromise(SDK.hashBlockHeader(header));
    expect(baselineHash).toBe(
      "964baf9a89b4c4aa99d8cb6f1b365af9fa951a4d8043a93c5da993c1",
    );
    const differentRoot = "44".repeat(32);
    const mutations: readonly SDK.Header[] = [
      { ...header, forcedTransactionsRoot: differentRoot },
      { ...header, transitionTraceRoot: differentRoot },
      { ...header, eventToStepRoot: differentRoot },
      { ...header, withdrawalCount: header.withdrawalCount + 1n },
      {
        ...header,
        forcedTransactionCount: header.forcedTransactionCount + 1n,
      },
      { ...header, l2TransactionCount: header.l2TransactionCount + 1n },
      { ...header, depositCount: header.depositCount + 1n },
      { ...header, totalEventCount: header.totalEventCount + 1n },
      { ...header, transitionStepCount: header.transitionStepCount + 1n },
      { ...header, blockSlot: header.blockSlot + 1n },
      {
        ...header,
        expectedNetworkId: header.expectedNetworkId === 0n ? 1n : 0n,
      },
      { ...header, minFeeA: header.minFeeA + 1n },
      { ...header, minFeeB: header.minFeeB + 1n },
    ];

    await Promise.all(
      mutations.map(async (mutation) => {
        await expect(
          Effect.runPromise(SDK.hashBlockHeader(mutation)),
        ).resolves.not.toBe(baselineHash);
      }),
    );
  });

  it("validates transition commitment count and root invariants", async () => {
    await expect(
      Effect.runPromise(
        SDK.makeHeaderTransitionCommitmentsProgram({
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: 0n,
          depositCount: 0n,
          validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    ).resolves.toEqual(SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS);

    const nonEmptyDepositWithoutTrace = await Effect.runPromise(
      Effect.either(
        SDK.makeHeaderTransitionCommitmentsProgram({
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: h32,
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: 0n,
          depositCount: 1n,
          validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    );
    expect(nonEmptyDepositWithoutTrace._tag).toBe("Left");
    if (nonEmptyDepositWithoutTrace._tag === "Left") {
      expect(nonEmptyDepositWithoutTrace.left.message).toContain(
        "empty transition_trace_root",
      );
    }

    await expect(
      Effect.runPromise(
        SDK.makeHeaderTransitionCommitmentsProgram({
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: h32,
          transitionTraceRoot: "44".repeat(32),
          eventToStepRoot: "55".repeat(32),
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: 0n,
          depositCount: 1n,
          validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    ).resolves.toMatchObject({
      depositCount: 1n,
      totalEventCount: 1n,
      transitionStepCount: 1n,
    });

    const zeroCountForNonEmptyRoot = await Effect.runPromise(
      Effect.either(
        SDK.makeHeaderTransitionCommitmentsProgram({
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: h32,
          depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          withdrawalCount: 0n,
          forcedTransactionCount: 0n,
          l2TransactionCount: 0n,
          depositCount: 0n,
          validationTracesRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          validationTraceCount: 0n,
        }),
      ),
    );
    expect(zeroCountForNonEmptyRoot._tag).toBe("Left");

    const negativeCount = await Effect.runPromise(
      Effect.either(
        SDK.validateHeaderTransitionCommitmentsProgram({
          ...SDK.EMPTY_HEADER_TRANSITION_COMMITMENTS,
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          withdrawalCount: -1n,
        }),
      ),
    );
    expect(negativeCount._tag).toBe("Left");
  });

  it("encodes reserve and datum-based payout fixtures", () => {
    const payoutDatum: SDK.PayoutDatum = {
      l2_value: value,
      l1_address: address,
      l1_datum: "NoDatum",
    };
    expectRoundTrip(payoutDatum, SDK.PayoutDatum);

    expectRoundTrip(
      {
        MintPayout: {
          withdrawal_utxo_out_ref: { transactionId: h32, outputIndex: 0n },
          withdrawal_input_index: 1n,
          withdrawal_spend_redeemer_index: 2n,
          hub_ref_input_index: 3n,
        },
      },
      SDK.PayoutMintRedeemer,
    );

    expectRoundTrip(
      {
        BurnPayout: {
          payout_input_index: 0n,
          payout_asset_name: h32,
          payout_spend_redeemer_index: 1n,
          hub_ref_input_index: 2n,
        },
      },
      SDK.PayoutMintRedeemer,
    );

    expectRoundTrip(
      {
        AddFunds: {
          payout_input_index: 0n,
          payout_output_index: 1n,
          reserve_input_index: 2n,
          reserve_change_output_index: null,
          reserve_spend_redeemer_index: 3n,
          payout_spend_redeemer_index: 4n,
          hub_ref_input_index: 5n,
        },
      },
      SDK.PayoutSpendRedeemer,
    );

    expectRoundTrip(
      {
        ConcludeWithdrawal: {
          payout_input_index: 0n,
          l1_output_index: 1n,
          burn_redeemer_index: 2n,
          hub_ref_input_index: 3n,
        },
      },
      SDK.PayoutSpendRedeemer,
    );

    expectRoundTrip(
      {
        reserve_input_index: 0n,
        payout_input_index: 1n,
        payout_spend_redeemer_index: 2n,
        hub_ref_input_index: 3n,
      },
      SDK.ReserveSpendRedeemer,
    );
  });
});
