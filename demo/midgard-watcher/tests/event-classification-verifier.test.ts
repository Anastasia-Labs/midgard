import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalTransitionEffectV1,
  canonicalCommittedWithdrawalTransitionEffectV1,
} from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "../../midgard-validation/tests/validation-fixtures.js";
import {
  evaluateWatcherBlockReplayV1,
  WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_V1_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES_V1,
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1,
  WATCHER_BLOCK_REPLAY_V1_SCHEMA_VERSION,
  type WatcherBlockReplayPriorUtxoV1,
} from "../src/block-replay.js";
import { watcherSha256CanonicalJsonV1 } from "../src/durable-store.js";
import {
  evaluateWatcherEventClassificationRulesV1,
  evaluateWatcherEventClassificationV1,
  WATCHER_EVENT_CLASSIFICATION_REASON_CODES_V1,
  watcherForcedIntervalIsDueV1,
  watcherTimedL1EventIsDueV1,
} from "../src/event-classification-verifier.js";
import { WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION } from "../src/phase-a-verifier.js";
import { makeWatcherStateQueueHeaderV1 } from "../src/state-queue-indexer.js";
import {
  createGenuineW15DepositWithdrawalAuthoritiesV1,
  type GenuineW15AuthorityFixtureSetV1,
  type W15AcceptedAuthorityScenarioV1,
} from "./support/w15-authority-scenarios.js";
import {
  createGenuineW16SettlementAuthoritiesV1,
  type GenuineW16SettlementAuthorityFixtureSetV1,
  type GenuineW16SettlementAuthorityV1,
} from "./support/w16-authority-scenarios.js";
import {
  type GenuineW25PublicReplayFixtureV1,
  makeGenuineW25PublicReplayFixtureV1,
} from "./support/w25-authority-fixtures.js";

const source = (
  overrides: Partial<
    Parameters<
      typeof evaluateWatcherEventClassificationRulesV1
    >[0]["sources"][number]
  > = {},
) => ({
  fingerprint:
    "Deposit:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:0",
  phase: "Deposit" as const,
  inclusionTime: "11",
  withdrawalValidity: null,
  forcedInterval: null,
  forcedValidity: null,
  settlementKind: null,
  ...overrides,
});

const trace = (
  overrides: Partial<
    Parameters<
      typeof evaluateWatcherEventClassificationRulesV1
    >[0]["trace"][number]
  > = {},
) => ({
  fingerprint:
    "Deposit:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:0",
  phase: "Deposit" as const,
  stepIndex: 0,
  preRoot: "a".repeat(64),
  postRoot: "b".repeat(64),
  mutationCount: 1,
  ...overrides,
});

const rules = (sources = [source()], entries = [trace()]) =>
  evaluateWatcherEventClassificationRulesV1({
    startTime: "10",
    endTime: "20",
    sources,
    trace: entries,
  });

const hex32 = "a".repeat(64);
const emptyRoot =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const header = makeWatcherStateQueueHeaderV1({
  nextHeaderHash: null,
  datumSha256: hex32,
  prevUtxosRoot: emptyRoot,
  utxosRoot: emptyRoot,
  withdrawalsRoot: emptyRoot,
  forcedTransactionsRoot: emptyRoot,
  transactionsRoot: emptyRoot,
  depositsRoot: emptyRoot,
  transitionTraceRoot: emptyRoot,
  eventToStepRoot: emptyRoot,
  validationTracesRoot: emptyRoot,
  withdrawalCount: "0",
  forcedTransactionCount: "0",
  l2TransactionCount: "0",
  depositCount: "0",
  totalEventCount: "0",
  transitionStepCount: "0",
  validationTraceCount: "0",
  startTime: "10",
  endTime: "20",
  blockSlot: "0",
  expectedNetworkId: "0",
  minFeeA: "0",
  minFeeB: "0",
  prevHeaderHash: "b".repeat(56),
  operatorVkey: "c".repeat(56),
  protocolVersion: "1",
  daAttestationPolicyId: null,
});

if (header === null) throw new Error("W26 test header did not parse");

const acceptedW25 = () => {
  const phaseACore = {
    schemaVersion: WATCHER_PHASE_A_VERIFIER_V1_SCHEMA_VERSION,
    action: "accept",
    reasonCodes: [],
    rejectionSelection: "first_rejection_by_phase_then_program_counter_v1",
    consensusProfileId: "midgard-consensus-profile-v1",
    headerHash: header.headerHash,
    payloadEnvelopeSha256: hex32,
    payloadSha256: hex32,
    reconstructionDigest: hex32,
    ruleBundleCommitment: hex32,
    transactionCount: 0,
    acceptedCount: 0,
    acceptedTxIds: [],
    rejections: [],
    selectedRejection: null,
  };
  const phaseA = {
    ...phaseACore,
    resultDigest: watcherSha256CanonicalJsonV1(phaseACore),
  };
  const binding = {
    headerHash: header.headerHash,
    payloadEnvelopeSha256: hex32,
    reconstructionDigest: hex32,
    phaseAResultDigest: phaseA.resultDigest,
    ruleBundleCommitment: hex32,
    authorityManifestDigest: hex32,
    sourceManifestDigest: hex32,
    effectManifestDigest: hex32,
    forcedValidationFacts: [],
    priorStateRoot: hex32,
    postStateRoot: hex32,
  };
  const prerequisite = {
    schemaVersion:
      WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_V1_SCHEMA_VERSION,
    requiredVerifier: "W26",
    inputDigest: watcherSha256CanonicalJsonV1(binding),
    w29Eligibility: "requires_w26_accept",
  };
  const core = {
    schemaVersion: WATCHER_BLOCK_REPLAY_V1_SCHEMA_VERSION,
    action: "accept",
    reasonCodes: [],
    verifiedRequires: "w26 required",
    downstreamPrerequisite: prerequisite,
    rejectionSelection: "first_rejection_by_phase_then_program_counter_v1",
    consensusProfileId: "midgard-consensus-profile-v1",
    headerHash: binding.headerHash,
    payloadEnvelopeSha256: binding.payloadEnvelopeSha256,
    payloadSha256: hex32,
    reconstructionDigest: binding.reconstructionDigest,
    phaseAResultDigest: binding.phaseAResultDigest,
    ruleBundleCommitment: binding.ruleBundleCommitment,
    authorityManifestDigest: binding.authorityManifestDigest,
    sourceManifestDigest: binding.sourceManifestDigest,
    effectManifestDigest: binding.effectManifestDigest,
    priorStateRoot: binding.priorStateRoot,
    expectedPriorStateRoot: hex32,
    postStateRoot: binding.postStateRoot,
    expectedPostStateRoot: hex32,
    transactionCount: 0,
    acceptedCount: 0,
    acceptedTxIds: [],
    intermediateRoots: [],
    transactionRoots: [],
    eventRoots: [],
    stageMismatches: [],
    rejections: [],
    selectedRejection: null,
    forcedValidationFacts: [],
  };
  const receipt = { ...core, resultDigest: watcherSha256CanonicalJsonV1(core) };
  return { receipt, phaseA };
};

const FIXED_KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));
const FIXED_ADDRESS = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(FIXED_KEY.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes(),
);
const FLOW_OUTPUT = makeOutput(10n, FIXED_ADDRESS);
const FORCED_VALID_INPUT = outRefFromByte(0x71);
const FORCED_VALID_NATIVE = makeNativeTx({
  spendInputs: [FORCED_VALID_INPUT],
  outputs: [FLOW_OUTPUT],
  privateKey: FIXED_KEY,
});
const FORCED_INVALID_CASES = Object.freeze({
  NonExistentInputUtxo: Object.freeze({
    input: outRefFromByte(0x72),
    native: makeNativeTx({
      spendInputs: [outRefFromByte(0x72)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
    }),
    operatorValidity: "NonExistentInputUtxo" as const,
  }),
  InvalidSignature: Object.freeze({
    input: outRefFromByte(0x73),
    native: makeNativeTx({
      spendInputs: [outRefFromByte(0x73)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
      invalidVkeyWitness: true,
    }),
    operatorValidity: "InvalidSignature" as const,
  }),
  FailedScript: Object.freeze({
    input: outRefFromByte(0x74),
    native: makeNativeTx({
      spendInputs: [outRefFromByte(0x74)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
      scriptWitnesses: [
        nativeScriptWitness({
          type: "sig",
          keyHash: Buffer.alloc(28, 0x06),
        }),
      ],
    }),
    operatorValidity: "FailedScript" as const,
  }),
  FeeTooLow: Object.freeze({
    input: outRefFromByte(0x75),
    native: makeNativeTx({
      spendInputs: [outRefFromByte(0x75)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
      fee: 0n,
    }),
    operatorValidity: "FeeTooLow" as const,
  }),
  UnbalancedTx: Object.freeze({
    input: outRefFromByte(0x76),
    native: makeNativeTx({
      spendInputs: [outRefFromByte(0x76)],
      outputs: [makeOutput(9n, FIXED_ADDRESS)],
      privateKey: FIXED_KEY,
    }),
    operatorValidity: "UnbalancedTx" as const,
  }),
});

const forcedPayloadForNative = (native: ReturnType<typeof makeNativeTx>) => {
  const source = deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(
    native.txCbor,
  );
  const full = decodeMidgardNativeTxFullV1FromCanonicalCbor(native.txCbor);
  return Object.freeze({
    tx_id: computeMidgardNativeTxIdV1(full).toString("hex"),
    transaction_commitment:
      computeMidgardNativeTxProofCommitmentV1(source).toString("hex"),
    source: Object.freeze({
      compact_cbor: source.compactCbor.toString("hex"),
      witness_set_compact_cbor: source.witnessSetCompactCbor.toString("hex"),
      field_preimage_lengths_cbor:
        source.fieldPreimageLengthsCbor.toString("hex"),
    }),
    terminal_receipt_reference: null,
  });
};

const genuineW15Input = () => ({
  forcedPayloadOverride: forcedPayloadForNative(FORCED_VALID_NATIVE),
  forcedCanonicalNativeTxCbor: FORCED_VALID_NATIVE.txCbor,
  forcedVariants: Object.entries(FORCED_INVALID_CASES).map(
    ([key, invalidCase], index) => ({
      key,
      nonceByte: ["9d", "9e", "9f", "aa", "ab"][index]!,
      payload: forcedPayloadForNative(invalidCase.native),
      canonicalNativeTxCbor: invalidCase.native.txCbor,
      operatorValidity: invalidCase.operatorValidity,
    }),
  ),
});

const settlementRecord = (authority: W15AcceptedAuthorityScenarioV1) => ({
  outRef: authority.event.outRef,
  outputCborHex: authority.event.outputCborHex,
  datumCborHex: authority.event.datumCborHex,
  assetNameHex: authority.event.assetNameHex,
  policyId: authority.event.policyId,
});

let genuineW15: GenuineW15AuthorityFixtureSetV1;
let genuineW16: GenuineW16SettlementAuthorityFixtureSetV1;
let repeatIsolationEvidence: Readonly<{
  w15DuplicateDisposeSharedPromise: boolean;
  w15FreshReferences: boolean;
  w16DuplicateDisposeSharedPromise: boolean;
  w16FreshReferences: boolean;
}>;

beforeAll(async () => {
  const firstW15 =
    await createGenuineW15DepositWithdrawalAuthoritiesV1(genuineW15Input());
  const firstW15Transports = firstW15.forced.context.transportAttestations;
  const firstW15Dispose = firstW15.dispose();
  const duplicateW15Dispose = firstW15.dispose();
  await firstW15Dispose;
  genuineW15 =
    await createGenuineW15DepositWithdrawalAuthoritiesV1(genuineW15Input());
  const w16Input = {
    deposit: settlementRecord(genuineW15.deposit),
    withdrawal: settlementRecord(genuineW15.withdrawal),
  };
  const firstW16 = await createGenuineW16SettlementAuthoritiesV1(w16Input);
  const firstW16Transports = firstW16.spawn.context.transportAttestations;
  const firstW16Dispose = firstW16.dispose();
  const duplicateW16Dispose = firstW16.dispose();
  await firstW16Dispose;
  genuineW16 = await createGenuineW16SettlementAuthoritiesV1(w16Input);
  repeatIsolationEvidence = Object.freeze({
    w15DuplicateDisposeSharedPromise: firstW15Dispose === duplicateW15Dispose,
    w15FreshReferences:
      firstW15Transports !== genuineW15.forced.context.transportAttestations &&
      firstW15.forced.result !== genuineW15.forced.result,
    w16DuplicateDisposeSharedPromise: firstW16Dispose === duplicateW16Dispose,
    w16FreshReferences:
      firstW16Transports !== genuineW16.spawn.context.transportAttestations &&
      firstW16.spawn.result !== genuineW16.spawn.result,
  });
}, 120_000);

afterAll(async () => {
  await genuineW16?.dispose();
  await genuineW15?.dispose();
}, 120_000);

const userEventAuthority = (authority: W15AcceptedAuthorityScenarioV1) => ({
  result: authority.result,
  context: authority.context,
});

const settlementAuthority = (authority: GenuineW16SettlementAuthorityV1) => ({
  result: authority.result,
  context: authority.context,
});

const forcedNativeAuthority = (
  authority: W15AcceptedAuthorityScenarioV1,
  native: ReturnType<typeof makeNativeTx>,
) => ({
  eventOutRef: authority.event.outRef,
  canonicalNativeTxCbor: native.txCbor,
});

const ledgerEntries = (
  values: readonly (readonly [Buffer, Buffer])[],
): readonly WatcherBlockReplayPriorUtxoV1[] =>
  values.map(([outRef, output]) => ({
    outRef: outRef.toString("hex"),
    outputCbor: output.toString("hex"),
  }));

const withdrawalTransitionEffect = (
  authority: W15AcceptedAuthorityScenarioV1,
  committedValid: boolean,
) => {
  const decoded = Data.from(
    authority.event.eventCborHex,
    SDK.WithdrawalEvent,
  ) as { readonly info: SDK.WithdrawalInfo };
  return canonicalCommittedWithdrawalTransitionEffectV1({
    committedValid,
    // The Plutus-Data `OutputReference` in the event datum is a *different*
    // encoding from the ledger out-ref; going from one to the other means
    // re-encoding through §5.3's fixed-index field-0/1 item, never CML's
    // minimal-index `TransactionInput` CBOR.
    outRefCbor: outRefFromTxId(
      Buffer.from(decoded.info.body.l2_outref.transactionId, "hex"),
      decoded.info.body.l2_outref.outputIndex,
    ),
  });
};

const acceptedPublicW25 = async (fixture: GenuineW25PublicReplayFixtureV1) => {
  const receipt = await evaluateWatcherBlockReplayV1(fixture.replayInput);
  expect(receipt).toMatchObject({ action: "accept", reasonCodes: [] });
  return receipt;
};

describe("W26 canonical event classification rules", () => {
  it("uses the on-chain timed-event boundaries exactly", () => {
    expect(watcherTimedL1EventIsDueV1(10n, 20n, 10n)).toBe(false);
    expect(watcherTimedL1EventIsDueV1(10n, 20n, 11n)).toBe(true);
    expect(watcherTimedL1EventIsDueV1(10n, 20n, 20n)).toBe(true);
    expect(watcherTimedL1EventIsDueV1(10n, 20n, 21n)).toBe(false);
  });

  it("uses the forced validity interval intersection, including open ends", () => {
    expect(watcherForcedIntervalIsDueV1(10n, 20n, -1n, -1n)).toBe(true);
    expect(watcherForcedIntervalIsDueV1(10n, 20n, -1n, 10n)).toBe(true);
    expect(watcherForcedIntervalIsDueV1(10n, 20n, 21n, -1n)).toBe(false);
    expect(watcherForcedIntervalIsDueV1(10n, 20n, 21n, 20n)).toBe(false);
    expect(watcherForcedIntervalIsDueV1(10n, 20n, 20n, 10n)).toBe(false);
  });

  it("flags a due source omitted from the trace and an included late source", () => {
    expect(rules([source()], [])).toStrictEqual([
      {
        code: "omitted_due_event",
        fingerprint: source().fingerprint,
        field: "$.trace",
      },
    ]);
    expect(rules([source({ inclusionTime: "21" })], [trace()])).toStrictEqual([
      {
        code: "out_of_window_event",
        fingerprint: source().fingerprint,
        field: "$.trace",
      },
    ]);
  });

  it("flags fabricated trace events and orders duplicate diagnostics stably", () => {
    const duplicate = trace({ stepIndex: 1 });
    expect(rules([], [trace(), duplicate])).toStrictEqual([
      {
        code: "duplicate_trace_event",
        fingerprint: trace().fingerprint,
        field: "$.trace",
      },
      {
        code: "fabricated_trace_event",
        fingerprint: trace().fingerprint,
        field: "$.trace.source",
      },
    ]);
    expect(rules([source(), source()], [trace()])).toStrictEqual([
      {
        code: "duplicate_source_event",
        fingerprint: source().fingerprint,
        field: "$.sources",
      },
    ]);
  });

  it("rejects both forced classification directions without trusting a label", () => {
    const forced = source({
      fingerprint:
        "ForcedTransaction:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:0",
      phase: "ForcedTransaction",
      forcedInterval: { start: "0", end: "-1" },
    });
    const forcedAsNormal = trace({
      fingerprint: forced.fingerprint,
      phase: "L2Transaction",
    });
    const normalAsForced = trace({
      fingerprint:
        "L2Transaction:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      phase: "ForcedTransaction",
    });
    expect(rules([forced], [forcedAsNormal])).toContainEqual({
      code: "trace_event_phase_mismatch",
      fingerprint: forced.fingerprint,
      field: "$.trace.phase",
    });
    expect(rules([], [normalAsForced])).toContainEqual({
      code: "trace_event_phase_mismatch",
      fingerprint: normalAsForced.fingerprint,
      field: "$.trace.phase",
    });
  });

  it("requires valid forced work to mutate and invalid forced work to be a no-op", () => {
    const forced = source({
      fingerprint:
        "ForcedTransaction:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:0",
      phase: "ForcedTransaction",
      forcedInterval: { start: "0", end: "-1" },
      forcedValidity: "valid",
    });
    expect(
      rules(
        [forced],
        [
          trace({
            fingerprint: forced.fingerprint,
            phase: "ForcedTransaction",
          }),
        ],
      ),
    ).toStrictEqual([]);
    const invalid = {
      ...forced,
      forcedValidity: "invalid" as const,
    };
    expect(
      rules(
        [invalid],
        [
          trace({
            fingerprint: invalid.fingerprint,
            phase: "ForcedTransaction",
            mutationCount: 0,
            postRoot: "a".repeat(64),
          }),
        ],
      ),
    ).toStrictEqual([]);
  });

  it("requires a valid withdrawal to delete exactly once via payout", () => {
    const withdrawal = source({
      fingerprint:
        "Withdrawal:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:0",
      phase: "Withdrawal",
      withdrawalValidity: "valid",
      settlementKind: "initialize_payout",
    });
    expect(
      rules(
        [withdrawal],
        [trace({ fingerprint: withdrawal.fingerprint, phase: "Withdrawal" })],
      ),
    ).toStrictEqual([]);
    expect(
      rules(
        [withdrawal],
        [
          trace({
            fingerprint: withdrawal.fingerprint,
            phase: "Withdrawal",
            mutationCount: 0,
            postRoot: "a".repeat(64),
          }),
        ],
      ),
    ).toContainEqual({
      code: "withdrawal_validity_mismatch",
      fingerprint: withdrawal.fingerprint,
      field: "$.withdrawal",
    });
  });

  it("requires an invalid withdrawal to be a refund no-op", () => {
    const withdrawal = source({
      fingerprint:
        "Withdrawal:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa:0",
      phase: "Withdrawal",
      withdrawalValidity: "invalid",
      settlementKind: "refund_withdrawal",
    });
    expect(
      rules(
        [withdrawal],
        [
          trace({
            fingerprint: withdrawal.fingerprint,
            phase: "Withdrawal",
            mutationCount: 0,
            postRoot: "a".repeat(64),
          }),
        ],
      ),
    ).toStrictEqual([]);
    expect(
      rules(
        [withdrawal],
        [trace({ fingerprint: withdrawal.fingerprint, phase: "Withdrawal" })],
      ),
    ).toContainEqual({
      code: "withdrawal_validity_mismatch",
      fingerprint: withdrawal.fingerprint,
      field: "$.withdrawal",
    });
  });

  it("keeps W26 diagnostics outside W24/W25 validation reject-code ownership", () => {
    const priorOwners = new Set<string>([
      ...WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES_V1,
      ...WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1,
    ]);
    expect(
      WATCHER_EVENT_CLASSIFICATION_REASON_CODES_V1.every(
        (code) => !priorOwners.has(code),
      ),
    ).toBe(true);
  });

  it("binds W25's W26 prerequisite and rejects tampered or substituted receipts", () => {
    const { receipt, phaseA } = acceptedW25();
    expect(
      evaluateWatcherEventClassificationV1({
        header,
        blockReplay: receipt,
        phaseA,
        userEventAuthorities: [],
        settlementAuthorities: [],
        forcedNativeTransactions: [],
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["authority_substitution"],
      w29Eligibility: "w26_accepted_not_w29_verified",
      classificationPrerequisite: {
        requiredVerifier: "W26",
        inputDigest: receipt.downstreamPrerequisite.inputDigest,
      },
    });
    expect(
      evaluateWatcherEventClassificationV1({
        header,
        blockReplay: { ...receipt, sourceManifestDigest: "d".repeat(64) },
        phaseA,
        userEventAuthorities: [],
        settlementAuthorities: [],
        forcedNativeTransactions: [],
      }),
    ).toMatchObject({
      action: "error",
      reasonCodes: ["w25_digest_mismatch"],
    });
    const substituted = {
      ...receipt,
      downstreamPrerequisite: {
        ...receipt.downstreamPrerequisite,
        requiredVerifier: "W27",
      },
    };
    const replacement = {
      ...substituted,
      resultDigest: watcherSha256CanonicalJsonV1(substituted),
    };
    expect(
      evaluateWatcherEventClassificationV1({
        header,
        blockReplay: replacement,
        phaseA,
        userEventAuthorities: [],
        settlementAuthorities: [],
        forcedNativeTransactions: [],
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["w25_prerequisite_mismatch"],
    });
  });

  it("accepts W25's globally indexed interleaved trace shape before authority coverage", () => {
    const { receipt, phaseA } = acceptedW25();
    const txId = "e".repeat(64);
    const core = {
      ...receipt,
      eventRoots: [
        {
          stepIndex: 2,
          phase: "Deposit",
          eventKeyFingerprint: `Deposit:${"f".repeat(64)}:0`,
          preRoot: "b".repeat(64),
          postRoot: "c".repeat(64),
          mutationCount: 1,
        },
      ],
      transactionRoots: [
        {
          txIndex: 0,
          txId,
          preRoot: "a".repeat(64),
          postRoot: "b".repeat(64),
          mutationCount: 1,
          committedStepIndex: 0,
          committedPreRoot: "a".repeat(64),
          committedPostRoot: "b".repeat(64),
        },
        {
          txIndex: 1,
          txId: "d".repeat(64),
          preRoot: "b".repeat(64),
          postRoot: "b".repeat(64),
          mutationCount: 0,
          committedStepIndex: 1,
          committedPreRoot: "b".repeat(64),
          committedPostRoot: "b".repeat(64),
        },
      ],
    };
    const interleaved = {
      ...core,
      resultDigest: watcherSha256CanonicalJsonV1(
        Object.fromEntries(
          Object.entries(core).filter(([key]) => key !== "resultDigest"),
        ),
      ),
    };
    expect(
      evaluateWatcherEventClassificationV1({
        header,
        blockReplay: interleaved,
        phaseA,
        userEventAuthorities: [],
        settlementAuthorities: [],
        forcedNativeTransactions: [],
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["authority_substitution"],
    });
  });

  it("emits distinct schema, W25-action, prerequisite, and digest diagnostics", () => {
    const { receipt, phaseA } = acceptedW25();
    const evaluate = (blockReplay: unknown) =>
      evaluateWatcherEventClassificationV1({
        header,
        blockReplay,
        phaseA,
        userEventAuthorities: [],
        settlementAuthorities: [],
        forcedNativeTransactions: [],
      });
    expect(evaluate({})).toMatchObject({
      action: "error",
      reasonCodes: ["unknown_schema"],
    });
    expect(evaluate({ ...receipt, action: "reject" })).toMatchObject({
      action: "reject",
      reasonCodes: ["w25_not_accepted"],
    });
    expect(
      evaluate({
        ...receipt,
        downstreamPrerequisite: {
          ...receipt.downstreamPrerequisite,
          requiredVerifier: "W27",
        },
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["w25_prerequisite_mismatch"],
    });
    expect(
      evaluate({ ...receipt, sourceManifestDigest: "f".repeat(64) }),
    ).toMatchObject({
      action: "error",
      reasonCodes: ["w25_digest_mismatch"],
    });
  });

  it("accepts repeat-isolated genuine W25 forced receipts for valid mutation and all five invalid no-op categories", async () => {
    expect(repeatIsolationEvidence).toStrictEqual({
      w15DuplicateDisposeSharedPromise: true,
      w15FreshReferences: true,
      w16DuplicateDisposeSharedPromise: true,
      w16FreshReferences: true,
    });
    const validAuthority = genuineW15.forced;
    const validEffect = buildCanonicalTransitionEffectV1([
      { type: "delete", outRefCbor: FORCED_VALID_INPUT },
      {
        type: "insert",
        outRefCbor: outRefFromTxId(FORCED_VALID_NATIVE.txId),
        outputCbor: FLOW_OUTPUT,
      },
    ]);
    const validFixture = await makeGenuineW25PublicReplayFixtureV1({
      userEvent: validAuthority,
      canonicalNativeTxCbor: FORCED_VALID_NATIVE.txCbor,
      transitionEffect: validEffect,
      priorState: ledgerEntries([[FORCED_VALID_INPUT, FLOW_OUTPUT]]),
      postState: ledgerEntries([
        [outRefFromTxId(FORCED_VALID_NATIVE.txId), FLOW_OUTPUT],
      ]),
    });
    const validReceipt = await acceptedPublicW25(validFixture);
    expect(validReceipt.eventRoots).toMatchObject([{ mutationCount: 2 }]);
    expect(validReceipt.forcedValidationFacts).toMatchObject([
      {
        authenticatedOperatorValidity: "TxIsValid",
        canonicalOperatorValidity: "TxIsValid",
        phaseAStatus: "accepted",
        phaseBStatus: "accepted",
        canonicalEffectMutationCount: 2,
      },
    ]);
    expect(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(FORCED_VALID_NATIVE.txCbor)
        .validity,
    ).toBe("TxIsValid");
    expect(
      evaluateWatcherEventClassificationV1({
        header: validFixture.header,
        blockReplay: validReceipt,
        phaseA: validFixture.phaseA,
        userEventAuthorities: [userEventAuthority(validAuthority)],
        settlementAuthorities: [],
        forcedNativeTransactions: [
          forcedNativeAuthority(validAuthority, FORCED_VALID_NATIVE),
        ],
      }),
    ).toMatchObject({ action: "accept", reasonCodes: [], findings: [] });

    const noOpEffect = buildCanonicalTransitionEffectV1([]);
    const exercisedCategories: string[] = [];
    for (const [category, invalidCase] of Object.entries(
      FORCED_INVALID_CASES,
    ) as [
      keyof typeof FORCED_INVALID_CASES,
      (typeof FORCED_INVALID_CASES)[keyof typeof FORCED_INVALID_CASES],
    ][]) {
      const authority = genuineW15.forcedVariants[category]!;
      if (
        !("terminalClassification" in authority.event) ||
        authority.event.terminalClassification === undefined
      ) {
        throw new Error(`${category} lacks genuine W15 classification`);
      }
      expect(authority.event.terminalClassification.operatorValidity).toBe(
        invalidCase.operatorValidity,
      );
      expect(
        decodeMidgardNativeTxFullV1FromCanonicalCbor(invalidCase.native.txCbor)
          .validity,
      ).toBe("TxIsValid");
      const priorState =
        category === "NonExistentInputUtxo"
          ? []
          : ledgerEntries([[invalidCase.input, FLOW_OUTPUT]]);
      const fixture = await makeGenuineW25PublicReplayFixtureV1({
        userEvent: authority,
        canonicalNativeTxCbor: invalidCase.native.txCbor,
        transitionEffect: noOpEffect,
        priorState,
        postState: priorState,
        ...(category === "FeeTooLow" ? { minFeeB: 1n } : {}),
      });
      const receipt = await acceptedPublicW25(fixture);
      expect(receipt.eventRoots).toMatchObject([{ mutationCount: 0 }]);
      expect(receipt.forcedValidationFacts).toHaveLength(1);
      expect(receipt.forcedValidationFacts[0]).toMatchObject({
        authenticatedOperatorValidity: invalidCase.operatorValidity,
        canonicalOperatorValidity: invalidCase.operatorValidity,
        canonicalEffectMutationCount: 0,
      });
      expect(
        evaluateWatcherEventClassificationV1({
          header: fixture.header,
          blockReplay: receipt,
          phaseA: fixture.phaseA,
          userEventAuthorities: [userEventAuthority(authority)],
          settlementAuthorities: [],
          forcedNativeTransactions: [
            forcedNativeAuthority(authority, invalidCase.native),
          ],
        }),
        category,
      ).toMatchObject({ action: "accept", reasonCodes: [], findings: [] });
      exercisedCategories.push(category);
    }
    expect(exercisedCategories).toStrictEqual([
      "NonExistentInputUtxo",
      "InvalidSignature",
      "FailedScript",
      "FeeTooLow",
      "UnbalancedTx",
    ]);
  });

  it("rejects omission, substitution, duplication, and tampering of genuine forced W15, W25, and native-source evidence", async () => {
    const authority = genuineW15.forcedVariants.UnbalancedTx!;
    const invalidCase = FORCED_INVALID_CASES.UnbalancedTx;
    const priorState = ledgerEntries([[invalidCase.input, FLOW_OUTPUT]]);
    const fixture = await makeGenuineW25PublicReplayFixtureV1({
      userEvent: authority,
      canonicalNativeTxCbor: invalidCase.native.txCbor,
      transitionEffect: buildCanonicalTransitionEffectV1([]),
      priorState,
      postState: priorState,
    });
    const receipt = await acceptedPublicW25(fixture);
    expect(receipt.forcedValidationFacts).toHaveLength(1);
    const evaluate = (
      overrides: {
        readonly receipt?: unknown;
        readonly userAuthorities?: Parameters<
          typeof evaluateWatcherEventClassificationV1
        >[0]["userEventAuthorities"];
        readonly nativeAuthorities?: Parameters<
          typeof evaluateWatcherEventClassificationV1
        >[0]["forcedNativeTransactions"];
      } = {},
    ) =>
      evaluateWatcherEventClassificationV1({
        header: fixture.header,
        blockReplay: overrides.receipt ?? receipt,
        phaseA: fixture.phaseA,
        userEventAuthorities: overrides.userAuthorities ?? [
          userEventAuthority(authority),
        ],
        settlementAuthorities: [],
        forcedNativeTransactions: overrides.nativeAuthorities ?? [
          forcedNativeAuthority(authority, invalidCase.native),
        ],
      });

    const classificationMutation = () =>
      structuredClone(authority.result) as unknown as {
        state: {
          snapshot: {
            terminalEvents: Array<{
              eventId: string;
              nonceOutRef: string;
              terminalClassification?: {
                operatorValidity: string;
                terminalPointDigest: string;
              };
            }>;
          };
        };
      };
    const omittedClassification = classificationMutation();
    delete omittedClassification.state.snapshot.terminalEvents[0]!
      .terminalClassification;
    const substitutedClassification = classificationMutation();
    substitutedClassification.state.snapshot.terminalEvents[0]!.terminalClassification!.operatorValidity =
      "TxIsValid";
    const tamperedClassification = classificationMutation();
    tamperedClassification.state.snapshot.terminalEvents[0]!.terminalClassification!.terminalPointDigest =
      "f".repeat(64);
    for (const resultValue of [
      omittedClassification,
      substitutedClassification,
      tamperedClassification,
    ]) {
      expect(
        evaluate({
          userAuthorities: [
            { result: resultValue, context: authority.context },
          ],
        }),
      ).toMatchObject({
        action: "error",
        reasonCodes: ["user_event_authority_invalid"],
      });
    }
    expect(
      evaluate({
        userAuthorities: [
          userEventAuthority(authority),
          userEventAuthority(authority),
        ],
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["authority_substitution"],
    });

    const mismatchedNonce = classificationMutation();
    mismatchedNonce.state.snapshot.terminalEvents[0]!.nonceOutRef = `${"e".repeat(64)}#0`;
    const mismatchedEventId = classificationMutation();
    mismatchedEventId.state.snapshot.terminalEvents[0]!.eventId = Data.to(
      { transactionId: "e".repeat(64), outputIndex: 0n },
      SDK.OutputReference,
    );
    for (const resultValue of [mismatchedNonce, mismatchedEventId]) {
      expect(
        evaluate({
          userAuthorities: [
            { result: resultValue, context: authority.context },
          ],
        }),
      ).toMatchObject({
        action: "error",
        reasonCodes: ["user_event_authority_invalid"],
      });
    }

    const genuineFact = receipt.forcedValidationFacts[0]!;
    for (const forcedValidationFacts of [
      [],
      [{ ...genuineFact, canonicalOperatorValidity: "TxIsValid" }],
      [genuineFact, genuineFact],
      [{ ...genuineFact, canonicalEffectMutationCount: 1 }],
    ]) {
      const tamperedReceipt = {
        ...receipt,
        forcedValidationFacts,
      };
      expect(evaluate({ receipt: tamperedReceipt })).toMatchObject({
        action: "error",
        reasonCodes: ["w25_digest_mismatch"],
      });
    }
    const createdOutputFingerprint = `ForcedTransaction:${authority.event.outRef.replace("#", ":")}`;
    expect(receipt.eventRoots[0]!.eventKeyFingerprint).not.toBe(
      createdOutputFingerprint,
    );
    expect(
      evaluate({
        receipt: {
          ...receipt,
          eventRoots: [
            {
              ...receipt.eventRoots[0]!,
              eventKeyFingerprint: createdOutputFingerprint,
            },
          ],
        },
      }),
    ).toMatchObject({
      action: "error",
      reasonCodes: ["w25_digest_mismatch"],
    });

    const substitutedNative = forcedNativeAuthority(
      authority,
      FORCED_VALID_NATIVE,
    );
    const tamperedNativeBytes = Buffer.from(invalidCase.native.txCbor);
    tamperedNativeBytes[tamperedNativeBytes.length - 1] ^= 0x01;
    for (const nativeAuthorities of [
      [],
      [substitutedNative],
      [
        forcedNativeAuthority(authority, invalidCase.native),
        forcedNativeAuthority(authority, invalidCase.native),
      ],
      [
        {
          eventOutRef: authority.event.outRef,
          canonicalNativeTxCbor: tamperedNativeBytes,
        },
      ],
    ]) {
      expect(evaluate({ nativeAuthorities })).toMatchObject({
        action: "reject",
        reasonCodes: ["forced_source_mismatch"],
      });
    }
  });

  it("accepts genuine withdrawal initialize/refund authorities and rejects W16 omission, substitution, duplication, and tampering", async () => {
    const authority = genuineW15.withdrawal;
    const validEffect = withdrawalTransitionEffect(authority, true);
    const deleted = validEffect.operations[0];
    if (deleted?.type !== "delete")
      throw new Error("valid withdrawal effect did not delete its L2 input");
    const priorState = ledgerEntries([[deleted.outRefCbor, FLOW_OUTPUT]]);
    const validFixture = await makeGenuineW25PublicReplayFixtureV1({
      userEvent: authority,
      settlement: genuineW16.initializePayout,
      transitionEffect: validEffect,
      priorState,
      postState: [],
    });
    const validReceipt = await acceptedPublicW25(validFixture);
    const invalidFixture = await makeGenuineW25PublicReplayFixtureV1({
      userEvent: authority,
      settlement: genuineW16.refundWithdrawal,
      transitionEffect: withdrawalTransitionEffect(authority, false),
      priorState,
      postState: priorState,
    });
    const invalidReceipt = await acceptedPublicW25(invalidFixture);
    const evaluate = (input: {
      readonly fixture: GenuineW25PublicReplayFixtureV1;
      readonly receipt: unknown;
      readonly settlementAuthorities: readonly ReturnType<
        typeof settlementAuthority
      >[];
    }) =>
      evaluateWatcherEventClassificationV1({
        header: input.fixture.header,
        blockReplay: input.receipt,
        phaseA: input.fixture.phaseA,
        userEventAuthorities: [userEventAuthority(authority)],
        settlementAuthorities: input.settlementAuthorities,
        forcedNativeTransactions: [],
      });
    expect(
      evaluate({
        fixture: validFixture,
        receipt: validReceipt,
        settlementAuthorities: [
          settlementAuthority(genuineW16.initializePayout),
        ],
      }),
    ).toMatchObject({ action: "accept", reasonCodes: [], findings: [] });
    expect(
      evaluate({
        fixture: invalidFixture,
        receipt: invalidReceipt,
        settlementAuthorities: [
          settlementAuthority(genuineW16.refundWithdrawal),
        ],
      }),
    ).toMatchObject({ action: "accept", reasonCodes: [], findings: [] });

    expect(
      evaluate({
        fixture: validFixture,
        receipt: validReceipt,
        settlementAuthorities: [],
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["authority_substitution"],
    });
    expect(
      evaluate({
        fixture: validFixture,
        receipt: validReceipt,
        settlementAuthorities: [
          settlementAuthority(genuineW16.refundWithdrawal),
        ],
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["withdrawal_validity_mismatch"],
    });
    expect(
      evaluate({
        fixture: validFixture,
        receipt: validReceipt,
        settlementAuthorities: [
          settlementAuthority(genuineW16.initializePayout),
          settlementAuthority(genuineW16.initializePayout),
        ],
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["authority_substitution"],
    });
    const tamperedSettlement = {
      ...settlementAuthority(genuineW16.initializePayout),
      result: {
        ...genuineW16.initializePayout.result,
        resultDigest: "f".repeat(64),
      },
    };
    expect(
      evaluate({
        fixture: validFixture,
        receipt: validReceipt,
        settlementAuthorities: [tamperedSettlement],
      }),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["authority_substitution"],
    });
  });
});
