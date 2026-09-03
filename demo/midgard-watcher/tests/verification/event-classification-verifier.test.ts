import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalTransitionEffect,
  canonicalCommittedWithdrawalTransitionEffect,
} from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
} from "../../../midgard-validation/tests/validation-fixtures.js";
import { makeWatcherStateQueueHeader } from "../../src/indexers/state-queue-indexer.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  evaluateWatcherBlockReplay,
  WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
  type WatcherBlockReplayPriorUtxo,
} from "../../src/verification/block-replay.js";
import {
  evaluateWatcherEventClassification,
  evaluateWatcherEventClassificationRules,
  WATCHER_EVENT_CLASSIFICATION_REASON_CODES,
  watcherForcedIntervalIsDue,
  watcherTimedL1EventIsDue,
} from "../../src/verification/event-classification-verifier.js";
import { WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION } from "../../src/verification/phase-a-verifier.js";
import {
  type GenuineReplayPublicReplayFixture,
  makeGenuineReplayPublicReplayFixture,
} from "../support/replay-authority-fixtures.js";
import {
  createGenuineSettlementAuthorities,
  type GenuineSettlementAuthority,
  type GenuineSettlementAuthorityFixtureSet,
} from "../support/settlement-authority-scenarios.js";
import {
  createGenuineUserEventDepositWithdrawalAuthorities,
  type GenuineUserEventAuthorityFixtureSet,
  genuineUserEventForcedPayloadForCanonicalTx,
  type UserEventAcceptedAuthorityScenario,
  userEventForcedOperatorVerdictForClassification,
} from "../support/user-event-authority-scenarios.js";

const source = (
  overrides: Partial<
    Parameters<
      typeof evaluateWatcherEventClassificationRules
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
      typeof evaluateWatcherEventClassificationRules
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
  evaluateWatcherEventClassificationRules({
    startTime: "10",
    endTime: "20",
    sources,
    trace: entries,
  });

const hex32 = "a".repeat(64);
const emptyRoot =
  "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8";
const header = makeWatcherStateQueueHeader({
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
    schemaVersion: WATCHER_PHASE_A_VERIFIER_SCHEMA_VERSION,
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
    resultDigest: watcherSha256CanonicalJson(phaseACore),
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
    schemaVersion: WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_SCHEMA_VERSION,
    requiredVerifier: "W26",
    inputDigest: watcherSha256CanonicalJson(binding),
    w29Eligibility: "requires_w26_accept",
  };
  const core = {
    schemaVersion: WATCHER_BLOCK_REPLAY_SCHEMA_VERSION,
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
  const receipt = { ...core, resultDigest: watcherSha256CanonicalJson(core) };
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
const FLOW_OUTPUT = makeOutput(FUNDED_OUTPUT_LOVELACE, FIXED_ADDRESS);
const FORCED_VALID_INPUT = outRefFromByte(0x71);
const FORCED_VALID_NATIVE = makeNativeTx({
  spendInputs: [FORCED_VALID_INPUT],
  outputs: [FLOW_OUTPUT],
  privateKey: FIXED_KEY,
});
const FORCED_INVALID_CASES = Object.freeze({
  InputNotFound: Object.freeze({
    input: outRefFromByte(0x72),
    native: makeNativeTx({
      spendInputs: [outRefFromByte(0x72)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
    }),
    operatorValidity: "InputNotFound" as const,
  }),
  AddressWitnessSignatureInvalid: Object.freeze({
    input: outRefFromByte(0x73),
    native: makeNativeTx({
      spendInputs: [outRefFromByte(0x73)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
      invalidVkeyWitness: true,
    }),
    operatorValidity: "AddressWitnessSignatureInvalid" as const,
  }),
  WitnessNativeScriptFalse: Object.freeze({
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
    operatorValidity: "WitnessNativeScriptFalse" as const,
  }),
  FeeBelowMinimum: Object.freeze({
    input: outRefFromByte(0x75),
    native: makeNativeTx({
      spendInputs: [outRefFromByte(0x75)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
      fee: 0n,
    }),
    operatorValidity: "FeeBelowMinimum" as const,
  }),
  ValueNotPreserved: Object.freeze({
    input: outRefFromByte(0x76),
    native: makeNativeTx({
      spendInputs: [outRefFromByte(0x76)],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE - 1n, FIXED_ADDRESS)],
      privateKey: FIXED_KEY,
    }),
    operatorValidity: "ValueNotPreserved" as const,
  }),
});

// Routed through the shared derivation rather than rebuilt here: the payload now
// travels with the §8 carriage vector its mint redeemer supplies (#594), and two
// hand-rolled copies of that pairing would be two chances to get the vector wrong
// in a way the fixture cannot detect.
const forcedPayloadForNative = (native: ReturnType<typeof makeNativeTx>) =>
  genuineUserEventForcedPayloadForCanonicalTx(native.txCbor);

const genuineUserEventInput = () => ({
  forcedPayloadOverride: forcedPayloadForNative(FORCED_VALID_NATIVE),
  forcedVariants: Object.entries(FORCED_INVALID_CASES).map(
    ([key, invalidCase], index) => ({
      key,
      nonceByte: ["9d", "9e", "9f", "aa", "ab"][index]!,
      payload: forcedPayloadForNative(invalidCase.native),
      operatorValidity: userEventForcedOperatorVerdictForClassification(
        invalidCase.operatorValidity,
      ),
    }),
  ),
});

const settlementRecord = (authority: UserEventAcceptedAuthorityScenario) => ({
  outRef: authority.event.outRef,
  outputCborHex: authority.event.outputCborHex,
  datumCborHex: authority.event.datumCborHex,
  assetNameHex: authority.event.assetNameHex,
  policyId: authority.event.policyId,
});

let genuineW15: GenuineUserEventAuthorityFixtureSet;
let genuineW16: GenuineSettlementAuthorityFixtureSet;
let repeatIsolationEvidence: Readonly<{
  w15DuplicateDisposeSharedPromise: boolean;
  w15FreshReferences: boolean;
  w16DuplicateDisposeSharedPromise: boolean;
  w16FreshReferences: boolean;
}>;

beforeAll(async () => {
  const firstW15 = await createGenuineUserEventDepositWithdrawalAuthorities(
    genuineUserEventInput(),
  );
  const firstUserEventTransports =
    firstW15.forced.context.transportAttestations;
  const firstUserEventDispose = firstW15.dispose();
  const duplicateUserEventDispose = firstW15.dispose();
  await firstUserEventDispose;
  genuineW15 = await createGenuineUserEventDepositWithdrawalAuthorities(
    genuineUserEventInput(),
  );
  const settlementInput = {
    deposit: settlementRecord(genuineW15.deposit),
    withdrawal: settlementRecord(genuineW15.withdrawal),
  };
  const firstW16 = await createGenuineSettlementAuthorities(settlementInput);
  const firstSettlementTransports =
    firstW16.spawn.context.transportAttestations;
  const firstSettlementDispose = firstW16.dispose();
  const duplicateSettlementDispose = firstW16.dispose();
  await firstSettlementDispose;
  genuineW16 = await createGenuineSettlementAuthorities(settlementInput);
  repeatIsolationEvidence = Object.freeze({
    w15DuplicateDisposeSharedPromise:
      firstUserEventDispose === duplicateUserEventDispose,
    w15FreshReferences:
      firstUserEventTransports !==
        genuineW15.forced.context.transportAttestations &&
      firstW15.forced.result !== genuineW15.forced.result,
    w16DuplicateDisposeSharedPromise:
      firstSettlementDispose === duplicateSettlementDispose,
    w16FreshReferences:
      firstSettlementTransports !==
        genuineW16.spawn.context.transportAttestations &&
      firstW16.spawn.result !== genuineW16.spawn.result,
  });
}, 120_000);

afterAll(async () => {
  await genuineW16?.dispose();
  await genuineW15?.dispose();
}, 120_000);

const userEventAuthority = (authority: UserEventAcceptedAuthorityScenario) => ({
  result: authority.result,
  context: authority.context,
});

const settlementAuthority = (authority: GenuineSettlementAuthority) => ({
  result: authority.result,
  context: authority.context,
});

const forcedNativeAuthority = (
  authority: UserEventAcceptedAuthorityScenario,
  native: ReturnType<typeof makeNativeTx>,
) => ({
  eventOutRef: authority.event.outRef,
  canonicalNativeTxCbor: native.txCbor,
});

const ledgerEntries = (
  values: readonly (readonly [Buffer, Buffer])[],
): readonly WatcherBlockReplayPriorUtxo[] =>
  values.map(([outRef, output]) => ({
    outRef: outRef.toString("hex"),
    outputCbor: output.toString("hex"),
  }));

const withdrawalTransitionEffect = (
  authority: UserEventAcceptedAuthorityScenario,
  committedValid: boolean,
) => {
  const decoded = Data.from(
    authority.event.eventCborHex,
    SDK.WithdrawalEvent,
  ) as { readonly info: SDK.WithdrawalInfo };
  return canonicalCommittedWithdrawalTransitionEffect({
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

const acceptedPublicW25 = async (fixture: GenuineReplayPublicReplayFixture) => {
  const receipt = await evaluateWatcherBlockReplay(fixture.replayInput);
  expect(receipt).toMatchObject({ action: "accept", reasonCodes: [] });
  return receipt;
};

describe("W26 canonical event classification rules", () => {
  it("uses the on-chain timed-event boundaries exactly", () => {
    expect(watcherTimedL1EventIsDue(10n, 20n, 10n)).toBe(false);
    expect(watcherTimedL1EventIsDue(10n, 20n, 11n)).toBe(true);
    expect(watcherTimedL1EventIsDue(10n, 20n, 20n)).toBe(true);
    expect(watcherTimedL1EventIsDue(10n, 20n, 21n)).toBe(false);
  });

  it("uses the forced validity interval intersection, including open ends", () => {
    expect(watcherForcedIntervalIsDue(10n, 20n, -1n, -1n)).toBe(true);
    expect(watcherForcedIntervalIsDue(10n, 20n, -1n, 10n)).toBe(true);
    expect(watcherForcedIntervalIsDue(10n, 20n, 21n, -1n)).toBe(false);
    expect(watcherForcedIntervalIsDue(10n, 20n, 21n, 20n)).toBe(false);
    expect(watcherForcedIntervalIsDue(10n, 20n, 20n, 10n)).toBe(false);
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
      ...WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES,
      ...WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
    ]);
    expect(
      WATCHER_EVENT_CLASSIFICATION_REASON_CODES.every(
        (code) => !priorOwners.has(code),
      ),
    ).toBe(true);
  });

  it("binds W25's W26 prerequisite and rejects tampered or substituted receipts", () => {
    const { receipt, phaseA } = acceptedW25();
    expect(
      evaluateWatcherEventClassification({
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
      evaluateWatcherEventClassification({
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
      resultDigest: watcherSha256CanonicalJson(substituted),
    };
    expect(
      evaluateWatcherEventClassification({
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
      resultDigest: watcherSha256CanonicalJson(
        Object.fromEntries(
          Object.entries(core).filter(([key]) => key !== "resultDigest"),
        ),
      ),
    };
    expect(
      evaluateWatcherEventClassification({
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
      evaluateWatcherEventClassification({
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
    const validEffect = buildCanonicalTransitionEffect([
      { type: "delete", outRefCbor: FORCED_VALID_INPUT },
      {
        type: "insert",
        outRefCbor: outRefFromTxId(FORCED_VALID_NATIVE.txId),
        outputCbor: FLOW_OUTPUT,
      },
    ]);
    const validFixture = await makeGenuineReplayPublicReplayFixture({
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
        authenticatedOperatorValidity: "ForcedTxValid",
        canonicalOperatorValidity: "ForcedTxValid",
        phaseAStatus: "accepted",
        phaseBStatus: "accepted",
        canonicalEffectMutationCount: 2,
      },
    ]);
    expect(
      decodeMidgardNativeTxFullFromCanonicalCbor(FORCED_VALID_NATIVE.txCbor)
        .validity,
    ).toBe("TxIsValid");
    expect(
      evaluateWatcherEventClassification({
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

    const noOpEffect = buildCanonicalTransitionEffect([]);
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
        decodeMidgardNativeTxFullFromCanonicalCbor(invalidCase.native.txCbor)
          .validity,
      ).toBe("TxIsValid");
      const priorState =
        category === "InputNotFound"
          ? []
          : ledgerEntries([[invalidCase.input, FLOW_OUTPUT]]);
      const fixture = await makeGenuineReplayPublicReplayFixture({
        userEvent: authority,
        canonicalNativeTxCbor: invalidCase.native.txCbor,
        transitionEffect: noOpEffect,
        priorState,
        postState: priorState,
        ...(category === "FeeBelowMinimum" ? { minFeeB: 1n } : {}),
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
        evaluateWatcherEventClassification({
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
      "InputNotFound",
      "AddressWitnessSignatureInvalid",
      "WitnessNativeScriptFalse",
      "FeeBelowMinimum",
      "ValueNotPreserved",
    ]);
  });

  it("rejects omission, substitution, duplication, and tampering of genuine forced W15, W25, and native-source evidence", async () => {
    const authority = genuineW15.forcedVariants.ValueNotPreserved!;
    const invalidCase = FORCED_INVALID_CASES.ValueNotPreserved;
    const priorState = ledgerEntries([[invalidCase.input, FLOW_OUTPUT]]);
    const fixture = await makeGenuineReplayPublicReplayFixture({
      userEvent: authority,
      canonicalNativeTxCbor: invalidCase.native.txCbor,
      transitionEffect: buildCanonicalTransitionEffect([]),
      priorState,
      postState: priorState,
    });
    const receipt = await acceptedPublicW25(fixture);
    expect(receipt.forcedValidationFacts).toHaveLength(1);
    const evaluate = (
      overrides: {
        readonly receipt?: unknown;
        readonly userAuthorities?: Parameters<
          typeof evaluateWatcherEventClassification
        >[0]["userEventAuthorities"];
        readonly nativeAuthorities?: Parameters<
          typeof evaluateWatcherEventClassification
        >[0]["forcedNativeTransactions"];
      } = {},
    ) =>
      evaluateWatcherEventClassification({
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
      "ForcedTxValid";
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
      [{ ...genuineFact, canonicalOperatorValidity: "ForcedTxValid" }],
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
    const validFixture = await makeGenuineReplayPublicReplayFixture({
      userEvent: authority,
      settlement: genuineW16.initializePayout,
      transitionEffect: validEffect,
      priorState,
      postState: [],
    });
    const validReceipt = await acceptedPublicW25(validFixture);
    const invalidFixture = await makeGenuineReplayPublicReplayFixture({
      userEvent: authority,
      settlement: genuineW16.refundWithdrawal,
      transitionEffect: withdrawalTransitionEffect(authority, false),
      priorState,
      postState: priorState,
    });
    const invalidReceipt = await acceptedPublicW25(invalidFixture);
    const evaluate = (input: {
      readonly fixture: GenuineReplayPublicReplayFixture;
      readonly receipt: unknown;
      readonly settlementAuthorities: readonly ReturnType<
        typeof settlementAuthority
      >[];
    }) =>
      evaluateWatcherEventClassification({
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
