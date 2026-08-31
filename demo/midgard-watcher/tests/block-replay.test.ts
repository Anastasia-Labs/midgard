/**
 * W25 Phase-B/block-replay evidence.
 *
 * The candidate fixtures below come from the canonical validation suite. This
 * suite deliberately does not manufacture a second transaction or ledger
 * implementation: it drives the watcher adapter with canonical
 * `PhaseAValidatedTx` values, then checks the replay record it persists for
 * roots, ordering, and exact canonical rejection attribution.
 */
import {
  adjudicateMidgardNativeTxFullV1Validity,
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { buildCountedRoot, encodeData } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  buildCanonicalTransitionEffectV1,
  buildValidationMachineLedgerInsertOpV1,
  buildValidationMachineLedgerMutationSteps,
  canonicalCommittedWithdrawalTransitionEffectV1,
  type CanonicalTransitionEffectV1,
  deriveCanonicalDepositTransitionEffectV1,
  LedgerColumns,
  type ValidationMachineLedgerOp,
} from "@al-ft/midgard-validation";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { blake2b } from "../../midgard-core/node_modules/@noble/hashes/blake2.js";
import { wrapDaPayloadV1 } from "../../midgard-core/src/da-payload-envelope.js";
import { MidgardRedeemerTag } from "../../midgard-validation/src/midgard-redeemers.js";
import {
  FUNDED_OUTPUT_LOVELACE_V1,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeQueued,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "../../midgard-validation/tests/validation-fixtures.js";
import {
  assertWatcherFullBlockReplayResultV1,
  evaluateWatcherBlockReplayCandidatesV1,
  makeWatcherPhaseBConfigV1,
  WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES_V1,
  WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODES_V1,
  WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_V1_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES_V1,
  WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES_V1,
  WATCHER_BLOCK_REPLAY_PROTOCOL_MINUS_UNCLAIMED_V1,
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1,
  WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES_V1,
  WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT_V1,
  watcherBlockReplayCommittedStepsV1,
  type WatcherBlockReplayEventAuthorityV1,
  watcherBlockReplayForcedValidityForRejectCodeV1,
  watcherBlockReplayPriorStateV1,
  type WatcherBlockReplayPriorUtxoV1,
  watcherBlockReplayRejectionProjectionV1,
  watcherBlockReplayStageForRejectionV1,
} from "../src/block-replay.js";
import { watcherSha256CanonicalJsonV1 } from "../src/durable-store.js";
import {
  evaluateWatcherHeaderRootReconstructionV1,
  makeWatcherAuthenticatedHeaderObservationV1,
  type WatcherHeaderRootReconstructionResultV1,
} from "../src/header-root-reconstruction.js";
import {
  evaluateWatcherBlockReplayV1,
  makeWatcherBlockReplayReconstructedStateV1,
} from "../src/index.js";
import { watcherL1TransportAttestationDetailsV1 } from "../src/l1-adapter.js";
import {
  evaluateWatcherPhaseABlockV1,
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1,
  WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1,
  WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1,
} from "../src/phase-a-verifier.js";
import {
  computeWatcherRuleBundleV1Commitment,
  makeWatcherCanonicalRuleBundleV1,
  type WatcherRuleBundleV1,
} from "../src/rule-bundle-v1.js";
import type { WatcherStateQueueHeaderV1 } from "../src/state-queue-indexer.js";
import {
  createGenuineW15DepositWithdrawalAuthoritiesV1,
  type GenuineW15AuthorityFixtureSetV1,
  genuineW15ForcedPayloadForCanonicalTxV1,
  type W15AcceptedAuthorityScenarioV1,
  w15ForcedOperatorVerdictForClassificationV1,
} from "./support/w15-authority-scenarios.js";
import {
  createGenuineW16SettlementAuthoritiesV1,
  type GenuineW16SettlementAuthorityFixtureSetV1,
  type GenuineW16SettlementAuthorityV1,
} from "./support/w16-authority-scenarios.js";
import { createWatcherOpaqueAuthorityHarnessV1 } from "./support/watcher-opaque-authority-harness.js";

const header = { blockSlot: 0n } as Parameters<
  typeof makeWatcherPhaseBConfigV1
>[0];
const config = makeWatcherPhaseBConfigV1(header);

const FIXED_KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));
const FIXED_ADDRESS = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(FIXED_KEY.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes(),
);
const FIXED_ADDRESS_DATA: SDK.AddressData = {
  paymentCredential: {
    PublicKeyCredential: [FIXED_KEY.to_public().hash().to_hex()],
  },
  stakeCredential: null,
};

const FLOW_OUTPUT = makeOutput(FUNDED_OUTPUT_LOVELACE_V1, FIXED_ADDRESS);
const WITHDRAWAL_FLOW_INPUT = outRefFromByte(0x51);
const WITHDRAWAL_FLOW_NATIVE = makeNativeTx({
  spendInputs: [WITHDRAWAL_FLOW_INPUT],
  outputs: [FLOW_OUTPUT],
  privateKey: FIXED_KEY,
});
const FORCED_FLOW_INPUT = outRefFromByte(0x71);
const FORCED_FLOW_NATIVE = makeNativeTx({
  spendInputs: [FORCED_FLOW_INPUT],
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
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE_V1 - 1n, FIXED_ADDRESS)],
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
  genuineW15ForcedPayloadForCanonicalTxV1(native.txCbor);

let genuineW15: GenuineW15AuthorityFixtureSetV1;
let genuineW16: GenuineW16SettlementAuthorityFixtureSetV1;
let repeatIsolationEvidence: Readonly<{
  harnessDistinctDeploymentFixture: boolean;
  harnessDistinctTransports: boolean;
  harnessFrozenTransports: boolean;
  harnessDistinctProviders: boolean;
  w15DistinctTransports: boolean;
  w15FrozenTransports: boolean;
  w15DistinctDeploymentAuthority: boolean;
  w15DistinctProvider: boolean;
  w15ConcurrentLeasePartition: boolean;
  w15SetupFailureReleasedLease: boolean;
  w15DuplicateDisposeSharedPromise: boolean;
  w15DisposeOverlapRejected: boolean;
  w15StaleDisposePreservedFreshState: boolean;
  w15DeterministicReplay: boolean;
  w16DistinctTransports: boolean;
  w16FrozenTransports: boolean;
  w16DistinctDeploymentAuthority: boolean;
  w16DistinctProvider: boolean;
  w16ConcurrentLeasePartition: boolean;
  w16SetupFailureReleasedLease: boolean;
  w16DuplicateDisposeSharedPromise: boolean;
  w16DisposeOverlapRejected: boolean;
  w16StaleDisposePreservedFreshState: boolean;
  w16DeterministicReplay: boolean;
}>;

const genuineW15Input = () => ({
  depositL2Address: FIXED_ADDRESS_DATA,
  withdrawalL2OutRef: {
    transactionId: WITHDRAWAL_FLOW_NATIVE.txId.toString("hex"),
    outputIndex: 0n,
  },
  forcedPayloadOverride: forcedPayloadForNative(FORCED_FLOW_NATIVE),
  forcedVariants: [
    ...Object.entries(FORCED_INVALID_CASES).map(
      ([key, invalidCase], index) => ({
        key,
        nonceByte: ["9d", "9e", "9f", "aa", "ab"][index]!,
        payload: forcedPayloadForNative(invalidCase.native),
        operatorValidity: w15ForcedOperatorVerdictForClassificationV1(
          invalidCase.operatorValidity,
        ),
      }),
    ),
    {
      key: "Mismatch",
      nonceByte: "ac",
      payload: forcedPayloadForNative(
        FORCED_INVALID_CASES.ValueNotPreserved.native,
      ),
      operatorValidity: "ForcedTxValid" as const,
    },
  ],
});

const settlementRecord = (authority: W15AcceptedAuthorityScenarioV1) => ({
  outRef: authority.event.outRef,
  outputCborHex: authority.event.outputCborHex,
  datumCborHex: authority.event.datumCborHex,
  assetNameHex: authority.event.assetNameHex,
  policyId: authority.event.policyId,
});

const deploymentAuthorityReference = (publicContext: unknown): object => {
  if (typeof publicContext !== "object" || publicContext === null) {
    throw new Error("genuine authority public context is malformed");
  }
  const deploymentAuthority = (
    publicContext as Readonly<{ deploymentAuthority?: unknown }>
  ).deploymentAuthority;
  if (typeof deploymentAuthority !== "object" || deploymentAuthority === null) {
    throw new Error("genuine authority deployment identity is malformed");
  }
  return deploymentAuthority;
};

const authenticatedProviderReference = (publicContext: unknown): object => {
  if (typeof publicContext !== "object" || publicContext === null) {
    throw new Error("genuine authority public context is malformed");
  }
  const authenticatedProvider = (
    publicContext as Readonly<{ authenticatedProvider?: unknown }>
  ).authenticatedProvider;
  if (
    typeof authenticatedProvider !== "object" ||
    authenticatedProvider === null
  ) {
    throw new Error("genuine authority provider identity is malformed");
  }
  return authenticatedProvider;
};

const exactlyOneFactoryLeaseWinner = <T>(
  results: readonly PromiseSettledResult<T>[],
  expectedRejectionMessage: string,
): Readonly<{ winner: T; deterministicRejection: boolean }> => {
  const fulfilled = results.filter(
    (result): result is PromiseFulfilledResult<T> =>
      result.status === "fulfilled",
  );
  const rejected = results.filter(
    (result): result is PromiseRejectedResult => result.status === "rejected",
  );
  if (fulfilled.length !== 1 || rejected.length !== 1) {
    throw new Error("concurrent authority factory lease was not exclusive");
  }
  const rejectionReason: unknown = rejected[0]!.reason;
  return Object.freeze({
    winner: fulfilled[0]!.value,
    deterministicRejection:
      rejectionReason instanceof Error &&
      rejectionReason.message === expectedRejectionMessage,
  });
};

const rejectedWithMessage = (
  result: PromiseSettledResult<unknown> | undefined,
  expectedMessage: string,
): boolean => {
  if (result?.status !== "rejected") return false;
  const reason: unknown = result.reason;
  return reason instanceof Error && reason.message === expectedMessage;
};

beforeAll(async () => {
  const firstHarness = await createWatcherOpaqueAuthorityHarnessV1();
  await firstHarness.dispose();
  const secondHarness = await createWatcherOpaqueAuthorityHarnessV1();
  await secondHarness.dispose();
  const [w15SetupFailure] = await Promise.allSettled([
    createGenuineW15DepositWithdrawalAuthoritiesV1({
      ...genuineW15Input(),
      transportFixtureRoot:
        "/dev/shm/midgard-w25-intentionally-missing-parent/w15",
    }),
  ]);
  const w15LeasePartition = exactlyOneFactoryLeaseWinner(
    await Promise.allSettled([
      createGenuineW15DepositWithdrawalAuthoritiesV1(genuineW15Input()),
      createGenuineW15DepositWithdrawalAuthoritiesV1(genuineW15Input()),
    ]),
    "W15 opaque authority fixture lease is not idle",
  );
  const firstW15 = w15LeasePartition.winner;
  const w15DisposeOne = firstW15.dispose();
  const w15DisposeTwo = firstW15.dispose();
  const [w15DisposeOverlap] = await Promise.allSettled([
    createGenuineW15DepositWithdrawalAuthoritiesV1(genuineW15Input()),
  ]);
  await Promise.all([w15DisposeOne, w15DisposeTwo]);
  genuineW15 =
    await createGenuineW15DepositWithdrawalAuthoritiesV1(genuineW15Input());
  const w15FreshTransports = genuineW15.deposit.context.transportAttestations;
  const w15FreshProvider = authenticatedProviderReference(
    genuineW15.deposit.context.publicContext,
  );
  const w15FreshAuthority = deploymentAuthorityReference(
    genuineW15.deposit.context.publicContext,
  );
  await firstW15.dispose();
  const w15StaleDisposePreservedFreshState =
    genuineW15.deposit.context.transportAttestations === w15FreshTransports &&
    genuineW15.deposit.context.transportAttestations.length === 2 &&
    genuineW15.deposit.context.transportAttestations.every(
      (context) => watcherL1TransportAttestationDetailsV1(context) !== null,
    ) &&
    authenticatedProviderReference(genuineW15.deposit.context.publicContext) ===
      w15FreshProvider &&
    deploymentAuthorityReference(genuineW15.deposit.context.publicContext) ===
      w15FreshAuthority;
  const w16Input = {
    deposit: settlementRecord(genuineW15.deposit),
    withdrawal: settlementRecord(genuineW15.withdrawal),
  };
  const [w16SetupFailure] = await Promise.allSettled([
    createGenuineW16SettlementAuthoritiesV1({
      ...w16Input,
      transportFixtureRoot:
        "/dev/shm/midgard-w25-intentionally-missing-parent/w16",
    }),
  ]);
  const w16LeasePartition = exactlyOneFactoryLeaseWinner(
    await Promise.allSettled([
      createGenuineW16SettlementAuthoritiesV1(w16Input),
      createGenuineW16SettlementAuthoritiesV1(w16Input),
    ]),
    "W16 opaque authority fixture lease is not idle",
  );
  const firstW16 = w16LeasePartition.winner;
  const w16DisposeOne = firstW16.dispose();
  const w16DisposeTwo = firstW16.dispose();
  const [w16DisposeOverlap] = await Promise.allSettled([
    createGenuineW16SettlementAuthoritiesV1(w16Input),
  ]);
  await Promise.all([w16DisposeOne, w16DisposeTwo]);
  genuineW16 = await createGenuineW16SettlementAuthoritiesV1(w16Input);
  const w16FreshTransports = genuineW16.spawn.context.transportAttestations;
  const w16FreshProvider = authenticatedProviderReference(
    genuineW16.spawn.context.publicContext,
  );
  const w16FreshAuthority = deploymentAuthorityReference(
    genuineW16.spawn.context.publicContext,
  );
  await firstW16.dispose();
  const w16StaleDisposePreservedFreshState =
    genuineW16.spawn.context.transportAttestations === w16FreshTransports &&
    genuineW16.spawn.context.transportAttestations.length === 2 &&
    genuineW16.spawn.context.transportAttestations.every(
      (context) => watcherL1TransportAttestationDetailsV1(context) !== null,
    ) &&
    authenticatedProviderReference(genuineW16.spawn.context.publicContext) ===
      w16FreshProvider &&
    deploymentAuthorityReference(genuineW16.spawn.context.publicContext) ===
      w16FreshAuthority;
  repeatIsolationEvidence = Object.freeze({
    harnessDistinctDeploymentFixture:
      firstHarness.deploymentFixture !== secondHarness.deploymentFixture,
    harnessDistinctTransports:
      firstHarness.transportAttestations !==
        secondHarness.transportAttestations &&
      firstHarness.transportAttestations.length === 2 &&
      secondHarness.transportAttestations.length === 2,
    harnessFrozenTransports:
      Object.isFrozen(firstHarness.transportAttestations) &&
      Object.isFrozen(secondHarness.transportAttestations),
    harnessDistinctProviders:
      firstHarness.providers !== secondHarness.providers &&
      firstHarness.providers[0] !== secondHarness.providers[0] &&
      firstHarness.providers.length === 2 &&
      secondHarness.providers.length === 2,
    w15DistinctTransports:
      firstW15.deposit.context.transportAttestations !==
        genuineW15.deposit.context.transportAttestations &&
      firstW15.deposit.context.transportAttestations.length === 2 &&
      genuineW15.deposit.context.transportAttestations.length === 2,
    w15FrozenTransports:
      Object.isFrozen(firstW15.deposit.context.transportAttestations) &&
      Object.isFrozen(genuineW15.deposit.context.transportAttestations),
    w15DistinctDeploymentAuthority:
      deploymentAuthorityReference(firstW15.deposit.context.publicContext) !==
      deploymentAuthorityReference(genuineW15.deposit.context.publicContext),
    w15DistinctProvider:
      authenticatedProviderReference(firstW15.deposit.context.publicContext) !==
      authenticatedProviderReference(genuineW15.deposit.context.publicContext),
    w15ConcurrentLeasePartition: w15LeasePartition.deterministicRejection,
    w15SetupFailureReleasedLease: w15SetupFailure?.status === "rejected",
    w15DuplicateDisposeSharedPromise: w15DisposeOne === w15DisposeTwo,
    w15DisposeOverlapRejected: rejectedWithMessage(
      w15DisposeOverlap,
      "W15 opaque authority fixture lease is not idle",
    ),
    w15StaleDisposePreservedFreshState,
    w15DeterministicReplay:
      firstW15.deposit.event.outRef === genuineW15.deposit.event.outRef &&
      firstW15.deposit.event.eventContentDigest ===
        genuineW15.deposit.event.eventContentDigest &&
      firstW15.deposit.event.outputDigest ===
        genuineW15.deposit.event.outputDigest,
    w16DistinctTransports:
      firstW16.spawn.context.transportAttestations !==
        genuineW16.spawn.context.transportAttestations &&
      firstW16.spawn.context.transportAttestations.length === 2 &&
      genuineW16.spawn.context.transportAttestations.length === 2,
    w16FrozenTransports:
      Object.isFrozen(firstW16.spawn.context.transportAttestations) &&
      Object.isFrozen(genuineW16.spawn.context.transportAttestations),
    w16DistinctDeploymentAuthority:
      deploymentAuthorityReference(firstW16.spawn.context.publicContext) !==
      deploymentAuthorityReference(genuineW16.spawn.context.publicContext),
    w16DistinctProvider:
      authenticatedProviderReference(firstW16.spawn.context.publicContext) !==
      authenticatedProviderReference(genuineW16.spawn.context.publicContext),
    w16ConcurrentLeasePartition: w16LeasePartition.deterministicRejection,
    w16SetupFailureReleasedLease: w16SetupFailure?.status === "rejected",
    w16DuplicateDisposeSharedPromise: w16DisposeOne === w16DisposeTwo,
    w16DisposeOverlapRejected: rejectedWithMessage(
      w16DisposeOverlap,
      "W16 opaque authority fixture lease is not idle",
    ),
    w16StaleDisposePreservedFreshState,
    w16DeterministicReplay:
      watcherSha256CanonicalJsonV1(firstW16.spawn.observation.transition) ===
      watcherSha256CanonicalJsonV1(genuineW16.spawn.observation.transition),
  });
}, 120_000);

afterAll(async () => {
  await genuineW16?.dispose();
  await genuineW15?.dispose();
}, 120_000);

// Every `outRef` below is §5.3's fixed-index field-0/1 item — the ledger MPF
// trie key — so each is exactly 38 bytes and its output index is the
// non-minimal `19 0000`, never the minimal `00` CML would emit. The two tx ids
// and all eight roots are downstream of that key width: the spend-input items a
// fixture transaction commits determine its id, and the trie keys determine
// every root, so re-pinning the out-refs necessarily re-pins the rest.
const FIXED_TWO_TX_ROOTS = [
  {
    sequence: 0,
    txIndex: 0,
    txId: "5aa36d0b6f5cc700f18f54386542bd937ba7b96625eff82e81c1f451686e94dd",
    stepIndex: null,
    phase: null,
    operation: "delete",
    outRef:
      "8258201111111111111111111111111111111111111111111111111111111111111111190000",
    preRoot: "49476a071f7393279ca22a35d4ebe3b3316190c47890e5af7f3f12fded51c915",
    postRoot:
      "82fc6f18dd68ee99bc196356f2464631186bac1391508525f5e6267bd860bfce",
  },
  {
    sequence: 1,
    txIndex: 0,
    txId: "5aa36d0b6f5cc700f18f54386542bd937ba7b96625eff82e81c1f451686e94dd",
    stepIndex: null,
    phase: null,
    operation: "insert",
    outRef:
      "8258205aa36d0b6f5cc700f18f54386542bd937ba7b96625eff82e81c1f451686e94dd190000",
    preRoot: "82fc6f18dd68ee99bc196356f2464631186bac1391508525f5e6267bd860bfce",
    postRoot:
      "6bb3c6e322f6cf64aacde28c9d4e489dd5173a92ba76e270771c2e7226fc1fad",
  },
  {
    sequence: 2,
    txIndex: 1,
    txId: "72e8307b8f82e2e380eed82386cf0d480304166cedb40011147e65fb110ab9ff",
    stepIndex: null,
    phase: null,
    operation: "delete",
    outRef:
      "8258201212121212121212121212121212121212121212121212121212121212121212190000",
    preRoot: "6bb3c6e322f6cf64aacde28c9d4e489dd5173a92ba76e270771c2e7226fc1fad",
    postRoot:
      "52b9c88cd96dfa08f6f35d7c484b3a89059f2ee485ca8a58efeaa2c52171ebbd",
  },
  {
    sequence: 3,
    txIndex: 1,
    txId: "72e8307b8f82e2e380eed82386cf0d480304166cedb40011147e65fb110ab9ff",
    stepIndex: null,
    phase: null,
    operation: "insert",
    outRef:
      "82582072e8307b8f82e2e380eed82386cf0d480304166cedb40011147e65fb110ab9ff190000",
    preRoot: "52b9c88cd96dfa08f6f35d7c484b3a89059f2ee485ca8a58efeaa2c52171ebbd",
    postRoot:
      "6d4a5867c105f9c81fa71dfea9c531063c1b3d88d28539b77941eab4ec6c58ac",
  },
] as const;

const entries = (
  values: readonly (readonly [Buffer, Buffer])[],
): readonly WatcherBlockReplayPriorUtxoV1[] =>
  values.map(([outRef, output]) => ({
    outRef: outRef.toString("hex"),
    outputCbor: output.toString("hex"),
  }));

const replay = async (
  candidates: Parameters<
    typeof evaluateWatcherBlockReplayCandidatesV1
  >[0]["candidates"],
  priorState: readonly WatcherBlockReplayPriorUtxoV1[],
  expectedPostStateRoot?: string,
) => {
  const prior = await watcherBlockReplayPriorStateV1(priorState);
  return await evaluateWatcherBlockReplayCandidatesV1({
    candidates,
    priorState,
    expectedPriorStateRoot: prior.root,
    ...(expectedPostStateRoot === undefined ? {} : { expectedPostStateRoot }),
    config,
  });
};

const h32 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(32);
const h28 = (byte: number): string =>
  byte.toString(16).padStart(2, "0").repeat(28);

const L1_PROVENANCE: SDK.EvidenceProvenanceV1 = {
  trustClass: "authenticated_cardano_l1",
  sourceId: "watcher-local-node",
  grade: "security",
};
const DA_PROVENANCE: SDK.EvidenceProvenanceV1 = {
  trustClass: "public_or_permissionless_da",
  sourceId: "watcher-da-peer-1",
  grade: "security",
};
const CHAIN_POINT = { slot: 4242n, blockHash: h32(7) } as const;

const RULE_BUNDLE: WatcherRuleBundleV1 = makeWatcherCanonicalRuleBundleV1({
  constructionIdentity: {
    manifestId: h32(0x21),
    network: "Preprod",
    releaseEvidenceDigest: h32(0x22),
    programCommitments: {
      "transition-order-v1": h32(0x23),
      "validation-machine-v1": h32(0x24),
    },
  },
  targetParameterSnapshot: { finalityDepth: 12 },
});
const RULE_BUNDLE_COMMITMENT =
  computeWatcherRuleBundleV1Commitment(RULE_BUNDLE);

const headerHashOf = (value: SDK.HeaderV1): string =>
  Buffer.from(
    blake2b(Buffer.from(Data.to(value, SDK.HeaderV1), "hex"), { dkLen: 28 }),
  ).toString("hex");

const sortEntries = (
  values: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...values].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );
const bufferEntries = (values: readonly SDK.DaPayloadEntry[]) =>
  values.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));
const dataHex = <A>(value: A, schema: Parameters<typeof Data.to>[1]): string =>
  encodeData(value, schema as never).toString("hex");

const outputReference = (byte: number): SDK.OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});
const addressData = (byte: number): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [h28(byte)] },
  stakeCredential: null,
});
const depositEvent = (byte: number): PublicFixtureEvent => {
  const id = outputReference(byte);
  return {
    eventKey: { DepositEventKey: { deposit_id: id } },
    phase: "Deposit",
    domain: "deposits",
    entry: [
      dataHex(id, SDK.OutputReferenceSchema),
      dataHex(
        {
          l2_address: addressData(byte + 1),
          l2_network_id: 0n,
          l2_datum: null,
        } satisfies SDK.DepositInfo,
        SDK.DepositInfoSchema,
      ),
    ],
  };
};

const publicEventFromW15 = (
  authority: W15AcceptedAuthorityScenarioV1,
  forcedNative?: ReturnType<typeof makeNativeTx>,
): PublicFixtureEvent => {
  const event = authority.event;
  if (event.kind === "deposit") {
    const decoded = Data.from(event.eventCborHex, SDK.DepositEvent) as {
      readonly info: SDK.DepositInfo;
    };
    return Object.freeze({
      eventKey: {
        DepositEventKey: {
          deposit_id: Data.from(
            event.eventId,
            SDK.OutputReference as never,
          ) as SDK.OutputReference,
        },
      },
      phase: "Deposit" as const,
      domain: "deposits" as const,
      entry: [
        event.eventId,
        dataHex(decoded.info, SDK.DepositInfoSchema),
      ] as SDK.DaPayloadEntry,
    });
  }
  if (event.kind === "withdrawal") {
    const decoded = Data.from(event.eventCborHex, SDK.WithdrawalEvent) as {
      readonly info: SDK.WithdrawalInfo;
    };
    return Object.freeze({
      eventKey: {
        WithdrawalEventKey: {
          withdrawal_id: Data.from(
            event.eventId,
            SDK.OutputReference as never,
          ) as SDK.OutputReference,
        },
      },
      phase: "Withdrawal" as const,
      domain: "withdrawals" as const,
      entry: [
        event.eventId,
        dataHex(decoded.info, SDK.WithdrawalInfoSchema),
      ] as SDK.DaPayloadEntry,
    });
  }
  if (forcedNative === undefined) {
    throw new Error("forced W15 public event requires canonical native bytes");
  }
  const decoded = Data.from(event.eventCborHex, SDK.TxOrderEventV1) as {
    readonly tx: {
      readonly tx_id: string;
      readonly transaction_commitment: string;
      readonly source: SDK.L2TransactionSourceV1["source"];
    };
  };
  if (
    !("terminalClassification" in event) ||
    event.terminalClassification === undefined
  ) {
    throw new Error("forced W15 event lacks terminal classification");
  }
  const verdict = w15ForcedOperatorVerdictForClassificationV1(
    event.terminalClassification.operatorValidity,
  );
  // The ORDER event binds the SUBMITTED source, but the committed DA leaf
  // carries the operator-ADJUDICATED one (§2.4.3(e)) — the payload
  // reconstruction authenticates exactly that. Re-derive through the single
  // stamping helper by the leaf's verdict rather than copying the event's
  // submitted triple.
  const adjudicatedSource = deriveMidgardNativeTxProofSourceV1(
    adjudicateMidgardNativeTxFullV1Validity(
      decodeMidgardNativeTxFullV1FromCanonicalCbor(forcedNative.txCbor),
      verdict === "ForcedTxValid" ? "TxIsValid" : "TxIsInvalid",
    ),
  );
  return Object.freeze({
    eventKey: {
      ForcedTransactionEventKey: {
        tx_order_id: Data.from(
          event.eventId,
          SDK.OutputReference as never,
        ) as SDK.OutputReference,
      },
    },
    phase: "ForcedTransaction" as const,
    domain: "forced_transactions" as const,
    entry: [
      event.eventId,
      dataHex(
        {
          tx_id: decoded.tx.tx_id,
          source: {
            compact_cbor: adjudicatedSource.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              adjudicatedSource.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              adjudicatedSource.fieldPreimageLengthsCbor.toString("hex"),
          },
          verdict,
        },
        SDK.ForcedInclusionTxV1Schema,
      ),
    ] as SDK.DaPayloadEntry,
    forcedPreimage: [
      event.eventId,
      forcedNative.txCbor.toString("hex"),
    ] as SDK.DaPayloadEntry,
  });
};

const cardanoOutputAssets = (
  outputCborHex: string,
): Readonly<Record<string, bigint>> => {
  const value = CML.TransactionOutput.from_cbor_hex(outputCborHex).amount();
  const assets: Record<string, bigint> = { lovelace: value.coin() };
  const multiasset = value.multi_asset();
  if (multiasset !== undefined) {
    const policies = multiasset.keys();
    for (let policyIndex = 0; policyIndex < policies.len(); policyIndex += 1) {
      const policy = policies.get(policyIndex);
      const policyAssets = multiasset.get_assets(policy);
      if (policyAssets === undefined) continue;
      const names = policyAssets.keys();
      for (let nameIndex = 0; nameIndex < names.len(); nameIndex += 1) {
        const name = names.get(nameIndex);
        const quantity = policyAssets.get(name);
        if (quantity !== undefined) {
          assets[`${policy.to_hex()}${name.to_hex()}`] = quantity;
        }
      }
    }
  }
  return Object.freeze(assets);
};

const depositEffectFromW15 = (
  authority: W15AcceptedAuthorityScenarioV1,
): CanonicalTransitionEffectV1 => {
  const event = authority.event;
  if (event.kind !== "deposit" || authority.parsed.state === null) {
    throw new Error("deposit authority is not parser-accepted");
  }
  const decoded = Data.from(event.eventCborHex, SDK.DepositEvent) as {
    readonly id: SDK.OutputReference;
    readonly info: SDK.DepositInfo;
  };
  return deriveCanonicalDepositTransitionEffectV1({
    configuredNetwork: authority.parsed.state.network,
    eventId: decoded.id,
    l2NetworkId: decoded.info.l2_network_id,
    l2Address: decoded.info.l2_address,
    l2DatumCbor:
      decoded.info.l2_datum === null
        ? null
        : Buffer.from(Data.to(decoded.info.l2_datum as never), "hex"),
    l1Assets: cardanoOutputAssets(event.outputCborHex),
    depositPolicyId: event.policyId,
    depositAssetNameHex: event.assetNameHex,
  });
};

const withdrawalEffectFromW15 = (
  authority: W15AcceptedAuthorityScenarioV1,
  committedValid: boolean,
): CanonicalTransitionEffectV1 => {
  if (authority.event.kind !== "withdrawal") {
    throw new Error("withdrawal authority has the wrong kind");
  }
  const decoded = Data.from(
    authority.event.eventCborHex,
    SDK.WithdrawalEvent,
  ) as { readonly info: SDK.WithdrawalInfo };
  const outRef = decoded.info.body.l2_outref;
  return canonicalCommittedWithdrawalTransitionEffectV1({
    committedValid,
    // The Plutus-Data `OutputReference` in the event datum is a *different*
    // encoding from the ledger out-ref; going from one to the other means
    // re-encoding through §5.3's fixed-index field-0/1 item, never CML's
    // minimal-index `TransactionInput` CBOR.
    outRefCbor: outRefFromTxId(
      Buffer.from(outRef.transactionId, "hex"),
      outRef.outputIndex,
    ),
  });
};

const nativeEffect = (input: {
  readonly spent: readonly Buffer[];
  readonly native: ReturnType<typeof makeNativeTx>;
  readonly outputs: readonly Buffer[];
}): CanonicalTransitionEffectV1 =>
  buildCanonicalTransitionEffectV1([
    ...input.spent.map((outRefCbor) => ({
      type: "delete" as const,
      outRefCbor,
    })),
    ...input.outputs.map((outputCbor, outputIndex) => ({
      type: "insert" as const,
      outRefCbor: outRefFromTxId(input.native.txId, BigInt(outputIndex)),
      outputCbor,
    })),
  ]);

type CommittedEffectGroup = Readonly<{
  eventKey: SDK.EventKey;
  phase: SDK.TransitionPhase;
  effect: CanonicalTransitionEffectV1;
}>;

const committedStepsForEffects = async (
  priorState: readonly WatcherBlockReplayPriorUtxoV1[],
  groups: readonly CommittedEffectGroup[],
): Promise<readonly SDK.TransitionStep[]> => {
  const prior = await watcherBlockReplayPriorStateV1(priorState);
  const operations: ValidationMachineLedgerOp[] = groups.flatMap(({ effect }) =>
    effect.operations.map((operation) =>
      operation.type === "delete"
        ? { type: "delete" as const, key: operation.outRefCbor }
        : buildValidationMachineLedgerInsertOpV1({
            key: operation.outRefCbor,
            outputCbor: operation.outputCbor,
          }),
    ),
  );
  const mutationSteps = await buildValidationMachineLedgerMutationSteps({
    initialEntries: priorState.map((entry) => ({
      outRef: Buffer.from(entry.outRef, "hex"),
      output: Buffer.from(entry.outputCbor, "hex"),
    })),
    operations,
  });
  let root = prior.root;
  let cursor = 0;
  return Object.freeze(
    groups.map((group, stepIndex) => {
      const preRoot = root;
      cursor += group.effect.operations.length;
      if (group.effect.operations.length > 0) {
        const machineRoot = mutationSteps[cursor - 1]!.postRoot.toString("hex");
        root =
          machineRoot === "00".repeat(32)
            ? SDK.EMPTY_MERKLE_TREE_ROOT
            : machineRoot;
      }
      return Object.freeze({
        schema_version: 1n,
        step_index: BigInt(stepIndex),
        event_key: group.eventKey,
        phase: group.phase,
        pre_utxos_root: preRoot,
        post_utxos_root: root,
      });
    }),
  );
};

const settlementAuthority = (
  fixture: GenuineW16SettlementAuthorityV1,
): NonNullable<WatcherBlockReplayEventAuthorityV1["settlement"]> => ({
  result: fixture.result,
  context: fixture.context,
  observationDigest: fixture.observation.observationDigest,
});

const eventAuthority = (input: {
  readonly event: PublicFixtureEvent;
  readonly userEvent: W15AcceptedAuthorityScenarioV1;
  readonly settlement?: GenuineW16SettlementAuthorityV1;
  readonly effect: CanonicalTransitionEffectV1;
  readonly forcedNative?: ReturnType<typeof makeNativeTx>;
}): WatcherBlockReplayEventAuthorityV1 => ({
  eventKey: input.event.eventKey,
  phase: input.event.phase,
  userEvent: {
    result: input.userEvent.result,
    context: input.userEvent.context,
  },
  ...(input.settlement === undefined
    ? {}
    : { settlement: settlementAuthority(input.settlement) }),
  transitionEffect: input.effect,
  ...(input.forcedNative === undefined
    ? {}
    : {
        canonicalNativeTxCbor: input.forcedNative.txCbor,
        programMaterialSidecarCbor: makeQueued(
          input.forcedNative.txId,
          input.forcedNative.txCbor,
        ).programMaterialSidecarCbor,
      }),
});

const watcherHeaderRecord = (
  value: SDK.HeaderV1,
  headerHash: string,
): WatcherStateQueueHeaderV1 => ({
  headerHash,
  headerCborHex: Data.to(value, SDK.HeaderV1),
  nextHeaderHash: null,
  datumSha256: h32(3),
  prevUtxosRoot: value.prevUtxosRoot,
  utxosRoot: value.utxosRoot,
  withdrawalsRoot: value.withdrawalsRoot,
  forcedTransactionsRoot: value.forcedTransactionsRoot,
  transactionsRoot: value.transactionsRoot,
  depositsRoot: value.depositsRoot,
  transitionTraceRoot: value.transitionTraceRoot,
  eventToStepRoot: value.eventToStepRoot,
  validationTracesRoot: value.validationTracesRoot,
  withdrawalCount: value.withdrawalCount.toString(),
  forcedTransactionCount: value.forcedTransactionCount.toString(),
  l2TransactionCount: value.l2TransactionCount.toString(),
  depositCount: value.depositCount.toString(),
  totalEventCount: value.totalEventCount.toString(),
  transitionStepCount: value.transitionStepCount.toString(),
  validationTraceCount: value.validationTraceCount.toString(),
  startTime: value.startTime.toString(),
  endTime: value.endTime.toString(),
  blockSlot: value.blockSlot.toString(),
  expectedNetworkId: value.expectedNetworkId.toString(),
  minFeeA: value.minFeeA.toString(),
  minFeeB: value.minFeeB.toString(),
  prevHeaderHash: value.prevHeaderHash,
  operatorVkey: value.operatorVkey,
  protocolVersion: value.protocolVersion.toString(),
  daAttestationPolicyId: null,
});

type PublicFixtureEvent = Readonly<{
  eventKey: SDK.EventKey;
  phase: Exclude<SDK.TransitionPhase, "L2Transaction">;
  domain: "withdrawals" | "forced_transactions" | "deposits";
  entry: SDK.DaPayloadEntry;
  forcedPreimage?: SDK.DaPayloadEntry;
}>;

type PublicReplayFixture = Readonly<{
  observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
  reconstruction: WatcherHeaderRootReconstructionResultV1;
  phaseA: Awaited<ReturnType<typeof evaluateWatcherPhaseABlockV1>>;
  envelope: Buffer;
  priorState: readonly WatcherBlockReplayPriorUtxoV1[];
  eventAuthorities: readonly WatcherBlockReplayEventAuthorityV1[];
  header: SDK.HeaderV1;
}>;

const publicInput = (fixture: PublicReplayFixture) => ({
  observation: fixture.observation,
  reconstruction: fixture.reconstruction,
  phaseA: fixture.phaseA,
  payloadEnvelopeCbor: fixture.envelope,
  daProvenance: DA_PROVENANCE,
  priorState: fixture.priorState,
  eventAuthorities: fixture.eventAuthorities,
  ruleBundle: RULE_BUNDLE,
  ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
});

const buildPublicReplayFixture = async (input: {
  readonly txCbors?: readonly Buffer[];
  readonly events?: readonly PublicFixtureEvent[];
  readonly steps: readonly SDK.TransitionStep[];
  readonly priorState: readonly WatcherBlockReplayPriorUtxoV1[];
  readonly postState: readonly WatcherBlockReplayPriorUtxoV1[];
  readonly eventAuthorities?: readonly WatcherBlockReplayEventAuthorityV1[];
  readonly eventToStep?: readonly {
    readonly key: SDK.EventKey;
    readonly value: SDK.EventToStepValue;
  }[];
  readonly requireAcceptedBindings?: boolean;
  readonly minFeeB?: bigint;
}): Promise<PublicReplayFixture> => {
  const txCbors = input.txCbors ?? [];
  const events = input.events ?? [];
  const transactions = txCbors.map((canonicalCbor) => {
    const full = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalCbor);
    const proof =
      deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(canonicalCbor);
    const source: SDK.L2TransactionSourceV1 = {
      tx_id: computeMidgardNativeTxIdV1(full).toString("hex"),
      source: {
        compact_cbor: proof.compactCbor.toString("hex"),
        witness_set_compact_cbor: proof.witnessSetCompactCbor.toString("hex"),
        field_preimage_lengths_cbor:
          proof.fieldPreimageLengthsCbor.toString("hex"),
      },
    };
    return { canonicalCbor, source };
  });
  const transactionEntries: SDK.DaPayloadEntry[] = transactions.map(
    ({ source }) => [
      source.tx_id,
      dataHex(source, SDK.L2TransactionSourceV1Schema),
    ],
  );
  const preimageEntries: SDK.DaPayloadEntry[] = transactions.map(
    ({ canonicalCbor, source }) => [
      source.tx_id,
      canonicalCbor.toString("hex"),
    ],
  );
  const withdrawalEntries = events
    .filter(({ domain }) => domain === "withdrawals")
    .map(({ entry }) => entry);
  const forcedEntries = events
    .filter(({ domain }) => domain === "forced_transactions")
    .map(({ entry }) => entry);
  const depositEntries = events
    .filter(({ domain }) => domain === "deposits")
    .map(({ entry }) => entry);
  const forcedPreimages = events.flatMap(({ forcedPreimage }) =>
    forcedPreimage === undefined ? [] : [forcedPreimage],
  );
  const transitionEntries: SDK.DaPayloadEntry[] = input.steps.map((step) => [
    dataHex(step.step_index, Data.Integer()),
    dataHex(step, SDK.TransitionStepSchema),
  ]);
  const eventToStep =
    input.eventToStep ??
    input.steps.map((step) => ({
      key: step.event_key,
      value: { step_index: step.step_index, phase: step.phase },
    }));
  const eventToStepEntries: SDK.DaPayloadEntry[] = eventToStep.map(
    ({ key, value }) => [
      dataHex(key, SDK.EventKeySchema),
      dataHex(value, SDK.EventToStepValueSchema),
    ],
  );
  const validationKeys = [
    ...transactions.map(
      ({ source }) =>
        ({
          L2TransactionEventKey: { tx_id: source.tx_id },
        }) satisfies SDK.EventKey,
    ),
    ...events
      .filter(({ domain }) => domain === "forced_transactions")
      .map(({ eventKey }) => eventKey),
  ];
  const validationTraceEntries: SDK.DaPayloadEntry[] = validationKeys.map(
    (eventKey, index) => [
      dataHex(eventKey, SDK.EventKeySchema),
      dataHex(
        {
          schema_version: 1n,
          machine_version: 1n,
          trace_root: h32(140 + index),
          step_count: 1n,
          initial_state_hash: h32(150 + index),
          terminal_state_hash: h32(160 + index),
          verdict: "Accepted",
          rejection_code_hash: h32(170 + index),
        } satisfies SDK.ValidationTraceDescriptorV1,
        SDK.ValidationTraceDescriptorV1Schema,
      ),
    ],
  );
  const utxoEntries: SDK.DaPayloadEntry[] = input.postState.map((entry) => [
    entry.outRef,
    entry.outputCbor,
  ]);
  const countedRoot = async (
    domain: SDK.RootDomain,
    values: readonly SDK.DaPayloadEntry[],
  ): Promise<string> =>
    (await buildCountedRoot(domain, bufferEntries(values))).root;
  const priorRoot = await watcherBlockReplayPriorStateV1(input.priorState);
  const postRoot = await watcherBlockReplayPriorStateV1(input.postState);
  const counts = {
    withdrawalCount: BigInt(withdrawalEntries.length),
    forcedTransactionCount: BigInt(forcedEntries.length),
    l2TransactionCount: BigInt(transactionEntries.length),
    depositCount: BigInt(depositEntries.length),
    totalEventCount: BigInt(
      withdrawalEntries.length +
        forcedEntries.length +
        transactionEntries.length +
        depositEntries.length,
    ),
    transitionStepCount: BigInt(transitionEntries.length),
    validationTraceCount: BigInt(validationTraceEntries.length),
  };
  const header: SDK.HeaderV1 = {
    prevUtxosRoot: priorRoot.root,
    utxosRoot: postRoot.root,
    withdrawalsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      withdrawalEntries,
    ),
    forcedTransactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      forcedEntries,
    ),
    transactionsRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      transactionEntries,
    ),
    depositsRoot: await countedRoot(SDK.ROOT_DOMAINS.deposits, depositEntries),
    transitionTraceRoot: await countedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      transitionEntries,
    ),
    eventToStepRoot: await countedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      eventToStepEntries,
    ),
    validationTracesRoot: await countedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      validationTraceEntries,
    ),
    ...counts,
    startTime: 10n,
    endTime: 20n,
    blockSlot: 0n,
    expectedNetworkId: 0n,
    minFeeA: 0n,
    minFeeB: input.minFeeB ?? 0n,
    prevHeaderHash: h28(90),
    operatorVkey: h28(91),
    protocolVersion: BigInt(RULE_BUNDLE.protocolVersion),
  };
  const headerHash = headerHashOf(header);
  const payload: SDK.DaPayloadV1 = {
    version: SDK.DA_PAYLOAD_V1_VERSION,
    block_body: {
      header_hash: headerHash,
      header,
      utxos: sortEntries(utxoEntries),
      withdrawals: sortEntries(withdrawalEntries),
      forced_transactions: sortEntries(forcedEntries),
      transactions: sortEntries(transactionEntries),
      deposits: sortEntries(depositEntries),
      transition_trace: sortEntries(transitionEntries),
      event_to_step: sortEntries(eventToStepEntries),
      transaction_preimages: sortEntries(preimageEntries),
      forced_transaction_preimages: sortEntries(forcedPreimages),
      cek_program_material: [],
      validation_traces: sortEntries(validationTraceEntries),
      counts,
    },
  };
  const envelope = await wrapDaPayloadV1(SDK.encodeDaPayloadV1(payload), {
    mode: "identity",
  });
  const observation = await makeWatcherAuthenticatedHeaderObservationV1({
    header: watcherHeaderRecord(header, headerHash),
    chainPoint: CHAIN_POINT,
    confirmationDepth: 12,
    sourceMode: "local_node",
    provenance: L1_PROVENANCE,
  });
  const reconstruction = await evaluateWatcherHeaderRootReconstructionV1({
    observation,
    payloadEnvelopeCbor: envelope,
    daProvenance: DA_PROVENANCE,
  });
  const phaseA = await evaluateWatcherPhaseABlockV1({
    observation,
    reconstruction,
    payloadEnvelopeCbor: envelope,
    daProvenance: DA_PROVENANCE,
    ruleBundle: RULE_BUNDLE,
    ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
  });
  if (input.requireAcceptedBindings !== false) {
    expect(reconstruction.action).toBe("accept");
    expect(phaseA.action).toBe("accept");
  }
  return {
    observation,
    reconstruction,
    phaseA,
    envelope,
    priorState: input.priorState,
    eventAuthorities: input.eventAuthorities ?? [],
    header,
  };
};

describe("W25 published rejection-code partition", () => {
  it("is a disjoint total 13/27/10 partition of the canonical 50-code vocabulary", () => {
    expect(WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES_V1).toStrictEqual(
      Object.values(RejectCodes),
    );
    expect(WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES_V1).toHaveLength(50);
    expect(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1).toHaveLength(13);
    expect(WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES_V1).toHaveLength(27);
    expect(WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES_V1).toHaveLength(10);
    const claimed = [
      ...WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1,
      ...WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES_V1,
      ...WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES_V1,
    ];
    expect(new Set(claimed).size).toBe(50);
    expect([...claimed].sort()).toStrictEqual(
      [...WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES_V1].sort(),
    );
    expect(WATCHER_BLOCK_REPLAY_PROTOCOL_MINUS_UNCLAIMED_V1).toHaveLength(40);
    const expectedForcedValidity = (
      code: (typeof RejectCodes)[keyof typeof RejectCodes],
      phase: "phaseA" | "phaseB",
    ): string => {
      if (code === RejectCodes.InputNotFound) return "InputNotFound";
      if (
        code === RejectCodes.InvalidSignature ||
        code === RejectCodes.MissingRequiredWitness
      ) {
        return "AddressWitnessSignatureInvalid";
      }
      if (code === RejectCodes.NativeScriptInvalid) {
        // The one phase-split code: it must carry the same arm the node
        // classifier commits into the leaf for the phase that rejected.
        return phase === "phaseA"
          ? "WitnessNativeScriptFalse"
          : "ExecutionNativeScriptFalse";
      }
      if (
        code === RejectCodes.PlutusScriptInvalid ||
        code === RejectCodes.PlutusEvaluationUnavailable
      ) {
        return "PlutusExecutionFailed";
      }
      if (code === RejectCodes.MinFee) return "FeeBelowMinimum";
      return "ValueNotPreserved";
    };
    for (const phase of ["phaseA", "phaseB"] as const) {
      expect(
        Object.values(RejectCodes).map((code) => ({
          code,
          validity: watcherBlockReplayForcedValidityForRejectCodeV1(
            code,
            phase,
          ),
        })),
      ).toStrictEqual(
        Object.values(RejectCodes).map((code) => ({
          code,
          validity: expectedForcedValidity(code, phase),
        })),
      );
    }
    expect(WATCHER_PHASE_A_CANONICAL_REJECT_CODES_V1).toStrictEqual(
      WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES_V1,
    );
    expect(
      WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1.filter(
        (code) =>
          !new Set<string>(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1).has(
            code,
          ),
      ),
    ).toStrictEqual(WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES_V1);
    expect(
      WATCHER_PHASE_A_EXCLUDED_REJECT_CODES_V1.filter(
        (code) =>
          !new Set<string>(WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES_V1).has(
            code,
          ),
      ),
    ).toStrictEqual(
      WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1.filter(
        (code) =>
          !new Set<string>(WATCHER_PHASE_A_REACHABLE_REJECT_CODES_V1).has(code),
      ),
    );
  });

  it("keeps evidence and dominated claims inside Phase B's published set", () => {
    for (const code of WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES_V1) {
      expect(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1).toContain(code);
    }
    for (const code of WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODES_V1) {
      expect(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES_V1).toContain(code);
      expect(WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES_V1).not.toContain(
        code,
      );
    }
  });
});

describe("W25 roots and deterministic replay", () => {
  it("replays a Deposit before an L2 spend and repeat-isolates parser authority factories", async () => {
    expect(repeatIsolationEvidence).toStrictEqual({
      harnessDistinctDeploymentFixture: true,
      harnessDistinctTransports: true,
      harnessFrozenTransports: true,
      harnessDistinctProviders: true,
      w15DistinctTransports: true,
      w15FrozenTransports: true,
      w15DistinctDeploymentAuthority: true,
      w15DistinctProvider: true,
      w15ConcurrentLeasePartition: true,
      w15SetupFailureReleasedLease: true,
      w15DuplicateDisposeSharedPromise: true,
      w15DisposeOverlapRejected: true,
      w15StaleDisposePreservedFreshState: true,
      w15DeterministicReplay: true,
      w16DistinctTransports: true,
      w16FrozenTransports: true,
      w16DistinctDeploymentAuthority: true,
      w16DistinctProvider: true,
      w16ConcurrentLeasePartition: true,
      w16SetupFailureReleasedLease: true,
      w16DuplicateDisposeSharedPromise: true,
      w16DisposeOverlapRejected: true,
      w16StaleDisposePreservedFreshState: true,
      w16DeterministicReplay: true,
    });
    const userEvent = genuineW15.deposit;
    const event = publicEventFromW15(userEvent);
    const effect = depositEffectFromW15(userEvent);
    const inserted = effect.operations[0];
    if (inserted === undefined || inserted.type !== "insert") {
      throw new Error("genuine deposit did not derive one canonical insert");
    }
    const native = makeNativeTx({
      spendInputs: [inserted.outRefCbor],
      outputs: [inserted.outputCbor],
      privateKey: FIXED_KEY,
    });
    const produced = outRefFromTxId(native.txId);
    const l2Effect = nativeEffect({
      spent: [inserted.outRefCbor],
      native,
      outputs: [inserted.outputCbor],
    });
    const groups: readonly CommittedEffectGroup[] = [
      { eventKey: event.eventKey, phase: "Deposit", effect },
      {
        eventKey: {
          L2TransactionEventKey: { tx_id: native.txId.toString("hex") },
        },
        phase: "L2Transaction",
        effect: l2Effect,
      },
    ];
    const steps = await committedStepsForEffects([], groups);
    const authority = eventAuthority({
      event,
      userEvent,
      settlement: genuineW16.absorbToReserve,
      effect,
    });
    const fixture = await buildPublicReplayFixture({
      txCbors: [native.txCbor],
      events: [event],
      steps,
      priorState: [],
      postState: entries([[produced, inserted.outputCbor]]),
      eventAuthorities: [authority],
    });
    const result = await evaluateWatcherBlockReplayV1(publicInput(fixture));
    expect(result.action).toBe("accept");
    expect(result.reasonCodes).toStrictEqual([]);
    expect(genuineW16.spawn.parsed).toMatchObject({
      action: "accept",
      protocolDecision: "indexed",
    });
    expect(genuineW16.spawn.observation.transition.kind).toBe(
      "spawn_settlement",
    );
    expect(result.eventRoots).toMatchObject([
      {
        stepIndex: 0,
        mutationCount: 1,
        preRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        postRoot:
          "4ca10b30d2e8cc797b52492b7f032f5f4272c01a3b315d5485fd6861a83a92ff",
      },
    ]);
    expect(
      result.intermediateRoots.map(
        ({ operation, outRef, preRoot, postRoot }) => ({
          operation,
          outRef,
          preRoot,
          postRoot,
        }),
      ),
    ).toStrictEqual([
      {
        operation: "insert",
        outRef: inserted.outRefCbor.toString("hex"),
        preRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        postRoot:
          "4ca10b30d2e8cc797b52492b7f032f5f4272c01a3b315d5485fd6861a83a92ff",
      },
      {
        operation: "delete",
        outRef: inserted.outRefCbor.toString("hex"),
        preRoot:
          "4ca10b30d2e8cc797b52492b7f032f5f4272c01a3b315d5485fd6861a83a92ff",
        postRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      },
      {
        operation: "insert",
        outRef: produced.toString("hex"),
        preRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        postRoot:
          "e44dfc58ff680f66ac69719213544893bf40f18146ae06dc3515011bb062579e",
      },
    ]);
    expect(result.authorityManifestDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.sourceManifestDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.effectManifestDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.verifiedRequires).toBe(
      WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT_V1,
    );
    expect(result.downstreamPrerequisite).toStrictEqual({
      schemaVersion:
        WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_V1_SCHEMA_VERSION,
      requiredVerifier: "W26",
      inputDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
      w29Eligibility: "requires_w26_accept",
    });

    const omittedAuthority = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      eventAuthorities: [],
    });
    expect(omittedAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["missing_event_authority"],
    });
    const duplicatedAuthority = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      eventAuthorities: [authority, authority],
    });
    expect(duplicatedAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["duplicate_event_authority"],
    });
    const substitutedAuthority = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      eventAuthorities: [
        { ...authority, eventKey: depositEvent(0x34).eventKey },
      ],
    });
    expect(substitutedAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["user_event_authority_identity_mismatch"],
    });
    const mutatedEffect = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      eventAuthorities: [
        {
          ...authority,
          transitionEffect: buildCanonicalTransitionEffectV1([]),
        },
      ],
    });
    expect(mutatedEffect).toMatchObject({
      action: "error",
      reasonCodes: ["transition_effect_semantics_mismatch"],
    });
  });

  it("replays a Withdrawal after an L2 transaction and checks its literal terminal root", async () => {
    const userEvent = genuineW15.withdrawal;
    const event = publicEventFromW15(userEvent);
    const l2Effect = nativeEffect({
      spent: [WITHDRAWAL_FLOW_INPUT],
      native: WITHDRAWAL_FLOW_NATIVE,
      outputs: [FLOW_OUTPUT],
    });
    const effect = withdrawalEffectFromW15(userEvent, true);
    const priorState = entries([[WITHDRAWAL_FLOW_INPUT, FLOW_OUTPUT]]);
    const groups: readonly CommittedEffectGroup[] = [
      {
        eventKey: {
          L2TransactionEventKey: {
            tx_id: WITHDRAWAL_FLOW_NATIVE.txId.toString("hex"),
          },
        },
        phase: "L2Transaction",
        effect: l2Effect,
      },
      { eventKey: event.eventKey, phase: "Withdrawal", effect },
    ];
    const steps = await committedStepsForEffects(priorState, groups);
    const authority = eventAuthority({
      event,
      userEvent,
      settlement: genuineW16.initializePayout,
      effect,
    });
    const fixture = await buildPublicReplayFixture({
      txCbors: [WITHDRAWAL_FLOW_NATIVE.txCbor],
      events: [event],
      steps,
      priorState,
      postState: [],
      eventAuthorities: [authority],
    });
    const result = await evaluateWatcherBlockReplayV1(publicInput(fixture));
    expect(result.action).toBe("accept");
    expect(result.reasonCodes).toStrictEqual([]);
    expect(result.eventRoots).toMatchObject([
      {
        stepIndex: 1,
        mutationCount: 1,
        preRoot:
          "4021f48461d5b6cbb121823b22084b1712f52dce0c85dc110ddfa0255dbc9a95",
        postRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      },
    ]);
    expect(
      result.intermediateRoots.map(
        ({ operation, outRef, preRoot, postRoot }) => ({
          operation,
          outRef,
          preRoot,
          postRoot,
        }),
      ),
    ).toStrictEqual([
      {
        operation: "delete",
        outRef: WITHDRAWAL_FLOW_INPUT.toString("hex"),
        preRoot:
          "5644ea026e5cd1d8e5b39c0818e365cb272723b9fe1f68f6a757b751a1538725",
        postRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      },
      {
        operation: "insert",
        outRef: outRefFromTxId(WITHDRAWAL_FLOW_NATIVE.txId).toString("hex"),
        preRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        postRoot:
          "4021f48461d5b6cbb121823b22084b1712f52dce0c85dc110ddfa0255dbc9a95",
      },
      {
        operation: "delete",
        outRef: outRefFromTxId(WITHDRAWAL_FLOW_NATIVE.txId).toString("hex"),
        preRoot:
          "4021f48461d5b6cbb121823b22084b1712f52dce0c85dc110ddfa0255dbc9a95",
        postRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      },
    ]);

    const refundEffect = withdrawalEffectFromW15(userEvent, false);
    const refundGroups: readonly CommittedEffectGroup[] = [
      groups[0]!,
      { eventKey: event.eventKey, phase: "Withdrawal", effect: refundEffect },
    ];
    const refundSteps = await committedStepsForEffects(
      priorState,
      refundGroups,
    );
    const refundedProduced = outRefFromTxId(WITHDRAWAL_FLOW_NATIVE.txId);
    const refundFixture = await buildPublicReplayFixture({
      txCbors: [WITHDRAWAL_FLOW_NATIVE.txCbor],
      events: [event],
      steps: refundSteps,
      priorState,
      postState: entries([[refundedProduced, FLOW_OUTPUT]]),
      eventAuthorities: [
        eventAuthority({
          event,
          userEvent,
          settlement: genuineW16.refundWithdrawal,
          effect: refundEffect,
        }),
      ],
    });
    const refund = await evaluateWatcherBlockReplayV1(
      publicInput(refundFixture),
    );
    expect(refund).toMatchObject({ action: "accept", reasonCodes: [] });
    expect(refund.eventRoots).toMatchObject([
      {
        stepIndex: 1,
        mutationCount: 0,
        preRoot:
          "4021f48461d5b6cbb121823b22084b1712f52dce0c85dc110ddfa0255dbc9a95",
        postRoot:
          "4021f48461d5b6cbb121823b22084b1712f52dce0c85dc110ddfa0255dbc9a95",
      },
    ]);
    expect(
      refund.intermediateRoots.map(({ operation, preRoot, postRoot }) => ({
        operation,
        preRoot,
        postRoot,
      })),
    ).toStrictEqual([
      {
        operation: "delete",
        preRoot:
          "5644ea026e5cd1d8e5b39c0818e365cb272723b9fe1f68f6a757b751a1538725",
        postRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      },
      {
        operation: "insert",
        preRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        postRoot:
          "4021f48461d5b6cbb121823b22084b1712f52dce0c85dc110ddfa0255dbc9a95",
      },
    ]);
    expect(refund.downstreamPrerequisite.inputDigest).not.toBe(
      result.downstreamPrerequisite.inputDigest,
    );

    const mutatedSettlement = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      eventAuthorities: [
        {
          ...authority,
          settlement: {
            ...authority.settlement!,
            observationDigest: h32(0xee),
          },
        },
      ],
    });
    expect(mutatedSettlement).toMatchObject({
      action: "error",
      reasonCodes: ["settlement_authority_identity_mismatch"],
    });
  });

  it("replays a ForcedTransaction before a later L2 spend without stale-state batching", async () => {
    const userEvent = genuineW15.forced;
    const event = publicEventFromW15(userEvent, FORCED_FLOW_NATIVE);
    const forcedProduced = outRefFromTxId(FORCED_FLOW_NATIVE.txId);
    const laterNative = makeNativeTx({
      spendInputs: [forcedProduced],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
    });
    const laterProduced = outRefFromTxId(laterNative.txId);
    const forcedEffect = nativeEffect({
      spent: [FORCED_FLOW_INPUT],
      native: FORCED_FLOW_NATIVE,
      outputs: [FLOW_OUTPUT],
    });
    const laterEffect = nativeEffect({
      spent: [forcedProduced],
      native: laterNative,
      outputs: [FLOW_OUTPUT],
    });
    const priorState = entries([[FORCED_FLOW_INPUT, FLOW_OUTPUT]]);
    const groups: readonly CommittedEffectGroup[] = [
      {
        eventKey: event.eventKey,
        phase: "ForcedTransaction",
        effect: forcedEffect,
      },
      {
        eventKey: {
          L2TransactionEventKey: { tx_id: laterNative.txId.toString("hex") },
        },
        phase: "L2Transaction",
        effect: laterEffect,
      },
    ];
    const steps = await committedStepsForEffects(priorState, groups);
    const authority = eventAuthority({
      event,
      userEvent,
      effect: forcedEffect,
      forcedNative: FORCED_FLOW_NATIVE,
    });
    const fixture = await buildPublicReplayFixture({
      txCbors: [laterNative.txCbor],
      events: [event],
      steps,
      priorState,
      postState: entries([[laterProduced, FLOW_OUTPUT]]),
      eventAuthorities: [authority],
    });
    const result = await evaluateWatcherBlockReplayV1(publicInput(fixture));
    expect(result.action).toBe("accept");
    expect(result.reasonCodes).toStrictEqual([]);
    if (!("ForcedTransactionEventKey" in authority.eventKey)) {
      throw new Error("forced authority key narrowed to another event kind");
    }
    const forcedOrderId =
      authority.eventKey.ForcedTransactionEventKey.tx_order_id;
    expect(result.forcedValidationFacts).toStrictEqual([
      {
        eventKeyFingerprint: `ForcedTransaction:${forcedOrderId.transactionId}:${forcedOrderId.outputIndex.toString()}`,
        stepIndex: 0,
        authenticatedOperatorValidity: "ForcedTxValid",
        canonicalOperatorValidity: "ForcedTxValid",
        phaseAStatus: "accepted",
        phaseARejectCode: null,
        phaseBStatus: "accepted",
        phaseBRejectCode: null,
        canonicalEffectDigest: forcedEffect.digest,
        canonicalEffectMutationCount: 2,
      },
    ]);
    expect(Object.isFrozen(result.forcedValidationFacts)).toBe(true);
    expect(Object.isFrozen(result.forcedValidationFacts[0])).toBe(true);
    expect(result.eventRoots).toMatchObject([
      {
        stepIndex: 0,
        mutationCount: 2,
        preRoot:
          "0731b86274a38588a437eaafb5288833d274eb69b244f22017b3b1dd8001d78a",
        postRoot:
          "5953343c3f81ddf5c326ba94abdf4f6b2f862d2055d8f8425e01ad23aaac7dfd",
      },
    ]);
    expect(
      result.intermediateRoots.map(
        ({ operation, outRef, preRoot, postRoot }) => ({
          operation,
          outRef,
          preRoot,
          postRoot,
        }),
      ),
    ).toStrictEqual([
      {
        operation: "delete",
        outRef: FORCED_FLOW_INPUT.toString("hex"),
        preRoot:
          "0731b86274a38588a437eaafb5288833d274eb69b244f22017b3b1dd8001d78a",
        postRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      },
      {
        operation: "insert",
        outRef: forcedProduced.toString("hex"),
        preRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        postRoot:
          "5953343c3f81ddf5c326ba94abdf4f6b2f862d2055d8f8425e01ad23aaac7dfd",
      },
      {
        operation: "delete",
        outRef: forcedProduced.toString("hex"),
        preRoot:
          "5953343c3f81ddf5c326ba94abdf4f6b2f862d2055d8f8425e01ad23aaac7dfd",
        postRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      },
      {
        operation: "insert",
        outRef: laterProduced.toString("hex"),
        preRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        postRoot:
          "904811f9e63105745563c76031dd888b345a1ac96b0bb1a1acb0fb6122ef836a",
      },
    ]);

    const mutatedNativeBytes = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      eventAuthorities: [
        {
          ...authority,
          canonicalNativeTxCbor: WITHDRAWAL_FLOW_NATIVE.txCbor,
        },
      ],
    });
    expect(mutatedNativeBytes).toMatchObject({
      action: "error",
      reasonCodes: ["transition_effect_semantics_mismatch"],
    });

    const noOpEffect = buildCanonicalTransitionEffectV1([]);
    const expectedOutcomes = {
      InputNotFound: {
        phaseAStatus: "accepted",
        phaseARejectCode: null,
        phaseBStatus: "rejected",
        phaseBRejectCode: RejectCodes.InputNotFound,
      },
      AddressWitnessSignatureInvalid: {
        phaseAStatus: "rejected",
        phaseARejectCode: RejectCodes.InvalidSignature,
        phaseBStatus: "not_run",
        phaseBRejectCode: null,
      },
      WitnessNativeScriptFalse: {
        phaseAStatus: "rejected",
        phaseARejectCode: RejectCodes.NativeScriptInvalid,
        phaseBStatus: "not_run",
        phaseBRejectCode: null,
      },
      FeeBelowMinimum: {
        phaseAStatus: "rejected",
        phaseARejectCode: RejectCodes.MinFee,
        phaseBStatus: "not_run",
        phaseBRejectCode: null,
      },
      ValueNotPreserved: {
        phaseAStatus: "accepted",
        phaseARejectCode: null,
        phaseBStatus: "rejected",
        phaseBRejectCode: RejectCodes.ValueNotPreserved,
      },
    } as const;
    const invalidReplayEvidence: Partial<
      Record<
        keyof typeof FORCED_INVALID_CASES,
        Readonly<{
          fixture: PublicReplayFixture;
          authority: WatcherBlockReplayEventAuthorityV1;
          event: PublicFixtureEvent;
          result: Awaited<ReturnType<typeof evaluateWatcherBlockReplayV1>>;
        }>
      >
    > = {};
    for (const [category, invalidCase] of Object.entries(
      FORCED_INVALID_CASES,
    ) as [
      keyof typeof FORCED_INVALID_CASES,
      (typeof FORCED_INVALID_CASES)[keyof typeof FORCED_INVALID_CASES],
    ][]) {
      expect(
        decodeMidgardNativeTxFullV1FromCanonicalCbor(invalidCase.native.txCbor)
          .validity,
      ).toBe("TxIsValid");
      const invalidUserEvent = genuineW15.forcedVariants[category]!;
      const invalidEvent = publicEventFromW15(
        invalidUserEvent,
        invalidCase.native,
      );
      const invalidPriorState =
        category === "InputNotFound"
          ? []
          : entries([[invalidCase.input, FLOW_OUTPUT]]);
      const invalidSteps = await committedStepsForEffects(invalidPriorState, [
        {
          eventKey: invalidEvent.eventKey,
          phase: "ForcedTransaction",
          effect: noOpEffect,
        },
      ]);
      const invalidAuthority = eventAuthority({
        event: invalidEvent,
        userEvent: invalidUserEvent,
        effect: noOpEffect,
        forcedNative: invalidCase.native,
      });
      const invalidFixture = await buildPublicReplayFixture({
        events: [invalidEvent],
        steps: invalidSteps,
        priorState: invalidPriorState,
        postState: invalidPriorState,
        eventAuthorities: [invalidAuthority],
        ...(category === "FeeBelowMinimum" ? { minFeeB: 1n } : {}),
      });
      const invalidResult = await evaluateWatcherBlockReplayV1(
        publicInput(invalidFixture),
      );
      expect(invalidResult.action, category).toBe("accept");
      expect(invalidResult.intermediateRoots, category).toStrictEqual([]);
      expect(invalidResult.eventRoots, category).toMatchObject([
        { stepIndex: 0, mutationCount: 0 },
      ]);
      expect(invalidResult.forcedValidationFacts, category).toMatchObject([
        {
          stepIndex: 0,
          authenticatedOperatorValidity: invalidCase.operatorValidity,
          canonicalOperatorValidity: invalidCase.operatorValidity,
          ...expectedOutcomes[category],
          canonicalEffectDigest: noOpEffect.digest,
          canonicalEffectMutationCount: 0,
        },
      ]);
      invalidReplayEvidence[category] = Object.freeze({
        fixture: invalidFixture,
        authority: invalidAuthority,
        event: invalidEvent,
        result: invalidResult,
      });
    }

    const restartEvidence = invalidReplayEvidence.ValueNotPreserved!;
    const restarted = await evaluateWatcherBlockReplayV1(
      publicInput(restartEvidence.fixture),
    );
    expect(restarted).toStrictEqual(restartEvidence.result);
    expect(restarted.resultDigest).toBe(restartEvidence.result.resultDigest);

    const omittedAuthority = await evaluateWatcherBlockReplayV1({
      ...publicInput(restartEvidence.fixture),
      eventAuthorities: [],
    });
    expect(omittedAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["missing_event_authority"],
    });
    const duplicateAuthority = await evaluateWatcherBlockReplayV1({
      ...publicInput(restartEvidence.fixture),
      eventAuthorities: [restartEvidence.authority, restartEvidence.authority],
    });
    expect(duplicateAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["duplicate_event_authority"],
    });

    const mismatchUserEvent = genuineW15.forcedVariants.Mismatch!;
    const mismatchEvent = publicEventFromW15(
      mismatchUserEvent,
      FORCED_INVALID_CASES.ValueNotPreserved.native,
    );
    const mismatchPriorState = entries([
      [FORCED_INVALID_CASES.ValueNotPreserved.input, FLOW_OUTPUT],
    ]);
    const mismatchSteps = await committedStepsForEffects(mismatchPriorState, [
      {
        eventKey: mismatchEvent.eventKey,
        phase: "ForcedTransaction",
        effect: noOpEffect,
      },
    ]);
    const mismatchAuthority = eventAuthority({
      event: mismatchEvent,
      userEvent: mismatchUserEvent,
      effect: noOpEffect,
      forcedNative: FORCED_INVALID_CASES.ValueNotPreserved.native,
    });
    const mismatchFixture = await buildPublicReplayFixture({
      events: [mismatchEvent],
      steps: mismatchSteps,
      priorState: mismatchPriorState,
      postState: mismatchPriorState,
      eventAuthorities: [mismatchAuthority],
    });
    expect(
      await evaluateWatcherBlockReplayV1(publicInput(mismatchFixture)),
    ).toMatchObject({
      action: "error",
      reasonCodes: ["transition_effect_semantics_mismatch"],
    });

    const tamperedFactResult = {
      ...restartEvidence.result,
      forcedValidationFacts: restartEvidence.result.forcedValidationFacts.map(
        (fact) => ({ ...fact, canonicalEffectMutationCount: 1 }),
      ),
    };
    const { resultDigest: _tamperedDigest, ...tamperedFactMaterial } =
      tamperedFactResult;
    expect(watcherSha256CanonicalJsonV1(tamperedFactMaterial)).not.toBe(
      restartEvidence.result.resultDigest,
    );

    const inputNotFoundEvidence = invalidReplayEvidence.InputNotFound!;
    const invalidSignatureEvidence =
      invalidReplayEvidence.AddressWitnessSignatureInvalid!;
    const orderedGroups: readonly CommittedEffectGroup[] = [
      {
        eventKey: inputNotFoundEvidence.event.eventKey,
        phase: "ForcedTransaction",
        effect: noOpEffect,
      },
      {
        eventKey: invalidSignatureEvidence.event.eventKey,
        phase: "ForcedTransaction",
        effect: noOpEffect,
      },
    ];
    const orderedPriorState = entries([
      [FORCED_INVALID_CASES.AddressWitnessSignatureInvalid.input, FLOW_OUTPUT],
    ]);
    const orderedFixture = await buildPublicReplayFixture({
      events: [inputNotFoundEvidence.event, invalidSignatureEvidence.event],
      steps: await committedStepsForEffects(orderedPriorState, orderedGroups),
      priorState: orderedPriorState,
      postState: orderedPriorState,
      eventAuthorities: [
        invalidSignatureEvidence.authority,
        inputNotFoundEvidence.authority,
      ],
    });
    const orderedResult = await evaluateWatcherBlockReplayV1(
      publicInput(orderedFixture),
    );
    expect(
      orderedResult.forcedValidationFacts.map(
        ({ authenticatedOperatorValidity }) => authenticatedOperatorValidity,
      ),
    ).toStrictEqual(["InputNotFound", "AddressWitnessSignatureInvalid"]);
    const reversedGroups = [...orderedGroups].reverse();
    const reversedFixture = await buildPublicReplayFixture({
      events: [inputNotFoundEvidence.event, invalidSignatureEvidence.event],
      steps: await committedStepsForEffects(orderedPriorState, reversedGroups),
      priorState: orderedPriorState,
      postState: orderedPriorState,
      eventAuthorities: [
        inputNotFoundEvidence.authority,
        invalidSignatureEvidence.authority,
      ],
    });
    const reversedResult = await evaluateWatcherBlockReplayV1(
      publicInput(reversedFixture),
    );
    expect(
      reversedResult.forcedValidationFacts.map(
        ({ authenticatedOperatorValidity }) => authenticatedOperatorValidity,
      ),
    ).toStrictEqual(["AddressWitnessSignatureInvalid", "InputNotFound"]);
    expect(reversedResult.downstreamPrerequisite.inputDigest).not.toBe(
      orderedResult.downstreamPrerequisite.inputDigest,
    );
  });

  it("binds accepted W21/W22/W23/W24 evidence through the public replay entry point", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE_V1, FIXED_ADDRESS);
    const native = makeNativeTx({
      spendInputs: [spent],
      outputs: [output],
      privateKey: FIXED_KEY,
    });
    const txId = native.txId.toString("hex");
    const produced = outRefFromTxId(native.txId);
    const priorState = entries([[spent, output]]);
    const postState = entries([[produced, output]]);
    const fixture = await buildPublicReplayFixture({
      txCbors: [native.txCbor],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: { L2TransactionEventKey: { tx_id: txId } },
          phase: "L2Transaction",
          pre_utxos_root:
            "427ba76822e773ce7ad8392ff4758785dade1722a30219e54070b20b1c9159b7",
          post_utxos_root:
            "52b9c88cd96dfa08f6f35d7c484b3a89059f2ee485ca8a58efeaa2c52171ebbd",
        },
      ],
      priorState,
      postState,
    });

    const result = await evaluateWatcherBlockReplayV1(publicInput(fixture));
    expect(() => assertWatcherFullBlockReplayResultV1(result)).not.toThrow();
    expect(() => assertWatcherFullBlockReplayResultV1({ ...result })).toThrow(
      "watcher full block-replay result is not admitted",
    );
    expect(result).toMatchObject({
      action: "accept",
      reasonCodes: [],
      headerHash: fixture.reconstruction.headerHash,
      payloadEnvelopeSha256: fixture.reconstruction.payloadEnvelopeSha256,
      reconstructionDigest: fixture.reconstruction.resultDigest,
      phaseAResultDigest: fixture.phaseA.resultDigest,
      ruleBundleCommitment: RULE_BUNDLE_COMMITMENT,
      priorStateRoot:
        "427ba76822e773ce7ad8392ff4758785dade1722a30219e54070b20b1c9159b7",
      postStateRoot:
        "52b9c88cd96dfa08f6f35d7c484b3a89059f2ee485ca8a58efeaa2c52171ebbd",
      acceptedTxIds: [txId],
    });
    expect(result.intermediateRoots).toStrictEqual([
      {
        sequence: 0,
        txIndex: 0,
        txId,
        stepIndex: 0,
        phase: "L2Transaction",
        operation: "delete",
        outRef: spent.toString("hex"),
        preRoot:
          "427ba76822e773ce7ad8392ff4758785dade1722a30219e54070b20b1c9159b7",
        postRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      },
      {
        sequence: 1,
        txIndex: 0,
        txId,
        stepIndex: 0,
        phase: "L2Transaction",
        operation: "insert",
        outRef: produced.toString("hex"),
        preRoot:
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
        postRoot:
          "52b9c88cd96dfa08f6f35d7c484b3a89059f2ee485ca8a58efeaa2c52171ebbd",
      },
    ]);
    expect(result.transactionRoots).toStrictEqual([
      {
        txIndex: 0,
        txId,
        preRoot:
          "427ba76822e773ce7ad8392ff4758785dade1722a30219e54070b20b1c9159b7",
        postRoot:
          "52b9c88cd96dfa08f6f35d7c484b3a89059f2ee485ca8a58efeaa2c52171ebbd",
        mutationCount: 2,
        committedStepIndex: 0,
        committedPreRoot:
          "427ba76822e773ce7ad8392ff4758785dade1722a30219e54070b20b1c9159b7",
        committedPostRoot:
          "52b9c88cd96dfa08f6f35d7c484b3a89059f2ee485ca8a58efeaa2c52171ebbd",
      },
    ]);
    expect(result.resultDigest).toBe(
      watcherSha256CanonicalJsonV1(
        Object.fromEntries(
          Object.entries(result).filter(([key]) => key !== "resultDigest"),
        ),
      ),
    );

    const durable = makeWatcherBlockReplayReconstructedStateV1({
      result,
      chainPointId: `${CHAIN_POINT.slot.toString()}:${CHAIN_POINT.blockHash}`,
      inputIds: [fixture.reconstruction.payloadEnvelopeSha256!],
    });
    expect(durable).toMatchObject({
      blockHash: fixture.reconstruction.headerHash,
      priorStateRoot: result.priorStateRoot,
      postStateRoot: result.postStateRoot,
      inputIds: [fixture.reconstruction.payloadEnvelopeSha256],
    });

    const unsupportedReconstruction = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      reconstruction: {
        ...fixture.reconstruction,
        schemaVersion: "future-reconstruction-v2",
      } as never,
    });
    expect(unsupportedReconstruction).toMatchObject({
      action: "error",
      reasonCodes: ["reconstruction_unsupported_schema"],
    });
    const badReconstructionDigest = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      reconstruction: {
        ...fixture.reconstruction,
        resultDigest: h32(0xee),
      },
    });
    expect(badReconstructionDigest).toMatchObject({
      action: "error",
      reasonCodes: ["reconstruction_digest_mismatch"],
    });
    const unsupportedPhaseA = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      phaseA: {
        ...fixture.phaseA,
        schemaVersion: "future-phase-a-v2",
      } as never,
    });
    expect(unsupportedPhaseA).toMatchObject({
      action: "error",
      reasonCodes: ["phase_a_unsupported_schema"],
    });
    const badPhaseADigest = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      phaseA: { ...fixture.phaseA, resultDigest: h32(0xef) },
    });
    expect(badPhaseADigest).toMatchObject({
      action: "error",
      reasonCodes: ["phase_a_digest_mismatch"],
    });
    const corruptedEnvelope = Buffer.from(fixture.envelope);
    corruptedEnvelope[corruptedEnvelope.length - 1] ^= 1;
    const corruptedBytes = await evaluateWatcherBlockReplayV1({
      ...publicInput(fixture),
      payloadEnvelopeCbor: corruptedEnvelope,
    });
    expect(corruptedBytes).toMatchObject({
      action: "error",
      reasonCodes: ["canonical_reconstruction_failed"],
    });

    let unknownCode: unknown;
    try {
      watcherBlockReplayRejectionProjectionV1({
        rejected: {
          txId: native.txId,
          code: "E_FUTURE_UNKNOWN" as never,
          consensusPhase: "terminal",
          detail: "future producer code",
        },
        indexByTxId: new Map([[txId, 0]]),
      });
    } catch (error) {
      unknownCode = error;
    }
    expect(unknownCode).toMatchObject({ code: "unknown_reject_code" });
  });

  it("reproduces every canonical intermediate mutation root and its exact post root", async () => {
    const firstInput = outRefFromByte(0x11);
    const secondInput = outRefFromByte(0x12);
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE_V1, FIXED_ADDRESS);
    const first = makePhaseBCandidate({
      spent: [firstInput],
      outputs: [output],
      privateKey: FIXED_KEY,
    });
    const second = makePhaseBCandidate({
      spent: [secondInput],
      arrivalSeq: 1n,
      outputs: [output],
      privateKey: FIXED_KEY,
    });
    const priorState = entries([
      [firstInput, output],
      [secondInput, output],
    ]);

    // #517: the candidate-level entry point recomputes every root but binds
    // none of them to a committed value, so it is never an acceptance. The
    // roots below are still the point of this case; the verdict is pinned by
    // the adversarial case that follows.
    const unbound = await replay([first, second], priorState);
    expect(unbound.action).toBe("reject");
    expect(unbound.priorStateRoot).toBe(
      "49476a071f7393279ca22a35d4ebe3b3316190c47890e5af7f3f12fded51c915",
    );
    expect(unbound.postStateRoot).toBe(
      "6d4a5867c105f9c81fa71dfea9c531063c1b3d88d28539b77941eab4ec6c58ac",
    );
    expect(unbound.intermediateRoots).toStrictEqual(FIXED_TWO_TX_ROOTS);
    expect(unbound.transactionRoots).toHaveLength(2);
    for (const [index, root] of unbound.intermediateRoots.entries()) {
      expect(root.sequence).toBe(index);
      expect(root.postRoot).toMatch(/^[0-9a-f]{64}$/u);
      if (index > 0) {
        expect(root.preRoot).toBe(
          unbound.intermediateRoots[index - 1]?.postRoot,
        );
      }
    }
    expect(unbound.transactionRoots[0]?.preRoot).toBe(unbound.priorStateRoot);
    expect(unbound.transactionRoots[1]?.preRoot).toBe(
      unbound.transactionRoots[0]?.postRoot,
    );
    expect(unbound.postStateRoot).toBe(unbound.transactionRoots[1]?.postRoot);

    const bound = await replay(
      [first, second],
      priorState,
      unbound.postStateRoot!,
    );
    expect(bound).toMatchObject({
      action: "reject",
      priorStateRoot: unbound.priorStateRoot,
      postStateRoot: unbound.postStateRoot,
      intermediateRoots: unbound.intermediateRoots,
    });
  });

  it("refuses acceptance while either committed binding is unrun", async () => {
    // #517. Before this case, `finalizeResult` derived `accept` from an empty
    // `reasonCodes` set while both bindings that compare a recomputed root
    // against an operator commitment - the committed transition trace and the
    // header `utxosRoot` - were skipped whenever the caller supplied neither.
    // A replayed block was therefore accepted with nothing compared at all.
    const spent = outRefFromByte(0x11);
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE_V1, FIXED_ADDRESS);
    const native = makeNativeTx({
      spendInputs: [spent],
      outputs: [output],
      privateKey: FIXED_KEY,
    });
    const txId = native.txId.toString("hex");
    const committedPriorRoot =
      "427ba76822e773ce7ad8392ff4758785dade1722a30219e54070b20b1c9159b7";
    const committedPostRoot =
      "52b9c88cd96dfa08f6f35d7c484b3a89059f2ee485ca8a58efeaa2c52171ebbd";
    const priorState = entries([[spent, output]]);
    const postState = entries([[outRefFromTxId(native.txId), output]]);

    // Control: the same block through the fully bound public entry point,
    // where both bindings run against the L1-committed material.
    const fixture = await buildPublicReplayFixture({
      txCbors: [native.txCbor],
      steps: [
        {
          schema_version: 1n,
          step_index: 0n,
          event_key: { L2TransactionEventKey: { tx_id: txId } },
          phase: "L2Transaction",
          pre_utxos_root: committedPriorRoot,
          post_utxos_root: committedPostRoot,
        },
      ],
      priorState,
      postState,
    });
    const accepted = await evaluateWatcherBlockReplayV1(publicInput(fixture));
    expect(accepted).toMatchObject({
      action: "accept",
      reasonCodes: [],
      postStateRoot: committedPostRoot,
    });

    // The adversarial case: the identical transaction over the identical prior
    // state, replayed with neither committed binding supplied. The recomputed
    // roots are byte-identical to the accepted control, so the binding gate is
    // the only thing separating the two verdicts.
    const candidate = makePhaseBCandidate({
      spent: [spent],
      outputs: [output],
      privateKey: FIXED_KEY,
    });
    const unrun = await replay([candidate], priorState);
    expect(unrun.acceptedTxIds).toStrictEqual([txId]);
    expect(unrun.priorStateRoot).toBe(committedPriorRoot);
    expect(unrun.postStateRoot).toBe(committedPostRoot);
    expect(unrun.action).toBe("reject");
    expect(unrun.reasonCodes).toStrictEqual([
      "committed_trace_binding_unrun",
      "post_state_binding_unrun",
    ]);

    // Half the bindings is still not acceptance: supplying the *correct*
    // committed post-state root leaves the transition trace unbound, and the
    // remaining reason code names exactly which binding never ran.
    const halfBound = await replay([candidate], priorState, committedPostRoot);
    expect(halfBound.action).toBe("reject");
    expect(halfBound.reasonCodes).toStrictEqual([
      "committed_trace_binding_unrun",
    ]);
    expect(halfBound.stageMismatches).toStrictEqual([]);

    // And the durable W03 record cannot be minted from an unbound replay.
    for (const result of [unrun, halfBound]) {
      expect(() =>
        makeWatcherBlockReplayReconstructedStateV1({
          result,
          chainPointId: `${CHAIN_POINT.slot.toString()}:${CHAIN_POINT.blockHash}`,
          inputIds: [h32(0x5a)],
        }),
      ).toThrow("result_not_accepted");
    }
  });

  it("fails closed on trace substitution, omission, duplication/reorder, trailing steps, wrong roots, and event_to_step drift", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE_V1, FIXED_ADDRESS);
    const native = makeNativeTx({
      spendInputs: [spent],
      outputs: [output],
      privateKey: FIXED_KEY,
    });
    const txId = native.txId.toString("hex");
    const eventKey = {
      L2TransactionEventKey: { tx_id: txId },
    } satisfies SDK.EventKey;
    const priorState = entries([[spent, output]]);
    const postState = entries([[outRefFromTxId(native.txId), output]]);
    const correctStep: SDK.TransitionStep = {
      schema_version: 1n,
      step_index: 0n,
      event_key: eventKey,
      phase: "L2Transaction",
      pre_utxos_root:
        "427ba76822e773ce7ad8392ff4758785dade1722a30219e54070b20b1c9159b7",
      post_utxos_root:
        "52b9c88cd96dfa08f6f35d7c484b3a89059f2ee485ca8a58efeaa2c52171ebbd",
    };

    const wrongRoots = await buildPublicReplayFixture({
      txCbors: [native.txCbor],
      steps: [
        {
          ...correctStep,
          pre_utxos_root: h32(0xa1),
          post_utxos_root: h32(0xa2),
        },
      ],
      priorState,
      postState,
    });
    expect(
      await evaluateWatcherBlockReplayV1(publicInput(wrongRoots)),
    ).toMatchObject({
      action: "reject",
      reasonCodes: ["transition_trace_mismatch", "intermediate_root_mismatch"],
    });

    const omitted = await buildPublicReplayFixture({
      txCbors: [native.txCbor],
      steps: [],
      eventToStep: [
        {
          key: eventKey,
          value: { step_index: 0n, phase: "L2Transaction" },
        },
      ],
      priorState,
      postState,
    });
    expect(
      await evaluateWatcherBlockReplayV1(publicInput(omitted)),
    ).toMatchObject({
      action: "error",
      reasonCodes: ["transition_trace_mismatch"],
    });

    const malformedFixtures = await Promise.all([
      buildPublicReplayFixture({
        txCbors: [native.txCbor],
        steps: [correctStep],
        eventToStep: [
          {
            key: eventKey,
            value: { step_index: 1n, phase: "Deposit" },
          },
        ],
        priorState,
        postState,
        requireAcceptedBindings: false,
      }),
      buildPublicReplayFixture({
        txCbors: [native.txCbor],
        steps: [{ ...correctStep, step_index: 1n }, correctStep],
        eventToStep: [
          {
            key: eventKey,
            value: { step_index: 0n, phase: "L2Transaction" },
          },
        ],
        priorState,
        postState,
        requireAcceptedBindings: false,
      }),
      buildPublicReplayFixture({
        txCbors: [native.txCbor],
        steps: [
          correctStep,
          {
            ...correctStep,
            step_index: 1n,
            event_key: depositEvent(0x44).eventKey,
            phase: "Deposit",
          },
        ],
        priorState,
        postState,
        requireAcceptedBindings: false,
      }),
    ]);
    const malformedResults = await Promise.all(
      malformedFixtures.map((fixture) =>
        evaluateWatcherBlockReplayV1(publicInput(fixture)),
      ),
    );
    expect(malformedResults.map(({ action }) => action)).toStrictEqual([
      "error",
      "error",
      "error",
    ]);
    expect(
      malformedResults.map(({ reasonCodes }) => reasonCodes),
    ).toStrictEqual([
      ["transition_trace_mismatch"],
      ["transition_trace_mismatch"],
      ["canonical_reconstruction_failed"],
    ]);
  });

  it("is restart/replay deterministic and never depends on caller array order", async () => {
    const firstInput = outRefFromByte(0x13);
    const secondInput = outRefFromByte(0x14);
    const first = makePhaseBCandidate({ spent: [firstInput], arrivalSeq: 0n });
    const second = makePhaseBCandidate({
      spent: [secondInput],
      arrivalSeq: 1n,
    });
    const priorState = entries([
      [firstInput, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)],
      [secondInput, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)],
    ]);
    const initial = await replay([first, second], priorState);
    const restarted = await replay([second, first], priorState);
    expect(restarted.resultDigest).toBe(initial.resultDigest);
    expect(restarted.acceptedTxIds).toStrictEqual(initial.acceptedTxIds);
    expect(restarted.intermediateRoots).toStrictEqual(
      initial.intermediateRoots,
    );
  });

  it("fails closed before replay for an uncommitted prior root and after replay for a bad post root", async () => {
    const input = outRefFromByte(0x15);
    const candidate = makePhaseBCandidate({ spent: [input] });
    const priorState = entries([
      [input, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)],
    ]);
    const prior = await watcherBlockReplayPriorStateV1(priorState);
    const priorMismatch = await evaluateWatcherBlockReplayCandidatesV1({
      candidates: [candidate],
      priorState,
      expectedPriorStateRoot: "ab".repeat(32),
      config,
    });
    expect(priorMismatch.stageMismatches).toMatchObject([
      { stage: "prior_state" },
    ]);

    const postMismatch = await replay([candidate], priorState, "cd".repeat(32));
    expect(prior.root).toBe(postMismatch.priorStateRoot);
    expect(postMismatch.action).toBe("reject");
    expect(postMismatch.stageMismatches).toMatchObject([
      { stage: "post_state" },
    ]);
  });
});

describe("W25 canonical rejection attribution and adversarial ordering", () => {
  it.each([
    [RejectCodes.InputNotFound, "resolveInputs", null, "spends"],
    [
      RejectCodes.InputNotFound,
      "resolveInputs",
      "reference input not found: x",
      "references",
    ],
    [RejectCodes.NativeScriptInvalid, "nativeScripts", null, "scripts"],
    [RejectCodes.ValueNotPreserved, "valueAndMint", null, "value"],
    [RejectCodes.DependencyCycle, "resolveInputs", null, "dependencies"],
    [RejectCodes.DependsOnRejectedTx, "resolveInputs", null, "dependencies"],
  ] as const)("attributes %s to %s", (code, consensusPhase, detail, stage) => {
    expect(
      watcherBlockReplayStageForRejectionV1({ code, consensusPhase, detail }),
    ).toBe(stage);
  });

  it("attributes canonical spend, reference, script, and value rejections without watcher predicates", async () => {
    const spend = outRefFromByte(0x21);
    const reference = outRefFromByte(0x22);
    const cases = [
      [
        makePhaseBCandidate({ spent: [spend] }),
        [],
        "spends",
        RejectCodes.InputNotFound,
      ],
      [
        makePhaseBCandidate({ spent: [spend], referenceInputs: [reference] }),
        [[spend, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]],
        "references",
        RejectCodes.InputNotFound,
      ],
      [
        makePhaseBCandidate({
          spent: [spend],
          scriptWitnesses: [nativeScriptWitness({ type: "after", slot: 1n })],
        }),
        [[spend, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]],
        "scripts",
        RejectCodes.InvalidFieldType,
      ],
      [
        makePhaseBCandidate({
          spent: [spend],
          outputLovelace: FUNDED_OUTPUT_LOVELACE_V1 + 1n,
        }),
        [[spend, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]],
        "value",
        RejectCodes.ValueNotPreserved,
      ],
    ] as const;
    for (const [candidate, state, stage, code] of cases) {
      const result = await replay([candidate], entries(state));
      expect(result.action).toBe("reject");
      expect(result.rejections).toMatchObject([{ code, stage }]);
    }
  });

  it("reports cycles and rejected descendants in canonical priority order", async () => {
    const input = outRefFromByte(0x31);
    const parent = makePhaseBCandidate({
      spent: [input],
      outputLovelace: FUNDED_OUTPUT_LOVELACE_V1 - 1n,
      arrivalSeq: 0n,
    });
    const child = makePhaseBCandidate({
      spent: [parent.graph.produced[0]![LedgerColumns.OUTREF]],
      outputLovelace: FUNDED_OUTPUT_LOVELACE_V1 - 1n,
      arrivalSeq: 1n,
    });
    const cascade = await replay(
      [parent, child],
      entries([[input, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]]),
    );
    expect(cascade.rejections.map((rejection) => rejection.code)).toStrictEqual(
      [RejectCodes.ValueNotPreserved, RejectCodes.DependsOnRejectedTx],
    );
    expect(cascade.selectedRejection?.code).toBe(
      RejectCodes.DependsOnRejectedTx,
    );
    expect(cascade.rejections[1]?.stage).toBe("dependencies");

    const first = makePhaseBCandidate({ spent: [outRefFromByte(0x32)] });
    const second = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [first.graph.produced[0]![LedgerColumns.OUTREF]],
    });
    const cyclic = {
      ...first,
      graph: {
        ...first.graph,
        spentOutRefHexes: [
          second.graph.produced[0]![LedgerColumns.OUTREF].toString("hex"),
        ],
      },
    };
    const cycle = await replay([cyclic, second], []);
    expect(cycle.rejections.map((rejection) => rejection.code)).toStrictEqual([
      RejectCodes.DependencyCycle,
      RejectCodes.DependencyCycle,
    ]);
    expect(
      cycle.rejections.every(({ stage }) => stage === "dependencies"),
    ).toBe(true);
  });

  it("executes a deterministic corpus for every evidenced Phase-B rejection code", async () => {
    const observed = new Set<string>();
    const collect = async (
      candidates: Parameters<typeof replay>[0],
      state: readonly WatcherBlockReplayPriorUtxoV1[],
    ) => {
      const result = await replay(candidates, state);
      for (const rejection of result.rejections) {
        observed.add(rejection.code);
      }
      return result;
    };

    const missing = outRefFromByte(0x81);
    await collect([makePhaseBCandidate({ spent: [missing] })], []);

    const invalidField = outRefFromByte(0x82);
    await collect(
      [
        makePhaseBCandidate({
          spent: [invalidField],
          scriptWitnesses: [nativeScriptWitness({ type: "after", slot: 1n })],
        }),
      ],
      entries([[invalidField, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]]),
    );

    const minAda = outRefFromByte(0x8c);
    await collect(
      [
        makePhaseBCandidate({
          spent: [minAda],
          outputs: [makeOutput(1n)],
        }),
      ],
      entries([[minAda, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]]),
    );

    const value = outRefFromByte(0x83);
    await collect(
      [
        makePhaseBCandidate({
          spent: [value],
          outputLovelace: FUNDED_OUTPUT_LOVELACE_V1 - 1n,
        }),
      ],
      entries([[value, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]]),
    );

    const witness = outRefFromByte(0x84);
    await collect(
      [makePhaseBCandidate({ spent: [witness], omitVkeyWitness: true })],
      entries([[witness, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]]),
    );

    const validity = outRefFromByte(0x85);
    await collect(
      [
        makePhaseBCandidate({
          spent: [validity],
          validityIntervalStart: 1n,
          validityIntervalEnd: 10n,
        }),
      ],
      entries([[validity, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]]),
    );

    const doubleSpend = outRefFromByte(0x86);
    const reference = outRefFromByte(0x87);
    await collect(
      [
        makePhaseBCandidate({ arrivalSeq: 0n, spent: [doubleSpend] }),
        makePhaseBCandidate({
          arrivalSeq: 1n,
          spent: [doubleSpend],
          referenceInputs: [reference],
        }),
      ],
      entries([
        [doubleSpend, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)],
        [reference, makeOutput(1n)],
      ]),
    );

    const cascadeInput = outRefFromByte(0x88);
    const cascadeParent = makePhaseBCandidate({
      spent: [cascadeInput],
      outputLovelace: FUNDED_OUTPUT_LOVELACE_V1 - 1n,
    });
    const cascadeChild = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [cascadeParent.graph.produced[0]![LedgerColumns.OUTREF]],
      outputLovelace: FUNDED_OUTPUT_LOVELACE_V1 - 1n,
    });
    await collect(
      [cascadeParent, cascadeChild],
      entries([[cascadeInput, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]]),
    );

    const cycleFirst = makePhaseBCandidate({
      spent: [outRefFromByte(0x89)],
    });
    const cycleSecond = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [cycleFirst.graph.produced[0]![LedgerColumns.OUTREF]],
    });
    await collect(
      [
        {
          ...cycleFirst,
          graph: {
            ...cycleFirst.graph,
            spentOutRefHexes: [
              cycleSecond.graph.produced[0]![LedgerColumns.OUTREF].toString(
                "hex",
              ),
            ],
          },
        },
        cycleSecond,
      ],
      [],
    );

    const plutusInput = outRefFromByte(0x8a);
    const plutus = plutusV3ScriptWitness(Buffer.from("010203", "hex"));
    await collect(
      [
        makePhaseBCandidate({
          spent: [plutusInput],
          outputs: [
            makeProtectedScriptOutput(
              hashScriptWitness(plutus),
              FUNDED_OUTPUT_LOVELACE_V1,
            ),
          ],
          scriptWitnesses: [plutus],
          redeemerTxWitsPreimageCbor: makeRedeemersCbor([
            { tag: MidgardRedeemerTag.Receiving, index: 0n },
          ]),
          scriptLanguages: ["PlutusV3"],
        }),
      ],
      entries([[plutusInput, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]]),
    );

    const nativeInput = outRefFromByte(0x8b);
    const native = nativeScriptWitness({ type: "after", slot: 1n });
    await collect(
      [
        makePhaseBCandidate({
          spent: [nativeInput],
          outputs: [
            makeProtectedScriptOutput(
              hashScriptWitness(native),
              FUNDED_OUTPUT_LOVELACE_V1,
            ),
          ],
          scriptWitnesses: [native],
          redeemerTxWitsPreimageCbor: makeRedeemersCbor([
            { tag: MidgardRedeemerTag.Receiving, index: 0n },
          ]),
        }),
      ],
      entries([[nativeInput, makeOutput(FUNDED_OUTPUT_LOVELACE_V1)]]),
    );

    expect([...observed].sort()).toStrictEqual(
      [...WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES_V1].sort(),
    );
  });

  it("binds every transition event to its canonical event-to-step identity", () => {
    const txId = "aa".repeat(32);
    const steps = watcherBlockReplayCommittedStepsV1({
      transitionTrace: [
        {
          key: 0n,
          value: {
            schema_version: 1n,
            step_index: 0n,
            event_key: { L2TransactionEventKey: { tx_id: txId } },
            phase: "L2Transaction",
            pre_utxos_root: "bb".repeat(32),
            post_utxos_root: "cc".repeat(32),
          },
        },
      ],
      eventToStep: [
        {
          key: { L2TransactionEventKey: { tx_id: txId } },
          value: { step_index: 0n, phase: "L2Transaction" },
        },
      ],
    });
    expect(steps).toStrictEqual([
      expect.objectContaining({
        stepIndex: 0,
        txId,
        eventToStepIndex: 0,
        eventToStepPhase: "L2Transaction",
      }),
    ]);
  });
});
