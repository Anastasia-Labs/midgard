import {
  encodeMidgardNativeTxCanonical,
  materializeMidgardNativeTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { decodeMidgardForcedTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import {
  buildCanonicalTransitionEffect,
  LedgerColumns,
} from "@al-ft/midgard-validation";
import { MidgardRedeemerTag } from "@al-ft/midgard-validation/midgard-redeemers";
import {
  FUNDED_OUTPUT_LOVELACE,
  hashScriptWitness,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  makeProtectedScriptOutput,
  makeRedeemersCbor,
  nativeScriptWitness,
  outRefFromByte,
  outRefFromTxId,
  plutusV3ScriptWitness,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { CML } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  evaluateWatcherBlockReplay,
  makeWatcherBlockReplayReconstructedState,
} from "../../src/index.js";
import type { WatcherLocalUserEventAuthority } from "../../src/indexers/user-event-indexer.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  assertWatcherFullBlockReplayResult,
  evaluateWatcherBlockReplayCandidates,
  makeWatcherPhaseBConfig,
  WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_SCHEMA_VERSION,
  WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_PROTOCOL_MINUS_UNCLAIMED,
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT,
  watcherBlockReplayCommittedSteps,
  type WatcherBlockReplayEventAuthority,
  watcherBlockReplayPriorState,
  type WatcherBlockReplayPriorUtxo,
  watcherBlockReplayRejectionProjection,
  watcherBlockReplayStageForRejection,
} from "../../src/verification/block-replay.js";
import {
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES,
  WATCHER_PHASE_A_EXCLUDED_REJECT_CODES,
  WATCHER_PHASE_A_REACHABLE_REJECT_CODES,
} from "../../src/verification/phase-a-verifier.js";
import {
  buildPublicReplayFixture,
  CHAIN_POINT,
  type CommittedEffectGroup,
  committedStepsForEffects,
  dataHex,
  depositEffectFromLocal,
  entries,
  localEventAuthority,
  localEventWindow,
  type LocalReplayEvent,
  nativeEffect,
  publicEventFromLocal,
  type PublicFixtureEvent,
  publicInput,
  type PublicReplayFixture,
  readLocalReplayEvent,
  RULE_BUNDLE_COMMITMENT,
  withdrawalEffectFromLocal,
} from "../support/block-replay-public-fixture.js";
import { makeForcedTxFixture } from "../support/forced-submission-fixture.js";
import {
  createLocalReplayUserEventAuthorities,
  type LocalReplayUserEventAuthorities,
} from "../support/local-user-event-authority-fixture.js";
import {
  genuineUserEventForcedPayloadForCanonicalTx,
  userEventForcedOperatorVerdictForClassification,
} from "../support/user-event-forced-order-fixture.js";

const header = { blockSlot: 0n } as Parameters<
  typeof makeWatcherPhaseBConfig
>[0];
const config = makeWatcherPhaseBConfig(header);

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

const FLOW_OUTPUT = makeOutput(FUNDED_OUTPUT_LOVELACE, FIXED_ADDRESS);
const WITHDRAWAL_FLOW_INPUT = outRefFromByte(0x51);
const WITHDRAWAL_FLOW_NATIVE = makeNativeTx({
  spendInputs: [WITHDRAWAL_FLOW_INPUT],
  outputs: [FLOW_OUTPUT],
  privateKey: FIXED_KEY,
});
const FORCED_FLOW_INPUT = outRefFromByte(0x71);
const FORCED_FLOW_NATIVE = makeForcedTxFixture({
  spendInputs: [FORCED_FLOW_INPUT],
  outputs: [FLOW_OUTPUT],
  privateKey: FIXED_KEY,
});
const FORCED_INVALID_CASES = Object.freeze({
  InputNotFound: Object.freeze({
    input: outRefFromByte(0x72),
    native: makeForcedTxFixture({
      spendInputs: [outRefFromByte(0x72)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
    }),
    operatorValidity: "InputNotFound" as const,
  }),
  AddressWitnessSignatureInvalid: Object.freeze({
    input: outRefFromByte(0x73),
    native: makeForcedTxFixture({
      spendInputs: [outRefFromByte(0x73)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
      invalidVkeyWitness: true,
    }),
    operatorValidity: "AddressWitnessSignatureInvalid" as const,
  }),
  WitnessNativeScriptFalse: Object.freeze({
    input: outRefFromByte(0x74),
    native: makeForcedTxFixture({
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
    native: makeForcedTxFixture({
      spendInputs: [outRefFromByte(0x75)],
      outputs: [FLOW_OUTPUT],
      privateKey: FIXED_KEY,
      fee: 0n,
    }),
    operatorValidity: "FeeBelowMinimum" as const,
  }),
  ValueNotPreserved: Object.freeze({
    input: outRefFromByte(0x76),
    native: makeForcedTxFixture({
      spendInputs: [outRefFromByte(0x76)],
      outputs: [makeOutput(FUNDED_OUTPUT_LOVELACE - 1n, FIXED_ADDRESS)],
      privateKey: FIXED_KEY,
    }),
    operatorValidity: "ValueNotPreserved" as const,
  }),
});

const FORCED_VARIANT_NONCES = Object.freeze({
  InputNotFound: "d4",
  AddressWitnessSignatureInvalid: "d5",
  WitnessNativeScriptFalse: "d6",
  FeeBelowMinimum: "d7",
  ValueNotPreserved: "d8",
  Mismatch: "d9",
});

let localAuthorities: LocalReplayUserEventAuthorities;
let depositLocal: LocalReplayEvent;
let withdrawalLocal: LocalReplayEvent;
let forcedLocal: LocalReplayEvent;
let forcedVariantLocal: Readonly<
  Record<keyof typeof FORCED_VARIANT_NONCES, LocalReplayEvent>
>;

beforeAll(async () => {
  localAuthorities = await createLocalReplayUserEventAuthorities({
    deposit: { nonceByte: "d1", l2Address: FIXED_ADDRESS_DATA },
    withdrawals: [
      {
        key: "flow",
        nonceByte: "d2",
        l2OutRef: {
          transactionId: WITHDRAWAL_FLOW_NATIVE.txId.toString("hex"),
          outputIndex: 0n,
        },
        info: {
          body: {
            l2_outref: {
              transactionId: WITHDRAWAL_FLOW_NATIVE.txId.toString("hex"),
              outputIndex: 0n,
            },
            l2_owner: FIXED_KEY.to_public().hash().to_hex(),
            l2_value: new Map([["", new Map([["", FUNDED_OUTPUT_LOVELACE]])]]),
            l1_address: FIXED_ADDRESS_DATA,
            l1_datum: "NoDatum",
          },
          signature: ["aa", "bb"],
          validity: "WithdrawalIsValid",
        },
      },
    ],
    forcedOrders: [
      {
        key: "flow",
        nonceByte: "d3",
        payload: genuineUserEventForcedPayloadForCanonicalTx(
          FORCED_FLOW_NATIVE.txCbor,
        ),
      },
      ...(
        Object.entries(FORCED_VARIANT_NONCES) as [
          keyof typeof FORCED_VARIANT_NONCES,
          string,
        ][]
      ).map(([key, nonceByte]) => ({
        key,
        nonceByte,
        payload: genuineUserEventForcedPayloadForCanonicalTx(
          (key === "Mismatch"
            ? FORCED_INVALID_CASES.ValueNotPreserved
            : FORCED_INVALID_CASES[key]
          ).native.txCbor,
        ),
      })),
    ],
  });
  const read = async (
    authority: WatcherLocalUserEventAuthority | null | undefined,
  ): Promise<LocalReplayEvent> => {
    if (authority === null || authority === undefined)
      throw new Error("local replay authority was not published");
    return readLocalReplayEvent(authority);
  };
  depositLocal = await read(localAuthorities.deposit);
  withdrawalLocal = await read(localAuthorities.withdrawals.flow);
  forcedLocal = await read(localAuthorities.forcedOrders.flow);
  const variants: Partial<
    Record<keyof typeof FORCED_VARIANT_NONCES, LocalReplayEvent>
  > = {};
  for (const key of Object.keys(
    FORCED_VARIANT_NONCES,
  ) as (keyof typeof FORCED_VARIANT_NONCES)[])
    variants[key] = await read(localAuthorities.forcedOrders[key]);
  forcedVariantLocal = variants as Record<
    keyof typeof FORCED_VARIANT_NONCES,
    LocalReplayEvent
  >;
}, 120_000);

afterAll(async () => {
  await localAuthorities?.close();
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

const replay = async (
  candidates: Parameters<
    typeof evaluateWatcherBlockReplayCandidates
  >[0]["candidates"],
  priorState: readonly WatcherBlockReplayPriorUtxo[],
  expectedPostStateRoot?: string,
) => {
  const prior = await watcherBlockReplayPriorState(priorState);
  return await evaluateWatcherBlockReplayCandidates({
    candidates,
    priorState,
    expectedPriorStateRoot: prior.root,
    ...(expectedPostStateRoot === undefined ? {} : { expectedPostStateRoot }),
    config,
  });
};

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

describe("W25 published rejection-code partition", () => {
  it("is a disjoint total 13/27/10 partition of the canonical 50-code vocabulary", () => {
    expect(WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES).toStrictEqual(
      Object.values(RejectCodes),
    );
    expect(WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES).toHaveLength(50);
    const claimed = [
      ...WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
      ...WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES,
      ...WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES,
    ];
    expect(new Set(claimed).size).toBe(50);
    expect([...claimed].sort()).toStrictEqual(
      [...WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES].sort(),
    );
    expect(WATCHER_BLOCK_REPLAY_PROTOCOL_MINUS_UNCLAIMED).toHaveLength(40);
    expect(WATCHER_PHASE_A_CANONICAL_REJECT_CODES).toStrictEqual(
      WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES,
    );
    expect(
      WATCHER_PHASE_A_REACHABLE_REJECT_CODES.filter(
        (code) =>
          !new Set<string>(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES).has(
            code,
          ),
      ),
    ).toStrictEqual(WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES);
    expect(
      WATCHER_PHASE_A_EXCLUDED_REJECT_CODES.filter(
        (code) =>
          !new Set<string>(WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES).has(
            code,
          ),
      ),
    ).toStrictEqual(
      WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES.filter(
        (code) =>
          !new Set<string>(WATCHER_PHASE_A_REACHABLE_REJECT_CODES).has(code),
      ),
    );
  });

  it("keeps evidence and dominated claims inside Phase B's published set", () => {
    for (const code of WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES) {
      expect(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES).toContain(code);
    }
    for (const code of WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODES) {
      expect(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES).toContain(code);
      expect(WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES).not.toContain(code);
    }
  });
});

describe("W25 roots and deterministic replay", () => {
  it("replays a locally published Deposit before an L2 spend", async () => {
    const local = depositLocal;
    const event = publicEventFromLocal(local);
    const effect = depositEffectFromLocal(local);
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
    const authority = localEventAuthority({ event, local, effect });
    const fixture = await buildPublicReplayFixture({
      txCbors: [native.txCbor],
      events: [event],
      steps,
      priorState: [],
      postState: entries([[produced, inserted.outputCbor]]),
      eventAuthorities: [authority],
      eventWindow: localEventWindow(local),
      ruleBundle: localAuthorities.ruleBundle,
    });
    const result = await evaluateWatcherBlockReplay(publicInput(fixture));
    expect(result.action).toBe("accept");
    expect(result.reasonCodes).toStrictEqual([]);
    // Roots are recomputed independently from the canonical ledger contents
    // each mutation leaves behind, not taken from the replay under test.
    const emptyRoot = (await watcherBlockReplayPriorState([])).root;
    const depositedRoot = (
      await watcherBlockReplayPriorState(
        entries([[inserted.outRefCbor, inserted.outputCbor]]),
      )
    ).root;
    const producedRoot = (
      await watcherBlockReplayPriorState(
        entries([[produced, inserted.outputCbor]]),
      )
    ).root;
    expect(emptyRoot).toBe(
      "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
    );
    expect(result.eventRoots).toMatchObject([
      {
        stepIndex: 0,
        mutationCount: 1,
        preRoot: emptyRoot,
        postRoot: depositedRoot,
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
        preRoot: emptyRoot,
        postRoot: depositedRoot,
      },
      {
        operation: "delete",
        outRef: inserted.outRefCbor.toString("hex"),
        preRoot: depositedRoot,
        postRoot: emptyRoot,
      },
      {
        operation: "insert",
        outRef: produced.toString("hex"),
        preRoot: emptyRoot,
        postRoot: producedRoot,
      },
    ]);
    expect(result.authorityManifestDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.sourceManifestDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.effectManifestDigest).toMatch(/^[0-9a-f]{64}$/u);
    expect(result.verifiedRequires).toBe(
      WATCHER_BLOCK_REPLAY_VERIFIED_CONTRACT,
    );
    expect(result.downstreamPrerequisite).toStrictEqual({
      schemaVersion:
        WATCHER_BLOCK_REPLAY_DOWNSTREAM_PREREQUISITE_SCHEMA_VERSION,
      requiredVerifier: "W26",
      inputDigest: expect.stringMatching(/^[0-9a-f]{64}$/u),
      w29Eligibility: "requires_w26_accept",
    });

    // The caller's authority record is snapshotted synchronously: mutating it
    // while replay is in flight cannot change the admitted result.
    const mutableEventKey = structuredClone(authority.eventKey);
    const localReplayInFlight = evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      eventAuthorities: [{ ...authority, eventKey: mutableEventKey }],
    });
    Reflect.set(mutableEventKey, "DepositEventKey", {
      deposit_id: outputReference(0x34),
    });
    expect(await localReplayInFlight).toStrictEqual(result);

    const omittedAuthority = await evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      eventAuthorities: [],
    });
    expect(omittedAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["missing_event_authority"],
    });
    const duplicatedAuthority = await evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      eventAuthorities: [authority, authority],
    });
    expect(duplicatedAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["duplicate_event_authority"],
    });
    const substitutedAuthority = await evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      eventAuthorities: [
        { ...authority, eventKey: depositEvent(0x34).eventKey },
      ],
    });
    expect(substitutedAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["user_event_authority_identity_mismatch"],
    });
    if (authority.phase !== "Deposit") {
      throw new Error("deposit authority fixture has another phase");
    }
    const mutatedEffect = await evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      eventAuthorities: [
        {
          ...authority,
          transitionEffect: buildCanonicalTransitionEffect([]),
        },
      ],
    });
    expect(mutatedEffect).toMatchObject({
      action: "error",
      reasonCodes: ["transition_effect_semantics_mismatch"],
    });
  });

  it("replays the committed withdrawal claim after an L2 transaction without settlement authority", async () => {
    const local = withdrawalLocal;
    const event = publicEventFromLocal(local);
    const eventWindow = localEventWindow(local);
    const l2Effect = nativeEffect({
      spent: [WITHDRAWAL_FLOW_INPUT],
      native: WITHDRAWAL_FLOW_NATIVE,
      outputs: [FLOW_OUTPUT],
    });
    const effect = withdrawalEffectFromLocal(local, true);
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
    const authority = localEventAuthority({ event, local, effect });
    const fixture = await buildPublicReplayFixture({
      txCbors: [WITHDRAWAL_FLOW_NATIVE.txCbor],
      eventWindow,
      events: [event],
      steps,
      priorState,
      postState: [],
      eventAuthorities: [authority],
      ruleBundle: localAuthorities.ruleBundle,
    });
    const result = await evaluateWatcherBlockReplay(publicInput(fixture));
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

    const refundEvent = publicEventFromLocal(local, {
      withdrawalValidity: "NonExistentWithdrawalUtxo",
    });
    const refundEffect = withdrawalEffectFromLocal(local, false);
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
      eventWindow,
      events: [refundEvent],
      steps: refundSteps,
      priorState,
      postState: entries([[refundedProduced, FLOW_OUTPUT]]),
      eventAuthorities: [
        localEventAuthority({
          event: refundEvent,
          local,
          effect: refundEffect,
        }),
      ],
      ruleBundle: localAuthorities.ruleBundle,
    });
    const refund = await evaluateWatcherBlockReplay(publicInput(refundFixture));
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

    const originalPayload = Buffer.from(fixture.envelope);
    const mutablePrior = fixture.priorState.map((entry) => ({ ...entry }));
    const replaying = evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      payloadEnvelopeCbor: originalPayload,
      priorState: mutablePrior,
    });
    originalPayload.fill(0);
    mutablePrior[0]!.outputCbor = "00";
    mutablePrior.length = 0;
    const snapshotted = await replaying;
    expect(snapshotted.resultDigest).toBe(result.resultDigest);
    await expect(
      evaluateWatcherBlockReplay({
        ...publicInput(fixture),
        priorState: null as never,
      }),
    ).resolves.toMatchObject({
      action: "error",
      reasonCodes: ["malformed_prior_state"],
    });
    await expect(
      evaluateWatcherBlockReplay({
        ...publicInput(fixture),
        payloadEnvelopeCbor: null as never,
      }),
    ).resolves.toMatchObject({
      action: "error",
      reasonCodes: ["canonical_reconstruction_failed"],
    });
  });

  it("replays a ForcedTransaction before a later L2 spend without stale-state batching", async () => {
    const local = forcedLocal;
    expect("terminalClassification" in local.event).toBe(false);
    const event = publicEventFromLocal(local, {
      forcedNative: FORCED_FLOW_NATIVE,
    });
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
    const authority = localEventAuthority({
      event,
      local,
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
      eventWindow: localEventWindow(local),
      ruleBundle: localAuthorities.ruleBundle,
    });
    const result = await evaluateWatcherBlockReplay(publicInput(fixture));
    expect(result.action).toBe("accept");
    expect(result.reasonCodes).toStrictEqual([]);
    if (
      authority.phase !== "ForcedTransaction" ||
      !("ForcedTransactionEventKey" in authority.eventKey)
    ) {
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

    const callerNativeBytes = Buffer.from(authority.canonicalNativeTxCbor);
    const forcedReplayInFlight = evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      eventAuthorities: [
        { ...authority, canonicalNativeTxCbor: callerNativeBytes },
      ],
    });
    callerNativeBytes.fill(0);
    expect(await forcedReplayInFlight).toStrictEqual(result);

    const mutatedNativeBytes = await evaluateWatcherBlockReplay({
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

    const noOpEffect = buildCanonicalTransitionEffect([]);
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
          authority: WatcherBlockReplayEventAuthority;
          event: PublicFixtureEvent;
          result: Awaited<ReturnType<typeof evaluateWatcherBlockReplay>>;
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
        decodeMidgardForcedTxFullFromCanonicalCbor(invalidCase.native.txCbor),
      ).not.toHaveProperty("validity");
      const invalidLocal = forcedVariantLocal[category];
      const invalidEvent = publicEventFromLocal(invalidLocal, {
        forcedNative: invalidCase.native,
        forcedVerdict: userEventForcedOperatorVerdictForClassification(
          invalidCase.operatorValidity,
        ),
      });
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
      const invalidAuthority = localEventAuthority({
        event: invalidEvent,
        local: invalidLocal,
        effect: noOpEffect,
        forcedNative: invalidCase.native,
      });
      const invalidFixture = await buildPublicReplayFixture({
        events: [invalidEvent],
        steps: invalidSteps,
        priorState: invalidPriorState,
        postState: invalidPriorState,
        eventAuthorities: [invalidAuthority],
        eventWindow: localEventWindow(invalidLocal),
        ruleBundle: localAuthorities.ruleBundle,
        ...(category === "FeeBelowMinimum" ? { minFeeB: 1n } : {}),
      });
      const invalidResult = await evaluateWatcherBlockReplay(
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
    const restarted = await evaluateWatcherBlockReplay(
      publicInput(restartEvidence.fixture),
    );
    expect(restarted).toStrictEqual(restartEvidence.result);
    expect(restarted.resultDigest).toBe(restartEvidence.result.resultDigest);

    const omittedAuthority = await evaluateWatcherBlockReplay({
      ...publicInput(restartEvidence.fixture),
      eventAuthorities: [],
    });
    expect(omittedAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["missing_event_authority"],
    });
    const duplicateAuthority = await evaluateWatcherBlockReplay({
      ...publicInput(restartEvidence.fixture),
      eventAuthorities: [restartEvidence.authority, restartEvidence.authority],
    });
    expect(duplicateAuthority).toMatchObject({
      action: "error",
      reasonCodes: ["duplicate_event_authority"],
    });

    const mismatchLocal = forcedVariantLocal.Mismatch;
    const mismatchEvent = publicEventFromLocal(mismatchLocal, {
      forcedNative: FORCED_INVALID_CASES.ValueNotPreserved.native,
      forcedVerdict: "ForcedTxValid",
    });
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
    const mismatchAuthority = localEventAuthority({
      event: mismatchEvent,
      local: mismatchLocal,
      effect: noOpEffect,
      forcedNative: FORCED_INVALID_CASES.ValueNotPreserved.native,
    });
    const mismatchFixture = await buildPublicReplayFixture({
      events: [mismatchEvent],
      steps: mismatchSteps,
      priorState: mismatchPriorState,
      postState: mismatchPriorState,
      eventAuthorities: [mismatchAuthority],
      eventWindow: localEventWindow(mismatchLocal),
      ruleBundle: localAuthorities.ruleBundle,
    });
    const mismatchResult = await evaluateWatcherBlockReplay(
      publicInput(mismatchFixture),
    );
    expect(mismatchResult).toMatchObject({
      action: "reject",
      reasonCodes: ["transition_effect_semantics_mismatch"],
      acceptedCount: 0,
      acceptedTxIds: [],
      intermediateRoots: [],
      eventRoots: [{ stepIndex: 0, mutationCount: 0 }],
      forcedValidationFacts: [
        {
          stepIndex: 0,
          authenticatedOperatorValidity: "ForcedTxValid",
          canonicalOperatorValidity: "ValueNotPreserved",
          phaseAStatus: "accepted",
          phaseBStatus: "rejected",
          phaseBRejectCode: RejectCodes.ValueNotPreserved,
          canonicalEffectDigest: noOpEffect.digest,
          canonicalEffectMutationCount: 0,
        },
      ],
    });
    expect(mismatchResult.postStateRoot).toBe(mismatchResult.priorStateRoot);
    expect(mismatchResult.eventRoots[0]?.preRoot).toBe(
      mismatchResult.priorStateRoot,
    );
    expect(mismatchResult.eventRoots[0]?.postRoot).toBe(
      mismatchResult.priorStateRoot,
    );

    const forcedRejected = FORCED_INVALID_CASES.ValueNotPreserved.native;
    const rejectedNormalNative = {
      ...forcedRejected,
      txCbor: encodeMidgardNativeTxCanonical(
        materializeMidgardNativeTxFromCanonical({
          ...forcedRejected.tx,
          validity: "TxIsValid",
        }),
      ),
    };
    const rejectedNormalId = rejectedNormalNative.txId.toString("hex");
    const rejectedNormalFixture = await buildPublicReplayFixture({
      txCbors: [rejectedNormalNative.txCbor],
      steps: await committedStepsForEffects(mismatchPriorState, [
        {
          eventKey: { L2TransactionEventKey: { tx_id: rejectedNormalId } },
          phase: "L2Transaction",
          effect: noOpEffect,
        },
      ]),
      priorState: mismatchPriorState,
      postState: mismatchPriorState,
    });
    const rejectedNormalResult = await evaluateWatcherBlockReplay(
      publicInput(rejectedNormalFixture),
    );
    expect(rejectedNormalResult).toMatchObject({
      action: "reject",
      reasonCodes: ["phase_b_rejection"],
      acceptedCount: 0,
      acceptedTxIds: [],
      intermediateRoots: [],
      rejections: [
        {
          index: 0,
          txId: rejectedNormalId,
          code: RejectCodes.ValueNotPreserved,
        },
      ],
      transactionRoots: [
        {
          txIndex: 0,
          txId: rejectedNormalId,
          mutationCount: 0,
          committedStepIndex: 0,
          preRoot: rejectedNormalResult.priorStateRoot,
          postRoot: rejectedNormalResult.priorStateRoot,
          committedPreRoot: rejectedNormalResult.priorStateRoot,
          committedPostRoot: rejectedNormalResult.priorStateRoot,
        },
      ],
    });
    expect(rejectedNormalResult.postStateRoot).toBe(
      rejectedNormalResult.priorStateRoot,
    );

    const tamperedFactResult = {
      ...restartEvidence.result,
      forcedValidationFacts: restartEvidence.result.forcedValidationFacts.map(
        (fact) => ({ ...fact, canonicalEffectMutationCount: 1 }),
      ),
    };
    const { resultDigest: _tamperedDigest, ...tamperedFactMaterial } =
      tamperedFactResult;
    expect(watcherSha256CanonicalJson(tamperedFactMaterial)).not.toBe(
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
      eventWindow: localEventWindow(forcedLocal),
      ruleBundle: localAuthorities.ruleBundle,
    });
    const orderedResult = await evaluateWatcherBlockReplay(
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
      eventWindow: localEventWindow(forcedLocal),
      ruleBundle: localAuthorities.ruleBundle,
    });
    const reversedResult = await evaluateWatcherBlockReplay(
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
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE, FIXED_ADDRESS);
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

    const result = await evaluateWatcherBlockReplay(publicInput(fixture));
    expect(() => assertWatcherFullBlockReplayResult(result)).not.toThrow();
    expect(() => assertWatcherFullBlockReplayResult({ ...result })).toThrow(
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
      watcherSha256CanonicalJson(
        Object.fromEntries(
          Object.entries(result).filter(([key]) => key !== "resultDigest"),
        ),
      ),
    );

    const durable = makeWatcherBlockReplayReconstructedState({
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

    const unsupportedReconstruction = await evaluateWatcherBlockReplay({
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
    const badReconstructionDigest = await evaluateWatcherBlockReplay({
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
    const unsupportedPhaseA = await evaluateWatcherBlockReplay({
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
    const badPhaseADigest = await evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      phaseA: { ...fixture.phaseA, resultDigest: h32(0xef) },
    });
    expect(badPhaseADigest).toMatchObject({
      action: "error",
      reasonCodes: ["phase_a_digest_mismatch"],
    });
    const corruptedEnvelope = Buffer.from(fixture.envelope);
    corruptedEnvelope[corruptedEnvelope.length - 1] ^= 1;
    const corruptedBytes = await evaluateWatcherBlockReplay({
      ...publicInput(fixture),
      payloadEnvelopeCbor: corruptedEnvelope,
    });
    expect(corruptedBytes).toMatchObject({
      action: "error",
      reasonCodes: ["canonical_reconstruction_failed"],
    });

    let unknownCode: unknown;
    try {
      watcherBlockReplayRejectionProjection({
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
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE, FIXED_ADDRESS);
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
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE, FIXED_ADDRESS);
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
    const accepted = await evaluateWatcherBlockReplay(publicInput(fixture));
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
        makeWatcherBlockReplayReconstructedState({
          result,
          chainPointId: `${CHAIN_POINT.slot.toString()}:${CHAIN_POINT.blockHash}`,
          inputIds: [h32(0x5a)],
        }),
      ).toThrow("result_not_accepted");
    }
  });

  it("fails closed on trace substitution, omission, duplication/reorder, trailing steps, wrong roots, and event_to_step drift", async () => {
    const spent = outRefFromByte(0x11);
    const output = makeOutput(FUNDED_OUTPUT_LOVELACE, FIXED_ADDRESS);
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
      await evaluateWatcherBlockReplay(publicInput(wrongRoots)),
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
      await evaluateWatcherBlockReplay(publicInput(omitted)),
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
        evaluateWatcherBlockReplay(publicInput(fixture)),
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
      [firstInput, makeOutput(FUNDED_OUTPUT_LOVELACE)],
      [secondInput, makeOutput(FUNDED_OUTPUT_LOVELACE)],
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
    const priorState = entries([[input, makeOutput(FUNDED_OUTPUT_LOVELACE)]]);
    const prior = await watcherBlockReplayPriorState(priorState);
    const priorMismatch = await evaluateWatcherBlockReplayCandidates({
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
      watcherBlockReplayStageForRejection({ code, consensusPhase, detail }),
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
        [[spend, makeOutput(FUNDED_OUTPUT_LOVELACE)]],
        "references",
        RejectCodes.InputNotFound,
      ],
      [
        makePhaseBCandidate({
          spent: [spend],
          scriptWitnesses: [nativeScriptWitness({ type: "after", slot: 1n })],
        }),
        [[spend, makeOutput(FUNDED_OUTPUT_LOVELACE)]],
        "scripts",
        RejectCodes.InvalidFieldType,
      ],
      [
        makePhaseBCandidate({
          spent: [spend],
          outputLovelace: FUNDED_OUTPUT_LOVELACE + 1n,
        }),
        [[spend, makeOutput(FUNDED_OUTPUT_LOVELACE)]],
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
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
      arrivalSeq: 0n,
    });
    const child = makePhaseBCandidate({
      spent: [parent.graph.produced[0]![LedgerColumns.OUTREF]],
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
      arrivalSeq: 1n,
    });
    const cascade = await replay(
      [parent, child],
      entries([[input, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
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
      state: readonly WatcherBlockReplayPriorUtxo[],
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
      entries([[invalidField, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const minAda = outRefFromByte(0x8c);
    await collect(
      [
        makePhaseBCandidate({
          spent: [minAda],
          outputs: [makeOutput(1n)],
        }),
      ],
      entries([[minAda, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const value = outRefFromByte(0x83);
    await collect(
      [
        makePhaseBCandidate({
          spent: [value],
          outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
        }),
      ],
      entries([[value, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    const witness = outRefFromByte(0x84);
    await collect(
      [makePhaseBCandidate({ spent: [witness], omitVkeyWitness: true })],
      entries([[witness, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
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
      entries([[validity, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
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
        [doubleSpend, makeOutput(FUNDED_OUTPUT_LOVELACE)],
        [reference, makeOutput(1n)],
      ]),
    );

    const cascadeInput = outRefFromByte(0x88);
    const cascadeParent = makePhaseBCandidate({
      spent: [cascadeInput],
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
    });
    const cascadeChild = makePhaseBCandidate({
      arrivalSeq: 1n,
      spent: [cascadeParent.graph.produced[0]![LedgerColumns.OUTREF]],
      outputLovelace: FUNDED_OUTPUT_LOVELACE - 1n,
    });
    await collect(
      [cascadeParent, cascadeChild],
      entries([[cascadeInput, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
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
              FUNDED_OUTPUT_LOVELACE,
            ),
          ],
          scriptWitnesses: [plutus],
          redeemerTxWitsPreimageCbor: makeRedeemersCbor([
            { tag: MidgardRedeemerTag.Receiving, index: 0n },
          ]),
          scriptLanguages: ["PlutusV3"],
        }),
      ],
      entries([[plutusInput, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
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
              FUNDED_OUTPUT_LOVELACE,
            ),
          ],
          scriptWitnesses: [native],
          redeemerTxWitsPreimageCbor: makeRedeemersCbor([
            { tag: MidgardRedeemerTag.Receiving, index: 0n },
          ]),
        }),
      ],
      entries([[nativeInput, makeOutput(FUNDED_OUTPUT_LOVELACE)]]),
    );

    expect([...observed].sort()).toStrictEqual(
      [...WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES].sort(),
    );
  });

  it("binds every transition event to its canonical event-to-step identity", () => {
    const txId = "aa".repeat(32);
    const steps = watcherBlockReplayCommittedSteps({
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
