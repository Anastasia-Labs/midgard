import * as SDK from "@al-ft/midgard-sdk";
import { h28, h32 } from "@al-ft/midgard-test-support/hex";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  nativeScriptWitness,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML } from "@lucid-evolution/lucid";

import {
  evaluateWatcherBlockReplayCandidates,
  makeWatcherPhaseBConfig,
  watcherBlockReplayPriorState,
  type WatcherBlockReplayPriorUtxo,
} from "../../src/verification/block-replay.js";
import {
  dataHex,
  type PublicFixtureEvent,
} from "../support/block-replay-public-fixture.js";
import { makeForcedTxFixture } from "../support/forced-submission-fixture.js";
import {
  fixtureDepositEvent,
  fixtureForcedOrderEvent,
  type FixtureUserEvent,
  fixtureWithdrawalEvent,
} from "../support/user-event-authority-fixture.js";
import { genuineUserEventForcedPayloadForCanonicalTx } from "../support/user-event-forced-order-fixture.js";

const header = { blockSlot: 0n } as Parameters<
  typeof makeWatcherPhaseBConfig
>[0];

export const config = makeWatcherPhaseBConfig(header);

export const FIXED_KEY = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));

export const FIXED_ADDRESS = Buffer.from(
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

export const FLOW_OUTPUT = makeOutput(FUNDED_OUTPUT_LOVELACE, FIXED_ADDRESS);

export const WITHDRAWAL_FLOW_INPUT = outRefFromByte(0x51);

export const WITHDRAWAL_FLOW_NATIVE = makeNativeTx({
  spendInputs: [WITHDRAWAL_FLOW_INPUT],
  outputs: [FLOW_OUTPUT],
  privateKey: FIXED_KEY,
});

export const FORCED_FLOW_INPUT = outRefFromByte(0x71);

export const FORCED_FLOW_NATIVE = makeForcedTxFixture({
  spendInputs: [FORCED_FLOW_INPUT],
  outputs: [FLOW_OUTPUT],
  privateKey: FIXED_KEY,
});

export const FORCED_INVALID_CASES = Object.freeze({
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

export const depositOrigin: FixtureUserEvent = fixtureDepositEvent({
  nonceByte: "d1",
  l2Address: FIXED_ADDRESS_DATA,
  originalAssets: { lovelace: 3_000_000n },
});

export const withdrawalOrigin: FixtureUserEvent = fixtureWithdrawalEvent({
  nonceByte: "d2",
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
});

export const forcedOrigin: FixtureUserEvent = fixtureForcedOrderEvent({
  nonceByte: "d3",
  payload: genuineUserEventForcedPayloadForCanonicalTx(
    FORCED_FLOW_NATIVE.txCbor,
  ),
});

export const forcedVariantOrigins: Readonly<
  Record<keyof typeof FORCED_VARIANT_NONCES, FixtureUserEvent>
> = Object.freeze(
  Object.fromEntries(
    (
      Object.entries(FORCED_VARIANT_NONCES) as [
        keyof typeof FORCED_VARIANT_NONCES,
        string,
      ][]
    ).map(([key, nonceByte]) => [
      key,
      fixtureForcedOrderEvent({
        nonceByte,
        payload: genuineUserEventForcedPayloadForCanonicalTx(
          (key === "Mismatch"
            ? FORCED_INVALID_CASES.ValueNotPreserved
            : FORCED_INVALID_CASES[key]
          ).native.txCbor,
        ),
      }),
    ]),
  ) as Record<keyof typeof FORCED_VARIANT_NONCES, FixtureUserEvent>,
);

// Every `outRef` below is §5.3's fixed-index field-0/1 item — the ledger MPF
// trie key — so each is exactly 38 bytes and its output index is the
// non-minimal `19 0000`, never the minimal `00` CML would emit. The two tx ids
// and all eight roots are downstream of that key width: the spend-input items a
// fixture transaction commits determine its id, and the trie keys determine
// every root, so re-pinning the out-refs necessarily re-pins the rest.
export const FIXED_TWO_TX_ROOTS = [
  {
    sequence: 0,
    txIndex: 0,
    txId: "5aa36d0b6f5cc700f18f54386542bd937ba7b96625eff82e81c1f451686e94dd",
    stepIndex: null,
    phase: null,
    operation: "delete",
    outRef:
      "8258201111111111111111111111111111111111111111111111111111111111111111190000",
    preRoot: "93aa873a2fd64d035256c5525f1e67734c39bb09b610b817d46277c5f68c801f",
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
      "7f1e6e9609d587e66a1846736464fe6050339404a74bd5996fd1dbf862a5816c",
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
    preRoot: "7f1e6e9609d587e66a1846736464fe6050339404a74bd5996fd1dbf862a5816c",
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
      "d7c918403e67154cdbd5065dd58eb9f911ae13dffb2e0861e2d8b98bd634437e",
  },
] as const;

export const replay = async (
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

export const outputReference = (byte: number): SDK.OutputReference => ({
  transactionId: h32(byte),
  outputIndex: 0n,
});

const addressData = (byte: number): SDK.AddressData => ({
  paymentCredential: { PublicKeyCredential: [h28(byte)] },
  stakeCredential: null,
});

export const depositEvent = (byte: number): PublicFixtureEvent => {
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
