import {
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  ledgerEd25519SignatureReady,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
} from "@al-ft/midgard-core";
import {
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardForcedTxCanonical,
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  materializeMidgardForcedTxFromCanonical,
  type MidgardForcedTxCanonical,
} from "@al-ft/midgard-core/codec";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  outRefFromByte,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { beforeAll, expect, it } from "vitest";

import { replayForcedTransitionEffect } from "../../src/verification/block-replay.replay-forced-transition-effect.js";
import type { ValidatedEventAuthority } from "../../src/verification/block-replay.watcher-block-replay-prior-state.js";
import {
  WATCHER_FORCED_TX_VALID,
  watcherForcedOperatorVerdict,
} from "../../src/verification/user-event.js";
import { makeForcedTxFixture } from "../support/forced-submission-fixture.js";

// The watcher waits for the verifier at startup; this replay runs without it.
beforeAll(() => ledgerEd25519SignatureReady());

const transaction = (
  outputs: readonly Buffer[],
  scriptWitnesses: readonly Buffer[],
): Buffer => {
  const tx: MidgardForcedTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
    body: {
      // The mint admits only a transaction that spends something.
      spendInputsPreimageCbor: encodeMidgardFieldPreimage([
        encodeMidgardSpendInputItem({
          txId: Buffer.alloc(32, 0x11),
          outputIndex: 0,
        }),
      ]),
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: encodeMidgardFieldPreimage(outputs),
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: encodeMidgardFieldPreimage(scriptWitnesses),
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  };
  return encodeMidgardForcedTxCanonical(
    materializeMidgardForcedTxFromCanonical(tx),
  );
};

/** The entries of an address-and-value output map, without its header. */
const plainOutputBody = encodeMidgardTxOutput({
  address: Buffer.concat([Buffer.from([0x60]), Buffer.alloc(28, 0x11)]),
  value: { lovelace: 1_000_000n, assets: new Map() },
}).subarray(1);

type Stage = "phaseA" | "phaseB";

// Transactions the tx-order mint admits whose items do not decode, with the
// rejection the node commits for them and the phase that commits it. Outputs
// are read after input resolution, so a transaction spending an input absent
// from the empty pre-state is refused for that input first.
const SHAPES: readonly (readonly [
  string,
  SDK.RejectionReason,
  Stage,
  Buffer,
])[] = [
  [
    "an output with a non-canonical inline datum",
    { InputNotFound: { source_kind: 0n, input_index: 0n } },
    "phaseB",
    transaction(
      [
        Buffer.concat([
          Buffer.from([0xa3]),
          plainOutputBody,
          Buffer.from([0x02]),
          encodeCbor(Buffer.from("1800", "hex")),
        ]),
      ],
      [],
    ),
  ],
  [
    "an output whose address does not decode",
    { InputNotFound: { source_kind: 0n, input_index: 0n } },
    "phaseB",
    transaction(
      [
        Buffer.concat([
          Buffer.from([0xa2, 0x00]),
          encodeCbor(Buffer.from([0x99])),
          // Key 1 and the value, after key 0 and its 29-byte address.
          plainOutputBody.subarray(1 + 2 + 29),
        ]),
      ],
      [],
    ),
  ],
  [
    "a native script whose body does not decode",
    { WitnessNativeScriptMalformed: { script_index: 0n } },
    "phaseA",
    transaction([], [Buffer.from("820043820700", "hex")]),
  ],
];

const replay = (
  txCbor: Buffer,
  committed: string,
  state: ReadonlyMap<string, Buffer> = new Map(),
) =>
  replayForcedTransitionEffect({
    authority: {
      phase: "ForcedTransaction",
      canonicalNativeTxCbor: txCbor,
      committedForcedValidity: committed,
      eventKeyFingerprint: "undecodable-item",
    } as ValidatedEventAuthority,
    state,
    phaseAConfig: {
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      concurrency: 1,
      strictnessProfile: "phase1_midgard",
    },
    phaseBConfig: {
      l1SlotConfig: NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod,
      nowCardanoSlotNo: 100n,
      bucketConcurrency: 1,
      enforceScriptBudget: true,
    },
    step: {
      stepIndex: 0,
      phase: "ForcedTransaction",
      txId: null,
      eventKeyFingerprint: "undecodable-item",
      preRoot: "00".repeat(32),
      postRoot: "00".repeat(32),
      eventToStepIndex: 0,
      eventToStepPhase: "ForcedTransaction",
    },
  });

/** `[3, h'00']`: a PlutusV3 item whose script bytes are not a byte string. */
const UNDECODABLE_PLUTUS = Buffer.from("82034100", "hex");

/** The input the shapes spend, held by the fixture key, and its output. */
const SPENT = outRefFromByte(0x11);
const FUNDED_STATE: ReadonlyMap<string, Buffer> = new Map([
  [SPENT.toString("hex"), makeOutput(FUNDED_OUTPUT_LOVELACE)],
]);
/** A funded output carrying `[3, h'00']` as its reference script. */
const fundedWithUndecodableRef = (() => {
  const funded = makeOutput(FUNDED_OUTPUT_LOVELACE);
  return Buffer.concat([
    Buffer.from([funded[0]! + 1]),
    funded.subarray(1),
    Buffer.from([0x03]),
    UNDECODABLE_PLUTUS,
  ]);
})();

// Forced transactions carrying an undecodable Plutus item, of each item kind:
// a field-6 witness (kind 0), refused at the witness-script phase, and an
// output reference script (kind 1), refused at the script-source phase after
// its input resolves. The tx-order mint admits native items only, so these
// reach replay only through a forced event built outside the mint; replay
// must still reach the committed verdict.
const PLUTUS_SHAPES: readonly (readonly [
  string,
  SDK.RejectionReason,
  Stage,
  Buffer,
])[] = [
  [
    "a Plutus witness whose program does not decode",
    {
      PlutusScriptUndecodable: { item_kind: 0n, item_index: 0n, class: 1n },
    },
    "phaseA",
    transaction([], [UNDECODABLE_PLUTUS]),
  ],
  [
    "a Plutus reference script whose program does not decode",
    {
      PlutusScriptUndecodable: { item_kind: 1n, item_index: 0n, class: 1n },
    },
    "phaseB",
    // Signed by the key holding its input: outputs are read after the
    // spend-input signer check.
    makeForcedTxFixture({
      spendInputs: [SPENT],
      outputs: [fundedWithUndecodableRef],
    }).txCbor,
  ],
];

const expectReplayReachesVerdict = async (
  reason: SDK.RejectionReason,
  stage: Stage,
  txCbor: Buffer,
  state?: ReadonlyMap<string, Buffer>,
) => {
  const verdict = watcherForcedOperatorVerdict({
    ForcedTxInvalid: { reason },
  })!;
  expect(verdict).not.toBeNull();
  const accepted = await replay(txCbor, verdict, state);
  expect(accepted?.fact).toMatchObject({
    phaseAStatus: stage === "phaseA" ? "rejected" : "accepted",
    phaseBStatus: stage === "phaseA" ? "not_run" : "rejected",
    canonicalOperatorValidity: verdict,
    authenticatedOperatorValidity: verdict,
  });
  expect(accepted?.effect).toMatchObject({ operations: [] });
  // A block that claims the transaction valid disagrees with the replay.
  const claimedValid = await replay(txCbor, WATCHER_FORCED_TX_VALID, state);
  expect(claimedValid?.fact.canonicalOperatorValidity).toBe(verdict);
  expect(claimedValid?.fact.authenticatedOperatorValidity).toBe(
    WATCHER_FORCED_TX_VALID,
  );
};

it.each(PLUTUS_SHAPES)(
  "accepts the node's rejection of a forced transaction with %s",
  async (_name, reason, stage, txCbor) =>
    expectReplayReachesVerdict(reason, stage, txCbor, FUNDED_STATE),
);

it.each(SHAPES)(
  "accepts the node's rejection of a forced transaction with %s",
  async (_name, reason, stage, txCbor) =>
    expectReplayReachesVerdict(reason, stage, txCbor),
);
