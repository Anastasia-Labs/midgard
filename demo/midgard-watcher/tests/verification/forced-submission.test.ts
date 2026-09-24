import {
  decodeMidgardForcedTxFullFromCanonicalCbor,
  deriveMidgardForcedTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalTransitionEffect } from "@al-ft/midgard-validation";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeOutput,
  outRefFromByte,
  outRefFromTxId,
} from "@al-ft/midgard-validation/tests/validation-fixtures";
import { CML, Data } from "@lucid-evolution/lucid";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { evaluateWatcherBlockReplay } from "../../src/verification/block-replay.js";
import {
  bindWatcherOriginEventClaim,
  type WatcherCommittedEventClaim,
} from "../../src/verification/event-claims.js";
import {
  buildPublicReplayFixture,
  committedStepsForEffects,
  localEventAuthority,
  localEventWindow,
  type LocalReplayEvent,
  publicEventFromLocal,
  publicInput,
  readLocalReplayEvent,
} from "../support/block-replay-public-fixture.js";
import { makeForcedTxFixture } from "../support/forced-submission-fixture.js";
import {
  createLocalReplayUserEventAuthorities,
  type LocalReplayUserEventAuthorities,
} from "../support/local-user-event-authority-fixture.js";
import { genuineUserEventForcedPayloadForCanonicalTx } from "../support/user-event-forced-order-fixture.js";

const key = CML.PrivateKey.from_normal_bytes(Buffer.alloc(32, 7));
const address = Buffer.from(
  CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(key.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes(),
);
const input = outRefFromByte(0x71);
const output = makeOutput(FUNDED_OUTPUT_LOVELACE, address);
const options = { spendInputs: [input], outputs: [output], privateKey: key };
const submitted = makeForcedTxFixture(options);
const substituted = makeForcedTxFixture({
  ...options,
  invalidVkeyWitness: true,
});
const entries = (outRef: Buffer) => [
  { outRef: outRef.toString("hex"), outputCbor: output.toString("hex") },
];
const prior = entries(input);
const next = outRefFromTxId(submitted.txId);
const effect = buildCanonicalTransitionEffect([
  { type: "delete", outRefCbor: input },
  { type: "insert", outRefCbor: next, outputCbor: output },
]);
const noOp = buildCanonicalTransitionEffect([]);
let authorities: LocalReplayUserEventAuthorities;
let forcedOrigin: LocalReplayEvent;

beforeAll(async () => {
  authorities = await createLocalReplayUserEventAuthorities({
    forcedOrders: [
      {
        key: "submitted",
        nonceByte: "e1",
        payload: genuineUserEventForcedPayloadForCanonicalTx(submitted.txCbor),
      },
    ],
  });
  const capability = authorities.forcedOrders.submitted;
  if (capability === undefined)
    throw new Error("local forced order was not published");
  forcedOrigin = await readLocalReplayEvent(capability);
}, 120_000);
afterAll(async () => {
  await authorities?.close();
});

const claim = (
  verdict: SDK.OperatorVerdict = "ForcedTxValid",
): WatcherCommittedEventClaim => {
  const event = forcedOrigin.event;
  const origin = Data.from(event.eventCborHex, SDK.TxOrderEvent);
  return {
    phase: "ForcedTransaction",
    eventIdCborHex: event.eventId,
    valueCborHex: Data.to(
      {
        tx_id: origin.tx.tx_id,
        submitted_source: origin.tx.submitted_source,
        verdict,
      },
      SDK.ForcedInclusionTxV1,
    ),
    canonicalNativeTxCborHex: submitted.txCbor.toString("hex"),
  };
};

const replay = async (verdict: SDK.OperatorVerdict, missingInput = false) => {
  const accepted = verdict === "ForcedTxValid";
  const transitionEffect = accepted && !missingInput ? effect : noOp;
  const priorState = missingInput ? [] : prior;
  const event = publicEventFromLocal(forcedOrigin, {
    forcedNative: submitted,
    forcedVerdict: verdict,
  });
  const fixture = await buildPublicReplayFixture({
    events: [event],
    steps: await committedStepsForEffects(priorState, [
      {
        eventKey: event.eventKey,
        phase: "ForcedTransaction",
        effect: transitionEffect,
      },
    ]),
    priorState,
    postState: missingInput ? [] : accepted ? entries(next) : prior,
    eventAuthorities: [
      localEventAuthority({
        event,
        local: forcedOrigin,
        effect: transitionEffect,
        forcedNative: submitted,
      }),
    ],
    eventWindow: localEventWindow(forcedOrigin),
    ruleBundle: authorities.ruleBundle,
  });
  return evaluateWatcherBlockReplay(publicInput(fixture));
};

describe("immutable forced submission through local watcher authority", () => {
  it("reconstructs the unchanged L1 submission and applies the exact independently accepted DA effect", async () => {
    const bound = bindWatcherOriginEventClaim(forcedOrigin.event, claim());
    expect(bound.phase).toBe("ForcedTransaction");
    expect(
      decodeMidgardForcedTxFullFromCanonicalCbor(submitted.txCbor),
    ).not.toHaveProperty("validity");
    const receipt = await replay("ForcedTxValid");
    expect(receipt).toMatchObject({
      action: "accept",
      reasonCodes: [],
      forcedValidationFacts: [
        {
          authenticatedOperatorValidity: "ForcedTxValid",
          canonicalOperatorValidity: "ForcedTxValid",
          canonicalEffectMutationCount: 2,
        },
      ],
    });
  });

  it("detects an accepted claim whose public prior ledger makes the transaction invalid", async () => {
    const receipt = await replay("ForcedTxValid", true);
    expect(receipt.action).not.toBe("accept");
    expect(receipt.forcedValidationFacts).toMatchObject([
      {
        authenticatedOperatorValidity: "ForcedTxValid",
        canonicalOperatorValidity: "InputNotFound",
        canonicalEffectMutationCount: 0,
      },
    ]);
  });

  it("detects a rejected claim when independent replay accepts the same submission", async () => {
    const receipt = await replay({
      ForcedTxInvalid: { reason: "ValueNotPreserved" },
    });
    expect(receipt.action).not.toBe("accept");
    expect(receipt.forcedValidationFacts).toMatchObject([
      {
        authenticatedOperatorValidity: "ValueNotPreserved",
        canonicalOperatorValidity: "ForcedTxValid",
        canonicalEffectMutationCount: 2,
      },
    ]);
  });

  it("preserves exact reason coordinates for adjudication even when their coarse watcher tags match", () => {
    const first = claim({
      ForcedTxInvalid: {
        reason: { AddressWitnessSignatureInvalid: { witness_index: 0n } },
      },
    });
    const foreign = claim({
      ForcedTxInvalid: {
        reason: { AddressWitnessSignatureInvalid: { witness_index: 1n } },
      },
    });
    const bind = (value: WatcherCommittedEventClaim) => {
      const bound = bindWatcherOriginEventClaim(forcedOrigin.event, value);
      if (bound.phase !== "ForcedTransaction")
        throw new Error("expected forced claim");
      return bound;
    };
    const a = bind(first);
    const b = bind(foreign);
    expect(a.operatorValidity).toBe(b.operatorValidity);
    expect(a.committed.verdict).not.toEqual(b.committed.verdict);
    expect(first.valueCborHex).not.toBe(foreign.valueCborHex);
    expect(a.committed.submitted_source).toEqual(b.committed.submitted_source);
    // Binding authenticates the claim, not its truth. The typed-reason proof
    // must still adjudicate witness 1; the coarse tag cannot rewrite it to 0.
    expect(b.committed.verdict).toEqual({
      ForcedTxInvalid: {
        reason: { AddressWitnessSignatureInvalid: { witness_index: 1n } },
      },
    });
  });

  it("refuses substituted witness material even when the body-derived ID is identical", () => {
    expect(substituted.txId).toEqual(submitted.txId);
    const original = claim();
    const source = deriveMidgardForcedTxProofSourceFromCanonicalCbor(
      substituted.txCbor,
    );
    const leaf = Data.from(original.valueCborHex, SDK.ForcedInclusionTxV1);
    const replacement = {
      ...original,
      canonicalNativeTxCborHex: substituted.txCbor.toString("hex"),
      valueCborHex: Data.to(
        {
          ...leaf,
          submitted_source: {
            compact_cbor: source.compactCbor.toString("hex"),
            witness_set_compact_cbor:
              source.witnessSetCompactCbor.toString("hex"),
            field_preimage_lengths_cbor:
              source.fieldPreimageLengthsCbor.toString("hex"),
          },
        },
        SDK.ForcedInclusionTxV1,
      ),
    };
    expect(() =>
      bindWatcherOriginEventClaim(forcedOrigin.event, replacement),
    ).toThrow("originating order");
  });

  it("refuses missing public material and another order's otherwise identical claim", () => {
    const original = claim();
    expect(() =>
      bindWatcherOriginEventClaim(forcedOrigin.event, {
        ...original,
        canonicalNativeTxCborHex: null,
      }),
    ).toThrow();
    const foreignId = Data.to(
      { transactionId: "aa".repeat(32), outputIndex: 1n },
      SDK.OutputReference,
    );
    expect(() =>
      bindWatcherOriginEventClaim(forcedOrigin.event, {
        ...original,
        eventIdCborHex: foreignId,
      }),
    ).toThrow("originating event");
  });
});
