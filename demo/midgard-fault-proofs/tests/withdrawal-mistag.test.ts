import { computeHash32 } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { buildCanonicalMidgardLedgerOutputMaterial } from "@al-ft/midgard-validation";
import { CML, Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  buildCountedRoot,
  keyValuePhasProof,
  keyValuePhasRootWithCount,
} from "../src/transition-trace/phas.js";
import { prepareWithdrawalMistag } from "../src/withdrawal-mistag/prepare-withdrawal-mistag.js";
import {
  withdrawalMistagStates,
  withdrawalMistagStepPayloadCbor,
} from "../src/withdrawal-mistag/submit-withdrawal-mistag-steps.js";

const makeEvidence = async ({
  lovelace,
  validity,
  outputAssets = new Map(),
  l2Value,
  l1DatumCbor,
}: {
  readonly lovelace: bigint;
  readonly validity: SDK.WithdrawalValidity;
  /** The ledger output's non-ADA assets. */
  readonly outputAssets?: Map<string, Map<string, bigint>>;
  /** The requested value; by default exactly the ledger output's value. */
  readonly l2Value?: SDK.Value;
  /** Raw `l1_datum` CBOR spliced into the committed bytes as written. */
  readonly l1DatumCbor?: string;
}) => {
  const privateKey = CML.PrivateKey.generate_ed25519();
  const publicKey = privateKey.to_public();
  const owner = publicKey.hash().to_hex();
  const withdrawalId: SDK.OutputReference = {
    transactionId: "01".repeat(32),
    outputIndex: 0n,
  };
  const body: SDK.WithdrawalBody = {
    l2_outref: withdrawalId,
    l2_owner: owner,
    l2_value:
      l2Value ?? new Map([["", new Map([["", lovelace]])], ...outputAssets]),
    l1_address: {
      paymentCredential: { PublicKeyCredential: [owner] },
      stakeCredential: null,
    },
    l1_datum: "NoDatum",
  };
  const bodyCbor =
    l1DatumCbor === undefined
      ? SDK.withdrawalBodyBytes(body)
      : replacePlutusConstrFieldCbor(
          SDK.withdrawalBodyBytes(body),
          [4],
          l1DatumCbor,
        );
  const message = computeHash32(
    Buffer.concat([
      Buffer.from("MidgardWithdrawalV1", "utf8"),
      Buffer.from(bodyCbor, "hex"),
    ]),
  );
  const typedInfo: SDK.WithdrawalInfo = {
    body,
    signature: [
      Buffer.from(publicKey.to_raw_bytes()).toString("hex"),
      privateKey.sign(message).to_hex(),
    ],
    validity,
  };
  const infoCbor =
    l1DatumCbor === undefined
      ? SDK.committedWithdrawalValueBytes(typedInfo)
      : replacePlutusConstrFieldCbor(
          SDK.committedWithdrawalValueBytes(typedInfo),
          [0, 4],
          l1DatumCbor,
        );
  const info = Data.from(infoCbor, SDK.WithdrawalInfo);
  const outputCbor = encodeMidgardTxOutput({
    address: Buffer.concat([Buffer.from([0x60]), Buffer.from(owner, "hex")]),
    value: { lovelace, assets: outputAssets },
  });
  const material = buildCanonicalMidgardLedgerOutputMaterial({
    outputIndex: 0,
    outputCbor,
  });
  const ledgerKey = encodeMidgardSpendInputItem({
    txId: Buffer.from(withdrawalId.transactionId, "hex"),
    outputIndex: 0,
  });
  const ledger = await keyValuePhasRootWithCount([
    { key: ledgerKey, value: material.descriptorCbor },
  ]);
  const ledgerProof = await keyValuePhasProof(
    ledger,
    ledgerKey,
    material.descriptorCbor,
  );

  const sourceKey = Buffer.from(
    SDK.committedWithdrawalKeyBytes(withdrawalId),
    "hex",
  );
  const sourceValue = Buffer.from(infoCbor, "hex");
  const source = await buildCountedRoot("WithdrawalsRootDomain", [
    { key: sourceKey, value: sourceValue },
  ]);
  const sourceProof = await keyValuePhasProof(
    { ...source, root: source.phasRoot },
    sourceKey,
    sourceValue,
  );

  const eventKey: SDK.EventKey = {
    WithdrawalEventKey: { withdrawal_id: withdrawalId },
  };
  const eventValue: SDK.EventToStepValue = {
    step_index: 0n,
    phase: "Withdrawal",
  };
  const eventKeyBytes = Buffer.from(Data.to(eventKey, SDK.EventKey), "hex");
  const eventValueBytes = Buffer.from(
    Data.to(eventValue, SDK.EventToStepValue),
    "hex",
  );
  const event = await buildCountedRoot("EventToStepRootDomain", [
    { key: eventKeyBytes, value: eventValueBytes },
  ]);
  const eventProof = await keyValuePhasProof(
    { ...event, root: event.phasRoot },
    eventKeyBytes,
    eventValueBytes,
  );

  const transitionValue: SDK.TransitionStep = {
    schema_version: SDK.TRANSITION_STEP_SCHEMA_VERSION,
    step_index: 0n,
    event_key: eventKey,
    phase: "Withdrawal",
    pre_utxos_root: ledger.root,
    post_utxos_root: SDK.EMPTY_MERKLE_TREE_ROOT,
  };
  const transitionKeyBytes = Buffer.from(Data.to(0n), "hex");
  const transitionValueBytes = Buffer.from(
    Data.to(transitionValue, SDK.TransitionStep),
    "hex",
  );
  const trace = await buildCountedRoot("TransitionTraceRootDomain", [
    { key: transitionKeyBytes, value: transitionValueBytes },
  ]);
  const traceProof = await keyValuePhasProof(
    { ...trace, root: trace.phasRoot },
    transitionKeyBytes,
    transitionValueBytes,
  );

  return {
    challengedHeaderHash: "14".repeat(28),
    committedWithdrawal: {
      domain: "WithdrawalsRootDomain" as const,
      root: source.root,
      phas_root: source.phasRoot,
      count: source.count,
      key: withdrawalId,
      value: info,
      proof: sourceProof,
    },
    committedWithdrawalInfoCbor: infoCbor,
    eventToStep: {
      domain: "EventToStepRootDomain" as const,
      root: event.root,
      phas_root: event.phasRoot,
      count: event.count,
      key: eventKey,
      value: eventValue,
      proof: eventProof,
    },
    transitionStep: {
      domain: "TransitionTraceRootDomain" as const,
      root: trace.root,
      phas_root: trace.phasRoot,
      count: trace.count,
      key: 0n,
      value: transitionValue,
      proof: traceProof,
    },
    ledgerEvidence: {
      PresentLedgerOutput: {
        output_cbor: outputCbor.toString("hex"),
        membership_proof: ledgerProof,
      },
    },
  };
};

const policy = "0a".repeat(28);
/** `count` distinct one-unit assets under `policy`. */
const tokens = (count: number): Map<string, Map<string, bigint>> =>
  new Map([
    [
      policy,
      new Map(
        Array.from(
          { length: count },
          (_, index) => [index.toString(16).padStart(4, "0"), 1n] as const,
        ),
      ),
    ],
  ]);

describe("withdrawal-mistag preparation and lifecycle handoffs", () => {
  it("prepares a valid withdrawal marked invalid through the terminal state", async () => {
    const prepared = await prepareWithdrawalMistag(
      await makeEvidence({
        lovelace: 1_000_000n,
        validity: "UnpayableWithdrawalValue",
      }),
    );
    expect(prepared.direction).toBe("valid-marked-invalid");
    expect(prepared.actualValid).toBe(true);
    expect(
      "PresentLedgerOutput" in prepared.ledgerEvidence &&
        prepared.ledgerEvidence.PresentLedgerOutput.descriptor_cbor.length > 0,
    ).toBe(true);
    const states = withdrawalMistagStates(prepared);
    expect(states[4]).toMatchObject({
      claimed_valid: false,
      actual_valid: true,
    });
  });

  it("prepares UnpayableWithdrawalValue truth marked valid", async () => {
    const prepared = await prepareWithdrawalMistag(
      await makeEvidence({ lovelace: 1n, validity: "WithdrawalIsValid" }),
    );
    expect(prepared.direction).toBe("invalid-marked-valid");
    expect(prepared.coreValid).toBe(true);
    expect(prepared.payable).toBe(false);
    expect(prepared.actualValid).toBe(false);
    expect(withdrawalMistagStates(prepared)[4]).toMatchObject({
      claimed_valid: true,
      actual_valid: false,
    });
  });

  it("refuses an honestly tagged exact-valid withdrawal", async () => {
    await expect(
      prepareWithdrawalMistag(
        await makeEvidence({
          lovelace: 1_000_000n,
          validity: "WithdrawalIsValid",
        }),
      ),
    ).rejects.toThrow(/honestly tagged/u);
  });

  // The proof judges the requested value by step-03's
  // `output_value_matches_v1` and asset-count bound.
  it("judges a second asset name under the ADA policy invalid", async () => {
    const prepared = await prepareWithdrawalMistag(
      await makeEvidence({
        lovelace: 2_000_000n,
        validity: "WithdrawalIsValid",
        l2Value: new Map([
          [
            "",
            new Map([
              ["", 2_000_000n],
              ["01", 1n],
            ]),
          ],
        ]),
      }),
    );
    expect(prepared.direction).toBe("invalid-marked-valid");
    expect(prepared.coreValid).toBe(false);
  });

  it("judges mixed-width asset names in byte order valid", async () => {
    const prepared = await prepareWithdrawalMistag(
      await makeEvidence({
        lovelace: 5_000_000n,
        validity: "IncorrectWithdrawalValue",
        outputAssets: new Map([
          [
            policy,
            new Map([
              ["0000", 2n],
              ["ff", 1n],
            ]),
          ],
        ]),
      }),
    );
    expect(prepared.direction).toBe("valid-marked-invalid");
    expect(prepared.coreValid).toBe(true);
  });

  it("does not count lovelace toward the asset-count bound", async () => {
    const prepared = await prepareWithdrawalMistag(
      await makeEvidence({
        lovelace: 50_000_000n,
        validity: "TooManyTokensInWithdrawal",
        outputAssets: tokens(SDK.WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT),
      }),
    );
    expect(prepared.direction).toBe("valid-marked-invalid");
    expect(prepared.coreValid).toBe(true);
  });

  it("judges one asset over the bound invalid", async () => {
    const prepared = await prepareWithdrawalMistag(
      await makeEvidence({
        lovelace: 50_000_000n,
        validity: "WithdrawalIsValid",
        outputAssets: tokens(SDK.WITHDRAWAL_MISTAG_MAXIMUM_ASSET_COUNT + 1),
      }),
    );
    expect(prepared.direction).toBe("invalid-marked-valid");
    expect(prepared.coreValid).toBe(false);
  });
});

/** An inline datum whose map repeats key 1; decoding keeps one entry. */
const REPEATED_KEY_DATUM = "d87b9fa201020103ff";

/** The node's verdict over the same committed bytes and ledger output. */
const nodeValidity = async (
  evidence: Awaited<ReturnType<typeof makeEvidence>>,
): Promise<string> => {
  const bodyCbor = plutusConstrFieldCbor(evidence.committedWithdrawalInfoCbor, [
    0,
  ]);
  const result = await Effect.runPromise(
    SDK.classifyWithdrawalFromLedger({
      l2Owner: evidence.committedWithdrawal.value.body.l2_owner,
      l2ValueCbor: plutusConstrFieldCbor(bodyCbor, [2]),
      eventInfoCbor: evidence.committedWithdrawalInfoCbor,
      ledgerOutRef: encodeMidgardSpendInputItem({
        txId: Buffer.from("01".repeat(32), "hex"),
        outputIndex: 0,
      }),
      ledgerOutput: Buffer.from(
        evidence.ledgerEvidence.PresentLedgerOutput.output_cbor,
        "hex",
      ),
    }),
  );
  return result.validity;
};

describe("withdrawal-mistag payout sizing over committed bytes", () => {
  const prepareRepeatedKey = async (
    lovelace: bigint,
    validity: SDK.WithdrawalValidity,
  ) => {
    const evidence = await makeEvidence({
      lovelace,
      validity,
      l1DatumCbor: REPEATED_KEY_DATUM,
    });
    return {
      evidence,
      prepared: await prepareWithdrawalMistag(evidence),
      node: await nodeValidity(evidence),
    };
  };

  it("sizes a repeated-key datum payout as the node does", async () => {
    const { evidence, prepared, node } = await prepareRepeatedKey(
      2_000_000n,
      "UnpayableWithdrawalValue",
    );
    const bodyCbor = plutusConstrFieldCbor(
      evidence.committedWithdrawalInfoCbor,
      [0],
    );
    const sizing = {
      bodyCbor,
      cardanoValueSize: prepared.cardanoValueSize,
    };
    expect(prepared.exactOutputBytes).toBe(
      SDK.withdrawalMistagExactPayoutOutputBytes(sizing),
    );
    expect(prepared.requiredLovelace).toBe(
      SDK.withdrawalMistagMinimumLovelace(sizing),
    );
    // The decoded body keeps one map entry and sizes a shorter payout.
    expect(prepared.exactOutputBytes).toBeGreaterThan(
      SDK.withdrawalMistagExactPayoutOutputBytes({
        ...sizing,
        bodyCbor: SDK.withdrawalBodyBytes(
          evidence.committedWithdrawal.value.body,
        ),
      }),
    );
    expect(node).toBe("WithdrawalIsValid");
    expect(prepared.direction).toBe("valid-marked-invalid");
  });

  it("agrees with the node one lovelace either side of the minimum", async () => {
    const { prepared: probe } = await prepareRepeatedKey(
      2_000_000n,
      "UnpayableWithdrawalValue",
    );
    const minimum = probe.requiredLovelace;
    const below = await prepareRepeatedKey(minimum - 1n, "WithdrawalIsValid");
    expect(below.node).toBe("UnpayableWithdrawalValue");
    expect(below.prepared.payable).toBe(false);
    expect(below.prepared.direction).toBe("invalid-marked-valid");
    const exact = await prepareRepeatedKey(minimum, "UnpayableWithdrawalValue");
    expect(exact.node).toBe("WithdrawalIsValid");
    expect(exact.prepared.payable).toBe(true);
    expect(exact.prepared.direction).toBe("valid-marked-invalid");
  });

  it("refuses committed bytes that do not encode the carried withdrawal", async () => {
    const evidence = await makeEvidence({
      lovelace: 2_000_000n,
      validity: "UnpayableWithdrawalValue",
      l1DatumCbor: REPEATED_KEY_DATUM,
    });
    await expect(
      prepareWithdrawalMistag({
        ...evidence,
        committedWithdrawalInfoCbor: replacePlutusConstrFieldCbor(
          evidence.committedWithdrawalInfoCbor,
          [0, 4],
          "d87b9fa10104ff",
        ),
      }),
    ).rejects.toThrow(/do not encode the committed withdrawal/u);
  });
});

/** An inline datum carrying constructor 2^64 - 1, which no typed encoder writes. */
const LARGEST_CONSTRUCTOR_DATUM = "d87b9fd866821bffffffffffffffff80ff";

describe("withdrawal-mistag payloads carry the committed bytes", () => {
  for (const [label, l1DatumCbor] of [
    ["the largest constructor index", LARGEST_CONSTRUCTOR_DATUM],
    ["a repeated map key", REPEATED_KEY_DATUM],
  ] as const) {
    it(`carries a datum with ${label} through every step payload`, async () => {
      const evidence = await makeEvidence({
        lovelace: 2_000_000n,
        validity: "UnpayableWithdrawalValue",
        l1DatumCbor,
      });
      const prepared = await prepareWithdrawalMistag(evidence);
      const infoCbor = evidence.committedWithdrawalInfoCbor;
      expect(prepared.committedWithdrawalInfoCbor).toBe(infoCbor);
      const committed = [
        infoCbor,
        infoCbor,
        infoCbor,
        plutusConstrFieldCbor(infoCbor, [0]),
      ];
      const paths = [[0, 5], [0], [0], [0]];
      for (const step of [0, 1, 2, 3] as const) {
        expect(
          plutusConstrFieldCbor(
            withdrawalMistagStepPayloadCbor(prepared, step),
            paths[step]!,
          ),
        ).toBe(committed[step]);
      }
    });
  }
});
