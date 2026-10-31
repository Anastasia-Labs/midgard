import { type FraudProofRawL1Transaction } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { unsafeCorrectionLockWitnessForTest } from "../../src/indexers/authenticated-state-queue-observation.js";
import { h28, h32 } from "../support/deployment-authority-fixture.js";

const value = (policyId: string, assetName: string): CML.Value => {
  const multiasset = CML.MultiAsset.new();
  multiasset.set(
    CML.ScriptHash.from_hex(policyId),
    CML.AssetName.from_hex(assetName),
    1n,
  );
  return CML.Value.new(2_000_000n, multiasset);
};

/**
 * The CorrectionLock witness of one RemoveUnavailableBlockAfterTimeout step,
 * derived from a transaction that spends `locksIn`, references `referenced`
 * and outputs `locksOut`, burning `burned` availability tokens.
 */
const availabilityLockWitness = ({
  locksIn,
  referenced = [],
  locksOut,
  approach,
  challengeAssetName,
  burned = [],
}: {
  locksIn: readonly SDK.CorrectionLockDatum[];
  referenced?: readonly SDK.CorrectionLockDatum[];
  locksOut: readonly SDK.CorrectionLockDatum[];
  approach: "prune" | "head";
  challengeAssetName: string;
  burned?: readonly string[];
}) => {
  const hub = h28("c2");
  const stateQueuePolicy = h28("c3");
  const availabilityPolicy = h28("c4");
  const lockAddress = credentialToAddress(
    "Preprod",
    scriptHashToCredential(h28("c1")),
  );
  const lockOutput = (datum: SDK.CorrectionLockDatum) =>
    CML.TransactionOutput.new(
      CML.Address.from_bech32(lockAddress),
      value(hub, SDK.CORRECTION_LOCK_ASSET_NAME),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(Data.to(datum, SDK.CorrectionLockDatum)),
      ),
      undefined,
    );
  const resolved = (
    txByte: string,
    datums: readonly SDK.CorrectionLockDatum[],
  ) =>
    datums.map((datum, index) => {
      const output = lockOutput(datum);
      return {
        input: CML.TransactionInput.new(
          CML.TransactionHash.from_hex(h32(txByte)),
          BigInt(index),
        ),
        resolved: {
          outRef: `${h32(txByte)}#${index.toString()}`,
          outputCbor: output.to_canonical_cbor_hex(),
          datumCbor: output.datum()!.as_datum()!.to_canonical_cbor_hex(),
          referenceScriptCbor: null,
        },
      };
    });
  const spent = resolved("e1", locksIn);
  const refs = resolved("e2", referenced);
  const inputs = CML.TransactionInputList.new();
  spent.forEach(({ input }) => inputs.add(input));
  const outputs = CML.TransactionOutputList.new();
  locksOut.forEach((datum) => outputs.add(lockOutput(datum)));
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  if (refs.length > 0) {
    const referenceInputs = CML.TransactionInputList.new();
    refs.forEach(({ input }) => referenceInputs.add(input));
    body.set_reference_inputs(referenceInputs);
  }
  const unavailableHeaderHash = h28("c7");
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(stateQueuePolicy),
    CML.AssetName.from_hex(
      `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${h28(approach === "prune" ? "c8" : "c7")}`,
    ),
    -1n,
  );
  for (const assetName of burned)
    mint.set(
      CML.ScriptHash.from_hex(availabilityPolicy),
      CML.AssetName.from_hex(assetName),
      -1n,
    );
  body.set_mint(mint);
  const mintPolicies = [stateQueuePolicy, availabilityPolicy].sort();
  const outRef = { transactionId: h32("e3"), outputIndex: 0n };
  const redeemer = Data.to(
    {
      RemoveUnavailableBlockAfterTimeout: {
        yield_to_ref_input_index: 0n,
        unavailable_header_hash: unavailableHeaderHash,
        challenge_asset_name: challengeAssetName,
        removal_approach:
          approach === "prune"
            ? {
                PruneTimedOutBlockDescendant: {
                  confirmed_state_ref_input_index: 0n,
                  timed_out_node_input_outref: outRef,
                  timed_out_node_output_index: 0n,
                },
              }
            : {
                RemoveTimedOutHead: {
                  confirmed_state_input_outref: outRef,
                  confirmed_state_output_index: 0n,
                },
              },
      },
    },
    SDK.StateQueueRedeemer,
  );
  return {
    unavailableHeaderHash,
    derive: () =>
      unsafeCorrectionLockWitnessForTest({
        raw: {
          txHash: CML.hash_transaction(body).to_hex(),
          resolvedInputs: spent.map(({ resolved }) => resolved),
          resolvedReferenceInputs: refs.map(({ resolved }) => resolved),
        } as unknown as FraudProofRawL1Transaction,
        body,
        mintPolicies,
        redeemers: [
          {
            purpose: "mint",
            index: mintPolicies.indexOf(stateQueuePolicy).toString(),
            cborHex: redeemer,
          },
        ],
        stateQueuePolicyId: stateQueuePolicy,
        correctionLockAddress: lockAddress,
        hubOraclePolicyId: hub,
        fraudProofPolicyId: h28("c5"),
        fraudProofAddress: lockAddress,
        availabilityChallengePolicyId: availabilityPolicy,
      }),
  };
};

describe("availability Timeout CorrectionLock continuation", () => {
  const challengeAssetName = `44414348${"c7".repeat(28)}`;
  const lockedFor = (
    targetHeaderHash: string,
    identity: SDK.CorrectionIdentity,
  ): SDK.CorrectionLockDatum => ({
    Locked: {
      target_header_hash: targetHeaderHash,
      correction_identity: identity,
    },
  });
  const heldIdentity: SDK.CorrectionIdentity = {
    AvailabilityChallenge: { challenge_asset_name: challengeAssetName },
  };
  const held = lockedFor(h28("c7"), heldIdentity);

  it("admits the Timeout that takes the lock, each prune under it, and the head removal that releases it", () => {
    const takes = availabilityLockWitness({
      locksIn: ["Idle"],
      locksOut: [held],
      approach: "head",
      challengeAssetName,
      burned: [challengeAssetName],
    });
    const prunes = availabilityLockWitness({
      locksIn: [held],
      locksOut: [held],
      approach: "prune",
      challengeAssetName,
    });
    const releases = availabilityLockWitness({
      locksIn: [held],
      locksOut: ["Idle"],
      approach: "head",
      challengeAssetName,
    });
    expect(takes.derive()).toMatchObject({
      kind: "correction_transition",
      targetHeaderHash: takes.unavailableHeaderHash,
      correctionIdentity: heldIdentity,
      previousDatum: "Idle",
      nextDatum: held,
    });
    expect(prunes.derive()).toMatchObject({
      kind: "correction_transition",
      targetHeaderHash: prunes.unavailableHeaderHash,
      correctionIdentity: heldIdentity,
      previousDatum: held,
      nextDatum: held,
    });
    expect(releases.derive()).toMatchObject({
      kind: "correction_transition",
      correctionIdentity: heldIdentity,
      previousDatum: held,
      nextDatum: "Idle",
    });
  });

  it.each([
    ["another target header", lockedFor(h28("d7"), heldIdentity)],
    [
      "another availability challenge",
      lockedFor(h28("c7"), {
        AvailabilityChallenge: {
          challenge_asset_name: `44414348${"d7".repeat(28)}`,
        },
      }),
    ],
    ["an attestation timeout", lockedFor(h28("c7"), "AttestationTimeout")],
  ] as const)("refuses to continue a lock held for %s", (_, previous) => {
    for (const approach of ["prune", "head"] as const)
      expect(
        availabilityLockWitness({
          locksIn: [previous],
          locksOut: [previous],
          approach,
          challengeAssetName,
        }).derive,
      ).toThrow(
        "availability timeout continues a CorrectionLock held by another correction",
      );
  });

  it.each([
    ["no lock spent", { locksIn: [], locksOut: [held] }],
    ["two locks spent", { locksIn: [held, held], locksOut: [held] }],
    [
      "a lock referenced",
      { locksIn: [held], referenced: [held], locksOut: [held] },
    ],
    ["no lock output", { locksIn: [held], locksOut: [] }],
  ] as const)("refuses a Timeout step with %s", (_, topology) => {
    expect(
      availabilityLockWitness({
        ...topology,
        approach: "prune",
        challengeAssetName,
      }).derive,
    ).toThrow("availability timeout has invalid CorrectionLock topology");
  });
});
