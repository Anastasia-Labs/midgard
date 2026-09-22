import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { selectWatcherAvailabilityAction } from "../../src/availability/action.js";
import { selectWatcherAvailabilityFunding } from "../../src/availability/runtime.js";

const address = credentialToAddress("Preprod", {
  type: "Key",
  hash: "11".repeat(28),
});
const utxo = (index: number, lovelace = 10_000_000n): UTxO => ({
  txHash: "22".repeat(32),
  outputIndex: index,
  address,
  assets: { lovelace },
});
const fixture = () => {
  const parameters = SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(
      SDK.DA_AVAILABILITY_RESPONSE_GEOMETRY_MEASUREMENT_CANDIDATE,
    ),
    daBondLovelace: 10_000_000_000n,
    challengerBondLovelace: 10_000_000_000n,
    maxOpenFeeLovelace: 500_000n,
    maxPublicationFeeLovelace: 500_000n,
    maxSettlementFeeLovelace: 500_000n,
    maxCloseFeeLovelace: 1_000_000n,
    maxTimeoutFeeLovelace: 1_200_000n,
  });
  const bytes = new Uint8Array(16_000).fill(7);
  const commitment = SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: "33".repeat(28),
    headerHash: "44".repeat(28),
    payload: bytes,
    bondOwner: "55".repeat(28),
    responseGeometry: parameters.response_geometry,
  });
  const bondInput = { transactionId: "66".repeat(32), outputIndex: 0n };
  const available: SDK.DaAvailabilityBondDatum = {
    Available: {
      commitment,
      da_bond_asset_name: SDK.daAvailabilityBondAssetName(bondInput),
      committee_signers_hash: "77".repeat(32),
      attested_signers: "80" + "00".repeat(31),
    },
  };
  const plan = SDK.buildDaAvailabilityChallengeDatumPlan({
    availableBond: available,
    bondInputOutRef: bondInput,
    challenger: "11".repeat(28),
    openedAt: 1_000n,
    parameters,
  });
  const queue: SDK.StateQueueUTxO = {
    utxo: utxo(3),
    datum: { key: "Empty", next: "Empty", data: "" },
    assetName: "",
  };
  const snapshot: SDK.DaAvailabilityChallengeSnapshot = {
    headerHash: commitment.header_hash,
    bond: utxo(0),
    bondDatum: plan.challengedBond,
    queue,
    confirmedState: {
      ...queue,
      datum: { ...queue.datum, next: { Key: { key: commitment.header_hash } } },
    },
    correctionLock: {
      ...utxo(4),
      datum: Data.to("Idle", SDK.CorrectionLockDatum),
    },
    terminal: utxo(1),
    terminalDatum: plan.terminalAccumulator,
    tranches: [{ utxo: utxo(2), datum: plan.trancheThreads[0]! }],
  };
  return { parameters, bytes, available, plan, snapshot };
};

describe("watcher availability lifecycle action selection", () => {
  it("opens for post-attestation public withholding and leaves available bytes unchallenged", () => {
    const { available, snapshot } = fixture();
    const attested = { ...snapshot, bondDatum: available };
    expect(selectWatcherAvailabilityAction(attested, false, 2_000n)).toEqual({
      action: "open",
    });
    expect(selectWatcherAvailabilityAction(attested, true, 2_000n)).toBeNull();
  });

  it("preserves a partial response until the deadline, then settles the timed-out tranche", () => {
    const { snapshot, plan, bytes, parameters } = fixture();
    if (!("ChallengedBond" in plan.challengedBond))
      throw new Error("fixture did not open a challenge");
    const challenged = plan.challengedBond.ChallengedBond;
    const publications = SDK.planDaAvailabilityPublications({
      commitment: challenged.commitment,
      payload: bytes,
      challengeAssetName: challenged.challenge_asset_name,
    });
    const partial = SDK.advanceDaAvailabilityTranche({
      active: snapshot.tranches[0]!.datum,
      publication: publications[0]!.publications[0]!,
      responseGeometry: parameters.response_geometry,
      inclusiveValidityUpper: 2_000n,
      carrierOutputIndex: 1n,
    });
    const continued = {
      ...snapshot,
      tranches: [{ utxo: utxo(5), datum: partial, carrier: utxo(1) }],
    };
    expect(
      selectWatcherAvailabilityAction(
        continued,
        false,
        challenged.response_deadline - 1n,
      ),
    ).toBeNull();
    expect(
      selectWatcherAvailabilityAction(
        continued,
        false,
        challenged.response_deadline,
      )?.action,
    ).toBe("settle");
  });

  it("chooses terminal timeout or answered close only after every tranche was settled", () => {
    const { snapshot } = fixture();
    const terminal = { ...snapshot.terminalDatum!, next_tranche_index: 1n };
    expect(
      selectWatcherAvailabilityAction(
        {
          ...snapshot,
          terminalDatum: { ...terminal, has_timed_out_tranche: true },
        },
        false,
        9_000_000n,
      )?.action,
    ).toBe("timeout");
    expect(
      selectWatcherAvailabilityAction(
        {
          ...snapshot,
          terminalDatum: { ...terminal, has_timed_out_tranche: false },
        },
        false,
        9_000_000n,
      )?.action,
    ).toBe("close");
    expect(() =>
      selectWatcherAvailabilityAction(
        { ...snapshot, tranches: [] },
        false,
        9_000_000n,
      ),
    ).toThrow("next unsettled tranche");
  });

  it("waits for earlier queue headers before starting timeout removal", () => {
    const { snapshot } = fixture();
    const later = {
      ...snapshot,
      confirmedState: {
        ...snapshot.confirmedState,
        datum: {
          ...snapshot.confirmedState.datum,
          next: { Key: { key: "aa".repeat(28) } },
        },
      },
      terminalDatum: {
        ...snapshot.terminalDatum!,
        next_tranche_index: 1n,
        has_timed_out_tranche: true,
      },
    };
    expect(
      selectWatcherAvailabilityAction(later, false, 9_000_000n),
    ).toBeNull();
  });

  it("resumes descendant pruning after timeout burned the bond and finally removes the head", () => {
    const { snapshot, plan } = fixture();
    const lock: SDK.CorrectionLockDatum = {
      Locked: {
        target_header_hash: snapshot.headerHash,
        correction_identity: {
          AvailabilityChallenge: {
            challenge_asset_name: plan.challengeAssetName,
          },
        },
      },
    };
    const timedOut = {
      ...snapshot,
      bond: undefined,
      bondDatum: undefined,
      correctionLock: {
        ...utxo(4),
        datum: Data.to(lock, SDK.CorrectionLockDatum),
      },
    };
    expect(
      selectWatcherAvailabilityAction(
        { ...timedOut, descendant: snapshot.queue },
        false,
        9_000_000n,
      )?.action,
    ).toBe("prune");
    expect(
      selectWatcherAvailabilityAction(timedOut, false, 9_000_000n)?.action,
    ).toBe("remove");
    expect(() =>
      selectWatcherAvailabilityAction(
        { ...timedOut, bond: utxo(0) },
        false,
        9_000_000n,
      ),
    ).toThrow("live DA bond");
  });
});

describe("watcher availability funding", () => {
  it("keeps collateral disjoint and preserves working capital beyond the locked bond", () => {
    const selection = selectWatcherAvailabilityFunding({
      utxos: [utxo(0, 5n), utxo(1, 100n), utxo(2, 30n)],
      collateralLovelace: 5n,
      openingLovelace: 100n,
      requiredWorkingLovelace: 130n,
    });
    expect(selection.collateral.outputIndex).toBe(0);
    expect(selection.exactOpening?.outputIndex).toBe(1);
    expect(() =>
      selectWatcherAvailabilityFunding({
        utxos: [utxo(0, 5n), utxo(1, 100n)],
        collateralLovelace: 5n,
        openingLovelace: 100n,
        requiredWorkingLovelace: 130n,
      }),
    ).toThrow("timeout removal path");
  });
  it("refuses to count datum-bearing outputs as collateral or fee capital", () => {
    expect(() =>
      selectWatcherAvailabilityFunding({
        utxos: [{ ...utxo(0, 5n), datum: Data.void() }, utxo(1, 100n)],
        collateralLovelace: 5n,
        openingLovelace: 100n,
        requiredWorkingLovelace: 100n,
      }),
    ).toThrow("separate plain-ADA collateral");
  });
  it("excludes reserved spending capital while allowing the actor's existing collateral", () => {
    const collateral = utxo(0, 5n);
    const reserved = utxo(1, 100n);
    const args = {
      utxos: [collateral, reserved, utxo(2, 30n)],
      collateralLovelace: 5n,
      openingLovelace: 100n,
      requiredWorkingLovelace: 30n,
      reservedOutRefs: new Set([
        `${collateral.txHash}#${collateral.outputIndex}`,
        `${reserved.txHash}#${reserved.outputIndex}`,
      ]),
    };
    const selection = selectWatcherAvailabilityFunding(args);
    expect(selection.collateral.outputIndex).toBe(0);
    expect(selection.funding.outputIndex).toBe(2);
    expect(selection.exactOpening).toBeUndefined();
    expect(() =>
      selectWatcherAvailabilityFunding({
        ...args,
        requiredWorkingLovelace: 31n,
      }),
    ).toThrow("timeout removal path");
  });
});
