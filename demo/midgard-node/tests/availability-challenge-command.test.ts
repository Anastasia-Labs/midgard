import * as SDK from "@al-ft/midgard-sdk";
import { Constr, Data, type UTxO } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  assertAvailabilityCommandRemovalCapital,
  parseAvailabilityOutRef,
  planAvailabilityCommandAction,
  runAvailabilityChallengeCommand,
} from "../src/commands/availability-challenge.js";
import { TEST_AVAILABILITY_PARAMETERS as parameters } from "./helpers/availability-challenge.js";

const hash = "11".repeat(28);
const challenge = SDK.buildDaAvailabilityChallengeDatumPlan({
  commitment: SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: "22".repeat(28),
    headerHash: hash,
    payload: Uint8Array.of(1),
    responseGeometry: parameters.response_geometry,
  }),
  challengerFundingOutRef: {
    transactionId: "66".repeat(32),
    outputIndex: 0n,
  },
  challenger: "77".repeat(28),
  openedAt: 1_000n,
  parameters,
});
const lock = (datum: SDK.CorrectionLockDatum): UTxO => ({
  txHash: "88".repeat(32),
  outputIndex: 0,
  address: "correction-lock",
  assets: { lovelace: 3_000_000n },
  datum: Data.to(datum, SDK.CorrectionLockDatum),
});
const snapshot = {
  headerHash: hash,
  recordDatum: challenge.record,
  terminalDatum: challenge.terminalAccumulator,
  correctionLock: lock("Idle"),
};

describe("operational availability commands", () => {
  it("requires the challenger bond, the challenge record and the full current descendant removal reserve", () => {
    const collateral = {
      ...lock("Idle"),
      address: "actor",
      assets: { lovelace: 100_000_000n },
    };
    // Open spends one challenger coin holding exactly the challenger bond, the
    // challenge record lovelace and the fee.
    const opening =
      parameters.challenger_bond_lovelace +
      parameters.challenge_record_lovelace +
      parameters.max_open_fee_lovelace;
    const available = {
      ...collateral,
      txHash: "99".repeat(32),
      assets: {
        lovelace:
          opening + 2n * parameters.max_timeout_fee_lovelace + 1_000_000n,
      },
    };
    const funding = {
      action: "open" as const,
      parameters,
      remainingRemovalSteps: 2,
      minimumChangeLovelace: 1_000_000n,
      walletAddress: "actor",
      walletUtxos: [{ ...available, datum: undefined }, collateral],
      collateral,
      reservedOutRefs: new Set<string>(),
    };
    expect(() =>
      assertAvailabilityCommandRemovalCapital(funding),
    ).not.toThrow();
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        remainingRemovalSteps: 3,
      }),
    ).toThrow(/remaining descendant removal path/);
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        walletUtxos: [
          {
            ...available,
            datum: undefined,
            assets: { lovelace: available.assets.lovelace - 1n },
          },
          collateral,
        ],
      }),
    ).toThrow(/remaining descendant removal path/);
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        reservedOutRefs: new Set([`${available.txHash}#0`]),
      }),
    ).toThrow(/reserved inputs/);
  });

  it("does not count collateral, foreign outputs or native assets as timeout working capital", () => {
    const collateral: UTxO = {
      txHash: "88".repeat(32),
      outputIndex: 0,
      address: "actor",
      assets: { lovelace: 100_000_000n },
    };
    const reserve = 2n * parameters.max_timeout_fee_lovelace + 1_000_000n;
    const funding = {
      action: "timeout" as const,
      parameters,
      remainingRemovalSteps: 2,
      minimumChangeLovelace: 1_000_000n,
      walletAddress: "actor",
      collateral,
      reservedOutRefs: new Set<string>(),
    };
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        walletUtxos: [
          collateral,
          { ...collateral, txHash: "99".repeat(32), address: "foreign" },
          {
            ...collateral,
            txHash: "aa".repeat(32),
            assets: { lovelace: reserve, token: 1n },
          },
        ],
      }),
    ).toThrow(/remaining descendant removal path/);
    expect(() =>
      assertAvailabilityCommandRemovalCapital({
        ...funding,
        walletUtxos: [
          collateral,
          {
            ...collateral,
            txHash: "bb".repeat(32),
            assets: { lovelace: reserve },
          },
        ],
      }),
    ).not.toThrow();
  });

  it("parses exact output references and refuses ambiguous indexes", () => {
    expect(parseAvailabilityOutRef(`${"aa".repeat(32)}#3`)).toEqual({
      txHash: "aa".repeat(32),
      outputIndex: 3,
    });
    for (const value of [
      `${"aa".repeat(32)}#03`,
      `${"aa".repeat(32)}#65536`,
      "unknown#0",
    ])
      expect(() => parseAvailabilityOutRef(value)).toThrow(/canonical/);
  });

  it("refuses invalid journal and missing actor credentials before reading manifests or calling providers", async () => {
    const options = {
      headerHash: hash,
      manifest: "/unread-manifest.json",
      journal: "/tmp/availability.sqlite",
      walletSeedEnv: "AVAILABILITY_ACTOR_SEED",
    };
    await expect(
      runAvailabilityChallengeCommand(
        "status",
        { ...options, journal: "relative.sqlite" },
        {},
      ),
    ).rejects.toThrow(/absolute durable path/);
    await expect(
      runAvailabilityChallengeCommand("open", options, {}),
    ).rejects.toThrow(/actor seed is missing/);
  });

  it("refuses premature timeout and settles expired tranches before the head timeout", () => {
    expect(() =>
      planAvailabilityCommandAction(
        "timeout",
        snapshot,
        Number(challenge.responseDeadline),
      ),
    ).toThrow(/deadline/);
    expect(
      planAvailabilityCommandAction(
        "timeout",
        snapshot,
        Number(challenge.responseDeadline) + 1,
      ),
    ).toBe("settle");
    const terminal = {
      ...snapshot,
      terminalDatum: {
        ...challenge.terminalAccumulator,
        next_tranche_index: 1n,
        has_timed_out_tranche: true,
      },
    };
    expect(
      planAvailabilityCommandAction(
        "timeout",
        terminal,
        Number(challenge.responseDeadline) + 1,
      ),
    ).toBe("timeout");
    expect(() =>
      planAvailabilityCommandAction(
        "timeout",
        {
          ...terminal,
          terminalDatum: {
            ...terminal.terminalDatum,
            has_timed_out_tranche: false,
          },
        },
        Number(challenge.responseDeadline) + 1,
      ),
    ).toThrow(/Fully answered/);
  });

  it("resumes descendant pruning and head removal only under the matching availability lock", () => {
    const locked = {
      headerHash: hash,
      correctionLock: lock({
        Locked: {
          target_header_hash: hash,
          correction_identity: {
            AvailabilityChallenge: {
              challenge_asset_name: challenge.challengeAssetName,
            },
          },
        },
      }),
    };
    const descendant: SDK.StateQueueUTxO = {
      utxo: lock("Idle"),
      assetName: "00",
      datum: { key: "Empty", next: "Empty", data: new Constr(0, []) },
    };
    expect(
      planAvailabilityCommandAction(
        "timeout",
        { ...locked, descendant },
        10_000,
      ),
    ).toBe("prune");
    expect(planAvailabilityCommandAction("timeout", locked, 10_000)).toBe(
      "remove",
    );
    expect(() =>
      planAvailabilityCommandAction(
        "timeout",
        { ...locked, headerHash: "99".repeat(28) },
        10_000,
      ),
    ).toThrow(/matching active removal lock/);
  });
});
