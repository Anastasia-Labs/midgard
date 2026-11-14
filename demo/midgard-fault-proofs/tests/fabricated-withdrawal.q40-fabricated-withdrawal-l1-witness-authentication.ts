import "./fabricated-withdrawal.q40-fabricated-withdrawal-proof-plan.js";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { authenticateFabricatedHistoryWitness } from "../src/fabricated-history-witness.js";
import {
  classifyFabricatedWithdrawalFault,
  type FabricatedWithdrawalL1Witness,
  FabricatedWithdrawalRejection,
  prepareFabricatedWithdrawalFromCommittedLeaves,
} from "../src/prepare-fabricated-withdrawal.js";
import {
  AU_LEAF,
  AU_WITHDRAWALS_PHAS_ROOT,
  AU_WITHDRAWALS_ROOT,
  AUTHENTIC_WITHDRAWAL_ID,
  buildWithdrawalsBlockFixture,
  FABRICATED_WITHDRAWAL_ID,
  FI_LEAF,
  HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
  HEADER_END_TIME,
  HEADER_START_TIME,
  historyWitness,
  KEY_AUTHENTIC_WITHDRAWAL_ID,
  l1Observation,
  MM_LEAF,
  VALUE_AUTHENTIC_WITHDRAWAL_INFO,
  type WithdrawalLeafEntry,
} from "./fabricated-withdrawal.build-withdrawals-block-fixture.js";
import {
  absentIdentityWitness,
  authenticWithdrawalInfo,
  eventDatumBytes,
  presentEventWitness,
  withdrawalEventDatum,
  withdrawalEventUtxoFixture,
} from "./fabricated-withdrawal.q40-fabricated-withdrawal-evidence-admission.js";
import { h28, h32 } from "./helpers/canonical-block-evidence-fixture.js";

describe("Q40 fabricated-withdrawal L1 witness authentication", () => {
  const leafOf = async (
    leaf: WithdrawalLeafEntry,
    witness: FabricatedWithdrawalL1Witness,
  ) => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [leaf] });
    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedWithdrawalsRoot: fixture.withdrawalsRoot,
      withdrawalCount: fixture.withdrawalCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness,
    });
    return plan.challengedLeaf;
  };

  it("refuses an absence claim without an authenticated history token, and any witness that is not authenticated L1 security-grade evidence", async () => {
    const leaf = await leafOf(FI_LEAF, absentIdentityWitness());
    // A gap-shaped datum without its list NFT cannot authenticate absence.
    await expect(
      classifyFabricatedWithdrawalFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: absentIdentityWitness(false),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
    await expect(
      classifyFabricatedWithdrawalFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: {
          ...absentIdentityWitness(),
          observation: l1Observation({
            provenance: {
              trustClass: "operator_admin_api",
              sourceId: "node-admin",
              grade: "diagnostic",
              diagnosticLabel: "operator diagnostic",
            },
          }),
        },
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
  });

  it("refuses a present-event witness that is not bound to the committed identity", async () => {
    const leaf = await leafOf(MM_LEAF, presentEventWitness());
    // The observed asset name is not `out_ref_to_nonce(committed_withdrawal_id)`.
    await expect(
      classifyFabricatedWithdrawalFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: presentEventWitness({ observedEventAssetName: h32(0x4d) }),
      }),
    ).rejects.toMatchObject({
      code: "history_witness_invalid",
    });
    // The retained datum names a different withdrawal identity.
    await expect(
      classifyFabricatedWithdrawalFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: presentEventWitness({
          eventDatumCbor: eventDatumBytes(
            withdrawalEventDatum({ id: FABRICATED_WITHDRAWAL_ID }),
          ),
        }),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
  });

  it("refuses to challenge the authentic block, whose header committed exactly the authentic order", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [AU_LEAF] });
    // The Aiken-measured roots of `authentic_withdrawal_block_v1`.
    expect(fixture.withdrawalsPhasRoot).toBe(AU_WITHDRAWALS_PHAS_ROOT);
    expect(fixture.withdrawalsRoot).toBe(AU_WITHDRAWALS_ROOT);
    const attempt = async () =>
      await prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedWithdrawalsRoot: fixture.withdrawalsRoot,
        withdrawalCount: fixture.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: presentEventWitness(),
      });
    await expect(attempt()).rejects.toBeInstanceOf(
      FabricatedWithdrawalRejection,
    );
    await expect(attempt()).rejects.toMatchObject({
      code: "authentic_content_matches_commitment",
    });
  });

  it("refuses to challenge a block whose committed leaf differs from the authentic order only in its operator-owned validity verdict", async () => {
    // Decision 0007: the L1 order datum's `WithdrawalIsValid` is a placeholder,
    // so the honest verdict for a withdrawal whose L2 output does not exist is
    // the operator's own claim, not fabricated content. This is the honest
    // control of every withdrawal fixture: it must survive this family with the
    // verdict it actually earned, and a wrong verdict belongs to
    // `withdrawalMistag`.
    const revalidated: SDK.WithdrawalInfo = {
      ...authenticWithdrawalInfo(),
      validity: "NonExistentWithdrawalUtxo",
    };
    const value = SDK.committedWithdrawalValueBytes(revalidated);
    expect(value).not.toBe(VALUE_AUTHENTIC_WITHDRAWAL_INFO);
    expect(
      await Effect.runPromise(SDK.withdrawalContentCommitment(revalidated)),
    ).toBe(HASH_AUTHENTIC_WITHDRAWAL_CONTENT);
    const fixture = await buildWithdrawalsBlockFixture({
      leaves: [{ key: KEY_AUTHENTIC_WITHDRAWAL_ID, value }],
    });
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedWithdrawalsRoot: fixture.withdrawalsRoot,
        withdrawalCount: fixture.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: presentEventWitness(),
      }),
    ).rejects.toMatchObject({
      name: "FabricatedWithdrawalRejectionV1",
      code: "authentic_content_matches_commitment",
    });
  });

  it("proves an authentic event ineligible for the challenged block, on either side of the window", async () => {
    const leaf = await leafOf(MM_LEAF, presentEventWitness());
    for (const inclusionTime of [HEADER_START_TIME, HEADER_END_TIME + 1n]) {
      await expect(
        classifyFabricatedWithdrawalFault({
          leaf,
          headerStartTime: HEADER_START_TIME,
          headerEndTime: HEADER_END_TIME,
          witness: presentEventWitness({
            eventDatumCbor: eventDatumBytes(
              withdrawalEventDatum({ inclusionTime }),
            ),
          }),
        }),
      ).resolves.toMatchObject({
        fault: {
          IneligibleWithdrawalEvent: { event_inclusion_time: inclusionTime },
        },
      });
    }
  });
});

it("authenticates an equal-key filler as absence without counting its funds or using nonce liveness", async () => {
  const anchor = withdrawalEventUtxoFixture();
  const node = Data.from(anchor.datum!, SDK.EventHistoryNode);
  const filler = {
    ...anchor,
    datum: Data.to(
      { ...node, payload: { Filler: { refund_key: h28(0x44) } } },
      SDK.EventHistoryNode,
    ),
  };
  const result = await authenticateFabricatedHistoryWitness(
    historyWitness(filler),
    "Withdrawal",
    AUTHENTIC_WITHDRAWAL_ID,
  );
  expect(result.witness.kind).toBe("Absent");
  expect(result.captured).toBeUndefined();
  expect(result.witness.anchor.utxo.assets.lovelace).toBe(5_000_000n);
});
