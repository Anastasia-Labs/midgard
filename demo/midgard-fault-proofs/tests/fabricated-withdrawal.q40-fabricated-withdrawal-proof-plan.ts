import "./fabricated-withdrawal.fabricated-withdrawal-production-evidence-authority.js";

import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  fabricatedWithdrawalBlockEvidenceFromVerifiedPayload,
  prepareFabricatedWithdrawalFromCommittedLeaves,
} from "../src/prepare-fabricated-withdrawal.js";
import {
  AUTHENTIC_INCLUSION_TIME,
  buildWithdrawalsBlockFixture,
  DA_PROVENANCE,
  FI_LEAF,
  FI_WITHDRAWALS_PHAS_ROOT,
  FI_WITHDRAWALS_ROOT,
  HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
  HASH_DIVERTED_WITHDRAWAL_CONTENT,
  HEADER_END_TIME,
  HEADER_START_TIME,
  KEY_AUTHENTIC_WITHDRAWAL_ID,
  KEY_FABRICATED_WITHDRAWAL_ID,
  MM_LEAF,
  MM_WITHDRAWALS_PHAS_ROOT,
  MM_WITHDRAWALS_ROOT,
  VALUE_AUTHENTIC_WITHDRAWAL_INFO,
  VALUE_DIVERTED_WITHDRAWAL_INFO,
  WITHDRAWAL_POLICY_ID,
} from "./fabricated-withdrawal.build-withdrawals-block-fixture.js";
import {
  absentIdentityWitness,
  authenticWithdrawalInfo,
  historyCommitment,
  historyOpening,
  l1AddressOf,
  presentEventWitness,
} from "./fabricated-withdrawal.q40-fabricated-withdrawal-evidence-admission.js";
import { h28 } from "./helpers/canonical-block-evidence-fixture.js";

describe("Q40 fabricated-withdrawal proof plan", () => {
  it("builds a nonexistent-identity plan from an authenticated absence witness", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [FI_LEAF] });
    // The Aiken-measured roots of `fabricated_identity_block_v1`.
    expect(fixture.withdrawalsPhasRoot).toBe(FI_WITHDRAWALS_PHAS_ROOT);
    expect(fixture.withdrawalsRoot).toBe(FI_WITHDRAWALS_ROOT);

    const evidence = await fabricatedWithdrawalBlockEvidenceFromVerifiedPayload(
      {
        observation: fixture.observation,
        payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
        daProvenance: DA_PROVENANCE,
      },
    );
    const outputDir = await mkdtemp(
      join(tmpdir(), "q40-fabricated-withdrawal-"),
    );
    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: evidence.headerHash,
      committedWithdrawalsRoot: evidence.committedWithdrawalsRoot,
      withdrawalCount: evidence.withdrawalCount,
      headerStartTime: evidence.headerStartTime,
      headerEndTime: evidence.headerEndTime,
      entries: evidence.entries,
      witness: absentIdentityWitness(),
      outputDir,
    });

    expect(plan.violationId).toBe("fabricated-withdrawal");
    expect(plan.fraudCategoryId).toBe("0000000c");
    expect(plan.threadTokenAssetName).toBe(`0000000c${fixture.headerHash}`);
    expect(plan.withdrawalsPhasRoot).toBe(FI_WITHDRAWALS_PHAS_ROOT);
    expect(plan.committedWithdrawalsRoot).toBe(FI_WITHDRAWALS_ROOT);
    expect(plan.classification.verdict).toBe("WithdrawalIdentityAbsent");
    expect(plan.classification.fault).toBe("NonexistentWithdrawalIdentity");
    expect(plan.step02State).toEqual({
      stateQueuePolicyId: h28(0x15),
      challengedHeaderHash: fixture.headerHash,
      headerStartTime: "10",
      headerEndTime: "20",
      committedWithdrawalIdCbor: KEY_FABRICATED_WITHDRAWAL_ID,
      committedWithdrawalContentHash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
    });
    // An absence proof has no retained content to open at step 03.
    expect(plan.authenticContent.openingCbor).toBeNull();
    expect(
      plan.withdrawalInclusion.withdrawalMembershipProofCbor.length,
    ).toBeGreaterThan(0);
    expect(plan.files).toBeDefined();
    expect(
      JSON.parse(await readFile(plan.files!.withdrawalInclusionPath, "utf8")),
    ).toEqual(plan.withdrawalInclusion);
  });

  it("builds a content-mismatch plan from an authenticated present-event witness", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    // The Aiken-measured roots of `mismatched_content_block_v1`.
    expect(fixture.withdrawalsPhasRoot).toBe(MM_WITHDRAWALS_PHAS_ROOT);
    expect(fixture.withdrawalsRoot).toBe(MM_WITHDRAWALS_ROOT);

    const plan = await prepareFabricatedWithdrawalFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedWithdrawalsRoot: fixture.withdrawalsRoot,
      withdrawalCount: fixture.withdrawalCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
      committedWithdrawalIdCbor: KEY_AUTHENTIC_WITHDRAWAL_ID,
    });

    expect(plan.classification.verdict).toEqual({
      WithdrawalEventObserved: {
        commitment: { ...historyCommitment, policy: WITHDRAWAL_POLICY_ID },
      },
    });
    expect(plan.classification.fault).toEqual({
      MismatchedWithdrawalContent: {
        committed_withdrawal_content_hash: HASH_DIVERTED_WITHDRAWAL_CONTENT,
        authentic_withdrawal_content_hash: HASH_AUTHENTIC_WITHDRAWAL_CONTENT,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(plan.challengedLeaf.committedWithdrawalContentHash).toBe(
      HASH_DIVERTED_WITHDRAWAL_CONTENT,
    );
    expect(plan.authenticContent.openingCbor).toBe(historyOpening());
  });

  it("refuses leaves that do not open the committed counted withdrawals_root, in the root or in the cardinality", async () => {
    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    // Root arm: the supplied leaf is not the one the header committed.
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedWithdrawalsRoot: fixture.withdrawalsRoot,
        withdrawalCount: fixture.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: [
          [KEY_FABRICATED_WITHDRAWAL_ID, VALUE_AUTHENTIC_WITHDRAWAL_INFO],
        ],
        witness: absentIdentityWitness(),
      }),
    ).rejects.toMatchObject({ code: "withdrawals_root_mismatch" });

    // Cardinality arm: the header's own `withdrawal_count` disagrees with the
    // rebuilt leaf count, which is the half of the counted-root check a
    // root-only comparison would miss.
    const lied = await buildWithdrawalsBlockFixture({
      leaves: [MM_LEAF],
      withdrawalCountOverride: 7n,
    });
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: lied.headerHash,
        committedWithdrawalsRoot: MM_WITHDRAWALS_ROOT,
        withdrawalCount: lied.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: lied.entries,
        witness: presentEventWitness(),
      }),
    ).rejects.toMatchObject({ code: "withdrawals_root_mismatch" });
  });

  it("refuses an empty withdrawal source set and a pinned leaf the header never committed", async () => {
    const empty = await buildWithdrawalsBlockFixture({ leaves: [] });
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: empty.headerHash,
        committedWithdrawalsRoot: empty.withdrawalsRoot,
        withdrawalCount: empty.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: [],
        witness: absentIdentityWitness(),
      }),
    ).rejects.toMatchObject({ code: "no_committed_withdrawal_leaf" });

    const fixture = await buildWithdrawalsBlockFixture({ leaves: [MM_LEAF] });
    await expect(
      prepareFabricatedWithdrawalFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedWithdrawalsRoot: fixture.withdrawalsRoot,
        withdrawalCount: fixture.withdrawalCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: absentIdentityWitness(),
        committedWithdrawalIdCbor: KEY_FABRICATED_WITHDRAWAL_ID,
      }),
    ).rejects.toMatchObject({ code: "leaf_not_committed" });
  });

  it("refuses a committed leaf whose bytes are not the serialise_data form the on-chain membership check recomputes", async () => {
    // Lucid's typed encoder writes the `l2_value` map in indefinite form, which is
    // not what `cbor.serialise` produces for the same typed leaf. A block that
    // committed those bytes commits a leaf no step can reproduce, so the family
    // refuses it rather than building a proof that cannot verify on chain.
    const rawLucidValue = Data.to(
      {
        ...authenticWithdrawalInfo(),
        body: {
          ...authenticWithdrawalInfo().body,
          l1_address: l1AddressOf(0x5d),
        },
      },
      SDK.WithdrawalInfo,
    );
    expect(rawLucidValue).not.toBe(VALUE_DIVERTED_WITHDRAWAL_INFO);
    const fixture = await buildWithdrawalsBlockFixture({
      leaves: [{ key: KEY_AUTHENTIC_WITHDRAWAL_ID, value: rawLucidValue }],
    });
    // The counted root is honest about the bytes the operator published, so the
    // refusal has to come from the leaf decoder, not from the root gate.
    expect(fixture.withdrawalsRoot).not.toBe(MM_WITHDRAWALS_ROOT);
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
    ).rejects.toMatchObject({ code: "non_canonical_da_payload" });
  });
});
