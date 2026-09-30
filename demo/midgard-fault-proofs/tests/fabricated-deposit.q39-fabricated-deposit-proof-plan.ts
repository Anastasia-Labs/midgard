import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  classifyFabricatedDepositFault,
  fabricatedDepositBlockEvidenceFromVerifiedPayload,
  FabricatedDepositRejection,
  prepareFabricatedDepositFromCommittedLeaves,
} from "../src/prepare-fabricated-deposit.js";
import {
  absentIdentityWitness,
  AUTHENTIC_INCLUSION_TIME,
  buildDepositsBlockFixture,
  DA_PROVENANCE,
  DEPOSIT_POLICY_ID,
  FABRICATED_DEPOSIT_ID,
  FI_DEPOSITS_PHAS_ROOT,
  FI_DEPOSITS_ROOT,
  FI_LEAF,
  HASH_AUTHENTIC_DEPOSIT_INFO,
  HASH_DIVERTED_DEPOSIT_INFO,
  HEADER_END_TIME,
  HEADER_START_TIME,
  KEY_AUTHENTIC_DEPOSIT_ID,
  KEY_FABRICATED_DEPOSIT_ID,
  l1Observation,
  MM_DEPOSITS_PHAS_ROOT,
  MM_DEPOSITS_ROOT,
  MM_LEAF,
  VALUE_AUTHENTIC_DEPOSIT_INFO,
} from "./fabricated-deposit.build-deposits-block-fixture.js";
import {
  depositEventDatum,
  historyCommitment,
  historyOpening,
  presentEventWitness,
} from "./fabricated-deposit.q39-fabricated-deposit-evidence-admission.js";
import { h28, h32 } from "./helpers/canonical-block-evidence-fixture.js";

describe("Q39 fabricated-deposit proof plan", () => {
  it("builds a nonexistent-identity plan from an authenticated absence witness", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [FI_LEAF] });
    // The Aiken-measured roots of `fabricated_identity_block_v1`.
    expect(fixture.depositsPhasRoot).toBe(FI_DEPOSITS_PHAS_ROOT);
    expect(fixture.depositsRoot).toBe(FI_DEPOSITS_ROOT);

    const evidence = await fabricatedDepositBlockEvidenceFromVerifiedPayload({
      observation: fixture.observation,
      payloadEnvelopeCbor: fixture.payloadEnvelopeCbor,
      daProvenance: DA_PROVENANCE,
    });
    const outputDir = await mkdtemp(join(tmpdir(), "q39-fabricated-deposit-"));
    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: evidence.headerHash,
      committedDepositsRoot: evidence.committedDepositsRoot,
      depositCount: evidence.depositCount,
      headerStartTime: evidence.headerStartTime,
      headerEndTime: evidence.headerEndTime,
      entries: evidence.entries,
      witness: absentIdentityWitness(),
      outputDir,
    });

    expect(plan.violationId).toBe("fabricated-deposit");
    expect(plan.fraudCategoryId).toBe("0000000b");
    expect(plan.threadTokenAssetName).toBe(`0000000b${fixture.headerHash}`);
    expect(plan.depositsPhasRoot).toBe(FI_DEPOSITS_PHAS_ROOT);
    expect(plan.committedDepositsRoot).toBe(FI_DEPOSITS_ROOT);
    expect(plan.classification.verdict).toBe("DepositIdentityAbsent");
    expect(plan.classification.fault).toBe("NonexistentDepositIdentity");
    expect(plan.step02State).toEqual({
      stateQueuePolicyId: h28(0x15),
      challengedHeaderHash: fixture.headerHash,
      headerStartTime: "10",
      headerEndTime: "20",
      committedDepositIdCbor: KEY_FABRICATED_DEPOSIT_ID,
      committedDepositInfoHash: HASH_AUTHENTIC_DEPOSIT_INFO,
    });
    // An absence proof has no retained content to open at step 03.
    expect(plan.authenticContent.openingCbor).toBeNull();
    expect(
      plan.depositInclusion.depositMembershipProofCbor.length,
    ).toBeGreaterThan(0);
    expect(plan.files).toBeDefined();
    expect(
      JSON.parse(await readFile(plan.files!.depositInclusionPath, "utf8")),
    ).toEqual(plan.depositInclusion);
  });

  it("builds a content-mismatch plan from an authenticated present-event witness", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    // The Aiken-measured roots of `mismatched_content_block_v1`.
    expect(fixture.depositsPhasRoot).toBe(MM_DEPOSITS_PHAS_ROOT);
    expect(fixture.depositsRoot).toBe(MM_DEPOSITS_ROOT);

    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedDepositsRoot: fixture.depositsRoot,
      depositCount: fixture.depositCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
      committedDepositIdCbor: KEY_AUTHENTIC_DEPOSIT_ID,
    });

    expect(plan.classification.verdict).toEqual({
      DepositEventObserved: {
        commitment: { ...historyCommitment, policy: DEPOSIT_POLICY_ID },
      },
    });
    expect(plan.classification.fault).toEqual({
      MismatchedDepositContent: {
        committed_deposit_info_hash: HASH_DIVERTED_DEPOSIT_INFO,
        authentic_deposit_info_hash: HASH_AUTHENTIC_DEPOSIT_INFO,
        event_inclusion_time: AUTHENTIC_INCLUSION_TIME,
      },
    });
    expect(plan.challengedLeaf.committedDepositInfoHash).toBe(
      HASH_DIVERTED_DEPOSIT_INFO,
    );
    expect(plan.authenticContent.openingCbor).toBe(historyOpening());
  });

  it("refuses leaves that do not open the committed counted deposits_root, in the root or in the cardinality", async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    // Root arm: the supplied leaf is not the one the header committed.
    await expect(
      prepareFabricatedDepositFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedDepositsRoot: fixture.depositsRoot,
        depositCount: fixture.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: [[KEY_FABRICATED_DEPOSIT_ID, VALUE_AUTHENTIC_DEPOSIT_INFO]],
        witness: absentIdentityWitness(),
      }),
    ).rejects.toMatchObject({ code: "deposits_root_mismatch" });

    // Cardinality arm: the header's own `deposit_count` disagrees with the
    // rebuilt leaf count, which is the half of the counted-root check a
    // root-only comparison would miss.
    const lied = await buildDepositsBlockFixture({
      leaves: [MM_LEAF],
      depositCountOverride: 7n,
    });
    await expect(
      prepareFabricatedDepositFromCommittedLeaves({
        headerHash: lied.headerHash,
        committedDepositsRoot: MM_DEPOSITS_ROOT,
        depositCount: lied.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: lied.entries,
        witness: presentEventWitness(),
      }),
    ).rejects.toMatchObject({ code: "deposits_root_mismatch" });
  });

  it("refuses an empty deposit source set and a pinned leaf the header never committed", async () => {
    const empty = await buildDepositsBlockFixture({ leaves: [] });
    await expect(
      prepareFabricatedDepositFromCommittedLeaves({
        headerHash: empty.headerHash,
        committedDepositsRoot: empty.depositsRoot,
        depositCount: empty.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: [],
        witness: absentIdentityWitness(),
      }),
    ).rejects.toMatchObject({ code: "no_committed_deposit_leaf" });

    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    await expect(
      prepareFabricatedDepositFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedDepositsRoot: fixture.depositsRoot,
        depositCount: fixture.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: absentIdentityWitness(),
        committedDepositIdCbor: KEY_FABRICATED_DEPOSIT_ID,
      }),
    ).rejects.toMatchObject({ code: "leaf_not_committed" });
  });
});

describe("Q39 fabricated-deposit L1 witness authentication", () => {
  const fiLeaf = async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [FI_LEAF] });
    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedDepositsRoot: fixture.depositsRoot,
      depositCount: fixture.depositCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: absentIdentityWitness(),
    });
    return plan.challengedLeaf;
  };

  const mmLeaf = async () => {
    const fixture = await buildDepositsBlockFixture({ leaves: [MM_LEAF] });
    const plan = await prepareFabricatedDepositFromCommittedLeaves({
      headerHash: fixture.headerHash,
      committedDepositsRoot: fixture.depositsRoot,
      depositCount: fixture.depositCount,
      headerStartTime: HEADER_START_TIME,
      headerEndTime: HEADER_END_TIME,
      entries: fixture.entries,
      witness: presentEventWitness(),
    });
    return plan.challengedLeaf;
  };

  it("refuses an absence claim without an authenticated history token, and any witness that is not authenticated L1 security-grade evidence", async () => {
    const leaf = await fiLeaf();
    // A gap-shaped datum without its list NFT cannot authenticate absence.
    await expect(
      classifyFabricatedDepositFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: absentIdentityWitness(false),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
    await expect(
      classifyFabricatedDepositFault({
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
    const leaf = await mmLeaf();
    // The observed asset name is not `out_ref_to_nonce(committed_deposit_id)`.
    await expect(
      classifyFabricatedDepositFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: presentEventWitness({ observedEventAssetName: h32(0x4d) }),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
    // The retained datum names a different deposit identity.
    await expect(
      classifyFabricatedDepositFault({
        leaf,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        witness: presentEventWitness({
          eventDatumCbor: Data.to(
            depositEventDatum({ id: FABRICATED_DEPOSIT_ID }),
            SDK.DepositDatum,
          ),
        }),
      }),
    ).rejects.toMatchObject({ code: "history_witness_invalid" });
  });

  it("refuses to challenge a header that committed exactly the authentic content", async () => {
    const fixture = await buildDepositsBlockFixture({
      leaves: [
        { key: KEY_AUTHENTIC_DEPOSIT_ID, value: VALUE_AUTHENTIC_DEPOSIT_INFO },
      ],
    });
    const attempt = async () =>
      await prepareFabricatedDepositFromCommittedLeaves({
        headerHash: fixture.headerHash,
        committedDepositsRoot: fixture.depositsRoot,
        depositCount: fixture.depositCount,
        headerStartTime: HEADER_START_TIME,
        headerEndTime: HEADER_END_TIME,
        entries: fixture.entries,
        witness: presentEventWitness(),
      });
    await expect(attempt()).rejects.toBeInstanceOf(FabricatedDepositRejection);
    await expect(attempt()).rejects.toMatchObject({
      code: "authentic_content_matches_commitment",
    });
  });

  it("proves an authentic event ineligible for the challenged block, on either side of the window", async () => {
    const leaf = await mmLeaf();
    for (const inclusionTime of [HEADER_START_TIME, HEADER_END_TIME + 1n]) {
      const datum = depositEventDatum({ inclusionTime });
      await expect(
        classifyFabricatedDepositFault({
          leaf,
          headerStartTime: HEADER_START_TIME,
          headerEndTime: HEADER_END_TIME,
          witness: presentEventWitness({
            eventDatumCbor: Data.to(datum, SDK.DepositDatum),
          }),
        }),
      ).resolves.toMatchObject({
        fault: {
          IneligibleDepositEvent: { event_inclusion_time: inclusionTime },
        },
      });
    }
  });
});
