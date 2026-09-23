/** Production preparation/journal boundaries over real history policies.
 * Hub issuance and the challenged header are explicit fixtures. */
import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { afterAll, expect, it, vi } from "vitest";

import { type CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import {
  authenticateFabricatedHistoryWitness,
  fetchFabricatedHistoryWitness,
} from "../src/fabricated-history-witness.js";
import { prepareFabricatedDepositFromCommittedLeaves } from "../src/prepare-fabricated-deposit.js";
import { prepareFabricatedWithdrawalFromCommittedLeaves } from "../src/prepare-fabricated-withdrawal.js";
import { deriveFabricatedDepositStep03Handoff } from "../src/submit-fabricated-deposit-step-03.js";
import { deriveFabricatedWithdrawalStep03Handoff } from "../src/submit-fabricated-withdrawal-step-03.js";
import { buildCountedRoot } from "../src/transition-trace/phas.js";
import { createFabricatedDepositEvidenceAuthority } from "../src/workflow/fabricated-deposit-evidence.js";
import { createFabricatedWithdrawalEvidenceAuthority } from "../src/workflow/fabricated-withdrawal-evidence.js";
import { realBlueprintPath } from "./support/emulator/blueprints.js";
import {
  historyPairPayloads,
  insertHistoryFillerAfter,
  promoteHistoryPair,
  setupHistoryPair,
} from "./support/emulator/history-pair.js";
import { EMULATOR_PROTOCOL_PARAMETERS } from "./support/emulator/protocol-parameters.js";

const blueprintBytes = readFileSync(realBlueprintPath);
const blueprint = SDK.parseFaultProofBlueprint(
  JSON.parse(blueprintBytes.toString()),
);
const records: unknown[] = [];
type Kind = "Deposit" | "Withdrawal";
type Harness = Awaited<ReturnType<typeof setupHistoryPair>>;
const which = (kind: Kind) => (kind === "Deposit" ? 0 : 1);
const environment = (h: Harness, kind: Kind) => ({
  retentionAddress: h.applied[which(kind)]!.retention.address,
  inlineLimitBytes: 512n,
  maxPayloadBytes: 5000n,
  maxPayloadNodes: 512n,
});
const observation = (): SDK.AuthenticatedL1Observation => ({
  schemaVersion: SDK.CANONICAL_EVIDENCE_SOURCE_SCHEMA_VERSION,
  sourceMode: "local_node",
  provenance: {
    trustClass: "authenticated_cardano_l1",
    sourceId: "emulator-fixture",
    grade: "security",
  },
  chainPoint: { slot: 4242n, blockHash: "11".repeat(32) },
  confirmationDepth: 12,
});
const read = (h: Harness, kind: Kind, id: SDK.OutputReference) =>
  fetchFabricatedHistoryWitness({
    lucid: h.lucid,
    network: "Custom",
    hubOraclePolicyId: h.hubPolicy,
    history: environment(h, kind),
    observation: observation(),
    kind,
    id,
  });
const authority = (h: Harness, kind: Kind) =>
  (kind === "Deposit"
    ? createFabricatedDepositEvidenceAuthority
    : createFabricatedWithdrawalEvidenceAuthority)({
    lucid: h.lucid,
    network: "Custom",
    hubOraclePolicyId: h.hubPolicy,
    history: environment(h, kind),
    minimumConfirmationDepth: 1,
  });

const fixture = async (
  h: Harness,
  kind: Kind,
  payload: SDK.EventHistoryPayload,
  id: SDK.OutputReference,
  headerEnd: bigint,
) => {
  const keyCbor = Data.to(id, SDK.OutputReference);
  const valueCbor =
    "DepositPayload" in payload
      ? SDK.committedDepositValueBytes(payload.DepositPayload.event.info)
      : SDK.committedWithdrawalValueBytes(payload.WithdrawalPayload.event.info);
  const entry = {
    key: Buffer.from(keyCbor, "hex"),
    value: Buffer.from(valueCbor, "hex"),
  };
  const counted = await buildCountedRoot(
    kind === "Deposit"
      ? SDK.ROOT_DOMAINS.deposits
      : SDK.ROOT_DOMAINS.withdrawals,
    [entry],
  );
  const common = {
    headerHash: "22".repeat(28),
    headerStartTime: headerEnd - 100_000n,
    headerEndTime: headerEnd,
    entries: [[keyCbor, valueCbor]] as const,
    witness: await read(h, kind, id),
  };
  const prepare = () =>
    kind === "Deposit"
      ? prepareFabricatedDepositFromCommittedLeaves({
          ...common,
          depositCount: 1n,
          committedDepositsRoot: counted.root,
        })
      : prepareFabricatedWithdrawalFromCommittedLeaves({
          ...common,
          withdrawalCount: 1n,
          committedWithdrawalsRoot: counted.root,
        });
  // Only the fields the preparation boundary consumes are fixture-provided.
  // Header/DA admission remains a separate tested authority boundary.
  const evidence = {
    headerHash: common.headerHash,
    observation: common.witness.observation,
    header: {
      startTime: common.headerStartTime,
      endTime: headerEnd,
      depositCount: kind === "Deposit" ? 1n : 0n,
      withdrawalCount: kind === "Withdrawal" ? 1n : 0n,
      depositsRoot: counted.root,
      withdrawalsRoot: counted.root,
    },
    reconstruction: {
      deposits:
        kind === "Deposit"
          ? [
              {
                key: id,
                value: Data.from(valueCbor, SDK.DepositInfo),
                keyBytes: entry.key,
                valueBytes: entry.value,
              },
            ]
          : [],
      withdrawals:
        kind === "Withdrawal"
          ? [
              {
                key: id,
                value: Data.from(valueCbor, SDK.WithdrawalInfo),
                keyBytes: entry.key,
                valueBytes: entry.value,
              },
            ]
          : [],
    },
  } as unknown as CanonicalBlockEvidence;
  return { ...common, prepare, evidence };
};

afterAll(() => {
  const dir = join(process.cwd(), "../../artifacts/event-history");
  mkdirSync(dir, { recursive: true });
  writeFileSync(
    join(dir, "history-preparation-applied.json"),
    JSON.stringify(
      {
        scope:
          "Production preparation and retained artifact admission over actual list policies; fixture hub/header, not whole-family acceptance",
        blueprintSha256: createHash("sha256")
          .update(blueprintBytes)
          .digest("hex"),
        protocolParameters: EMULATOR_PROTOCOL_PARAMETERS,
        records,
      },
      (_, v) => (typeof v === "bigint" ? v.toString() : v),
      2,
    ) + "\n",
  );
});

for (const kind of ["Deposit", "Withdrawal"] as const) {
  it(`${kind}: prepares arbitrary nonexistent IDs without querying nonce UTxOs`, async () => {
    const h = await setupHistoryPair({ blueprint, records });
    const payloads = await promoteHistoryPair(h);
    const id = { transactionId: "ff".repeat(32), outputIndex: 999_999n };
    const nonceLookup = vi
      .spyOn(h.lucid, "utxosByOutRef")
      .mockRejectedValue(new Error("Legacy nonce lookup is forbidden"));
    try {
      const f = await fixture(
        h,
        kind,
        payloads[which(kind)]!,
        id,
        BigInt(h.emulator.now()) - 1000n,
      );
      const plan = await f.prepare();
      expect(plan.classification.fault).toBe(`Nonexistent${kind}Identity`);
      expect(plan.authenticContent.openingCbor).toBeNull();
      expect(plan.step02State.stateQueuePolicyId).toBe(h.hubPolicy);
      expect(nonceLookup).not.toHaveBeenCalled();
      const a = authority(h, kind);
      const artifact = await a.prepare(f.evidence, h.owner, 0);
      expect(artifact.l1Evidence.kind).toBe("absent_identity");
      expect(await a.readmit(JSON.parse(JSON.stringify(artifact)))).toEqual(
        artifact,
      );
      await expect(
        a.readmit({ ...artifact, owner: "33".repeat(28) }),
      ).rejects.toThrow(/digest mismatch/);
    } finally {
      nonceLookup.mockRestore();
    }
  });

  it(`${kind}: persists original Value and reopens after pointer churn without history access`, async () => {
    const h = await setupHistoryPair({ blueprint, records });
    const payloads = await promoteHistoryPair(h);
    const n = h.eventNonces[which(kind)]!;
    const id = { transactionId: n.txHash, outputIndex: BigInt(n.outputIndex) };
    const w = await read(h, kind, id);
    const authenticated = await authenticateFabricatedHistoryWitness(
      w,
      kind,
      id,
    );
    if (!authenticated.captured || authenticated.witness.kind !== "Present")
      throw new Error("Expected genuine Order");
    const time = authenticated.captured.commitment.inclusion_time;
    const f = await fixture(h, kind, payloads[which(kind)]!, id, time - 1n);
    const plan = await f.prepare();
    expect(plan.classification.fault).toEqual({
      [`Ineligible${kind}Event`]: { event_inclusion_time: time },
    });
    const decoded = Data.from(
      plan.authenticContent.openingCbor!,
      SDK.FabricatedDepositAuthenticContentOpening,
    );
    if (decoded === "NoAuthenticContent")
      throw new Error("Missing retained opening");
    expect(decoded.RetainedEventData.original_assets).toEqual(
      authenticated.captured.originalAssets,
    );
    expect(decoded.RetainedEventData.original_assets.get("")!.get("")).toBe(
      20_000_000n,
    );
    const a = authority(h, kind);
    const artifact = await a.prepare(f.evidence, h.owner, 0);
    const journal = JSON.parse(JSON.stringify(artifact));
    const node = authenticated.witness.anchor.node;
    const nextKey = (
      (BigInt("0x" + authenticated.witness.anchor.key!) +
        BigInt("0x" + (node.next ?? "ff".repeat(32)))) /
      2n
    )
      .toString(16)
      .padStart(64, "0");
    await h.submit(
      `${kind}-preparation-pointer-churn`,
      await insertHistoryFillerAfter(
        h,
        kind,
        authenticated.witness,
        nextKey,
        [],
      ),
    );
    expect(await h.lucid.utxosByOutRef([w.anchor])).toHaveLength(0);
    expect(await a.readmit(journal)).toEqual(artifact);
    const inaccessible = vi
      .spyOn(h.lucid, "utxosAt")
      .mockRejectedValue(
        new Error("History retrieval unavailable after capture"),
      );
    try {
      const reopened = a.readmitRetained(journal);
      expect(reopened).toEqual(artifact);
      expect(inaccessible).not.toHaveBeenCalled();
      const common = {
        state_queue_policy: h.hubPolicy,
        challenged_header_hash: f.headerHash,
        header_start_time: f.headerStartTime,
        header_end_time: f.headerEndTime,
      };
      const handoff =
        kind === "Deposit"
          ? await deriveFabricatedDepositStep03Handoff({
              state: {
                ...common,
                committed_deposit_id: id,
                committed_deposit_info_hash: "00".repeat(32),
                verdict: {
                  DepositEventObserved: {
                    commitment: authenticated.captured.commitment,
                  },
                },
              },
              openingCbor: reopened.authenticContent.openingCbor!,
            })
          : await deriveFabricatedWithdrawalStep03Handoff({
              state: {
                ...common,
                committed_withdrawal_id: id,
                committed_withdrawal_content_hash: "00".repeat(32),
                verdict: {
                  WithdrawalEventObserved: {
                    commitment: authenticated.captured.commitment,
                  },
                },
              },
              openingCbor: reopened.authenticContent.openingCbor!,
            });
      expect(handoff.fault).toEqual(plan.classification.fault);
      await expect(a.readmit(journal)).rejects.toThrow(/unavailable/);
    } finally {
      inaccessible.mockRestore();
    }
    const honest = await fixture(h, kind, payloads[which(kind)]!, id, time);
    await expect(honest.prepare()).rejects.toMatchObject({
      code: "authentic_content_matches_commitment",
    });
  });

  it(`${kind}: refuses wrong hub/list authentication and records semantic changes before capture`, async () => {
    const h = await setupHistoryPair({ blueprint, records });
    const n = h.eventNonces[which(kind)]!;
    const id = { transactionId: n.txHash, outputIndex: BigInt(n.outputIndex) };
    const before = await read(h, kind, id);
    expect(
      (await authenticateFabricatedHistoryWitness(before, kind, id)).witness
        .kind,
    ).toBe("Absent");
    await expect(
      authenticateFabricatedHistoryWitness(
        {
          ...before,
          hubOracleUtxo: {
            ...before.hubOracleUtxo,
            assets: { lovelace: 10_000_000n },
          },
        },
        kind,
        id,
      ),
    ).rejects.toThrow(/hub oracle/);
    await expect(
      authenticateFabricatedHistoryWitness(
        {
          ...before,
          anchor: { ...before.anchor, assets: { lovelace: 3_000_000n } },
        },
        kind,
        id,
      ),
    ).rejects.toThrow(/no unique/);
    const beforeFixture = await fixture(
      h,
      kind,
      historyPairPayloads(h)[which(kind)]!,
      id,
      BigInt(h.emulator.now()) - 1000n,
    );
    const a = authority(h, kind);
    const beforeArtifact = await a.prepare(beforeFixture.evidence, h.owner, 0);
    expect(beforeArtifact.l1Evidence.kind).toBe("absent_identity");
    const payloads = await promoteHistoryPair(h);
    await expect(
      a.readmit(JSON.parse(JSON.stringify(beforeArtifact))),
    ).rejects.toThrow(/facts changed before capture/);
    const present = await read(h, kind, id);
    expect(
      (await authenticateFabricatedHistoryWitness(present, kind, id)).witness
        .kind,
    ).toBe("Present");
    const f = await fixture(
      h,
      kind,
      payloads[which(kind)]!,
      id,
      BigInt(h.emulator.now()) - 1n,
    );
    await expect(
      authenticateFabricatedHistoryWitness(
        { ...present, history: { ...present.history, maxPayloadNodes: 1n } },
        kind,
        id,
      ),
    ).rejects.toThrow(/Data-node bound/);
    await expect(
      authenticateFabricatedHistoryWitness(
        { ...present, history: { ...present.history, inlineLimitBytes: 0n } },
        kind,
        id,
      ),
    ).rejects.toThrow(/explicit payload bounds/);
    const wrongKeyNode = Data.from(present.anchor.datum!, SDK.EventHistoryNode);
    await expect(
      authenticateFabricatedHistoryWitness(
        {
          ...present,
          anchor: {
            ...present.anchor,
            datum: Data.to(
              { ...wrongKeyNode, position: { Key: ["ff".repeat(32)] } },
              SDK.EventHistoryNode,
            ),
          },
        },
        kind,
        id,
      ),
    ).rejects.toThrow(/complete key/);
    expect(f.witness.anchor.txHash).not.toBe(before.anchor.txHash);
  });
}
