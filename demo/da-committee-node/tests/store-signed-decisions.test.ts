import { Client } from "pg";
import { describe, expect, it } from "vitest";

import { loadDaSigner, signDaAttestation } from "../src/signer.js";
import { type L1SourceState } from "../src/store.js";
import {
  closeTestCommitteeStore,
  openTestCommitteeStore,
  testStoreDatabase,
} from "./helpers/committee-store.js";
import {
  commitmentFor,
  signatureRecord,
} from "./peer-coordinator.signature-record.js";

const sourceState: L1SourceState = {
  schemaVersion: 1,
  sourceMode: "local_node",
  network: "Preprod",
  authoritySha256: "94".repeat(32),
  status: "healthy",
  observations: [],
  observedAt: "2026-10-07T00:00:00.000Z",
};

const signed = async (args: {
  readonly headerHash: string;
  readonly signerIndex: number;
  readonly endTime: string;
  readonly source: "local" | "peer";
  readonly payloadCborHex?: string;
}) => {
  const signer = await loadDaSigner(
    `hex:${"00".repeat(31)}${(args.signerIndex + 1).toString(16).padStart(2, "0")}`,
  );
  const commitment = commitmentFor(args.headerHash, args.payloadCborHex);
  const record = signatureRecord({
    deploymentFingerprint: "dep",
    headerHash: args.headerHash,
    signerIndex: args.signerIndex,
    committeeSignersHash: "77".repeat(32),
    commitment,
    signatureWitness: signDaAttestation({
      signer,
      signerIndex: args.signerIndex,
      availabilityCommitment: commitment.commitment,
    }),
  });
  return {
    ...record,
    source: args.source,
    broadcastStatus: args.source === "local" ? "local" : "posted",
    validation: {
      ...record.validation,
      l1Header: { ...record.validation.l1Header, endTime: args.endTime },
    },
  } as const;
};

describe("signed decisions (class B) for the obligations projection", () => {
  it("records each header this member signed with its end time and ignores peer signatures", async () => {
    const first = await openTestCommitteeStore();
    await first.saveL1SourceState(sourceState);
    const localHeader = "a1".repeat(28);
    const peerHeader = "b2".repeat(28);
    await first.saveDaSignature(
      await signed({
        headerHash: localHeader,
        signerIndex: 0,
        endTime: "1760000000123",
        source: "local",
      }),
    );
    await first.saveDaSignature(
      await signed({
        headerHash: peerHeader,
        signerIndex: 1,
        endTime: "1760000000456",
        source: "peer",
      }),
    );
    // A second local signature of the same header (another payload) is one
    // more signature row, but still one signed decision: the header hash
    // binds the header, so its end time cannot differ.
    await first.saveDaSignature(
      await signed({
        headerHash: localHeader,
        signerIndex: 0,
        endTime: "1760000000123",
        source: "local",
        payloadCborHex: "ccdd",
      }),
    );
    await expect(first.listSignedDecisions()).resolves.toEqual([
      { headerHash: localHeader, endTimeMs: 1760000000123n },
    ]);
  });

  it("refuses a local signature whose header end time is not a canonical decimal integer, writing no signature", async () => {
    const store = await openTestCommitteeStore();
    await store.saveL1SourceState(sourceState);
    const headerHash = "c3".repeat(28);
    await expect(
      store.saveDaSignature(
        await signed({
          headerHash,
          signerIndex: 0,
          endTime: "0x10",
          source: "local",
        }),
      ),
    ).rejects.toThrow();
    await expect(store.listSignedDecisions()).resolves.toEqual([]);
    await expect(store.listDaSignatures(headerHash)).resolves.toEqual([]);
  });

  it("backfills the end time of a signature stored before the column existed when the store reopens", async () => {
    const database = await testStoreDatabase();
    const before = await openTestCommitteeStore(database);
    await before.saveL1SourceState(sourceState);
    const headerHash = "d4".repeat(28);
    await before.saveDaSignature(
      await signed({
        headerHash,
        signerIndex: 0,
        endTime: "1760000000789",
        source: "local",
      }),
    );
    await closeTestCommitteeStore(before);
    // A store written before E6: the column holds nothing.
    const client = new Client({ connectionString: database.url });
    await client.connect();
    try {
      await client.query(
        "UPDATE committee_da_signatures SET end_time_ms = NULL",
      );
    } finally {
      await client.end();
    }
    const after = await openTestCommitteeStore(database);
    await expect(after.listSignedDecisions()).resolves.toEqual([
      { headerHash, endTimeMs: 1760000000789n },
    ]);
  });
});
