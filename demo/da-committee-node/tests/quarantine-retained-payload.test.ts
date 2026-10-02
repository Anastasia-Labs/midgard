import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import {
  computeDaSha256Hash,
  decodeDaPayloadByHeaderResponseCbor,
  decodeDaPayloadSubmitResponseCbor,
  encodeDaPayloadByHeaderRequestCbor,
  encodeDaPayloadSubmitRequestCbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import { availabilityResponderOperations } from "../src/availability/factory.js";
import { AvailabilityResponder } from "../src/availability/responder.js";
import { retainedAvailabilityPayload } from "../src/availability/retained-payload.js";
import { assertAvailabilityResponderSourceHealthy } from "../src/availability/source-authority.js";
import { SubmitterReconciler } from "../src/coordinator/submitter-reconciler.js";
import { StoreBackedDaAttestationProtocol } from "../src/da/libp2p/attestations.js";
import { DaLibp2pPayloadProtocolHandlers } from "../src/da/libp2p/payload-protocols.js";
import { type CommitteeStore, JsonFileCommitteeStore } from "../src/store.js";
import { PostgresCommitteeStore } from "../src/store/postgres.js";
import { tempDir } from "./helpers.js";
import {
  challengeFixture,
  commitment,
  deploymentFingerprint,
  deploymentIdentity,
  payload,
} from "./helpers/availability-challenge.js";
import { postgresTestDatabases } from "./helpers/postgres-database.js";
import {
  divergentBytes,
  divergentHeader,
  l1Source,
  localSignature,
  network,
  quarantinedStore,
  responderConfig,
  signedHeader,
  sourceState,
  verified,
} from "./helpers/quarantined-committee-store.js";

const databases = postgresTestDatabases("fx_d3");
const openStores = new Set<CommitteeStore>();

afterEach(async () => {
  await Promise.all([...openStores].map(async (store) => store.close?.()));
  openStores.clear();
});

afterAll(async () => {
  await databases.dropAll();
});

const openJson = async (): Promise<CommitteeStore> => {
  const store = await JsonFileCommitteeStore.open(await tempDir());
  openStores.add(store);
  return store;
};

const openPostgres = async (): Promise<CommitteeStore> => {
  const store = await PostgresCommitteeStore.open(
    (await databases.create()).url,
  );
  openStores.add(store);
  return store;
};

const retained = (
  store: CommitteeStore,
  frozen: SDK.DaAvailabilityCommitment = commitment,
  identity = deploymentIdentity,
) =>
  retainedAvailabilityPayload({
    store,
    deploymentFingerprint,
    deploymentIdentity: identity,
    commitment: frozen,
  });

const byHeader = (headerHash: string) =>
  encodeDaPayloadByHeaderRequestCbor({
    deploymentFingerprint: Buffer.from(deploymentFingerprint, "hex"),
    headerHash: Buffer.from(headerHash, "hex"),
    acceptedPayloadHashes: null,
    maxInlineBytes: 1_000_000,
  });

describe.each([
  ["JSON", openJson],
  ["Postgres", openPostgres],
] as const)("%s store: a quarantined member's existing promises", (_, open) => {
  it("keeps the exact verified bytes it signed retrievable and answerable", async () => {
    const store = await quarantinedStore(open);
    await expect(store.getL1SourceState()).resolves.toMatchObject({
      status: "quarantined",
    });
    // Quarantine stops decisions about the header ...
    await expect(
      store.getStateQueueHeader(signedHeader),
    ).resolves.toMatchObject({
      status: "conflicted",
      validationErrors: [expect.stringContaining("l1_source_quarantined")],
    });
    // ... but the bytes and their integrity status are untouched.
    await expect(store.getDaPayload(signedHeader)).resolves.toMatchObject({
      payloadCborHex: verified.payloadCborHex,
      payloadSha256: verified.payloadSha256,
      validationStatus: "verified",
    });
    expect(await retained(store)).toEqual(Buffer.from(payload));

    // Peers, watchers and the public retained-DA listener read through these
    // handlers.
    const handlers = new DaLibp2pPayloadProtocolHandlers({
      deploymentFingerprint,
      store,
    });
    const served = decodeDaPayloadByHeaderResponseCbor(
      await handlers.handlePayloadByHeader(byHeader(signedHeader)),
    );
    expect(served.status).toBe("found_inline");
    expect(served.payloadBytes?.equals(Buffer.from(payload))).toBe(true);

    // The responder core answers from those bytes. This responder has an
    // ungated `discover`: in production `discover` first runs
    // `assertAvailabilityResponderSourceHealthy` on this same store, which
    // refuses while the source is quarantined (see the "refuses a response
    // transaction" test below), so a quarantined member's own in-process
    // responder does not submit this answer while the quarantine holds.
    const executed: string[] = [];
    const responder = new AvailabilityResponder({
      deploymentFingerprint,
      deploymentIdentity,
      store,
      discover: async () => [challengeFixture().challenge],
      reconcile: async () => "ready",
      now: () => 2_000,
      execute: async (action) => {
        executed.push(action.kind);
        return "confirmed";
      },
    });
    await expect(responder.tick()).resolves.toMatchObject({
      action: "publish",
      status: "confirmed",
      headerHash: signedHeader,
    });
    expect(executed).toEqual(["publish"]);
  });

  it("never lets a different envelope replace the promised bytes", async () => {
    const inner = Buffer.from([1, 2, 3, 4]);
    const promised = await wrapDaPayload(inner, { mode: "identity" });
    const fixture = challengeFixture(promised);
    const store = await quarantinedStore(open, fixture.stored);
    const frozen = fixture.challenge.record.datum.commitment;
    expect(await retained(store, frozen)).toEqual(promised);
    const handlers = new DaLibp2pPayloadProtocolHandlers({
      deploymentFingerprint,
      store,
    });
    // The same content in another envelope: the D1 case a later candidate
    // must not win.
    const other = await wrapDaPayload(inner, { mode: "zstd" });
    expect(other.equals(promised)).toBe(false);
    const submitted = decodeDaPayloadSubmitResponseCbor(
      await handlers.handlePayloadSubmit(
        encodeDaPayloadSubmitRequestCbor({
          deploymentFingerprint: Buffer.from(deploymentFingerprint, "hex"),
          headerHash: Buffer.from(signedHeader, "hex"),
          payloadHash: computeDaSha256Hash(other),
          payloadSchemaVersion: 1,
          mode: "inline",
          payloadBytes: other,
          chunkManifest: null,
        }),
      ),
    );
    expect(submitted.status).toBe("conflict");
    expect(await retained(store, frozen)).toEqual(promised);
  });

  it("never serves bytes whose hash or identity does not match what was signed", async () => {
    const store = await quarantinedStore(open);
    // Divergent bytes stay refused through quarantine.
    await expect(store.getDaPayload(divergentHeader)).resolves.toMatchObject({
      validationStatus: "conflicted",
      conflictStatus: "conflicting_bytes",
    });
    const divergentCommitment = SDK.buildDaAvailabilityCommitment({
      deploymentIdentity,
      headerHash: divergentHeader,
      payload: divergentBytes,
      responseGeometry: commitment.response_geometry,
    });
    await expect(retained(store, divergentCommitment)).rejects.toThrow(
      /verified retained payload/u,
    );
    // Bytes that match the commitment but were never verified stay refused:
    // quarantine leaves their status as it was.
    for (const validationStatus of ["fetched", "root_mismatch"] as const) {
      const unverified = await quarantinedStore(open, {
        ...verified,
        validationStatus,
        conflictStatus: "none",
      });
      await expect(
        unverified.getDaPayload(signedHeader),
      ).resolves.toMatchObject({ validationStatus, conflictStatus: "none" });
      await expect(retained(unverified)).rejects.toThrow(
        /verified retained payload/u,
      );
    }
    // A commitment over other bytes for the signed header.
    const otherCommitment = SDK.buildDaAvailabilityCommitment({
      deploymentIdentity,
      headerHash: signedHeader,
      payload: Uint8Array.from([1, 2, 3, 5]),
      responseGeometry: commitment.response_geometry,
    });
    await expect(retained(store, otherCommitment)).rejects.toThrow(
      /frozen signed commitment/u,
    );
    // Another deployment's commitment.
    await expect(
      retained(
        store,
        { ...commitment, deployment_identity: "77".repeat(28) },
        deploymentIdentity,
      ),
    ).rejects.toThrow(/another deployment/u);
    // Bytes that no longer match their stored digest.
    const corrupt = {
      getDaPayload: async (headerHash: string) => {
        const record = await store.getDaPayload(headerHash);
        return record && { ...record, payloadCborHex: "01020305" };
      },
    };
    await expect(
      retainedAvailabilityPayload({
        store: corrupt,
        deploymentFingerprint,
        deploymentIdentity,
        commitment,
      }),
    ).rejects.toThrow(/stored digest/u);
    // Another deployment's retained record.
    await expect(
      retainedAvailabilityPayload({
        store,
        deploymentFingerprint: "55".repeat(32),
        deploymentIdentity,
        commitment,
      }),
    ).rejects.toThrow(/this deployment/u);
  });

  it("makes no new signature or attestation application", async () => {
    const store = await quarantinedStore(open);
    // The signature made before quarantine is kept, but no longer broadcast.
    await expect(store.listDaSignatures(signedHeader)).resolves.toEqual([
      expect.objectContaining({
        signerIndex: 0,
        signatureWitness: localSignature().signatureWitness,
        broadcastStatus: "post_failed",
      }),
    ]);
    // No new signature is persisted, local or from a peer.
    await expect(store.saveDaSignature(localSignature(1))).rejects.toThrow(
      /cannot persist a DA signature/u,
    );
    const attestations = new StoreBackedDaAttestationProtocol({
      deploymentFingerprint,
      localPeerId: "local-peer",
      committeeValidation: {} as never,
      availabilityCommitmentAuthority: {} as never,
      store,
    });
    await expect(
      attestations.acceptAttestation({
        record: { ...localSignature(1), source: "peer" },
        sourcePeerId: "remote-peer",
      }),
    ).resolves.toEqual({
      status: "rejected",
      reason: "L1 source is quarantined",
    });
    await expect(store.listDaSignatures(signedHeader)).resolves.toHaveLength(1);
    const reconcileAttestation = vi.fn(async () => "posted" as const);
    const reconciler = new SubmitterReconciler({
      deploymentFingerprint,
      committeeValidation: {} as never,
      availabilityCommitmentAuthority: {} as never,
      store,
      coordinator: { reconcileAttestation },
    });
    const header = await store.getStateQueueHeader(signedHeader);
    await expect(reconciler.reconcileHeader(header!)).resolves.toMatchObject({
      status: "skipped",
    });
    expect(reconcileAttestation).not.toHaveBeenCalled();
  });

  it("refuses a response transaction while its chain authority is quarantined", async () => {
    const store = await quarantinedStore(open);
    await expect(
      assertAvailabilityResponderSourceHealthy(store, responderConfig),
    ).rejects.toThrow(/healthy authenticated committee node L1 source/u);
    const point = {
      network,
      slot: 10,
      blockHash: "12".repeat(32),
      providerSource: "fixture",
      observedAt: "2026-10-01T00:00:00.000Z",
    };
    const { assertActuationCurrent } = availabilityResponderOperations({
      lucid: {} as never,
      readers: {
        currentPoint: async () => point,
        currentCursor: async () => ({
          sequence: 0,
          point,
          rollbackGeneration: 0,
        }),
        tipBlockNo: async () => 5,
        resolveInclusion: async () => ({}),
        foreignSpend: {} as never,
      },
      assertSourceHealthy: () =>
        assertAvailabilityResponderSourceHealthy(store, responderConfig),
      context: {} as never,
    });
    const execute = vi.fn(async () => "confirmed" as const);
    const responder = new AvailabilityResponder({
      deploymentFingerprint,
      deploymentIdentity,
      store,
      reconcile: async () => "ready",
      discover: async () => {
        await assertActuationCurrent();
        return [challengeFixture().challenge];
      },
      now: () => 2_000,
      execute,
    });
    await expect(responder.tick()).rejects.toThrow(
      /healthy authenticated committee node L1 source/u,
    );
    expect(execute).not.toHaveBeenCalled();
  });

  it("refuses a response transaction while its journal authority is unresolved", async () => {
    const store = await quarantinedStore(open);
    const execute = vi.fn(async () => "confirmed" as const);
    const withJournal = (reconcile: () => Promise<"ready" | "pending">) =>
      new AvailabilityResponder({
        deploymentFingerprint,
        deploymentIdentity,
        store,
        reconcile,
        discover: async () => [challengeFixture().challenge],
        now: () => 2_000,
        execute,
      });
    await expect(
      withJournal(async () => "pending").tick(),
    ).resolves.toMatchObject({ status: "pending" });
    await expect(
      withJournal(async () => {
        throw new Error(
          "Availability responder journal contains a conflicting transaction; authenticated recovery is required",
        );
      }).tick(),
    ).rejects.toThrow(/conflicting transaction/u);
    expect(execute).not.toHaveBeenCalled();
  });
});

describe("availability responder chain authority", () => {
  it("accepts only a healthy source bound to the configured authority", async () => {
    const store = await openJson();
    await expect(
      assertAvailabilityResponderSourceHealthy(store, responderConfig),
    ).rejects.toThrow(/healthy authenticated/u);
    await store.saveL1SourceState(sourceState("healthy", []));
    await expect(
      assertAvailabilityResponderSourceHealthy(store, responderConfig),
    ).resolves.toBeUndefined();
    await expect(
      assertAvailabilityResponderSourceHealthy(store, {
        network,
        l1Source: { ...l1Source, authorityNodeId: "node-b" },
      }),
    ).rejects.toThrow(/healthy authenticated/u);
    // A healthy source on another network, or not from the local node, is
    // not this responder's chain authority. Each keeps the configured
    // authority digest, so only the clause under test can refuse it.
    for (const other of [
      { network: "Mainnet" },
      { sourceMode: "external_providers" as const },
    ]) {
      const unbound = await openJson();
      await unbound.saveL1SourceState({
        ...sourceState("healthy", []),
        ...other,
      });
      await expect(
        assertAvailabilityResponderSourceHealthy(unbound, responderConfig),
      ).rejects.toThrow(/healthy authenticated/u);
    }
  });
});
