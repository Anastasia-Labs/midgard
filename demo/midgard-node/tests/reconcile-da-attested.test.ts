import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  type CanonicalDaAttestationObservation,
  classifyCanonicalDaAttestation,
  reconcileDaAttestedProgram,
} from "@/commands/reconcile.js";
import { Lucid, MidgardContracts } from "@/services/index.js";

vi.mock("@al-ft/midgard-sdk", async () => {
  const actual =
    await vi.importActual<typeof import("@al-ft/midgard-sdk")>(
      "@al-ft/midgard-sdk",
    );
  return {
    ...actual,
    fetchSortedStateQueueUTxOsProgram: vi.fn(),
    getStateQueueNodeV1FromStateQueueDatum: vi.fn(),
    hashBlockHeaderV1: vi.fn(),
  };
});

const HEADER_HASH = "11".repeat(28);
const DA_ATTESTATION_POLICY_ID = "22".repeat(28);

const observation = (
  override: Partial<CanonicalDaAttestationObservation> = {},
): CanonicalDaAttestationObservation => ({
  datumHeaderHash: HEADER_HASH,
  computedHeaderHash: HEADER_HASH,
  daAttestation: DA_ATTESTATION_POLICY_ID,
  outRef: `${"33".repeat(32)}#0`,
  ...override,
});

const classify = ({
  localPayloadPresent = true,
  observations = [observation()],
}: {
  readonly localPayloadPresent?: boolean;
  readonly observations?: readonly CanonicalDaAttestationObservation[];
} = {}) =>
  classifyCanonicalDaAttestation({
    headerHash: HEADER_HASH,
    expectedDaAttestationPolicyId: DA_ATTESTATION_POLICY_ID,
    localPayloadPresent,
    observations,
  });

describe("DA-attested reconciliation", () => {
  it("accepts the exact canonical state-queue marker without watcher trust", () => {
    expect(
      classify({
        localPayloadPresent: false,
      }),
    ).toEqual({
      status: "satisfied",
      reason: "attestation_applied",
      nextAction: null,
    });
  });

  it("reports a canonical unattested header with its payload as pending", () => {
    expect(
      classify({
        observations: [observation({ daAttestation: "" })],
      }),
    ).toMatchObject({
      status: "pending",
      reason: "attestation_pending",
    });
  });

  it("blocks an unattested header when the exact payload is unavailable", () => {
    expect(
      classify({
        localPayloadPresent: false,
        observations: [observation({ daAttestation: "" })],
      }),
    ).toMatchObject({
      status: "blocked",
      reason: "local_payload_missing",
    });
  });

  it("fails closed on a foreign DA-attestation policy marker", () => {
    expect(
      classify({
        observations: [
          observation({
            daAttestation: "44".repeat(28),
          }),
        ],
      }),
    ).toMatchObject({
      status: "blocked",
      reason: "unexpected_attestation_marker",
    });
  });

  it("fails closed when the datum key differs from the recomputed header", () => {
    expect(
      classify({
        observations: [
          observation({
            computedHeaderHash: "55".repeat(28),
          }),
        ],
      }),
    ).toMatchObject({
      status: "blocked",
      reason: "header_hash_mismatch",
    });
  });

  it("does not substitute evidence for a different canonical header", () => {
    expect(
      classify({
        observations: [
          observation({
            datumHeaderHash: "66".repeat(28),
            computedHeaderHash: "66".repeat(28),
          }),
        ],
      }),
    ).toMatchObject({
      status: "ambiguous",
      reason: "canonical_header_absent",
    });
  });

  it("fails closed on duplicate canonical observations", () => {
    expect(
      classify({
        observations: [
          observation(),
          observation({ outRef: `${"77".repeat(32)}#1` }),
        ],
      }),
    ).toMatchObject({
      status: "blocked",
      reason: "canonical_header_not_unique",
    });
  });

  it("uses configured Cardano state-queue evidence even when a watcher URL is supplied", async () => {
    const fetchSorted = vi.mocked(SDK.fetchSortedStateQueueUTxOsProgram);
    const getNode = vi.mocked(SDK.getStateQueueNodeV1FromStateQueueDatum);
    const hashHeader = vi.mocked(SDK.hashBlockHeaderV1);
    const watcherFetch = vi
      .spyOn(globalThis, "fetch")
      .mockRejectedValue(
        new Error("watcher access is not part of reconciliation"),
      );
    const contracts = {
      stateQueue: {
        spendingScriptAddress: "addr_test1statequeue",
        policyId: "aa".repeat(28),
      },
      daAttestation: { policyId: DA_ATTESTATION_POLICY_ID },
    } as unknown as MidgardContracts;
    const canonicalDatum = {
      key: { Key: { key: HEADER_HASH } },
      data: "canonical-node",
    };
    fetchSorted.mockReturnValue(
      Effect.succeed([
        {
          utxo: {
            txHash: "33".repeat(32),
            outputIndex: 0,
            address: contracts.stateQueue.spendingScriptAddress,
            assets: {},
            datum: "canonical-datum",
          },
          datum: canonicalDatum,
        },
      ] as never),
    );
    getNode.mockReturnValue(
      Effect.succeed({
        header: { canonical: true },
        da_attestation: DA_ATTESTATION_POLICY_ID,
      } as never),
    );
    hashHeader.mockReturnValue(Effect.succeed(HEADER_HASH) as never);

    const sql = ((..._args: readonly unknown[]) =>
      Effect.succeed([])) as unknown as SqlClient.SqlClient;
    const lucid = { api: {} } as unknown as Lucid;
    const result = await Effect.runPromise(
      reconcileDaAttestedProgram({
        headerHash: Buffer.from(HEADER_HASH, "hex"),
        watcherUrl: "https://watcher.example.invalid",
        deploymentFingerprint: "deployment-fingerprint",
        repair: false,
      }).pipe(
        Effect.provideService(Lucid, lucid),
        Effect.provideService(MidgardContracts, contracts),
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );

    expect(result.status).toBe("satisfied");
    expect(result.evidence).toContainEqual(
      expect.objectContaining({
        kind: "canonical_l1_da_attestation",
        detail: expect.objectContaining({
          source: "configured_cardano_l1_query",
          decisionReason: "attestation_applied",
        }),
      }),
    );
    expect(watcherFetch).not.toHaveBeenCalled();
    watcherFetch.mockRestore();
  });
});
