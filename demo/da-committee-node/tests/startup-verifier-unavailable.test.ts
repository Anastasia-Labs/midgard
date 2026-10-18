/**
 * The committee with a ledger Ed25519 verifier (libsodium) that never
 * initialises: the startup's first wait runs out at its bound and the process
 * exits non-zero, never holding or retrying; a handler or the submitter's
 * reconciliation, each waiting for the verifier, rejects with the bound's
 * verifier-unavailable error, never a refusal of what it was handed.
 */
import {
  findLedgerSignatureVerifierUnavailable,
  LEDGER_ED25519_READY_TIMEOUT_MS,
} from "@al-ft/midgard-core";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import { committeePromiseAdmission } from "../src/availability/promise-admission.js";
import { ingestDaConflictEvidence } from "../src/committee-service.ingest-da-conflict-evidence.js";
import { committeeFailureExit } from "../src/config-refusal-exit.js";
import { SubmitterReconciler } from "../src/coordinator/submitter-reconciler.js";
import { daSignatureRecordFromAttestation } from "../src/da/libp2p/attestations.da-signature-record-from-attestation.js";
import { StoreBackedDaAttestationProtocol } from "../src/da/libp2p/attestations.store-backed-da-attestation-protocol.js";
import {
  type CommitteeNodeLocalSetup,
  openCommitteeNodeRuntime,
} from "../src/node-runtime.js";
import { PeerSignaturePoller } from "../src/peer/poller.js";
import {
  CommitteeStartupFailedError,
  retryStartup,
  type StartingServer,
  startOrHold,
  startupFailureOutcome,
} from "../src/startup.js";

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (
      await importOriginal<{ default: { ready: Promise<void> } }>()
    ).default;
    const ready = new Promise<void>(() => undefined);
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? ready
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const HEADER = "11".repeat(32);

const rejectionOf = async (run: Promise<unknown>): Promise<unknown> =>
  run.then(
    (value) => ({ resolved: value }),
    (error: unknown) => error,
  );

describe("the committee startup with a verifier that never initialises", () => {
  let failure: unknown;
  const lines: string[] = [];
  const reasons: string[] = [];
  const server = { closed: 0, held: [] as string[], attempts: 0 };
  const attempt = () => {
    server.attempts += 1;
    return openCommitteeNodeRuntime(
      { config: {} } as unknown as CommitteeNodeLocalSetup,
      {},
    );
  };

  beforeAll(async () => {
    vi.useFakeTimers();
    const starting: StartingServer = {
      address: () => null,
      setReason: (reason) => reasons.push(reason),
      hold: (detail) => server.held.push(detail),
      close: async () => {
        server.closed += 1;
      },
    };
    const started = rejectionOf(
      startOrHold(
        starting,
        (line) => lines.push(line),
        () =>
          retryStartup({
            attempt,
            onFailure: (reason) => reasons.push(reason),
            write: (line) => lines.push(line),
            sleep: () => Promise.resolve(),
          }),
      ),
    );
    await vi.advanceTimersByTimeAsync(LEDGER_ED25519_READY_TIMEOUT_MS);
    failure = await started;
  });

  afterAll(() => {
    vi.useRealTimers();
  });

  it("exits non-zero at its bound, without a retry or a hold", () => {
    expect(failure).toBeInstanceOf(CommitteeStartupFailedError);
    expect(failure).toMatchObject({
      reason: "committee_startup_verifier_unavailable",
      attempts: 1,
    });
    expect(findLedgerSignatureVerifierUnavailable(failure)).toBeDefined();
    expect(startupFailureOutcome(failure)).toBe("exit");
    expect(committeeFailureExit(failure).code).toBe(1);
    expect(server).toEqual({ closed: 1, held: [], attempts: 1 });
    expect(reasons).toEqual([]);
    expect(lines).toEqual([]);
  });

  it.each([
    [
      "promise admission",
      () =>
        committeePromiseAdmission({
          config: {} as never,
          store: {} as never,
          signerValidation: {} as never,
          source: {} as never,
        }).check({
          record: { headerHash: HEADER },
          commitmentDigest: HEADER,
        } as never),
    ],
    [
      "conflict evidence",
      () =>
        ingestDaConflictEvidence({
          deploymentFingerprint: HEADER,
          registry: {} as never,
          store: {} as never,
          context: { topicName: "not-the-conflicts-topic" } as never,
          receivedAt: new Date(0),
        }),
    ],
    [
      "an announced attestation",
      () =>
        daSignatureRecordFromAttestation({
          deploymentFingerprint: HEADER,
          committeeValidation: {} as never,
          store: {} as never,
          announcerPeerId: "peer",
          attestation: {
            headerHash: Buffer.from(HEADER, "hex"),
            deploymentFingerprint: Buffer.from("22".repeat(32), "hex"),
            announcedByPeerId: "peer",
          } as never,
        }),
    ],
    [
      "an accepted attestation",
      () =>
        new StoreBackedDaAttestationProtocol({
          store: { getDaPayload: () => Promise.resolve(undefined) },
        } as never).acceptAttestation({
          record: { headerHash: HEADER } as never,
          sourcePeerId: "peer",
        }),
    ],
    [
      "a submitter reconciliation",
      () =>
        new SubmitterReconciler({
          deploymentFingerprint: HEADER,
          committeeValidation: {} as never,
          availabilityCommitmentAuthority: {
            deploymentIdentity: "99".repeat(28),
            responseGeometry: {
              chunkByteLength: 4_096,
              trancheByteLength: 4 * 1_024 * 1_024,
              maxTrancheCount: 16,
            },
          },
          store: {
            getDaPayload: () =>
              Promise.resolve({
                validationStatus: "verified",
                payloadSha256: "33".repeat(32),
                payloadCborHex: "aabb",
              } as never),
            listDaSignatures: () => Promise.resolve([]),
          },
          coordinator: {
            reconcileAttestation: () => Promise.resolve("posted"),
          },
        }).reconcileHeader({
          headerHash: "11".repeat(28),
          finalized: true,
          status: "unattested",
        } as never),
    ],
    [
      "a peer signature poll",
      () =>
        new PeerSignaturePoller({
          peers: [],
          store: {},
        } as never).pollPeerSignatures(HEADER),
    ],
  ] as const)(
    "rejects %s with the verifier-unavailable error",
    async (_site, run) => {
      const outcome = await rejectionOf(run());
      expect(findLedgerSignatureVerifierUnavailable(outcome)).toBe(outcome);
      expect(outcome).toHaveProperty(
        "message",
        `Ed25519 verification is unavailable: libsodium did not initialise within ${LEDGER_ED25519_READY_TIMEOUT_MS.toString()} ms`,
      );
    },
  );
});
