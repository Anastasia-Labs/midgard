import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { AvailabilityResponderAwaitingScanError } from "../src/availability/awaiting-scan-error.js";
import {
  AvailabilityResponder,
  type AvailabilityResponderDeps,
  availabilityResponderReportLine,
} from "../src/availability/responder.js";
import { retainedAvailabilityPayload } from "../src/availability/retained-payload.js";
import type { DaPayloadRecord } from "../src/domain.js";
import {
  challengeFixture,
  commitment,
  deploymentFingerprint,
  deploymentIdentity,
  payload,
  record,
  utxo,
} from "./helpers/availability-challenge.js";

describe("availability responder lifecycle", () => {
  it("answers retained data when the public source withholds after attestation, then settles and closes", async () => {
    const fixture = challengeFixture();
    let challenge = fixture.challenge;
    const executed: string[] = [];
    const deps: AvailabilityResponderDeps = {
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => fixture.stored },
      discover: async () => [challenge],
      reconcile: async () => "ready",
      now: () => 2_000,
      execute: async (action) => {
        executed.push(action.kind);
        if (action.kind === "publish") {
          challenge = {
            ...challenge,
            tranches: [
              {
                utxo: utxo(4),
                datum: SDK.advanceDaAvailabilityTranche({
                  active: action.tranche.datum,
                  publication: action.publication,
                  responseGeometry: commitment.response_geometry,
                  inclusiveValidityUpper: 3_000n,
                  carrierOutputIndex: 1n,
                }),
              },
            ],
          };
        } else if (action.kind === "settle") {
          challenge = {
            ...challenge,
            tranches: [],
            terminal: {
              ...challenge.terminal,
              datum: { ...challenge.terminal.datum, next_tranche_index: 1n },
            },
          };
        }
        return "confirmed";
      },
    };
    const responder = new AvailabilityResponder(deps);
    expect(await responder.tick()).toMatchObject({
      action: "publish",
      status: "confirmed",
    });
    expect(await responder.tick()).toMatchObject({
      action: "settle",
      status: "confirmed",
    });
    expect(await responder.tick()).toMatchObject({
      action: "close",
      status: "confirmed",
    });
    expect(executed).toEqual(["publish", "settle", "close"]);
  });

  it("resumes a partial answer from its chain offset after process restart", async () => {
    const bytes = new Uint8Array(5_000).fill(9);
    const fixture = challengeFixture(bytes);
    let challenge = fixture.challenge;
    const offsets: bigint[] = [];
    const deps: AvailabilityResponderDeps = {
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => fixture.stored },
      discover: async () => [challenge],
      reconcile: async () => "ready",
      now: () => 2_000,
      execute: async (action) => {
        if (action.kind !== "publish") throw new Error("Expected publication");
        offsets.push(action.publication.chunk_offset);
        challenge = {
          ...challenge,
          tranches: [
            {
              utxo: utxo(4),
              datum: SDK.advanceDaAvailabilityTranche({
                active: action.tranche.datum,
                publication: action.publication,
                responseGeometry: commitment.response_geometry,
                inclusiveValidityUpper: 3_000n,
                carrierOutputIndex: 1n,
              }),
            },
          ],
        };
        return "confirmed";
      },
    };
    await new AvailabilityResponder(deps).tick();
    await new AvailabilityResponder(deps).tick();
    expect(offsets).toEqual([0n, 4_096n]);
  });

  it("reconciles an ambiguous transaction before reading payloads or constructing another action", async () => {
    const fixture = challengeFixture();
    const discover = vi.fn(async () => [fixture.challenge]);
    const getDaPayload = vi.fn(async () => fixture.stored);
    const execute = vi.fn(async () => "confirmed" as const);
    const responder = new AvailabilityResponder({
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload },
      discover,
      execute,
      reconcile: async () => "pending",
    });
    expect(await responder.tick()).toMatchObject({ status: "pending" });
    expect(discover).not.toHaveBeenCalled();
    expect(getDaPayload).not.toHaveBeenCalled();
    expect(execute).not.toHaveBeenCalled();
  });

  it("leaves missing retained data visible and never fabricates a publication", async () => {
    const fixture = challengeFixture();
    const execute = vi.fn(async () => "confirmed" as const);
    const responder = new AvailabilityResponder({
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => undefined },
      discover: async () => [fixture.challenge],
      execute,
      reconcile: async () => "ready",
      now: () => 2_000,
    });
    expect(await responder.tick()).toMatchObject({ status: "unavailable" });
    expect(execute).not.toHaveBeenCalled();
  });

  it("publishes until the response deadline and never attempts one at or after it", async () => {
    const fixture = challengeFixture();
    const deadline = Number(fixture.challenge.record.datum.response_deadline);
    const tickAt = async (now: number) => {
      const execute = vi.fn(async () => "confirmed" as const);
      const report = await new AvailabilityResponder({
        deploymentIdentity,
        deploymentFingerprint,
        store: { getDaPayload: async () => fixture.stored },
        discover: async () => [fixture.challenge],
        execute,
        reconcile: async () => "ready",
        now: () => now,
      }).tick();
      return { report, execute };
    };
    const before = await tickAt(deadline - 1);
    expect(before.report).toMatchObject({
      action: "publish",
      status: "confirmed",
    });
    for (const now of [deadline, deadline + 1]) {
      const after = await tickAt(now);
      expect(after.report).toMatchObject({ status: "unavailable" });
      expect(after.execute).not.toHaveBeenCalled();
    }
  });
});

const retained = (stored: DaPayloadRecord | undefined) =>
  retainedAvailabilityPayload({
    store: { getDaPayload: async () => stored },
    deploymentFingerprint,
    deploymentIdentity,
    commitment,
  });

describe("retained availability responses", () => {
  it("publishes exact committed stored bytes without a current committee key", async () => {
    expect(await retained(record)).toEqual(Buffer.from(payload));
  });

  it("leaves absent payloads unavailable", async () => {
    expect(await retained(undefined)).toBeUndefined();
  });

  it("refuses corrupt bytes even when a record says verified", async () => {
    await expect(
      retained({ ...record, payloadCborHex: "01020305" }),
    ).rejects.toThrow(/digest/);
    const altered = Uint8Array.from([1, 2, 3, 5]);
    await expect(
      retained({
        ...record,
        payloadCborHex: Buffer.from(altered).toString("hex"),
        payloadSha256: computeDaSha256Hash(altered).toString("hex"),
      }),
    ).rejects.toThrow(/frozen signed commitment/);
  });

  it("refuses conflicted and foreign deployment records", async () => {
    await expect(
      retained({ ...record, conflictStatus: "conflicting_bytes" }),
    ).rejects.toThrow(/verified retained payload/);
    await expect(
      retained({ ...record, deploymentFingerprint: "55".repeat(32) }),
    ).rejects.toThrow(/this deployment/);
  });

  it("refuses a commitment naming another deployment identity before reading the store", async () => {
    const getDaPayload = vi.fn(async () => record);
    await expect(
      retainedAvailabilityPayload({
        store: { getDaPayload },
        deploymentFingerprint,
        deploymentIdentity,
        commitment: { ...commitment, deployment_identity: "ee".repeat(28) },
      }),
    ).rejects.toThrow("availability commitment belongs to another deployment");
    expect(getDaPayload).not.toHaveBeenCalled();
  });

  it.each<[string, Partial<DaPayloadRecord>]>([
    ["stored under another header", { headerHash: "ee".repeat(28) }],
    ...(
      ["fetched", "malformed_da", "root_mismatch", "conflicted"] as const
    ).map((validationStatus): [string, Partial<DaPayloadRecord>] => [
      `${validationStatus}, not verified`,
      { validationStatus },
    ]),
  ])("refuses a record %s", async (_name, change) => {
    await expect(retained({ ...record, ...change })).rejects.toThrow(
      "availability response requires a verified retained payload from this deployment",
    );
  });

  it("leaves a missing_da record unavailable", async () => {
    await expect(
      retained({ ...record, validationStatus: "missing_da" }),
    ).resolves.toBeUndefined();
  });

  it.each(["", "0", "AB", "zz", `${record.payloadCborHex}0`])(
    "refuses payload hex %j that is not canonical lowercase hex",
    async (payloadCborHex) => {
      await expect(retained({ ...record, payloadCborHex })).rejects.toThrow(
        "retained availability payload is not canonical hexadecimal",
      );
    },
  );
});

describe("availability responder awaiting the committee's next L1 scan", () => {
  const awaitingScan = {
    status: "awaiting_scan",
    detail: new AvailabilityResponderAwaitingScanError().message,
  };

  const responderWith = (
    overrides: Partial<
      Pick<AvailabilityResponderDeps, "reconcile" | "discover" | "execute">
    >,
  ) => {
    const fixture = challengeFixture();
    const discover = vi.fn(
      overrides.discover ?? (async () => [fixture.challenge]),
    );
    const execute = vi.fn(
      overrides.execute ?? (async () => "confirmed" as const),
    );
    const responder = new AvailabilityResponder({
      deploymentIdentity,
      deploymentFingerprint,
      store: { getDaPayload: async () => fixture.stored },
      discover,
      execute,
      reconcile: overrides.reconcile ?? (async () => "ready"),
      now: () => 2_000,
    });
    return { responder, discover, execute };
  };

  it("reports a reconcile refused for a lagging cursor as awaiting_scan and discovers nothing", async () => {
    const { responder, discover, execute } = responderWith({
      reconcile: async () => {
        throw new AvailabilityResponderAwaitingScanError();
      },
    });
    expect(await responder.tick()).toStrictEqual({
      challenges: 0,
      ...awaitingScan,
    });
    expect(discover).not.toHaveBeenCalled();
    expect(execute).not.toHaveBeenCalled();
  });

  it("reports a discovery refused for a lagging cursor as awaiting_scan", async () => {
    const { responder, execute } = responderWith({
      discover: async () => {
        throw new AvailabilityResponderAwaitingScanError();
      },
    });
    expect(await responder.tick()).toStrictEqual({
      challenges: 0,
      ...awaitingScan,
    });
    expect(execute).not.toHaveBeenCalled();
  });

  it("reports an action refused for a lagging cursor as awaiting_scan, not failed", async () => {
    const { responder, execute } = responderWith({
      execute: async () => {
        throw new AvailabilityResponderAwaitingScanError();
      },
    });
    expect(await responder.tick()).toStrictEqual({
      challenges: 1,
      headerHash: commitment.header_hash,
      ...awaitingScan,
    });
    expect(execute).toHaveBeenCalledOnce();
  });

  it("still throws a genuine reconcile error and still fails a genuine action error", async () => {
    await expect(
      responderWith({
        reconcile: async () => {
          throw new Error("kupmios read failed");
        },
      }).responder.tick(),
    ).rejects.toThrow("kupmios read failed");
    expect(
      await responderWith({
        execute: async () => {
          throw new Error("submit rejected");
        },
      }).responder.tick(),
    ).toMatchObject({ status: "failed", detail: "submit rejected" });
  });

  it("logs awaiting_scan as one compact stdout line and only failures to stderr", () => {
    const line = availabilityResponderReportLine({
      challenges: 0,
      status: "awaiting_scan",
      detail: awaitingScan.detail,
    });
    expect(line?.stream).toBe("stdout");
    expect(line?.line.split("\n")).toEqual([
      JSON.stringify({
        event: "availability_responder",
        challenges: 0,
        ...awaitingScan,
      }),
      "",
    ]);
    for (const status of ["failed", "unavailable"] as const) {
      expect(
        availabilityResponderReportLine({ challenges: 1, status }),
      ).toEqual({
        stream: "stderr",
        line: `${JSON.stringify({ event: "availability_responder", challenges: 1, status })}\n`,
      });
    }
    expect(
      availabilityResponderReportLine({ challenges: 1, status: "confirmed" })
        ?.stream,
    ).toBe("stdout");
    expect(
      availabilityResponderReportLine({ challenges: 0, status: "idle" }),
    ).toBeUndefined();
  });
});
