import { DatabaseSync } from "node:sqlite";

import { expect, it, vi } from "vitest";

import {
  assertWatcherProtocolParameterRuntimeAuthority,
  createWatcherProtocolParameterHistory,
  refreshWatcherProtocolParameterRuntimeAuthority,
  unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest,
} from "../../src/funding/prover-funding.js";
import type { WatcherProverFundingReservationRecord } from "../../src/funding/prover-funding-reservation.js";
import { makeWatcherDeploymentAuthorityFixture } from "../support/deployment-authority-fixture.js";
import { runtimeAuthority } from "./prover-funding-calculation.runtime-authority.js";
import { ogmiosParameters } from "./prover-funding-calculation.transaction-cbor.js";

it("authenticates historical parameters and refuses disk tampering, identity substitution and authority clones", async () => {
  const deploymentIdentity = makeWatcherDeploymentAuthorityFixture().result;
  const database = new DatabaseSync(":memory:");
  database.exec(
    "CREATE TABLE watcher_prover_funding_reservation_v1 (reservation_id TEXT PRIMARY KEY); INSERT INTO watcher_prover_funding_reservation_v1 VALUES ('" +
      "11".repeat(32) +
      "');",
  );
  const key = Buffer.alloc(32, 0x91);
  const record: WatcherProverFundingReservationRecord = {
    reservationId: "11".repeat(32),
    deploymentFingerprint: deploymentIdentity.manifestId,
    policyDigest: "22".repeat(32),
    reservationBasisDigest: "33".repeat(32),
    decisionDigest: "44".repeat(32),
    revision: "0",
    state: "active",
    activeInputs: [],
    pendingTransition: null,
    lastConfirmedTransitionDigest: null,
    conflictCode: null,
    recordDigest: "55".repeat(32),
  };
  const authority =
    await unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest({
      deploymentIdentity,
      ogmiosUrl: "http://127.0.0.1:1337",
      timeoutMs: 10_000,
      fetchImpl: vi.fn(async (_url, init) => {
        const { id } = JSON.parse(String(init?.body)) as { id: string };
        return new Response(
          JSON.stringify({
            jsonrpc: "2.0",
            id,
            result: { ...ogmiosParameters(), minFeeCoefficient: 45 },
          }),
        );
      }) as unknown as typeof fetch,
    });
  try {
    const history = createWatcherProtocolParameterHistory({
      database,
      deploymentIdentity,
      authenticationKey: key,
    });
    expect(history.read(record)).toBeNull();
    expect(() => history.remember(record, { ...authority })).toThrow(
      "not admitted",
    );
    expect(history.readCapacity(record)).toBeNull();
    expect(() => history.rememberCapacity(record, { ...authority })).toThrow(
      "not admitted",
    );
    history.remember(record, authority);
    history.rememberCapacity(record, authority);
    history.rememberCapacity(
      record,
      await runtimeAuthority(deploymentIdentity, 46, 1),
    );
    expect(history.readCapacity(record)!.snapshot.maxCollateralInputs).toBe(
      "3",
    );
    history.rememberCapacity(
      record,
      await runtimeAuthority(deploymentIdentity, 47, 5),
    );
    expect(history.readCapacity(record)!.snapshot.maxCollateralInputs).toBe(
      "5",
    );
    const restarted = createWatcherProtocolParameterHistory({
      database,
      deploymentIdentity,
      authenticationKey: key,
    });
    const capacity = restarted.readCapacity(record)!;
    expect(capacity.snapshot.maxCollateralInputs).toBe("5");
    expect(() => restarted.rememberCapacity(record, capacity)).toThrow(
      "admitted live local parameters",
    );
    expect(() =>
      restarted.readCapacity({
        ...record,
        reservationBasisDigest: "67".repeat(32),
      }),
    ).toThrow("identity mismatch");
    const recovered = restarted.read(record)!;
    expect(recovered.snapshot.minFeeA).toBe("45");
    expect(recovered.source).toBe("authenticated_history");
    expect(() =>
      assertWatcherProtocolParameterRuntimeAuthority(recovered),
    ).not.toThrow();
    await expect(
      refreshWatcherProtocolParameterRuntimeAuthority(recovered),
    ).rejects.toThrow("Historical funding parameters");
    expect(() =>
      restarted.read({ ...record, policyDigest: "66".repeat(32) }),
    ).toThrow("identity mismatch");
    const wrongKey = createWatcherProtocolParameterHistory({
      database,
      deploymentIdentity,
      authenticationKey: Buffer.alloc(32, 0x92),
    });
    expect(() => wrongKey.read(record)).toThrow("authentication mismatch");
    expect(() => wrongKey.readCapacity(record)).toThrow(
      "authentication mismatch",
    );
    const savedCapacity = database
      .prepare(
        "SELECT canonical_json, authentication_tag FROM watcher_prover_funding_capacity_v1 WHERE reservation_id = ?",
      )
      .get(record.reservationId)!;
    database
      .prepare(
        `UPDATE watcher_prover_funding_capacity_v1 SET canonical_json = replace(canonical_json, '"maxCollateralInputs":"5"', '"maxCollateralInputs":"6"') WHERE reservation_id = ?`,
      )
      .run(record.reservationId);
    expect(() => restarted.readCapacity(record)).toThrow(
      "authentication mismatch",
    );
    database
      .prepare(
        "UPDATE watcher_prover_funding_capacity_v1 SET canonical_json = (SELECT canonical_json FROM watcher_prover_funding_parameters_v1 WHERE reservation_id = ?), authentication_tag = (SELECT authentication_tag FROM watcher_prover_funding_parameters_v1 WHERE reservation_id = ?) WHERE reservation_id = ?",
      )
      .run(record.reservationId, record.reservationId, record.reservationId);
    expect(() => restarted.readCapacity(record)).toThrow("identity mismatch");
    database
      .prepare(
        "DELETE FROM watcher_prover_funding_capacity_v1 WHERE reservation_id = ?",
      )
      .run(record.reservationId);
    expect(restarted.readCapacity(record)).toBeNull();
    database
      .prepare(
        "INSERT INTO watcher_prover_funding_capacity_v1 VALUES (?, ?, ?)",
      )
      .run(
        record.reservationId,
        savedCapacity.canonical_json!,
        savedCapacity.authentication_tag!,
      );
    expect(restarted.readCapacity(record)!.snapshot.maxCollateralInputs).toBe(
      "5",
    );
    database
      .prepare(
        'UPDATE watcher_prover_funding_parameters_v1 SET canonical_json = replace(canonical_json, \'"minFeeA":"45"\', \'"minFeeA":"46"\') WHERE reservation_id = ?',
      )
      .run(record.reservationId);
    expect(() => restarted.read(record)).toThrow("authentication mismatch");
  } finally {
    database.close();
  }
});
