import { rmSync } from "node:fs";

import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, describe, expect, it, vi } from "vitest";

import { promiseAdmissionActorMetadata } from "../src/availability/promise-actor-metadata.js";
import {
  dirs,
  journals,
  scene,
} from "./helpers/availability-sdk-read-scope.js";

afterEach(() => {
  journals.splice(0).forEach((journal) => journal.close());
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true }));
});

describe("bounded admission journal metadata", () => {
  it("refuses a real retained row before materializing its identity digest", () => {
    const f = scene();
    const intent = SDK.inspectDaAvailabilitySignedIntent({
      deploymentIdentity: f.context.deploymentIdentity,
      actor: f.context.actor,
      headerHash: f.operation.headerHash,
      action: f.operation.action,
      signedCbor: f.tx.toTransaction().to_cbor_hex(),
    });
    const lease = f.journal.acquire(f.context.actor, "setup", Date.now(), 1000);
    f.journal.persist(lease, intent, Date.now());
    f.journal.release(lease);
    const actorSnapshot = vi.spyOn(f.journal, "actorSnapshot");
    const metadata = (maximumRetainedRecords: number) =>
      promiseAdmissionActorMetadata({
        journal: f.journal,
        actorId: f.context.actor,
        deploymentIdentity: f.context.deploymentIdentity,
        maximumRetainedRecords,
        hasCanonicalCompatibilityAuthority: true,
      });
    expect(() => metadata(0).snapshot()).toThrow("exceeds");
    expect(actorSnapshot).not.toHaveBeenCalled();
    expect(() => metadata(Number.NaN).snapshot()).toThrow("invalid");
    expect(actorSnapshot).not.toHaveBeenCalled();
    expect(metadata(1).snapshot().retainedRecordCount).toBe(1);
    expect(actorSnapshot).toHaveBeenCalledTimes(1);
    expect(f.journal.get(intent.id)?.intent).toEqual(intent);
  });
});
