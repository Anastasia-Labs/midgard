import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { statusIdentityMismatch } from "../src/committee-service.l1-tick.js";
import type { StateQueueHeaderRecord } from "../src/domain.js";

const OUT_REF = `${"34".repeat(32)}#0`;
const hash = (byte: string) => byte.repeat(32);

const unattested: SDK.DaAvailabilityStateQueueStatus = "Unattested";
const attested = (commitment: string): SDK.DaAvailabilityStateQueueStatus =>
  ({ Attested: { commitment_hash: commitment } }) as never;
const challenged = (
  commitment: string,
  asset: string,
): SDK.DaAvailabilityStateQueueStatus =>
  ({
    Challenged: { commitment_hash: commitment, challenge_asset_name: asset },
  }) as never;
const published = (terminal: string): SDK.DaAvailabilityStateQueueStatus =>
  ({ Published: { terminal_commitment: terminal } }) as never;

const record = (
  daAttestation: SDK.DaAvailabilityStateQueueStatus,
  status = "attested",
  stateQueueOutRef = OUT_REF,
): StateQueueHeaderRecord =>
  ({ stateQueueOutRef, daAttestation, status }) as StateQueueHeaderRecord;

/**
 * An output's datum never changes, so a stored record and this tick's record
 * of one output must carry one DA status identity; a stored merged or
 * removed record is history and never compared.
 */
describe("the stored-versus-observed DA status identity of one output", () => {
  it.each([
    [
      "Attested, another commitment",
      attested(hash("a1")),
      attested(hash("a2")),
    ],
    [
      "Challenged, another commitment",
      challenged(hash("a1"), "c1"),
      challenged(hash("a2"), "c1"),
    ],
    [
      "Challenged, another challenge asset",
      challenged(hash("a1"), "c1"),
      challenged(hash("a1"), "c2"),
    ],
    [
      "Published, another terminal",
      published(hash("b1")),
      published(hash("b2")),
    ],
    ["Unattested, then Attested", unattested, attested(hash("a1"))],
    [
      "Attested, then Challenged",
      attested(hash("a1")),
      challenged(hash("a1"), "c1"),
    ],
  ] as const)(
    "names the output when the identity differs: %s",
    (_name, stored, observed) => {
      expect(
        statusIdentityMismatch([record(stored)], [record(observed)]),
      ).toMatch(
        new RegExp(
          `^state-queue status at unchanged output ${OUT_REF}: stored=.+, observed=.+$`,
          "u",
        ),
      );
    },
  );

  it.each([
    ["Unattested", unattested],
    ["Attested", attested(hash("a1"))],
    ["Challenged", challenged(hash("a1"), "c1")],
    ["Published", published(hash("b1"))],
  ] as const)("finds nothing when both agree: %s", (_name, status) => {
    expect(
      statusIdentityMismatch([record(status)], [record(status)]),
    ).toBeUndefined();
  });

  it.each(["merged", "removed"])(
    "never compares a stored %s record",
    (status) => {
      expect(
        statusIdentityMismatch(
          [record(attested(hash("a1")), status)],
          [record(attested(hash("a2")))],
        ),
      ).toBeUndefined();
    },
  );

  it("compares only records of one output", () => {
    expect(
      statusIdentityMismatch(
        [record(attested(hash("a1")), "attested", `${"56".repeat(32)}#0`)],
        [record(attested(hash("a2")))],
      ),
    ).toBeUndefined();
  });
});
