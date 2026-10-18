import { describe, expect, it } from "vitest";

import { daAvailabilityCommitmentHash } from "../src/availability-challenge.assert-canonical-da-availability-commitment.js";
import { daAvailabilityPublishedTerminalCommitment } from "../src/availability-challenge.assert-canonical-da-availability-publication-datum.js";
import { buildDaAvailabilityCommitment } from "../src/availability-challenge.build-da-availability-commitment.js";
import {
  daAvailabilityStatusBinds,
  type DaPayloadAvailabilityAuthority,
  daPayloadAvailabilityBinding,
} from "../src/da-availability-payload-binding.js";
import type { DaAvailabilityStateQueueStatus } from "../src/da-availability-state.js";
import { availabilityResponseGeometry } from "../src/index.js";

const authority: DaPayloadAvailabilityAuthority = {
  deploymentIdentity: "c7".repeat(28),
  responseGeometry: availabilityResponseGeometry({
    chunkByteLength: 4096,
    trancheByteLength: 64 * 1024,
    maxTrancheCount: 1024,
  }),
};
const headerHash = "4e".repeat(28);
const payload = Uint8Array.from(
  { length: 9_000 },
  (_, i) => (i * 31 + 7) % 256,
);
const other = Uint8Array.from(payload, (byte, i) =>
  i === 8_999 ? byte ^ 1 : byte,
);
const commitment = buildDaAvailabilityCommitment({
  ...authority,
  headerHash,
  payload,
});
const hash = daAvailabilityCommitmentHash(commitment);
const terminal = daAvailabilityPublishedTerminalCommitment(commitment);

const binding = (
  status: DaAvailabilityStateQueueStatus,
  bytes: Uint8Array = payload,
  under: DaPayloadAvailabilityAuthority = authority,
) =>
  daPayloadAvailabilityBinding({
    status,
    authority: under,
    headerHash,
    payload: bytes,
  });

describe("a fetched payload against its node's availability commitment", () => {
  it("is unbound while the node is Unattested, whatever its bytes", () => {
    expect(binding("Unattested")).toBe("unbound");
    expect(binding("Unattested", other)).toBe("unbound");
    expect(daAvailabilityStatusBinds("Unattested")).toBe(false);
  });

  it("is bound to an Attested or Challenged node's commitment hash", () => {
    const challenged: DaAvailabilityStateQueueStatus = {
      Challenged: { commitment_hash: hash, challenge_asset_name: "00" },
    };
    for (const status of [
      { Attested: { commitment_hash: hash } },
      { Attested: { commitment_hash: hash.toUpperCase() } },
      challenged,
    ] as const) {
      expect(daAvailabilityStatusBinds(status)).toBe(true);
      expect(binding(status)).toBe("bound");
      expect(binding(status, other)).toBe("mismatch");
    }
  });

  it("is bound to a Published node's terminal commitment, not the commitment hash", () => {
    expect(binding({ Published: { terminal_commitment: terminal } })).toBe(
      "bound",
    );
    expect(
      binding({ Published: { terminal_commitment: terminal } }, other),
    ).toBe("mismatch");
    expect(binding({ Published: { terminal_commitment: hash } })).toBe(
      "mismatch",
    );
    expect(binding({ Attested: { commitment_hash: terminal } })).toBe(
      "mismatch",
    );
  });

  it("is a mismatch under another deployment's authority or for bytes that build no commitment", () => {
    const status = { Attested: { commitment_hash: hash } };
    expect(
      binding(status, payload, {
        ...authority,
        deploymentIdentity: "c8".repeat(28),
      }),
    ).toBe("mismatch");
    expect(binding(status, new Uint8Array())).toBe("mismatch");
  });
});
