import "./block-replay.registration.js";

import { RejectCodes } from "@al-ft/midgard-validation/types";
import { describe, expect, it } from "vitest";

import {
  WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_PROTOCOL_MINUS_UNCLAIMED,
  WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
  WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES,
} from "../../src/verification/block-replay.js";
import {
  WATCHER_PHASE_A_CANONICAL_REJECT_CODES,
  WATCHER_PHASE_A_EXCLUDED_REJECT_CODES,
  WATCHER_PHASE_A_REACHABLE_REJECT_CODES,
} from "../../src/verification/phase-a-verifier.js";

describe("W25 published rejection-code partition", () => {
  it("is a disjoint total 14/26/10 partition of the canonical 50-code vocabulary", () => {
    expect(WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES).toStrictEqual(
      Object.values(RejectCodes),
    );
    expect(WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES).toHaveLength(50);
    const claimed = [
      ...WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES,
      ...WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES,
      ...WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES,
    ];
    expect(new Set(claimed).size).toBe(50);
    expect([...claimed].sort()).toStrictEqual(
      [...WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES].sort(),
    );
    expect(WATCHER_BLOCK_REPLAY_PROTOCOL_MINUS_UNCLAIMED).toHaveLength(40);
    expect(WATCHER_PHASE_A_CANONICAL_REJECT_CODES).toStrictEqual(
      WATCHER_BLOCK_REPLAY_CANONICAL_REJECT_CODES,
    );
    expect(
      WATCHER_PHASE_A_REACHABLE_REJECT_CODES.filter(
        (code) =>
          !new Set<string>(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES).has(
            code,
          ),
      ),
    ).toStrictEqual(WATCHER_BLOCK_REPLAY_PHASE_A_OWNED_REJECT_CODES);
    expect(
      WATCHER_PHASE_A_EXCLUDED_REJECT_CODES.filter(
        (code) =>
          !new Set<string>(WATCHER_BLOCK_REPLAY_UNCLAIMED_REJECT_CODES).has(
            code,
          ),
      ),
    ).toStrictEqual(
      WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES.filter(
        (code) =>
          !new Set<string>(WATCHER_PHASE_A_REACHABLE_REJECT_CODES).has(code),
      ),
    );
  });

  it("keeps evidence and dominated claims inside Phase B's published set", () => {
    for (const code of WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES) {
      expect(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES).toContain(code);
    }
    for (const code of WATCHER_BLOCK_REPLAY_DOMINATED_REJECT_CODES) {
      expect(WATCHER_BLOCK_REPLAY_REACHABLE_REJECT_CODES).toContain(code);
      expect(WATCHER_BLOCK_REPLAY_EVIDENCED_REJECT_CODES).not.toContain(code);
    }
  });
});
