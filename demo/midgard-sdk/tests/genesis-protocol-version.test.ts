import { describe, expect, it } from "vitest";

import {
  type ConfirmedState,
  confirmedStateNextHeaderProtocolVersion,
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
  GENESIS_PROTOCOL_VERSION,
  makeGenesisConfirmedState,
} from "../src/index.js";

const ordinaryHeaderHash = "aa".repeat(28);
const nonGenesisRoot = "bb".repeat(32);

const confirmedState = (
  overrides: Partial<ConfirmedState> = {},
): ConfirmedState => ({
  headerHash: GENESIS_HEADER_HASH,
  prevHeaderHash: GENESIS_HEADER_HASH,
  utxoRoot: EMPTY_MERKLE_TREE_ROOT,
  startTime: 10n,
  endTime: 10n,
  protocolVersion: GENESIS_PROTOCOL_VERSION,
  ...overrides,
});

describe("V1 genesis protocol identity", () => {
  it("keeps sentinel 0 authenticated and yields version 1 for ordinary headers", () => {
    expect(makeGenesisConfirmedState(10n)).toEqual(confirmedState());
    expect(() => makeGenesisConfirmedState(-1n)).toThrow(
      /must be non-negative/u,
    );
    expect(confirmedStateNextHeaderProtocolVersion(confirmedState())).toBe(1n);
    expect(
      confirmedStateNextHeaderProtocolVersion(
        confirmedState({
          headerHash: ordinaryHeaderHash,
          utxoRoot: nonGenesisRoot,
          endTime: 11n,
          protocolVersion: 1n,
        }),
      ),
    ).toBe(1n);

    const invalidStates: readonly ConfirmedState[] = [
      confirmedState({ protocolVersion: 1n }),
      confirmedState({ headerHash: ordinaryHeaderHash }),
      confirmedState({ prevHeaderHash: ordinaryHeaderHash }),
      confirmedState({ utxoRoot: nonGenesisRoot }),
      confirmedState({ endTime: 11n }),
      confirmedState({ startTime: -1n, endTime: -1n }),
      confirmedState({
        headerHash: ordinaryHeaderHash,
        utxoRoot: nonGenesisRoot,
        startTime: 11n,
        endTime: 10n,
        protocolVersion: 1n,
      }),
      confirmedState({
        headerHash: ordinaryHeaderHash,
        utxoRoot: nonGenesisRoot,
        endTime: 11n,
        protocolVersion: 2n,
      }),
    ];
    for (const invalidState of invalidStates) {
      expect(confirmedStateNextHeaderProtocolVersion(invalidState)).toBeNull();
    }
  });
});
