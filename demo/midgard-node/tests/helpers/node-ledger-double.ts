/**
 * A local node over an emulator's ledger, for a deployment command run with
 * `--l1 node`: every ledger query is logged as `connection:query:deadline`,
 * and a submission lands in the next block. A real node's chain time is the
 * wall clock, so each block the emulator makes sets the mocked system time
 * to its time; a suite restores it with `vi.useRealTimers()`.
 */
import {
  encodeCbor,
  type L1NodeTransport,
  type LedgerQuery,
  type LedgerStateSession,
} from "@al-ft/l1-node-transport";
import { fakeL1NodeTransport } from "@al-ft/l1-node-transport/testing";
import { LedgerProvider } from "@al-ft/midgard-l1-follower/provider";
import {
  type Emulator,
  getAddressDetails,
  Lucid as makeLucid,
} from "@lucid-evolution/lucid";
import { vi } from "vitest";

import { stateOf, utxoAnswer } from "./follower-emulator.ledger.js";

export const nodeLedgerDouble = (emulator: Emulator) => {
  const state = stateOf(emulator);
  const queries: string[] = [];
  const closed: string[] = [];
  const submitted: string[] = [];
  /** The next block, at the system time it is made. */
  const nextBlock = () => {
    emulator.awaitBlock(1);
    vi.setSystemTime(emulator.now());
  };
  const live = () =>
    Object.values(state.ledger).flatMap(({ utxo, spent }) =>
      spent ? [] : [utxo],
    );
  const answer = (query: LedgerQuery): Uint8Array => {
    switch (query.query) {
      case "chain_point":
        return encodeCbor([BigInt(emulator.slot), Buffer.alloc(32)]);
      case "chain_block_no":
        return encodeCbor([1n, BigInt(emulator.blockHeight)]);
      case "utxo_by_txin": {
        const wanted = query.txIns.map(
          ({ txId, index }) => `${txId}#${index.toString()}`,
        );
        return utxoAnswer(
          live().filter(({ txHash, outputIndex }) =>
            wanted.includes(`${txHash}#${outputIndex.toString()}`),
          ),
        );
      }
      case "utxo_by_address": {
        const wanted = query.addresses.map((address) =>
          Buffer.from(address).toString("hex"),
        );
        return utxoAnswer(
          live().filter(({ address }) =>
            wanted.includes(getAddressDetails(address).address.hex),
          ),
        );
      }
      case "system_start":
      case "current_era":
      case "era_history":
      case "protocol_params":
      case "stake_deleg_deposits":
      case "filtered_delegations_and_rewards":
        throw new Error(`the ledger double does not answer ${query.query}`);
    }
  };
  /** A connection to the node, its queries logged under `name`. */
  const connection = (name: string): L1NodeTransport => {
    const session: LedgerStateSession = {
      query: async (query, options) => {
        queries.push(
          `${name}:${query.query}:${options?.deadline ?? "bounded"}`,
        );
        return answer(query);
      },
    };
    return fakeL1NodeTransport({
      query: (query) => session.query(query),
      withLedgerState: async (_at, use) => await use(session),
      submit: async (tx) => {
        const cbor = Buffer.from(tx).toString("hex");
        submitted.push(cbor);
        await emulator.submitTx(cbor);
        nextBlock();
        return { accepted: true };
      },
      hasTx: async () => false,
      close: async () => {
        closed.push(name);
      },
    });
  };
  /**
   * A command's Lucid over the node, as `--l1 node` opens it: one provider
   * whose own wallets (`ownWallets`, bech32) are read on a connection of
   * their own, `wallet`; every other query goes over `main`.
   */
  const lucid = async (ownWallets: readonly string[]) => {
    const { slotConfig, protocolParameters } = (
      await makeLucid(emulator, "Custom")
    ).config();
    return await makeLucid(
      new LedgerProvider({
        transport: connection("main"),
        pinPointMs: 0,
        ownWallets: {
          addresses: ownWallets.map((address) =>
            Buffer.from(getAddressDetails(address).address.hex, "hex"),
          ),
          ledger: () => connection("wallet"),
        },
      }),
      "Custom",
      { slotConfig, presetProtocolParameters: protocolParameters },
    );
  };
  return { queries, closed, submitted, live, lucid, nextBlock };
};
