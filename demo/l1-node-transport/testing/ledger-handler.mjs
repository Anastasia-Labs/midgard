// A fake sidecar handler (LEDGER_HANDLER) whose node ledger registers one
// script credential, `options.scriptHash`, with a 2 ADA deposit: undelegated
// unless `options.pool` names a pool key hash, holding `options.rewards`
// (default 0). Every other credential is absent. The node runs network
// `options.magic`; `options.silent` leaves ledger queries unanswered, as a
// node that stopped responding would.
import { encodeCbor } from "./fake-sidecar.mjs";

const tip = { point: { slot: 99, hash: "ef".repeat(32) }, blockNo: 42 };
const hexOf = (bytes) => Buffer.from(bytes).toString("hex");

export default (options) => {
  const registered = ([tag, hash]) =>
    Number(tag) === 1 && hexOf(hash) === options.scriptHash;
  const entries = (credentials, value) =>
    new Map(
      credentials.filter(registered).map((credential) => [credential, value]),
    );
  return {
    hello: ({ networkMagic }) =>
      networkMagic === options.magic
        ? undefined
        : {
            fatal: {
              code: "node_handshake_failed",
              message: "network magic differs",
              status: 69,
            },
          },
    ledgerQuery: async (query) => {
      if (options.silent) return await new Promise(() => {});
      switch (query.query) {
        case "chain_point":
          return encodeCbor([
            BigInt(tip.point.slot),
            Buffer.from(tip.point.hash, "hex"),
          ]);
        case "chain_block_no":
          return encodeCbor([1, tip.blockNo]);
        case "stake_deleg_deposits":
          return encodeCbor(entries(query.credentials, 2_000_000));
        case "filtered_delegations_and_rewards":
          return encodeCbor([
            options.pool === undefined
              ? new Map()
              : entries(query.credentials, Buffer.from(options.pool, "hex")),
            entries(query.credentials, options.rewards ?? 0),
          ]);
        default:
          return { error: { code: "unknown_query", message: query.query } };
      }
    },
  };
};
