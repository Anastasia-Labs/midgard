// The handler of the fake sidecar's conformance test: a three-block chain,
// one ledger state and a mempool holding one transaction.
import { encodeCbor, samePoint } from "../../testing/fake-sidecar.mjs";

const hash = (byte) => byte.repeat(32);
const blocks = [1, 2, 3].map((n) => ({
  point: { slot: 10 * n, hash: hash(`0${n}`) },
  blockNo: n,
  prevHash: n === 1 ? hash("00") : hash(`0${n - 1}`),
  block: Uint8Array.from([0x82, n, n]),
}));
const tip = { point: blocks[2].point, blockNo: 3 };

export default (options, controls) => ({
  hello: ({ networkMagic }) =>
    networkMagic === options.magic
      ? { nodeToClientVersion: 32784 }
      : {
          fatal: {
            code: "node_handshake_failed",
            message: "magic mismatch",
            status: 69,
          },
        },
  openStream: ({ points }, stream) => {
    const known = ["origin", ...blocks.map((block) => block.point)];
    const at = points.find((point) =>
      known.some((entry) => samePoint(entry, point)),
    );
    if (at === undefined) return { notFound: tip };
    const from = known.findIndex((entry) => samePoint(entry, at));
    for (const [index, block] of blocks.slice(from).entries()) {
      if (options.skipSequenceAt === index) stream.skipSequence();
      stream.rollForward({ ...block, tip });
    }
    if (options.rollBackAfter)
      stream.rollBackward({ point: blocks[0].point, tip });
    if (options.failAfter) stream.fail("node_connection_lost", "fake fault");
    return { intersection: at, tip };
  },
  ledgerQuery: (query) => {
    switch (query.query) {
      case "chain_point":
        return encodeCbor([
          BigInt(tip.point.slot),
          Buffer.from(tip.point.hash, "hex"),
        ]);
      case "chain_block_no":
        return encodeCbor([1, 3]);
      case "stake_deleg_deposits":
        return encodeCbor(new Map([[query.credentials[0], 2_000_000]]));
      case "filtered_delegations_and_rewards":
        return encodeCbor([new Map(), new Map([[query.credentials[0], 5]])]);
      default:
        return { error: { code: "unknown_query", message: query.query } };
    }
  },
  submit: (tx) =>
    tx[0] === 1 ? { accepted: true } : { rejection: Uint8Array.from([0x80]) },
  hasTx: (txId) => txId === hash("ab"),
  sizes: () => {
    if (!options.malformedAnswer)
      return { capacity: 100, size: 10, txCount: 1 };
    controls.writeFrame({ type: "ok", id: "not a number" });
    return new Promise(() => {});
  },
  ...(options.crashOnSubmit ? { submit: () => controls.exit(1) } : {}),
  ignoreInputEnd: options.ignoreInputEnd === true,
});
